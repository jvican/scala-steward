/*
 * Copyright 2018-2022 Scala Steward contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.scalasteward.core.buildtool.gradle

import better.files.File
import cats.implicits._
import org.typelevel.log4cats.Logger
import org.scalasteward.core.application.Config
import org.scalasteward.core.buildtool.BuildToolAlg
import org.scalasteward.core.vcs.data.BuildRoot
import org.scalasteward.core.data.Scope
import org.scalasteward.core.io.{FileAlg, ProcessAlg, WorkspaceAlg}
import org.scalasteward.core.edit.scalafix.ScalafixMigration
import org.scalasteward.core.util.Nel
import cats.effect.{MonadCancelThrow, Resource}
import cats.Defer

final class GradleAlg[F[_]](config: Config)(implicit
    fileAlg: FileAlg[F],
    logger: Logger[F],
    processAlg: ProcessAlg[F],
    workspaceAlg: WorkspaceAlg[F],
    F: MonadCancelThrow[F],
    D: Defer[F]
) extends BuildToolAlg[F] {
  override def containsBuild(buildRoot: BuildRoot): F[Boolean] =
    workspaceAlg
      .buildRootDir(buildRoot)
      .flatMap(repoDir => fileAlg.isRegularFile(repoDir / "build.gradle"))

  private val scalaStewardCommentHeader = "// --- scala-steward ---"

  def addDependencyTaskToBuild(repoDir: File): F[Unit] = {
    val stewardDependenciesTaskDef =
      s"""$scalaStewardCommentHeader
         |allprojects {
         |    task stewardDependencies() {
         |        doLast {
         |            println("repositories")
         |            project.repositories { repositories ->
         |                repositories.forEach { repo ->
         |                    println("name: $${repo.name}")
         |                    println("url: $${repo.url}")
         |                }
         |            }
         |            println("dependency-lock-file")
         |            println(project.projectDir.toString() + "/dependencies.lock")
         |        }
         |    }
         |}""".stripMargin

    for {
      _ <- fileAlg.editFile(
        repoDir / "build.gradle",
        contents => Some(contents + System.lineSeparator + stewardDependenciesTaskDef)
      )
    } yield ()
  }

  def removeDependencyTaskFromBuild(repoDir: File): F[Unit] =
    for {
      _ <- fileAlg.editFile(
        repoDir / "build.gradle",
        contents => Some(contents.split(scalaStewardCommentHeader)(0))
      )
    } yield ()

  override def getDependencies(buildRoot: BuildRoot): F[List[Scope.Dependencies]] =
    for {
      _ <- logger.info(s"Parsing Gradle dependencies and resolvers")
      repoDir <- workspaceAlg.buildRootDir(buildRoot)
      _ <- addDependencyTaskToBuild(repoDir)
      lines <- exec(gradleCmd(command.stewardDependencies), repoDir)
      dependencies <- parser.parseDependencies(lines)
      _ <- removeDependencyTaskFromBuild(repoDir)
    } yield dependencies

  override def runMigration(buildRoot: BuildRoot, migration: ScalafixMigration): F[Unit] =
    for {
      repoDir <- workspaceAlg.buildRootDir(buildRoot)
      _ <- logger.info(s"Running migrations $repoDir $migration")
      _ <- exec(gradleCmd(command.stewardDependencies), repoDir)
    } yield ()

  private def exec(command: Nel[String], repoDir: File): F[List[String]] =
    maybeIgnoreOptsFiles(repoDir).surround(processAlg.execSandboxed(command, repoDir))

  private def gradleCmd(commands: String*): Nel[String] =
    Nel("./gradlew", commands.toList)

  private def maybeIgnoreOptsFiles(dir: File): Resource[F, Unit] =
    if (config.ignoreOptsFiles) ignoreOptsFiles(dir) else Resource.unit[F]

  private def ignoreOptsFiles(dir: File): Resource[F, Unit] =
    fileAlg.removeTemporarily(dir / ".jvmopts")
}
