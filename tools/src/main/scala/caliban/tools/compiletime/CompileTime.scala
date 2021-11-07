package caliban.tools.compiletime

import caliban.GraphQL
import caliban.tools.Codegen.GenType
import caliban.tools.compiletime.Config.ClientGenerationSettings
import caliban.tools.{ Codegen, SchemaLoader }
import zio.{ ExitCode, URIO, ZEnv, ZIO }

object CompileTime {

  def generateClient[R](
    args: List[String]
  )(api: GraphQL[R], settings: ClientGenerationSettings): URIO[ZEnv, ExitCode] =
    args match {
      case baseDir :: Nil =>
        Codegen
          .generate(
            SchemaLoader.fromCaliban(api),
            settings.toCalibanCommonSettings.toOptions(
              schemaPath = "",
              toPath = {
                val dir = Utils.toPathDir(baseDir, settings.packageName)

                s"$dir/${settings.clientName}.scala"
              }
            ),
            GenType.Client
          )
          .exitCode
      case _              =>
        ZIO
          .fail(new RuntimeException(s"`CompileTime.generateClient` was called with invalid arguments: $args"))
          .exitCode
    }

}
