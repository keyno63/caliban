package caliban.wrappers

import caliban.CalibanError.{ ExecutionError, ValidationError }
import caliban.GraphQL._
import caliban.InputValue.ObjectValue
import caliban.Macros.gqldoc
import caliban.TestUtils._
import caliban.Value.StringValue
import caliban._
import caliban.execution.{ ExecutionRequest, FieldInfo }
import caliban.introspection.adt.{ __Directive, __DirectiveLocation }
import caliban.schema.{ GenericSchema, Schema }
import caliban.schema.Schema.auto._
import caliban.wrappers.ApolloCaching.GQLCacheControl
import caliban.wrappers.ApolloPersistedQueries.apolloPersistedQueries
import caliban.wrappers.Wrapper.{ ExecutionWrapper, FieldWrapper }
import caliban.wrappers.Wrappers._
import io.circe.syntax._
import zio._
import zio.query.ZQuery
import zio.test.Assertion._
import zio.test._

import scala.language.postfixOps

object WrappersSpec extends ZIOSpecDefault {
  override def spec =
    suite("WrappersSpec")(
      test("wrapPureValues false") {
        case class Test(a: Int, b: UIO[Int])
        for {
          ref         <- Ref.make[Int](0)
          wrapper      = new FieldWrapper[Any](false) {
                           def wrap[R1 <: Any](
                             query: ZQuery[R1, ExecutionError, ResponseValue],
                             info: FieldInfo
                           ): ZQuery[R1, ExecutionError, ResponseValue] =
                             ZQuery.fromZIO(ref.update(_ + 1)) *> query
                         }
          interpreter <- (graphQL(RootResolver(Test(1, ZIO.succeed(2)))) @@ wrapper).interpreter.orDie
          query        = gqldoc("""{ a b }""")
          _           <- interpreter.execute(query)
          counter     <- ref.get
        } yield assertTrue(counter == 1)
      },
      test("wrapPureValues true") {
        case class Test(a: Int, b: UIO[Int])
        for {
          ref         <- Ref.make[Int](0)
          wrapper      = new FieldWrapper[Any](true) {
                           def wrap[R1 <: Any](
                             query: ZQuery[R1, ExecutionError, ResponseValue],
                             info: FieldInfo
                           ): ZQuery[R1, ExecutionError, ResponseValue] =
                             ZQuery.fromZIO(ref.update(_ + 1)) *> query
                         }
          interpreter <- (graphQL(RootResolver(Test(1, ZIO.succeed(2)))) @@ wrapper).interpreter.orDie
          query        = gqldoc("""{ a b }""")
          _           <- interpreter.execute(query)
          counter     <- ref.get
        } yield assertTrue(counter == 2)
      },
      test("Max fields") {
        case class A(b: B)
        case class B(c: Int)
        case class Test(a: A)
        val interpreter = (graphQL(RootResolver(Test(A(B(2))))) @@ maxFields(2)).interpreter
        val query       = gqldoc("""
              {
                a {
                  b {
                    c
                  }
                }
              }""")
        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.errors == List(ValidationError("Query has too many fields: 3. Max fields: 2.", "")))
        }
      },
      test("Max fields with fragment") {
        case class A(b: B)
        case class B(c: Int)
        case class Test(a: A)
        val interpreter = (graphQL(RootResolver(Test(A(B(2))))) @@ maxFields(2)).interpreter
        val query       = gqldoc("""
              query test {
                a {
                  ...f
                }
              }

              fragment f on A {
                b {
                  c
                }
              }
              """)
        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.errors == List(ValidationError("Query has too many fields: 3. Max fields: 2.", "")))
        }
      },
      test("Max depth") {
        case class A(b: B)
        case class B(c: Int)
        case class Test(a: A)
        val interpreter = (graphQL(RootResolver(Test(A(B(2))))) @@ maxDepth(2)).interpreter
        val query       = gqldoc("""
              {
                a {
                  b {
                    c
                  }
                }
              }""")
        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.errors == List(ValidationError("Query is too deep: 3. Max depth: 2.", "")))
        }
      },
      test("Timeout") {
        case class Test(a: UIO[Int])

        object schema extends GenericSchema[Any] {
          val interpreter =
            (graphQL(RootResolver(Test(Clock.sleep(2 minutes).as(0)))) @@
              timeout(1 minute)).interpreter
        }

        val query = gqldoc("""
              {
                a
              }""")
        for {
          fiber <- schema.interpreter.flatMap(_.execute(query)).map(_.errors).fork
          _     <- TestClock.adjust(1 minute)
          res   <- fiber.join
        } yield assertTrue(res == List(ExecutionError("""Query was interrupted after timeout of 1 m:

              {
                a
              }""".stripMargin)))
      },
      test("Apollo Tracing") {
        case class Query(hero: Hero)
        case class Hero(name: UIO[String], friends: List[Hero] = Nil)

        object schema extends GenericSchema[Any] {
          implicit lazy val heroSchema: Schema[Any, Hero] = gen

          def api(latch: Promise[Nothing, Unit]) =
            graphQL(
              RootResolver(
                Query(
                  Hero(
                    latch.succeed(()) *> ZIO.sleep(1 second).as("R2-D2"),
                    List(
                      Hero(ZIO.sleep(2 second).as("Luke Skywalker")),
                      Hero(ZIO.sleep(3 second).as("Han Solo")),
                      Hero(ZIO.sleep(4 second).as("Leia Organa"))
                    )
                  )
                )
              )
            ) @@ ApolloTracing.apolloTracing
        }

        val query = gqldoc("""
              {
                hero {
                  name
                  friends {
                    name
                  }
                }
              }""")
        for {
          latch       <- Promise.make[Nothing, Unit]
          interpreter <- schema.api(latch).interpreter
          fiber       <- interpreter.execute(query).map(_.extensions.map(_.toString)).fork
          _           <- latch.await
          _           <- TestClock.adjust(4 seconds)
          result      <- fiber.join
        } yield assert(result)(
          isSome(
            equalTo(
              """{"tracing":{"version":1,"startTime":"1970-01-01T00:00:00.000Z","endTime":"1970-01-01T00:00:04.000Z","duration":4000000000,"parsing":{"startOffset":0,"duration":0},"validation":{"startOffset":0,"duration":0},"execution":{"resolvers":[{"path":["hero","name"],"parentType":"Hero","fieldName":"name","returnType":"String!","startOffset":0,"duration":1000000000},{"path":["hero","friends",0,"name"],"parentType":"Hero","fieldName":"name","returnType":"String!","startOffset":0,"duration":2000000000},{"path":["hero","friends",1,"name"],"parentType":"Hero","fieldName":"name","returnType":"String!","startOffset":0,"duration":3000000000},{"path":["hero"],"parentType":"Query","fieldName":"hero","returnType":"Hero!","startOffset":0,"duration":4000000000},{"path":["hero","friends"],"parentType":"Hero","fieldName":"friends","returnType":"[Hero!]!","startOffset":0,"duration":4000000000},{"path":["hero","friends",2,"name"],"parentType":"Hero","fieldName":"name","returnType":"String!","startOffset":0,"duration":4000000000}]}}}"""
            )
          )
        )
      },
      test("Apollo Caching") {
        case class Query(@GQLCacheControl(maxAge = Some(10.seconds)) hero: Hero)

        @GQLCacheControl(maxAge = Some(2.seconds))
        case class Hero(name: UIO[String], friends: List[Hero] = Nil)

        object schema extends GenericSchema[Any] {
          implicit lazy val heroSchema: Schema[Any, Hero] = gen
          def api                                         =
            graphQL(
              RootResolver(
                Query(
                  Hero(
                    ZIO.succeed("R2-D2"),
                    List(
                      Hero(ZIO.succeed("Luke Skywalker")),
                      Hero(ZIO.succeed("Han Solo")),
                      Hero(ZIO.succeed("Leia Organa"))
                    )
                  )
                )
              )
            ) @@ ApolloCaching.apolloCaching
        }

        val query = gqldoc("""
              {
                hero {
                  name
                  friends {
                    name
                  }
                }
              }""")
        for {
          interpreter <- schema.api.interpreter
          result      <- interpreter.execute(query).map(_.extensions.map(_.toString))
        } yield assert(result)(
          isSome(
            equalTo(
              "{\"cacheControl\":{\"version\":1,\"hints\":[{\"path\":[\"hero\"],\"maxAge\":10,\"scope\":\"PRIVATE\"}]}}"
            )
          )
        )
      },
      suite("Apollo Persisted Queries")(
        test("hash not found") {
          case class Test(test: String)
          val interpreter = (graphQL(RootResolver(Test("ok"))) @@ apolloPersistedQueries).interpreter
          interpreter
            .flatMap(
              _.executeRequest(
                GraphQLRequest(extensions =
                  Some(Map("persistedQuery" -> ObjectValue(Map("sha256Hash" -> StringValue("my-hash")))))
                )
              )
            )
            .map { response =>
              assertTrue(
                response.asJson.noSpaces == """{"data":null,"errors":[{"message":"PersistedQueryNotFound"}]}"""
              )
            }
            .provide(ApolloPersistedQueries.live)
        },
        test("cache poisoning") {
          case class Test(test: String, malicious: String)

          (for {
            interpreter <- (graphQL(RootResolver(Test("ok", "malicious"))) @@ apolloPersistedQueries).interpreter
            // The hash for the query "{test}"  attempting to poison the cache by passing in a different query
            extensions   =
              Some(
                Map(
                  "persistedQuery" -> ObjectValue(
                    Map("sha256Hash" -> StringValue("e005c1d727f7776a57a661d61a182816d8953c0432780beeae35e337830b1746"))
                  )
                )
              )
            r1          <- interpreter.executeRequest(GraphQLRequest(query = Some("{malicious}"), extensions = extensions))
            r2          <- interpreter.executeRequest(GraphQLRequest(extensions = extensions))
          } yield assertTrue(
            r1.asJson.noSpaces == """{"data":null,"errors":[{"message":"Provided sha does not match any query"}]}"""
          ) && assertTrue(r2.asJson.noSpaces == """{"data":null,"errors":[{"message":"PersistedQueryNotFound"}]}"""))
            .provideLayer(ApolloPersistedQueries.live)
        },
        test("hash found") {
          case class Test(test: String)

          (for {
            interpreter <- (graphQL(RootResolver(Test("ok"))) @@ apolloPersistedQueries).interpreter
            extensions   =
              Some(
                Map(
                  "persistedQuery" -> ObjectValue(
                    Map("sha256Hash" -> StringValue("e005c1d727f7776a57a661d61a182816d8953c0432780beeae35e337830b1746"))
                  )
                )
              )
            _           <- interpreter.executeRequest(GraphQLRequest(query = Some("{test}"), extensions = extensions))
            result      <- interpreter.executeRequest(GraphQLRequest(extensions = extensions))
          } yield assertTrue(result.asJson.noSpaces == """{"data":{"test":"ok"}}"""))
            .provide(ApolloPersistedQueries.live)
        }
      ),
      test("custom query directive") {
        val customWrapper        = new ExecutionWrapper[Any] {
          def wrap[R1 <: Any](
            f: ExecutionRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]]
          ): ExecutionRequest => ZIO[R1, Nothing, GraphQLResponse[CalibanError]] =
            request =>
              if (request.field.directives.exists(_.name == "customQueryDirective")) {
                ZIO.succeed {
                  GraphQLResponse(Value.BooleanValue(true), Nil)
                }
              } else f(request)
        }
        val customQueryDirective = __Directive(
          "customQueryDirective",
          None,
          Set(
            __DirectiveLocation.QUERY
          ),
          Nil
        )
        val interpreter          = (graphQL(
          resolver,
          List(
            customQueryDirective
          )
        ) @@ customWrapper).interpreter
        val query                = gqldoc("""
            query @customQueryDirective {
              characters {
                name
              }
            }""")
        interpreter.flatMap(_.execute(query)).map { response =>
          assertTrue(response.data.toString == """true""")
        }
      }
    )
}
