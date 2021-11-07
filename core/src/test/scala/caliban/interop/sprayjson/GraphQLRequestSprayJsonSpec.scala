package caliban.interop.sprayjson

import caliban.{ GraphQLRequest, Value }
import spray.json.{ enrichAny, JsObject, JsString }
import zio.test.Assertion.equalTo
import zio.test.environment.TestEnvironment
import zio.test.{ assert, DefaultRunnableSpec, ZSpec }

object GraphQLRequestSprayJsonSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("GraphQLRequestSprayJsonSpec")(
      test("can be parsed from JSON by sprayjson") {
        val request = JsObject(
          Map(
            "query"         -> JsString("{}"),
            "operationName" -> JsString("op"),
            "variables"     -> JsObject.empty
          )
        )
        assert(request.convertTo[GraphQLRequest])(
          equalTo(GraphQLRequest(query = Some("{}"), operationName = Some("op"), variables = Some(Map.empty)))
        )
      },
      test("can encode to JSON by sprayjson") {
        val res = GraphQLRequest(
          query = Some("{}"),
          operationName = Some("op"),
          variables = Some(
            Map(
              "hello"     -> Value.StringValue("world"),
              "answer"    -> Value.IntValue(42),
              "isAwesome" -> Value.BooleanValue(true),
              "name"      -> Value.NullValue
            )
          )
        )
        assert(res.toJson.toString())(
          equalTo(
            """{"query":"{}","operationName":"op","variables":{"hello":"world","answer":42,"isAwesome":true,"name":null},"extensions":null}"""
          )
        )
      }
    )
}
