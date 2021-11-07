package caliban.interop.sprayjson

import caliban.CalibanError.ExecutionError
import caliban.{ CalibanError, GraphQLResponse }
import caliban.ResponseValue.ObjectValue
import caliban.Value.{ IntValue, StringValue }
import spray.json._
import zio.test.Assertion.equalTo
import zio.test.assert
import zio.test.environment.TestEnvironment
import zio.test.{ DefaultRunnableSpec, ZSpec }

object GraphQLResponseSprayJsonSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("GraphQLResponseCirceSpec")(
      test("can be converted to JSON [sprayjson]") {
        val response = GraphQLResponse(StringValue("data"), Nil)
        assert(response.toJson)(
          equalTo(JsObject("data" -> JsString("data")))
        )
      },
      test("should include error objects for every error, including extensions [sprayjson]") {
        val errorExtensions = List(
          ("errorCode", StringValue("TEST_ERROR")),
          ("myCustomKey", StringValue("my-value"))
        )

        val response = GraphQLResponse(
          StringValue("data"),
          List(
            ExecutionError(
              "Resolution failed",
              extensions = Some(ObjectValue(errorExtensions))
            )
          )
        )

        assert(response.toJson)(
          equalTo(
            JsObject(
              "data"   -> JsString("data"),
              "errors" -> JsArray(
                JsObject(
                  "message"    -> JsString("Resolution failed"),
                  "extensions" -> JsObject("errorCode" -> JsString("TEST_ERROR"), "myCustomKey" -> JsString("my-value"))
                )
              )
            )
          )
        )
      },
      test("should not include errors element when there are none [sprayjson]") {
        val response = GraphQLResponse(
          StringValue("data"),
          List.empty
        )

        assert(response.toJson)(
          equalTo(
            JsObject(
              "data" -> JsString("data")
            )
          )
        )
      },
      test("can be parsed from JSON [sprayjson]") {
        val req = """{"data":{"value": 42},"errors":[{"message":"boom"}]}"""

        assert(req.parseJson.convertTo[GraphQLResponse[CalibanError]])(
          equalTo(
            GraphQLResponse(
              data = ObjectValue(List("value" -> IntValue(42))),
              errors = List(ExecutionError("boom"))
            )
          )
        )
      }
    )
}
