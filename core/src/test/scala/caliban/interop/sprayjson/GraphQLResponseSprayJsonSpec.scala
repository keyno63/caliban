package caliban.interop.sprayjson

import caliban.CalibanError.ExecutionError
import caliban.{ CalibanError, GraphQLResponse }
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ IntValue, StringValue }
import caliban.parsing.adt.LocationInfo
import spray.json._
import zio.test.Assertion.equalTo
import zio.test.assert
import zio.test.environment.TestEnvironment
import zio.test.{ DefaultRunnableSpec, ZSpec }

object GraphQLResponseSprayJsonSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("GraphQLResponseSprayJsonSpec")(
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
              locationInfo = Some(LocationInfo(1, 2)),
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
                  "locations"  -> JsArray(JsObject("column" -> JsNumber(1), "line" -> JsNumber(2))),
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
        val req =
          """ |{
            |   "data":{"value": 42},
            |   "errors":[
            |     {
            |       "message":"boom",
            |       "path": ["step", 0],
            |       "locations": [{"column": 1, "line": 2}],
            |       "extensions": {
            |         "argumentName": "id",
            |         "code": "BAD_USER_INPUT",
            |         "exception": {
            |           "stacktrace": [
            |              "trace"
            |           ]
            |         }
            |       }
            |     }]
            |}""".stripMargin

        val x = req.parseJson.convertTo[GraphQLResponse[CalibanError]]
        val y = GraphQLResponse(
          data = ObjectValue(List("value" -> IntValue(42))),
          errors = List(
            ExecutionError(
              "boom",
              path = List(Left("step"), Right(0)),
              locationInfo = Some(LocationInfo(1, 2)),
              extensions = Some(
                ObjectValue(
                  List(
                    "argumentName" -> StringValue("id"),
                    "code"         -> StringValue("BAD_USER_INPUT"),
                    "exception"    -> ObjectValue(
                      List("stacktrace" -> ListValue(List(StringValue("trace"))))
                    )
                  )
                )
              )
            )
          )
        )

        assert(req.parseJson.convertTo[GraphQLResponse[CalibanError]])(
          equalTo(
            GraphQLResponse(
              data = ObjectValue(List("value" -> IntValue(42))),
              errors = List(
                ExecutionError(
                  "boom",
                  path = List(Left("step"), Right(0)),
                  locationInfo = Some(LocationInfo(1, 2)),
                  extensions = Some(
                    ObjectValue(
                      List(
                        "argumentName" -> StringValue("id"),
                        "code"         -> StringValue("BAD_USER_INPUT"),
                        "exception"    -> ObjectValue(
                          List("stacktrace" -> ListValue(List(StringValue("trace"))))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      }
    )
}
