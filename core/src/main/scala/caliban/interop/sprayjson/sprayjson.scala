package caliban.interop.sprayjson

import caliban.InputValue.{ListValue, ObjectValue, VariableValue}
import caliban.Value.FloatValue.{BigDecimalNumber, DoubleNumber, FloatNumber}
import caliban.Value.IntValue.{BigIntNumber, IntNumber, LongNumber}
import caliban.{CalibanError, GraphQLRequest, GraphQLResponse, InputValue, ResponseValue, Value}
import caliban.Value.{BooleanValue, EnumValue, FloatValue, IntValue, NullValue, StringValue}
import spray.json._

private [caliban] trait IsSprayJsonEncoder[F[_]]
private [caliban] object IsSprayJsonEncoder {
  implicit val isSprayJsonEncoder: IsSprayJsonEncoder[RootJsonFormat] = null
}

object json {
  private[caliban] object ValueSprayJson extends DefaultJsonProtocol {

    import caliban.InputValue.sprayJsonInputFormat
    //val calibanSprayJsonWrite: RootJsonFormat[InputValue] = (obj: InputValue) => calibanSprayJsonWrite2(obj)

    def write(obj: InputValue): JsValue = obj match {
      case NullValue => JsNull
      case v: IntValue =>
        v match {
          case IntValue.IntNumber(value) => JsNumber(value)
          case IntValue.LongNumber(value) => JsNumber(value)
          case IntValue.BigIntNumber(value) => JsNumber(value)
        }
      case v: FloatValue =>
        v match {
          case FloatValue.FloatNumber(value) => JsNumber(value)
          case FloatValue.DoubleNumber(value) => JsNumber(value)
          case FloatValue.BigDecimalNumber(value) => JsNumber(value)
        }
      case StringValue(value) => JsString(value)
      case BooleanValue(value) => JsBoolean(value)
      case EnumValue(value) => JsString(value)
      case ListValue(value: List[InputValue]) => value.toJson
      case ObjectValue(value: Map[String, InputValue]) => value.toJson
      case VariableValue(value) => JsString(value)
    }

    def read(json: JsValue): InputValue = json match {
      case JsNull => NullValue
      case JsNumber(v) =>
        if (v.isWhole()) {
          if (v.isValidInt) IntNumber(v.toInt)
          else if (v.isValidLong) LongNumber(v.toLong)
          else BigIntNumber(v.toBigInt())
        } else {
          if (v.isDecimalFloat) FloatNumber(v.toFloat)
          else if (v.isDecimalDouble) DoubleNumber(v.toDouble)
          else BigDecimalNumber(v)
        }
    }
  }

  private def convertJsNumber(v: Value): JsNumber = {
    v match {
      case IntValue.IntNumber(value) => JsNumber(value)
      case IntValue.LongNumber(value) => JsNumber(value)
      case IntValue.BigIntNumber(value) => JsNumber(value)
      case FloatValue.FloatNumber(value) => JsNumber(value)
      case FloatValue.DoubleNumber(value) => JsNumber(value)
      case FloatValue.BigDecimalNumber(value) => JsNumber(value)
      case _ => throw new Exception("")
    }
  }

  private[caliban] object GraphQLRequestSprayJson extends CalibanSprayJson[GraphQLRequest] {
    override def write(request: GraphQLRequest): JsValue = JsObject(
      Map(
        "query" -> JsString(request.query.getOrElse("")),
        "operationName" -> JsString(request.operationName.getOrElse("")),
        "variables" -> request.variables.getOrElse(Map.empty).toJson,
        "extensions" -> request.extensions.getOrElse(Map.empty[String, InputValue]).toJson
      )
    )

    override def read(json: JsValue): GraphQLRequest =
      json match {
        case JsObject(m) => {
          val query: Option[String] = m.get("query").map(_.convertTo[String])
          val operationName: Option[String] = m.get("operationName").map(_.convertTo[String])
          val variables: Option[Map[String, InputValue]] = m.get("variables").map(_.convertTo[Map[String, InputValue]])
          val extensions: Option[Map[String, InputValue]] = m.get("extensions").map(_.convertTo[Map[String, InputValue]])
          GraphQLRequest(
            query = query,
            operationName = operationName,
            variables = variables,
            extensions = extensions
          )
        }
        case v @ _ => throw new Exception(s"failed to parse json value, value =[$v].")
      }
  }

  private[caliban] object GraphQLResponseSprayJson {
    def write(obj: GraphQLResponse[Any]): JsValue = obj match {
      case GraphQLResponse(data, Nil, None) => ???
      case GraphQLResponse(data, Nil, Some(extensions)) => ???
      case GraphQLResponse(data, errors, None) => ???
      case GraphQLResponse(data, errors, Some(extensions)) => ???
    }

    def read(json: JsValue): GraphQLResponse[CalibanError] = json match {
      case JsObject(m) => {
        val a: Option[ResponseValue] = ??? //m.get("data")
      }
    }

  }
}

private[caliban] trait CalibanSprayJson[T] extends DefaultJsonProtocol {
  def write(obj: T): JsValue
  def read(json: JsValue): T
}
