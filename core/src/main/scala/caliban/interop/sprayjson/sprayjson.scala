package caliban.interop.sprayjson

import caliban.InputValue.{ ListValue, ObjectValue, VariableValue }
import caliban.ResponseValue.{ sprayJsonResponseFormat, StreamValue }
import caliban.Value.FloatValue.{ BigDecimalNumber, DoubleNumber, FloatNumber }
import caliban.Value.IntValue.{ BigIntNumber, IntNumber, LongNumber }
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, InputValue, ResponseValue, Value }
import caliban.Value.{ BooleanValue, EnumValue, FloatValue, IntValue, NullValue, StringValue }
import caliban.parsing.adt.LocationInfo
import spray.json._

private[caliban] trait IsSprayJsonEncoder[F[_]]
private[caliban] object IsSprayJsonEncoder {
  implicit val isSprayJsonEncoder: IsSprayJsonEncoder[RootJsonFormat] = null
}

object json {

  private[sprayjson] implicit class extJsObject(v: JsValue) {
    def dropNullValues: JsValue = v match {
      case JsObject(fields) => JsObject(fields.filter { case (_, v) => v != JsNull })
      case JsArray(fields)  => JsArray(fields.filter(v => v != JsNull))
      case _                => throw new Exception(s"unmatched JsValue type. ${v.toString()}, ${v.getClass}")
    }
  }

  private[sprayjson] def jsonToResponseValue(json: JsValue): ResponseValue = json match {
    case JsNull          => NullValue
    case JsNumber(v)     =>
      if (v.isWhole()) {
        if (v.isValidInt) IntNumber(v.toInt)
        else if (v.isValidLong) LongNumber(v.toLong)
        else BigIntNumber(v.toBigInt())
      } else {
        if (v.isDecimalFloat) FloatNumber(v.toFloat)
        else if (v.isDecimalDouble) DoubleNumber(v.toDouble)
        else BigDecimalNumber(v)
      }
    case JsString(v)     => StringValue(v)
    case JsTrue          => BooleanValue(true)
    case JsFalse         => BooleanValue(false)
    case JsArray(v)      => ResponseValue.ListValue(v.toList.map(jsonToResponseValue))
    case JsObject(value) =>
      ResponseValue.ObjectValue(
        value.toList.map { case (k, v) => k -> jsonToResponseValue(v) }
      )
    case _               => NullValue
  }

  private[caliban] object ValueSprayJson extends DefaultJsonProtocol {

    import caliban.InputValue.sprayJsonInputFormat

    def write(obj: InputValue): JsValue = obj match {
      case NullValue                                   => JsNull
      case v: IntValue                                 =>
        v match {
          case IntValue.IntNumber(value)    => JsNumber(value)
          case IntValue.LongNumber(value)   => JsNumber(value)
          case IntValue.BigIntNumber(value) => JsNumber(value)
        }
      case v: FloatValue                               =>
        v match {
          case FloatValue.FloatNumber(value)      => JsNumber(value)
          case FloatValue.DoubleNumber(value)     => JsNumber(value)
          case FloatValue.BigDecimalNumber(value) => JsNumber(value)
        }
      case StringValue(value)                          => JsString(value)
      case BooleanValue(value)                         => JsBoolean(value)
      case EnumValue(value)                            => JsString(value)
      case ListValue(value: List[InputValue])          => value.toJson
      case ObjectValue(value: Map[String, InputValue]) => value.toJson
      case VariableValue(value)                        => JsString(value)
    }

    def read(json: JsValue): InputValue = json match {
      case JsNull          => NullValue
      case JsNumber(v)     =>
        if (v.isWhole()) {
          if (v.isValidInt) IntNumber(v.toInt)
          else if (v.isValidLong) LongNumber(v.toLong)
          else BigIntNumber(v.toBigInt())
        } else {
          if (v.isDecimalFloat) FloatNumber(v.toFloat)
          else if (v.isDecimalDouble) DoubleNumber(v.toDouble)
          else BigDecimalNumber(v)
        }
      case JsString(v)     => StringValue(v)
      case JsTrue          => BooleanValue(true)
      case JsFalse         => BooleanValue(false)
      case JsArray(v)      => ListValue(v.toList.map(read))
      case JsObject(value) =>
        ObjectValue(
          value.map { case (k, v) =>
            (k, read(v))
          }
        )
      case _               => NullValue
    }
  }

  private def convertJsNumber(v: Value): JsNumber =
    v match {
      case IntValue.IntNumber(value)          => JsNumber(value)
      case IntValue.LongNumber(value)         => JsNumber(value)
      case IntValue.BigIntNumber(value)       => JsNumber(value)
      case FloatValue.FloatNumber(value)      => JsNumber(value)
      case FloatValue.DoubleNumber(value)     => JsNumber(value)
      case FloatValue.BigDecimalNumber(value) => JsNumber(value)
      case _                                  => throw new Exception("")
    }

  private[caliban] object ResponseValueSprayJson extends DefaultJsonProtocol {
    def write(response: ResponseValue): JsValue = response match {
      case NullValue                                                       => JsNull
      case v: IntValue                                                     =>
        v match {
          case IntValue.IntNumber(value)    => JsNumber(value)
          case IntValue.LongNumber(value)   => JsNumber(value)
          case IntValue.BigIntNumber(value) => JsNumber(value)
        }
      case v: FloatValue                                                   =>
        v match {
          case FloatValue.FloatNumber(value)      => JsNumber(value)
          case FloatValue.DoubleNumber(value)     => JsNumber(value)
          case FloatValue.BigDecimalNumber(value) => JsNumber(value)
        }
      case StringValue(value)                                              => JsString(value)
      case BooleanValue(value)                                             => JsBoolean(value)
      case EnumValue(value)                                                => JsString(value)
      case ResponseValue.ListValue(value: List[ResponseValue])             => value.toJson
      case ResponseValue.ObjectValue(value: List[(String, ResponseValue)]) =>
        JsObject(
          value.foldLeft(Map.empty[String, JsValue])((p, a) => p ++ Map(a._1 -> a._2.toJson))
        )
      case sv: StreamValue                                                 => JsString(sv.toString)
    }
    // TODO: fix common function
    def read(json: JsValue): ResponseValue      = json match {
      case JsNull          => NullValue
      case JsNumber(v)     =>
        if (v.isWhole()) {
          if (v.isValidInt) IntNumber(v.toInt)
          else if (v.isValidLong) LongNumber(v.toLong)
          else BigIntNumber(v.toBigInt())
        } else {
          if (v.isDecimalFloat) FloatNumber(v.toFloat)
          else if (v.isDecimalDouble) DoubleNumber(v.toDouble)
          else BigDecimalNumber(v)
        }
      case JsString(v)     => StringValue(v)
      case JsTrue          => BooleanValue(true)
      case JsFalse         => BooleanValue(false)
      case JsArray(v)      => ResponseValue.ListValue(v.toList.map(read))
      case JsObject(value) =>
        ResponseValue.ObjectValue(
          value.toList.map { case (k, v) => k -> read(v) }
        )
      case _               => NullValue
    }
  }

  private[caliban] object GraphQLRequestSprayJson extends CalibanSprayJson[GraphQLRequest] {
    override def write(request: GraphQLRequest): JsValue = JsObject(
      "query"         -> JsString(request.query.getOrElse("")),
      "operationName" -> JsString(request.operationName.getOrElse("")),
      "variables"     -> request.variables.toJson,
      "extensions"    -> request.extensions.toJson
    )

    override def read(json: JsValue): GraphQLRequest =
      json match {
        case JsObject(m) =>
          val query: Option[String]                       = m.get("query").map(_.convertTo[String])
          val operationName: Option[String]               = m.get("operationName").map(_.convertTo[String])
          val variables: Option[Map[String, InputValue]]  = m.get("variables").map(_.convertTo[Map[String, InputValue]])
          val extensions: Option[Map[String, InputValue]] =
            m.get("extensions").map(_.convertTo[Map[String, InputValue]])
          GraphQLRequest(
            query = query,
            operationName = operationName,
            variables = variables,
            extensions = extensions
          )
        case v @ _       => throw new Exception(s"failed to parse json value, value =[$v].")
      }
  }

  private[caliban] object GraphQLResponseSprayJson extends DefaultJsonProtocol {
    def write(obj: GraphQLResponse[Any]): JsValue = obj match {
      case GraphQLResponse(data, Nil, None)                =>
        JsObject(
          "data" -> data.toJson
        )
      case GraphQLResponse(data, Nil, Some(extensions))    =>
        JsObject(
          "data"       -> data.toJson,
          "extensions" -> extensions.asInstanceOf[ResponseValue].toJson
        )
      case GraphQLResponse(data, errors, None)             =>
        JsObject(
          "data"   -> data.toJson,
          "errors" -> errors.map(handleError).toJson
        )
      case GraphQLResponse(data, errors, Some(extensions)) =>
        JsObject(
          "data"       -> data.toJson,
          "extensions" -> extensions.asInstanceOf[ResponseValue].toJson,
          "errors"     -> errors.map(handleError).toJson
        )
    }

    def read(json: JsValue): GraphQLResponse[CalibanError] = json match {
      case JsObject(m) =>
        val data   = m.get("data").map(_.convertTo[ResponseValue])
        val errors = m.get("errors").map(_.convertTo[List[CalibanError]])
        GraphQLResponse[CalibanError](
          data = data.get,
          errors = errors.getOrElse(List()),
          extensions = None
        )
      case v @ _       => throw new Exception(s"failed to parse json value, value =[$v], class ${v.getClass}.")
    }
  }

  private[caliban] implicit object LocationInfoReader extends DefaultJsonProtocol {
//    implicit val formatLocationInfo: RootJsonFormat[LocationInfo] = jsonFormat2(LocationInfo.apply)
//    implicit val formatLocationInfo: RootJsonFormat[LocationInfo] = jsonFormat(LocationInfo.apply, "column", "line")
    def write(li: LocationInfo): JsValue  = li match {
      case LocationInfo(a, b) => JsObject("column" -> JsNumber(a), "line" -> JsNumber(b))
    }
    def read(json: JsValue): LocationInfo = json match {
      case JsObject(m) =>
        val ret = for {
          data   <- m.get("column").map(_.convertTo[Int])
          errors <- m.get("line").map(_.convertTo[Int])
        } yield LocationInfo(data, errors)
        ret.get
      case v @ _       => throw new Exception(s"failed to parse json value, value =[$v], class ${v.getClass}.")
    }
  }

  implicit def sprayJsonLocalInfoFormat: RootJsonFormat[LocationInfo] = new RootJsonFormat[LocationInfo] {
    def write(li: LocationInfo): JsValue  =
      caliban.interop.sprayjson.json.LocationInfoReader.write(li)
    def read(json: JsValue): LocationInfo =
      caliban.interop.sprayjson.json.LocationInfoReader.read(json)
  }

  private[caliban] object ObjectValueReader extends DefaultJsonProtocol {
    def write(o: ResponseValue.ObjectValue): JsValue   =
      JsObject(
        o.fields.foldLeft(Map.empty[String, JsValue])((p, a) => p ++ Map(a._1 -> a._2.toJson))
      )
    def read(json: JsValue): ResponseValue.ObjectValue = json match {
      case JsObject(fields) =>
        ResponseValue.ObjectValue(
          fields.toList.map { case (k, v) => k -> jsonToResponseValue(v) }
        )
      case _                => throw new Exception(s"failed to parse json value, value =[$json], ${json.getClass}")
    }
  }

  implicit def sprayJsonObjectValueFormat: RootJsonFormat[ResponseValue.ObjectValue] =
    new RootJsonFormat[ResponseValue.ObjectValue] {
      override def read(json: JsValue): ResponseValue.ObjectValue =
        ObjectValueReader.read(json)
      override def write(obj: ResponseValue.ObjectValue): JsValue =
        ObjectValueReader.write(obj)
    }

  private[caliban] object ErrorValueSprayJson extends DefaultJsonProtocol {
    private def locationToJson(li: LocationInfo): JsValue =
      JsObject(
        "line"   -> li.line.toJson,
        "column" -> li.column.toJson
      )

    def write(ce: CalibanError): JsValue  = ce match {
      case CalibanError.ParsingError(msg, locationInfo, _, extensions)         =>
        JsObject(
          "message"    -> s"Parsing Error: $msg".toJson,
          "locations"  -> Some(locationInfo).collect { case Some(li) =>
            JsArray(locationToJson(li))
          }.toJson,
          "extensions" -> (extensions: Option[ResponseValue]).toJson.dropNullValues
        ).dropNullValues
      case CalibanError.ValidationError(msg, _, locationInfo, extensions)      =>
        JsObject(
          "message"    -> msg.toJson,
          "locations"  -> Some(locationInfo).collect { case Some(li) =>
            JsArray(locationToJson(li))
          }.toJson,
          "extensions" -> (extensions: Option[ResponseValue]).toJson.dropNullValues
        ).dropNullValues
      case CalibanError.ExecutionError(msg, path, locationInfo, _, extensions) =>
        JsObject(
          "message"    -> msg.toJson,
          "locations"  -> Some(locationInfo).collect { case Some(li) =>
            JsArray(locationToJson(li))
          }.toJson,
          "path"       -> Some(path).collect {
            case p if p.nonEmpty =>
              p.map {
                case Left(value)  => value.toJson
                case Right(value) => value.toJson
              }
          }.toJson,
          "extensions" -> (extensions: Option[ResponseValue]).toJson.dropNullValues
        ).dropNullValues
    }
    def read(json: JsValue): CalibanError = json match {
      case JsObject(m) =>
        val message    = m.get("message").map(_.convertTo[String])
        val path       = m.get("path").map(_.convertTo[List[Either[String, Int]]])
        val locations  = m.get("locations").map(_.convertTo[List[LocationInfo]]).map(_.head)
        val extensions = m.get("extensions").map(_.convertTo[ResponseValue.ObjectValue])
        CalibanError.ExecutionError(
          message.get,
          path.getOrElse(Nil),
          locations,
          extensions = extensions
        )
      case v @ _       => throw new Exception(s"failed to parse json value, value =[$v].")
    }
  }

  private def handleError(err: Any): JsValue =
    err match {
      case ce: CalibanError => ce.toJson
      case _                =>
        JsObject(
          "message" -> JsString(err.toString)
        )
    }
}

private[caliban] trait CalibanSprayJson[T] extends DefaultJsonProtocol {
  def write(obj: T): JsValue
  def read(json: JsValue): T
}
