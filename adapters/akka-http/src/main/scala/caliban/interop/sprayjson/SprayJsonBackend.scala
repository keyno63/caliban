package caliban.interop.sprayjson

import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import caliban.{ GraphQLRequest, GraphQLResponse, JsonBackend, ResponseValue, WSMessage }
import spray.json._
import caliban.interop.sprayjson.json._


class SprayJsonBackend extends JsonBackend {

  //case class GraphQL()
  def parseHttpRequest(
    query: Option[String],
    op: Option[String],
    vars: Option[String],
    exts: Option[String]
  ): Either[Throwable, GraphQLRequest] =
  {
    // TODO: ctrl failed to parse
    val variablesJs: Option[JsValue] = vars.map(_.parseJson)
    val extensionsJs: Option[JsValue] = exts.map(_.parseJson)
    val fields: Option[JsValue] = query.map {
      js => {
        val newJs = js.parseJson // TODO: use TRY and handle to parse error
        val mapp = Map("query" -> newJs)
        val mapo = op.map(o => "operationName" -> o.parseJson)
        val mapv = variablesJs.map(js => "variables" -> js)
        val mape = extensionsJs.map(js => "extensions" -> js)
        JsObject(
          mapp ++ mapo ++ mapv ++ mape
        )
      }
    }
    fields
      .map(_.convertTo[GraphQLRequest]) flatMap (Right(_))
  }


  def encodeGraphQLResponse(r: GraphQLResponse[Any]): String = {
    r.toJson.toString()
  }

  def parseWSMessage(text: String): Either[Throwable, WSMessage]                    = ???
//  {
//    text.toJson
//  }
  def encodeWSResponse[E](id: String, data: ResponseValue, errors: List[E]): String = ???
  def encodeWSError(id: String, error: String): String                              = ???

  def reqUnmarshaller: FromEntityUnmarshaller[GraphQLRequest] = implicitly

}
