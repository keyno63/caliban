package caliban

import caliban.ResponseValue.ObjectValue
import caliban.interop.circe._
import spray.json.{ JsValue, RootJsonFormat }

/**
 * Represents the result of a GraphQL query, containing a data object and a list of errors.
 */
case class GraphQLResponse[+E](data: ResponseValue, errors: List[E], extensions: Option[ObjectValue] = None)

object GraphQLResponse extends GraphQLResponseJsonCompat {
  implicit def circeEncoder[F[_]: IsCirceEncoder, E]: F[GraphQLResponse[E]] =
    caliban.interop.circe.json.GraphQLResponseCirce.graphQLResponseEncoder.asInstanceOf[F[GraphQLResponse[E]]]
  implicit def circeDecoder[F[_]: IsCirceDecoder, E]: F[GraphQLResponse[E]] =
    caliban.interop.circe.json.GraphQLResponseCirce.graphQLResponseDecoder.asInstanceOf[F[GraphQLResponse[E]]]

  // TODO: move to scala2
//  implicit object sprayJsonFormat extends RootJsonFormat[GraphQLResponse[Any]] {
//    override def write(response: GraphQLResponse[Any]): JsValue =
//      caliban.interop.sprayjson.json.GraphQLResponseSprayJson.write(response)
//    override def read(value: JsValue): GraphQLResponse[CalibanError] =
//      caliban.interop.sprayjson.json.GraphQLResponseSprayJson.read(value)
//  }

  implicit def sprayJsonFormat[T]: RootJsonFormat[GraphQLResponse[T]] = new RootJsonFormat[GraphQLResponse[T]] {
    override def write(response: GraphQLResponse[T]): JsValue =
      caliban.interop.sprayjson.json.GraphQLResponseSprayJson.write(response)
    override def read(value: JsValue): GraphQLResponse[T]     =
      caliban.interop.sprayjson.json.GraphQLResponseSprayJson.read(value).asInstanceOf[GraphQLResponse[T]]
  }
}
