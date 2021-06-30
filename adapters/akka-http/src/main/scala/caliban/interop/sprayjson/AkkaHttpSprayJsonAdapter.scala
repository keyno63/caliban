package caliban.interop.sprayjson

import caliban.AkkaHttpAdapter

trait AkkaHttpSprayJsonAdapter {
  val adapter: AkkaHttpAdapter = AkkaHttpAdapter(new SprayJsonBackend)

}
