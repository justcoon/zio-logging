package zio.logging

import zio.prelude.Newtype

object types {
  object ClientId extends Newtype[String]
  type ClientId = ClientId.Type
}

object TestLogAnnotation {

//  val ClientId: LogAnnotation[types.ClientId] =
//    LogAnnotation[types.ClientId](
//      name = "client_id",
//      combine = (_: types.ClientId, r: types.ClientId) => r,
//      render = types.ClientId.unwrap
//    )
}
