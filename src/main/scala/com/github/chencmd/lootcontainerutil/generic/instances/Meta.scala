package com.github.chencmd.lootcontainerutil.generic.instances

import doobie.Meta
import doobie.enumerated.JdbcType
import java.util.UUID

object MetaInstances {
  given Meta[UUID] = Meta.Basic.one[UUID](
    JdbcType.Char,
    List.empty,
    (rs, i) => UUID.fromString(rs.getString(i)),
    (ps, i, a) => ps.setString(i, a.toString),
    (rs, i, a) => rs.updateString(i, a.toString),
  )
}
