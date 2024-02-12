package com.github.chencmd.lootcontainerutil

import com.github.chencmd.lootcontainerutil.nbt.definition.NBTPath
import com.github.chencmd.lootcontainerutil.nbt.definition.NBTPathInterpolation
package object terms {
  type ItemMapper = (NBTPath, List[NBTPathInterpolation])
}
