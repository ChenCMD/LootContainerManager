package com.github.chencmd.lootcontainermanager.generic


object ThreadExtra {
  /**
  * @see [GiganticMinecraft/SeichiAssist:src/main/scala/com/github/unchama/util/ClassUtils.java](https://github.com/GiganticMinecraft/SeichiAssist/blob/c09c3e6ba2a0f484cd34c240d2644d2ac4b5a999/src/main/scala/com/github/unchama/util/ClassUtils.java)
  */
  def withContextClassLoader[T](loader: ClassLoader)(block: => T): T = {
    val initialThreadClassLoader = Thread.currentThread().getContextClassLoader()
    Thread.currentThread().setContextClassLoader(loader)

    try {
      block
    } finally {
      Thread.currentThread().setContextClassLoader(initialThreadClassLoader)
    }
  }
}
