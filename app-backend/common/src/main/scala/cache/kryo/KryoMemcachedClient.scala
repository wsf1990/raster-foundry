package com.azavea.rf.common.cache.kryo

import java.net.InetSocketAddress

import com.azavea.rf.common.Config

import scala.collection.JavaConverters._

import net.spy.memcached.transcoders.Transcoder
import net.spy.memcached._

/** Extends Memcached client configuration object to provide custom (kryo) transcoder. */
class KryoConnectionFactory extends DefaultConnectionFactory {
  override def getDefaultTranscoder: Transcoder[AnyRef] = {
    new KryoTranscoder
  }
}

/** Extends the standard [net.spy.MemcachedClient] to syntactically sweeten client creation */
class KryoMemcachedClient(addrs: InetSocketAddress*)
  extends MemcachedClient(new KryoConnectionFactory, addrs.toList.asJava)

object KryoMemcachedClient {
  def apply(addrs: InetSocketAddress): KryoMemcachedClient =
    new KryoMemcachedClient(addrs)

  def DEFAULT: KryoMemcachedClient =
    KryoMemcachedClient(new InetSocketAddress(Config.memcached.host, Config.memcached.port))
}

