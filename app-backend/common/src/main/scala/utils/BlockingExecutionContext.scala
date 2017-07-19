package com.azavea.rf.common.utils

import scala.concurrent.ExecutionContext

/** A type which represents a blocking execution context, should be passed to all blocking Futures */
case class BlockingExecutionContext(executionContext: ExecutionContext)

object BlockingExecutionContext {
  implicit def toExecutionContext(ec: BlockingExecutionContext): ExecutionContext = ec.executionContext
}
