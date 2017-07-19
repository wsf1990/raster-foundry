package com.azavea.rf.common

import scala.concurrent.ExecutionContext

package object utils {
  type ComputationContext[T] = ExecutionContext => BlockingExecutionContext => T

  /**
    * Function which is a bridge between our internal types
    * and Futures we want to be created in terms of the BlockingExecution context.
    * */
  def withComputationContext[T](code: ComputationContext[T])(implicit bec: BlockingExecutionContext): T =
    code(bec)(bec)
}
