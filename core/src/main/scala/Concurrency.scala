package org.tiramisu

import concurrent.ExecutionContext
import java.util.concurrent.Executors

trait Concurrency {
  private val executionService = Executors.newCachedThreadPool()
  val executionContext = ExecutionContext.fromExecutorService(executionService)
}
