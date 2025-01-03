package com.github.ephemient.aoc2024.exe

import kotlin.coroutines.Continuation
import kotlin.coroutines.CoroutineContext
import kotlin.coroutines.EmptyCoroutineContext
import kotlin.coroutines.startCoroutine
import kotlinx.atomicfu.atomic
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job

actual fun <T> runBlockingBenchmark(block: suspend CoroutineScope.() -> T): T {
    val continuation = ContinuationImpl<T>(Dispatchers.Unconfined)
    block.startCoroutine(CoroutineScope(continuation.context), continuation)
    return continuation.getCompleted()
}

private class ContinuationImpl<T>(context: CoroutineContext = EmptyCoroutineContext) : Continuation<T> {
    private val result = atomic<Result<T>?>(null)

    override val context: CoroutineContext = context + Job(context[Job])

    override fun resumeWith(result: Result<T>) {
        check(this.result.compareAndSet(null, result)) { "Already resumed, but proposed with update $result" }
    }

    fun getCompleted(): T = checkNotNull(result.value) { "The job has not completed yet" }.getOrThrow()
}
