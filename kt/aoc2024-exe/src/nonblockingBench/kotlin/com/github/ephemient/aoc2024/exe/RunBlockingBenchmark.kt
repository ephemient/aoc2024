package com.github.ephemient.aoc2024.exe

import kotlin.coroutines.Continuation
import kotlin.coroutines.CoroutineContext
import kotlin.coroutines.EmptyCoroutineContext
import kotlin.coroutines.startCoroutine
import kotlinx.coroutines.CompletableDeferred
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Deferred
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.ExperimentalCoroutinesApi
import kotlinx.coroutines.InternalForInheritanceCoroutinesApi
import kotlinx.coroutines.Job
import kotlinx.coroutines.completeWith

actual fun <T> runBlockingBenchmark(block: suspend CoroutineScope.() -> T): T {
    val deferred = CompletableContinuation<T>(Dispatchers.Unconfined)
    block.startCoroutine(CoroutineScope(deferred.context), deferred)
    @OptIn(ExperimentalCoroutinesApi::class)
    return deferred.getCompleted()
}

@OptIn(InternalForInheritanceCoroutinesApi::class)
private class CompletableContinuation<T> private constructor(
    context: CoroutineContext,
    private val deferred: CompletableDeferred<T>,
) : Continuation<T>, Deferred<T> by deferred {
    constructor(context: CoroutineContext = EmptyCoroutineContext) : this(context, CompletableDeferred(context[Job]))

    override val context: CoroutineContext = context + deferred
    override fun resumeWith(result: Result<T>) = check(deferred.completeWith(result)) { "Already resumed" }
}
