package com.github.ephemient.aoc2024.exe

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking

actual fun <T> runBlockingBenchmark(block: suspend CoroutineScope.() -> T): T = runBlocking(Dispatchers.Default, block)
