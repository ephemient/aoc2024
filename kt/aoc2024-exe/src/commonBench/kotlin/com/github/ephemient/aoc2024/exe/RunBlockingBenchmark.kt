package com.github.ephemient.aoc2024.exe

import kotlinx.coroutines.CoroutineScope

expect fun <T> runBlockingBenchmark(block: suspend CoroutineScope.() -> T): T
