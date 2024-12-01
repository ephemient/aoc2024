@file:JvmName("Main")

package com.github.ephemient.aoc2024.exe

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext

@Suppress("InjectDispatcher")
suspend fun main(vararg args: String): Unit = withContext(Dispatchers.Default) {
    mainImpl(args)
}
