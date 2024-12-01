package com.github.ephemient.aoc2024.exe

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking

@Suppress("InjectDispatcher")
fun main(vararg args: String): Unit = runBlocking(Dispatchers.Default) {
    mainImpl(args)
}
