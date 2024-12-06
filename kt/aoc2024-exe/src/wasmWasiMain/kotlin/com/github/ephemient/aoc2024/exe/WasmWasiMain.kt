package com.github.ephemient.aoc2024.exe

suspend fun main() {
    val argv = argv()
    mainImpl(argv.copyOfRange(2, argv.size))
}
