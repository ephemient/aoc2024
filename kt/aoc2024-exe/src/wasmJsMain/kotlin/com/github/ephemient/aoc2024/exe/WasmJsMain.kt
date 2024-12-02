package com.github.ephemient.aoc2024.exe

private fun argv(): String = js("process.argv.join(' ')")

suspend fun main() {
    mainImpl(argv().split(' ').drop(2).toTypedArray())
}
