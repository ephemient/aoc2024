package com.github.ephemient.aoc2024.exe

private fun argv(): JsArray<JsString> = js("process.argv")

suspend fun main() {
    val argv = argv()
    mainImpl(Array(argv.length - 2) { argv[it + 2].toString() })
}
