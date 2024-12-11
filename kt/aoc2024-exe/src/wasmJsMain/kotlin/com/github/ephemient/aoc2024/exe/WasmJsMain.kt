package com.github.ephemient.aoc2024.exe

@JsModule("node:process")
private external val argv: JsArray<JsString>

suspend fun main() {
    val argv = argv
    mainImpl(Array(argv.length) { argv[it].toString() })
}
