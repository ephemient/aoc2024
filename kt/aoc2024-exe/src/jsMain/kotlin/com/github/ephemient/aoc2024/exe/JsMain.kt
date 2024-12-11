package com.github.ephemient.aoc2024.exe

import js.globals.globalThis

suspend fun main() {
    mainImpl(globalThis.asDynamic().process.argv.unsafeCast<Array<String>>())
}
