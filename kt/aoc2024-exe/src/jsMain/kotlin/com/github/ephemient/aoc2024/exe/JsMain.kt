package com.github.ephemient.aoc2024.exe

import js.globals.globalThis

suspend fun main() {
    mainImpl(with(globalThis.asDynamic().process.argv.unsafeCast<Array<String>>()) { copyOfRange(2, size) })
}
