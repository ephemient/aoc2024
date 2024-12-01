package com.github.ephemient.aoc2024.exe

import js.globals.globalThis
import js.objects.Record
import node.buffer.BufferEncoding
import node.fs.readFileSync

internal actual fun getDayInput(day: Int): String {
    val dataDir = globalThis.asDynamic()
        .process.env.unsafeCast<Record<String, String>>()["AOC2024_DATADIR"]
        ?.ifEmpty { null } ?: "."
    return readFileSync("$dataDir/day$day.txt", BufferEncoding.utf8)
}
