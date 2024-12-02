package com.github.ephemient.aoc2024.exe

private fun getenv(name: String): String = js("process.env[name] ?? ''")

internal actual fun getDayInput(day: Int): String {
    val dataDir = getenv("AOC2024_DATADIR").ifEmpty { null } ?: "."
    return readFileSync("$dataDir/day$day.txt".toJsString(), "utf8".toJsString()).toString()
}
