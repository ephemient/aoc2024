package com.github.ephemient.aoc2024.exe

import java.io.File

internal actual fun getDayInput(day: Int): String =
    File(System.getenv("AOC2024_DATADIR")?.ifEmpty { null } ?: ".", "day$day.txt").readText()
