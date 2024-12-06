package com.github.ephemient.aoc2024.exe

internal actual fun getDayInput(day: Int): String = readFile("/data/day$day.txt").decodeToString()
