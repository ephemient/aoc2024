package com.github.ephemient.aoc2024.exe

import com.github.ephemient.aoc2024.days

internal suspend fun mainImpl(args: Array<out String>) {
    for (day in days.filter { it.name in args }.ifEmpty { days }) {
        println("Day ${day.name}")
        day.solver(getDayInput(day.day)).forEach { println(it()) }
        println()
    }
}
