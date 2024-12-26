package com.github.ephemient.aoc2024

expect val days: List<Day>

data class Day(
    val day: Int,
    val parts: Int,
    val solver: (String) -> List<suspend () -> Any?>,
    val name: String = day.toString(),
)

fun <T> Day(
    day: Int,
    create: (String) -> T,
    vararg parts: suspend (T) -> Any?,
    name: String = day.toString(),
): Day = Day(
    day = day,
    parts = parts.size,
    solver = { with(create(it)) { parts.map { suspend { it.invoke(this) } } } },
    name = name,
)
