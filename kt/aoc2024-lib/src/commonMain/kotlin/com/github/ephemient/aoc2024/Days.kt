package com.github.ephemient.aoc2024

val days: List<Day> = listOf(
    Day(1, ::Day1, Day1::part1, Day1::part2),
    Day(2, ::Day2, Day2::part1, Day2::part2),
    Day(3, ::Day3, Day3::part1, Day3::part2),
    Day(4, ::Day4, Day4::part1, Day4::part2),
    Day(5, ::Day5, Day5::part1, Day5::part2),
    Day(6, ::Day6, Day6::part1, Day6::part2),
    Day(7, ::Day7, Day7::part1, Day7::part2),
    Day(8, ::Day8, Day8::part1, Day8::part2),
    Day(9, ::Day9, Day9::part1, Day9::part2),
    Day(10, ::Day10, Day10::part1, Day10::part2),
    Day(11, ::Day11, Day11::part1, Day11::part2),
)

data class Day(
    val day: Int,
    val parts: Int,
    val solver: (String) -> List<suspend () -> Any?>,
    val name: String = day.toString(),
    val skipByDefault: Boolean = false,
)

fun <T> Day(
    day: Int,
    create: (String) -> T,
    vararg parts: suspend (T) -> Any?,
    name: String = day.toString(),
    skipByDefault: Boolean = false,
): Day = Day(
    day = day,
    parts = parts.size,
    solver = { with(create(it)) { parts.map { suspend { it.invoke(this) } } } },
    name = name,
    skipByDefault = skipByDefault,
)
