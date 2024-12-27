package com.github.ephemient.aoc2024

expect val days: List<Day>

data class Day(
    val day: Int,
    val parts: Int,
    val solver: (String) -> (List<suspend () -> Any?>),
    val name: String = day.toString(),
)

fun <T> Day(
    day: Int,
    create: (String) -> T,
    vararg parts: suspend (T) -> Any?,
    name: String = day.toString(),
): Day {
    val size = parts.size
    return Day(
        day = day,
        parts = size,
        solver = solver@{
            val solver = try {
                create(it)
            } catch (_: AssertionError) {
                return@solver List(size) { { "SKIPPED" } }
            }
            parts.map { suspend { it.invoke(solver) } }
        },
        name = name,
    )
}
