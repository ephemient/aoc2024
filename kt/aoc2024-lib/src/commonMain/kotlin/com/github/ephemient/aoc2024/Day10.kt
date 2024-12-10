package com.github.ephemient.aoc2024

class Day10(input: String) {
    private val levels: List<Set<IntPair>>
    init {
        val levels = List(10) { mutableSetOf<IntPair>() }
        for ((y, line) in input.lineSequence().withIndex()) {
            for ((x, char) in line.withIndex()) {
                if (char.isDigit()) levels[char.digitToInt()].add(y to x)
            }
        }
        this.levels = levels
    }

    private inline fun <T> bfs(start: (IntPair) -> T, plus: (T, T) -> T): Map<IntPair, T> =
        levels.subList(1, 10).fold(levels[0].associateWith(start)) { acc, points ->
            buildMap {
                for ((key, value) in acc) {
                    for (point in key.adj) {
                        if (point in points) {
                            put(point, if (contains(point)) plus(value, getValue(point)) else value)
                        }
                    }
                }
            }
        }

    fun part1() = bfs(::setOf, Set<IntPair>::plus).values.sumOf { it.size }

    fun part2() = bfs({ 1 }, Int::plus).values.sum()

    companion object {
        private val IntPair.adj: List<IntPair>
            get() = listOf(
                first - 1 to second,
                first to second - 1,
                first to second + 1,
                first + 1 to second,
            )
    }
}
