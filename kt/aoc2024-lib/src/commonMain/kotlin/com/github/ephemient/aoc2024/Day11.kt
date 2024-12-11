package com.github.ephemient.aoc2024

class Day11(input: String) {
    private val input = buildMap {
        for (word in input.splitToSequence(WHITESPACE)) {
            incBy(word.toLongOrNull() ?: continue, 1)
        }
    }

    fun part1() = solve(25)

    fun part2() = solve(75)

    internal fun solve(n: Int): Long {
        var counts = input
        repeat(n) {
            counts = buildMap {
                for ((num, count) in counts) {
                    if (num == 0L) {
                        incBy(1, count)
                    } else {
                        val string = num.toString()
                        if (string.length % 2 == 0) {
                            incBy(string.take(string.length / 2).toLong(), count)
                            incBy(string.drop(string.length / 2).toLong(), count)
                        } else {
                            incBy(2024 * num, count)
                        }
                    }
                }
            }
        }
        return counts.values.sum()
    }

    companion object {
        private val WHITESPACE = """\s+""".toRegex()

        private fun <K> MutableMap<K, Long>.incBy(key: K, value: Long) =
            put(key, getOrElse(key) { 0 } + value)
    }
}
