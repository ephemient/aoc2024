package com.github.ephemient.aoc2024

import kotlin.test.Test
import kotlin.test.assertEquals

class Day20Test {
    @Test
    fun part1() {
        assertEquals(
            listOf(14, 14, 2, 4, 2, 3, 1, 1, 1, 1, 1),
            listOf(2, 4, 6, 8, 10, 12, 20, 36, 38, 40, 64, 100)
                .map(Day20(example)::part1)
                .zipWithNext(Int::minus)
        )
    }

    @Test
    fun part2() {
        assertEquals(
            listOf(32, 31, 29, 39, 25, 23, 20, 19, 12, 14, 12, 22, 4, 3),
            listOf(50, 52, 54, 56, 58, 60, 62, 64, 66, 68, 70, 72, 74, 76, 100)
                .map(Day20(example)::part2)
                .zipWithNext(Int::minus)
        )
    }

    companion object {
        private val example =
            """
            |###############
            |#...#...#.....#
            |#.#.#.#.#.###.#
            |#S#...#.#.#...#
            |#######.#.#.###
            |#######.#.#...#
            |#######.#.###.#
            |###..E#...#...#
            |###.#######.###
            |#...###...#...#
            |#.#####.#.###.#
            |#.#...#.#.#...#
            |#.#.#.#.#.#.###
            |#...#...#...###
            |###############
            |""".trimMargin()
    }
}
