package com.github.ephemient.aoc2024

import kotlin.test.Test
import kotlin.test.assertEquals

class Day25Test {
    @Test
    fun part1() {
        assertEquals(3, Day25(example).part1())
    }

    companion object {
        private val example =
            """
            |#####
            |.####
            |.####
            |.####
            |.#.#.
            |.#...
            |.....
            |
            |#####
            |##.##
            |.#.##
            |...##
            |...#.
            |...#.
            |.....
            |
            |.....
            |#....
            |#....
            |#...#
            |#.#.#
            |#.###
            |#####
            |
            |.....
            |.....
            |#.#..
            |###..
            |###.#
            |###.#
            |#####
            |
            |.....
            |.....
            |.....
            |#....
            |#.#..
            |#.#.#
            |#####
            |""".trimMargin()
    }
}
