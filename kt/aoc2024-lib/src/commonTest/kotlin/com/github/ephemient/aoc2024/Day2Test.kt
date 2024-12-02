package com.github.ephemient.aoc2024

import kotlin.test.Test
import kotlin.test.assertEquals

class Day2Test {
    @Test
    fun part1() {
        assertEquals(2, Day2(example).part1())
    }

    @Test
    fun part2() {
        assertEquals(4, Day2(example).part2())
    }

    companion object {
        private val example =
            """
            |7 6 4 2 1
            |1 2 7 8 9
            |9 7 6 2 1
            |1 3 2 4 5
            |8 6 4 4 1
            |1 3 6 7 9
            |""".trimMargin()
    }
}
