package com.github.ephemient.aoc2024

import kotlin.test.Test
import kotlin.test.assertEquals

class Day1Test {
    @Test
    fun part1() {
        assertEquals(11, Day1(example).part1())
    }

    @Test
    fun part2() {
        assertEquals(31, Day1(example).part2())
    }

    companion object {
        private val example =
            """
            |3   4
            |4   3
            |2   5
            |1   3
            |3   9
            |3   3
            |""".trimMargin()
    }
}
