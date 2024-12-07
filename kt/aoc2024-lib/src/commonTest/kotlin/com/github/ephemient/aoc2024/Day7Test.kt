package com.github.ephemient.aoc2024

import kotlin.test.Test
import kotlin.test.assertEquals

class Day7Test {
    @Test
    fun part1() {
        assertEquals(3749L, Day7(example).part1())
    }

    @Test
    fun part2() {
        assertEquals(11387L, Day7(example).part2())
    }

    companion object {
        private val example =
            """
            |190: 10 19
            |3267: 81 40 27
            |83: 17 5
            |156: 15 6
            |7290: 6 8 6 15
            |161011: 16 10 13
            |192: 17 8 14
            |21037: 9 7 18 13
            |292: 11 6 16 20
            |""".trimMargin()
    }
}
