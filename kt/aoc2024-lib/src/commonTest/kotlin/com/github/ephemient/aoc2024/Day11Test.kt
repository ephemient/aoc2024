package com.github.ephemient.aoc2024

import kotlin.test.Test
import kotlin.test.assertEquals

class Day11Test {
    @Test
    fun part1() {
        assertEquals(7, Day11(example1).solve(1))
        assertEquals(22, Day11(example2).solve(6))
        assertEquals(55312, Day11(example2).solve(25))
    }

    companion object {
        private val example1 =
            """
            |0 1 10 99 999
            |""".trimMargin()
        private val example2 =
            """
            |125 17
            |""".trimMargin()
    }
}
