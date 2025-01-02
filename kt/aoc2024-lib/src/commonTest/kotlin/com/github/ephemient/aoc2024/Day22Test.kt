package com.github.ephemient.aoc2024

import kotlin.test.Test
import kotlin.test.assertEquals

class Day22Test {
    @Test
    fun part1() {
        assertEquals(37327623, Day22(example1).part1())
    }

    @Test
    fun part2() {
        assertEquals(23, Day22(example2).part2())
    }

    companion object {
        private val example1 =
            """
            |1
            |10
            |100
            |2024
            |""".trimMargin()
        private val example2 =
            """
            |1
            |2
            |3
            |2024
            |""".trimMargin()
    }
}
