package com.github.ephemient.aoc2024

import kotlin.test.Test
import kotlin.test.assertEquals

class Day21Test {
    @Test
    fun part1() {
        assertEquals(126384, Day21(example).part1())
    }

    companion object {
        private val example =
            """
            |029A
            |980A
            |179A
            |456A
            |379A
            |""".trimMargin()
    }
}
