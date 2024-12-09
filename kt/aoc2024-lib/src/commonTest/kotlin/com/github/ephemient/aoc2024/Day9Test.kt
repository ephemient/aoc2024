package com.github.ephemient.aoc2024

import kotlin.test.Test
import kotlin.test.assertEquals

class Day9Test {
    @Test
    fun part1() {
        assertEquals(1928L, Day9(example).part1())
    }

    @Test
    fun part2() {
        assertEquals(2858L, Day9(example).part2())
    }

    companion object {
        private val example =
            """
            |2333133121414131402
            |""".trimMargin()
    }
}