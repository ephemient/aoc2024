package com.github.ephemient.aoc2024

import kotlin.test.Test
import kotlin.test.assertEquals

class Day10Test {
    @Test
    fun part1() {
        assertEquals(36, Day10(example).part1())
    }

    @Test
    fun part2() {
        assertEquals(81, Day10(example).part2())
    }

    companion object {
        private val example =
            """
            |89010123
            |78121874
            |87430965
            |96549874
            |45678903
            |32019012
            |01329801
            |10456732
            |""".trimMargin()
    }
}
