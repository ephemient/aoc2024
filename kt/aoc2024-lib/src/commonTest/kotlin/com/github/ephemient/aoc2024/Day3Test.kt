package com.github.ephemient.aoc2024

import kotlin.test.Test
import kotlin.test.assertEquals

class Day3Test {
    @Test
    fun part1() {
        assertEquals(161, Day3(example1).part1())
    }

    @Test
    fun part2() {
        assertEquals(48, Day3(example2).part2())
    }

    companion object {
        private val example1 =
            """
            |xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
            |""".trimMargin()
        private val example2 =
            """
            |xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
            |""".trimMargin()
    }
}
