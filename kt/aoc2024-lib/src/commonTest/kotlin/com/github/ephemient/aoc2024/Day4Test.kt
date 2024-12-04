package com.github.ephemient.aoc2024

import kotlin.test.Test
import kotlin.test.assertEquals

class Day4Test {
    @Test
    fun part1() {
        assertEquals(18, Day4(example).part1())
    }

    @Test
    fun part2() {
        assertEquals(9, Day4(example).part2())
    }

    companion object {
        private val example =
            """
            |MMMSXXMASM
            |MSAMXMSMSA
            |AMXSXMAAMM
            |MSAMASMSMX
            |XMASAMXAMM
            |XXAMMXXAMA
            |SMSMSASXSS
            |SAXAMASAAA
            |MAMMMXMMMM
            |MXMXAXMASX
            |""".trimMargin()
    }
}
