package com.github.ephemient.aoc2024

import kotlin.test.Test
import kotlin.test.assertEquals

class Day19Test {
    @Test
    fun solve() {
        assertEquals(Day19.Solution(6, 16), Day19(example).solve())
    }

    companion object {
        private val example =
            """
            |r, wr, b, g, bwu, rb, gb, br
            |
            |brwrr
            |bggr
            |gbbr
            |rrbgbr
            |ubwu
            |bwurrg
            |brgr
            |bbrgwb
            |""".trimMargin()
    }
}
