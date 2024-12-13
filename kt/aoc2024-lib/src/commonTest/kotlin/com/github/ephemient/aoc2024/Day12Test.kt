package com.github.ephemient.aoc2024

import kotlin.test.Test
import kotlin.test.assertEquals

class Day12Test {
    @Test
    fun part1() {
        assertEquals(140, Day12(example1).part1())
        assertEquals(772, Day12(example2).part1())
        assertEquals(1930, Day12(example3).part1())
    }

    @Test
    fun part2() {
        assertEquals(80, Day12(example1).part2())
        assertEquals(436, Day12(example2).part2())
        assertEquals(236, Day12(example4).part2())
        assertEquals(368, Day12(example5).part2())
        assertEquals(1206, Day12(example3).part2())
    }

    companion object {
        private val example1 =
            """
            |AAAA
            |BBCD
            |BBCC
            |EEEC
            |""".trimMargin()
        private val example2 =
            """
            |OOOOO
            |OXOXO
            |OOOOO
            |OXOXO
            |OOOOO
            |""".trimMargin()
        private val example3 =
            """
            |RRRRIICCFF
            |RRRRIICCCF
            |VVRRRCCFFF
            |VVRCCCJFFF
            |VVVVCJJCFE
            |VVIVCCJJEE
            |VVIIICJJEE
            |MIIIIIJJEE
            |MIIISIJEEE
            |MMMISSJEEE
            |""".trimMargin()
        private val example4 =
            """
            |EEEEE
            |EXXXX
            |EEEEE
            |EXXXX
            |EEEEE
            |""".trimMargin()
        private val example5 =
            """
            |AAAAAA
            |AAABBA
            |AAABBA
            |ABBAAA
            |ABBAAA
            |AAAAAA
            |""".trimMargin()
    }
}
