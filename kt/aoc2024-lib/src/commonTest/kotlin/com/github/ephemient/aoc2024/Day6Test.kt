package com.github.ephemient.aoc2024

import kotlinx.coroutines.test.runTest
import kotlin.test.Test
import kotlin.test.assertEquals

class Day6Test {
    @Test
    fun part1() {
        assertEquals(41, Day6(example).part1())
    }

    @Test
    fun part2() = runTest{
        assertEquals(6, Day6(example).part2())
    }

    companion object {
        private val example =
            """
            |....#.....
            |.........#
            |..........
            |..#.......
            |.......#..
            |..........
            |.#..^.....
            |........#.
            |#.........
            |......#...
            |""".trimMargin()
    }
}
