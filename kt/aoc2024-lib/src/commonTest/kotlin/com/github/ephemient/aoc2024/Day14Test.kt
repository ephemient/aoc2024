package com.github.ephemient.aoc2024

import kotlin.test.Test
import kotlin.test.assertEquals

class Day14Test {
    @Test
    fun part1() {
        assertEquals(12, Day14(example).part1(11, 7))
    }

    companion object {
        val example =
            """
            |p=0,4 v=3,-3
            |p=6,3 v=-1,-3
            |p=10,3 v=-1,2
            |p=2,0 v=2,-1
            |p=0,0 v=1,3
            |p=3,0 v=-2,-2
            |p=7,6 v=-1,-3
            |p=3,0 v=-1,-2
            |p=9,3 v=2,3
            |p=7,3 v=-1,2
            |p=2,4 v=2,-3
            |p=9,5 v=-3,-3
            |""".trimMargin()
    }
}
