package com.github.ephemient.aoc2024

import org.junit.jupiter.api.condition.DisabledInNativeImage
import kotlin.test.Test
import kotlin.test.assertEquals

@DisabledInNativeImage
class Day17JvmTest {
    @Test
    fun part1() {
        assertEquals(listOf(4L, 6L, 3L, 5L, 6L, 3L, 5L, 2L, 1L, 0L), Day17Jvm(example1).part1())
    }

    @Test
    fun part2() {
        assertEquals(117440, Day17Jvm(example2).part2())
    }

    companion object {
        val example1 =
            """
            |Register A: 729
            |Register B: 0
            |Register C: 0
            |
            |Program: 0,1,5,4,3,0
            |""".trimMargin()
        val example2 =
            """
            |Register A: 2024
            |Register B: 0
            |Register C: 0
            |
            |Program: 0,3,5,4,3,0
            |""".trimMargin()
    }
}
