package com.github.ephemient.aoc2024

import kotlin.test.Test
import kotlin.test.assertEquals

class Day5Test {
    @Test
    fun part1() {
        assertEquals(143, Day5(example).part1())
    }

    @Test
    fun part2() {
        assertEquals(123, Day5(example).part2())
    }

    companion object {
        private val example =
            """
            |47|53
            |97|13
            |97|61
            |97|47
            |75|29
            |61|13
            |75|53
            |29|13
            |97|29
            |53|29
            |61|53
            |97|53
            |61|29
            |47|13
            |75|47
            |97|75
            |47|61
            |75|61
            |47|29
            |75|13
            |53|13
            |
            |75,47,61,53,29
            |97,61,53,29,13
            |75,29,13
            |75,97,47,61,53
            |61,13,29
            |97,13,75,29,47
            |""".trimMargin()
    }
}
