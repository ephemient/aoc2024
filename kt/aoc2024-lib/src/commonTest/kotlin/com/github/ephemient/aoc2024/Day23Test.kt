package com.github.ephemient.aoc2024

import kotlinx.coroutines.test.runTest
import kotlin.test.Test
import kotlin.test.assertEquals

class Day23Test {
    @Test
    fun part1() = runTest {
        assertEquals(7, Day23(example).part1())
    }

    @Test
    fun part2() = runTest {
        assertEquals("co,de,ka,ta", Day23(example).part2())
    }

    companion object {
        private val example =
            """
            |kh-tc
            |qp-kh
            |de-cg
            |ka-co
            |yn-aq
            |qp-ub
            |cg-tb
            |vc-aq
            |tb-ka
            |wh-tc
            |yn-cg
            |kh-ub
            |ta-co
            |de-co
            |tc-td
            |tb-wq
            |wh-td
            |ta-ka
            |td-qp
            |aq-cg
            |wq-ub
            |ub-vc
            |de-ta
            |wq-aq
            |wq-vc
            |wh-yn
            |ka-de
            |kh-ta
            |co-tc
            |wh-qp
            |tb-vc
            |td-yn
            |""".trimMargin()
    }
}
