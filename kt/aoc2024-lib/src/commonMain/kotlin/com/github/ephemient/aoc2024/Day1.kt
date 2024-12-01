package com.github.ephemient.aoc2024

import kotlin.math.abs

class Day1(input: String) {
    private val left: List<Int>
    private val right: List<Int>

    init {
        val left = mutableListOf<Int>()
        val right = mutableListOf<Int>()
        for (line in input.lineSequence()) {
            val parts = line.trim().split(splitter, limit = 2)
            val x = parts.getOrNull(0)?.toIntOrNull() ?: continue
            val y = parts.getOrNull(1)?.toIntOrNull() ?: continue
            left.add(x)
            right.add(y)
        }
        this.left = left.toList()
        this.right = right.toList()
    }

    fun part1() = left.sorted().zip(right.sorted(), Int::minus).sumOf(::abs)

    fun part2(): Int {
        val right = right.groupingBy { it }.eachCount()
        return left.sumOf { it * right.getOrElse(it) { 0 } }
    }

    companion object {
        private val splitter = """\s+""".toRegex()
    }
}
