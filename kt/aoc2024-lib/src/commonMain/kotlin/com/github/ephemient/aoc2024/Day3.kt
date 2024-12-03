package com.github.ephemient.aoc2024

class Day3(private val input: String) {
    fun part1(): Int = part1(input)

    fun part2(): Int = input.splitToSequence("do()").sumOf { part1(it.substringBefore("don't()")) }

    companion object {
        private val regex = """mul\((\d+),(\d+)\)""".toRegex()

        private fun part1(input: String): Int = regex.findAll(input).sumOf { match ->
            val (x, y) = match.destructured
            x.toInt() * y.toInt()
        }
    }
}
