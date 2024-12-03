package com.github.ephemient.aoc2024

class Day3(private val input: String) {
    fun part1(): Int = regex1.findAll(input).sumOf { match ->
        val (x, y) = match.destructured
        x.toInt() * y.toInt()
    }

    fun part2(): Int {
        var enable = true
        return regex2.findAll(input)
            .filter { match ->
                val (yes, no) = match.destructured
                enable = when {
                    yes.isNotEmpty() -> true
                    no.isNotEmpty() -> false
                    else -> return@filter enable
                }
                false
            }
            .sumOf { match ->
                val (_, _, x, y) = match.destructured
                x.toInt() * y.toInt()
            }
    }

    companion object {
        private val regex1 = """mul\((\d+),(\d+)\)""".toRegex()
        private val regex2 = """(do\(\))|(don't\(\))|mul\((\d+),(\d+)\)""".toRegex()
    }
}
