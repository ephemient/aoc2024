package com.github.ephemient.aoc2024

class Day2(input: String) {
    private val reports = input.lineSequence().mapNotNull { line ->
        val words = line.split(splitter)
        IntArray(words.size) { words[it].toIntOrNull() ?: return@mapNotNull null }
    }.toList()

    fun part1() = reports.count(::isSafe1)

    fun part2() = reports.count(::isSafe2)

    companion object {
        private val splitter = """\s+""".toRegex()

        private fun isSafe1(report: IntArray): Boolean {
            var decreasing = false
            var increasing = false
            return (0..report.size - 2).all {
                when (report[it + 1] - report[it]) {
                    in -3..-1 -> !increasing.also { decreasing = true }
                    in 1..3 -> !decreasing.also { increasing = true }
                    else -> false
                }
            }
        }

        private fun isSafe2(report: IntArray): Boolean {
            if (report.isEmpty()) return true
            val report2 = report.copyOfRange(1, report.size)
            return isSafe1(report2) || report2.indices.any {
                report2[it] = report[it]
                isSafe1(report2)
            }
        }
    }
}
