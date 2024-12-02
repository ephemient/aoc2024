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
            for (i in 0..report.size - 2) {
                when (report[i + 1] - report[i]) {
                    in -3..-1 -> decreasing = true
                    in 1..3 -> increasing = true
                    else -> return false
                }
            }
            return !(decreasing && increasing)
        }

        private fun isSafe2(report: IntArray): Boolean {
            if (report.isEmpty()) return true
            val report2 = report.copyOf(report.size - 1)
            for (i in report2.lastIndex downTo 0) {
                if (isSafe1(report2)) return true
                report2[i] = report[i + 1]
            }
            return isSafe1(report2)
        }
    }
}
