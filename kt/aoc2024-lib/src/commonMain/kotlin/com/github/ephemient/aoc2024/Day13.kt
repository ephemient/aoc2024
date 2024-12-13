package com.github.ephemient.aoc2024

class Day13(input: String) {
    private val machines = pattern.findAll(input).map { matchResult ->
        val (ax, ay, bx, `by`, x, y) = matchResult.destructured
        Machine(ax.toLong(), ay.toLong(), bx.toLong(), `by`.toLong(), x.toLong(), y.toLong())
    }.toList()

    fun part1() = machines.sumOf { it.solve() }

    fun part2() = machines.sumOf { it.copy(x = it.x + 10000000000000, y = it.y + 10000000000000).solve() }

    private data class Machine(
        val ax: Long,
        val ay: Long,
        val bx: Long,
        val `by`: Long,
        val x: Long,
        val y: Long,
    ) {
        fun solve(): Long {
            val aNumerator = x * `by` - y * bx
            val aDenominator = ax * `by` - bx * ay
            val bNumerator = x * ay - y * ax
            val bDenominator = ay * bx - `by` * ax
            return if (aNumerator % aDenominator == 0L && bNumerator % bDenominator == 0L) {
                val a = aNumerator / aDenominator
                val b = bNumerator / bDenominator
                3 * a + b
            } else 0
        }
    }

    companion object {
        private val pattern =
            """
            Button A: X\+(\d+), Y\+(\d+)
            Button B: X\+(\d+), Y\+(\d+)
            Prize: X=(\d+), Y=(\d+)
            """.trimIndent().toRegex()
    }
}
