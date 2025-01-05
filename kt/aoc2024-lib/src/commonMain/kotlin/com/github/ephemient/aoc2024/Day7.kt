package com.github.ephemient.aoc2024

class Day7(input: String) {
    private val equations = input.lineSequence().filter { it.isNotEmpty() }.map { line ->
        val (lhs, rhs) = line.split(": ", limit = 2)
        lhs.toLong() to rhs.split(' ').map { it.toLong() }
    }.toList()

    private fun solve(op: (Long, Long) -> LongArray) = equations.sumOf {
        val stack = mutableListOf(it)
        while (stack.isNotEmpty()) {
            val (x, values) = stack.removeLast()
            val y = values.last()
            if (values.size == 1) {
                if (x == y) return@sumOf it.first else continue
            }
            val rest = values.subList(0, values.lastIndex)
            for (z in op(x, y)) if (z >= 0) stack.add(z to rest)
        }
        0
    }

    fun part1() = solve { x, y ->
        longArrayOf(
            if (x >= y) x - y else -1,
            if (x % y == 0L) x / y else -1,
        )
    }

    fun part2() = solve { x, y ->
        longArrayOf(
            if (x >= y) x - y else -1,
            if (x % y == 0L) x / y else -1,
            if (x > y) {
                var d = 10L
                while (d <= y) d *= 10
                if (x % d == y) x / d else -1
            } else {
                -1
            },
        )
    }
}
