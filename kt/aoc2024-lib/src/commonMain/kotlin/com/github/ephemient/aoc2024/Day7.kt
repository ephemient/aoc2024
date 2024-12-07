package com.github.ephemient.aoc2024

class Day7(input: String) {
    private val equations = input.lineSequence().filter { it.isNotEmpty() }.map { line ->
        val (lhs, rhs) = line.split(": ", limit = 2)
        lhs.toLong() to rhs.split(' ').map { it.toLong() }
    }.toList()

    private fun solve(op: suspend SequenceScope<Long>.(Long, Long) -> Unit) = equations.sumOf {
        val stack = mutableListOf(it)
        while (stack.isNotEmpty()) {
            val (x, values) = stack.removeLast()
            val y = values.last()
            if (values.size == 1) {
                if (x == y) return@sumOf it.first else continue
            }
            val rest = values.subList(0, values.lastIndex)
            sequence { op(x, y) }.mapTo(stack) { it to rest }
        }
        0
    }

    fun part1() = solve { x, y ->
        if (x >= y) yield(x - y)
        if (x % y == 0L) yield(x / y)
    }

    fun part2() = solve { x, y ->
        if (x >= y) yield(x - y)
        if (x % y == 0L) yield(x / y)
        if (x > y) {
            var d = 10L
            while (d <= y) d *= 10
            if (x % d == y) yield(x / d)
        }
    }
}
