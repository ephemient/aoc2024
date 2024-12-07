package com.github.ephemient.aoc2024

class Day7(input: String) {
    private val equations = input.lineSequence().filter { it.isNotEmpty() }.map { line ->
        val (lhs, rhs) = line.split(": ", limit = 2)
        lhs.toLong() to rhs.split(' ').map { it.toLong() }
    }.toList()

    fun part1() = equations.sumOf { equation ->
        val stack = mutableListOf(equation)
        while (stack.isNotEmpty()) {
            val (x, values) = stack.removeLast()
            val y = values.last()
            if (values.size == 1) {
                if (x == y) return@sumOf equation.first else continue
            }
            val rest = values.subList(0, values.lastIndex)
            if (x >= y) stack.add(x - y to rest)
            if (x % y == 0L) stack.add(x / y to rest)
        }
        0
    }

    fun part2() = equations.sumOf { equation ->
        val stack = mutableListOf(equation)
        while (stack.isNotEmpty()) {
            val (x, values) = stack.removeLast()
            val y = values.last()
            if (values.size == 1) {
                if (x == y) return@sumOf equation.first else continue
            }
            val rest = values.subList(0, values.lastIndex)
            if (x >= y) stack.add(x - y to rest)
            if (x % y == 0L) stack.add(x / y to rest)
            if (x > y) {
                var d = 10L
                while (d <= y) d *= 10
                if (x % d == y) stack.add(x / d to rest)
            }
        }
        0
    }
}
