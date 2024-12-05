package com.github.ephemient.aoc2024

class Day5(input: String) {
    private val deps: Set<IntPair>
    private val updates: List<List<Int>>

    init {
        val (deps, updates) = input.split("\n\n")
        this.deps = deps.lines().mapTo(mutableSetOf()) { line ->
            val (first, second) = line.split('|', limit = 2)
            first.toInt() to second.toInt()
        }
        this.updates = updates.lines().mapNotNull { line ->
            line.ifEmpty { return@mapNotNull null }.split(',').map { it.toInt() }
        }
    }

    fun part1() = updates.sumOf { pages ->
        if (
            pages.withIndex().all { (i, x) ->
                pages.subList(i + 1, pages.size).all { y -> y to x !in deps }
            }
        ) pages[pages.size / 2] else 0
    }

    fun part2(): Int = updates.sumOf { update ->
        val pages = update.toMutableList()
        for (i in pages.indices) {
            while (true) {
                val x = pages[i]
                val j = i + 1 + pages.subList(i + 1, pages.size).indexOfFirst { it to x in deps }
                if (j > i) {
                    pages[i] = pages[j]
                    pages[j] = x
                } else break
            }
        }
        if (update != pages) pages[pages.size / 2] else 0
    }
}
