package com.github.ephemient.aoc2024

class Day5(input: String) {
    private val rdeps: Map<Int, Set<Int>>
    private val updates: List<List<Int>>

    init {
        val (deps, updates) = input.split("\n\n")
        this.rdeps = deps.lineSequence()
            .groupingBy { it.substringAfter('|').toInt() }
            .aggregate { _, accumulator: MutableSet<Int>?, element, _ ->
                val value = element.substringBefore('|').toInt()
                accumulator?.apply { add(value) } ?: mutableSetOf(value)
            }
        this.updates = updates.lines().mapNotNull { line ->
            line.ifEmpty { return@mapNotNull null }.split(',').map { it.toInt() }
        }
    }

    fun part1() = updates.sumOf { pages ->
        if (
            pages.withIndex().all { (i, x) ->
                rdeps[x]?.let { pages.subList(i + 1, pages.size).any(it::contains) } != true
            }
        ) pages[pages.size / 2] else 0
    }

    fun part2(): Int = updates.sumOf { update ->
        val pages = update.toMutableList()
        for (i in pages.indices) {
            while (true) {
                val x = pages[i]
                val j = pages.subList(i + 1, pages.size).indexOfFirst(rdeps[x].orEmpty()::contains)
                if (j >= 0) {
                    pages[i] = pages[i + 1 + j]
                    pages[i + 1 + j] = x
                } else break
            }
        }
        if (update != pages) pages[pages.size / 2] else 0
    }
}
