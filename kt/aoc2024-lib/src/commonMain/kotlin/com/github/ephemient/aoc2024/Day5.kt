package com.github.ephemient.aoc2024

class Day5(input: String) {
    private val rdeps: Map<Int, Set<Int>>
    private val correct: List<List<Int>>
    private val incorrect: List<List<Int>>

    init {
        val (deps, updates) = input.split("\n\n")
        this.rdeps = deps.lineSequence()
            .groupingBy { it.substringAfter('|').toInt() }
            .aggregate { _, accumulator: MutableSet<Int>?, element, _ ->
                val value = element.substringBefore('|').toInt()
                accumulator?.apply { add(value) } ?: mutableSetOf(value)
            }
        val (correct, incorrect) = updates.lines().mapNotNull { line ->
            line.ifEmpty { return@mapNotNull null }.split(',').map { it.toInt() }
        }.partition { pages ->
            pages.withIndex().all { (i, x) ->
                rdeps[x]?.let { pages.subList(i + 1, pages.size).any(it::contains) } != true
            }
        }
        this.correct = correct
        this.incorrect = incorrect
    }

    fun part1() = correct.sumOf { it[it.size / 2] }

    fun part2(): Int = incorrect.sumOf {
        val pages = it.toMutableList()
        for (i in pages.indices) {
            while (true) {
                val x = pages[i]
                val j = pages.subList(i + 1, pages.size).indexOfFirst(rdeps[x].orEmpty()::contains)
                if (j < 0) break
                pages[i] = pages[i + 1 + j]
                pages[i + 1 + j] = x
            }
        }
        pages[pages.size / 2]
    }
}
