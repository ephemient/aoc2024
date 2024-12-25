package com.github.ephemient.aoc2024

class Day25(input: String) {
    private val keys: List<List<Int>>
    private val locks: List<List<Int>>
    init {
        val keys = mutableListOf<MutableList<Int>>()
        val locks = mutableListOf<MutableList<Int>>()
        val empty = mutableListOf<Int>()
        input.lineSequence().fold('\u0000' to empty) { acc, line ->
            when {
                line.isEmpty() -> return@fold '\u0000' to empty
                acc.second === empty -> line.first() to mutableListOf<Int>()
                    .also((if (line.startsWith('.')) keys else locks)::add)
                else -> acc
            }.also { (first, counts) ->
                for ((i, char) in line.withIndex()) {
                    if (char == first) {
                        if (i < counts.size) counts[i]++ else counts.add(1)
                    }
                }
            }
        }
        this.keys = keys
        this.locks = locks
    }

    fun part1() = keys.sumOf { key ->
        locks.count { lock ->
            key.zip(lock).all { it.first >= it.second }
        }
    }
}
