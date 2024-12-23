package com.github.ephemient.aoc2024

import kotlinx.coroutines.flow.asFlow
import kotlinx.coroutines.flow.flatMapMerge
import kotlinx.coroutines.flow.flow
import kotlinx.coroutines.flow.fold

class Day22(input: String) {
    private val seeds = input.lines().mapNotNull { it.toIntOrNull() }

    suspend fun part1() = seeds.asFlow().flatMapMerge {
        flow { emit(generateSequence(it, ::step).elementAt(2000).toLong()) }
    }.fold(0, Long::plus)

    suspend fun part2(): Int {
        val data = IntArray(19 * 19 * 19 * 19)
        return seeds.asFlow().flatMapMerge {
            flow {
                val seen = BooleanArray(19 * 19 * 19 * 19)
                for (window in generateSequence(it, ::step).take(2001).map { it % 10 }.windowed(5)) {
                    val key = window.zipWithNext(Int::minus).fold(0) { a, b -> 19 * a + b + 9 }
                    if (!seen[key]) {
                        emit(IndexedValue(key, window.last()))
                        seen[key] = true
                    }
                }
            }
        }.fold(0) { acc, (key, value) -> maxOf(acc, (data[key] + value).also { data[key] = it }) }
    }

    companion object {
        private fun step(secret: Int): Int {
            val one = secret shl 6 xor secret and 16777215
            val two = one shr 5 xor one and 16777215
            return two shl 11 xor two and 16777215
        }
    }
}
