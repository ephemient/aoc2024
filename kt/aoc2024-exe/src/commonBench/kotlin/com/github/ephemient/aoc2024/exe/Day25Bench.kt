package com.github.ephemient.aoc2024.exe

import com.github.ephemient.aoc2024.Day24
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day25Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(24)
    }

    @Benchmark
    fun part1() = Day24(input).part1()
}
