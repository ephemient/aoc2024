package com.github.ephemient.aoc2024.exe

import com.github.ephemient.aoc2024.Day16
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Blackhole
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day16Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(16)
    }

    @Benchmark
    fun part1() = Day16(input).part1()

    @Benchmark
    fun part2() = Day16(input).part2()

    @Benchmark
    fun solve(bh: Blackhole) {
        val day16 = Day16(input)
        bh.consume(day16.part1())
        bh.consume(day16.part2())
    }
}
