package com.github.ephemient.aoc2024.exe

import com.github.ephemient.aoc2024.Day14
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Blackhole
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day14Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(14)
    }

    @Benchmark
    fun part1() = Day14(input).part1()

    @Benchmark
    fun part2() = Day14(input).part2()

    @Benchmark
    fun solve(bh: Blackhole) {
        val day14 = Day14(input)
        bh.consume(day14.part1())
        bh.consume(day14.part2())
    }
}
