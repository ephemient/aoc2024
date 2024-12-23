package com.github.ephemient.aoc2024.exe

import com.github.ephemient.aoc2024.Day17Jvm
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Blackhole
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day17JvmBench {
    private lateinit var input: String
    private lateinit var day17: Day17Jvm

    @Setup
    fun setup() {
        input = getDayInput(17)
        day17 = Day17Jvm(input)
    }

    @Benchmark
    fun part1() = Day17Jvm(input).part1()

    @Benchmark
    fun part1_cached() = day17.part1()

    @Benchmark
    fun part2() = Day17Jvm(input).part2()

    @Benchmark
    fun part2_cached() = day17.part2()

    @Benchmark
    fun solve(bh: Blackhole) {
        val day17 = Day17Jvm(input)
        bh.consume(day17.part1())
        bh.consume(day17.part2())
    }

    @Benchmark
    fun solve_cached(bh: Blackhole) {
        bh.consume(day17.part1())
        bh.consume(day17.part2())
    }
}
