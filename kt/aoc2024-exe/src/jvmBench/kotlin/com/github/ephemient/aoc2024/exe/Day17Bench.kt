package com.github.ephemient.aoc2024.exe

import com.github.ephemient.aoc2024.Day17
import com.github.ephemient.aoc2024.Day17Jvm
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Blackhole
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day17Bench {
    private lateinit var input: String
    private lateinit var day17: Day17
    private var day17Jvm: Day17Jvm? = null

    @Setup
    fun setup() {
        input = getDayInput(17)
        day17 = Day17(input)
        day17Jvm = try {
            Day17Jvm(input)
        } catch (_: AssertionError) {
            null
        }
    }

    @Benchmark
    fun part1() = Day17(input).part1()

    @Benchmark
    fun part1_cached() = day17.part1()

    @Benchmark
    fun part1_jvm() = Day17Jvm(input).part1()

    @Benchmark
    fun part1_jvm_cached() = day17Jvm!!.part1()

    @Benchmark
    fun part2() = Day17(input).part2()

    @Benchmark
    fun part2_cached() = day17.part2()

    @Benchmark
    fun part2_jvm() = Day17Jvm(input).part2()

    @Benchmark
    fun part2_jvm_cached() = day17Jvm!!.part2()

    @Benchmark
    fun solve(bh: Blackhole) {
        val day17 = Day17(input)
        bh.consume(day17.part1())
        bh.consume(day17.part2())
    }

    @Benchmark
    fun solve_cached(bh: Blackhole) {
        bh.consume(day17.part1())
        bh.consume(day17.part2())
    }

    @Benchmark
    fun solve_jvm(bh: Blackhole) {
        val day17Jvm = Day17Jvm(input)
        bh.consume(day17Jvm.part1())
        bh.consume(day17Jvm.part2())
    }

    @Benchmark
    fun solve_jvm_cached(bh: Blackhole) {
        bh.consume(day17Jvm!!.part1())
        bh.consume(day17Jvm!!.part2())
    }
}
