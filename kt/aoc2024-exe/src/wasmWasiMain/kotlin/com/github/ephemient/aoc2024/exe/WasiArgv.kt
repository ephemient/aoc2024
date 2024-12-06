package com.github.ephemient.aoc2024.exe

import kotlin.wasm.unsafe.Pointer
import kotlin.wasm.unsafe.UnsafeWasmMemoryApi
import kotlin.wasm.unsafe.withScopedMemoryAllocator

@WasmImport("wasi_snapshot_preview1", "args_sizes_get")
private external fun argsSizesGet(argcPtr: UInt, bufsizPtr: UInt): Int

@WasmImport("wasi_snapshot_preview1", "args_get")
private external fun argsGet(argvPtr: UInt, argvBufPtr: UInt): Int

@OptIn(UnsafeWasmMemoryApi::class)
internal fun argv(): Array<String> {
    val argc: Int
    val bufsiz: Int
    withScopedMemoryAllocator { allocator ->
        val argcPtr = allocator.allocate(Int.SIZE_BYTES)
        val bufsizPtr = allocator.allocate(Int.SIZE_BYTES)
        val errno = argsSizesGet(argcPtr.address, bufsizPtr.address)
        check(errno == 0) { "args_sizes_get: $errno" }
        argc = argcPtr.loadInt()
        bufsiz = bufsizPtr.loadInt()
    }
    val buffer = ByteArray(bufsiz)
    return withScopedMemoryAllocator { allocator ->
        val argvPtr = allocator.allocate(argc * Int.SIZE_BYTES)
        val errno = argsGet(argvPtr.address, allocator.allocate(bufsiz).address)
        check(errno == 0) { "args_get: $errno" }
        Array(argc) {
            val argPtr = Pointer(argvPtr.plus(it * Int.SIZE_BYTES).loadInt().toUInt())
            for (i in buffer.indices) {
                buffer[i] = argPtr.plus(i).loadByte()
                if (buffer[i] == 0.toByte()) return@Array buffer.decodeToString(endIndex = i)
            }
            error("missing \\0")
        }
    }
}
