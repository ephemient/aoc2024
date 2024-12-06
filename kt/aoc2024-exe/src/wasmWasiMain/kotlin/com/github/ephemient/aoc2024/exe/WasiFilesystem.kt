package com.github.ephemient.aoc2024.exe

import kotlin.wasm.unsafe.UnsafeWasmMemoryApi
import kotlin.wasm.unsafe.withScopedMemoryAllocator

@WasmImport("wasi_snapshot_preview1", "fd_prestat_get")
private external fun fdPrestatGet(fd: Int, prestat: UInt): Int

@WasmImport("wasi_snapshot_preview1", "fd_prestat_dir_name")
private external fun fdPrestatDirName(fd: Int, path: UInt, pathLen: Int): Int

@OptIn(UnsafeWasmMemoryApi::class)
private val preloadedFds = buildMap {
    withScopedMemoryAllocator { allocator ->
        val prestatPtr = allocator.allocate(8)

        var fd = 3
        while (true) {
            when (val errno = fdPrestatGet(fd, prestatPtr.address)) {
                8 -> break // errno.badf
                0 -> {} // errno.success
                else -> error("fd_prestat_get: $errno")
            }
            when (val prestatTag = prestatPtr.loadByte()) {
                0.toByte() -> { // preopentype.dir
                    val prNameLen = prestatPtr.plus(Int.SIZE_BYTES).loadInt()
                    val dirName = withScopedMemoryAllocator { allocator ->
                        val pathPtr = allocator.allocate(prNameLen)
                        val errno = fdPrestatDirName(fd, pathPtr.address, prNameLen)
                        check(errno == 0) { "fd_prestat_dir_name: $errno" }
                        ByteArray(prNameLen) { pathPtr.plus(it).loadByte() }
                    }.decodeToString()
                    put(dirName.removeSuffix("/") + "/", fd)
                }
                else -> error("unknown propentype $prestatTag")
            }
            fd++
        }
    }
}

@WasmImport("wasi_snapshot_preview1", "path_open")
private external fun pathOpen(
    fd: Int,
    dirflags: Int,
    path: UInt,
    pathLen: Int,
    oflags: Short,
    fsRightsBase: Long,
    fsRightsInheriting: Long,
    fdflags: Short,
    fdPtr: UInt,
): Int

@WasmImport("wasi_snapshot_preview1", "fd_filestat_get")
private external fun fdFilestatGet(fd: Int, filestat: UInt): Int

@WasmImport("wasi_snapshot_preview1", "fd_seek")
private external fun fdSeek(fd: Int, offset: Long, whence: Byte, filesize: UInt): Int

@WasmImport("wasi_snapshot_preview1", "fd_pread")
private external fun fdPread(fd: Int, iovsPtr: UInt, iovs: Int, offset: Long, sizePtr: UInt): Int

@WasmImport("wasi_snapshot_preview1", "fd_close")
private external fun fdClose(fd: Int): Int

// fd_read + fd_seek + fd_filestat_get
private const val READ_RIGHTS = 0x200006L
private const val BUFSIZ = 4096

@OptIn(UnsafeWasmMemoryApi::class)
internal fun readFile(path: String): ByteArray {
    val (prefix, dirfd) = checkNotNull(
        preloadedFds.filterKeys(path::startsWith).maxByOrNull { it.key.length }
    ) { "file not found: $path" }
    val fd = withScopedMemoryAllocator { allocator ->
        val pathBytes = path.removePrefix(prefix).encodeToByteArray()
        val pathPtr = allocator.allocate(pathBytes.size)
        for ((i, b) in pathBytes.withIndex()) pathPtr.plus(i).storeByte(b)
        val fdPtr = allocator.allocate(Int.SIZE_BYTES)
        // fsRightsBase = fd_read + fd_seek + fd_tell + fd_filestat_get
        val errno = pathOpen(dirfd, 1, pathPtr.address, pathBytes.size, 0, READ_RIGHTS, 0, 0, fdPtr.address)
        check(errno == 0) { "path_open: $errno" }
        fdPtr.loadInt()
    }
    try {
        var size = 0UL
        if (size == 0UL) withScopedMemoryAllocator { allocator ->
            val filestatPtr = allocator.allocate(8 * Long.SIZE_BYTES)
            val errno = fdFilestatGet(fd, filestatPtr.address)
            if (errno == 0) size = filestatPtr.plus(4 * Long.SIZE_BYTES).loadLong().toULong()
        }
        if (size == 0UL) withScopedMemoryAllocator { allocator ->
            val filesizePtr = allocator.allocate(Long.SIZE_BYTES)
            val errno = fdSeek(fd, 0, 2, filesizePtr.address)
            if (errno == 0) size = filesizePtr.loadLong().toULong()
        }
        withScopedMemoryAllocator { allocator ->
            val iovsPtr = allocator.allocate(Long.SIZE_BYTES)
            val sizePtr = allocator.allocate(Long.SIZE_BYTES)
            var offset = 0UL
            val buffers = buildList {
                var bufsiz = if (size in 1UL..Int.MAX_VALUE.toULong()) size.toInt() else BUFSIZ
                while (true) {
                    val bufferPtr = allocator.allocate(bufsiz)
                    iovsPtr.storeInt(bufferPtr.address.toInt())
                    iovsPtr.plus(Int.SIZE_BYTES).storeInt(bufsiz)
                    var errno: Int
                    do {
                        errno = fdPread(fd, iovsPtr.address, 1, offset.toLong(), sizePtr.address)
                    } while (errno == 6) // errno.again
                    check(errno == 0) { "fd_pread: $errno" }
                    val size = sizePtr.loadLong().toULong()
                    check(size <= bufsiz.toULong()) { "fd_read: $size > $bufsiz" }
                    if (size == 0UL) break
                    add(bufferPtr to size.toInt())
                    offset += size
                    bufsiz = BUFSIZ
                    check(offset <= Int.MAX_VALUE.toULong()) { "readFile: 22" }
                }
            }
            val buffer = ByteArray(offset.toInt())
            buffers.fold(0) { acc, (bufferPtr, size) ->
                repeat(size) {
                    buffer[acc + it] = bufferPtr.plus(it).loadByte()
                }
                acc + size
            }
            return buffer
        }
    } finally {
        fdClose(fd)
    }
}
