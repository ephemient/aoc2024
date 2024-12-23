package com.github.ephemient.aoc2024

import com.github.ephemient.aoc2024.Day17.IntList
import com.github.ephemient.aoc2024.Day17.Machine
import java.lang.invoke.MethodHandles
import java.lang.invoke.MethodType
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Label
import org.objectweb.asm.Opcodes

class Day17Jvm(input: String) {
    init {
        if (!System.getProperty("org.graalvm.nativeimage.imagecode").isNullOrEmpty()) {
            throw AssertionError("Unsupported in native image")
        }
    }

    private val delegate = Day17(input, ::machine)

    fun part1() = delegate.part1()

    fun part2() = delegate.part2()

    companion object {
        private val implName = Day17Jvm::class.java.name.replace('.', '/') + "\$Machine"
        private val intListName = IntList::class.java.name.replace('.', '/')
        private val machineName = Machine::class.java.name.replace('.', '/')

        private fun machine(program: IntList): Machine {
            val classWriter = ClassWriter(ClassWriter.COMPUTE_MAXS)
            classWriter.visit(Opcodes.V17, Opcodes.ACC_FINAL, implName, null, "java/lang/Object", arrayOf(machineName))
            classWriter.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null)?.run {
                visitCode()
                visitVarInsn(Opcodes.ALOAD, 0)
                visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)
                visitInsn(Opcodes.RETURN)
                visitMaxs(1, 1)
                visitEnd()
            }
            classWriter.visitMethod(Opcodes.ACC_PUBLIC, "invoke", "(JJJ)L$intListName;", null, null)?.run {
                visitCode()
                visitTypeInsn(Opcodes.NEW, intListName)
                visitInsn(Opcodes.DUP)
                visitMethodInsn(Opcodes.INVOKESPECIAL, intListName, "<init>", "()V", false)
                visitVarInsn(Opcodes.ASTORE, 0)
                visitInsn(Opcodes.LCONST_0)
                visitVarInsn(Opcodes.LSTORE, 7)
                val labels = Array(program.size) { Label() }
                val returnLabel = Label()
                val errorLabel = Label()
                for ((ip, instruction) in program.withIndex()) {
                    visitLabel(labels[ip])
                    if (ip == 0) {
                        visitFrame(Opcodes.F_FULL, 5, arrayOf(intListName, Opcodes.LONG, Opcodes.LONG, Opcodes.LONG, Opcodes.LONG), 0, null)
                    } else {
                        visitFrame(Opcodes.F_SAME, 0, null, 0, null)
                    }
                    when (program.getOrNull(ip + 1)) {
                        0L -> visitInsn(Opcodes.LCONST_0)
                        1L -> visitInsn(Opcodes.LCONST_1)
                        2L -> visitLdcInsn(2L)
                        3L -> visitLdcInsn(3L)
                        4L -> visitVarInsn(Opcodes.LLOAD, 1)
                        5L -> visitVarInsn(Opcodes.LLOAD, 3)
                        6L -> visitVarInsn(Opcodes.LLOAD, 5)
                        else -> {
                            visitJumpInsn(Opcodes.GOTO, errorLabel)
                            continue
                        }
                    }
                    visitVarInsn(Opcodes.LSTORE, 7)
                    when (instruction) {
                        0L -> {
                            visitVarInsn(Opcodes.LLOAD, 1)
                            visitVarInsn(Opcodes.LLOAD, 7)
                            visitInsn(Opcodes.L2I)
                            visitInsn(Opcodes.LSHR)
                            visitVarInsn(Opcodes.LSTORE, 1)
                        }
                        1L -> {
                            visitVarInsn(Opcodes.LLOAD, 3)
                            when (val operand = program[ip + 1]) {
                                0L -> visitInsn(Opcodes.LCONST_0)
                                1L -> visitInsn(Opcodes.LCONST_1)
                                else -> visitLdcInsn(operand)
                            }
                            visitInsn(Opcodes.LXOR)
                            visitVarInsn(Opcodes.LSTORE, 3)
                        }
                        2L -> {
                            visitVarInsn(Opcodes.LLOAD, 7)
                            visitLdcInsn(7L)
                            visitInsn(Opcodes.LAND)
                            visitVarInsn(Opcodes.LSTORE, 3)
                        }
                        3L -> {
                            visitVarInsn(Opcodes.LLOAD, 1)
                            visitInsn(Opcodes.LCONST_0)
                            visitInsn(Opcodes.LCMP)
                            visitJumpInsn(Opcodes.IFNE, labels.getOrElse(program[ip + 1].toInt()) { errorLabel }) }
                        4L -> {
                            visitVarInsn(Opcodes.LLOAD, 3)
                            visitVarInsn(Opcodes.LLOAD, 5)
                            visitInsn(Opcodes.LXOR)
                            visitVarInsn(Opcodes.LSTORE, 3)
                        }
                        5L -> {
                            visitVarInsn(Opcodes.ALOAD, 0)
                            visitVarInsn(Opcodes.LLOAD, 7)
                            visitLdcInsn(7L)
                            visitInsn(Opcodes.LAND)
                            visitMethodInsn(Opcodes.INVOKESTATIC, "java/lang/Long", "valueOf", "(J)Ljava/lang/Long;", false)
                            visitMethodInsn(Opcodes.INVOKEINTERFACE, "java/util/Collection", "add", "(Ljava/lang/Object;)Z", true)
                            visitInsn(Opcodes.POP)
                        }
                        6L -> {
                            visitVarInsn(Opcodes.LLOAD, 1)
                            visitVarInsn(Opcodes.LLOAD, 7)
                            visitInsn(Opcodes.L2I)
                            visitInsn(Opcodes.LSHR)
                            visitVarInsn(Opcodes.LSTORE, 3)
                        }
                        7L -> {
                            visitVarInsn(Opcodes.LLOAD, 1)
                            visitVarInsn(Opcodes.LLOAD, 7)
                            visitInsn(Opcodes.L2I)
                            visitInsn(Opcodes.LSHR)
                            visitVarInsn(Opcodes.LSTORE, 5)
                        }
                        else -> {
                            visitJumpInsn(Opcodes.GOTO, errorLabel)
                            continue
                        }
                    }
                    visitJumpInsn(Opcodes.GOTO, labels.getOrElse(ip + 2) { returnLabel })
                }
                visitLabel(returnLabel)
                visitFrame(Opcodes.F_SAME, 0, null, 0, null)
                visitVarInsn(Opcodes.ALOAD, 0)
                visitInsn(Opcodes.ARETURN)
                visitLabel(errorLabel)
                visitFrame(Opcodes.F_SAME, 0, null, 0, null)
                visitTypeInsn(Opcodes.NEW, "java/lang/IllegalStateException")
                visitInsn(Opcodes.DUP)
                visitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/IllegalStateException", "<init>", "()V", false)
                visitInsn(Opcodes.ATHROW)
                visitMaxs(5, 9)
                visitEnd()
            }
            classWriter.visitEnd()
            val lookup = MethodHandles.lookup().defineHiddenClass(classWriter.toByteArray(), true)
            return lookup.findConstructor(lookup.lookupClass(), MethodType.methodType(Void::class.javaPrimitiveType))() as Machine
        }
    }
}
