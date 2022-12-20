package day20

import java.lang.Math.floorMod

fun part1(input: String): Int {
    val encryptedMessage: List<Int> = input.trimEnd().lines().map(String::toInt)

    val indexesAsId = encryptedMessage.indices.toMutableList()

    encryptedMessage.forEachIndexed { id, n ->
        val i = indexesAsId.indexOf(id)
        indexesAsId.remove(id)
        indexesAsId.add(floorMod(i + n, indexesAsId.size), id)
    }

    val izero = indexesAsId.indexOf(encryptedMessage.indexOf(0))
    val x = encryptedMessage[indexesAsId[(izero + 1000) % indexesAsId.size]]
    val y = encryptedMessage[indexesAsId[(izero + 2000) % indexesAsId.size]]
    val z = encryptedMessage[indexesAsId[(izero + 3000) % indexesAsId.size]]

    return x + y + z
}

fun part2(input: String): Long {
    val key = 811589153L
    val encryptedMessage: List<Long> = input.trimEnd().lines().map(String::toLong).map { it * key }

    val indexesAsId = encryptedMessage.indices.toMutableList()

    repeat(10) {
        encryptedMessage.forEachIndexed { id, n ->
            val i = indexesAsId.indexOf(id)
            indexesAsId.remove(id)
            indexesAsId.add(floorMod(i + n, indexesAsId.size), id)
        }
    }

    val izero = indexesAsId.indexOf(encryptedMessage.indexOf(0))
    val x = encryptedMessage[indexesAsId[(izero + 1000) % indexesAsId.size]]
    val y = encryptedMessage[indexesAsId[(izero + 2000) % indexesAsId.size]]
    val z = encryptedMessage[indexesAsId[(izero + 3000) % indexesAsId.size]]

    return x + y + z
}
