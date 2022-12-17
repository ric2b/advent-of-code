package day17

import kotlin.math.max

data class Pos(val x: Int, val y: Long)

data class Rock(val pointOffsets: List<Pos>, var bottomLeft: Pos) {
    fun points(): List<Pos> = pointOffsets.map { Pos(bottomLeft.x + it.x, bottomLeft.y + it.y) }
    fun xEdges() = listOf(bottomLeft.x, bottomLeft.x + pointOffsets.maxOf { it.x })
    fun intersecting(other: Rock): Boolean = points().any { point -> other.points().any { point == it } }
}

fun part1(input: String) = tetris(input)
fun part2(input: String) = tetris(input, spawnRocks = 1_000_000_000_000)

private val rockPatterns: List<List<Pos>> = listOf(
    (0..3).map { Pos(it, 0) },
    listOf(Pos(0, 1), Pos(1, 1), Pos(2, 1), Pos(1, 2), Pos(1, 0)),
    listOf(Pos(0, 0), Pos(1, 0), Pos(2, 0), Pos(2, 1), Pos(2, 2)),
    (0..3L).map { Pos(0, it) },
    listOf(Pos(0, 0), Pos(1, 0), Pos(0, 1), Pos(1, 1)),
)

typealias PatternIndex = Int
typealias JetIndex = Int
typealias RockIndex = Long
typealias Height = Long

fun tetris(input: String, spawnRocks: Long = 2022): Long {
    val jets: List<Int> = input.trimEnd().map { if (it == '>') 1 else -1 }

    val restedRocks: MutableList<Rock> = mutableListOf()
    var patternIndex = 0
    var jetIndex = 0
    val columnHeights = MutableList(7) { -1L }

    val lastSeenAt: MutableMap<Pair<PatternIndex, JetIndex>, Pair<RockIndex, Height>> = mutableMapOf()

    for (rockIndex in 0 until spawnRocks) {
        val currentHeight = columnHeights.maxOrNull()!! + 1L

        val key = Pair(patternIndex, jetIndex)
        if (lastSeenAt.containsKey(key)) {
            val (prevRockIndex, prevHeight) = lastSeenAt.getValue(key)
            val repetitions: Long = (spawnRocks - rockIndex) / (prevRockIndex - rockIndex)
            val remainder: Long = (spawnRocks - rockIndex) % (prevRockIndex - rockIndex)
            val patternPeriod = prevHeight - currentHeight
            if (remainder == 0L) {
                return currentHeight + patternPeriod * repetitions
            }
        } else {
            lastSeenAt[key] = Pair(rockIndex, currentHeight)
        }

        val rock = Rock(rockPatterns[patternIndex], Pos(2, currentHeight + 3L))
        patternIndex = (patternIndex + 1) % rockPatterns.size

        while (true) {
            val simRockJet = rock.copy(bottomLeft = rock.bottomLeft.copy(x = rock.bottomLeft.x + jets[jetIndex]))
            jetIndex = (jetIndex + 1) % jets.size

            if (simRockJet.xEdges().all { it in 0..6 } && restedRocks.none { simRockJet.intersecting(it) }) {
                rock.bottomLeft = simRockJet.bottomLeft
            }

            val simRockDrop = rock.copy(bottomLeft = rock.bottomLeft.copy(y = rock.bottomLeft.y - 1))

            if (simRockDrop.bottomLeft.y < 0 || restedRocks.any { simRockDrop.intersecting(it) }) {
                restedRocks.add(0, rock)
                rock.points().forEach { columnHeights[it.x] = max(columnHeights[it.x], it.y) }

                // To make things faster, filter out rocks that are below the lowest column.
                // TODO: For some reason this only works when removing if below the lowest column - 4, not sure why the magic number
                restedRocks.removeIf { restedRock -> restedRock.bottomLeft.y < columnHeights.minOrNull()!! - 4 }
                break
            }

            rock.bottomLeft = simRockDrop.bottomLeft
        }
    }

    return columnHeights.maxOrNull()!! + 1L
}
