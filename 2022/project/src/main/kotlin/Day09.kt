package day09

data class Pos(val x: Int, val y: Int) {
    operator fun plus(b: Pos) = Pos(x + b.x, y + b.y)

    fun touching(b: Pos): Boolean {
        (-1..1).forEach { dx ->
            (-1..1).forEach { dy ->
                if (b.x == x+dx && b.y == y+dy) return true
            }
        }

        return false
    }
}

fun part1(input: String): Int {
    val movements: List<String> = input.trimEnd().lines().flatMap { line ->
        val (_, direction, amount) = Regex("(\\w) (\\d+)").find(line)!!.groupValues
        List(amount.toInt()) { direction }
    }

    var headPosition = Pos(0, 0)
    var tailPosition = Pos(0, 0)
    val visitedByTail: MutableSet<Pos> = mutableSetOf(tailPosition)

    movements.forEach { direction ->
        when (direction) {
            "U" -> headPosition += Pos(0, 1)
            "D" -> headPosition += Pos(0, -1)
            "L" -> headPosition += Pos(-1, 0)
            "R" -> headPosition += Pos(1, 0)
        }

        if (!headPosition.touching(tailPosition)) {
            if (tailPosition.x != headPosition.x) {
                tailPosition += if (headPosition.x < tailPosition.x) Pos(-1, 0) else Pos(1, 0)
            }
            if (tailPosition.y != headPosition.y) {
                tailPosition += if (headPosition.y < tailPosition.y) Pos(0, -1) else Pos(0, 1)
            }
            visitedByTail.add(tailPosition)
        }
    }
    return visitedByTail.size
}

fun part2(input: String): Int {
    val movements: List<String> = input.trimEnd().lines().flatMap { line ->
        val (_, direction, amount) = Regex("(\\w) (\\d+)").find(line)!!.groupValues
        List(amount.toInt()) { direction }
    }

    val ropePositions: MutableList<Pos> = MutableList(10) { Pos(0, 0) }
    val visitedByTail: MutableSet<Pos> = mutableSetOf(ropePositions.last())

    movements.forEach { direction ->
        when (direction) {
            "U" -> ropePositions[0] += Pos(0, 1)
            "D" -> ropePositions[0] += Pos(0, -1)
            "L" -> ropePositions[0] += Pos(-1, 0)
            "R" -> ropePositions[0] += Pos(1, 0)
        }

        for (i in 1 until ropePositions.size) {
            val relativeHead = ropePositions[i - 1]
            val section = ropePositions[i]

            if (!section.touching(relativeHead)) {
                if (section.x != relativeHead.x) {
                    ropePositions[i] += if (relativeHead.x < section.x) Pos(-1, 0) else Pos(1, 0)
                }
                if (section.y != relativeHead.y) {
                    ropePositions[i] += if (relativeHead.y < section.y) Pos(0, -1) else Pos(0, 1)
                }
            }
        }

        visitedByTail.add(ropePositions.last())
    }

    return visitedByTail.size
}