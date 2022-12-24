package day12

data class Pos(val y: Int, val x: Int) {
    fun neighbours(heightMap: List<List<Int>>): List<Pos> {
        val height = heightMap[y][x]
        return rawNeighbours().filter { it.isValid(heightMap) && heightMap[it.y][it.x] <= height + 1 }
    }

    private fun isValid(heightMap: List<List<Any>>): Boolean {
        return y in 0..heightMap.lastIndex && x in 0..heightMap[y].lastIndex
    }

    private fun rawNeighbours(): List<Pos> {
        return listOf(
            Pos(y, x - 1),
            Pos(y, x + 1),
            Pos(y - 1, x),
            Pos(y + 1, x),
        )
    }
}

fun djikstra(heightMap: List<List<Int>>, start: Pos?, goal: Pos): List<Pos> {
    val queue: MutableSet<Pos> = mutableSetOf()
    val distance: MutableMap<Pos, Int> = if (start != null) mutableMapOf(start to 0) else mutableMapOf()
    val previous: MutableMap<Pos, Pos> = mutableMapOf()

    heightMap.forEachIndexed { row, line ->
        line.forEachIndexed { column, height ->
            queue.add(Pos(row, column))
            if (start == null && height == 0) distance[Pos(row, column)] = 0
        }
    }

    while (queue.isNotEmpty()) {
        val u = queue.minByOrNull { distance.getOrDefault(it, Int.MAX_VALUE) }!!
        queue.remove(u)

        u.neighbours(heightMap).filter { it in queue }.forEach { v ->
            if (u in distance) {
                val cost = distance.getValue(u) + 1
                if (cost < distance.getOrDefault(v, Int.MAX_VALUE)) {
                    distance[v] = cost
                    previous[v] = u
                }
            }
        }
    }

    val path: MutableList<Pos> = mutableListOf()
    var u: Pos? = goal
    while (u != null) {
        path.add(0, u)
        u = previous[u]
    }

    return path
}

fun part1(input: String): Int {
    val rawMap: List<List<Char>> = input.trimEnd().lines().map { it.toList() }
    var start = Pos(0, 0)
    var goal = Pos(0, 0)
    val heightMap: List<List<Int>> = rawMap.mapIndexed { row, line ->
        line.mapIndexed { column, char ->
            when (char) {
                'S' -> { start = Pos(row, column); 'a'.code - 'a'.code }
                'E' -> { goal = Pos(row, column); 'z'.code - 'a'.code }
                else -> char.code - 'a'.code
            }
        }
    }

    val daWae = djikstra(heightMap, start, goal)
    if (daWae.isEmpty()) throw IllegalArgumentException("You do not know da wae")
    return daWae.size - 1
}

fun part2(input: String): Int {
    val rawMap: List<List<Char>> = input.trimEnd().lines().map { it.toList() }
    var goal = Pos(0, 0)
    val heightMap: List<List<Int>> = rawMap.mapIndexed { row, line ->
        line.mapIndexed { column, char ->
            when (char) {
                'S' -> 'a'.code - 'a'.code
                'E' -> { goal = Pos(row, column); 'z'.code - 'a'.code }
                else -> char.code - 'a'.code
            }
        }
    }

    val daWae = djikstra(heightMap, start = null, goal)
    if (daWae.isEmpty()) throw IllegalArgumentException("You do not know da wae")
    return daWae.size - 1
}