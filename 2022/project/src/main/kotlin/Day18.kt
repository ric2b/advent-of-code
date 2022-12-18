package day18

data class Cube(val x: Int, val y: Int, val z: Int) {
    fun neighbors() = listOf(copy(x = x+1), copy(x = x-1), copy(y = y+1), copy(y = y-1), copy(z = z+1), copy(z = z-1))
    fun neighborsInRange(ranges: List<IntRange>) = neighbors().filter { it.x in ranges[0] && it.y in ranges[1] && it.z in ranges[2] }
    fun stateIn(grid: Grid) = grid[x][y][z]
    fun setStateIn(grid: Grid, state: State) = state.let { grid[x][y][z] = it }
}

fun surfaceArea(cubes: Collection<Cube>) = cubes.sumOf { cube -> cube.neighbors().count { it !in cubes } }

fun part1(input: String): Int {
    val cubes: Set<Cube> = input.trimEnd().lines().map { it.split(",").map(String::toInt) }
        .map { (x, y, z) -> Cube(x, y, z) }.toSet()

    return surfaceArea(cubes)
}

enum class State { LAVA, AIR, TRAPPED }
typealias Grid = Array<Array<Array<State>>>

fun findTrappedAir(grid: Grid, ranges: List<IntRange>): List<Cube> {
    val queue: MutableList<Cube> = mutableListOf(Cube(0, 0, 0))

    while (queue.isNotEmpty()) {
        val current = queue.removeFirst()
        current.setStateIn(grid, State.AIR)
        current.neighborsInRange(ranges).forEach { neighbor ->
            if (neighbor.stateIn(grid) == State.TRAPPED) {
                neighbor.setStateIn(grid, State.AIR)
                queue.add(neighbor)
            }
        }
    }

    return ranges[0].flatMap { x -> ranges[1].flatMap { y -> ranges[2].map { z -> Cube(x, y, z) } } }
        .filter { it.stateIn(grid) == State.TRAPPED }
}

fun part2(input: String): Int {
    val lava: Set<Cube> = input.trimEnd().lines().map { it.split(",").map(String::toInt) }
        .map { (x, y, z) -> Cube(x, y, z) }.toSet()

    val ranges = listOf(0..lava.maxOf { it.x } + 1, 0..lava.maxOf { it.y } + 1, 0..lava.maxOf { it.z } + 1)

    val grid: Grid = Array(ranges[0].count()) { x ->
        Array(ranges[1].count()) { y ->
            Array(ranges[2].count()) { z -> if (Cube(x, y, z) in lava) State.LAVA else State.TRAPPED }
        }
    }

    return surfaceArea(lava) - surfaceArea(findTrappedAir(grid, ranges))
}
