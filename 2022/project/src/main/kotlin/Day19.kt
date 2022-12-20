package day19

import kotlin.math.max
import kotlin.math.min

enum class Material { Geode, Obsidian, Clay, Ore }

typealias RobotType = Material
typealias RobotCosts = Map<RobotType, Map<Material, Int>>
typealias Resources = Map<Material, Int>
typealias Robots = Map<RobotType, Int>

data class Blueprint(val id: Int, val robotCosts: RobotCosts) {
    fun availableBuilds(ownedRobots: Robots): Set<RobotType> {
        return robotCosts.filterValues { costs -> costs.keys.all { material -> ownedRobots.getValue(material) > 0 } }.keys
    }
}

data class Inventory(
    val resources: Resources = Material.values().associateWith { 0 },
    val robots: Robots = mapOf(Material.Obsidian to 0, Material.Clay to 0, Material.Ore to 1),
)

private val inputRegex = Regex("Blueprint (\\d+): Each ore robot costs (\\d+) ore. Each clay robot costs (\\d+) ore. Each obsidian robot costs (\\d+) ore and (\\d+) clay. Each geode robot costs (\\d+) ore and (\\d+) obsidian.")

fun part1(input: String): Int {
    val blueprints = input.trimEnd().lines().map {
        val matches = inputRegex.findAll(it).map(MatchResult::groupValues).first().drop(1).map(String::toInt)

        Blueprint(
            id = matches[0],
            robotCosts = mapOf(
                Material.Geode to mapOf(Material.Ore to matches[5], Material.Obsidian to matches[6]),
                Material.Obsidian to mapOf(Material.Ore to matches[3], Material.Clay to matches[4]),
                Material.Clay to mapOf(Material.Ore to matches[2]),
                Material.Ore to mapOf(Material.Ore to matches[1]),
            ),
        )
    }

    val blueprintMaxGeodes: Map<Int, Int> = blueprints.associate {
        bestScore = 0
        scoreCache.clear()
        it.id to score(it, Inventory(), 24)
    }

    blueprintMaxGeodes.forEach { (id, geodes) -> println("$id: $geodes") }
    return blueprintMaxGeodes.map { (id, geodes) -> id * geodes }.sum()
}

fun part2(input: String): Int {
    val blueprints = input.trimEnd().lines().map {
        val matches = inputRegex.findAll(it).map(MatchResult::groupValues).first().drop(1).map(String::toInt)

        Blueprint(
            id = matches[0],
            robotCosts = mapOf(
                Material.Geode to mapOf(Material.Ore to matches[5], Material.Obsidian to matches[6]),
                Material.Obsidian to mapOf(Material.Ore to matches[3], Material.Clay to matches[4]),
                Material.Clay to mapOf(Material.Ore to matches[2]),
                Material.Ore to mapOf(Material.Ore to matches[1]),
            ),
        )
    }

    val blueprintMaxGeodes: Map<Int, Int> = blueprints.take(3).associate {
        bestScore = 0
        scoreCache.clear()
        it.id to score(it, Inventory(), 32)
    }

    blueprintMaxGeodes.forEach { (id, geodes) -> println("$id: $geodes") }
    return blueprintMaxGeodes.values.toList().reduce { a, b -> a * b }
}

data class StateKey(val minutesLeft: Int, val inventory: Collection<Int>)

var bestScore: Int = 0
val producedIfRobotBuiltEveryMinute = Array(33) { ( it - 1 ) * it / 2 }
val scoreCache: MutableMap<StateKey, Int> = HashMap(3_000_000)
fun score(blueprint: Blueprint, inventory: Inventory, minutesLeft: Int): Int {
    val stateKey = StateKey(minutesLeft, inventory.resources.values + inventory.robots.values)

    return scoreCache.getOrPut(stateKey) {
        val (resources, robots) = inventory

        val currentScore = resources.getValue(Material.Geode)
        bestScore = max(bestScore, currentScore)

        if (minutesLeft <= 0 || currentScore + producedIfRobotBuiltEveryMinute[minutesLeft] <= bestScore) {
            return@getOrPut currentScore
        }

        val buildOptions = blueprint.availableBuilds(robots).let { robotTypes ->
            robotTypes.filter { material ->
                // If already have more inventory and production than needed for the rest of time, don't build more
                material == Material.Geode || blueprint.robotCosts.values.any {
                    resources.getValue(material) + minutesLeft * robots.getValue(material) <  minutesLeft * it.getOrDefault(material, 0)
                }
            }
        }

        return@getOrPut buildOptions.maxOf { robotType ->
            val timeWaiting = blueprint.robotCosts.getValue(robotType).maxOf { (material, cost) ->
                if (cost <= resources.getValue(material)) {
                    0
                } else {
                    val missingResources = cost - resources.getValue(material)
                    val production = robots.getValue(material)
                    (missingResources / production) + if (missingResources % production == 0) 0 else 1
                }
            }

            val resourcesAfterProduce = Material.values().associateWith {
                resources.getValue(it) + min(minutesLeft, timeWaiting + 1) * robots.getOrDefault(it, 0)
            }

            if (minutesLeft < timeWaiting + 1) {
                return@maxOf currentScore
            }

            val resourcesAfterBuild = Material.values().associateWith {
                if (it == Material.Geode) {
                    resourcesAfterProduce.getValue(it) + if (robotType == Material.Geode) minutesLeft - timeWaiting - 1 else 0
                } else {
                    resourcesAfterProduce.getValue(it) - blueprint.robotCosts.getValue(robotType).getOrDefault(it, 0)
                }
            }

            if (robotType == Material.Geode) {
                return@maxOf score(blueprint, inventory.copy(resources = resourcesAfterBuild), minutesLeft - timeWaiting - 1)
            }

            val newInventory = inventory.copy(resources = resourcesAfterBuild, robots = robots.plus(robotType to 1 + robots.getValue(robotType)))
            score(blueprint, newInventory, minutesLeft - timeWaiting - 1)
        }
    }
}
