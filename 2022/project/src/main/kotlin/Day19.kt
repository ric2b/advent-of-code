package day19

import kotlin.math.max

enum class Material { Ore, Clay, Obsidian, Geode }

typealias RobotType = Material
typealias RobotCosts = Map<RobotType, Map<Material, Int>>
typealias Resources = Map<Material, Int>
typealias Robots = Map<RobotType, Int>

data class Blueprint(val id: Int, val robotCosts: RobotCosts) {
    fun availableBuilds(resources: Resources): Set<RobotType> {
        return robotCosts
            .filterValues { costs -> costs.all { (material, cost) -> resources.getValue(material) >= cost } }.keys
    }
}

data class Inventory(
    val resources: Resources = Material.values().associateWith { 0 },
    val robots: Robots = Material.values().associateWith { if (it == Material.Ore) 1 else 0 },
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

    bestScore.clear()
    scoreCache.clear()

    val blueprintMaxGeodes: Map<Int, Int> = blueprints.associate { it.id to score(it, Inventory(), 24) }
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

    bestScore.clear()
    scoreCache.clear()

    val blueprintMaxGeodes: Map<Int, Int> = blueprints.take(3).associate { it.id to score(it, Inventory(), 32) }
    blueprintMaxGeodes.forEach { (id, geodes) -> println("$id: $geodes") }
    return blueprintMaxGeodes.values.toList().reduce { a, b -> a * b }
}

val bestScore: MutableMap<Int, Int> = mutableMapOf()
val producedIfRobotBuiltEveryMinute = Array(33) { ( it - 1 ) * it / 2 }
val scoreCache: MutableMap<Triple<Blueprint, Inventory, Int>, Int> = mutableMapOf()
fun score(blueprint: Blueprint, inventory: Inventory, minutesLeft: Int): Int {
    return scoreCache.getOrPut(Triple(blueprint, inventory, minutesLeft)) {
        val (resources, robots) = inventory

        val currentScore = resources.getValue(Material.Geode)
        bestScore[blueprint.id] = max(bestScore.getOrDefault(blueprint.id, 0), currentScore)

        val theoreticalBestScoreFromHere = currentScore + robots.getValue(Material.Geode) * minutesLeft + producedIfRobotBuiltEveryMinute[minutesLeft]
        if (minutesLeft <= 0 || theoreticalBestScoreFromHere <= bestScore.getValue(blueprint.id)) {
            return@getOrPut currentScore
        }

        val buildOptions = blueprint.availableBuilds(resources).let { robotTypes ->
            if (Material.Geode in robotTypes) { // build Geode whenever possible
                listOf(Material.Geode)
            } else {
                robotTypes.filter { material -> // If already have more of a type than needed every turn, don't build more
                    robots.getValue(material) < blueprint.robotCosts.values.maxOf { it.getOrDefault(material, 0) }
                }.plus(null) // option of not building to accumulate resources
            }
        }

        val resourcesAfterProduce = resources.keys.associateWith { resources.getValue(it) + robots.getValue(it) }

        return@getOrPut buildOptions.maxOf { robotType ->
            if (robotType == null) {
                return@maxOf score(blueprint, inventory.copy(resources = resourcesAfterProduce), minutesLeft - 1)
            }

            val resourcesAfterBuild = resourcesAfterProduce.keys.associateWith {
                resourcesAfterProduce.getValue(it) - blueprint.robotCosts.getValue(robotType).getOrDefault(it, 0)
            }

            val newInventory = inventory.copy(resources = resourcesAfterBuild, robots = robots.plus(robotType to 1 + robots.getValue(robotType)))
            score(blueprint, newInventory, minutesLeft - 1)
        }
    }
}
