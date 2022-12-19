import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Disabled
import org.junit.jupiter.api.Test

@Disabled
class Day19KtTest {
    private val testInput =
        """
            Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
            Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
        """.trimIndent()
    val myInput = this::class.java.classLoader.getResource("day19.txt")!!.readText()

    @Test
    fun `test part 1 with example`() = assertEquals(33, day19.part1(testInput))

    @Test
    fun `test part 1 with my input`() = assertEquals(1719, day19.part1(myInput))

    @Test
    fun `test part 2 with example`() = assertEquals(3472, day19.part2(testInput))

    @Test
    fun `test part 2 with my input`() = assertEquals(19530, day19.part2(myInput))
}
