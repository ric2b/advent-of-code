import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class Day12KtTest {
    private val testInput =
        """
            Sabqponm
            abcryxxl
            accszExk
            acctuvwj
            abdefghi
        """.trimIndent()
    val myInput = this::class.java.classLoader.getResource("day12.txt")!!.readText()

    @Test
    fun `test part 1 with example`() = assertEquals(31, day12.part1(testInput))

    @Test
    fun `test part 1 with my input`() = assertEquals(420, day12.part1(myInput))

    @Test
    fun `test part 2 with example`() = assertEquals(29, day12.part2(testInput))

    @Test
    fun `test part 2 with my input`() = assertEquals(414, day12.part2(myInput))
}
