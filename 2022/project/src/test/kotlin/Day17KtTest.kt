import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class Day17KtTest {
    private val testInput =
        """
            >>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>
        """.trimIndent()
    val myInput = this::class.java.classLoader.getResource("day17.txt")!!.readText()

    @Test
    fun `test part 1 with example`() = assertEquals(3068, day17.part1(testInput))

    @Test
    fun `test part 1 with my input`() = assertEquals(3109, day17.part1(myInput))

    @Test
    fun `test part 2 with example`() = assertEquals(1514285714288, day17.part2(testInput))

    @Test
    fun `test part 2 with my input`() = assertEquals(1541449275365, day17.part2(myInput))
}
