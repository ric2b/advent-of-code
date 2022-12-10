import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class Day02KtTest {
    private val testInput =
        """
            A Y
            B X
            C Z
        """.trimIndent()
    private val myInput = this::class.java.classLoader.getResource("day02.txt")!!.readText()

    @Test
    fun `test part 1 with example`() = assertEquals(15, day02.part1(testInput))

    @Test
    fun `test part 1 with my input`() = assertEquals(15523, day02.part1(myInput))

    @Test
    fun `test part 2 with example`() = assertEquals(12, day02.part2(testInput))

    @Test
    fun `test part 2 with my input`() = assertEquals(15702, day02.part2(myInput))
}