import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.io.File

class Day05KtTest {
    @Test
    fun testPart1() {
        val testInput =
            """
                    [D]    
                [N] [C]    
                [Z] [M] [P]
                 1   2   3 
                
                move 1 from 2 to 1
                move 3 from 1 to 3
                move 2 from 2 to 1
                move 1 from 1 to 2
            """.trimIndent()

        assertEquals("CMZ", day05.part1(testInput))
    }

    @Test
    fun testPart1MyInput() {
        val myInput = this::class.java.classLoader.getResource("day05.txt")!!.readText()
        assertEquals("GFTNRBZPF", day05.part1(myInput))
    }

    @Test
    fun testPart2MyInput() {
        val myInput = this::class.java.classLoader.getResource("day05.txt")!!.readText()
        assertEquals("VRQWPDSGP", day05.part2(myInput))
    }
}