package day07

sealed class ElfNode(open val size: Long)

class ElfFile(size: Long) : ElfNode(size)

data class ElfDirectory(val children: MutableMap<String, ElfNode> = mutableMapOf()): ElfNode(size = 0) {
    val descendants get(): Collection<ElfNode> = children.values + children.values.filterIsInstance<ElfDirectory>().flatMap(ElfDirectory::descendants)
    val files get() = descendants.filterIsInstance<ElfFile>()
    val directories get() = descendants.filterIsInstance<ElfDirectory>()
    override val size get() = files.sumOf { it.size }
}

fun loadFileSystemInfo(rawCommands: List<String>): ElfDirectory {
    val root = ElfDirectory()

    val gotoRootRegex = Regex("\\$ cd /")
    val goUpRegex = Regex("\\$ cd ..")
    val enterDirRegex = Regex("\\$ cd (\\w+)")
    val listRegex = Regex("\\$ ls")
    val dirInfoRegex = Regex("dir (\\w+)")
    val fileInfoRegex = Regex("(\\d+) ([\\w.]+)")

    var workingDirectory = root
    val parents: MutableList<ElfDirectory> = mutableListOf()

    rawCommands.forEach { rawCommand ->
        when {
            gotoRootRegex.matches(rawCommand) -> workingDirectory = root
            goUpRegex.matches(rawCommand) -> workingDirectory = parents.removeLast()
            enterDirRegex.matches(rawCommand) -> {
                val dirName = enterDirRegex.find(rawCommand)!!.groupValues[1]
                parents.add(workingDirectory)
                val newDirectory = ElfDirectory()
                workingDirectory.children[dirName] = newDirectory
                workingDirectory = newDirectory
            }
            listRegex.matches(rawCommand) -> { /* no-op */ }
            dirInfoRegex.matches(rawCommand) -> {
                val dirName = dirInfoRegex.find(rawCommand)!!.groupValues[1]
                workingDirectory.children[dirName] = ElfDirectory()
            }
            fileInfoRegex.matches(rawCommand) -> {
                val (rawSize, fileName) = fileInfoRegex.find(rawCommand)!!.groupValues.slice(1..2)
                workingDirectory.children[fileName] = ElfFile(size = rawSize.toLong())
            }
            else -> throw IllegalArgumentException(rawCommand)
        }
    }

    return root
}

fun part1(input: String): Long {
    val root = loadFileSystemInfo(input.trimEnd().lines())
    return root.directories.filter { it.size <= 100000 }.sumOf { it.size }
}

fun part2(input: String): Long {
    val root = loadFileSystemInfo(input.trimEnd().lines())

    val diskSpace = 70000000
    val freeSpaceGoal = 30000000
    val currentFreeSpace = diskSpace - root.size
    val needToDelete = freeSpaceGoal - currentFreeSpace

    return root.directories.filter { it.size >= needToDelete }.minOf { it.size }
}