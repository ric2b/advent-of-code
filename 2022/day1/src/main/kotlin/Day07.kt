package day07

data class ElfFile(val size: Long)
data class ElfDirectory(
    val parent: ElfDirectory?,
    val files: MutableMap<String, ElfFile> = mutableMapOf(),
    val directories: MutableMap<String, ElfDirectory> = mutableMapOf()
) {
    val size get() = allFiles(this).sumOf { it.size }
}

fun allFiles(directory: ElfDirectory): List<ElfFile> {
    return directory.files.values + directory.directories.values.flatMap { allFiles(it) }
}

fun allDirectories(directory: ElfDirectory): List<ElfDirectory> {
    return listOf(directory) + directory.directories.values.flatMap { allDirectories(it) }
}

fun loadFileSystemInfo(rawCommands: List<String>): ElfDirectory {
    val root = ElfDirectory(parent = null)

    val gotoRootRegex = Regex("\\$ cd /")
    val goUpRegex = Regex("\\$ cd ..")
    val enterDirRegex = Regex("\\$ cd (\\w+)")
    val listRegex = Regex("\\$ ls")
    val dirInfoRegex = Regex("dir (\\w+)")
    val fileInfoRegex = Regex("(\\d+) ([\\w.]+)")

    var workingDirectory = root

    rawCommands.forEach { rawCommand ->
        when {
            gotoRootRegex.matches(rawCommand) -> workingDirectory = root
            goUpRegex.matches(rawCommand) -> workingDirectory = workingDirectory.parent!!
            enterDirRegex.matches(rawCommand) -> {
                val dirName = enterDirRegex.find(rawCommand)!!.groupValues[1]
                val newDirectory = ElfDirectory(parent = workingDirectory)
                workingDirectory.directories[dirName] = newDirectory
                workingDirectory = newDirectory
            }
            listRegex.matches(rawCommand) -> { /* no-op */ }
            dirInfoRegex.matches(rawCommand) -> {
                val dirName = dirInfoRegex.find(rawCommand)!!.groupValues[1]
                val newDirectory = ElfDirectory(parent = workingDirectory)
                workingDirectory.directories[dirName] = newDirectory
            }
            fileInfoRegex.matches(rawCommand) -> {
                val (rawSize, fileName) = fileInfoRegex.find(rawCommand)!!.groupValues.slice(1..2)
                workingDirectory.files[fileName] = ElfFile(size = rawSize.toLong())
            }
            else -> throw IllegalArgumentException(rawCommand)
        }
    }

    return root
}

fun part1(input: String): Long {
    val root = loadFileSystemInfo(input.trimEnd().lines())
    return allDirectories(root).filter { it.size <= 100000 }.sumOf { it.size }
}

fun part2(input: String): Long {
    val root = loadFileSystemInfo(input.trimEnd().lines())

    val diskSpace = 70000000
    val freeSpaceGoal = 30000000
    val currentFreeSpace = diskSpace - root.size
    val needToDelete = freeSpaceGoal - currentFreeSpace

    return allDirectories(root).filter { it.size >= needToDelete }.minOf { it.size }
}