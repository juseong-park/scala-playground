package commands

import files.{Directory, DirectoryEntry}
import filesystem.State

import scala.annotation.tailrec

class Cd(dir: String) extends Command {
  override def apply(state: State): State = {
    val root = state.root
    val workingDir = state.workingDirectory

    val absolutePath = {
      if (dir.startsWith(Directory.SEPARATOR)) dir
      else if (workingDir.isRoot) workingDir + dir
      else workingDir.path + Directory.SEPARATOR + dir
    }

    val destination = findEntry(root, absolutePath)
    if (destination == null || !destination.isDirectory)
      state.setMessage(dir + ": no such directory")
    else
      State(root, destination.toDirectory)
  }

  def findEntry(root: Directory, path: String): DirectoryEntry = {
    @tailrec
    def findEntryHelper(currentDir: Directory, path: List[String]): DirectoryEntry = {
      if (path.isEmpty || path.head.isEmpty)  currentDir
      else if (path.tail.isEmpty) currentDir.findEntry(path.head)
      else {
        val nextDir = currentDir.findEntry(path.head)
        if (nextDir == null || !nextDir.isDirectory) null
        else findEntryHelper(nextDir.toDirectory, path.tail)
      }
    }

    val tokens = path.substring(1).split(Directory.SEPARATOR).toList
    findEntryHelper(root, tokens)
  }
}
