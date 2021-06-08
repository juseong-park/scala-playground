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

    @tailrec
    def collapseRelativeTokens(path: List[String], result: List[String]): List[String] = {
      if (path.isEmpty) result
      else if (".".equals(path.head)) collapseRelativeTokens(path.tail, result)
      else if ("..".equals(path.head)) {
        if (result.isEmpty) null
        else collapseRelativeTokens(path.tail, result.init)
      } else collapseRelativeTokens(path.tail, result :+ path.head)
    }

    val tokens = path.substring(1).split(Directory.SEPARATOR).toList
    val newTokens = collapseRelativeTokens(tokens, List())
    if (newTokens == null) null
    else findEntryHelper(root, newTokens)
  }
}
