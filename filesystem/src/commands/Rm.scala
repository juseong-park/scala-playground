package commands

import files.Directory
import filesystem.State

class Rm(name: String) extends Command {
  override def apply(state: State): State = {
    val workingDir = state.workingDirectory
    val absolutePath =
      if (name.startsWith(Directory.SEPARATOR)) name
      else if (workingDir.isRoot) workingDir.path + name
      else workingDir.path + Directory.SEPARATOR + name

    if (Directory.ROOT_PATH.equals(absolutePath))
      state.setMessage("command not supported")
    else
      remove(state, absolutePath)
  }

  def remove(state: State, path: String): State = {
    def removeHelper(currentDir: Directory, path: List[String]): Directory = {
      if (path.isEmpty) currentDir
      else if (path.tail.isEmpty) currentDir.removeEntry(path.head)
      else {
        val nextDir = currentDir.findEntry(path.head)
        if (!nextDir.isDirectory) currentDir
        else {
          val newNextDir = removeHelper(nextDir.toDirectory, path.tail)
          if (newNextDir == nextDir) currentDir
          else currentDir.replaceEntry(path.head, newNextDir)
        }
      }
    }

    val tokens = path.substring(1).split(Directory.SEPARATOR).toList
    val newRoot = removeHelper(state.root, tokens)

    if (newRoot == state.root) {
      state.setMessage(path + ": no such file or directory")
    } else
      State(newRoot, newRoot.findDescendant(state.workingDirectory.path.substring(1)))
  }
}
