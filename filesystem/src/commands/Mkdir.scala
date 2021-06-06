package commands

import files.{Directory, DirectoryEntry}
import filesystem.State

class Mkdir(name: String) extends Command {
  def isIllegal(name: String): Boolean = name.contains(".")
  def doMkdir(state: State, name: String): State = {
    def updateStructures(currentDir: Directory, path: List[String], newEntry: DirectoryEntry): Directory = {
      if (path.isEmpty) currentDir.addEntry(newEntry)
      else {
        val oldEntry = currentDir.findEntry(path.head).toDirectory
        currentDir.replaceEntry(oldEntry.name, updateStructures(oldEntry, path.tail, newEntry))
      }
    }

    val workingDir = state.workingDirectory
    val allDirsInPath = workingDir.getAllFoldersInPath
    val newDir = Directory.empty(workingDir.path, name)

    val newRoot = updateStructures(state.root, allDirsInPath, newDir)
    val newWorkingDir = newRoot.findDescendant(allDirsInPath)

    State(newRoot, newWorkingDir)
  }

  override def apply(state: State): State = {
    val workingDir = state.workingDirectory
    if (workingDir.hasEntry(name)) {
      state.setMessage(s"Entry $name already exists")
    } else if (name.contains(Directory.SEPARATOR)) {
      state.setMessage(s"$name must not contain separator")
    } else if (isIllegal(name)) {
      state.setMessage(s"$name: illegal entry name")
    } else {
      doMkdir(state, name)
    }
  }
}
