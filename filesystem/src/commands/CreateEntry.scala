package commands

import files.{Directory, DirectoryEntry}
import filesystem.State

abstract class CreateEntry(name: String) extends Command {
  override def apply(state: State): State = {
    val workingDir = state.workingDirectory
    if (workingDir.hasEntry(name)) {
      state.setMessage(s"Entry $name already exists")
    } else if (name.contains(Directory.SEPARATOR)) {
      state.setMessage(s"$name must not contain separator")
    } else if (isIllegal(name)) {
      state.setMessage(s"$name: illegal entry name")
    } else {
      doCreateEntry(state, name)
    }
  }

  def isIllegal(name: String): Boolean = name.contains(".")

  def doCreateEntry(state: State, name: String): State = {
    def updateStructures(currentDir: Directory, path: List[String], newEntry: DirectoryEntry): Directory = {
      if (path.isEmpty) currentDir.addEntry(newEntry)
      else {
        val oldEntry = currentDir.findEntry(path.head).toDirectory
        currentDir.replaceEntry(oldEntry.name, updateStructures(oldEntry, path.tail, newEntry))
      }
    }

    val workingDir = state.workingDirectory
    val allDirsInPath = workingDir.getAllFoldersInPath
    val newEntry = createEntry(state)
    val newRoot = updateStructures(state.root, allDirsInPath, newEntry)
    val newWorkingDir = newRoot.findDescendant(allDirsInPath)

    State(newRoot, newWorkingDir)
  }

  def createEntry(state: State): DirectoryEntry
}
