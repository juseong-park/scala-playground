package commands

import files.{DirectoryEntry, File}
import filesystem.State

class Touch(name: String) extends CreateEntry(name) {
  override def createEntry(state: State): DirectoryEntry =
    File.empty(state.workingDirectory.parentPath, name)
}
