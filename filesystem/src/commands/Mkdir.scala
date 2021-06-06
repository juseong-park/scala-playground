package commands

import files.{Directory, DirectoryEntry}
import filesystem.State

class Mkdir(name: String) extends CreateEntry(name) {
  override def createEntry(state: State): DirectoryEntry =
    Directory.empty(state.workingDirectory.path, name)
}
