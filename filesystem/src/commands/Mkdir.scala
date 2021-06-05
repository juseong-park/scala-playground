package commands

import files.Directory
import filesystem.State

class Mkdir(name: String) extends Command {
  def isIllegal(name: String): Boolean = name.contains(".")
  def doMkdir(state: State, name: String): State = ???

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
