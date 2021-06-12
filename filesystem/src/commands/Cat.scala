package commands
import filesystem.State

class Cat(filename: String) extends Command {
  override def apply(state: State): State = {
    val workingDir = state.workingDirectory

    val dirEntry = workingDir.findEntry(filename)
    if (dirEntry == null || !dirEntry.isFile) state.setMessage(filename + ": no such file")
    else state.setMessage(dirEntry.toFile.contents)
  }
}
