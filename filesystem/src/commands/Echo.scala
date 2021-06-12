package commands
import files.{Directory, File}
import filesystem.State

import scala.annotation.tailrec

class Echo(args: Array[String]) extends Command {
  override def apply(state: State): State = {
    if (args.isEmpty) state
    else if (args.length == 1) state.setMessage(args.head)
    else {
      val filename = args(args.length - 1)
      val operator = args(args.length - 2)
      val contents = createContent(args, args.length - 2)

      if (">>".equals(operator))
        doEcho(state, contents, filename, append = true)
      else if  (">".equals(operator))
        doEcho(state, contents, filename, append = false)
      else
        state.setMessage(createContent(args, args.length))
    }
  }

  def createContent(args: Array[String], topIndex: Int): String = {
    @tailrec
    def createContentHelper(index: Int, acc: String): String = {
      if (index >= topIndex) acc
      else createContentHelper(index + 1, acc + " " + args(index))
    }
    createContentHelper(0, "")
  }

  def getRoot(currentDir: Directory, path: List[String], contents: String, append: Boolean): Directory = {
    if (path.isEmpty) {
      val dirEntry = currentDir.findEntry(path.head)
      if (dirEntry == null) currentDir.addEntry(new File(currentDir.path, path.head, contents))
      else if (dirEntry.isDirectory) currentDir
      else currentDir.replaceEntry(path.head, dirEntry.toFile.setContents(contents, append))
    } else {
      val nextDir = currentDir.findEntry(path.head).toDirectory
      val newNextDir = getRoot(nextDir, path.tail, contents, append)

      if (newNextDir == nextDir) currentDir
      else currentDir.replaceEntry(path.head, newNextDir)
    }
  }

  def doEcho(state: State, contents: String, filename: String, append: Boolean): State = {
    if (filename.contains(Directory.SEPARATOR))
      state.setMessage("Echo: filename must not contain separators")
    else {
      val newRoot = getRoot(state.root, state.workingDirectory.getAllFoldersInPath :+ filename, contents, append)
      if (newRoot == state.root)
        state.setMessage(filename + ": no such file")
      else {
        State(newRoot, newRoot.findDescendant(state.workingDirectory.getAllFoldersInPath))
      }
    }
  }
}
