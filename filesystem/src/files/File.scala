package files

import filesystem.FileSystemException

class File(override val parentPath: String, override val name: String, val contents: String)
  extends DirectoryEntry(parentPath, name) {
  override def toDirectory: Directory =
    throw new FileSystemException("A file cannot be converted to a directory")

  override def toFile: File = this

  override def getType: String = "File"

  override def isDirectory: Boolean = false

  override def isFile: Boolean = true

  def setContents(newContents: String, append: Boolean): File =
    if (append) new File(parentPath, name, contents + "\n" + newContents)
    else new File(parentPath, name, newContents)
}

object File {
  def empty(parentPath: String, name: String): File = new File(parentPath, name, "")
}
