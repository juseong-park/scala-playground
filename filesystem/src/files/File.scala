package files

import filesystem.FileSystemException

class File(override val parentPath: String, override val name: String, contents: String)
  extends DirectoryEntry(parentPath, name) {
  override def toDirectory: Directory =
    throw new FileSystemException("A file cannot be converted to a directory")

  override def toFile: File = this

  override def getType: String = "File"
}

object File {
  def empty(parentPath: String, name: String): File = new File(parentPath, name, "")
}
