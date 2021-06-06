package files

abstract class DirectoryEntry(val parentPath: String, val name: String) {
  def path: String = {
    val separator =
      if (Directory.ROOT_PATH.equals(parentPath)) ""
      else Directory.SEPARATOR
    parentPath + separator + name
  }
  def toDirectory: Directory
  def toFile: File
  def getType: String
  def isDirectory: Boolean
  def isFile: Boolean
}
