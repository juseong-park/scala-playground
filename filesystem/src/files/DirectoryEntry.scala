package files

abstract class DirectoryEntry(val parentPath: String, val name: String) {
  def path: String = parentPath + Directory.SEPARATOR + name
  def toDirectory: Directory
  def toFile: File
  def getType: String
}
