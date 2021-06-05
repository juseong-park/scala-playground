package files

class Directory(override val parentPath: String, override val name: String, val contents: List[DirectoryEntry])
  extends DirectoryEntry(parentPath, name) {
  def hasEntry(name: String): Boolean = ???
}

object Directory {
  val ROOT_PATH = "/"
  val SEPARATOR = "/"
  def ROOT: Directory = Directory.empty("", "");

  def empty(parentPath: String, name: String) = new Directory(parentPath, name, List())
}