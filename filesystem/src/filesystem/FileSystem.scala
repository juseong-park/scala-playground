package filesystem

import commands.Command
import files.Directory

object FileSystem extends App {
  val root = Directory.ROOT
  io.Source.stdin.getLines().foldLeft(State(root, root))((state, line) => {
    state.show()
    Command.from(line).apply(state)
  })
}
