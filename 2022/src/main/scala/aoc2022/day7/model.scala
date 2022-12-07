package aoc2022.day7

object model {

  trait Input
  trait FSItem:
    def name: String
  trait Command                            extends Input
  trait Output                             extends Input
  case class Cd(dir: String)               extends Command
  case object Ls                           extends Command
  case class File(name: String, size: Long) extends Output with FSItem
  case class Dir(name: String, files: List[File] = Nil, subdirs: List[Dir] = Nil) extends Output with FSItem:

    lazy val cumSize: Long = files.map(_.size).sum + subdirs.map(_.cumSize).sum
    def filterDirs(f: Dir => Boolean): List[Dir] = subdirs.filter(f) ++ subdirs.flatMap(_.filterDirs(f))


  case class FSStack(fsMap: List[(String, FSItem)] = List.empty, path: List[String] = List.empty):
    lazy val up: FSStack               = FSStack(fsMap, path.dropRight(1))
    lazy val pathStr: String           = path.mkString(FSStack.sep)
    lazy val init: FSStack             = FSStack(List(("/", Dir("/"))), List("/"))
    def down(dirName: String): FSStack = FSStack(fsMap, path :+ dirName)
    def add(fsItem: FSItem): FSStack   = FSStack(fsMap :+ (pathStr -> fsItem), path)

  object FSStack:
    val sep:String = "-"

    def traverseInput(fsStack: FSStack, input: Input): FSStack =
      input match {
        case Cd("/")      => fsStack.init
        case Cd("..")     => fsStack.up
        case Cd(dirName)  => fsStack.down(dirName)
        case Ls           => fsStack
        case item: FSItem => fsStack.add(item)
      }

    def toRootDir(stack: FSStack): Dir =
      stack
        .fsMap
        .groupMapReduce(_._1.split(sep).toList.reverse)(i => List(i._2))(_ ++ _)
        .toList
        .sortBy(-_._1.length)
        .foldLeft((Map.empty[String, Dir], Option.empty[Dir])) {
          case ((dirIndex, _), (path, fsItems)) =>
            val files = fsItems.collect{ case f: File => f }
            val dirs = fsItems.collect{ case d: Dir => d }.map(d => (d.name :: path).mkString(sep)).flatMap(dirIndex.get)
            val dir = Dir(path.head,files,dirs)
            if(path == List("/")) (Map.empty, Some(dir))
            else (dirIndex + (path.mkString(sep) -> dir), None)
        }._2.get


}
