//> using lib "com.lihaoyi::os-lib:0.8.1"
//> using lib "com.lihaoyi::requests:0.7.1"
import os.Path

import scala.util.{Failure, Success, Try}

object Templater {
  def main(args: Array[String]): Unit =
    val year = 2022
    val targetYear = s"aoc$year"


    val base =  if (os.pwd.last == ".scala-build") os.pwd / os.up else os.pwd
    val srcBase = base / os.up / "src" / "main" / "scala" / targetYear
    val testBase = base / os.up / "src" / "test" / "scala" / targetYear
    val resourceBase = base / os.up / "src" / "main" / "resources" / targetYear
    val templateSrcDir   =  srcBase / "template"
    val templateTestDir   =  testBase / "template"


    val day = args.toList.headOption.getOrElse {
      println("Please provide a day number -- [day num]")
      sys.exit(1)
    }
    println("Creating new day: " + day)

    if(os.exists(templateSrcDir) && os.exists(templateTestDir))
        val targetFolder = s"day$day"
        val targetSrcDir = srcBase / targetFolder
        val targetTestDir = testBase / targetFolder

        // create puzzle-input.txt
        val puzzleInputFile = resourceBase / targetFolder / "puzzle-input.txt"

        val userAgent = sys.env("GITHUB_URL") + " " + sys.env("EMAIL_ADDRESS")
        val sessionId = sys.env("SESSION_ID")

        if(!os.exists(puzzleInputFile) )
          val data = Try(requests.get(s"https://adventofcode.com/$year/day/$day/input", verifySslCerts=false, check = true, headers = Map("User-Agent" -> userAgent, "Cookie" -> s"session=${sessionId}", "Cache-Control"-> "no-cache")))

          data match
            case Success(response) =>
              os.write(
                puzzleInputFile,
                response,
                createFolders = true
              )
            case Failure(e) =>
              println(s"Something went wrong loading input to $puzzleInputFile")
              println(e)
        else
          println(puzzleInputFile.toString + " already exists")

        def replaceAll(s: String) =
          s.replace("template", targetFolder)
           .replace("[day]", day.toString)
           .replace("[year]", year.toString)

        def copyTemplate(from: Path, to: Path) =
          if(!os.exists(to) )
            os.copy.over(from, to, createFolders = true)
            os.list(to).foreach { f =>
              val content = os.read(f)
              os.write.over(f, replaceAll(content))
            }
          else
            println(to.toString + " already exists")

        if(!os.exists(targetSrcDir) )
            copyTemplate(templateSrcDir, targetSrcDir)
        else
            println(targetSrcDir.toString + " already exists")

        if(!os.exists(targetTestDir) )
          copyTemplate(templateTestDir, targetTestDir)
        else
          println(targetTestDir.toString + " already exists")


    else
        println("Can't find template: " + templateSrcDir + " or " + templateTestDir)
        sys.exit(1)
}


