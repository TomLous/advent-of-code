//> using scala 3.3.1
//> using lib "com.lihaoyi::os-lib:0.9.2"
//> using lib "com.lihaoyi::requests:0.8.0"
//> using lib "com.github.tkqubo:html-to-markdown:0.8.3"
import os.Path
import com.github.tkqubo.html2md.Html2Markdown

import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

object Templater {
  def main(args: Array[String]): Unit =
    val year = 2019
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

        // create input.txt
        val puzzleInputFile = resourceBase / targetFolder / "input.txt"
        val readmeFile = srcBase / targetFolder / "README.md"

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
           .replace("DayN", "Day" + day.toString)
           .replace("[year]", year.toString)

        def copyTemplate(from: Path, to: Path) =
          if(!os.exists(to) )
            os.copy.over(from, to, createFolders = true)
            os.list(to).foreach { f =>
              val content = os.read(f)
              os.write.over(f, replaceAll(content))
            }
            if(os.exists(to / "DayN.scala")){
              os.move(to / "DayN.scala", to / s"Day$day.scala")
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

        val info = Try(requests.get(s"https://adventofcode.com/$year/day/$day", verifySslCerts=false, check = true, headers = Map("User-Agent" -> userAgent, "Cookie" -> s"session=${sessionId}", "Cache-Control"-> "no-cache")))


        info match
          case Success(response) =>
            val markdown:String = response.text()
              .replaceAll("""(?si).*?<main>(.*)</main>.*""", "$1")
              .replaceAll("""(?si)<style>.*?</style>""", "")
              .replaceAll("""(?si)<h2.*?>(.*?)</h2>""", "\n### $1\n\n")
              .replaceAll("""(?si)<pre><code>(.*?)</code></pre>""", "\n```\n$1\n```")
              .replaceAll("""(?si)<p>(.*?)</p>""", "$1\n")
              .replaceAll("""(?si)<code>(.*?)</code>""", "`$1`")
              .replaceAll("""(?si)<em.*?>(.*?)</em>""", "**$1**")
              .replaceAll("""(?si)`\*\*(.*?)\*\*`""", "**`$1`**")
              .replaceAll("""(?si)<a href="(.*?)".*?>(.*?)</a>""", "[$2]($1)")
              .replaceAll("""(?si)<li>(.*?)</li>""", "- $1")
              .replaceAll("""(?si)<ul>(.*?)</ul>""", "\n$1")
              .replaceAll("""(?si)<article.*?>(.*?)</article>""", "\n---\n$1")
              .replaceAll("""(?si)<span.*?>(.*?)</span>""", "$1")
              .replaceAll("""(?si)<form.*?>(.*?)</form>""", "")
              .replaceAll("""(?si)You can also \[Share.*""", "")
//              .replaceAll("""(?si)<p class="day-success">.*""", "")



            """(?si)<pre><code>(.*?)</code></pre>(.*?<code><em>(\d+)</em></code>)?""".r.findAllIn(response.text()).matchData.zipWithIndex.foreach { (m, i) =>
              Try{os.write(
                resourceBase / targetFolder / s"example-part${i+1}.txt",
                m.group(1)
              )}
              Try{os.write(
                resourceBase / targetFolder / s"example-part${i+1}-target.txt",
                m.group(3)
              )}
            }



            """(?si)Your puzzle answer was <code>(\d+)</code>""".r.findAllIn(response.text()).matchData.zipWithIndex.foreach { (m, i) =>
              Try{os.write(
                resourceBase / targetFolder / s"result-part${i+1}.txt",
                m.group(1)
              )}
            }


            val safemd = Regex.quote(markdown).replaceAll("""\$""", """\\\$""")


            val readmeContent =  os.read(readmeFile).replaceAll("""(?si)## Description(.*)##""", "## Description\n\n" + safemd  + "\n##")

            os.write.over(
              readmeFile,
              readmeContent
            )
          case Failure(e) =>
            println(s"Something went wrong wrinting readme to $readmeFile")
            println(e)


    else
        println("Can't find template: " + templateSrcDir + " or " + templateTestDir)
        sys.exit(1)
}


