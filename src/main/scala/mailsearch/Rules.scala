package mailsearch

import java.io._
import com.pff._

// Definitions of search rules
object Rules {
  import Regexes._

  def throughout(s: String, email: PSTMessage): Boolean =
    ("(?i)" + s).r.findFirstIn(s"${email.getBody} ${email.getSubject}").isDefined

  def participant(s: String, email: PSTMessage): Boolean =
    ("(?i)" + s).r.findFirstIn(s"${email.getRecipientsString} ${email.getDisplayTo} ${email.getDisplayCC} ${email.getDisplayBCC}").isDefined

  def and(ss: String*): PSTMessage => Boolean =
    m => ss.foldLeft(None:Option[Boolean]) {
      case (Some(a), s) => Some(a && throughout(s, m))
      case (None, s) => Some(throughout(s, m))
    } getOrElse false

  def readRules(dir: File): Map[String, List[String]] = {
    val files = dir.listFiles(new FilenameFilter {
      def accept(d: File, name: String) = name matches "(?i).*\\.rule"
    })
    files.toList.map { f => (f.getName.replaceAll("(?i)\\.rule$", "") -> {
      val source = scala.io.Source.fromFile(f)
      val lines = source.getLines.toList
      source.close
      lines
    })}.toMap
  }

  def writeRules(dir: File, m: Map[String, List[String]]): Unit = {
    m.foreach { case (k, v) =>
      val f = new PrintWriter(new BufferedWriter(new FileWriter(
        dir.getAbsolutePath + File.separatorChar + k + ".rule")))
      v.foreach(f.println)
      f.close
    }
  }

}

