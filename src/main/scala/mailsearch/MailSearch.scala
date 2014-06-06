package mailsearch

import com.pff._
import scala.collection.JavaConversions._
import scala.util.matching._
import java.io._
import scala.collection.immutable.ListMap

object MailSearch {

  var counter = 0
  var emitted = 0
  var attached = 0

  def main(args: Array[String]): Unit = {
    if (args.size != 3)
      println("""
      | This program takes three arguments: <ruleDir> <inFile> <outDir>
      |   where <ruleDir> is the directory containing rules
      |         <inFile>  is the PST file to process
      |         <outDir>  is the directory that should contain the output
      |""".stripMargin)
    else {
      try {
        val rulesDir = new File(args(0))
        val inFile = new PSTFile(args(1))
        val outDir = new File(args(2))
        outDir.mkdirs
        println("")
        counter = 0
        val rules = Rules.readRules(rulesDir)
        processFolder(inFile.getRootFolder, rules, outDir)
      } finally {
        println(s""" Processed $counter messages
        | Emitted $emitted messages with $attached attachments
        """.stripMargin)
      }
    }
  }

  def processFolder(folder: PSTFolder, rules: Map[String, List[String]], outDir: File): Unit = {
    folder.getSubFolders.foreach(processFolder(_, rules, outDir))
    processMessages(folder, rules, outDir)
  }

  @annotation.tailrec def processMessages(folder: PSTFolder, rules: Map[String, List[String]], outDir: File): Unit = {
    val m = folder.getNextChild()
    if (m != null) {
      processMessage(m.asInstanceOf[PSTMessage], rules, outDir)
      processMessages(folder, rules, outDir)
    }
  }

  def processMessage(email: PSTMessage, rules: Map[String, List[String]], outDir: File): Unit =
    if (!email.isUnsent) {
      counter += 1
      print(s" Processed $counter messages                                       \r")
      val ns = findMatches(email, rules)
      if (!ns.isEmpty) {
        val attachments = processAttachments(email, outDir)
        // Print the email to the output file
        val fileName = outDir.getAbsolutePath + File.separatorChar + internetId(email) + "." + "txt"
        val outFile = new PrintWriter(new BufferedWriter(new FileWriter(fileName)))
        outFile.println(formatMatch(email, attachments, ns))
        outFile.close
        emitted += 1
      }
    }

  def processAttachments(email: PSTMessage, outDir: File): List[String] = {
    val n = email.getNumberOfAttachments
    Range(0, n).map { x =>
      val attachment = email.getAttachment(x)
      val fileName = attachment.getFilename
      val in = attachment.getFileInputStream
      val outFileName = outDir.getAbsolutePath +
                        File.separatorChar +
                        internetId(email) + "." +
                        attachment.getFilename
      val out = new BufferedOutputStream(new FileOutputStream(outFileName))
      sys.process.BasicIO.transferFully(in, out)
      out.close
      attached += 1
      outFileName
    }.toList
  }

  def findMatches(email: PSTMessage, rules: Map[String, List[String]]): List[String] = {
    rules.foldLeft(List[String]()) {
      case (ns, (n, r)) => if (Rules.and(r:_*)(email)) n :: ns else ns
    }
  }

  def internetId(email: PSTMessage): String =
    email.getInternetMessageId.replaceAll("<|>", "")

  val fields: Map[String, PSTMessage => String] = ListMap(
    "ID" -> (internetId(_)),
    "Date" -> (_.getMessageDeliveryTime.toString),
    "Subject" -> (_.getSubject),
    "From: (Name)" -> (_.getSenderName),
    "From: (Address)" -> (_.getSenderEmailAddress),
    "From: (Type)" -> (_.getSenderAddrtype),
    "To" -> (_.getDisplayTo),
    "CC" -> (_.getDisplayCC),
    "BCC" -> (_.getDisplayBCC)
  )

  def formatMatch(email: PSTMessage, attachments: List[String], rules: List[String]): String =
    (fields ++ Map("Attached" -> ((_:PSTMessage) => attachments.mkString(",")),
                   "Matched" -> ((_:PSTMessage) => rules.mkString(",")))).map {
      case (k, f) => s"$k: ${f(email)}\n"
    }.mkString ++ "\n=== BODY ===\n" ++ email.getBody

}

