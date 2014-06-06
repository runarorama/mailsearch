package mailsearch

// Regular expression combinators
object Regexes {
  def or(terms: String*) = "(" + terms.mkString("|") + ")"
  def word(term: String) = s"\\b$term\\b"
  def phrase(terms: String*) = "\\b" + terms.mkString("\\W+") + "\\b"
  def within(n: Int, t1: String, t2: String) =
    s"(?:$t1\\W+(?:\\w+\\W+){0,${n-1}}?$t2|$t2\\W+(?:\\w+\\W+){0,${n-1}}?$t1)"
  def prefix(term: String) = s"$term\\w*"
}

