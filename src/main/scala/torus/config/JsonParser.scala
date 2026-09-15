package torus.config

/**
 * Minimal recursive-descent JSON parser, used by both generation and tests.
 *
 * Returns `Any` where the actual Scala types are:
 *   object  -> Map[String, Any]
 *   array   -> List[Any]
 *   string  -> String
 *   number  -> Double
 *   bool    -> Boolean
 *   null    -> null
 *
 * Self-contained on purpose: no external dependency (upickle/circe/play-json)
 * so build.sbt stays minimal.
 *
 * There is no schema and no mapping to case classes — the parse produces raw nested
 * maps and lists, and the ACCESSORS at the bottom (`asMap`, `getInt`, `getStrOpt`, ...)
 * are what turn a map into a typed field, one field at a time. Two consequences the
 * config files rely on:
 *
 *  - UNKNOWN KEYS ARE IGNORED. Nothing walks the map looking for fields it does not
 *    recognise, which is why `"_comment"` and `"description"` annotations can sit
 *    anywhere in the manifests.
 *  - A MISSING KEY FAILS WHERE IT IS READ, naming the key ([[getInt]] and friends), not
 *    at parse time.
 *
 * JSON has ONE number type, so every number arrives as a `Double` and [[getInt]]
 * truncates it. Widths and dimensions are small integers, so this is exact; it would
 * not be for a value past 2^53.
 */
object JsonParser {

  // ── The parser ────────────────────────────────────────────────────────────────────
  // One instance per parse: `pos` is a mutable cursor into `text` that every method
  // below advances, so a Parser is single-use and is never shared. Each `parseX` is
  // entered with the cursor ON the construct's first character and leaves it just past
  // the construct's last — that invariant is the whole contract between them.
  private final class Parser(text: String) {
    private var pos: Int = 0

    /** Look at the current character WITHOUT consuming it — the one-character
      * lookahead the whole grammar dispatches on. Running off the end is a malformed
      * document, so it throws rather than returning a sentinel. */
    private def peek(): Char =
      if (pos < text.length) text.charAt(pos)
      else throw new RuntimeException(s"Unexpected end of JSON at position $pos")

    private def consume(): Char = {
      val c = peek()
      pos += 1
      c
    }

    /** Whitespace is insignificant between tokens, so every method that could be
      * entered after one calls this first. */
    private def skipWs(): Unit = {
      while (pos < text.length && text.charAt(pos).isWhitespace) pos += 1
    }

    /** Consume one required punctuation character, or fail naming both what was wanted
      * and what was there. */
    private def expect(c: Char): Unit = {
      skipWs()
      val got = consume()
      if (got != c) throw new RuntimeException(s"Expected '$c' but got '$got' at position ${pos - 1}")
    }

    /** Entry point: one whole document. The trailing check is what makes this a
      * document parser rather than a prefix parser — two concatenated objects in one
      * file are an error, not a silently accepted first object. */
    def parse(): Any = {
      skipWs()
      val v = parseValue()
      skipWs()
      if (pos != text.length)
        throw new RuntimeException(s"Trailing characters at position $pos")
      v
    }

    /** The grammar's dispatch. JSON is designed so one character of lookahead names
      * the value's type unambiguously, which is what keeps this parser a single pass
      * with no backtracking. */
    private def parseValue(): Any = {
      skipWs()
      peek() match {
        case '{' => parseObject()
        case '[' => parseArray()
        case '"' => parseString()
        case 't' | 'f' => parseBool()
        case 'n' => parseNull()
        case c if c == '-' || c.isDigit => parseNumber()
        case other =>
          throw new RuntimeException(s"Unexpected character '$other' at position $pos")
      }
    }

    private def parseObject(): Map[String, Any] = {
      expect('{')
      skipWs()
      // LinkedHashMap keeps the file's key order while building, purely so a debug dump
      // reads like the source; `.toMap` need not preserve it and nothing depends on it.
      val builder = scala.collection.mutable.LinkedHashMap.empty[String, Any]
      // The empty object is its own case: the loop below assumes at least one member,
      // since it parses a key before ever testing for the closing brace.
      if (peek() == '}') { consume(); return builder.toMap }
      while (true) {
        skipWs()
        val key = parseString()
        skipWs()
        expect(':')
        val value = parseValue()
        builder.update(key, value)
        skipWs()
        peek() match {
          case ',' => consume()
          case '}' => consume(); return builder.toMap
          case other =>
            throw new RuntimeException(s"Expected ',' or '}' but got '$other' at position $pos")
        }
      }
      // Unreachable: every exit from the loop is a `return`. It is here because in
      // Scala 2 `while (true)` has type Unit, not Nothing, so without a trailing
      // expression of the declared type the method would not typecheck.
      builder.toMap // unreachable
    }

    private def parseArray(): List[Any] = {
      expect('[')
      skipWs()
      val builder = List.newBuilder[Any]
      if (peek() == ']') { consume(); return builder.result() }
      while (true) {
        val v = parseValue()
        builder += v
        skipWs()
        peek() match {
          case ',' => consume()
          case ']' => consume(); return builder.result()
          case other =>
            throw new RuntimeException(s"Expected ',' or ']' but got '$other' at position $pos")
        }
      }
      builder.result() // unreachable — see parseObject
    }

    /** A string, with the escapes JSON defines. The `u` escape is read as exactly four
      * hex digits and converted to one Char, so a character outside the BMP arrives as
      * its two surrogate halves — correct for JSON, and never exercised by these
      * files. */
    private def parseString(): String = {
      expect('"')
      val sb = new StringBuilder
      while (true) {
        val c = consume()
        c match {
          case '"' => return sb.toString()
          case '\\' =>
            val esc = consume()
            esc match {
              case '"'  => sb += '"'
              case '\\' => sb += '\\'
              case '/'  => sb += '/'
              case 'b'  => sb += '\b'
              case 'f'  => sb += '\f'
              case 'n'  => sb += '\n'
              case 'r'  => sb += '\r'
              case 't'  => sb += '\t'
              case 'u'  =>
                val hex = (0 until 4).map(_ => consume()).mkString
                sb += Integer.parseInt(hex, 16).toChar
              case other =>
                throw new RuntimeException(s"Invalid escape '\\$other' at position ${pos - 1}")
            }
          case other => sb += other
        }
      }
      sb.toString() // unreachable — see parseObject
    }

    /** Every number is a Double, JSON having only one numeric type. The scan is
      * deliberately permissive — it takes the longest run of characters a number could
      * use and lets `toDouble` be the judge, so a malformed literal fails with Java's
      * own message rather than a hand-written grammar check. */
    private def parseNumber(): Double = {
      val start = pos
      if (peek() == '-') consume()
      while (pos < text.length && {
        val c = text.charAt(pos)
        c.isDigit || c == '.' || c == 'e' || c == 'E' || c == '+' || c == '-'
      }) pos += 1
      text.substring(start, pos).toDouble
    }

    private def parseBool(): Boolean = {
      if (text.startsWith("true", pos))  { pos += 4; true }
      else if (text.startsWith("false", pos)) { pos += 5; false }
      else throw new RuntimeException(s"Invalid boolean at position $pos")
    }

    private def parseNull(): AnyRef = {
      if (text.startsWith("null", pos)) { pos += 4; null }
      else throw new RuntimeException(s"Invalid null at position $pos")
    }
  }

  // ── Accessors ─────────────────────────────────────────────────────────────────────
  // The typed view over the raw `Any` tree. Loaders read one field at a time through
  // these, so a schema violation is reported as "field X is not a number" at the exact
  // field rather than as a cast failure somewhere downstream.

  def parse(text: String): Any = new Parser(text).parse()

  // The casts are unchecked by necessity: erasure leaves nothing of a Map's key/value
  // types to match on, so `case m: Map[_, _]` is as far as the check goes. The element
  // types are verified lazily instead, by the getters below, as each field is read.
  def asMap(any: Any): Map[String, Any] = any match {
    case m: Map[_, _] => m.asInstanceOf[Map[String, Any]]
    case other        => sys.error(s"Expected JSON object, got: $other")
  }

  def asList(any: Any): List[Any] = any match {
    case l: List[_] => l.asInstanceOf[List[Any]]
    case other      => sys.error(s"Expected JSON array, got: $other")
  }

  // getX vs getXOpt is the REQUIRED / OPTIONAL distinction, and it is the only place
  // the schema's optionality is stated: a missing key is an error in `getX` and `None`
  // in `getXOpt`. A field that is present but the wrong type fails in both.
  //
  // Both accept `Int` as well as `Double` so a map assembled in Scala, rather than
  // parsed from text, works unchanged.
  def getInt(m: Map[String, Any], key: String): Int = m.get(key) match {
    case Some(d: Double) => d.toInt
    case Some(i: Int)    => i
    case Some(other)     => sys.error(s"Field '$key' is not a number: $other")
    case None            => sys.error(s"Missing required field '$key'")
  }

  def getIntOpt(m: Map[String, Any], key: String): Option[Int] = m.get(key).map {
    case d: Double => d.toInt
    case i: Int    => i
    case other     => sys.error(s"Field '$key' is not a number: $other")
  }

  def getBool(m: Map[String, Any], key: String): Boolean = m.get(key) match {
    case Some(b: Boolean) => b
    case Some(other)      => sys.error(s"Field '$key' is not a boolean: $other")
    case None             => sys.error(s"Missing required field '$key'")
  }

  def getStrOpt(m: Map[String, Any], key: String): Option[String] = m.get(key).map {
    case s: String => s
    case other     => sys.error(s"Field '$key' is not a string: $other")
  }

  def getStr(m: Map[String, Any], key: String): String = m.get(key) match {
    case Some(s: String) => s
    case Some(other)     => sys.error(s"Field '$key' is not a string: $other")
    case None            => sys.error(s"Missing required field '$key'")
  }
}
