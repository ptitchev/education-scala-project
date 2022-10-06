@main def hello: Unit = 
  println("Hello world!")
  println(msg)

def msg = "I was compiled by Scala 3. :)"

// Declaration of some basic parsers
object Parser

:
def createParser[A](f: Input => ParseResult[A]): Parser[A] = input => f(input)
/** parse an integer. */
def int: Parser[Int] = createParser(???)
/** parse exactly the string s */
def string(s: String): Parser[String] = createParser(???)
/** parse according to a regular expression */
def regex(r: String): Parser[String] = createParser(???)