import scala.annotation.tailrec
import ParseResult.*
import Parser.createParser
import scala.util.matching.Regex

// Common interface for parsers
trait Parser[A]:
  self =>
  /** parse with this and then parse with pb. */
  def ~[B](pb: => Parser[B]): Parser[(A, B)] =
      for {
        a <- this
        b <- pb
      } yield (a, b)
  /** parse with this, or parse with pb if this fails. */
  def |[B](pb: Parser[B]): Parser[Either[A, B]] = ???
  /** createParser :
   * if this.parse(input) -> succeed  then return ParseSucceed(Left(value))
   * else pb.parse(input) -> succeed then return ParseSucceed(Right(value))
   *                      -> fail then return ParseFailure(input)*/
  /** try to parse with this. It does not fail if the parsing did not work. */
  def ? : Parser[Option[A]] =
    Parser.createParser(input => {
      this.parse(input) match
        case ParseResult.ParseSucceed(value, input) => ParseResult.ParseSucceed(Option(value), input)
        case ParseResult.ParseFailure(input) => ParseResult.ParseSucceed(Option.empty, input)
    })
  /** use this to parse multiple times, until it does not apply. */
  def repeat: Parser[List[A]] = ???
  /** convert the value output by the parser. */
  def map[B](f: A => B): Parser[B] = Parser.createParser(input => self.parse(input).map(f))
  def flatMap[B](f: A => Parser[B]): Parser[B] = Parser.createParser {
    input =>
      val result: ParseResult[A] = self.parse(input)
      result.flatMap(r => f(a).parse())
  }

  final def parse(s: String): ParseResult[A] = parse(Input(s))
  def parse(input: Input): ParseResult[A]

object Parser:
  def createParser[A](f: Input => ParseResult[A]): Parser[A] = input => f(input)

  /** parse an integer. */
  @tailrec
  def parse_integer(input : Input): ParseResult[Int] = {
    val numeric = """([0-9])""".r
    val current_string = input.current(1)
    current_string match {
      case numeric(current_string)=>  parse_integer(input.next(1))
      case "-" => if (input.offset == 0)
                    parse_integer(input.next(1))
                  else
                    ParseSucceed(input.data.substring(0, input.offset).toInt, input)
      case _ => if (input.offset == 0)
                  ParseFailure(input)
                else
                  ParseSucceed(input.data.substring(0, input.offset).toInt, input)
    }
  }
  def int: Parser[Int] = createParser(parse_integer)

  /** parse exactly the string s */
  @tailrec
  def parse_string(input: Input, s: String): ParseResult[String] = {
    val current_string = input.current(s.length())
    val len = input.data.length() - s.length()
    if (current_string == s)
      ParseSucceed(input.data.substring(input.offset, input.offset + s.length()), input.next(s.length()))
    else if (input.offset >= len)
      ParseFailure(input)
    else
      parse_string(input.next(1), s)
  }

  def string(s: String): Parser[String] = createParser(input => parse_string(input, s))

  /** parse according to a regular expression */
/** parse exactly the string s */


  def parserRegex(input: Input, reg: String): ParseResult[String] = {
    val regex: Regex = reg.r /** create a regex from the input string -> must be [$-$] */
    val extractRegex = (regex findAllIn input.data).mkString("")
    if (extractRegex.isEmpty)
      ParseFailure(input)
    else if (extractRegex.length == input.data.length)
      ParseSucceed(extractRegex, input.copy(offset = input.data.length - 1))
    else
      ParseSucceed(extractRegex, input.copy(offset = extractRegex.length))
  }
  /** parse according to a regular expression */
  def regex(reg: String): Parser[String] = {

    createParser(input => parserRegex(input, reg))
  }

// Result of parse
enum ParseResult[+A]:
  case ParseFailure(onInput: Input) extends ParseResult[Nothing]
  case ParseSucceed(value: A, remainingInput: Input) extends ParseResult[A]

  def map[B](f: A => B): ParseResult[B] =
    this match
      case ParseSucceed(v, i) => ParseSucceed(f(v), i)
      case ParseFailure(i) => ParseFailure(i)
  def flatMap[B](f: A => ParseResult[B]): ParseResult[B] =
    this match
      case ParseSucceed(v, i) => f(v)
      case ParseFailure(i) => ParseFailure(i)
// this class move 1 by 1


case class Input(data: String, offset: Int = 0):
  def current(n: Int): String = data.substring(offset,offset+n)
  def next(n: Int): Input = copy(offset = offset+n)
  def remaining: String = data.substring(offset)


@main
def main(): Unit =

// example with 1 as step, we start with offset = 2 so current = 3  (it acts  1 by 1 )
  println("offset start = 2, step = 1 \n current : " +Input("123456789",2).current(1))
  // example with 3 as step, we start with offset = 2 so current = 345, it takes everything between current(1) and current(3)
  println("offset start = 2, step = 3 \n current : " +Input("123456789",2).current(3))
  // example with 1 as step, we start with offset = 2 so current = 3 and next = 4  (it acts like 1 by 1 example)
  println("offset start = 2, step = 1 \n next : " +Input("123456789",2).next(1).current(1))
  // example with 3 as step, we start with offset = 2 so current = 345, next = 678, it takes everything between the end of the current and 3 values later
  println("offset start = 2, step = 3\n next : " +Input("123456789",2).next(3).current(3))
  // of course we can do some other test like
  println("offset start = 1,  step_next = 3, step_current = 2 \n current : " +Input("123456789",1).next(3).current(2))

  println("\n")

  // example of working parser
  println(Parser.int.parse("12a"))
  //example of a failed parser
  println(Parser.int.parse("A12a"))

  println(Parser.int.parse("-12a"))

  println(Parser.int.parse("12-a"))

  println("\n")


  println(Parser.string("AB").parse("ABC"))

  println(Parser.string("AC").parse("ABC"))

  println(Parser.string("B").parse("ABC"))

  println(Parser.string("Y").parse("ABC"))

  println("\n")


  println(Parser.regex("A+[0-9]+").parse("A12B"))

  println(Parser.regex("[A-Z]").parse("12"))

  println(Parser.regex("[1-9]").parse("A12B"))


