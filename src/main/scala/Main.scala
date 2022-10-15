import scala.annotation.tailrec
import ParseResult.*

// Common interface for parsers
trait Parser[A]:
  /** parse with this and then parse with pb. */
  def ~[B](pb: => Parser[B]): Parser[(A, B)] = ???
  /** parse with this, or parse with pb if this fails. */
  def |[B](pb: Parser[B]): Parser[Either[A, B]] = ???
  /** try to parse with this. It does not fail if the parsing did not work. */
  def ? : Parser[Option[A]] = ???
  /** use this to parse multiple times, until it does not apply. */
  def repeat: Parser[List[A]] = ???
  /** convert the value output by the parser. */
  def map[B](f: A => B): Parser[B] = ???
  def flatMap[B](f: A => Parser[B]): Parser[B] = ???

  final def parse(s: String): ParseResult[A] = parse(Input(s))
  def parse(input: Input): ParseResult[A]

object Parser:
  def createParser[A](f: Input => ParseResult[A]): Parser[A] = input => f(input)

  /** parse an integer. */
  @tailrec
  def parse_integer(input : Input): ParseResult[Int] = {
    val numeric = """([0-9]+)""".r
    val current_string = input.current(1)
    current_string match {
      case numeric(current_string)=>  parse_integer(input.next(1))
      case "-" => if (input.offset == 0)
                    parse_integer(input.next(1))
                  else
                    ParseFailure(input)
      case _ => if (input.offset == 0)
                  ParseFailure(input)
                else
                  ParseSucceed(input.data.substring(0, input.offset).toInt, input)
    }
  }
  def int: Parser[Int] = createParser(parse_integer)

  /** parse exactly the string s */
  def string(s: String): Parser[String] = createParser(???)

  /** parse according to a regular expression */
  def regex(r: String): Parser[String] = createParser(???)

// Result of parse
enum ParseResult[+A]:
  case ParseFailure(onInput: Input) extends ParseResult[Nothing]
  case ParseSucceed(value: A, remainingInput: Input) extends ParseResult[A]

  def map[B](f: A => B): ParseResult[B] = ???
  def flatMap[B](f: A => ParseResult[B]): ParseResult[B] = ???
// this class move 1 by 1
case class Input_1(data: String, offset: Int = 0):
  def current: String = data.substring(offset,offset+1)
  def next: Input_1 = copy(offset = offset+1)
  def remaining: String = data.substring(offset)

case class Input(data: String, offset: Int = 0):
  def current(n: Int): String = data.substring(offset,offset+n)
  def next(n: Int): Input = copy(offset = offset+n)
  def remaining: String = data.substring(offset)


@main
def main(): Unit =
  // example 1 by 1, we start with offset = 2 so current = 3 (because we start at 0 so 2 is the third value) and the next is 4
  println("offset start = 2, 1 by 1 \n current : " + Input_1("123456789",2).current)
  println("offset start = 2, 1 by 1 \n next : " +Input_1("123456789",2).next.current)

  // example with 1 as step, we start with offset = 2 so current = 3  (it acts like 1 by 1 example)
  println("offset start = 2, step = 1 \n step = 1, current : " +Input("123456789",2).current(1))
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


