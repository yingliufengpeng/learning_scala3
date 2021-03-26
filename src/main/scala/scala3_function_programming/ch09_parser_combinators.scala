package scala3_function_programming
import scala3_function_programming.ch08_property_based_testing.{Gen, Prop, SGen}

import scala.annotation.targetName
import scala.language.implicitConversions
import scala.util.matching.Regex

object ch09_parser_combinators {


  object parsers:
//    type Parsers = [ParserError, Parser[+_]] =>> {}

    type Parser = [A] =>> Location => Result[A]

    enum Result[+A]:
      case Success(get: A, charsConsumed: Int)
      case Failure(get: ParserError, isCommitted: Boolean) extends Result[Nothing]

      
      def mapError(f: ParserError => ParserError): Result[A] = this match
        case Failure(e, b) => Failure(f(e), b)
        case _ => this
    
      
      def uncommit: Result[A] = this match
        case Failure(e, true) => Failure(e, false)
        case _ => this
      
      
      def addCommit(isCommitted: Boolean): Result[A] = this match
        case Failure(e, c) => Failure(e, c || isCommitted)
        case p => p
    
      def advanceSuccess(n: Int): Result[A] = this match
        case Success(a, m) => Success(a, m + n)
        case p => p

    import Result._

    case class Location(input: String, offset: Int = 0):
      // 出现语法检查的错误在于编译器默认使用了当前环境中的string的隐式转换的逻辑
      lazy val line = input.substring(0, offset + 1).count( _ == '\n') + 1
      lazy val col = input.substring(0, offset + 1).lastIndexOf('\n') match
        case -1 => offset + 1
        case lineStart => offset - lineStart

      lazy val loc = s"line, col"
      
      def toError(msg: String): ParserError =
        ParserError(List((this, msg)))
        
      def advanceBy(n: Int): Location =
        copy(offset = offset + n)
        

    case class ParserError(stack: List[(Location, String)]):
      def push(loc: Location, msg: String): ParserError =
        copy(stack = (loc, msg) :: stack)
        
      def label[A](s: String): ParserError =
        ParserError(latestLoc.map((_, s)).toList)
        
      def latestLoc: Option[Location] =
        latest map (_._1)
        
      def latest: Option[(Location, String)] =
        stack.lastOption
        
      def errorLocation: List[Location] =
        stack.map(_._1)
        
      def errorMessage: List[String] =
        stack.map(_._2)
   
    object ParserError:
      def error(msg: String): ParserError =
        error(Location("begin", 0), msg)
      
      def error(loc: Location, msg: String): ParserError =
        ParserError(List((loc, msg)))
    
    trait Parsers:
      self_parsers =>

      // 隐式转换写到最上面以便于下面的可以获得到
      given fromString2ParserString: Conversion[String, Parser[String]] = string(_)
      given Conversion[Regex, Parser[String]] = e => summon[Conversion[String, Parser[String]]](e.toString)

      
      def succeed[A](a: A): Parser[A] = string("").map(_ => a)

      
      def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

      
      def digit(c: Char): Parser[Int] = char(c).map(_.toInt)

      
      def string(s: String): Parser[String] =
        (location: Location) =>
          if (location.input.startsWith(s)) then
            Success(s, s.length)
          else
            Failure(Location(location.input).toError(s"Excepted:  $s" ), true)
      
      
      def orString(s1: String, s2: String): Parser[String] = s1 or s2
      
      
      def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] =  
        (s: Location) =>
          s1(s) match
            case Failure(e, false) => s2(s)
            case r => r
      
      
      def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
        p.flatMap(a => p2.map(b => (a, b)))

      
      def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
//        product(p, p2).map((a, b) => f(a, b))
        product(p, p2).map(f.tupled)


      // 目前来说,比较丑陋的写法
      implicit def asStringParser[A](v: A)(using p: A => Parser[String]): Parser[String] =
        p(v)
     
      
      extension [A, B, C >: A] (p: Parser[A])

        // 一元运算符的定义
        // errors
        def label(msg: String): Parser[A] =
          (s: Location) => p(s).mapError(_.label(msg))

        def scope(msg: String): Parser[A] =
          (s: Location) => p(s).mapError(_.push(s, msg))

        def fail: Parser[A] =
          (s: Location) => 
            Failure(ParserError.error("index"), true)

        def attempt: Parser[A] =
          (s: Location) => p(s).uncommit
 
        // parsers

        def run(input: String): Result[A] =
          p(Location(input))
          

        def | (p2: => Parser[C]): Parser[C] = self_parsers.or(p, p2)

        @targetName("||_or")
        def or (p2: => Parser[C]): Parser[C] = self_parsers.or(p, p2)

        def map(f: A => B): Parser[B] = flatMap(e => succeed(f(e)))

        def flatMap(g: A => Parser[B]): Parser[B] =
          (s: Location) =>
            p(s) match
              case Success(a, n) =>
                g(a)(s.advanceBy(n))
                  .addCommit(n != 0)
                  .advanceSuccess(n)

//              case e @ Failure(_, _) => e.asInstanceOf[Result[B]]
              case e @ Failure(_, _) => e 
                  
           
        def many: Parser[List[A]] =
           map2(p, p.many)(_ :: _) or succeed(List())

        def many1: Parser[List[A]] = map2(many, p)((acc, c) => c :: acc)

        def slice: Parser[String] = map(_ => "1")

        def lisntOfN(n: Int): Parser[List[A]] =
          map2(p, p.lisntOfN(n - 1))((a, acc) => a :: acc)


        // 二元运算符的定义
        def ** (p2: Parser[B]): Parser[(A, B)] = product(p, p2)
      

      object Laws:
        def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
          in.forAll{ s => p1.run(s) == p1.run(s) }

        def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
          equal(p, p.map(identity))(in)


        def labelLaw[A](p: Parser[A], inputs: Gen[String]): Prop =
          (inputs ** Gen.string).forAll {
            case (input, msg) =>
              p.label(msg).run(input) match
                case Failure(e, _) => e.errorMessage == List(msg)
                case _ => true
          }
          
      
      def test(): Unit = {

        val p = "abra".label("kkk") ** " ".many ** "second magic world".label("cadabra")

        val spaces = " ".many
        val p1 =  "abra" ** "spaces" ** "cadabra".scope("magic spell")
        val p2 = "abba" ** spaces ** "babba".scope("gibberish")

        val p3 = p1 or p2

        val p4 = (("abra" ** spaces ** "abra").attempt ** "cadabra") or ("abra" ** spaces ** "cadabra")

        val p5 = "kk".many

        val p8 = Regex("kk").many


      }
  
      def test2(): Unit = {
//        lazy val general_parser: Parser[List[Char]] = middle_bracket_parser ** parens_parser ** larger_bracket_parser
//        lazy val middle_bracket_parser: Parser[List[Char]]  = char('[') |>| general_parser |<| char(']')
//        lazy val parens_parser: Parser[List[Char]] = char('(') ** general_parser ** char(')')
//        lazy val larger_bracket_parser: Parser[List[Char]] = char('{') ** general_parser ** char('}')
//        
        
      }


  object jsons:
    enum JSON:
      case JNUll
      case JNumber(get: Double)
      case JString(get: String)
      case JBool(get: Boolean)
      case JArray(get: IndexedSeq[JSON])
      case JObject(get: Map[String, JSON])
  

  @main def parser_combinator_start(): Unit =  {

  }








}
