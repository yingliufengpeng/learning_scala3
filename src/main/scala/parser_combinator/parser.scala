package parser_combinator
import scala.language.implicitConversions
import scala.language.postfixOps
import scala.util.matching.Regex

object parser {

  type Parser[+A] = Location => Result[A]
  object Parser:
    import Result._
    val r = 10

  case class Location(input: String, offest: Int = 0):
    def toError(msg: String): ParserError = ParserError((this, msg) ::Nil)
  
    def advanceBy(n: Int): Location = copy(offest = offest + n)

  enum Result[+A]:
    case Success(v: A, consumed: Int)
    case Failure(parserErr: ParserError, committed: Boolean)
    
    def mapError(f: ParserError => ParserError): Result[A] = this match
      case Failure(e, c) => Failure(f(e), c)
      case p => p 
  
    def uncommit: Result[A] = this match
      case Failure(v, true) => Failure(v, false)
      case p => p

    def commit: Result[A] = this match
      case Failure(v, false) => Failure(v, true)
      case p => p

    def addCommit(isCommitted: Boolean): Result[A] = this match
      case Failure(v, c) => Failure(v, c || isCommitted)
      case p => p 
  
    def advanceSuccess(n: Int): Result[A] = this match
      case Success(v, c) => Success(v, c + n)
      case p => p 
  
  object Result:
    def fail[A]: Result[A] =
      Failure(ParserError(), true)

  import Result._ 
  case class ParserError(stack: List[(Location, String)] = List.empty):
    def push(loc: Location, msg: String): ParserError = 
      copy(stack = (loc, msg) :: stack)
      
    def label(s: String): ParserError =
      copy(stack = latestLoc.map((_, s)).toList)
      
    def latestLoc: Option[Location] =
      latest.map(_._1)
    
    // Get the last element of the stack or None if the stack is Empty
    def latest: Option[(Location, String)] =
      stack.headOption
  

  trait Parsers[Parser[+_]]:
    self =>
    
    given Conversion[Char, Parser[Char]] = char(_)
    given Conversion[String, Parser[String]] = string(_)
    
    
    def run[A](p: Parser[A])(input: String): Result[A]
    
    def string(str: String): Parser[String] = regex(Regex(str))
    
    def hidden_chacters: Parser[List[Char]]
    
    def char(ch: Char): Parser[Char] =
      string(ch.toString).map(_.charAt(0))
      
    def regex(reg: Regex): Parser[String] 

    def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
      fa flatMap (e => success(f(e)))

    def map2[A, B, C](fa: Parser[A], fb: => Parser[B])(f: (A, B) => C): Parser[C] = {
      for
        a <- fa
        b <- fb
      yield {
        f(a, b)
      }
    }

    def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B]

    def default_success[A](v: => A): Parser[A] =
      string("") map (_ => v)

    def success[A](v: => A): Parser[A]
 
    
//    def fail[A]: Parser[A] 
    
    def attempt[A](fa: Parser[A]): Parser[A]
    
    def commit[A](fa: Parser[A]): Parser[A]
     
    def slice[A](fa: Parser[A]): Parser[String]
    
    def listOfN[A](n: Int, fa: Parser[A]): Parser[List[A]] =
      (1 to n).foldLeft(default_success(List.empty[A]))((acc, _) => (acc map2 fa)(_ :+ _))
  
    def many[A](p: Parser[A]): Parser[List[A]] =  
      map2(p, many(p) )(_ :: _)  or success(List.empty[A])
    
    def many1[A](p: Parser[A]): Parser[List[A]] =
      map2(p, many(p))(_ :: _)
      
    def some[A](p: Parser[A]): Parser[Option[A]] =
      p.map(Some(_)) or success(None)
     
    def or[A](fa: Parser[A], fb: => Parser[A]): Parser[A]

    // 后面的两种只是在报错的时候来做触发
    def scope[A](msg: String)(fa: Parser[A]): Parser[A]

    def label[A](msg: String)(fa: Parser[A]): Parser[A]
    
    implicit class ParserOps[A](fa: Parser[A]):
      def run(input: String) = self.run(fa)(input)
      
      def map[B](f: A => B): Parser[B] = self.map(fa)(f)
      
      def map2[B, C](fb: => Parser[B])(f: (A, B) => C): Parser[C] = self.map2(fa, fb)(f)
      
      def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(fa)(f)

      def slice: Parser[String] = self.slice(fa)

      def many: Parser[List[A]] = self.many(fa)
      def many1: Parser[List[A]] = self.many1(fa)
      def * : Parser[List[A]] = self.many(fa)
      def + : Parser[List[A]] = self.many1(fa)
      
      
      def or(fb: => Parser[A]): Parser[A] = self.or(fa, fb)
      def some: Parser[Option[A]] = self.some(fa)
      def ?? : Parser[Option[A]] = self.some(fa)
      
      def scope(msg: String): Parser[A] = self.scope(msg)(fa)
      def label(msg: String): Parser[A] = self.label(msg)(fa)
  
      def attempt: Parser[A] = self.attempt(fa)
      def commit: Parser[A] = self.commit(fa)
      
//      def |> [B, C](fb: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(fa, fb)(f)
      def ~[B](fb: => Parser[B]): Parser[(A, B)] = (fa map2 fb)((_, _))
      def |-|[B](fb: => Parser[B]): Parser[(A, B)] = (fa map2 fb)((_, _))

      def <-- [B](fb: Parser[B]): Parser[A] = fa |<| fb 
      def --> [B](fb: Parser[B]): Parser[B] = fa |>| fb

      def |<| [B](fb: Parser[B]): Parser[A] = (fa ~ fb).map(_._1).attempt
      def |>| [B](fb: Parser[B]): Parser[B] = (fa ~ fb).map(_._2).attempt
      
    

  object MyParsers extends Parsers[Parser]:
    import Result._


    override def commit[A](fa: Parser[A]): Parser[A] =
      location =>
        fa(location).commit

    override def attempt[A](fa: Parser[A]): Parser[A] =
      location => 
        fa(location).uncommit

    override def run[A](fa: Parser[A])(input: String): Result[A] = fa(Location(input))


    override def regex(reg: Regex): Parser[String] =
      
      location =>
        val origin_str = location.input.substring(location.offest)
        val some_v = reg findFirstIn origin_str
        some_v match
          case Some(v) if origin_str.startsWith(v)  => Success(v, v.length)
          case _ =>
            val r = location.toError(s"Expected pattern: ${reg.pattern} found ${origin_str}")
            Failure(r, false)

//    override def string(str: String): Parser[String] = regex(Regex(str))


    override def string(str: String): Parser[String] =
      location =>
     
        val origin_str = location.input.substring(location.offest)
        if origin_str.length >= str.length && origin_str.substring(0, str.length) == str then
          Success(str, str.length)
        else
          val r = location.toError(s"Expected: [---$str---] found [--->${origin_str}<---]")
          Failure(r, false)

    override def many[A](p: Parser[A]): Parser[List[A]] = {
      location =>
        val buf = collection.mutable.ListBuffer.empty[A]
        def go(number: Int): Result[List[A]] = {
//          println(f"buf is $buf")
//          val r = p(location.advanceBy(number))
//          println(f"r is $r")
          p(location.advanceBy(number)) match
            case Success(v, c) => buf += v ; go(number + c)
            case f@Failure(_, true) => f.copy()
            case Failure(_, false) => Success(buf.toList, number)
        }

        go(0) 
      
    }
    
    def white_spaces: Parser[List[Char]] = sigle_white_space.many
    def sigle_white_space: Parser[Char] = char(' ')
    def sinle_line_break: Parser[Char] = regex("\\r?\\n".r).map(_.charAt(0))
    override def hidden_chacters: Parser[List[Char]] = (sigle_white_space or sinle_line_break).many
    
    def string_parser: Parser[String] = regex("\".*?\"".r).map(e => e.substring(1, e.length - 1))
    def int_parser: Parser[Int] = regex("\\d+".r).map(_.toInt)
    
    
   
 
    override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] =
      location =>
        fa(location) match
          case Success(v, n) => 
            f(v)(location.advanceBy(n)) // Advance the source location before calling the second parser 
            .addCommit(n !=  0)         // Commit if the first the parser has consumed any characters
            .advanceSuccess(n)          // If successful, we increment the number of characters consumed by n, to account for 
                                          // characters already consumed by f
          case p@ Failure(_, _) => p.copy()

    override def success[A](v: => A): Parser[A] =
      _ =>
        Success(v, 0)

    override def slice[A](fa: Parser[A]): Parser[String] = ???
 

    override def or[A](fa: Parser[A], fb: => Parser[A]): Parser[A] =
      location =>
        fa(location) match
          case Failure(_, false) => fb(location)
          case p => p 

    override def scope[A](msg: String)(fa: Parser[A]): Parser[A] =
      location =>
        fa(location).mapError(_.push(location, msg))
        
    // Call a helper method on ParserError, which is also named label
    override def label[A](msg: String)(fa: Parser[A]): Parser[A] =
      location =>
        fa(location).mapError(_.label(msg))
  
  
    def test(): Unit = {
      val p1 =  string("3355") 
      val p2 = char('6')  
      val p3 = (p1 map2 p2)((s1, s2) => s1 + s2)
      
      val r = p3.run("33556xxx")
      println(f"p1 is $r")
    }
  
    def test2(): Unit = {
      val p1 = char('3')
      val r = p1.run("4ddddddddddd")
      println(s"r is $r")
    }
  
    def test3(): Unit = {
      val p1 = string("3")
      val p2 = string("4")
      val p3 = (p1 map2 p2)(_ + _)
      val r = p3.run("344")
      println(s"r is $r")
    }
  
    def test4(): Unit = {
      val p1 = string("4").many1
      val r = p1.run("4" * 10)
      println(s"r is $r")
    }
  
    def test5(): Unit = {
      val p1 = string("44") 
      val p2 = string("55")
      val p3 = p1 or p2 
      val r = p3.run("555")
      println(f"r is $r")
    }
  
    def test6(): Unit = {
      val left_brace = char('{')
      val right_brace = char('}')

      val p = left_brace <-- (white_spaces --> right_brace)
      val r = p.run("{  }")
      println(s"r is $r")
     
      
    }

    def test7(): Unit = {
      val left_brace = char('{')
      val right_brace = char('}')

      val p = left_brace <-- white_spaces --> right_brace
      val r = p.run("{  }")
      println(s"r is $r")

      val p2 = left_brace |<| white_spaces |>| right_brace
      val r2 = p2.run("{  }")
      println(s"r2 is $r2")
       
    }
  
    def test8(): Unit = {
      val p1 = char('4').some.map(_.getOrElse(""))
      val p2 = char('5').many
      val p3 = p1 ~ p2 
      val r = p3.run("4" + "5" * 100 + "44")
      println(s"r is $r")
    }
  
    def test9(): Unit = {
      val p1 = hidden_chacters |>| (char('3').many1.map(_.mkString("")) |<| hidden_chacters ).many1 |<| hidden_chacters 
      val text =
        """
          |
          |
          |3333
          |33
          |
          |3333
          |
          |
          |
          |
          |
          |""".stripMargin
      
      val r = p1.run(text)
      println(s"r is $r")
    }
  
    def test10(): Unit = {
      val p = hidden_chacters |>| char('3').many
      val r = p.run("  3333333  ")
      println(s"r is $r")
      
      val p1 = sigle_white_space
      val r1 = p1.run(" ")
      println(f"r1 is $r1")
      
      val p2 = sinle_line_break
      val r2 = p2.run("\n")
      println(f"r2 is $r2")      
      
      val p3 = hidden_chacters
      val r3 = p3.run("  \n \n  ")
      println(f"r3 is $r3")

      val p4 = sigle_white_space.many 
      val r4 = p4.run("    ")
      println(f"r4 is $r4")
    }
  
    def test11(): Unit = {
      val p = hidden_chacters |>|  string_parser
      val r = p.run("""  "   dfsfsdf dds2 "  """)
      println(f"r is $r")
      
      val p2 = hidden_chacters |>| int_parser
      val r2 = p2.run("   33 ")
      println(f"r2 is $r2")
    }
  
    def test12(): Unit = {
      lazy val p: Parser[List[(Char, Char)]] = (char('(') ~ (p or success(List.empty)) ~ char(')')).map {
        case ((l, ls), r) => ls :+ (l, r)
      }
      
      val r = p.run("((((()))))")
      println(f"r is $r")
    }
  
  
    def test13(): Unit = {
      
      val t = string_parser |<| char(':') 
      
      lazy val p: Parser[Map[String, _]] = {
        lazy val key = string_parser
        lazy val colon = char(':')
        lazy val tts_inits = (key |<| colon |-| p |<| char(',')).many
        lazy val tts_tail = (key |<| colon |-| p).some
        lazy val value = (tts_inits ~ tts_tail).map(tttt => tttt._1 ++ tttt._2.map(List(_)).getOrElse(List.empty)).map(_.toMap)
        char('{') |>| value |<| char('}')
      }
      val r = p.run("""{"kk":{}}""")
      println(f"r is $r")
    }
  
    def test14(): Unit = {
      val p = hidden_chacters |>| char('3') ~ char('4') ~ char('5') ~ char('6')
      val p2 = p.many
      val r = p.run("  3456")
      println(f"r is $r")
    }
  
    def test15(): Unit = {
  
      val p1 = char('3') |<| char(',')
      val p2 = p1.many
      
      val p3 = ((char('3') |<| char(',')).many ~ char('3')).some
      
      val r = p3.run("3,3,3,3")
      println(f"r is $r")
      
      // ((a,)*a)?
      
    }
  
    def test16(): Unit = {
      
//      val p: Parser[Char] = '3'
//      val p1 = p* 
//      val p2 = p+
//      val p3 = p??
//      
//      val r = (p*).run("333333")
//      println(f"r is $r")
//
//      val p4 = ((char('3') |<| char(',')).many ~ char('3')).some
//      val p5 = (((p |<| ',')*) ~ p )??
//      val r2 = p5.run("3,3,3,3,3")
//      println(f"r2 is $r2")

 
      lazy val mapParser: Parser[Map[String, _]] = {
        lazy val left_brace = hidden_chacters |>| '{' |<| hidden_chacters
        lazy val right_brace = hidden_chacters |>| '}' |<| hidden_chacters
        lazy val key = hidden_chacters |>| string_parser |<| hidden_chacters
        lazy val colon = hidden_chacters |>| char(':') |<| hidden_chacters
        lazy val comma = hidden_chacters |>| ',' |<| hidden_chacters
        lazy val tts_inits = (key |<| colon |-| mapParser |<| comma)*
        lazy val value = (tts_inits |-| (key |<| colon |-| mapParser))??
        lazy val res = value map (t => t.map(tt => tt._1 :+ tt._2).getOrElse(List.empty).toMap)
        left_brace |>| res |<| right_brace
      }
      
      val str =
        """
          |{
          | "kk": {}, 
          | "vv": {},
          | "zz": {
          |   "vv": {
          |      "zz": {},
          |      "jj": {}
          |   }
          | }
          |}
          |
          |
          |""".stripMargin
      
      val r3 = mapParser.run(str)
      println(f"r3 is $r3")
      
    }
    
    def test18(): Unit = {
      lazy val general_parser: Parser[List[Char]] = (middle_bracket_parser or parens_parser or larger_bracket_parser ).many.map(e => e.flatten)
           
      lazy val middle_bracket_parser: Parser[List[Char]]  = (char('[') |-| general_parser |-| char(']')) map {case ((ch1, t), ch2) => (ch1 :: t) :+ ch2 }
      lazy val parens_parser: Parser[List[Char]] = (char('(') |-| general_parser |-| char(')')) map { case ((ch1, t), ch2) => (ch1 :: t) :+ ch2 }
      lazy val larger_bracket_parser: Parser[List[Char]] = (char('{') |-| general_parser |-| char('}')) map { case ((ch1, t), ch2) => (ch1 :: t) :+ ch2}
      
      val inputs = List("[{}[]{{[([{}[]])]}}]", "[][]{}{")
      inputs.foreach(i => {
        val r = general_parser.run(i)
        r match
          case Success(values, _) if values.size == i.size => println(s"ok")
          case _ => println(s"error")
      })
     
 
    }
  
    def test17(): Unit = {
      val str =
        """
          |
          |
          | {
          |   "kk": 4,
          |   "dd": "fsdfsfdsf",
          |   "vv": [],
          |   "zz": [3, 4, {
          |     "jj": {},
          |     "z z": [3, "434", 3]
          |   }]
          |     
          | }
          |
          |""".stripMargin
          
      val p = jsonParser(MyParsers)
      val r = p.run(str)
      p.run(str) match
        case Success(v, _) => println(f"v is $v")
        case _ =>
  }

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] =
    import JSON._ 
    import P.*
    
    lazy val sum_parser: Parser[JSON] =
      lazy val p1 = null_parser
      lazy val p2 = bool_parser
      lazy val p3 = double_parser
      lazy val p4 = string_parser
      lazy val p5 = array_parser
      lazy val p6 = json_parser
      p1 or p2 or p3 or p4 or p5 or p6
    
    lazy val null_parser = hidden_chacters |>| string("null").map(_ => JNUll) |<| hidden_chacters
    lazy val comma = hidden_chacters |>| char(',') |<| hidden_chacters
    lazy val bool_parser = (hidden_chacters |>| (string("true") or string("false")) |<| hidden_chacters).map{ 
      case "true" => JBool(true)
      case _ => JBool(false)
    }
    lazy val double_parser = hidden_chacters |>| regex("\\d+".r).map(e => JNumber(e.toDouble)) |<| hidden_chacters

    lazy val general_parser = regex("\".*?\"".r).map(e => e)
    lazy val string_parser = general_parser.map(e => JString(e))
    
    // [ o, o, o, o]
    lazy val array_parser: Parser[JSON] =
      lazy val left: Parser[Char] = hidden_chacters |>| char('[') |<| hidden_chacters
      lazy val right: Parser[Char] = hidden_chacters |>| char(']') |<| hidden_chacters
      lazy val comma: Parser[Char] = hidden_chacters |>| char(',') |<| hidden_chacters
      lazy val p: Parser[List[JSON]] = ((((sum_parser |<| comma)*) |-| sum_parser )??).
        map (t => t.map(tt => tt._1 :+ tt._2).getOrElse(List.empty) )
      lazy val r = left |>| p |<| right
      r.map(ts => JArray(ts.toIndexedSeq))
    
    // {"kk": {}, "vv": {"kk": "zz"}
    lazy val json_parser: Parser[JSON] =
      lazy val key: Parser[String] = hidden_chacters |>| general_parser |<| hidden_chacters
      lazy val colon: Parser[Char] = hidden_chacters |>| char(':') |<| hidden_chacters
      lazy val value: Parser[JSON] = sum_parser
      lazy val tt: Parser[(String, JSON)] = key |<| colon |-| value

      lazy val left: Parser[Char] = hidden_chacters |>| char('{') |<| hidden_chacters
      lazy val right: Parser[Char] = hidden_chacters |>| char('}') |<| hidden_chacters
      lazy val comma: Parser[Char] = hidden_chacters |>| char(',') |<| hidden_chacters
      lazy val inits: Parser[Map[String, JSON]] = ((tt |<| comma)*) map (_.toMap)
      lazy val r: Parser[Map[String, JSON]] = ((inits |-| tt)??).
        map (op_t => op_t.map(t => t._1 + t._2).getOrElse(Map.empty))

      lazy val res: Parser[Map[String, JSON]] = left |>| r |<| right
      res.map(e => JObject(e))
  
    json_parser
  
 

  @main def parser_start(): Unit = {
//    MyParsers.test17()
    MyParsers.test18()
  }
}
