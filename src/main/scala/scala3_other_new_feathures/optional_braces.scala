package scala3_other_new_feathures

import java.util

object optional_braces {
  
  // The compiler enfores two rules for well-indented programs, flagging violations
  // as warning 
  // 1. In a brace-delimited region, no statement is allowed to start to the left of 
  // the first statemetn after the opening brace that starts a new line.
  // This rule is helpful for finding missing closing braces. It prevents errors like.
  
  // 2 If significant indentation is turned off, and we are at the start of an indented 
  // sub_spart of an expression, and the indented part ends in a newline, the next
  // statement must start start  at an indentation width less than the sub-part. This
  // prevents errors where an opening brace was forgotten, as in 
  
  // There are two rules:
  // 1 An <indetn> is insered at a line break, if 
  //    An indentation region can start at the current position in the source, and 
  //    The first token on the next line has an indentation with stritly greater than 
  //      the current indentation width
  //  An indentation argion can start
  //    after the leading parameters of an extension, or
  //    after a with in a given instance, or
  //    after a ":at end of line" token 
  //    after one of the follwing tokens:
  //      = => ?=> <- catch do else finally for if match return then throw try while yield
  // If an <indent> is inserted, the indentation width of the token  on the next line 
  //   is pushed onto IW is pushed onto IW, which makes it the new current indentation 
  //   width.
  //  the first token on the next line has an indentation width strictly less than the 
  //    current identation width, and 
  //  the last token on the previous line is not one of the following tokens which indicate
  //    the previous statement continue: then else do catch finally yield match
  //  the first token on the next line is not a leading infix operator 
  // If an <outdent> is inserted, the top element is popped from IW. If the indentation 
  //   width of the token on the next line is still less than the new current indentation 
  //    width, step(2) repeats. Therefore, serval <outdent> tokens may be inserted in a row.
  // 
  //  An <outdent> is also inserted if the next token following a statemetn sequence starting 
  //  with an <indent> closes an indentation regtion, i,e, is one of then, else, do, catch,
  //      finally, yieldj, }, ), ], or case.
  //  An <outdent> is finaly inserted in front of a comma that follows a statement sequence
  //  starting with an <indent> if the indented region is itself enclosed in parentheses.
  //
  //  Indetation tokens are only inserted in regions where newline statement separator also
  //    inferred: at the top-level, inside bradces {...}, but not inside parenthese (...)
  //    , patterns or types
  //
  // The indentation rules for match expression and catch clause are refined as follows:
  //  An indentation region is opened after a match or catch also if the following case 
  // appears at the indentation width that's current for the match self.
  //  In that case, the indentation region closes at the first token at the same indentation
  //  width that is not a case, or at any token with a smaller indentation width, whichever
  //  comes first.
  //  
  //
  //
  //
  
  
  trait A: 
    def f: Int 
  
  class C(x: Int) extends A:
    def f = x 
  
  object O:
    def f = 3
  
  enum Color:
    case Red, Green, Blue
  
  val a = new A:
    def f = 3
  
  
  def largeMethod(): Unit =
    val r = 1
 
    val r2 = 10
  
  end largeMethod
  
  
  enum IndentWidth:
    case Run(ch: Char, n: Int)           // 叶子节点
    case Conc(l: IndentWidth, r: Run)    // 中间节点  左节点 是 抽象节点 右节点时 叶子节点
  
    def <= (that: IndentWidth): Boolean = this match
      case Run(ch1, n1) =>
        that match
          case Run(ch2, n2) =>
            n1 <= n2 && (ch1 == ch2 || n1 == 0)
          case Conc(l, _) => this <= l 
      case Conc(l1, r1) =>
        that match
          case Conc(l2, r2) => 
            println(f"l1 is $l1 l2 is $l2  r1 is $r1 r2 is $r2")
            l1 == l2 && r1 <= r2
          case _ => false
    
    def < (that: IndentWidth): Boolean =
      this <= that && !(that <= this)
      
    
    override def toString: String =  
      this match
        case Run(ch, n) => 
          val kind = ch match
            case ' ' => "space"
            case '\t' => "tab"
            case _ => f"$ch-character"
          val suffix = if n == 1 then "" else "s"
          f"$n $kind$suffix"
        case Conc(l, r) =>
          f"$l, $r"
     
  object IndentWidth:
    private inline val MaxCached = 40
    // spaces and tabs 都是叶子节点
    private val spaces: IArray[Run] = IArray.tabulate(MaxCached + 1)(Run(' ', _))
    private val tabs: IArray[Run] = IArray.tabulate(MaxCached + 1)(Run('\t', _))
    
    def IRun(ch: Char, n: Int): Run =
      if n <= MaxCached && ch == ' ' then 
        spaces(n) 
      else if n <= MaxCached && ch == '\t' then 
        tabs(n)
      else
        Run(ch, n)
    end IRun 
    
    def IConc(left: IndentWidth, right: Run): IndentWidth =
      Conc(left, right)
    end IConc 
    
    val Zero = Run(' ', 0)
  end IndentWidth
  
  @main def optional_braces_start(): Unit = {
    
    val x = -10
    if (x < 0) {
      println("i")
      println("j")
      println("k")  
    }
    
    if (x < 0)  
      println(1)
      println(2)
    else  
      println(3)
      println(4)
    println(5)
    def f(x: Int, fun: Int => Int) = x 
   
    println(IndentWidth.Zero)
    println(IndentWidth.IRun('\t', 3))
    val r10 = IndentWidth.IRun('\t', 3)
    val r11 = IndentWidth.IRun(' ', 4)
    val node = IndentWidth.IConc(r10, r11)
    val r12 = IndentWidth.IRun(' ', 5)
    val node2 = IndentWidth.IConc(node, r12)
    println(f"node: $node")
    println(f"node2: $node2")
    
    println(node2 <= node2)

  }

}
