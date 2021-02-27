package parser_combinator

object scala3_parser {
  type Token = Int 
  val LPAREN = 3
  
  enum Region:
    def outer: Region | Null 
    def isOutermost: Boolean = outer == null 
    def enclosing: Region = outer
    var knownWidth: IndentWidth | Null = null 
    final def indentWidth: IndentWidth =
      if knownWidth == null then ??? else knownWidth
    
    def proposeKnownWidth(width: IndentWidth, lastToken: Token): Unit =
      if knownWidth == null then 
        this match
          case InParens(_, _) if lastToken != LPAREN =>
            useOuterWidth()
          case _ =>
            knownWidth = width
            
    def useOuterWidth(): Unit = 
      if enclosing.knownWidth == null then enclosing.useOuterWidth() 
      knownWidth = enclosing.knownWidth
    
    case InString(mutline: Boolean, outer: Region) // s"ddd${n} ${c} "
    case InParens(perfix: Token, outer: Region) // [], or ()
    case InBraces(outer: Region) // {}
    case Incase(outer: Region) // case 
    case Indented(width: IndentWidth, others: Set[IndentWidth], prefix: Token, outer: Region | Null) // ______空格, \t\t\t\t制表符
  
  enum IndentWidth:
    case Run(ch: Char, n: Int) // 可以看成是递归终止的条件
    case Conc(l: IndentWidth, r: Run)
  
    def <= (that: IndentWidth): Boolean = this match
      case Run(ch1, n1) =>
        that match
          case Run(ch2, n2) => n1 <= n2 && (ch1 == ch2 || n1 == 0)
          case Conc(l, _) => this <= l

      case Conc(l1, r1) => 
        that match
          case Conc(l2, r2) => l1 == l2 && r1 <= r2
          case _ => false 
  
    def < (that: IndentWidth): Boolean = this <= that && !(that <= this)
  
    def toPrefix: String = this match  
      case Run(ch, n) => ch.toString * n
      case Conc(l, r) => l.toPrefix ++ r.toPrefix

    override def toString: String = {
      def kind(ch: Char) = ch match {
        case ' ' => "space"
        case '\t' => "tab"
        case _ => s"'$ch'-character"
      }
      this match {
        case Run(ch, n) => s"$n ${kind(ch)}${if (n == 1) "" else "s"}"
        case Conc(l, r) => s"$l, $r"
      }
    }
  
    
    
  
}
