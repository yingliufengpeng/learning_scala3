package dotty_new.tools.dot.util

object Dotty_Grammar {

  // Tree CONSTRUCTION ----
  // Convert tree to formal parameter list 
  // Convert tree to formal parameter
  // Convert (qual)ident to type identifier
  
  // PLACEHODERS
  // the implicit parameters introduced by _ in the current expression. Parameters appear in the reverse order
  
  // COMBINATORS 组合器
  
  // part {seqparator part }
  
  // Parse body while checking (under -no-indent) that a { is not missing before it. This is done as fllows: 
  // If the next token S is indented relative to the current reion, and the end of body is followed by a new 
  // line and another statement, check that other statement is indented less then S.
  
  
  // Check that this is not the start of a statement that's indented relative to the current region.
  
  // The last offset where a colon at the end of line would be requied if a subsequent { ... } block 
  // would be converted to an indentation region
  
  // Parse indentation region body and rewrite it to be in braces instead
  
  // The region to eliminate when replacing an opening ( or { that ends a line. The (  or { is at in.offset
  
  // The region to eliminate when replacing a closing ) or } that starts a new line the ( or } precedes in.
  
  // Parse brace-enclosed body and rewrite it to be an indentation region instead, if possible. If possible means:
  //  1 not inside (), [], case ... => 
  //  2 opening brace { is at end of line
  //  3 closing brace } is start of the line 
  //  4 there is at least one token between the braces
  //  5 the closing brace is also at the end of the line, or it is followed by one of then, else, do, catch, finally, yield
  //    or match 
  //  6 the opening brace does not follow a =>. The reason for this condition is that rewriting back to braces does not work 
  //    after => (since in most cases braces are omitted after a => it would be annoying if braces were inserted)
  
  
  // Drop (...) or {...} replacing the closing element with endStr
  
  // If all other characters on the same line as span are blanks, widen to the whole line
  
  // Drop current token, if it is a then or do 
  
  // rewrite code with (...) around the souce code at t 
  
  // In the tokens following the current one, does query precede any of tokens that
  //  1 must start a statement, or
  //  2 separate two statements, or 
  //  3 continue a statement (e.g else, catch), or 
  //  4 terminate the current scope?
  
  // Is the following sequence the generators of a for-expression enclosed in (...)?
  
  // Are the next token the "GivenSig" part of a given definition, i,e. an identifier followed by type and value parameters,
  //   followed by :?
  
  // Is current ident a *, and is it followed by ) or ,) ?
  
  
  // OPERAND/OPERATOR STACK
  
  // operand { infixop operand | MatchClause} [postfixop], 
  // respecting rules of associativity and precedence.
  
  // Accept identifier and return its name as a term name
  
  // Accept indentifier and return Ident with its name as a term name 
  
  // Accept identifier and return Iden with its name as a type name
  
  // Accept identifier or match clause acting as selector on given tree t 
  
  // DotSelectors ::= {.'id}
  
  // SimpleRef ::= id    |  [id '.'] 'this'    |  [id '.'] 'super' [ClassQualifier] '.' id 
  
  // MixinQualifier ::= [id]
  
  // Qualid ::= id {.id}
  
  // SinleTon ::= SimpleRef | SimpleLiteral | SingleTon '.' id -- not yet | Singleton '(' Singletons ')' -- not yet | SingleTon '[ Types ']'
  
  // SimpleLiteral ::= ['-'] interLiteral |  ['-'] floatingPointLiteral   | booleanLiteral |  characterLiteral |  stringLiteral 
  
  // Literal ::= SimpleLiteral  | proessedStringLiteral |  symbolLiteral | 'null' 
  
  // NEW LINES 
  
  // TYPES 
  
  // Same as typ, but if this results in a wildcard it emits a syntax error and return a tree fro type Any instead 
  
  // Type ::= FunType  | HKTypeParamClause '=>>' Type   |   FunParamClause '=>>' 'Type'   |  MatchType  |  InfixType 
  
  // FunType ::= (MonoFunType | PolyFunType) 
  
  // MonoFunType ::= FunArgTypes ('=> | '?=>') Type 
  
  // PolyFunType ::= HKTypeParamClause '=> ' Type
  
  // FunArgTypes ::= InfixType |  ('[[' ['using'] )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}
