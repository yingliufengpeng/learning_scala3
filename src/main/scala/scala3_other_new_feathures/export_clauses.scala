package scala3_other_new_feathures

import java.util.Scanner

object export_clauses {
  
  class BitMap 
  class InkJet 
  class Printer:
    type PrinterType 
    def print(bits: BitMap): Unit = ()
    def status: List[String] = List("jj")
    def status2 = status 
  
  class Scanner2:
    def scan(): BitMap = BitMap()
    def status: List[String] = List("kk")
  
  class Copier:
    private val printUnit = new Printer {
      override type PrinterType = InkJet}
    private val scanUnit = Scanner2()
  
    export scanUnit.scan 
    export printUnit.{status => pstatus, _}
  
    def status: List[String] = pstatus ++ scanUnit.status ++ status2
  
  
  class C { type T}
  object O {val c: C = C()}
  export O.c 
  def f:c.T = ???
  
  // If an export clause contains a wildcard or given selector, it is forbiddent for 
  // its qualifier path to refer to a package. This is because it is not yet 
  // known how to sefely track wilcard dependencies to a package for the purpose 
  // of incrmental compilation 
  
  @main def export_clauses_start(): Unit = {
    val copier = Copier() 
    println(f"copier is ${copier.scan()}")
    
    val r2 = copier.status
    println(f"r2 is $r2")
    
    val r3 = copier.status2
    println(f"r3 is $r3")
  }

}
