import std/[dom, uri, sugar]
import results
include karax/prelude

import ./lambdas, ./deductions

var
  exprStr = ""
  deduction: Result[TypeDeduction, SyntaxError]

setRenderer do(route: RouterData) -> VNode:

  let newExprStr =
    if route.hashPart == "": ""
    else: ($route.hashPart)[1..^1].decodeUrl()

  if newExprStr != exprStr:
    exprStr = newExprStr
    if exprStr == "":
      reset deduction
    else:
      deduction = parseLambda(exprStr).map(expr => deduceType(expr))

  buildHtml(tdiv):
    form:
      input(`type` = "text", value = exprStr, placeholder = "enter simply typed lambda expression ..")
      input(`type` = "submit", value = "deduce")
      proc onsubmit(e: Event, n: VNode) =
        e.preventDefault()
        window.location.hash = $n.dom[0].InputElement.value

    if deduction.isOk():
      tdiv(id = "content"):
        render(deduction.vResultPrivate)
      
    elif (let e = deduction.error; e.msg != ""):
      tdiv(id = "error"): text e.msg
    
    else:
      tdiv(id = "info"):
        text "some example expressions, showing whats supported:"
        ul:
          li: text r"λx: Bool → Nat. x true"
          li: text r"\x: Bool -> Nat. x true"
          li: text r"(\x: Bool. if x then 0 else 42) false"
          li: text r"(\f: Int -> Int. \x: Int. (f x) + 2) (\x: Int. x * 2) 4"
