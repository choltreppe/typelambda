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
      input(`type` = "text", value = exprStr)
      input(`type` = "submit", value = "compute")
      proc onsubmit(e: Event, n: VNode) =
        e.preventDefault()
        window.location.hash = $n.dom[0].InputElement.value

    if deduction.isOk():
      render(deduction.vResultPrivate)
    elif (let e = deduction.error; e.msg != ""):
      tdiv(id = "error"): text e.msg