import std/[sequtils, strutils, strformat, options, tables]
import fusion/matching
include karax/prelude

import ./lambdas

type
  TypeDeduction* = ref object
    expr: LambdaExpr
    typ: Option[TypeExpr]
    ctx: Context
    case kind: LambdaExprKind
    of lekVar, lekInt, lekBool: discard
    of lekAbs: body: TypeDeduction
    of lekApp: lhs, rhs: TypeDeduction
    of lekIf: cond, ifBody, elseBody: TypeDeduction
    of lekOp: opl, opr: TypeDeduction

  Context = Table[string, Option[TypeExpr]]

func newContext: Context = discard

func `$`(expr: Option[TypeExpr]): string =
  if Some(@expr) ?= expr: $expr
  else: "_"

func `$`(d: TypeDeduction): string =
  result = "(" & (
    case d.kind
    of lekVar, lekInt, lekBool: @[]
    of lekAbs: @[d.body]
    of lekApp: @[d.lhs, d.rhs]
    of lekIf: @[d.cond, d.ifBody, d.elseBody]
    of lekOp: @[d.opl, d.opr]
  ).mapIt($it).join("  ")
  result.add &"  |  {d.ctx} ⊢ ({d.expr}): {d.typ})"

proc render(expr: Option[TypeExpr]): seq[VNode] =
  if Some(@expr) ?= expr:
    render(expr)
  else:
    @[buildHtml(span(class = "type-error"))]

proc render(ctx: Context): seq[VNode] =
  result = @[text "{"]
  var first = true
  for name, typ in ctx:
    if not first: result &= text ", "
    first = false
    result &= text &"{name}:"
    result &= render(typ)
  result &= text "}"

proc add*(parent: VNode, kids: seq[VNode]) =
  for kid in kids: parent.add(kid)

proc render*(deduction: TypeDeduction): VNode =

  proc renderInner(deduction: TypeDeduction): VNode =
    buildHtml(tdiv(class = "deduction-container")):

      tdiv(class = "name"): text(
        if deduction.kind == lekOp: &"T-{opNames[deduction.expr.op]}"
        else: &"T-{deduction.kind}"
      )

      tdiv(class = "deduction"):

        tdiv(class = "top "&(if deduction.kind == lekVar: "var" else: "")):

          case deduction.kind
          of lekVar:
            text $deduction.expr & ":"
            render(deduction.typ)
            text " ∈ "
            render(deduction.ctx)

          of lekInt, lekBool: discard

          of lekAbs:
            renderInner(deduction.body)

          of lekApp:
            for d in [deduction.lhs, deduction.rhs]:
              renderInner(d)

          of lekIf:
            for d in [deduction.cond, deduction.ifBody, deduction.elseBody]:
              renderInner(d)

          of lekOp:
            for d in [deduction.opl, deduction.opr]:
              renderInner(d)

        tdiv(class = "bottom"):
          render(deduction.ctx)
          text &" ⊢ ({deduction.expr}): "
          render(deduction.typ)

  buildHtml(tdiv(id = "deduction-tree")):
    renderInner(deduction)


func deduceType*(expr: LambdaExpr, ctx = newContext()): TypeDeduction =
  result = TypeDeduction(expr: expr, ctx: ctx, kind: expr.kind)

  case expr.kind
  of lekVar:
    if expr.name in ctx:
      result.typ = ctx[expr.name]

  of lekInt:  result.typ = some(newBaseType"Int")
  of lekBool: result.typ = some(newBaseType"Bool")

  of lekAbs:
    var ctx = ctx
    if expr.param in ctx:
      if (Some(@typ) ?= ctx[expr.param]) and typ != expr.paramType:
        ctx[expr.param] = none(TypeExpr)
    else:
      ctx[expr.param] = some(expr.paramType)
    result.body = deduceType(expr.body, ctx)
    if Some(@typ) ?= result.body.typ:
      result.typ = some(expr.paramType -> typ)

  of lekApp:
    result.lhs = deduceType(expr.lhs, ctx)
    result.rhs = deduceType(expr.rhs, ctx)
    if (Some(@lhsType) ?= result.lhs.typ) and (Some(@rhsType) ?= result.rhs.typ) and
        lhsType.kind == funcType and lhsType.lhs == rhsType:
      result.typ = some(lhsType.rhs)

  of lekIf:
    result.cond     = deduceType(expr.cond    , ctx)
    result.ifBody   = deduceType(expr.ifBody  , ctx)
    result.elseBody = deduceType(expr.elseBody, ctx)
    if (Some(@condType) ?= result.cond.typ) and
        condType.kind == baseType and condType.name == "Bool" and
        result.ifBody.typ == result.elseBody.typ:
      result.typ = result.ifBody.typ

  of lekOp:
    result.opl = deduceType(expr.opl, ctx)
    result.opr = deduceType(expr.opr, ctx)
    if (Some(@oplType) ?= result.opl.typ) and
        result.opl.typ == result.opr.typ and oplType.kind == baseType and
        oplType.name in ["Int", "Nat", "Num", "Float"]:
      result.typ = result.opl.typ