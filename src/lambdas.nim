import std/[parseutils, strutils, strformat, tables, sets, options, sugar]
import fusion/matching
import results, parlexgen
include karax/prelude


type
  TypeExprKind* = enum baseType, funcType
  TypeExpr* = ref object
    case kind*: TypeExprKind
    of baseType: name*: string
    of funcType: lhs*, rhs*: TypeExpr

  LambdaExprKind* = enum
    lekVar="VAR", lekInt="INT", lekBool="BOOL",
    lekAbs="ABS", lekApp="APP",
    lekIf="IF", lekOp

  Op* = enum opAdd="+", opSub="-", opMul="*", opDiv="/"

  LambdaExpr* = ref object
    case kind*: LambdaExprKind
    of lekVar: name*: string
    of lekInt: num*: int
    of lekBool: bit*: bool
    of lekAbs:
      param*: string
      paramType*: TypeExpr
      body*: LambdaExpr
    of lekApp:
      lhs*, rhs*: LambdaExpr
    of lekIf:
      cond*, ifBody*, elseBody*: LambdaExpr
    of lekOp:
      op*: Op
      opl*, opr*: LambdaExpr

  SyntaxError* = object
    pos*: Slice[int]
    msg*: string

const opNames*: array[Op, string] = [opAdd: "ADD", opSub: "SUB", opMul: "MUL", opDiv: "DIV"]

func newBaseType*(name: string): TypeExpr =
  TypeExpr(kind: baseType, name: name)

func `->`*(lhs, rhs: TypeExpr): TypeExpr =
  TypeExpr(kind: funcType, lhs: lhs, rhs: rhs)


func `$`*(expr: TypeExpr): string =
  case expr.kind
  of baseType: expr.name
  of funcType:
    (
      case expr.lhs.kind
      of baseType: $expr.lhs
      of funcType: &"({expr.lhs})"
    ) &
    &"→{expr.rhs}"

func `$`*(expr: LambdaExpr): string =
  case expr.kind
  of lekVar: expr.name
  of lekInt: $expr.num
  of lekBool: $expr.bit
  of lekAbs:
    if expr.paramType == nil:
      &"λ{expr.param}.{expr.body}"
    else:
      &"λ{expr.param}:{expr.paramType}.{expr.body}"
  of lekApp:
    (
      if expr.lhs.kind != lekAbs: $expr.lhs
      else: &"({expr.lhs})"
    ) & " " & (
      if expr.rhs.kind != lekApp: $expr.rhs
      else: &"({expr.rhs})"
    )
  of lekIf: &"if {expr.cond} then {expr.ifBody} else {expr.elseBody}"
  of lekOp: &"{expr.opl} {expr.op} {expr.opr}"


proc render*(expr: TypeExpr): seq[VNode] =
  if expr == nil:
    result = @[buildHtml(span(class = "type-error"))]
  else:
    result = @[]
    case expr.kind
    of baseType: result &= text expr.name
    of funcType:
      case expr.lhs.kind
      of baseType:
        result &= render(expr.lhs)
      of funcType:
        result &= text "("
        result &= render(expr.lhs)
        result &= text ")"
      result &= text "→"
      result &= render(expr.rhs)


func `==`*(a, b: TypeExpr): bool =
  if system.`==`(a, nil) or system.`==`(b, nil) or a.kind != b.kind: false
  else:
    case a.kind
    of baseType: a.name == b.name
    of funcType: a.lhs == b.lhs and a.rhs == b.rhs


proc parseLambda*(code: string): Result[LambdaExpr, SyntaxError] =

  type
    TokenKind = enum
      IDENT, NUM,
      TRUE="true", FALSE="false",
      LPAR="(", RPAR=")",
      LAMBDA="λ", DOT=".",
      COLON=":", ARROW="→",
      IF="if", THEN="then", ELSE="else",
      ADD="+", SUB="-", MUL="*", DIV="/"

    Token = object
      case kind: TokenKind
      of IDENT: name: string
      of NUM: num: int
      else: discard
      pos: Slice[int]
      col: int

  const tokenPatterns = [
    TRUE: "true", FALSE: "false",
    LPAR: r"\(", RPAR: r"\)",
    LAMBDA: r"λ|\\", DOT: r"\.",
    COLON: r"\:", ARROW: r"\-\>|→",
    IF: "if", THEN: "then", ELSE: "else",
    ADD: r"\+", SUB: r"\-", MUL: r"\*", DIV: "/"
  ]

  const identPattern = r"[a-zA-Z0-9\_]+"
  
  makeLexer lex[Token]:
    for k in TRUE..high(TokenKind):
      (tokenPatterns[k]): Token(kind: k, pos: pos ..< pos+len(match), col: runeCol)
    r"\-?[0-9]+": Token(kind: NUM, num: parseInt(match), pos: pos ..< pos+len(match), col: runeCol)
    identPattern: Token(kind: IDENT, name: match, pos: pos ..< pos+len(match), col: runeCol)
    r"\s": discard

  makeParser parse[Token]:

    abstraction[LambdaExpr]:
      (LAMBDA, IDENT, COLON, type_expr, DOT, abstraction):
        LambdaExpr(kind: lekAbs, param: s[1].name, paramType: s[3], body: s[5])
      if_expr: s[0]

    if_expr[LambdaExpr]:
      (IF, application, THEN, application, ELSE, application):
        LambdaExpr(kind: lekIf, cond: s[1], ifBody: s[3], elseBody: s[5])
      application: s[0]

    application[LambdaExpr]:
      (application, op1): LambdaExpr(kind: lekApp, lhs: s[0], rhs: s[1])
      op1: s[0]

    op1[LambdaExpr]:
      (op1, MUL, op2): LambdaExpr(kind: lekOp, op: opMul, opl: s[0], opr: s[2])
      (op1, DIV, op2): LambdaExpr(kind: lekOp, op: opDiv, opl: s[0], opr: s[2])
      op2: s[0]

    op2[LambdaExpr]:
      (op2, ADD, variable): LambdaExpr(kind: lekOp, op: opAdd, opl: s[0], opr: s[2])
      (op2, SUB, variable): LambdaExpr(kind: lekOp, op: opSub, opl: s[0], opr: s[2])
      variable: s[0]

    variable[LambdaExpr]:
      IDENT: LambdaExpr(kind: lekVar, name: s[0].name)
      NUM: LambdaExpr(kind: lekInt, num: s[0].num)
      TRUE: LambdaExpr(kind: lekBool, bit: true)
      FALSE: LambdaExpr(kind: lekBool, bit: false)
      (LPAR, abstraction, RPAR): s[1]

    type_expr[TypeExpr]:
      IDENT: newBaseType(s[0].name)
      (LPAR, type_expr, RPAR): s[1]
      (IDENT, ARROW, type_expr): newBaseType(s[0].name) -> s[2]
      (LPAR, type_expr, RPAR, ARROW, type_expr): s[1] -> s[4]

  try: ok(parse(code, lex))

  except LexingError as e:
    err(SyntaxError(pos: e.pos..e.pos, msg: &"{e.runeCol}: unexpected '{code[e.pos]}'"))

  except ParsingError[Token, TokenKind] as e:
    
    var expectedList = collect:
      for kind in e.expectedTerminals:
        case kind
        of IDENT: "an identifier"
        of NUM: "a number literal"
        else: "\'" & $kind & "\'"

    if e.expectedEOF:
      expectedList.add "EOL"
    assert len(expectedList) > 0

    let expectedStr =
      if len(expectedList) == 1: expectedList[0]
      else: expectedList[0 ..< ^1].join(", ") & " or " & expectedList[^1]

    let foundStr = 
      if Some(@token) ?= e.token:
        "\'" & (
          case token.kind
          of IDENT: token.name
          of NUM: $token.num
          else: $token.kind
        ) & "\'"
      else: "EOL"
    
    var msg = "found " & foundStr & ", but expected " & expectedStr

    let pos =
      if Some(@token) ?= e.token:
        msg = &"{token.col}: {msg}"
        token.pos
      else: high(code) .. high(code)

    err(SyntaxError(pos: pos, msg: msg))