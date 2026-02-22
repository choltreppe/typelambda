version       = "0.1.0"
author        = "Joel Lienhard"
description   = "deduce typed-lambda types"
license       = "MIT"
backend       = "js"
srcDir        = "src"
binDir        = "html"
bin           = @["main"]


requires "nim >= 2.2.6"
requires "fusion"
requires "results"
requires "parlexgen"
requires "karax"


before build:
  mkDir "html"
  cpFile "src/index.html", "html/index.html"
  exec "sassc src/style.sass html/style.css"