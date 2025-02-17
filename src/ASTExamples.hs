module ASTExamples where

import Data.Map qualified as Map
import Syntax

-- assign.js
wAssign :: Block
wAssign = Block [Assign (Name "x") (Val (BoolVal True)), Assign (Name "y") (Val (BoolVal False))]

-- assignConflict.js
wAssignConflict :: Block
wAssignConflict =
  Block
    [ Assign (Name "x") (Val (BoolVal True)),
      Assign (Name "y") (Val (BoolVal False)),
      Assign (Name "x") (Val (NumberVal 1))
    ]

-- if.js
wIf :: Block
wIf =
  Block
    [ Assign (Name "x") (Val (BoolVal True)),
      Assign (Name "y") (Val (NumberVal 2)),
      If
        (Op2 (Var (Name "x")) Eq (Val (BoolVal True)))
        (Block [Update (Name "y") (Val (NumberVal 3))])
        (Block [Update (Name "y") (Val (NumberVal 4))])
    ]

-- updateConflict.js
wUpdateConflict :: Block
wUpdateConflict =
  Block
    [ Assign (Name "x") (Val (NumberVal 3)),
      Update (Name "x") (Val (StringVal "string"))
    ]

-- ifLiteral.js
wIfLiteral :: Block
wIfLiteral =
  Block
    [ Assign (Name "x") (Val (BoolVal True)),
      Assign (Name "y") (Val (NumberVal 2)),
      If
        (Var (Name "x"))
        (Block [Update (Name "x") (Val (BoolVal False)), Update (Name "y") (Val (NumberVal 3))])
        (Block [Update (Name "y") (Val (NumberVal 4))])
    ]

-- ifBranchConflict.js
wIfBranchConflict :: Block
wIfBranchConflict =
  Block
    [ Assign (Name "x") (Val (BoolVal True)),
      Assign (Name "y") (Val (NumberVal 2)),
      If
        (Var (Name "x"))
        (Block [Update (Name "x") (Val (NumberVal 5)), Update (Name "y") (Val (NumberVal 3))])
        (Block [Update (Name "y") (Val (NumberVal 4))])
    ]

-- while.js
wWhile :: Block
wWhile =
  Block
    [ Assign (Name "x") (Val (ObjectVal (Map.fromList [("y", NumberVal 2), ("z", BoolVal True)]))),
      While
        (Op2 (Var (Dot (Var (Name "x")) "z")) Eq (Val (BoolVal True)))
        ( Block
            [ Update (Dot (Var (Name "x")) "y") (Val (NumberVal 3)),
              Update (Dot (Var (Name "x")) "z") (Val (BoolVal False))
            ]
        )
    ]

-- whlieCondConflict.js
wWhileCondConflict :: Block
wWhileCondConflict =
  Block
    [ Assign (Name "x") (Val (ObjectVal (Map.fromList [("y", NumberVal 2), ("z", BoolVal True)]))),
      While
        (Op2 (Op2 (Var (Dot (Var (Name "x")) "z")) Plus (Val (NumberVal 3))) Eq (Val (NumberVal 2)))
        ( Block
            [ Update (Dot (Var (Name "x")) "y") (Val (NumberVal 3)),
              Update (Dot (Var (Name "x")) "z") (Val (BoolVal False))
            ]
        )
    ]

wFor :: Block
wFor =
  Block
    [ Assign (Name "x") (Val (NumberVal 0)),
      Assign (Name "y") (Val (BoolVal True)),
      Assign (Name "z") (Val (StringVal "checker")),
      For
        (Assign (Name "i") (Val (NumberVal 0)))
        (Op2 (Var (Name "i")) Lt (Val (NumberVal 10)))
        (Update (Name "i") (Op2 (Var (Name "i")) Plus (Val (NumberVal 1))))
        (Block [Update (Name "x") (Op2 (Var (Name "x")) Plus (Val (NumberVal 1))), Update (Name "y") (Val (BoolVal False)), Update (Name "z") (Val (StringVal "checker"))])
    ]

wFunctionDef :: Block
wFunctionDef =
  Block
    [ FunctionDef
        "f"
        (FunctionType [("x", NumberType), ("y", NumberType)] NumberType)
        (Block [Return (Op2 (Var (Name "x")) Plus (Var (Name "y")))])
    ]

-- forWrongVar.js
wForWrongVar :: Block
wForWrongVar =
  Block
    [ Assign (Name "x") (Val (NumberVal 0)),
      Assign (Name "y") (Val (BoolVal True)),
      Assign (Name "z") (Val (StringVal "checker")),
      For
        (Assign (Name "i") (Val (NumberVal 0)))
        (Op2 (Var (Name "i")) Lt (Val (NumberVal 10)))
        (Update (Name "j") (Op2 (Var (Name "i")) Plus (Val (NumberVal 1))))
        (Block [Update (Name "x") (Op2 (Var (Name "x")) Plus (Val (NumberVal 1))), Update (Name "y") (Val (BoolVal False)), Update (Name "z") (Val (StringVal "checker"))])
    ]

-- forNoAssign.js
wForNoAssign :: Block
wForNoAssign =
  Block
    [ Assign (Name "x") (Val (NumberVal 0)),
      Assign (Name "y") (Val (BoolVal True)),
      Assign (Name "z") (Val (StringVal "checker")),
      For
        (Update (Name "i") (Val (NumberVal 0)))
        (Op2 (Var (Name "i")) Lt (Val (NumberVal 10)))
        (Update (Name "i") (Op2 (Var (Name "i")) Plus (Val (NumberVal 1))))
        (Block [Update (Name "x") (Op2 (Var (Name "x")) Plus (Val (NumberVal 1))), Update (Name "y") (Val (BoolVal False)), Update (Name "z") (Val (StringVal "checker"))])
    ]