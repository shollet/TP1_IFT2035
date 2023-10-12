# TP1_IFT2035

Voici les tests que j'ai fais pour s2l:

ghci> s2l (Snum 5)  -- 5
->Llit 5
ghci> s2l (Snum 42)   -- 42
->Llit 42

ghci> s2l (Ssym "x")    -- "x"
->Lid "x"
ghci> s2l (Ssym "hello")    -- "hello"
->Lid "hello"


ghci> s2l (Snode (Ssym "lambda") [Snode (Ssym "x") [], Ssym "y"])   -- (lambda (x) y)
->Labs "x" (Lid "y")
ghci> s2l (Snode (Ssym "lambda") [Snode (Ssym "x") [], Snum 5])     -- (lambda (x) 5)
->Labs "x" (Llit 5)

ghci> s2l (Snode (Ssym "lambda") [Snode (Ssym "x") [Ssym "y", Ssym "z"], Ssym "x"])  -- (lambda (x y z) x)
->Lrec [("x",Lid "x"),("y",Lid "y"),("z",Lid "z")] (Lid "x")
ghci> s2l (Snode (Ssym "lambda") [Ssym "x"])    -- (lambda x)
-> error
ghci> s2l (Snode (Ssym "lambda") [Ssym "x", Ssym "y", Ssym "z"])    -- (lambda x y z)
-> error

ghci> s2l (Snode (Ssym "if") [Ssym "x", Ssym "y", Ssym "z"])    -- (if x then y else z)
->Lite (Lid "x") (Lid "y") (Lid "z")
ghci> s2l (Snode (Ssym "if") [Ssym "x", Snum 1, Snum 0])    -- (if x then 1 else 0)
->Lite (Lid "x") (Llit 1) (Llit 0)
ghci> s2l (Snode (Ssym "if") [Ssym "x", Snum 1])    -- (if x then 1)
-> error

ghci> s2l (Snode (Ssym "let") [Snode (Ssym "x") [], Ssym "y", Ssym "z"])    -- (let x is y in z)
->Ldec "x" (Lid "y") (Lid "z")
ghci> s2l (Snode (Ssym "let") [Snode (Ssym "x") [], Snum 5, Ssym "x"])  -- (let x is 5 in x)
->Ldec "x" (Llit 5) (Lid "x")
ghci> s2l (Snode (Ssym "let") [Snode (Ssym "x") [Ssym "y", Ssym "z"], Snum 5, Ssym "y"])  -- (let x is 5 in let y is 5 let z is 5 in y)
->Lrec [("x",Lid "x"),("y",Lid "y"),("z",Lid "z")] (Lid "y")
ghci> s2l (Snode (Ssym "let") [Ssym "x", Snum 1])
-> error

ghci> s2l (Snode (Ssym "set!") [Ssym "x", Ssym "y"])
->Lassign (Lid "x") (Lid "y")

ghci> s2l (Snode (Ssym "begin") [Ssym "x"])
->Lid "x"

ghci> s2l (Snode (Ssym "begin") [Ssym "x", Ssym "y"])
->Lassign (Lid "x") (Lid "y")

ghci> s2l (Snode (Ssym "quote") [Ssym "x"])
->Lid "x"

ghci> s2l (Snode (Ssym "cons") [Ssym "x", Ssym "y"])
->Lfuncall (Lid "cons") [Lid "x",Lid "y"]

ghci> s2l (Snode (Ssym "car") [Ssym "x"])
->Lfuncall (Lid "car") [Lid "x"]

ghci> s2l (Snode (Ssym "cdr") [Ssym "x"])
->Lfuncall (Lid "cdr") [Lid "x"]

ghci> s2l (Snode (Ssym "ref!") [Ssym "x"])
->Lmkref (Lid "x")

ghci> s2l (Snode (Ssym "deref") [Ssym "x"])
Lderef (Lid "x")

ghci> s2l (Snode (Ssym "setref!") [Ssym "x", Ssym "y"])
->Lassign (Lid "x") (Lid "y")

ghci> s2l (Snode (Ssym "letrec") [Snode (Ssym "x") [], Ssym "y", Ssym "z"])
->Lrec [("x",Lfuncall (Lid "fix") [Labs "x" (Lid "y")])] (Lid "z")

ghci> s2l (Snode (Ssym "letrec") [Snode (Ssym "x") [Ssym "a"], Ssym "y", Ssym "z"])
->Lrec [("x",Lfuncall (Lid "fix") [Labs "x" (Lid "y")]),("a",Lfuncall (Lid "fix") [Labs "x" (Lid "y")])] (Lid "z")

ghci> s2l (Snode (Ssym "fix") [Snode (Ssym "x") [], Ssym "y"])
->Lrec [("x",Lfuncall (Lid "fix") [Labs "x" (Lid "y")])] (Lid "x")

ghci> s2l (Snode (Ssym "fix") [Snode (Ssym "x") [Ssym "a"], Ssym "y"])
->Lrec [("x",Lfuncall (Lid "fix") [Labs "x" (Lid "y")]),("a",Lfuncall (Lid "fix") [Labs "x" (Lid "y")])] (Lid "x")