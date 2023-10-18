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


ghci> s2l (Snode (Ssym "lambda") [Snode (Ssym "x") [], Snum 5])     -- (lambda x 5)
->Labs "x" (Llit 5)
ghci> s2l (Snode (Ssym "lambda") [Snode (Ssym "x") [], Snode (Ssym "lambda") [Snode (Ssym "y") [], Snum 5]])     -- (lambda x (lambda y 5))
->Labs "x" (Labs "y" (Llit 5))


ghci> s2l (Snode (Ssym "ref!") [Snum 5])     -- (ref! 5)
->Lmkref (Llit 5)
ghci> s2l (Snode (Ssym "ref!") [Snode (Ssym "lambda") [Snode (Ssym "x") [], Snum 5]])     -- (ref! (lambda x 5))
->Lmkref (Labs "x" (Llit 5))


ghci> s2l (Snode (Ssym "get!") [Ssym "x"])   -- (get! x)
->Lderef (Lid "x")
ghci> s2l (Snode (Ssym "get!") [Snode (Ssym "lambda") [Snode (Ssym "x") [], Snum 5]])   -- (get! (lambda x 5))
->Lderef (Labs "x" (Llit 5))


ghci> s2l (Snode (Ssym "set!") [Ssym "x", Snum 5])   -- (set! x 5)
->Lassign (Lid "x") (Llit 5)
ghci> s2l (Snode (Ssym "set!") [Snode (Ssym "lambda") [Snode (Ssym "x") [], Snum 5], Snum 5])   -- (set! (lambda x 5) 5)
->Lassign (Labs "x" (Llit 5)) (Llit 5)

ghci> s2l (Snode (Ssym "+") [Snum 3, Snum 4])   -- (+ 3 4)
->Lfuncall (Lid "+") [Llit 3,Llit 4]
ghci> s2l (Snode (Ssym "+") [Snode (Ssym "lambda") [Snode (Ssym "x") [], Snum 5], Snum 4])   -- (+ (lambda x 5) 4)
->Lfuncall (Lid "+") [Labs "x" (Llit 5),Llit 4]

ghci> s2l (Snode (Ssym "-") [Snum 7, Snum 2])   -- (- 7 2)
->Lfuncall (Lid "-") [Llit 7,Llit 2]
ghci> s2l (Snode (Ssym "-") [Snode (Ssym "lambda") [Snode (Ssym "x") [], Snum 5], Snum 4])   -- (- (lambda x 5) 4)
->Lfuncall (Lid "-") [Labs "x" (Llit 5),Llit 4]


ghci> s2l (Snode (Ssym "*") [Snum 6, Snum 3])   -- (* 6 3)
->Lfuncall (Lid "*") [Llit 6,Llit 3]
ghci> s2l (Snode (Ssym "*") [Snode (Ssym "lambda") [Snode (Ssym "x") [], Snum 5], Snum 4])   -- (* (lambda x 5) 4)
->Lfuncall (Lid "*") [Labs "x" (Llit 5),Llit 4]


ghci> s2l (Snode (Ssym "/") [Snum 8, Snum 4])   -- (/ 8 4)
->Lfuncall (Lid "/") [Llit 8,Llit 4]
ghci> s2l (Snode (Ssym "/") [Snode (Ssym "lambda") [Snode (Ssym "x") [], Snum 5], Snum 4])   -- (/ (lambda x 5) 4)
->Lfuncall (Lid "/") [Labs "x" (Llit 5),Llit 4]

ghci> s2l (Snode (Ssym "<") [Snum 3, Snum 4])   -- (< 3 4)
->Lfuncall (Lid "<") [Llit 3,Llit 4]
ghci> s2l (Snode (Ssym "<") [Snode (Ssym "lambda") [Snode (Ssym "x") [], Snum 5], Snum 4])   -- (< (lambda x 5) 4)
->Lfuncall (Lid "<") [Labs "x" (Llit 5),Llit 4]


ghci> s2l (Snode (Ssym ">") [Snum 5, Snum 2])   -- (> 5 2)
->Lfuncall (Lid ">") [Llit 5,Llit 2]
ghci> s2l (Snode (Ssym ">") [Snode (Ssym "lambda") [Snode (Ssym "x") [], Snum 5], Snum 4])   -- (> (lambda x 5) 4)
->Lfuncall (Lid ">") [Labs "x" (Llit 5),Llit 4]


ghci> s2l (Snode (Ssym "=") [Snum 5, Snum 5])   -- (= 5 5)
->Lfuncall (Lid "=") [Llit 5,Llit 5]
ghci> s2l (Snode (Ssym "=") [Snode (Ssym "lambda") [Snode (Ssym "x") [], Snum 5], Snum 4])   -- (= (lambda x 5) 4)
->Lfuncall (Lid "=") [Labs "x" (Llit 5),Llit 4]


ghci> s2l (Snode (Ssym "<=") [Snum 7, Snum 8])  -- (<= 7 8)
->Lfuncall (Lid "<=") [Llit 7,Llit 8]
ghci> s2l (Snode (Ssym "<=") [Snode (Ssym "lambda") [Snode (Ssym "x") [], Snum 5], Snum 4])  -- (<= (lambda x 5) 4)
->Lfuncall (Lid "<=") [Labs "x" (Llit 5),Llit 4]


ghci> s2l (Snode (Ssym ">=") [Snum 7, Snum 8])  -- (>= 7 8)
->Lfuncall (Lid ">=") [Llit 7,Llit 8]
ghci> s2l (Snode (Ssym ">=") [Snode (Ssym "lambda") [Snode (Ssym "x") [], Snum 5], Snum 4])  -- (>= (lambda x 5) 4)
->Lfuncall (Lid ">=") [Labs "x" (Llit 5),Llit 4]


ghci> s2l (Snode (Ssym "if") [Snode (Ssym "<") [Snum 5, Snum 10], Snum 1, Snum (-1)])   -- (if (< 5 10) 1 -1)
->Lite (Lfuncall (Lid "<") [Llit 5,Llit 10]) (Llit 1) (Llit (-1))
ghci> s2l (Snode (Ssym "if") [Snode (Ssym "<") [Snum 5, Snum 10], Snode (Ssym "lambda") [Snode (Ssym "x") [], Snum 5], Snum (-1)])   -- (if (< 5 10) (lambda x 5) -1)
->Lite (Lfuncall (Lid "<") [Llit 5,Llit 10]) (Labs "x" (Llit 5)) (Llit (-1))


ghci> s2l (Snode (Ssym "let") [Snode (Ssym "x") [], Snum 5, Snode (Ssym "+") [Ssym "x", Snum 3]])   -- (let (x 5) (+ x 3))
->Ldec "x" (Llit 5) (Lfuncall (Lid "+") [Lid "x",Llit 3])
ghci> s2l (Snode (Ssym "let") [Snode (Ssym "x") [], Snum 5, Snode (Ssym "let") [Snode (Ssym "y") [], Snum 3, Snode (Ssym "+") [Ssym "x", Ssym "y"]]])   -- (let (x 5) (let (y 3) (+ x y)))
->Ldec "x" (Llit 5) (Ldec "y" (Llit 3) (Lfuncall (Lid "+") [Lid "x",Lid "y"]))


ghci> s2l (Snode (Ssym "letrec") [Snode (Ssym "f") [], Snode (Ssym "lambda") [Snode (Ssym "x") [], Snode (Ssym "+") [Ssym "x", Snum 1]], Snode (Ssym "f") [Snum 5]])    -- (letrec (f (lambda x (+ x 1))) (f 5))
->Lrec [("f",Labs "x" (Lfuncall (Lid "+") [Lid "x",Llit 1]))] (Lfuncall (Lid "f") [Llit 5])
ghci> s2l (Snode (Ssym "letrec") [Snode (Ssym "f") [], Snode (Ssym "lambda") [Snode (Ssym "x") [], Snode (Ssym "+") [Ssym "x", Snum 1]], Snode (Ssym "letrec") [Snode (Ssym "g") [], Snode (Ssym "lambda") [Snode (Ssym "y") [], Snode (Ssym "+") [Ssym "y", Snum 1]], Snode (Ssym "g") [Snum 5]]])    -- (letrec (f (lambda x (+ x 1))) (letrec (g (lambda y (+ y 1))) (g 5)))
->Lrec [("f",Labs "x" (Lfuncall (Lid "+") [Lid "x",Llit 1]))] (Lrec [("g",Labs "y" (Lfuncall (Lid "+") [Lid "y",Llit 1]))] (Lfuncall (Lid "g") [Llit 5]))