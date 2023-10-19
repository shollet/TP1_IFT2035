-- TP-2  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Évaluateur
-- - Pretty printer

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Librairie d'analyse syntaxique.
import Data.Char        -- Conversion de Chars de/vers Int et autres
-- import Numeric       -- Pour la fonction showInt
import System.IO        -- Pour stdout, hPutStr
-- import Data.Maybe    -- Pour isJust and fromJust

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          | Snode Sexp [Sexp]           -- Une liste non vide
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3) ==> Snode (Ssym "+")
--                   [Snum 2, Snum 3]
--
-- (/ (* (- 68 32) 5) 9)
--     ==>
-- Snode (Ssym "/")
--       [Snode (Ssym "*")
--              [Snode (Ssym "-")
--                     [Snum 68, Snum 32],
--               Snum 5],
--        Snum 9]

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                (pChar '\n' <|> eof); return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote :: Parser Sexp
pQuote = do { pChar '\''; pSpaces; e <- pSexp;
              return (Snode (Ssym "quote") [e]) }

-- Une liste est de la forme:  ( {e} [. e] )
pList :: Parser Sexp
pList  = do { pChar '('; pSpaces;
              ses <- pTail;
                    return (case ses of [] -> Snil
                                        se : ses' -> Snode se ses')
            }
pTail :: Parser [Sexp]
pTail  = do { pChar ')'; return [] }
     -- <|> do { pChar '.'; pSpaces; e <- pSexp; pSpaces;
     --          pChar ')' <|> error ("Missing ')' after: " ++ show e);
     --          return e }
     <|> do { e <- pSexp; pSpaces; es <- pTail; return (e : es) }

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pSpaces;
                pList <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _p s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Snode h t) =
    let showTail [] = showChar ')'
        showTail (e : es) =
            showChar ' ' . showSexp' e . showTail es
    in showChar '(' . showSexp' h . showTail t

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire L(ambda)exp(ression)                     --
---------------------------------------------------------------------------

type Var = String

data Lexp = Llit Int             -- Litéral entier.
          | Lid Var              -- Référence à une variable.
          | Labs Var Lexp        -- Fonction anonyme prenant un argument.
          | Lfuncall Lexp [Lexp] -- Appel de fonction, avec arguments "curried".
          | Lmkref Lexp          -- Construire une "ref-cell".
          | Lderef Lexp          -- Chercher la valeur d'une "ref-cell".
          | Lassign Lexp Lexp    -- Changer la valeur d'une "ref-cell".
          | Lite Lexp Lexp Lexp  -- If/then/else.
          | Ldec Var Lexp Lexp   -- Déclaration locale non-récursive.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Lrec [(Var, Lexp)] Lexp
          deriving (Show, Eq)

-- Conversion de Sexp à Lambda --------------------------------------------

s2l :: Sexp -> Lexp
s2l (Snum n) = Llit n   -- Convertit en litéral entier.

s2l (Ssym s) = Lid s    -- Convertit en référence à une variable.    

s2l (Snode (Ssym "lambda") [Snode (Ssym x) [] , e]) = Labs x (s2l e)   -- Convertit un argument en fonction anonyme.
s2l (Snode (Ssym "lambda") [Snode (Ssym x) xs, e]) =             -- Convertit plusieurs arguments en fonction anonyme.  
    Lrec (map extractSymAndConvert (Ssym x : xs)) (s2l e)
  where
    extractSymAndConvert :: Sexp -> (Var, Lexp)
    extractSymAndConvert (Ssym x') = (x', Lid x')
    extractSymAndConvert _        = error "Expected Ssym in lambda args"
s2l (Snode (Ssym "lambda") _) = error "Mauvais nombre d'arguments"  -- Gestion d'erreurs pour les lambdas mal formées.

s2l (Snode (Ssym "if") [e1, e2, e3]) = Lite (s2l e1) (s2l e2) (s2l e3)  -- Convertit un if/then/else.
s2l (Snode (Ssym "if") _) = error "Mauvais nombre d'arguments"  -- Gestion d'erreurs pour les if mal formées.

s2l (Snode (Ssym "let") [Snode (Ssym x) [], e1, e2]) = Ldec x (s2l e1) (s2l e2) -- Convertit un let/in.
s2l (Snode (Ssym "let") [Snode (Ssym x) xs, _, e2]) =          -- Convertit plusieurs let/in. 
    Lrec (map extractSymAndConvert (Ssym x : xs)) (s2l e2)
  where
    extractSymAndConvert :: Sexp -> (Var, Lexp)
    extractSymAndConvert (Ssym x') = (x', Lid x')
    extractSymAndConvert _        = error "Expected Ssym in let args"
s2l (Snode (Ssym "let") _) = error "Mauvais nombre d'arguments" -- Gestion d'erreurs pour les let mal formées.

s2l (Snode (Ssym "set!") [e1, e2]) = Lassign (s2l e1) (s2l e2)
s2l (Snode (Ssym "set!") _) = error "Mauvais nombre d'arguments"
s2l (Snode (Ssym "begin") []) = error "begin sans arguments"
s2l (Snode (Ssym "begin") [e]) = s2l e
s2l (Snode (Ssym "begin") (e : es)) = Lassign (s2l e) (s2l (Snode (Ssym "begin") es))
s2l (Snode (Ssym "quote") [e]) = s2l e
s2l (Snode (Ssym "quote") _) = error "Mauvais nombre d'arguments"
s2l (Snode (Ssym "cons") [e1, e2]) = Lfuncall (Lid "cons") [s2l e1, s2l e2]
s2l (Snode (Ssym "cons") _) = error "Mauvais nombre d'arguments"
s2l (Snode (Ssym "car") [e]) = Lfuncall (Lid "car") [s2l e]
s2l (Snode (Ssym "car") _) = error "Mauvais nombre d'arguments"
s2l (Snode (Ssym "cdr") [e]) = Lfuncall (Lid "cdr") [s2l e]
s2l (Snode (Ssym "cdr") _) = error "Mauvais nombre d'arguments"
s2l (Snode (Ssym "ref!") [e]) = Lmkref (s2l e)
s2l (Snode (Ssym "ref!") _) = error "Mauvais nombre d'arguments"
s2l (Snode (Ssym "deref") [e]) = Lderef (s2l e)
s2l (Snode (Ssym "deref") _) = error "Mauvais nombre d'arguments"
s2l (Snode (Ssym "setref!") [e1, e2]) = Lassign (s2l e1) (s2l e2)
s2l (Snode (Ssym "setref!") _) = error "Mauvais nombre d'arguments"
s2l (Snode (Ssym "letrec") [Snode (Ssym x) [], e1, e2]) =
    Lrec [(x, Lfuncall (Lid "fix") [Labs x (s2l e1)])] (s2l e2)
s2l (Snode (Ssym "letrec") [Snode (Ssym x) xs, e1, e2]) =
    let vars = x : map (\(Ssym s) -> s) xs -- il y a un probleme ici
    in Lrec (map (, Lfuncall (Lid "fix") [Labs x (s2l e1)]) vars) (s2l e2)
s2l (Snode (Ssym "letrec") _) = error "Mauvais nombre d'arguments"
s2l (Snode (Ssym "fix") [Snode (Ssym x) [], e]) =
    Lrec [(x, Lfuncall (Lid "fix") [Labs x (s2l e)])] (Lid x)
s2l (Snode (Ssym "fix") [Snode (Ssym x) xs, e]) =
    let vars = x : map (\(Ssym s) -> s) xs -- l y a un probleme ici
    in Lrec (map (\x' -> (x', Lfuncall (Lid "fix") [Labs x (s2l e)])) vars) (Lid x) -- il y a une manière plus simple mais c pas dans le tp
s2l (Snode (Ssym "fix") _) = error "Mauvais nombre d'arguments"
s2l se = error ("Expression Slip inconnue: " ++ showSexp se)

---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

-- Représentation du "tas" ------------------------------------------------

-- Notre tas est représenté par une arbre binaire de type "trie".
-- La position du nœud qui contient l'info pour l'addresse `p` est
-- déterminée par la séquence de bits dans la représentation binaire de `p`.

data Heap = Hempty | Hnode (Maybe Value) Heap Heap

hlookup :: Heap -> Int -> Maybe Value
hlookup Hempty _ = Nothing
hlookup (Hnode mv _ _) 0 = mv
hlookup _ p | p < 0 = error "hlookup sur une adresse négative"
hlookup (Hnode _ e o) p = hlookup (if p `mod` 2 == 0 then e else o) (p `div` 2)

hinsert :: Heap -> Int -> Value -> Heap
hinsert _ p _ 
    | p < 0 = error "hinsert sur une adresse négative"
hinsert Hempty 0 v = Hnode (Just v) Hempty Hempty
hinsert Hempty p v = hinsert (Hnode Nothing Hempty Hempty) p v
hinsert (Hnode mv e o) p v
    | even p = Hnode mv (hinsert e (p `div` 2) v) o
    | otherwise = Hnode mv e (hinsert o (p `div` 2) v)



-- Représentation de l'environnement --------------------------------------

-- Type des tables indexées par des `α` et qui contiennent des `β`.
-- Il y a de bien meilleurs choix qu'une liste de paires, mais
-- ça suffit pour notre prototype.
type Map α β = [(α, β)]

-- Transforme une `Map` en une fonctions (qui est aussi une sorte de "Map").
mlookup :: Map Var β -> (Var -> β)
mlookup [] x = error ("Variable inconnue: " ++ show x)
mlookup ((x,v) : xs) x' = if x == x' then v else mlookup xs x'

madd :: Map Var β -> Var -> β -> Map Var β
madd m x v = (x,v) : m

-- On représente l'état de notre mémoire avec non seulement le "tas" mais aussi
-- avec un compteur d'objets de manière a pouvoir créer une "nouvelle" addresse
-- (pour `ref!`) simplement en incrémentant ce compteur.
type LState = (Heap, Int)

-- Type des valeurs manipulée à l'exécution.
data Value = Vnum Int
           | Vbool Bool
           | Vref Int
           | Vfun ((LState, Value) -> (LState, Value))

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vbool b) = showsPrec p b
    showsPrec _p (Vref p) = (\s -> "ptr<" ++ show p ++ ">" ++ s)
    showsPrec _ _ = showString "<function>"

type Env = Map Var Value

-- L'environnement initial qui contient les fonctions prédéfinies.

env0 :: Env
env0 = let binop :: (Value -> Value -> Value) -> Value
           binop op = Vfun (\ (s1, v1)
                            -> (s1, Vfun (\ (s2, v2)
                                         -> (s2, v1 `op` v2))))

           biniiv :: (Int -> Int -> Value) -> Value
           biniiv op = binop (\ v1 v2
                              -> case (v1, v2) of
                                  (Vnum x, Vnum y) -> x `op` y
                                  _ -> error ("Pas des entiers: "
                                             ++ show v1 ++ "," ++ show v2))

           binii wrap f = biniiv (\ x y -> wrap (f x y))

       in [("+", binii Vnum (+)),
           ("*", binii Vnum (*)),
           ("/", binii Vnum div),
           ("-", binii Vnum (-)),
           ("true",  Vbool True),
           ("false", Vbool False),
           ("<",  binii Vbool (<)),
           (">",  binii Vbool (>)),
           ("=",  binii Vbool (==)),
           (">=", binii Vbool (>=)),
           ("<=", binii Vbool (<=))]

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

state0 :: LState
state0 = (Hempty, 0)

eval :: LState -> Env -> Lexp -> (LState, Value)
eval s _env (Llit n) = (s, Vnum n)
-- ¡¡ COMPLETER !!
eval s env (Lid var) = (s, mlookup env var)
eval s env (Labs var e) = (s, Vfun (\(s', arg) -> eval s' (madd env var arg) e))
eval s env (Lfuncall e es) = 
    let (s1, Vfun f) = eval s env e
        evalArgs :: LState -> Env -> [Lexp] -> (LState, [Value])
        evalArgs s env [] = (s, [])
        evalArgs s env (e' : es') = (s, )
    



{-
data Value = Vnum Int
           | Vbool Bool
           | Vref Int
           | Vfun ((LState, Value) -> (LState, Value))

data Lexp = Llit Int             -- Litéral entier.
          | Lid Var              -- Référence à une variable.
          | Labs Var Lexp        -- Fonction anonyme prenant un argument.
          | Lfuncall Lexp [Lexp] -- Appel de fonction, avec arguments "curried".
          | Lmkref Lexp          -- Construire une "ref-cell".
          | Lderef Lexp          -- Chercher la valeur d'une "ref-cell".
          | Lassign Lexp Lexp    -- Changer la valeur d'une "ref-cell".
          | Lite Lexp Lexp Lexp  -- If/then/else.
          | Ldec Var Lexp Lexp   -- Déclaration locale non-récursive.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Lrec [(Var, Lexp)] Lexp
          deriving (Show, Eq)
-}

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp = snd . eval state0 env0 . s2l

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do s <- readFile filename
       (hPutStr stdout . show)
           (let sexps s' = case parse pSexps filename s' of
                             Left _ -> [Ssym "#<parse-error>"]
                             Right es -> es
            in map evalSexp (sexps s))

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

valOf :: String -> Value
valOf = evalSexp . sexpOf
