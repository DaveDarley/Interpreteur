-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}

-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Bibliothèque d'analyse syntaxique.
import Data.Char                -- Conversion de Chars de/vers Int et autres.
import System.IO                -- Pour stdout, hPutStr

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3)  ==  (((() . +) . 2) . 3)
--          ==>  Scons (Scons (Scons Snil (Ssym "+"))
--                            (Snum 2))
--                     (Snum 3)
--
-- (/ (* (- 68 32) 5) 9)
--     ==  (((() . /) . (((() . *) . (((() . -) . 68) . 32)) . 5)) . 9)
--     ==>
-- Scons (Scons (Scons Snil (Ssym "/"))
--              (Scons (Scons (Scons Snil (Ssym "*"))
--                            (Scons (Scons (Scons Snil (Ssym "-"))
--                                          (Snum 68))
--                                   (Snum 32)))
--                     (Snum 5)))
--       (Snum 9)

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                pChar '\n'; return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment); return () }

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

-- La notation "'E" est équivalente à "(shorthand-quote E)"
-- La notation "`E" est équivalente à "(shorthand-backquote E)"
-- La notation ",E" est équivalente à "(shorthand-comma E)"
pQuote :: Parser Sexp
pQuote = do { c <- satisfy (\c -> c `elem` "'`,"); pSpaces; e <- pSexp;
              return (Scons
                      (Scons Snil
                             (Ssym (case c of
                                     ',' -> "shorthand-comma"
                                     '`' -> "shorthand-backquote"
                                     _   -> "shorthand-quote")))
                      e) }

-- Une liste (Tsil) est de la forme ( [e .] {e} )
pTsil :: Parser Sexp
pTsil = do _ <- char '('
           pSpaces
           (do { _ <- char ')'; return Snil }
            <|> do hd <- (do e <- pSexp
                             pSpaces
                             (do _ <- char '.'
                                 pSpaces
                                 return e
                              <|> return (Scons Snil e)))
                   pLiat hd)
    where pLiat :: Sexp -> Parser Sexp
          pLiat hd = do _ <- char ')'
                        return hd
                 <|> do e <- pSexp
                        pSpaces
                        pLiat (Scons hd e)

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pTsil <|> pQuote <|> pSymbol
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
    readsPrec _ s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                        --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) = showHead (Scons e1 e2) . showString ")"
    where showHead (Scons Snil e') = showString "(" . showSexp' e'
          showHead (Scons e1' e2')
            = showHead e1' . showString " " . showSexp' e2'
          showHead e = showString "(" . showSexp' e . showString " ."

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire                                          --
---------------------------------------------------------------------------

type Var = String

data Ltype = Lint               -- Int
           | Lboo               -- Bool
           | Larw Ltype Ltype   -- τ₁ → τ₂
           | Ltup [Ltype]       -- tuple τ₁...τₙ
           deriving (Show, Eq)

data Lexp = Lnum Int                    -- Constante entière.
          | Lvar Var                    -- Référence à une variable.
          | Lhastype Lexp Ltype         -- Annotation de type.
          | Lcall Lexp Lexp             -- Appel de fonction, avec un argument.
          | Lfun Var Lexp               -- Fonction anonyme prenant un argument.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Llet [(Var, Lexp)] Lexp
          | Lif Lexp Lexp Lexp          -- Expression conditionelle.
          | Ltuple [Lexp]               -- Construction de tuple
          | Lfetch Lexp [Var] Lexp      -- lecture d'un tuple
          deriving (Show, Eq)


---------------------------------------------------------------------------
-- Conversion de Sexp à Lexp                                             --
---------------------------------------------------------------------------

sexp2list :: Sexp -> [Sexp]
sexp2list s = loop s []
    where
      loop (Scons hds tl) acc = loop hds (tl : acc)
      loop Snil acc = acc
      loop _ _ = error ("Improper list: " ++ show s)

-- Analyse une Sexp et construit une Lexp équivalente.

-- reste  let avec d::= declaration variable avec son type
-- mais aussi fonction avec son type

s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym s) = Lvar s
s2l (se@(Scons _ _)) = case sexp2list se of
    (Ssym "hastype" : e : t : []) -> Lhastype (s2l e) (s2t t)
    (Ssym "call" : e0) -> aideCall e0
    (Ssym "fun" : e0) -> aideFun e0
    (Ssym "if" : e1 : e2 : e3 : []) -> Lif (s2l e1) (s2l e2) (s2l e3)
    (Ssym "tuple" : e1) -> Ltuple (aideTuple e1)
    (Ssym "let" : e1 ) ->  aideLet e1
    (Ssym "fetch" : e1 : e2 : e3 : []) -> Lfetch (s2l e1) (aideFetch e2) (s2l e3)



-- ¡¡ COMPLÉTER !!
    _ -> error ("Unrecognized Psil expression: " ++ (showSexp se))
s2l se = error ("Unrecognized Psil expression: " ++ (showSexp se))




aide3Let :: [Sexp] -> [String]
aide3Let (x:y:[]) = [showSexp y]
aide3Let (x:xs) = case x of
        (Scons a b) -> case a of
                        (Scons d c) -> let newTab = aidelet4 [showSexp c] "v"
                                       in newTab ++ aide3Let xs

        _ -> aide3Let xs






aideLet :: [Sexp] -> Lexp
aideLet (x:xs) = case x of
  (Scons a b) -> case a of
    (Scons Snil c) -> case (s2l c) of
          Lvar d   ->  aideeLet xs [(d,(s2l b))]
          _ -> error ("L'argument x de l'exp (x e) doit etre un string")


    (Scons k l)   -> case k of

          (Scons Snil e) -> case (s2l e) of
                Lvar f -> aideeLet xs [(f,(Lhastype (s2l b) (s2t l)))]
                _ -> error ("L'argument x de l'exp (x t e) doit etre un string")


          _           -> let newTab = sexp2list (x)
                         in case newTab of
                                 (g:h) -> case (s2l g) of

                                   Lvar i -> let typ = sexpOf( concat( ["("] ++ (aide5Let h) ++ [")"]) )
                                                 fun = sexpOf( concat(["(fun "] ++ (aide3Let h) ++ [")"])  )
                                             in aideeLet xs [(i,(Lhastype (s2l fun) (s2t typ)))]

                                   _ -> error ("Dans L'espression Llet [(a,b)] c, a doit etre un String et rien d'autre")










-- recuperation des types dans les declarations de la forme :
-- (x (x1 t1) (x2 t2) t e)

aide5Let :: [Sexp] -> [String]
aide5Let (t:e:[]) = [showSexp t]
aide5Let (x:xs)   = case x of
        (Scons a b) -> (aidelet4 [showSexp b] "f" ) ++ (aide5Let xs)
        _ -> aide5Let xs



-- Fonction qui permet de reecrire un let de la forme :
-- (let  (x (x1 τ1) ... (xn τn) τ e) e2)
-- appeler cette fonction avec "f" considere qu'on veut recuperer le type
-- appeler cette fonction avec "v" considere qu'on veut recuperer les arguments de la function e

aidelet4 :: [[Char]] -> [Char] -> [[Char]]
aidelet4 (x:[]) a = case a of
                         "f" -> [x] ++ [" -> "]
                         "v" -> [x] ++ [" "]


aideeLet :: [Sexp] -> [(Var,Lexp)] -> Lexp
aideeLet (x:[]) v = Llet v (s2l x)
aideeLet (x:xs) v = case x of
    (Scons a b) -> case a of
      (Scons Snil c) -> case (s2l c) of
              Lvar d   ->  aideeLet xs (v++[(d,(s2l b))])
              _ -> error ("L'argument x de l'exp (x e) doit etre un string")


      (Scons k l)   -> case k of
            (Scons Snil e) -> case (s2l e) of
                    Lvar f -> aideeLet xs (v++[(f,(Lhastype (s2l b) (s2t l)))])
                    _ -> error ("L'argument x de l'exp (x t e) doit etre un string")

            _           -> let newTab = sexp2list (x)
                           in case newTab of
                                   (g:h) -> case (s2l g) of
                                             Lvar i -> let typ = sexpOf( concat( ["("] ++ (aide5Let h) ++ [")"]) )
                                                           fun = sexpOf( concat(["(fun "] ++ (aide3Let h) ++ [")"])  )
                                                       in aideeLet xs (v++[(i,(Lhastype (s2l fun) (s2t typ)))])

                                             _ -> error ("Dans L'espression Llet [(a,b)] c, a doit etre un String et rien d'autre")





-- Transforme un tuple en un tableau de Lexp
-- en transformant chaque element de ce tuple en Lexp

aideTuple :: [Sexp] -> [Lexp]
aideTuple (x:[]) = [s2l x]
aideTuple (x:xs) = [s2l x] ++ aideTuple xs



aideFetch :: Sexp -> [Var]
aideFetch s = let newTab = sexp2list (s)
               in fromSexp2Var newTab

fromSexp2Var :: [Sexp] -> [Var]
fromSexp2Var (x:[]) = case (s2l x) of
                            Lvar y -> [y]
fromSexp2Var (x:xs) = case (s2l x) of
                            Lvar y -> [y] ++ fromSexp2Var xs


aideCall :: [Sexp] -> Lexp
aideCall (x:y:[]) = Lcall (s2l x) (s2l y)
aideCall (x:y:xs) = aideCall2 xs (Lcall (s2l x) (s2l y))

aideCall2 :: [Sexp] -> Lexp -> Lexp
aideCall2 (x:[]) s = Lcall s (s2l x)
aideCall2 (x:xs) s = aideCall2 xs (Lcall s (s2l x))


aideFun :: [Sexp] -> Lexp
aideFun (x:[]) = error ("Manque lexp1 dans (fun var lexp1)")
aideFun (x:y:[]) = case (s2l x) of
        (Lvar c) -> Lfun c (s2l y)
        _ -> error ("Le premier argument du Lfun doit etre un string")

aideFun (x:xs) = let newtab = reverse (x:xs)
                 in case newtab of
                 (x:y:ys) -> case (s2l y) of
                    Lvar a -> aideFun2 ys (Lfun a (s2l x))
                    _ -> error ("Le premier argument du Lfun doit etre un string")


aideFun2 :: [Sexp] -> Lexp -> Lexp
aideFun2 (x:[]) lex = case (s2l x) of
                    Lvar a -> Lfun a lex
                    _ -> error ("Mauvais type premier argument du (fun a ..)")

aideFun2 (x:xs) lex = case (s2l x) of
                     Lvar a -> aideFun2 xs (Lfun a lex)
                     _ -> error ("Mauvais type premier argument du (fun a ..)")




recupTypes :: [Sexp] -> [Ltype]
recupTypes (x:[]) = [s2t x]
recupTypes (x:xs) = [s2t x] ++ recupTypes xs

aideTypeFun :: [Sexp] ->Ltype
aideTypeFun (x:[]) = s2t x
aideTypeFun (Ssym "->":xs) = aideTypeFun xs
aideTypeFun (x:xs) = Larw (s2t x) (aideTypeFun xs)


s2t :: Sexp -> Ltype
s2t (Ssym "Int") = Lint
s2t (Ssym "Bool") = Lboo
s2t (se@(Scons _ _)) = case sexp2list se of
    (Ssym "Tuple" : e ) -> Ltup (recupTypes e)
    (x:xs) -> aideTypeFun (x:xs)

-- ¡¡ COMPLÉTER !!
s2t s = error ("Unrecognized Psil type: " ++ (showSexp s))

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

-- Type des valeurs renvoyées par l'évaluateur.
data Value = Vnum Int
           | Vbool Bool
           | Vtuple [Value]
           | Vfun (Maybe String) (Value -> Value)

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vbool b) = showsPrec p b
    showsPrec p (Vtuple vs) = showValues "[" vs
        where showValues _ [] = showString "]"
              showValues sep (v:vs')
                = showString sep . showsPrec p v . showValues " " vs'
    showsPrec _ (Vfun (Just n) _)
      = showString "<fun " . showString n . showString ">"
    showsPrec _ (Vfun Nothing _) = showString "<fun>"

type Env = [(Var, Value, Ltype)]

-- L'environnement initial qui contient les fonctions prédéfinies et leur type.
env0 :: Env
env0 = [prim "+"  (+) Vnum  Lint,
        prim "-"  (-) Vnum  Lint,
        prim "*"  (*) Vnum  Lint,
        prim "/"  div Vnum  Lint,
        prim "="  (==) Vbool Lboo,
        prim ">=" (>=) Vbool Lboo,
        prim "<=" (<=) Vbool Lboo]
       where prim name op cons typ
               = (name,
                  Vfun (Just name)
                       (\ (Vnum x) -> Vfun Nothing
                                          (\ (Vnum y) -> cons (x `op` y))),
                  Larw Lint (Larw Lint typ))

-- Point d'entrée de l'évaluation
eval :: Env -> Lexp -> Value
eval env e
  -- Extrait la liste des variables et la liste de leur valeurs,
  -- et ignore leurs types, qui n'est plus utile pendant l'évaluation.
  = eval2 (map (\(x,_,_) -> x) env) e (map (\(_,v,_) -> v) env)

e2lookup :: [Var] -> Var -> Int          -- Find position within environment
e2lookup env x = e2lookup' env 0
    where e2lookup' :: [Var] -> Int -> Int
          e2lookup' [] _ = error ("Variable inconnue: " ++ show x)
          e2lookup' (x':_) i | x == x' = i
          e2lookup' (_:xs) i = e2lookup' xs (i+1)

-------------- La fonction d'évaluation principale.  ------------------------
-- Au lieu de recevoir une liste de paires (Var, Val), on passe la liste
-- des noms de variables (`senv`) et la liste des valeurs correspondantes
-- (`venv`) séparément de manière à ce que (eval2 senv e) renvoie une
-- fonction qui a déjà fini d'utiliser `senv`.
eval2 :: [Var] -> Lexp -> ([Value] -> Value)
eval2 _    (Lnum n)= \_ -> Vnum n
eval2 senv (Lhastype e _) = eval2 senv e

eval2 senv (Lvar x)
  -- Calcule la position que la variable aura dans `venv`.
  = let i = e2lookup senv x
    -- Renvoie une fonction qui n'a plus besoin de charcher et comparer le nom.
    -- De cette manière, si la fonction renvoyée par (eval2 senv v) est appelée
    -- plusieurs fois, on aura fait la recherche dans `senv` une seule fois.
    in \venv -> venv !! i

eval2 senv (Lif exp a b) = \venv ->case (eval2 senv exp venv) of
                                     Vbool d -> case d of
                                            True ->  eval2 senv a venv
                                            False -> eval2 senv b venv

eval2 senv (Ltuple (x:xs)) = \venv -> Vtuple (aideLtup senv (x:xs) venv)

-- ancienne methode evaluation "Llet tab exp"
--eval2 senv (Llet (x:xs) lexp)  = \venv ->  (aidelet2  (x:xs) lexp senv venv)

-- Nouvelle methode evaluation Llet
eval2 senv (Llet (x:xs) lexp)  = \venv ->  let newVar = aideLetVar (x:xs) senv
                                           in  let newValue = aideLetValue (x:xs) newVar venv newValue
                                               in eval2  newVar lexp newValue


eval2 senv (Lfetch a b c) = \venv -> case b of
          (x:[]) -> let monVar = x
                        maValeur = eval2 senv a venv
                    in case maValeur of
                         Vtuple (x:xs) -> let newTab = [monVar] ++ senv
                                              newTab2 = [x] ++ venv
                                          in eval2 newTab c newTab2
                         _ -> let newTab = [monVar] ++ senv
                                  newTab2 = [maValeur] ++ venv
                              in eval2 newTab c newTab2


          (x:xs) -> case (eval2 senv a venv) of
                        Vtuple tab -> aideLfetch senv venv b  tab c
                        _ -> error (" Nombre de variables correspond pas au nombre de valeurs ")

eval2 senv (Lcall e1 e2) = \venv -> case eval2 senv e1 venv of
                                       Vfun _ f -> let arg = eval2 senv e2 venv
                                                   in f arg
                                       _ -> error ("Not a function")

eval2 senv (Lfun var lexp) = \venv -> Vfun Nothing (\arg -> eval2 (var:senv) lexp (arg:venv) )


aideLetVar :: [(Var,Lexp)] -> [Var] -> [Var]
aideLetVar ((x,y):[]) env =  [x] ++ env
aideLetVar ((x,y):xs) env = aideLetVar xs ([x] ++ env)

aideLetValue :: [(Var,Lexp)] -> [Var] -> [Value]  -> [Value] -> [Value]
aideLetValue ((x,y):[]) env  venv newValue =  [eval2 env y newValue] ++ venv

aideLetValue ((x,y):xs) env  venv newValue  = let newValue2 = [eval2 env y newValue] ++ venv
                                              in aideLetValue xs env newValue2 newValue

{-
-- ancienne methode pour "LLet tab exp"

aidelet2 :: [(Var,Lexp)] -> Lexp -> [Var] -> [Value] -> Value
aidelet2 ((x,y):[]) lexp env venv =  let newTab = (x:env)
                                         maValeur = eval2 env y venv
                                     in  let newTab2 = [maValeur] ++ venv
                                         in eval2 newTab lexp newTab2


aidelet2 ((x,y):xs) lexp env venv  =   let newTab = (x:env)
                                           maValeur = eval2 env y venv
                                       in  let newTab2 = [maValeur] ++ venv
                                           in aidelet2 xs lexp newTab newTab2

-}


aideLfetch :: [Var] -> [Value] -> [Var] -> [Value] -> Lexp -> Value
aideLfetch senv venv (x:[]) (y:[]) toExecute =  let monVar = x
                                                    maValeur = y
                                                in  let newTab = [monVar] ++ senv
                                                        newTab2 = [maValeur] ++ venv
                                                    in eval2 newTab toExecute newTab2

aideLfetch senv venv (x:xs) (y:ys) toExecute =  let monVar = x
                                                    maValeur = y
                                                in  let newTab = [monVar] ++ senv
                                                        newTab2 = [maValeur] ++ venv
                                                    in aideLfetch newTab newTab2 xs ys toExecute



aideLtup :: [Var] -> [Lexp] -> [Value] ->[Value]
aideLtup senv (x:[]) venv = [eval2 senv x venv]
aideLtup senv (x:xs) venv = [eval2 senv x venv] ++ aideLtup senv xs venv



-- ¡¡¡ COMPLETER ICI !!! --

---------------------------------------------------------------------------
-- Vérificateur de types                                                 --
---------------------------------------------------------------------------

aidecheckLfetch :: [Ltype] -> [Var] -> TEnv -> TEnv
aidecheckLfetch (x:[]) (y:[]) tenv = ((y,x):tenv)
aidecheckLfetch (x:xs) (y:ys) tenv = aidecheckLfetch xs ys ((y,x):tenv)

type TEnv = [(Var, Ltype)]
type TypeError = String

-- Les valeurs ne servent à rien pendant la vérification de type,
-- donc extrait la partie utile de `env0`.
tenv0 :: TEnv
tenv0 = (map (\(x,_,t) -> (x,t)) env0)

tlookup :: [(Var, a)] -> Var -> a
tlookup [] x = error ("Variable inconnue: " ++ x)
tlookup ((x',t):_) x | x == x' = t
tlookup (_:env) x = tlookup env x

infer :: TEnv -> Lexp -> Ltype
infer _ (Lnum _) = Lint
infer tenv (Lvar x) = tlookup tenv x
infer _ (Lfun _ _)     = error "Can't infer type of `fun`"
infer _ (Lfetch _ _ _) = error "Can't infer type of `fetch`"
infer _ (Lif _ _ _)    = error "Can't infer type of `if`"

infer tenv (Ltuple (x:xs)) = Ltup (aideInferTuple tenv (x:xs) [])

infer tenv (Lhastype a b ) = case (check tenv a b) of
            Nothing -> b
            _ -> error ("L'expression que vs essayez d'inferer est invalide")

infer tenv (Llet (x:xs) lexp) = infer (aideInferlet (x:xs) tenv)  lexp

infer tenv (Lcall lexp1 lexp2) = case (infer tenv lexp1) of
           (Larw t1 t2) -> case (check tenv lexp2 t1) of
                              Nothing -> t2
                              _ -> error ("Vs essayer d'inferer un code invalide")

aideInferlet :: [(Var,Lexp)] -> TEnv -> TEnv
aideInferlet ((x,y):[]) tenv = ((x,infer tenv y):tenv)
aideInferlet ((x,y):xs) tenv = ((x,infer tenv y):(aideInferlet xs tenv))

aideInferTuple :: TEnv -> [Lexp] -> [Ltype] -> [Ltype]
aideInferTuple tenv (x:[]) tabType = let inferX = infer tenv x
                                     in tabType ++ [inferX]
aideInferTuple tenv (x:xs) tabType = let inferX = infer tenv x
                                     in aideInferTuple tenv xs (tabType ++ [inferX])

-- ¡¡¡ COMPLETER ICI !!! --


check :: TEnv -> Lexp -> Ltype -> Maybe TypeError
check tenv (Lfun x body) (Larw t1 t2) = check ((x,t1):tenv) body t2
check _ (Lfun _ _) t = Just ("Expected a function type: " ++ show t)

check tenv (Lfetch valeur variables exp) t = case (infer tenv valeur) of
            Ltup tabTypTuple -> let newTenv = aidecheckLfetch tabTypTuple variables tenv
                                in check newTenv exp t

check tenv (Lif exp rep1 rep2) t = case (check tenv exp Lboo) of
           Nothing -> case (check tenv rep1 t) of
                        Nothing -> (check tenv rep2 t)

           _ -> error ("Lif recoit un booleen comme 1er parametre")



-- ¡¡¡ COMPLETER ICI !!! --
check tenv e t
  -- Essaie d'inférer le type et vérifie alors s'il correspond au
  -- type attendu.
  = let t' = infer tenv e
    in if t == t' then Nothing
       else Just ("Type mismatch: " ++ show t ++ " != " ++ show t')













---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename
  = do filestring <- readFile filename
       (hPutStr stdout)
           (let sexps s = case parse pSexps filename s of
                            Left _ -> [Ssym "#<parse-error>"]
                            Right es -> es
            in (concat
                (map (\ sexp -> let { ltyp = infer tenv0 lexp
                                   ; lexp = s2l sexp
                                   ; val = eval env0 lexp }
                               in "  " ++ show val
                                  ++ " : " ++ show ltyp ++ "\n")
                     (sexps filestring))))

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

typeOf :: String -> Ltype
typeOf = infer tenv0 . lexpOf

valOf :: String -> Value
valOf = eval env0 . lexpOf
