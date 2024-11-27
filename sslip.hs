-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
--
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
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          | Snode Sexp [Sexp]           -- Une liste non vide
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.

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
pSymchar    = alphaNum <|> satisfy (\c -> not (isAscii c)
                                          || c `elem` "!@$%^&*_+-=:|/?<>")
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

instance Show Sexp where
    showsPrec p = showSexp'


-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire Lexp                                     --
---------------------------------------------------------------------------

type Var = String

data Type = Terror String        -- Utilisé quand le type n'est pas connu.
          | Tnum                 -- Type des nombres entiers.
          | Tbool                -- Type des booléens.
          | Tfob [Type] Type     -- Type des fobjets.
          deriving  Eq

data Lexp = Lnum Int             -- Constante entière.
          | Lbool Bool           -- Constante Booléenne.
          | Lvar Var             -- Référence à une variable.
          | Ltype Lexp Type      -- Annotation de type.
          | Ltest Lexp Lexp Lexp -- Expression conditionelle.
          | Lfob [(Var, Type)] Lexp -- Construction de fobjet.
          | Lsend Lexp [Lexp]    -- Appel de fobjet.
          | Llet Var Lexp Lexp   -- Déclaration non-récursive.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Lfix [(Var, Lexp)] Lexp
          deriving (Show, Eq)

instance Show Type where
    show (Terror msg) = msg
    show Tnum = "Num"
    show Tbool = "Bool"
    show (Tfob paramTypes returnType) =
        let paramStr = if null paramTypes
                      then "-> "
                      else unwords (map show paramTypes) ++ " -> "
        in "(" ++ paramStr ++ show returnType ++ ")"

showVar :: Var -> String
showVar v = v

showLexp :: Lexp -> String
showLexp (Lnum n) = show n
showLexp (Lbool b) = show b
showLexp (Lvar v) = showVar v
showLexp (Ltype e t) = "(: " ++ showLexp e ++ " " ++ show t ++ ")"
showLexp (Ltest e1 e2 e3) = "(if " ++ showLexp e1 ++ " " ++ showLexp e2
                            ++ " " ++ showLexp e3 ++ ")"
showLexp (Lfob params body) = 
    let showParam = unwords ["(" ++ showVar v ++ " " ++ show t ++ ")" | (v, t) <- params]
    in "(fob (" ++ showParam ++ ") " ++ showLexp body ++ ")"
showLexp (Lsend f args) = "(" ++ showLexp f ++ " " ++ unwords (map showLexp args) ++ ")"
showLexp (Llet x e1 e2) = "(let " ++ showVar x ++ " " ++ showLexp e1 ++ " " ++ showLexp e2 ++ ")"
showLexp (Lfix bindings body) = 
    let showBinding (v, e) = "(" ++ showVar v ++ " " ++ showLexp e ++ ")"
    in "(fix (" ++ unwords (map showBinding bindings) ++ ") " ++ showLexp body ++ ")"
  

-- Transforme une variable Sexp en Var.
svar2lvar :: Sexp -> Var
svar2lvar (Ssym v) = v
svar2lvar se = error ("Pas un symbole: " ++ showSexp se)

-- Permet d'evluer le type d'une variable.
varType :: Sexp -> (Var, Type)
varType (Snode (Ssym v) [t]) = (v, evalType t)
varType se = error ("Pas un symbole: " ++ showSexp se)

-- Permet de transformer une des expressions imbriqué en liste.
s2list :: Sexp -> [Sexp]
s2list Snil = []
s2list (Snode se1 ses) = se1 : ses
s2list se = error ("Pas une liste: " ++ showSexp se)

-- Permet d'évaluer le type d'une expression.
evalType :: Sexp -> Type
evalType (Ssym "Num") = Tnum
evalType (Ssym "Bool") = Tbool
evalType (Snode t1 []) = evalType t1
-- Type d'une fonction sans arguments.
evalType (Snode (Ssym "->") [Ssym t]) = Tfob [] (evalType (Ssym t))
-- Type d'une fonction avec arguments.
evalType (Snode t1 rest) = 
    let t2 = last rest
        t3 = init (init rest)
    in Tfob (evalType t1 : map evalType t3) (evalType t2)
evalType otherType = Terror (show otherType)

-- Première passe simple qui analyse une Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym s) = Lvar s
s2l (Snode (Ssym ":") [e,t]) = Ltype (s2l e) (evalType t)
s2l (Snode (Ssym n) [Ssym t]) = Ltype (s2l (Ssym n)) (evalType (Ssym t))
s2l (Snode (Ssym "if") [e1, e2, e3])
  = Ltest (s2l e1) (s2l e2) (s2l e3)

s2l (Snode (Ssym "fob") [args, body])
    = Lfob (map varType (s2list args)) (s2l body)

s2l (Snode (Ssym "let") [x, e1, e2])
  = Llet (svar2lvar x) (s2l e1) (s2l e2)

s2l (Snode (Ssym "fix") (decls : body))
  = let sdecl2ldecl :: Sexp -> (Var, Lexp)
        -- Declaration de variable simple
        sdecl2ldecl (Snode (Ssym v) [e]) = (v, (s2l e))
        -- Declaration typée
        sdecl2ldecl (Snode (Ssym v) [t, e]) = (v, Ltype (s2l e) (evalType t))
        sdecl2ldecl (Snode (Snode (Ssym v) args) [])
          = error ("Pas de corps pour la déclaration: " ++ v)
        -- Declaration de fonction
        sdecl2ldecl (Snode (Snode (Ssym v) args) [e])
          = (v, Lfob (map varType args) (s2l e))
        -- Declaration complète
        sdecl2ldecl (Snode (Snode (Ssym v) args) [t, e])
          = (v, Lfob (map varType args) (Ltype (s2l e) (evalType t)))
        sdecl2ldecl se = error ("Declation Psil inconnue in fix: " ++ show se)

        {- isDeclComplete = if length body == 1 
          then Lfix (map sdecl2ldecl (s2list decls)) (s2l (head body))
          else Lfix (map sdecl2ldecl (s2list decls)) (Ltype (s2l (last body)) (evalType (head body))) -}
    in Lfix (map sdecl2ldecl (s2list decls)) (s2l (head body))

s2l (Snode f args)
  = Lsend (s2l f) (map s2l args)

s2l se = error ("Expression Psil inconnue: " ++ showSexp se)

---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

-- Type des valeurs manipulées à l'exécution.
data Value = Vnum Int
           | Vbool Bool
           | Vbuiltin ([Value] -> Value)
           | Vfob VEnv Int Dexp -- L'entier indique le nombre d'arguments.

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vbool b) = showsPrec p b
    showsPrec _ (Vbuiltin _) = showString "<primitive>"
    showsPrec _ (Vfob _ _ _) = showString "<fobjet>"

type Env = [(Var, Type, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: Env
env0 = let binop f op =
              Vbuiltin (\vs -> case vs of
                         [Vnum n1, Vnum n2] -> f (n1 `op` n2)
                         [_, _] -> error "Pas un nombre"
                         _ -> error "Nombre d'arguments incorrect")
           intbin = Tfob [Tnum, Tnum] Tnum
           boolbin = Tfob [Tnum, Tnum] Tbool

       in [("+", intbin,  binop Vnum (+)),
           ("*", intbin,  binop Vnum (*)),
           ("/", intbin,  binop Vnum div),
           ("-", intbin,  binop Vnum (-)),
           ("<", boolbin, binop Vbool (<)),
           (">", boolbin, binop Vbool (>)),
           ("<=", boolbin, binop Vbool (<=)),
           (">=", boolbin, binop Vbool (>=)),
           ("=", boolbin, binop Vbool (==)),
           ("true",  Tbool, Vbool True),
           ("false", Tbool, Vbool False)]

---------------------------------------------------------------------------
-- Vérification des types                                                --
---------------------------------------------------------------------------

type TEnv = [(Var, Type)]

-- `check c Γ e` renvoie le type de `e` dans l'environnement `Γ`.
-- Si `c` est vrai, on fait une vérification complète, alors que s'il
-- est faux, alors on présume que le code est typé correctement et on
-- se contente de chercher le type.
check :: Bool -> TEnv -> Lexp -> Type
check _ _ (Lnum _) = Tnum
check _ _ (Lbool _) = Tbool
check _ env (Lvar x) = case lookup x env of
                        Just t -> t
                        Nothing -> Terror ("Variable inconnue: " ++ x)

check strict env (Ltype e t) =
    let inferredType = check strict env e
    in if strict && inferredType /= t
      then Terror ("Type error: type attendu : " ++ show t 
                  ++ ", type actuel: " ++ show inferredType ++ 
                  ", dans l'expression: " ++ showLexp (Ltype e t))
      else t

check strict env (Ltest e1 e2 e3) = 
    let t1 = check strict env e1
        t2 = check strict env e2
        t3 = check strict env e3
    in if strict && t1 /= Tbool
      then Terror  ("Type error: Type attendu pour la condition : Bool, type actuel : " 
                  ++ show t1 ++ "dans l'expression: " ++ showLexp e1)
      else if strict && t2 /= t3
            then Terror ("Type error: Les branches ont des types differents : " ++ 
                  show t2 ++ " et " ++ show t3 ++
                  ", dans l'expression: " ++ showLexp (Ltest e1 e2 e3))
            else t2

check strict env (Lsend e0 args) =
    let funcType = if null args
                    then Tfob [] (check strict env e0) 
                    else check strict env e0
        argTypes = map (check strict env) args
    in case funcType of
        Tfob paramTypes returnType ->
            if length paramTypes /= length argTypes
            then Terror "Type error: Nombre incorrect d'arguments"
            else if strict && or (zipWith (/=) paramTypes argTypes)
                then Terror (show returnType)
                else returnType
        other -> Terror (show other)

check strict env (Lfob params body) =
    let extendedEnv = foldl (\acc (x, t) -> (x, t) : acc) env params
        bodyType = check strict extendedEnv body
        paramTypes = map snd params
    in if bodyType == Terror "Type inconnu"
      then Terror "Type error: Erreur dans le corps du fobjet"
      else Tfob paramTypes bodyType

check strict env (Llet x e1 e2) =
    let t1 = check strict env e1
        extendedEnv = (x, t1) : env
        t2 = check strict extendedEnv e2
    in t2

check strict env (Lfix bindings body) =
    let -- Étape 1 : Deviner les types des `ei` en mode non-strict
        paramNames = map fst bindings
        initialEnv = [(x, Terror "Type inconnu") | x <- paramNames] ++ env
        guessedTypes = map (check False initialEnv . snd) bindings
        
        -- Étape 2 : Construire Γ′ avec les types devinés
        extendedEnv = zip paramNames guessedTypes ++ env
        
        -- Étape 3 : Vérifier les `ei` en mode strict
        checkedTypes = map (check strict extendedEnv . snd) bindings

        differentType = [x | x <- guessedTypes, x `notElem` checkedTypes]
        
    in if strict && or (zipWith (/=) guessedTypes checkedTypes)
      then let
            -- Convertit les déclaration en chaîne de caractères 
            showBinding (v, e) = "(" ++ showVar v ++ " " ++ showLexp e ++ ")"
            -- Convertit les types en chaîne de caractères
            showType t = unwords (map show t)
        in Terror ("Type error: types incoherents '" ++ 
        showType differentType ++ "' dans l'expression: " ++ "(" ++
        unwords (map showBinding bindings) ++ ")" )
      else check strict extendedEnv body

---------------------------------------------------------------------------
-- Pré-évaluation
---------------------------------------------------------------------------

-- Dexp simplifie le code en éliminant deux aspects qui ne sont plus
-- utiles lors de l'évaluation:
-- - Les annotations de types.
-- - Les noms de variables, remplacés par des entiers qui représentent
--   la position de la variable dans l'environnement.  On appelle ces entiers
--   des [indexes de De Bruijn](https://en.wikipedia.org/wiki/De_Bruijn_index).

type VarIndex = Int

data Dexp = Dnum Int             -- Constante entière.
          | Dbool Bool           -- Constante Booléenne.
          | Dvar VarIndex        -- Référence à une variable.
          | Dtest Dexp Dexp Dexp -- Expression conditionelle.
          | Dfob Int Dexp        -- Construction de fobjet de N arguments.
          | Dsend Dexp [Dexp]    -- Appel de fobjet.
          | Dlet Dexp Dexp       -- Déclaration non-récursive.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Dfix [Dexp] Dexp
          deriving (Show, Eq)

-- Renvoie le "De Buijn index" de la variable, i.e. sa position
-- dans le contexte.
lookupDI :: TEnv -> Var -> Int -> Int
lookupDI ((x1, _) : xs) x2 n = if x1 == x2 then n else lookupDI xs x2 (n + 1)
lookupDI _ x _ = error ("Variable inconnue: " ++ show x)

-- Conversion de Lexp en Dexp.
-- Les types contenus dans le "TEnv" ne sont en fait pas utilisés.
l2d :: TEnv -> Lexp -> Dexp
l2d _ (Lnum n) = Dnum n
l2d _ (Lbool b) = Dbool b
l2d tenv (Lvar v) = Dvar (lookupDI tenv v 0)
l2d tenv (Ltype e _) = l2d tenv e
l2d tenv (Ltest e1 e2 e3) = Dtest (l2d tenv e1) (l2d tenv e2) (l2d tenv e3)
l2d tenv (Lfob params body) = 
    -- Ajout des paramètres de la fonction à l'environnement
    let newEnv = [(x, t) | (x, t) <- params] ++ tenv
    -- Convertir le corps avec le nouvel environnement
    in Dfob (length params) (l2d newEnv body) 
l2d tenv (Llet var e1 e2) = 
    let newEnv = (var, Terror "Type inconnu") : tenv
    in Dlet (l2d tenv e1) (l2d newEnv e2)
l2d tenv (Lsend f args) = Dsend (l2d tenv f) (map (l2d tenv) args)
l2d tenv (Lfix bindings body) = 
    let paramNames = map fst bindings
        newEnv = [(x, Terror "Type inconnu") | x <- paramNames] ++ tenv
    in Dfix (map (l2d newEnv . snd) bindings) (l2d newEnv body)
    
l2d _ l = error (show l)

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------
lookupVE :: VEnv -> Int -> Value
lookupVE env n = 
  if n < 0 || n >= length env
  then error ("l'index de la variable n'est pas dans l'environnement : " 
              ++ show n)
  else env !! n

type VEnv = [Value]

eval :: VEnv -> Dexp -> Value
eval _   (Dnum n) = Vnum n
eval _   (Dbool b) = Vbool b
eval env (Dvar var) = lookupVE env var
eval env (Dtest e1 e2 e3) = 
    case eval env e1 of
      Vbool True -> eval env e2
      Vbool False -> eval env e3
      _ -> error "Condition non booléenne"
eval env (Dfob n body) = Vfob env n body
eval env (Dsend f actuals) = 
  let fv = eval env f  -- Évaluation de la fonction
      actualsv = map (eval env) actuals  -- Évaluation des arguments
  in case fv of
      -- Si c'est une fonction prédéfinie, on l'applique
      Vbuiltin bi -> bi actualsv  
      Vfob fEnv n body -> 
        -- Vérifie que le nombre d'arguments est correct
        if length actualsv == n then
          -- Applique la fonction avec l'environnement étendu  
          eval (actualsv ++ fEnv) body  
        else
          error ("Nombre d'arguments incorrect pour la fonction")
      -- Si ce n'est ni une fonction prédéfinie ni un fobjet, on lève une erreur
      v -> error ("Pas une fonction: " ++ show v)  
eval env (Dlet e1 e2) = 
  -- Évalue la première expression pour obtenir la valeur associée à la variable
  let val1 = eval env e1
  -- Ajoute la nouvelle variable liée à la valeur dans l'environnement  
      env' = val1 : env
  -- Évalue le corps `e2` avec l'environnement mis à jour  
  in eval env' e2         

eval env (Dfix decls body) = 
  let   
        newEnv = map (eval newEnv) decls ++ env
    -- Evalue le corps de la fonction avec le nouvel environnement
    in eval newEnv body  

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

tenv0 :: TEnv
tenv0 = map (\(x,t,_v) -> (x,t)) env0

venv0 :: VEnv
venv0 = map (\(_x,_t,v) -> v) env0

-- Évaluation sans vérification de types.
evalSexp :: Sexp -> Value
evalSexp = eval venv0 . l2d tenv0 . s2l

checkSexp :: Sexp -> Type
checkSexp = check True tenv0 . s2l

tevalSexp :: Sexp -> Either (Type, Value) String
tevalSexp se = let le = s2l se
               in case check True tenv0 le of
                    Terror err -> Right err
                    t -> Left (t, eval venv0 (l2d tenv0 le))

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do inputHandle <- openFile filename ReadMode
       hSetEncoding inputHandle utf8
       s <- hGetContents inputHandle
       (hPutStr stdout . show)
           (let sexps s' = case parse pSexps filename s' of
                             Left _ -> [Ssym "#<parse-error>"]
                             Right es -> es
            in map tevalSexp (sexps s))
       hClose inputHandle

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

valOf :: String -> Value
valOf = evalSexp . sexpOf
