{-# LANGUAGE RecordWildCards #-}    -- per utilitzar fields

import Data.Char (isUpper)
import Data.List (nub, isInfixOf)
import Data.List.Split (splitOn)    -- cal instal·lar: cabal install split
import Data.String.Utils (strip)    -- cal instal·lar: cabal install MissingH
import Data.Maybe (mapMaybe, fromMaybe)

type Programa = [ Regla ]

data Regla = Regla { _cap::Atom, _cos::[ Atom ] } deriving (Eq, Show)

data Atom = Atom { _nomPredicat::String, _termes::[ Term ] }
    deriving (Eq, Show)

data Term = Var String | Sym String
   deriving (Eq, Show)

type Sustitucio = [ (Term, Term) ]     -- [(variable, constant), (variable, constant), ...]

type BaseConeixement = [ Atom ]

-- INSTRUCCIONES PARA PODER EJECUTAR EL CÓDIGO
-- Para compilar: ghc -o Pallus Pallus.hs
-- Para ejecutar: ./Pallus

sustitucioBuida :: Sustitucio
sustitucioBuida = []

consequencia :: Programa -> BaseConeixement -> BaseConeixement
consequencia [] _ = []
consequencia (x:xs) bc =  avaluaRegla (bc) (x)++ consequencia (xs) (bc)

avaluaRegla :: BaseConeixement -> Regla -> BaseConeixement
avaluaRegla bc (Regla {_cap = atom, _cos = []}) = [atom]
avaluaRegla [] rule = []
avaluaRegla bc rule = map f (removeDuplicates (filter (not.null)(getList (tail list) (head list))))
  where
    list = listSustitution (bc) (_cos rule)
    removeDuplicates :: Eq a => [a] -> [a]
    removeDuplicates [] = []
    removeDuplicates (x:xs) = x:removeDuplicates (filter (/=x) xs)
    f = sustitueix (_cap rule)

getValue :: Maybe a -> a
getValue (Just x) = x

sustitueix :: Atom -> Sustitucio -> Atom
sustitueix atom sus = Atom {_nomPredicat = _nomPredicat atom, _termes = sustitueix'  (_termes atom) (sus)}
  where
    sustitueix' :: [Term] -> Sustitucio -> [Term]
    sustitueix'  [] _  = []
    sustitueix'  termes sus  = getValue(find):sustitueix' (tail termes) (sus)
      where
        find = lookup (head termes) (sus)


getList :: [[Sustitucio]]->[Sustitucio] ->[Sustitucio]
getList [] list = list
getList (y:ys) x = list ++ getList (ys) (list)
    where
      list = comprovaSustitucions (x) (y)
      comprovaSustitucions :: [Sustitucio] -> [Sustitucio] -> [Sustitucio]
      comprovaSustitucions [] _ = []
      comprovaSustitucions (x:xs) list = (map  f (list)) ++ comprovaSustitucions (xs) (list)
        where
          comprovaConstants :: Sustitucio -> Sustitucio -> Sustitucio
          comprovaConstants ([]) sus = sus
          comprovaConstants (x:xs) sus
            | elem (x) (sus) = comprovaConstants (xs) (sus)
            | find == Nothing = comprovaConstants (xs) (x:sus)
            | getValue(find) == snd x = comprovaConstants (xs) (sus)
            | otherwise = []
            where find = lookup (fst x) (sus)
          f = comprovaConstants (x)

listSustitution :: BaseConeixement -> [Atom] -> [[Sustitucio]]
listSustitution bc [x]  = [listSustitution' (bc) (x)]
listSustitution bc (x:xs)
  | sus /= [] = listSustitution' (bc) (x) : sus
  | otherwise = []
  where
    sus = listSustitution (bc) (xs)

listSustitution' :: BaseConeixement -> Atom -> [Sustitucio]
listSustitution' [] _ = []
listSustitution' (x:xs) atom
  | sus == Nothing = listSustitution' (xs) (atom)
  | otherwise = getValue (sus): listSustitution' (xs) (atom)
  where sus = unifica (x) (atom)

unifica :: Atom -> Atom -> Maybe Sustitucio
unifica atom1 atom2
  | _nomPredicat atom1 /= _nomPredicat atom2 = Nothing
  | (_termes atom1) == (_termes atom2) = Just sustitucioBuida
  | otherwise = unifica' (_termes atom1) (_termes atom2) ([])
  where
    unifica' :: [Term] -> [Term]-> Sustitucio -> Maybe Sustitucio
    unifica' [] _ sus = Just sus
    unifica' (x:xs) (y:ys) sus
      | x == y = unifica' (xs) (ys) (sus)
      | isSym(x) && isSym(y) = Nothing
      | null (sus) = unifica' (xs) (ys) (sus++[(y,x)])
      | lookup (x) (sus) == Nothing = unifica' (xs) (ys) (sus++[(y,x)])
      | elem (y,x) (sus) = unifica' (xs) (ys) (sus)
      | otherwise = Nothing
      where
        isSym :: Term -> Bool
        isSym (Var a) = False
        isSym (Sym a) = True

-- Devuelve la lista de sustituciones encontradas para una query
avaluaAtom :: [[Term]] -> [Term]-> [Sustitucio]
avaluaAtom bc query = map f (bc)
  where
    f = avaluaAtom' (query)
    avaluaAtom' ::  [Term] -> [Term] -> Sustitucio
    avaluaAtom' [] _ = []
    avaluaAtom' (x:xs) (y:ys) = (x,y):avaluaAtom' (xs) (ys)

getAnswers :: Programa -> Programa -> [String]
getAnswers programa queries = map f (queries)
  where
    f = answerQuery (programa)
    answerQuery :: Programa -> Regla -> String
    answerQuery programa query
      | isFact(query) = avaluaQuery (inferir (programa) ([])) (head (_cos query))
      | otherwise = show (avaluaAtom (constants) (_termes(_cap query)))
      where
        bc = inferir (programa++[query]) ([])
        constants = [_termes x | x <- bc, _nomPredicat x == "query"]
        avaluaQuery :: BaseConeixement -> Atom -> String
        avaluaQuery bc query
          | elem (query) (bc) == True = "true"
          | otherwise = "false"
        isFact :: Regla -> Bool
        isFact r
          | null(_termes(_cap r)) = True
          | otherwise = False

-- Genera la base de conecimiento iterativamente siguiente
-- el método explicado en el enunciado de la práctica
inferir :: Programa -> BaseConeixement -> BaseConeixement
inferir pr kb
  | newKb /= kb = inferir (pr) (newKb)
  | otherwise = newKb
  where newKb = consequencia (pr) (kb)

stringToRegla :: [String] -> Regla
stringToRegla rule
  | length ruleDecomposition == 1 = Regla { _cap = stringToAtom (consequence), _cos = []}
  | otherwise = Regla { _cap = stringToAtom (consequence), _cos = map stringToAtom ((splitAnd) (head ruleDecomposition))}
  where
    ruleDecomposition = splitRule rule
    consequence = last ruleDecomposition
    stringToAtom :: [String] -> Atom
    stringToAtom (x:xs) = Atom {_nomPredicat = x, _termes = (map stringToTerm (xs))}
    stringToTerm :: String -> Term
    stringToTerm word
      | isUpper (head word) = Var word
      | otherwise = Sym word
    splitAnd:: [String] -> [[String]]
    splitAnd line = splitOn ["&"] line
    splitRule :: [String] -> [[String]]
    splitRule rule = splitOn ["=>"] rule

splitString :: String -> [String]
splitString line = splitOn " " line

splitEnd :: [String] -> [[String]]
splitEnd rule = splitOn ["end"] rule

removePoint :: String -> String
removePoint word = init word

-- En esta implementación de la práctica he supuesto que en cada línea
-- solo hay una regla/hecho, es decir, las reglas estan separadas por saltos
-- de línea como por ejemplo:
--
-- progenitor ana brooke.
-- progenitor xerces brooke.
-- progenitor brooke damocles.

main = do
  contents <- getContents
  let entrada = splitEnd (map removePoint (lines (contents)))
  let programa = map (stringToRegla.splitString) (entrada !! 0)
  let queries = map (stringToRegla.splitString) (entrada !! 1)
  let answers = getAnswers (programa) (queries)
  mapM_ putStrLn answers
