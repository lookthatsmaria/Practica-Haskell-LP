# Implementació del Llenguatge de programació PALLÚS™️

> Programació Automàtica de Lògica Limitada i Ús Senzill


El **PALLÚS** és un llenguatge de programació lògic molt senzill, basat en llenguatges lògics com Prolog, però considerablament simplificat. PALLÚS és un llenguatge senzill i limitat, en particular no és Turing-complet.

En un programa PALLÚS tenim **constants** i **variables**. La seva representació textual exigeix que les constants i les variables comencin amb una lletra de l'alfabet. En el cas de les variables aquesta lletra serà majúscula, en el cas de les constants aquesta lletra és minúscula. Per exemple: `fghj`, `x`, `jordi`, `lp`, `magradalafib` són constants, `X`, `Intermedi`, `Problema` són variables. No admetem espais (tabuladors, salts de línia, etc) en els noms de les constants ni de les variables.

Els programes PALLÚS estan formats per **fets** i per **regles**.

Un **fet** és el nom d'un predicat (amb les mateixes restriccions textuals que les constants) seguit de zero o més constants separades per espais. El nombre de constants que segueix al nom del predicat defineix l'*aritat* del predicat. Un predicat només pot tenir una aritat, no podem fer servir el mateix predicat amb diferents aritats. Exemples de fets:

    progenitor ana brooke.
    progenitor xerces brooke.
    progenitor brooke damocles.

Fixem-nos que els fets de l'exemple acaben amb un punt. Aquest punt és un separador. Fets i regles han d'acabar amb punt.

Anomenarem **Àtom** a un predicat seguit de zero o més constants i/o variables. Així doncs, un fet és un àtom que només té constants (a aquests àtoms se'ls anomena *ground atoms*).

Les **regles** tenen uns **antecedents** i un **conseqüent**, separats per **=>**, és a dir, el format d'una regla és: **antecedents => conseqüent**. El conseqüent és un àtom. Els antecedents són diversos àtoms separats per la conjunció &. Exemples de regles:

    progenitor X Y => ancestre X Y.
    progenitor X Z & ancestre Z Y => ancestre X Y.

Així doncs, tenim el programa PALLÚS següent:

    progenitor ana brooke.
    progenitor xerces brooke.
    progenitor brooke damocles.
    progenitor X Y => ancestre X Y.
    progenitor X Z & ancestre Z Y => ancestre X Y.

Com que un fet pot considerar-se una regla sense antecedents (per tant, no cal fer servir el separador =>), **un programa PALLÚS és essencialment una col·lecció de regles**.

Podriem fer servir en Haskell els següents tipus de dades:

Un programa és un conjunt (en realitat una llista) de regles:

```haskell
type Programa = [ Regla ]
```

Una regla té un àtom pel conseqüent i una llista d'àtoms pels antecedents:

```haskell
data Regla = Regla Atom [ Atom ]
```
Un fet, doncs, és una regla amb una llista d'antecedents buida.

Un àtom té una String com a nom, i una llista de variables i/o constants (que anomenem termes):

```haskell
data Atom = Atom String [ Term ]
    deriving (Eq, Show)
```

Finalment, un terme és una variable (`Var ...`) o una constant (*Symbol*, `Sym ...`):

```haskell
data Term = Var String | Sym String
   deriving (Eq, Show)
```

<u>Incís</u>

Podeu fer servir *fields*, que no hem explicat al curs però són molt fàcils de fer servir. L'avantatge dels *fields* és que creen de franc funcions consultores pels elements del tipus. En el nostre cas, podriem definir:

```haskell
data Regla = Regla { _cap::Atom, _cos::[ Atom ] }
```

i ja tenim les funcions `_cap r` i `_cos r` (on `r` és una Regla) per treure els components de la regla, sense haver de definir-les nosaltres.

També podriem definir:

```haskell
data Atom = Atom { _nomPredicat::String, _termes::[ Term ] }
    deriving (Eq, Show)
```

Mireu aquesta web per saber-ne més: [https://www.haskell.org/tutorial/moretypes.html](https://www.haskell.org/tutorial/moretypes.html)

<u>Fi Incís</u>

Així doncs, una de les feines que haureu de fer serà convertir el format explicat fins ara a aquestes estructures de dades Haskell (haureu de *parsejar* el programa).

Ara, donat un programa PALLÚS, volem fer-li **preguntes** (o **queries**). Les preguntes tenen la forma d'una regla, però amb conseqüent de nom `query` seguit de totes les variables que apareixen a l'antecedent. Per exemple, donat el programa anterior, podriem fer les següents preguntes:

    progenitor xerces brooke => query.
    progenitor brooke xerces => query.
    ancestre brooke damocles => query.
    ancestre xerces damocles => query.
    ancestre Ancestre damocles => query Ancestre.
    ancestre xerces X & ancestre X damocles => query X.
    ancestre X Y & ancestre Y damocles => query X Y.

Fixeu-vos que, igual que les regles, les preguntes acaben en punt. Ara, què ha de fer el programa amb aquestes preguntes? Doncs donar una resposta, és clar!

1) `progenitor xerces brooke => query.` En aquest cas ha de respondre `true`, ja que la pregunta és un fet.

2) `progenitor brooke xerces => query.` En aquest cas ha de respondre `false`, ja que la pregunta és un fet inexistent, i no es pot deduir de les regles

3) `ancestre brooke damocles => query.` En aquest cas ha de respondre `true`, ja que la pregunta es pot deduir d'un fet i una de les regles.

4) `ancestre xerces damocles => query.` En aquest cas ha de respondre `true`, ja que la pregunta es pot deduir del programa

5) `ancestre Ancestre damocles => query Ancestre.` En aquest cas ha de respondre quelcom semblant a `Ancestre = brooke, Ancestre = ana, Ancestre = xerces`, ja que aquests tres valors de la variable `Ancestre` fan que la pregunta sigui certa.

6) `ancestre xerces X & ancestre X damocles => query X.` En aquest cas ha de respondre quelcom semblant a `X = brooke`, ja que aquest valor de `X` satisfà la query.

7) `ancestre X Y & ancestre Y damocles => query X Y.` En aquest cas ha de respondre quelcom semblant a `X = ana i Y = brooke, X = xerces i Y = brooke`, ja que aquestes assignacions a `X` i `Y` fan certa la pregunta.

En els fitxers separarem el programa de les preguntes amb **end.**. Aleshores, el vostre programa Haskell, donada l'entrada

    progenitor ana brooke.
    progenitor xerces brooke.
    progenitor brooke damocles.
    progenitor X Y => ancestre X Y.
    progenitor X Z & ancestre Z Y => ancestre X Y.
    end.
    progenitor xerces brooke => query.
    progenitor brooke xerces => query.
    ancestre brooke damocles => query.
    ancestre xerces damocles => query.
    ancestre Ancestre damocles => query Ancestre.
    ancestre xerces X & ancestre X damocles => query X.
    ancestre X Y & ancestre Y damocles => query X Y.
    end.

hauria de generar una resposta per cada pregunta, en linies separades. Per exemple, si el programa anterior l'anomenem `sample_program.inp`, la sortida hauria de ser (alguna variant més o menys arreglada de) aquesta:

    $ ./Pallus < sample_program.inp 
    true
    false
    true
    true
    [[(Var "Ancestre",Sym "brooke")],  [(Var "Ancestre",Sym "ana")],  [(Var "Ancestre",Sym "xerces")]]
    [[(Var "X",Sym "brooke")]]
    [[(Var "X",Sym "ana"),(Var "Y",Sym "brooke")],  [(Var "X",Sym "xerces"),(Var "Y",Sym "brooke")]]

Fixeu-vos que s'han fet servir les estructures de dades definides més amunt, i que el resultat, quan és diferent de booleà, consisteix en llistes de tuples, on cada tupla representa una sustitució (variable, constant). Definirem doncs:

```haskell
type Sustitucio = [ (Term, Term) ]     -- [(variable, constant), (variable, constant), ...]
```

Fixeu-vos que l'abast (*scope*) de les variables és la regla o la *query* on es troben, no va més enllà. Així, fer servir el mateix nom per variables de diferents regles no hauria de provocar cap conflicte. Fixeu-vos també que NO existeix la negació de predicats.

# Com funciona tot això?

En llenguatges de programació lògica com Prolog el procés de inferir els valors de les variables i aplicar les regles és força sofisticat. Afortunadament, les limitacions de PALLÚS fan que aquest procés sigui més senzill. El mètode que us proposem és simple, tot i que no té en compte l'eficiència.

Donat **un programa** (una llista de regles) i **<u>una</u>** **pregunta** (regla amb conseqüent de nom `query`) el procediment que seguirem serà el següent: Primer generarem tots els àtoms *ground* (àtoms amb termes constants) possibles a partir de les regles (programa més <u>una</u> pregunta). Un cop acabem, aparellem els termes de tots els atoms *ground* amb nom `query` amb les variables de la pregunta i aquesta serà la solució. Si la pregunta no té variables, només cal respondre `true` si trobem els antecedents de la pregunta entre els àtoms *ground*, i `false` en altre cas.

Per continuar la discussió ens caldrà definir el concepte de *Base de Coneixement*:

```haskell
type BaseConeixement = [ Atom ]
```

on tots els àtoms d'una base de coneixement són *ground* (el tipus, però, no ho reflecteix).

Generar tots els àtoms *ground* a partir del programa + <u>una</u> pregunta és un procés iteratiu. Comencem amb una base de coneixement buida, i anem fent servir els àtoms *ground* que ja tenim al programa i les regles per generar una nova base de coneixement. A partir d'aquesta nova base de coneixement tornem a fer servir els àtoms *ground* que tenim a la base de coneixement (en principi n'hem afegit de nous) més les regles per generar més àtoms *ground*, que afegirem a una nova base de coneixement. Així successivament fins que la base de coneixement nova sigui igual a l'anterior.

Hem de detallar més, però, què significa que a partir d'una base de coneixement i una regla, afegim nous àtoms *ground* a la base de coneixement. Una regla, recordem, té una llista d'àtoms com a antecedents. Aleshores, hem de mirar si podem *unificar* aquests àtoms amb els àtoms de la base de coneixement. És a dir, hem de mirar si podem sustituir les variables dels àtoms dels antecedent de la regla per constants, de manera que cada un dels àtoms (amb les variables sustituides per constants) sigui igual a algún dels àtoms de la base de coneixement. Això produeix una llista de possibles sustitucions que podem aplicar al conseqüent de la regla per obtenir nous àtoms *ground* que afegirem a la base de coneixement.

# Exemple

Tenim el programa PALLÚS, més <u>una</u> query:

    progenitor ana brooke.
    progenitor xerces brooke.
    progenitor brooke damocles.
    progenitor X Z & ancestre Z Y => ancestre X Y.
    progenitor X Y => ancestre X Y.
    ancestre X Y & ancestre Y damocles => query X Y.

que té la representació Haskell (que haurà de generar el vostre *parser*), d'acord als tipus que hem definit més amunt (estem utilitzant *fields*):

```haskell
pr :: Programa
pr =
  [
    Regla {_cap = Atom {_nomPredicat = "progenitor", _termes = [Sym "ana",Sym "brooke"]}, _cos = []},
    Regla {_cap = Atom {_nomPredicat = "progenitor", _termes = [Sym "xerces",Sym "brooke"]}, _cos = []},
    Regla {_cap = Atom {_nomPredicat = "progenitor", _termes = [Sym "brooke",Sym "damocles"]}, _cos = []},
    Regla {_cap = Atom {_nomPredicat = "ancestre", _termes = [Var "X",Var "Y"]}, _cos = [Atom {_nomPredicat = "progenitor", _termes = [Var "X",Var "Z"]},Atom {_nomPredicat = "ancestre", _termes = [Var "Z",Var "Y"]}]},
    Regla {_cap = Atom {_nomPredicat = "ancestre", _termes = [Var "X",Var "Y"]}, _cos = [Atom {_nomPredicat = "progenitor", _termes = [Var "X",Var "Y"]}]},
    Regla {_cap = Atom {_nomPredicat = "query", _termes = [Var "X",Var "Y"]}, _cos = [Atom {_nomPredicat = "ancestre", _termes = [Var "X",Var "Y"]},Atom {_nomPredicat = "ancestre", _termes = [Var "Y",Sym "damocles"]}]}
  ]
```

El procés iteratiu que hem mencionat comença amb una base de coneixement buida `kb0 = []`. Anem iterant una funció que podem anomenar, per exemple, `consequencia :: Programa -> BaseConeixement -> BaseConeixement` i anem generant nous àtoms *ground* que afegim a la base de coneixement:

```haskell
let kb1 = consequencia pr kb0
```

Ara `kb1` és (*pretty printed*): 

```haskell
[
 Atom {_nomPredicat = "progenitor", _termes = [Sym "ana",Sym "brooke"]},
 Atom {_nomPredicat = "progenitor", _termes = [Sym "xerces",Sym "brooke"]},
 Atom {_nomPredicat = "progenitor", _termes = [Sym "brooke",Sym "damocles"]}
]
```

Com `kb1 /= kb0`, continuem...

```haskell
let kb2 = consequencia pr kb1
```

resultant en una nova base de coneixement `kb2`:

```haskell
[
 Atom {_nomPredicat = "progenitor", _termes = [Sym "ana",Sym "brooke"]},
 Atom {_nomPredicat = "progenitor", _termes = [Sym "xerces",Sym "brooke"]},
 Atom {_nomPredicat = "progenitor", _termes = [Sym "brooke",Sym "damocles"]},
 Atom {_nomPredicat = "ancestre", _termes = [Sym "ana",Sym "brooke"]},
 Atom {_nomPredicat = "ancestre", _termes = [Sym "xerces",Sym "brooke"]},
 Atom {_nomPredicat = "ancestre", _termes = [Sym "brooke",Sym "damocles"]}
]
```

Ara `kb2 /= kb1`, per tant no podem aturar-nos...

```haskell
let kb3 = consequencia pr kb2
```

i obtenim `kb3`:

```haskell
[
 Atom {_nomPredicat = "progenitor", _termes = [Sym "ana",Sym "brooke"]},
 Atom {_nomPredicat = "progenitor", _termes = [Sym "xerces",Sym "brooke"]},
 Atom {_nomPredicat = "progenitor", _termes = [Sym "brooke",Sym "damocles"]},
 Atom {_nomPredicat = "ancestre", _termes = [Sym "ana",Sym "brooke"]},
 Atom {_nomPredicat = "ancestre", _termes = [Sym "xerces",Sym "brooke"]},
 Atom {_nomPredicat = "ancestre", _termes = [Sym "brooke",Sym "damocles"]},
 Atom {_nomPredicat = "ancestre", _termes = [Sym "ana",Sym "damocles"]},
 Atom {_nomPredicat = "ancestre", _termes = [Sym "xerces",Sym "damocles"]},
 Atom {_nomPredicat = "query", _termes = [Sym "ana",Sym "brooke"]},
 Atom {_nomPredicat = "query", _termes = [Sym "xerces",Sym "brooke"]}
]
```

`kb3 /= kb2`, així que fem una iteració més...

```haskell
let kb4 = consequencia pr kb3
```

obtenint `kb4`,

```haskell
[
 Atom {_nomPredicat = "progenitor", _termes = [Sym "ana",Sym "brooke"]},
 Atom {_nomPredicat = "progenitor", _termes = [Sym "xerces",Sym "brooke"]},
 Atom {_nomPredicat = "progenitor", _termes = [Sym "brooke",Sym "damocles"]},
 Atom {_nomPredicat = "ancestre", _termes = [Sym "ana",Sym "brooke"]},
 Atom {_nomPredicat = "ancestre", _termes = [Sym "xerces",Sym "brooke"]},
 Atom {_nomPredicat = "ancestre", _termes = [Sym "brooke",Sym "damocles"]},
 Atom {_nomPredicat = "ancestre", _termes = [Sym "ana",Sym "damocles"]},
 Atom {_nomPredicat = "ancestre", _termes = [Sym "xerces",Sym "damocles"]},
 Atom {_nomPredicat = "query", _termes = [Sym "ana",Sym "brooke"]},
 Atom {_nomPredicat = "query", _termes = [Sym "xerces",Sym "brooke"]}
]
```

I acabem, ja que `kb3 == kb4`.

El resultat d'aquest procés serà aparellar els termes del conseqüent amb nom `query` (que són només variables `[Var "X",Var "Y"]`) amb els termes dels àtoms de `kb4` que tinguin nom `query`. Hi ha dos possibles solucions, ``[Sym "ana",Sym "brooke"]`` i ``[Sym "xerces",Sym "brooke"]``. Per tant, una manera de representar les solucions és fent servir una llista de sustitucions (tal com hem definit el tipus més amunt):

```haskell
[ [(Var "X",Sym "ana"),(Var "Y",Sym "brooke")] , [(Var "X",Sym "xerces"),(Var "Y",Sym "brooke")] ]
```

Aquestes són les dues possibles respostes (dues sustitucions), tal com hem vist a l'exemple de més amunt. Naturalment, aquesta sortida està poc elaborada estèticament, vosaltres la podeu fer més maca.

# Avaluar Regles

Ara bé, com avaluem una regla per a que ens doni més àtoms *ground*? Ens anirà bé fer una funció:

```haskell
avaluaRegla :: BaseConeixement -> Regla -> BaseConeixement
```

que ens doni els àtoms *ground* que poden generar-se a partir de la regla. Per exemple, si la regla és un fet `progenitor brooke damocles.`:

```haskell
pr !! 2 --> Regla {_cap = Atom {_nomPredicat = "progenitor", _termes = [Sym "brooke",Sym "damocles"]}, _cos = []}
```

aleshores

```haskell
avaluaRegla kb (pr !! 2) --> [Atom {_nomPredicat = "progenitor", _termes = [Sym "brooke",Sym "damocles"]}]
```

Com aquest és un fet del programa, el resultat, independentment de la base de coneixement `kb` que fem servir, serà el mateix fet. En canvi, si tenim la regla `progenitor X Z & ancestre Z Y => ancestre X Y.`

```haskell
pr !! 3 --> Regla {_cap = Atom {_nomPredicat = "ancestre", _termes = [Var "X",Var "Y"]}, _cos = [Atom {_nomPredicat = "progenitor", _termes = [Var "X",Var "Z"]},Atom {_nomPredicat = "ancestre", _termes = [Var "Z",Var "Y"]}]}
```

podem avaluar-la en diferents bases de coneixement, que donarà resultats diferents:

```haskell
avaluaRegla kb1 (pr !! 3) --> []
avaluaRegla kb2 (pr !! 3) --> [Atom {_nomPredicat = "ancestre", _termes = [Sym "ana",Sym "damocles"]},Atom {_nomPredicat = "ancestre", _termes = [Sym "xerces",Sym "damocles"]]
avaluaRegla kb3 (pr !! 3) --> [Atom {_nomPredicat = "ancestre", _termes = [Sym "ana",Sym "damocles"]},Atom {_nomPredicat = "ancestre", _termes = [Sym "xerces",Sym "damocles"]]
```

Es deixa com a treball personal mirar de pensar per quina raó passa això, i quina diferència hi ha entre `kb1` i `kb2` per a que tinguem aquests resultats diferents (pensar això us ajudarà a intuir millor el procés d'avaluació d'una regla). Fixeu-vos també que el resultat és el mateix quan fem servir `kb2` i `kb3`.

# Unificació

Finalment, ens caldrà poder avaluar àtoms. Això ho podem fer trobant els fets de la base de coneixements que tenen concordànça amb l'àtom en qüestió. Serà interessant processar l'àtom d'acord a una llista de possibles sustitucions
i després **unificar** l'àtom amb els atom *ground* de la base de coneixement. Això exigirà tres funcions addicionals:

```haskell
avaluaAtom :: BaseConeixement -> Atom -> [ Sustitucio ] -> [ Sustitucio ]
unifica :: Atom -> Atom -> Maybe Sustitucio
sustitueix :: Atom -> Sustitucio -> Atom
```

i una constant

```haskell
sustitucioBuida :: Sustitucio
sustitucioBuida = []
```

La funciò `avaluaAtom kb atom llista` agafa una sustitució de la `llista`, l'executa sobre l'àtom, i després el resultat mira d'unificar-lo amb els àtoms de la base de coneixement. Això ho fem sobre totes les sustitucions de la `llista`. El resultat serà una llista de possibles sustitucions.

La funció `unifica atom1 atom2` retorna una (`Just`) sustitució si es poden unificar `atom1` i `atom2` o `Nothing` si no és possible la unificació (la funció `mapMaybe` us pot ser útil dins d'`avaluaAtom`). Penseu que tal i com estem fent el programa, `atom2` *sempre* serà *ground*. Com funciona? Dos àtoms, per poder-se unificar, han de tenir el mateix nom (`_nomPredicat` si feu servir *fields*). Després, si tenen el mateix nom, hem d'anar terme per terme de cada àtom, aparellats, comprovant que siguin la mateixa constant (en altre cas no s'unifiquen) o bé que siguin parelles (variable, constant). Aquí hem de tenir en compte una subtilesa, i és que no podem unificar la mateixa variable amb dues constants diferents. Veiem uns exemples (els farem en notació PALLÚS en lloc de fer servir la representació haskell):

1) `ancestre X brooke` **NO** s'unifica amb `progenitor ana brooke` perquè el seu nom és diferent `ancestre /= progenitor`.

2) `progenitor X X` **NO** s'unifica amb `progenitor ana brooke` perquè una variable, `X`, no pot valdre constants diferents,

3) `progenitor brooke damocles` **SÍ** s'unifica amb `progenitor brooke damocles`, però genera la sustitució buida (`Just []`) perquè no hi ha cap variable en joc.

4) `progenitor X damocles` **SÍ** s'unifica amb `progenitor brooke damocles`, i genera la sustitució `Just [(X,brooke)]`.

5) `progenitor X Y` **SÍ** s'unifica amb `progenitor brooke damocles`, i genera la sustitució `Just [(X,brooke), (Y,damocles)]`.

# Lliurament

La pràctica s'ha d'entregar no més tard de **diumenge 25 d'abril** a les 23:59 al Racó.

El vostre programa ha de tenir la següent capçalera:

```haskell
{-# LANGUAGE RecordWildCards #-}    -- per utilitzar fields

import Data.Char (isUpper)
import Data.List (nub, isInfixOf)
import Data.List.Split (splitOn)    -- cal instal·lar: cabal install split
import Data.String.Utils (strip)    -- cal instal·lar: cabal install MissingH
import Data.Maybe (mapMaybe, fromMaybe)
```

Fixeu-vos que hi ha dos paquets que no tindreu per defecte, per tant us caldrà instal·lar-los.
*No podeu utilitzar cap altre paquet* (i no us cal).

La vostra entrega ha de consistir en **un sol fitxer** `Pallus.hs`, degudament comentat, fent explícites les decisions que heu hagut de prendre.





