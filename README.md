# tp1_2035

Qu'est-ce que fix?

Problèmes dans le s2l:
* Les cas aves les Snil à la fin
* Cas de bases qui capturaient tous sans les Snil

Problèmes l2d:
* Comment trouver index de Bruijn?
* Tenter d'utiliser tuples (Var, Value) dans l2d pour env, mais env de type [Var]

Problèmes eval:
* getValue retourner [], erreur de type
* `getValue :: Int -> Int -> [Int] -> Maybe Int` erreur quand on retourne Int, mais `Nothing` correct
* `getValue :: Int -> Int -> [Int] -> Int` erreur quand on retourne `[]` ou `Nothing`
* Incertitudes pour cas Dlambda et Dfix
* `Dmatch a b c = Vcons a (Vcons b c)`?
