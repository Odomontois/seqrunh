module Lambda.Untyped.Lam where

import           Prelude                 hiding ( succ
                                                , pred
                                                , not
                                                , and
                                                )

import           Data.Char

type Name = Int

class Lam a where
    var::Name->a
    lam::Name->a->a
    app::a->a->a

a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z
    :: Name
[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z]
    = [0 .. 25]


type LamT = forall a . Lam a => a

lam2 :: Lam a => Name -> Name -> a -> a
lam2 v1 v2 = lam v1 . lam v2
lam3 :: Lam a => Name -> Name -> Name -> a -> a
lam3 v1 v2 v3 = lam v1 . lam v2 . lam v3

app2 :: Lam a => a -> a -> a -> a
app2 = (app .) . app

appv :: Name -> Name -> LamT
appv x y = app (var x) (var y)
appv2 :: Name -> Name -> Name -> LamT
appv2 x y z = app2 (var x) (var y) (var z)



ident, first, second, true, false, not, or, and, zero, succ, one, two, plus, times, power, phi, pred, minus, is0, le, ge, lt, gt, eq
    :: LamT
--  pred, minus, le, ge, lt, gt, eq :: LamT
ident = lam x $ var x

first = lam2 x y $ var x
second = lam2 x y $ var y

true = first
false = second

not = lam3 b t f $ appv2 b f t
or = lam2 x y $ lam2 t f $ app2 (var x) (var t) $ appv2 y t f
and = lam2 x y $ lam2 t f $ app2 (var x) (appv2 y t f) (var f)

zero = lam2 s z (var z)
succ = lam3 n s z $ app (var s) $ appv2 n s z
one = lam2 s z $ appv s z
two = lam2 s z $ app (var s) $ appv s z

plus = lam2 x y $ app2 (var x) succ (var y)
times = lam2 x y $ app2 (var x) (app plus (var y)) zero
power = lam2 x y $ app2 (var y) (app times (var x)) one


phi =
    lam2 p z $ app2 (var z) (app succ $ app (var p) first) (app (var p) first)
pred = lam x $ app (app2 (var x) phi (lam z $ app2 (var z) zero zero)) false

minus = lam2 x y $ app2 (var y) pred (var x)
is0 = lam x $ app2 (var x) (lam x false) true

le = lam2 x y $ app is0 $ app2 minus (var x) (var y)
ge = lam2 x y $ app is0 $ app2 minus (var y) (var x)

lt = lam2 x y $ app not $ app2 ge (var x) (var y)
gt = lam2 x y $ app not $ app2 le (var x) (var y)
eq = lam2 x y $ app2 and (app2 le (var x) (var y)) (app2 ge (var x) (var y))




instance Lam String where
    var name = chr (mod name 26 + ord 'a')
        : if name < 26 then [] else show (quot name 26)
    lam name t = "λ" <> var name <> "." <> t
    app x y = "(" <> x <> " " <> y <> ")"
