module Lambda.Untyped.Reduce where

import           Lambda.Untyped.Lam
import qualified Data.IntSet                   as IS
import           Data.IntSet                    ( IntSet )
import qualified Data.IntMap.Strict            as IM
import           Data.IntMap.Strict             ( IntMap )


data ReTerm = La Name ReTerm IntSet| Ap ReTerm ReTerm IntSet Name | Vr Name

pattern La' n t  <- La n t _    where La' n t = La n t $ IS.delete n $ free t
pattern Ap' f x  <- Ap f x _  _ where Ap' f x = Ap f x (IS.union (free f) (free x)) (max (fresh f) (fresh x))

fresh :: ReTerm -> Name
fresh (Vr n      ) = n + 1
fresh (Ap _ _ _ f) = f
fresh (La _ t _  ) = fresh t

free :: ReTerm -> IntSet
free (Vr n      ) = IS.singleton n
free (La _ _ f  ) = f
free (Ap _ _ f _) = f

subst :: ReTerm -> Name -> ReTerm -> ReTerm
subst x n s = sub x IM.empty  where
    sub v r =
        let f      = free v
            common = IS.intersection f $ IM.keysSet r
        in  if IS.notMember n f && IS.null common then v else sub1 v r

    sub1 v@(Vr name) ren | n == name = s
                         | otherwise = maybe v Vr $ IM.lookup name ren
    sub1 (La' name t) ren
        | IS.member name (free s)
        = let name' = fresh t `max` fresh s
          in  La' name' $ sub t (IM.insert name name' ren)
        | otherwise
        = La' name $ sub t ren
    sub1 (Ap' f y) ren = Ap' (sub f ren) (sub y ren)

step :: ReTerm -> Maybe ReTerm
step (Ap' (   La' n e) x) = pure $ subst e n x
step (Ap' fn@(Vr _   ) x) = Ap' fn <$> step x
step (Ap' f            x) = (`Ap'` x) <$> step f
step (La' n            x) = La' n <$> step x
step v                    = Nothing

redterm :: ReTerm -> ReTerm
redterm = (`maybe` redterm) <*> step

to::forall a.Lam a=>ReTerm->a
to (Ap' f x) = app (to f) (to x)
to (La' n x) = lam n (to x)
to (Vr n)  = var n

instance Lam ReTerm where
    var = Vr
    lam = La'
    app = Ap'


