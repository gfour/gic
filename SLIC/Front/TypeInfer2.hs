-- | Implementation of the type checker from the paper
--   \"Typing Haskell in Haskell\" by Mark P. Jones, 1999.
-- 
--   Notes:
-- 
--   * Does not support 'type' synonyms (assumed to have been expanded).
-- 
--   * Type classes and their instances should have already been collected
--     and given as Class/Inst values.
-- 
--   * Overlapping instances are not allowed.
-- 
--   * Needs a dependency analysis step to have broken down definitions to
--     binding groups.
-- 

module SLIC.Front.TypeInfer2 where

import Data.List ((\\), intersect, nub, partition, union)
import SLIC.AuxFun (ierr)

type Id = String
enumId :: Int -> Id
enumId n = "v"++(show n)

data Kind = Star | Kfun Kind Kind deriving (Eq)

data Type = TVar Tyvar
          | TCon Tycon
          | TAp  Type Type
          | TGen Int
            deriving (Eq)
                     
data Tyvar = Tyvar Id Kind deriving (Eq)
data Tycon = Tycon Id Kind deriving (Eq)

tChar :: Type
tChar = TCon (Tycon "Char" Star)

tArrow :: Type
tArrow = TCon (Tycon "(->)" (Kfun Star (Kfun Star Star)))

infixr 4 `fn`
fn :: Type -> Type -> Type
a `fn` b = TAp (TAp tArrow a) b

class HasKind t where
  kind :: t -> Kind
instance HasKind Tyvar where
  kind (Tyvar _ k) = k
instance HasKind Tycon where  
  kind (Tycon _ k) = k
instance HasKind Type where
  kind (TCon tc) = kind tc
  kind (TVar u)  = kind u
  kind (TAp t _) = case (kind t) of
                     (Kfun _ k) -> k
                     Star -> ierr "kind: Star"
  kind (TGen _)  = ierr "kind: TGen"

type Subst = [(Tyvar, Type)]

nullSubst :: Subst
nullSubst = []

(+->) :: Tyvar -> Type -> Subst
u +-> t = [(u, t)]

class Types t where
  apply :: Subst -> t -> t
  tv    :: t -> [Tyvar]
instance Types Type where
  apply s (TVar u)  = case lookup u s of
                        Just t  -> t
                        Nothing -> TVar u
  apply s (TAp l r) = TAp (apply s l) (apply s r)
  apply _ t         = t
  tv (TVar u)       = [u]
  tv (TAp l r)      = (tv l) `union` (tv r)
  tv _              = []
instance (Types a) => Types [a] where
  apply s = map (apply s)
  tv      = nub.concat.map tv

infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1

merge :: Subst -> Subst -> Maybe Subst
merge s1 s2 = if agree then Just s else Nothing
  where dom s'= map fst s'
        s     = s1++s2
        agree = all (\v -> apply s1 (TVar v) ==
                           apply s2 (TVar v))
                    (dom s1 `intersect` dom s2)

mgu :: Type -> Type -> Maybe Subst
mgu (TAp l r) (TAp l' r') = do s1 <- mgu l l'
                               s2 <- mgu (apply s1 r)
                                         (apply s1 r')
                               Just (s2 @@ s1)
mgu (TVar u) t            = varBind u t
mgu t (TVar u)            = varBind u t
mgu (TCon tc1) (TCon tc2)
  | tc1 == tc2            = Just nullSubst
mgu _ _                   = Nothing

varBind :: Tyvar -> Type -> Maybe Subst
varBind u t | t == TVar u       = Just nullSubst
            | u `elem` tv t     = Nothing
            | kind u == kind t  = Just (u +-> t)
            | otherwise         = Nothing

match :: Type -> Type -> Maybe Subst
match (TAp l r) (TAp l' r')     = do sl <- match l l'
                                     sr <- match r r'
                                     merge sl sr
match (TVar u) t
  | kind u == kind t            = Just (u +-> t)
match (TCon tc1) (TCon tc2)
  | tc1 == tc2                  = Just nullSubst
match _ _                       = Nothing

data Qual t = [Pred] :=> t deriving (Eq)

data Pred = IsIn Class Type deriving (Eq)

instance (Types t) => Types (Qual t) where
  apply s (ps :=> t) = apply s ps :=> apply s t
  tv (ps :=> t)      = tv ps `union` tv t
instance Types Pred where
  apply s (IsIn c t) = IsIn c (apply s t)
  tv (IsIn _ t)      = tv t

data Class = Class { name :: Id
                   , super :: [Class]
                   , insts :: [Inst] }
type Inst = Qual Pred

instance Eq Class where
  c == c' = name c == name c'

bySuper :: Pred -> [Pred]
bySuper p@(IsIn c t)
  = p : concat (map bySuper supers)
  where supers = [IsIn c' t | c' <- super c]

byInst :: Pred -> Inst -> Maybe [Pred]
byInst p (ps :=> h) = do u <- matchPred h p
                         Just (map (apply u) ps)
matchPred :: Pred -> Pred -> Maybe Subst
matchPred (IsIn c t) (IsIn c' t')
  | c==c'     = match t t'
  | otherwise = Nothing

reducePred :: Pred -> Maybe [Pred]
reducePred p@(IsIn c _) = foldr (|||) Nothing poss
  where poss = map (byInst p) (insts c)
        Nothing ||| y = y
        Just x  ||| _ = Just x

(||-) :: [Pred] -> Pred -> Bool
ps ||- p = any (p `elem`) (map bySuper ps) ||
           case reducePred p of
             Nothing -> False
             Just qs -> all (ps ||-) qs

data Scheme = Forall [Kind] (Qual Type) deriving (Eq)

instance Types Scheme where
  apply s (Forall ks qt) = Forall ks (apply s qt)
  tv      (Forall _  qt) = tv qt

quantify :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
  where vs' = [v | v<-tv qt, v `elem` vs]
        ks  = map kind vs'
        s   = zip vs' (map TGen [0..])

toScheme :: Type -> Scheme
toScheme t = Forall [] ([] :=> t)

data Assump = Id :>: Scheme

instance Types Assump where
  apply s (i :>: sc) = i :>: (apply s sc)
  tv      (_ :>: sc) = tv sc

find :: Id -> [Assump] -> Scheme
find i as = head [ sc | (i' :>: sc) <- as, i==i' ]

newtype TI a = TI (Subst -> Int -> (Subst, Int, a))

instance Monad TI where
  return x  = TI (\s n -> (s, n, x))
  TI c>>= f = TI (\s n ->
                   let (s', m, x) = c s n
                       TI fx      = f x
                   in  fx s' m)

runTI :: TI a -> a
runTI (TI c) = result
  where (_, _, result) = c nullSubst 0

getSubst :: TI Subst
getSubst = TI (\s n -> (s, n, s))

unify :: Type -> Type -> TI ()
unify t1 t2 = do s <- getSubst
                 case mgu (apply s t1) (apply s t2) of
                   Just u  -> extSubst u
                   Nothing -> error "unification error"
extSubst :: Subst -> TI ()
extSubst s' = TI (\s n -> (s' @@ s, n, ()))

newTVar :: Kind -> TI Type
newTVar k = TI (\s n ->
                 let v = Tyvar (enumId n) k
                 in  (s, n+1, TVar v))

freshInst :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = do ts <- mapM newTVar ks
                              return (inst ts qt)
class Instantiate t where
  inst :: [Type] -> t -> t
instance Instantiate Type where
  inst ts (TAp l r)  = TAp (inst ts l) (inst ts r)
  inst ts (TGen n)   = ts !! n
  inst _  t          = t
instance (Instantiate a) => Instantiate [a] where
  inst ts            = map (inst ts)
instance (Instantiate t) => Instantiate (Qual t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t
instance Instantiate Pred where
  inst ts (IsIn c t) = IsIn c (inst ts t)

type Infer e t = [Assump] -> e -> TI ([Pred], t)

data Literal = LitInt Integer
             | LitChar Char
tiLit :: Literal -> TI ([Pred], Type)
tiLit (LitChar _) = return ([], tChar)
tiLit (LitInt  _) = do v <- newTVar Star
                       return ([IsIn cNum v], v)

cNum :: Class
cNum = error "TODO: cNum"

data Pat = PVar Id
         | PLit Literal
         | PCon Assump [Pat]
tiPat :: Pat -> TI ([Pred], [Assump], Type)
tiPat (PVar i) = do v <- newTVar Star
                    return ([], [i :>: toScheme v], v)
tiPat (PLit l) = do (ps, t) <- tiLit l
                    return (ps, [], t)
tiPat (PCon (_ :>: sc) pats)
  = do (ps, as, ts) <- tiPats pats
       t' <- newTVar Star
       (qs :=> t) <- freshInst sc
       unify t (foldr fn t' ts)
       return (ps++qs, as, t')
tiPats :: [Pat] -> TI ([Pred], [Assump], [Type])
tiPats pats =
  do psasts <- mapM tiPat pats
     let ps = [p | (ps2,   _, _) <- psasts, p <- ps2]
         as = [a | (  _, as2, _) <- psasts, a <- as2]
         ts = [t | (  _,   _, t) <- psasts]
     return (ps, as, ts)

data Expr = Var Id
          | Lit Literal
          | Const Assump
          | Ap Expr Expr
          | Let BindGroup Expr
tiExpr :: Infer Expr Type
tiExpr as (Var i)
  = do let sc = find i as
       (ps :=> t) <- freshInst sc
       return (ps, t)
tiExpr _ (Const (_ :>: sc))
  = do (ps :=> t) <- freshInst sc
       return (ps, t)
tiExpr _ (Lit l)
  = do (ps, t) <- tiLit l
       return (ps, t)
tiExpr as (Ap e f)       
  = do (ps, te) <- tiExpr as e
       (qs, tf) <- tiExpr as f
       t <- newTVar Star
       unify (fn tf t) te
       return (ps++qs, t)
tiExpr as (Let bg e)
  = do (ps, as') <- tiBindGroup as bg
       (qs, t) <- tiExpr (as'++as) e
       return (ps++qs, t)

type Alt = ([Pat], Expr)
       
tiAlt :: Infer Alt Type
tiAlt as (pats, e)
  = do (ps, as', ts) <- tiPats pats
       (qs, t) <- tiExpr (as'++as) e
       return (ps++qs, foldr fn t ts)
tiAlts :: [Assump] -> [Alt] -> Type -> TI [Pred]
tiAlts as alts t
  = do psts <- mapM (tiAlt as) alts
       _ <- mapM (unify t) (map snd psts)
       return (concat (map fst psts))

reduce :: [Tyvar] -> [Tyvar] -> [Pred] -> ([Pred], [Pred])
reduce fs gs ps = (ds, rs')
  where (ds, rs) = split fs ps
        rs'      = useDefaults (fs++gs) rs
        
split :: [Tyvar] -> [Pred] -> ([Pred], [Pred])
split fs = partition (all (`elem` fs) . tv)
         . simplify []
         . toHnfs
toHnfs :: [Pred] -> [Pred]
toHnfs = concat.map toHnf
toHnf :: Pred -> [Pred]
toHnf p =
  if inHnf p then [p]
  else case reducePred p of
    Nothing -> error "context reduction error"
    Just ps -> toHnfs ps
inHnf :: Pred -> Bool
inHnf (IsIn _ t) = hnf t
  where hnf (TVar _)   = True
        hnf (TCon _)   = False
        hnf (TAp t' _) = hnf t'
        hnf (TGen _)   = ierr "isHnf: TGen"

simplify :: [Pred] -> [Pred] -> [Pred]
simplify rs [] = rs
simplify rs (p:ps) = simplify (p:(rs `diff2` qs)) (ps `diff2` qs)
  where qs = bySuper p
        rs2 `diff2` qs2 = [r | r <- rs2, r `notElem` qs2]

ambig :: [Tyvar] -> [Pred] -> [(Tyvar, [Pred], [Type])]
ambig vs ps
  = [(v, qs, defs v qs) |
     v <- tv ps \\ vs,
     let qs = [p | p <- ps, v `elem` tv p]]

stdClasses :: [Class]
stdClasses = error "TODO: stdClasses"

numClasses :: [Class]
numClasses = error "TODO: numClasses"

defaults :: [Type]
defaults = error "TODO: defaults"

defs :: Tyvar -> [Pred] -> [Type]       
defs v qs = [t | all ((TVar v) ==) ts,
                 all (`elem` stdClasses) cs,
                 any (`elem` numClasses) cs,
                 t <- defaults,
                 and [[] ||- IsIn c t | c <- cs]]
  where cs = [c | (IsIn c _) <- qs]
        ts = [t | (IsIn _ t) <- qs]
        
useDefaults :: [Tyvar] -> [Pred] -> [Pred]
useDefaults vs ps
  | any null tss = error "ambiguity error"
  | otherwise    = ps \\ ps'
  where ams = ambig vs ps
        tss = [ts | (_,  _, ts) <- ams]
        ps' = [p  | (_, qs,  _) <- ams, p <- qs]

topDefaults :: [Pred] -> Maybe Subst
topDefaults ps
  | any null tss = Nothing
  | otherwise    = Just (zip vs (map head tss))
  where ams = ambig [] ps
        tss = [ts | (_, _, ts) <- ams]
        vs  = [v  | (v, _,  _) <- ams]

type Expl = (Id, Scheme, [Alt])

tiExpl :: [Assump] -> Expl -> TI [Pred]
tiExpl as (_, sc, alts) =
  do (qs :=> t) <- freshInst sc
     ps <- tiAlts as alts t
     s <- getSubst
     let qs'      = apply s qs
         t'       = apply s t
         ps'      = [p | p <- apply s ps, not (qs' ||- p)]
         fs       = tv (apply s as)
         gs       = tv t' \\ fs
         (ds, rs) = reduce fs gs ps'
         sc'      = quantify gs (qs' :=> t')
     if sc /= sc' then
       error "signature too general"
     else if not (null rs) then
            error "context too weak"
          else
            return ds

type Impl = (Id, [Alt])

restricted :: [Impl] -> Bool
restricted bs = any simple bs
  where simple (_, alts) = any (null.fst) alts

tiImpls :: Infer [Impl] [Assump]
tiImpls as bs =
  do ts <- mapM (\_ -> newTVar Star) bs
     let is    = map fst bs
         scs   = map toScheme ts
         as'   = zipWith (:>:) is scs ++ as
         altss = map snd bs
     pss <- sequence (zipWith (tiAlts as') altss ts)
     s <- getSubst
     let ps'      = apply s (concat pss)
         ts'      = apply s ts
         fs       = tv (apply s as)
         vss      = map tv ts'
         gs       = foldr1 union vss \\ fs
         (ds, rs) = reduce fs (foldr1 intersect vss) ps'
     if restricted bs then
       let gs'  = gs \\ tv rs
           scs' = map (quantify gs' . ([] :=>)) ts'
       in  return (ds++rs, zipWith (:>:) is scs')
     else
       let scs' = map (quantify gs . (rs :=>)) ts'
       in  return (ds, zipWith (:>:) is scs')

type BindGroup = ([Expl], [[Impl]])

tiBindGroup :: Infer BindGroup [Assump]
tiBindGroup as (es, iss) =
  do let as' = [ v :>: sc | (v, sc, _) <- es ]
     (ps, as'') <- tiSeq tiImpls (as' ++ as) iss
     qs <- mapM (tiExpl (as''++as'++as)) es
     return (ps++concat qs, as''++as')

tiSeq :: Infer bg [Assump] -> Infer [bg] [Assump]
tiSeq _ _ []
  = return ([], [])
tiSeq ti as (bs : bss)
  = do (ps, as' ) <- ti as bs
       (qs, as'') <- tiSeq ti (as'++as) bss
       return (ps++qs, as''++as')

type Program = [BindGroup]

tiProgram :: [Assump] -> Program -> [Assump]
tiProgram as bgs =
  runTI $
  do (ps, as') <- tiSeq tiBindGroup as bgs
     s <- getSubst
     let ([], rs) = split [] (apply s ps)
     case topDefaults rs of
       Just s' -> return (apply (s' @@ s) as')
       Nothing -> error "top-level ambiguity"

