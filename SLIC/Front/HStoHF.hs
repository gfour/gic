-- | The Haskell to FL translator.
-- 
--   Translates the parsed Haskell syntax to FL.
-- 

module SLIC.Front.HStoHF (fromHStoHF, mkStrList) where

import Data.Map (empty, filterWithKey, fromList)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Language.Haskell.Exts.Syntax as S
import SLIC.AuxFun (errM, ierr)
import SLIC.Constants (tcMod)
import SLIC.Front.PatternCompiler (patComp, patCompMatches)
import SLIC.Front.Preprocessor (dummyCName, projCName, updCName)
import SLIC.Front.Renamer (renInvNames)
import SLIC.Front.Typeclass
import SLIC.State (Options(optStrict))
import SLIC.SyntaxAux
import SLIC.SyntaxFL
import SLIC.Types

-- * Fresh name generator and state

-- | Threaded counter, used to generate fresh names.
type TState = Int

-- | Result annotated by threaded state.
type TInfo a = (TState, a)

mapTI :: TState -> (a -> TState -> TInfo b) -> [a] -> TInfo [b]
mapTI st _ [] = (st, [])
mapTI st f (a:as) =
  let (st1,  b) = f a st
      (st2, bs) = mapTI st1 f as
  in  (st2, b:bs)

-- | Generates a fresh name (qualified with the given module name).
freshName :: TState -> TInfo QName
freshName (i) = (i+1, QN Nothing ("e$"++(show i)))

-- * Main translation

-- | The main translation function from a Haskell module to an FL module.
--   Also returns the declared class instances as a separate unit (since
--   processing them needs global typeclass information, available at a
--   later stage, when module interfaces are read).
fromHStoHF :: Options -> FPath -> S.Module -> (ModF, [TcInstF])
fromHStoHF opts fp (S.Module _ (S.ModuleName m) _ _ exports imports decls) =
    let fm = (m, fp)
        dataDecls [] = []
        dataDecls (d@(S.DataDecl {}) : ds) =
            (transl_dt fm d) : (dataDecls ds)
        dataDecls (d@(S.GDataDecl {}) : ds) =
            (transl_dt fm d) : (dataDecls ds)
        dataDecls (_ : ds) = dataDecls ds
        (st1, fDecls) = funDecls opts fm decls (0)
        (_  , tcInsts) = tcInstDecls opts fm decls st1
        fImports = (map (transl_imp fm) imports) ++ (map tcImport $ tcIDecls tcs)
        -- rename invalid names
        (fImportsFL, dtsFL, defsFH, tcInstsFH) =
          renInvNames (fImports, dataDecls decls, fDecls, tcInsts)
        -- the signatures of all module functions
        allSigs = fromList $ (map defSig defsFH)++(concatMap dtSigs dtsFL)
        fExports =
          case exports of
            Nothing -> allSigs   -- all functions are exported
            Just es ->           -- selected functions are exported
              let importedNames = concatMap (transl_espec fm dtsFL) es
              in  filterWithKey (\k _->k `elem` importedNames) allSigs
        progFH = Prog dtsFL defsFH
        -- run the pattern compiler to translate full patterns to simple ones
        (progFL, tcInstsFL) = patComp progFH tcInstsFH
        tcs = TcInfo (tcDecls fm decls) (map (tcISig m) tcInstsFL)
        tAnnot = fromList $ typeSigs fm decls
    in  (Mod fm fExports fImportsFL progFL tAnnot tcs, tcInstsFL)

funDecls :: Options -> MNameF -> [S.Decl] -> TState -> TInfo [DefFH]
funDecls opts fm decls stI =
  let funDecls_aux :: [S.Decl] -> TState -> TInfo [DefFH]
      funDecls_aux [] st = (st, [])
      funDecls_aux (d@(S.PatBind {}) : ds) st =
        let (st1, d' ) = transl_def opts fm d st
            (st2, ds') = funDecls_aux ds st1
        in  (st2, d' : ds')
      funDecls_aux (d@(S.FunBind [_]) : ds) st =
        let (st1, d' ) = transl_def opts fm d st
            (st2, ds') = funDecls_aux ds st1
        in  (st2, d' : ds')
      funDecls_aux ((S.FunBind matches) : ds) st =
        let (st1, d' ) = transl_fbind opts fm matches st
            (st2, ds') = funDecls_aux ds st1
        in  (st2, d' : ds')
      funDecls_aux ((S.DataDecl {}) : ds) st  = funDecls_aux ds st
      funDecls_aux ((S.ClassDecl {}) : ds) st = funDecls_aux ds st
      funDecls_aux ((S.InstDecl {}) : ds) st  = funDecls_aux ds st
      funDecls_aux ((S.TypeSig {}) : ds) st   = funDecls_aux ds st
      funDecls_aux ((S.GDataDecl {}) : ds) st = funDecls_aux ds st
      funDecls_aux (e : _) _ =
        errM fm $ "unsupported piece of Haskell syntax: "++(show e)
  in  funDecls_aux decls stI

-- | Translates Haskell type class declarations to FL metadata information, to
--   be attached to the module interface.
tcDecls :: MNameF -> [S.Decl] -> [TcDecl]
tcDecls fm decls =
  let tcDecl (S.ClassDecl _ [] cn [S.UnkindedVar tv] _ cdecls) =
        let tcn = getName cn
            mkCDecl (S.ClsDecl (S.TypeSig _ [S.Ident f] t)) =
              let t' = transl_type fm t
                  frmsN = length $ takeParams t'
                  frms = map (\i->f++"_$"++(show i)) [0..frmsN-1]
                  fsig = (f, frms)
              in  (fsig, t')
            mkCDecl d =
              errM fm $ "Unsupported class method declaration found: "++(show d)
        in  Just $ TcDecl tcn (getName tv) (map mkCDecl cdecls)
      tcDecl (S.ClassDecl _ (_:_) _ _ _ _) =
        errM fm "Class contexts are not supported yet"
      tcDecl (S.ClassDecl _ _ _ tvs _ _) | (length tvs /= 1) =
        errM fm "Class type list length not 1 (feature not supported yet)."
      tcDecl (S.ClassDecl _ [] _ [S.KindedVar _ _] _ _) =
        errM fm "Kinded variables in class declarations are not supported."
      tcDecl (S.ClassDecl {}) =
        ierr "Malformed class declaration."
      tcDecl _ = Nothing
  in  mapMaybe tcDecl decls

-- | Translate type class instance methods to FL instance definitions (to
--   be desugared to plain FL definitions at a later stage).
tcInstDecls :: Options -> MNameF -> [S.Decl] -> TState -> TInfo [TcInstFH]
tcInstDecls opts fm ((S.InstDecl _ ctxt clName iTypes iDecls) : ds) st =
  let clNameStr = lName $ getQName clName
      isInsDecl (S.InsDecl _) = True
      isInsDecl _ = False      
      clInstDecls decls st0 =
        if all isInsDecl decls then
          funDecls opts fm (map (\(S.InsDecl d)->d) decls) st0
        else
          errM fm "clInstDecls: found special instance declaration."
  in  case ctxt of
      [] ->
        case iTypes of
          [] -> errM fm $ "class instance "++clNameStr++" has no types"
          (_:_:_) -> errM fm "class instance has > 1 types"
          [iType] ->
            let (st1, tcInstMethods) = clInstDecls iDecls st
                tcInst = TcInst clNameStr (transl_type fm iType) tcInstMethods
                (st2, tcInsts) = tcInstDecls opts fm ds st1
            in  (st2, tcInst:tcInsts)
      _  -> errM fm "class instance contexts are not supported yet"
tcInstDecls _ _ [] st = (st, [])
tcInstDecls opts fm (_:ds) st = tcInstDecls opts fm ds st

typeSigs :: MNameF -> [S.Decl] -> TEnvL
typeSigs _ [] = []
typeSigs fm@(m, _) (ts@(S.TypeSig _ names t):ds) =
  case names of
    [n] ->
      let t' = transl_type fm t
          qn = QN (Just m) (getName n)
      in  (qn, (t', Just $ length $ takeParams t')) : typeSigs fm ds
    _   -> errM fm $ "Cannot handle type signature: "++(show ts)
typeSigs fm (_:ds) = typeSigs fm ds

-- | Translates an export specification to a list of exported qualified names.
transl_espec :: MNameF -> [Data] -> S.ExportSpec -> [QName]
transl_espec (m, _) _ (S.EVar evar) =
  [QN (Just m) (lName $ getQName evar)]
transl_espec fm dtsFL (S.EAbs dt)   =
  let dt' = getQName dt
  in  dcsToNames $ findConstrsOf fm dt' dtsFL
transl_espec fm@(m, _) _ (S.EThingWith _ names) =
  let aux (S.ConName cn) = QN (Just m) (getName cn)
      aux (S.VarName vn) = errM fm $ "Imported class methods are not supported (found method "++(show vn)++")"
  in  map aux names
transl_espec fm _ e' =
  errM fm $ "Cannot process exported symbol: "++(show e')

-- * Imports

-- | Translates the "import" declaration of names from some module. Since the
--   parser only provides names, the rest of the information is empty and will
--   be filled in later, when interface files are read.
transl_imp :: MNameF -> S.ImportDecl -> IDecl
transl_imp fm (S.ImportDecl _ (S.ModuleName mn) False False Nothing Nothing (Just (False, specs))) =
  let transl_ispec (S.IVar (S.Ident v)) =
        let qn = QN (Just mn) v
        in  [(qn, IInfo Nothing Nothing NFunc Nothing Nothing)]
      transl_ispec (S.IAbs (S.Ident c)) =
        let qn = QN (Just mn) c
        in  [(qn, IInfo Nothing Nothing NFunc Nothing Nothing)]
      -- translate a partially imported constructor
      transl_ispec (S.IThingWith dt names) =
        let transl_itw (S.ConName cn) =
              let qn = QN (Just mn) (getName cn)
              in  (qn, IInfo Nothing Nothing NConstr Nothing Nothing)
            transl_itw (S.VarName vn) = errM fm $ "Exported class methods are not supported (found method "++(show vn)++")"
            dtName = QN (Just mn) (getName dt)
        in  (dtName, IInfo Nothing Nothing NDType Nothing Nothing) :
            (map transl_itw names)
      transl_ispec imp = error $ "This import statement is not supported: "++(show imp)
  in  IDecl mn (Data.Map.fromList $ concatMap transl_ispec specs) Nothing
transl_imp fm imp = errM fm $ "The Haskell-to-FL translation does not support import statement: "++(show imp)

-- | The type class declarations of the program become explicit 'import'
--   declarations for the defunctionalized type class handlers.
tcImport :: TcDecl -> IDecl
tcImport (TcDecl _ _ methods) =
  let mkQN x = QN (Just tcMod) x
      auxII ((methodName, args), t) =
        let qn  = mkQN methodName
            qns = map mkQN args
            ar  = length args
            -- TODO: assume that the defunctionalized handler does pattern
            -- matching of depth=1
            iName0 = (qn, IInfo (Just t) (Just ar) NFunc Nothing (Just 1))
            iSig0  = (qn, qns)
        in  (iName0, iSig0)
      (iNames0, iSigs0) = unzip $ map auxII methods
      iNames = fromList iNames0
      iSigs  = fromList iSigs0
  in  IDecl tcMod iNames (Just (iSigs, empty))

-- * Data types

-- | Data type declaration translation.
transl_dt :: MNameF -> S.Decl -> Data
transl_dt fm (S.DataDecl _ S.DataType _ dtName tvs constrs _) =
  let dtId = QN Nothing (getName dtName)
      cs = map (transl_c fm) constrs
      as = map transl_dt_tv tvs
  in  Data dtId as cs
transl_dt fm (S.GDataDecl _ S.DataType _ gadtName tvs _ constrs _) =
  let dtId = QN Nothing (getName gadtName)
      cs = map (transl_gadt_c fm) constrs
      as = map transl_dt_tv tvs
  in  Data dtId as cs
transl_dt fm x =
    errM fm $ "transl_dt: unhandled Haskell construct"++(show x)

-- | Translates the type variable bindings in data type declarations.
transl_dt_tv :: S.TyVarBind -> SName
transl_dt_tv (S.KindedVar hsn _) = getName hsn
transl_dt_tv (S.UnkindedVar hsn) = getName hsn

-- | Simple constructor translation.
transl_c :: MNameF -> S.QualConDecl -> DConstr
transl_c fm (S.QualConDecl _ _ _ cDecl) =
  let trans_cd cName components =      
        let compsF = map (transl_comp fm) components
        in  DConstr (QN Nothing (getName cName)) compsF Nothing
      noSels cs = zip (repeat []) cs
  in  case cDecl of
        S.ConDecl cName components -> trans_cd cName (noSels components)
        S.InfixConDecl comp1 cName comp2 -> trans_cd cName (noSels [comp1, comp2])
        S.RecDecl cName components ->
          trans_cd cName components

transl_gadt_c :: MNameF -> S.GadtDecl -> DConstr
transl_gadt_c fm (S.GadtDecl _ hsn hst) =
  let t = transl_type fm hst
      ts = types t
      comps = map (\t0->DT t0 False Nothing) (init ts)
      rt = last ts
  in  DConstr (QN Nothing (getName hsn)) comps (Just rt)

-- | Constructor component types translation.
transl_comp :: MNameF -> ([S.Name], S.BangType) -> DT
transl_comp fm (ns, bt) =
  let sel =
        case ns of
          []    -> Nothing
          [qn]  -> Just (QN Nothing (getName qn))
          (_:_) ->
            errM fm $ "Too many selectors defined for component :: "++(show bt)
  in  case bt of
        S.BangedTy   typ -> DT (transl_type fm typ) True  sel
        S.UnBangedTy typ -> DT (transl_type fm typ) False sel
        S.UnpackedTy _   -> errM fm "Unpack types are not supported."

-- | Constructor component data type extraction.
transl_type :: MNameF -> S.Type -> Type
transl_type fm typ = 
  let tyConName (S.TyCon (S.UnQual hsName)) = getName hsName
      tyConName e = ierr $ "tyConName: invalid call, "++(show e)
      tyToType t =
        case tyConName t of
            "Int"     -> tInt
            "Bool"    -> tBool
            "Integer" -> tInteger
            "String"  -> tString
            dt        -> Tg (T (QN Nothing dt))
  in  case typ of
        S.TyCon (S.Special S.UnitCon) -> tUnit
        S.TyCon (S.UnQual _) -> tyToType typ          
        S.TyFun a b ->
          Tf (transl_type fm a) (transl_type fm b)
        S.TyParen t -> transl_type fm t
        S.TyList t -> Ta tList (transl_type fm t)        
        S.TyVar (S.Ident a) -> Tv a
        -- ignore the IO monad
        S.TyApp (S.TyCon (S.UnQual (S.Ident "IO"))) t -> transl_type fm t
        S.TyApp t1 t2 -> Ta (transl_type fm t1) (transl_type fm t2)
        S.TyTuple S.Boxed hts ->
          let tsN = length hts
          in  if tsN > maxTupleSize then
                error $ "transl_type: found tuple with too many elements, max="++
                        (show maxTupleSize)
              else
                mkTupleT $ map (transl_type fm) hts
        S.TyTuple S.Unboxed _ -> error "Unboxed tuples are not supported."
        _ ->  errM fm $ "Unsupported type in data declaration: "++(show typ)

-- | Definition translation.
transl_def :: Options -> MNameF -> S.Decl -> TState -> TInfo DefFH
transl_def opts fm (S.PatBind _ pat _ rhs _) st =
    case pat of
      S.PVar _ ->
          case rhs of
            S.GuardedRhss _ ->
              errM fm  "Guarded RHS in function/pattern definition is not supported."
            S.UnGuardedRhs hsExpr ->
              mkDefF opts fm pat hsExpr st
      _ -> errM fm $ "Unsupported pattern definition"++(show pat)
transl_def opts fm (S.FunBind [S.Match _ hsName pats _ rhs _]) st =
    let defQName = QN Nothing (getName hsName)
        getFrm pv@(S.PVar _)   = Frm (pv2v fm pv) (optStrict opts)
        getFrm (S.PBangPat pv) = Frm (pv2v fm pv) True
        getFrm (S.PParen pp)   = getFrm pp
        getFrm f = errM fm $ "Haskell-to-FL: unsupported formal in pattern, "++(show f)
        formals = map getFrm pats
        S.UnGuardedRhs body = rhs
        (st1, eFL) = transl_e opts fm body st
    in  (st1, DefF defQName formals eFL)
transl_def _ fm x _ =
    errM fm $ "transl_def: unhandled Haskell construct: "++(show x)

-- | Generate an FL definition from a simple "x = ..." binding.
mkDefF :: Options -> MNameF -> S.Pat -> S.Exp -> TState -> TInfo DefFH
mkDefF opts fm pat hsExpr st =
  let defQName = pv2v fm pat
      (st', eFL) = transl_e opts fm hsExpr st
  in  (st', DefF defQName [] eFL)

-- | Expression translation.
transl_e :: Options -> MNameF -> S.Exp -> TState -> TInfo ExprFH
transl_e _ _ (S.Var hsv) st =
    let qname = getQName hsv
        name  = pprint qname ""
    in  case reads name :: [(Int, String)] of
          [(n, "")] -> (st, ConF (LitInt n) [])
          _         -> (st, XF (V qname))
-- | Flatten curried applications to match the style of FL.
--   This also handles constructors.
transl_e opts fm app@(S.App _ _) st =
  let findFName (S.App (S.Var v) _) = getQName v
      findFName (S.App (S.Con c) _) = getQName c
      findFName (S.App f _)         = findFName f
      findFName (S.Paren x)         = findFName x
      findFName x =
          errM fm $ "findFName: unhandled Haskell construct, "++(show x)
      gatherArgs (S.App (S.Var _) arg) = [arg]
      gatherArgs (S.App (S.Con _) arg) = [arg]
      gatherArgs (S.App (S.Lambda _ _ _) arg) = [arg]
      gatherArgs (S.App (S.Paren f) arg) = gatherArgs (S.App f arg)
      gatherArgs (S.App f arg)         = (gatherArgs f) ++ [arg]
      gatherArgs (S.Paren x)           = gatherArgs x
      gatherArgs lam@(S.Lambda {})     = [lam]
      gatherArgs x =
          errM fm $ "gatherArgs: unhandled Haskell construct, "++(show x)
      isConstructor (S.App (S.Var _) _) = False
      isConstructor (S.App (S.Con _) _) = True
      isConstructor (S.InfixApp _ f _)  =
        case f of S.QVarOp _ -> False ; S.QConOp _ -> True
      isConstructor (S.App f _)         = isConstructor f
      isConstructor (S.Paren x)         = isConstructor x
      isConstructor (S.Lambda {})       = False
      isConstructor x =
          errM fm $ "isConstructor: unhandled Haskell construct, "++(show x)
      isLam (S.Var _)       = False
      isLam (S.Con _)       = False
      isLam (S.InfixApp {}) = False
      isLam (S.App f _)     = isLam f
      isLam (S.Paren x)     = isLam x
      isLam (S.Lambda {})   = True
      isLam x = errM fm $ "isLam: unhandled Haskell construct, "++(show x)
      fName = findFName app
      (st1, argsF) =
        mapTI st (transl_e opts fm) (gatherArgs app)
  in  if isConstructor app then
        (st1, ConstrF fName argsF)
      else if isLam app then
             let (st2, lamName) = freshName st1
                 innerLam lam@(S.Lambda {}) = lam
                 innerLam (S.App f _) = innerLam f
                 innerLam (S.Paren x) = innerLam x
                 innerLam x = ierr $ "leftmost expression is not lambda: "++(show x)
                 S.Lambda _ pats eLambda = innerLam app
                 bvs = simplePats fm pats
                 (st3, eBind) = transl_e opts fm eLambda st2
             in  (st3, mkLambdaLet opts lamName bvs eBind (FF (V lamName) argsF))
           else
             (st1, FF (V fName) argsF)
transl_e _ _ (S.Con (S.Special S.UnitCon)) st = (st, ConstrF bf_Unit [])
transl_e _ _ (S.Con cstr) st =
  let cstrF = getQName cstr
      qc = case cstrF of                         -- True/False are special
             QN _ "True"  -> ConF (CN CTrue) []
             QN _ "False" -> ConF (CN CFalse) []
             _            -> ConstrF cstrF []
  in  (st, qc)
transl_e opts fm (S.Case e pats) st =
  let (st1, eF   ) = transl_e opts fm e st
      (st2, patsF) = mapTI st1 (transl_pat opts fm) pats
  in  (st2, CaseF noCaseLoc eF underscoreVar patsF)
transl_e opts fm (S.Paren e) st = transl_e opts fm e st
transl_e opts fm (S.If cond e1 e2) st =
  let (st1, condFL) = transl_e opts fm cond st
      (st2, e1FL  ) = transl_e opts fm e1 st1
      (st3, e2FL  ) = transl_e opts fm e2 st2
  in  (st3, ConF (CN CIf) [condFL, e1FL, e2FL])
transl_e opts fm (S.InfixApp e1 app e2) st =
  let (st1, e1FL) = transl_e opts fm e1 st
      (st2, e2FL) = transl_e opts fm e2 st1
      eFL =
        case app of
          S.QVarOp hsn ->
            let op = getQName hsn
                opN = lName op
            in  if opN `elem` cBuiltinOps then
                  ConF (CN (cOpForStr opN)) [e1FL, e2FL]   -- built-in operator
                else
                  FF (V op) [e1FL, e2FL]       -- infix function application
          S.QConOp (S.Special S.Cons) -> ConstrF bf_Cons [e1FL, e2FL]  -- (:)
          S.QConOp hsn ->           
            error $ "Infix constructor application is not supported: "++(qName $ getQName hsn)
  in  (st2, eFL)
transl_e _ _ (S.Lit (S.Int i)) st = (st, ConF (LitInt (fromInteger i)) [])
transl_e _ fm (S.Lit l) st =
  let l' = case l of
             S.Char ch    -> charToInt ch
             S.String str -> mkStrList str
             _            -> errM fm $ "unhandled literal "++(show l)
  in  (st, l')
transl_e opts fm (S.Let binds e) st =
  let (st1, bindsFL) = transl_let_binds opts fm binds st
      (st2, eFL    ) = transl_e opts fm e st1
  in  (st2, LetF Nothing bindsFL eFL)
transl_e opts fm (S.NegApp e) st =
  let (st1, eFL) = transl_e opts fm e st
  in  (st1, ConF (CN CNeg) [eFL])
transl_e opts fm (S.Lambda _ pats e) st =
  let bvs = simplePats fm pats
      mkLam [] = transl_e opts fm e st
      mkLam (v:vs) =
        let (st1, vs')     = mkLam vs
            (st2, lamName) = freshName st1
        in  (st2, mkLambdaLet opts lamName [v] vs' (XF (V lamName)))
  in  mkLam bvs
transl_e opts fm (S.List l) st =
  let aux [] st0 = (st0, ConstrF bf_Nil [])
      aux (e:es) st0 =
        let (st1, eFL ) = transl_e opts fm e st0
            (st2, esFL) = aux es st1
        in  (st2, ConstrF bf_Cons [eFL, esFL])
  in  aux l st
transl_e opts fm (S.Do stmts) st =
  let aux ((S.Generator _ pat e):sts) st1 =
        let (st2, eFL  ) = transl_e opts fm e st1
            (st3, stsFL) = aux sts st2
        in  -- TODO: we ignore the scrutinee x<-...
            (st3, CaseF noCaseLoc eFL underscoreVar
                  [PatB (mkPat fm pat, PatInfo True) stsFL])
      aux [S.Qualifier e] st1 = transl_e opts fm e st1
      aux _ _ = error "transl_e: cannot process do notation"
  in  aux stmts st
transl_e opts fm (S.RecUpdate e fields) st =
  -- translate e{sel1=e1}{e2=sel2}... to upd_sel1(upd_sel2(e, e2), e1)
  -- this is different from 3.15.3 in the Haskell Report
  let (st1, eFL) = transl_e opts fm e st
  in  transl_rec_upd opts fm eFL fields st1
transl_e opts fm (S.RecConstr qn fields) st =
  transl_rec_upd opts fm (XF $ V $ dummyCName $ getQName qn) fields st
transl_e opts fm (S.Tuple el) st =
  let (st1, el1) = mapTI st (transl_e opts fm) el
      elN = length el
  in  if elN > maxTupleSize then
        error $ "Tuples with more than "++(show maxTupleSize)++
                " elements are not supported"
      else
        (st1, ConstrF (bf_Tuple elN) el1)
transl_e _ fm x _ =
  errM fm $ "transl_e: unhandled Haskell construct, "++(show x)

mkLambdaLet :: Options -> QName -> [QName] -> ExprFH -> ExprFH -> ExprFH
mkLambdaLet opts lamName fs eBind e =
  let frms = map (\v->Frm v (optStrict opts)) fs
  in  LetF Nothing [DefF lamName frms eBind] e

transl_fbind :: Options -> MNameF -> [S.Match] -> TState -> TInfo DefFH
transl_fbind opts fm matches st =
  let S.Match _ f ps _ _ _ = matches !! 0    -- calculate formals
      fQ = getQNameN f
      newFrms = map (\i ->procLName (\x->x++"$"++(show i)) fQ) [0..length ps-1]
      mkFrm qn = Frm qn False      -- assumed non-strict as in transl_def
      mkExp qn = XF (V qn)
      mkMatch (S.Match _ _ pats _ (S.UnGuardedRhs eR) _) st0 =
        let (st1, eR') = transl_e opts fm eR st0
        in  (st1, (map (mkPat fm) pats, Nothing, eR'))
      mkMatch (S.Match _ _ _ _ (S.GuardedRhss _) _) _ =
        error "The parser doesn't support guarded rhs's in fbinds."
      (st2, matchesFH) = mapTI st mkMatch matches
  in  (st2, DefF fQ (map mkFrm newFrms) $
            patCompMatches (map mkExp newFrms) matchesFH)

transl_rec_upd :: Options -> MNameF -> ExprFH ->
                  [S.FieldUpdate] -> TState -> TInfo ExprFH
transl_rec_upd opts fm innerExpr fields st =
  let aux [] st0 = (st0, innerExpr)
      aux ((S.FieldUpdate sel eSel):fs) st0 =
        let updFunc = (updCName $ getQName sel)
            (st1, fsFL  ) = aux fs st0
            (st2, eSelFL) = transl_e opts fm eSel st1
        in  (st2, FF (V updFunc) [fsFL, eSelFL])
      aux (fld:_) _ = errM fm $ "Unsupported records feature: "++(show fld)
  in  aux fields st

transl_let_binds :: Options -> MNameF -> S.Binds -> TState -> TInfo [DefFH]
transl_let_binds opts fm (S.BDecls decls) st =
  let (st1, defs) = mapTI st (transl_let_patbind opts fm) decls
  in  (st1, concat defs)
transl_let_binds _ _ (S.IPBinds _) _ = error "the parser doesn't support IPBinds"

-- | Translates a let-binding to a list of FL bindings. If the binding has
--   a destructuring pattern, it generates bindings for the inner patterns 
--   and then uses selector functions to get the sub-results, e.g.:
-- 
-- > let (a:(b:(_:xs))) = ...
-- > in  ...
-- 
--   becomes:
-- 
-- > let (a:e1) = ..
-- >     (b:e2) = e1
-- >     (_:xs) = e2
-- > in  ...
-- 
--   and eventually:
-- 
-- > let e0 = ...
-- >     a  = Cons_sel_0 e0
-- >     e1 = Cons_sel_1 e0
-- >     b  = Cons_sel_0 e1
-- >     e2 = Cons_sel_1 e1
-- >     xs = Cons_sel_1 e2
-- > in  ...
-- 
transl_let_patbind :: Options -> MNameF -> S.Decl -> TState -> TInfo [DefFH]
transl_let_patbind opts fm (S.PatBind _ pat _ rhs _) st =
  case pat of
    S.PVar _ ->
      case rhs of
        S.GuardedRhss _ ->
          errM fm  "Guarded RHS in let definition is not supported."
        S.UnGuardedRhs hsExpr ->
          let (st1, defF) = mkDefF opts fm pat hsExpr st
          in  (st1, [defF])
    S.PApp _ _ ->
      case rhs of
        S.GuardedRhss _ ->
          errM fm  "Guarded RHS in let-pattern definition is not supported."
        S.UnGuardedRhs e ->
          let -- generate alias name for the branch expression
              (st1, eName) = freshName st
              (st2, eFL   ) = transl_e opts fm e st1
              eDef = DefF eName [] eFL
              -- translate pattern to FL full pattern
              transl_pat_fl (S.PVar hsn) = FPatV $ QN Nothing (getName hsn)
              transl_pat_fl (S.PWildCard) = FPatV underscoreVar
              transl_pat_fl (S.PApp hsn pps) =
                FPatC (getQName hsn) (map transl_pat_fl pps)
              transl_pat_fl (S.PParen p) = transl_pat_fl p
              transl_pat_fl p = error $ "Unsupported pattern in let: "++(show p)
              -- translate to simple pattern and set of subpattern bindings
              (st3, (patFL, subBnds)) = fp2sp (transl_pat_fl pat) st2
              -- generate resulting simple-pattern destructuring bindings
              bnds = (patFL, eName) : subBnds
              mkBindDefs (SPat c' ps', eName') =
                let mkProjDef (v, i) =
                      DefF v [] (FF (V (QN Nothing (projCName (lName c') i))) 
                                 [XF (V eName')])
                in  map mkProjDef $
                    filter (\(v', _)->v'/=underscoreVar) $ zip ps' [0..]
          in  (st3, eDef : (concatMap mkBindDefs bnds))
    _ -> errM fm $ "let-pattern not supported: "++(show pat)
transl_let_patbind opts fm fbind@(S.FunBind [S.Match {}]) st =
  mapTI st (transl_def opts fm) [fbind]
transl_let_patbind _ fm decl _ = 
  errM fm $ "let-binding not supported: "++(show decl)
              
-- | Translate a full pattern to a simple patern. All full sub-patterns are
--   replaced by new variables and new bindings are returned for them. For
--   example:      
-- 
-- > let (a:(b:(c:xs))) = ...
-- > in  ...
-- 
--   becomes:
-- 
-- > let (a:e1) = ...
-- >     (b:e2) = e1
-- >     (c:xs) = e2
-- > in  ...
-- 
--   so fp2sp (a:(b:(c:xs))) = (a:e, [(b:e2, e1), (c:xs, e2)]).
-- 
fp2sp :: FullPat -> TState -> TInfo (SimplePat, [(SimplePat, QName)])
-- simple variable patterns are illegal in let-expressions here
-- fp2sp pat@(PatF pv@(FPatV _) e) st = (st, [pat])
fp2sp (FPatV _) _ = ierr "fp2sp: found var"
fp2sp (FPatI _) _ = ierr "fp2sp: found int literal"
-- constructor patterns with just variables are not interesting
fp2sp (FPatC c ps) st =
  if all isFPV ps then 
    (st, (SPat c (map (\(FPatV v)->v) ps), []))
  else
    let p2s :: FullPat -> TState -> TInfo (QName, [Maybe (SimplePat, QName)])
        p2s (FPatV v) st' = (st', (v, []))
        p2s (FPatI i) _ = error $ "p2s: found int literal "++(show i)
        p2s (FPatC c0 ps0) st' = 
          let (st'', eName1) = freshName st'
              (st''', newBnds) = mapTI st'' p2s ps0
              -- newBnd :: (QName, Maybe (SimplePat, QName))
              newBnd = (Just (SPat c0 (map fst newBnds), eName1))
          in  (st''', (eName1, newBnd : (concatMap snd newBnds)))
        (st1, ps') = mapTI st p2s ps
        bvars = map fst ps'
        -- newBindings = mapMaybe snd ps'
    in  (st1, (SPat c bvars, catMaybes $ concatMap snd ps'))
              
pv2v :: MNameF -> S.Pat -> QName
pv2v _ (S.PVar (S.Ident hId)) = QN Nothing hId
pv2v fm pv = errM fm $ "unhandled pattern variable: "++(show pv)

isPV :: S.Pat -> Bool
isPV (S.PVar (S.Ident _)) = True
isPV _ = False

simplePats :: MNameF -> [S.Pat] -> [QName]
simplePats fm pats =
  if all isPV pats then
    map (pv2v fm) pats
  else errM fm "lambdas with complex patterns are not supported"

-- | Pattern translation.
transl_pat :: Options -> MNameF -> S.Alt -> TState -> TInfo PatFH
transl_pat opts fm (S.Alt _ hsPat alts _) st =
  case alts of
    S.UnGuardedAlt e ->
      let (st1, eFH) = transl_e opts fm e st
      in  (st1, PatB (mkPat fm hsPat, PatInfo True) eFH)
    _ -> errM fm "transl_pat: can't handle alts"            

-- | Pattern translation.
mkPat :: MNameF -> S.Pat -> FullPat
mkPat fm@(m, _) pat =
  case pat of
    S.PVar bv -> FPatV $ QN (Just m) (getName bv)
    S.PWildCard -> FPatV underscoreVar
    S.PParen pat' -> mkPat fm pat'
    S.PList [] -> FPatC bf_Nil []
    S.PApp (S.Special S.UnitCon) [] -> FPatC bf_Unit []
    S.PApp hsn el -> FPatC (getQName hsn) (map (mkPat fm) el)
    S.PInfixApp e0 (S.Special S.Cons) e1 ->
      FPatC bf_Cons [mkPat fm e0, mkPat fm e1]
    S.PInfixApp e0 hsn e1 ->
      FPatC (getQName hsn) [mkPat fm e0, mkPat fm e1]
    S.PTuple el ->
      let elN = length el
      in  if elN > maxTupleSize then
            error $ "mkPat: tuples with more than "++(show maxTupleSize)++
                    " elements are not supported"
          else
            FPatC (bf_Tuple elN) (map (mkPat fm) el)
    S.PLit (S.Int i) -> FPatI i
    _ -> errM fm $ "Pattern not supported: "++(show pat)

-- | Extracts the name of a Haskell identifier or symbol.
getName :: S.Name -> String
getName (S.Ident hId) = hId
getName (S.Symbol s) = s

-- | Translates a simple Haskell name to an unqualified name.
getQNameN :: S.Name -> QName
getQNameN n = QN Nothing (getName n)

-- | Translates a Haskell name.
getQName :: S.QName -> QName
getQName (S.UnQual n) = getQNameN n
getQName n = error $ "getQName found name "++(show n)

findConstrsOf :: MNameF -> QName -> [Data] -> [DConstr]
findConstrsOf fm dt [] =
  errM fm $ "The constructors of "++(qName dt)++" were not found"
findConstrsOf fm dt ((Data dt' _ dcs):dts) =
  if dt==dt' then dcs else findConstrsOf fm dt dts
  
dcsToNames :: [DConstr] -> [QName]
dcsToNames dcs = map dcName dcs
