{-# LANGUAGE TemplateHaskell #-}
module Category.Functor.Foldable.TH where

import Data.Char (GeneralCategory (..), generalCategory)
import Data.Maybe (fromMaybe, isNothing)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Prelude

-- Copied from recursion-schemes
isPuncChar :: Char -> Bool
isPuncChar c = c `elem` ",;()[]{}`"

isSymbolChar :: Char -> Bool
isSymbolChar c = not (isPuncChar c) && case generalCategory c of
    MathSymbol              -> True
    CurrencySymbol          -> True
    ModifierSymbol          -> True
    OtherSymbol             -> True
    DashPunctuation         -> True
    OtherPunctuation        -> c `notElem` "'\""
    ConnectorPunctuation    -> c /= '_'
    _                       -> False

fName :: Name -> Name
fName orig = mkName (f (nameBase orig))
    where f name | isInfixName name = name ++ "$"
                 | otherwise        = name ++ "F"

          isInfixName :: String -> Bool
          isInfixName = all isSymbolChar

unBndr :: TyVarBndr -> Kind
unBndr (KindedTV _ k) = k
unBndr _ = error "Automatically generated variable binders should have kind annotations."

-- | For a type `MyType (a :: i) (b :: j) :: k -> l -> Type`,
-- create a base functor `MyTypeF (a :: i) (b :: j) (r :: k -> l -> Type) :: k -> l -> Type`
-- such that `Fix (MyTypeF a b) c d` is isomorphic to `MyType a b c d`.
makeBaseFunctor :: Name -> Int -> DecsQ
makeBaseFunctor refName pos = do
    -- The name for the variable for instances of recursion.
    recName <- newName "r"
    TyConI (DataD ctx origName varBndrs kind' origCons _) <- reify refName
    let newN = fName origName
    let actualVars = take pos varBndrs
    let kind = if pos == length varBndrs && isNothing kind'
        then Nothing
        else Just (foldr (\ty tys -> AppT (AppT ArrowT (unBndr ty)) tys) (fromMaybe StarT kind') (drop pos varBndrs))
    -- The kind of the recursive variable should be the same as the kind of the original data type.
    -- Similarly, the kind of the new datatype is the same as the kind of the old.
    let recBndr = maybe (KindedTV recName StarT) (KindedTV recName) kind
    pure [DataD ctx newN (actualVars ++ [recBndr]) kind (map (ForallC [recBndr] [] . makeBaseConstructor (length varBndrs - pos) origName recName (concatMap conn origCons)) origCons) []]
  where
    conn :: Con -> [Name]
    conn (NormalC n _) = [n]
    conn (RecC n _) = [n]
    conn (InfixC _ n _) = [n]
    conn (ForallC _ _ con) = conn con
    conn (GadtC ns _ _) = ns
    conn (RecGadtC ns _ _) = ns

    makeBaseType :: Name -> Name -> [Name] -> Type -> Type
    makeBaseType orig recn conns = makeBaseType'
      where
        makeBaseType' (ForallT vars ctx ty) = ForallT vars (map makeBaseType' ctx) (makeBaseType' ty)
        makeBaseType' (ForallVisT vars ty) = ForallVisT vars (makeBaseType' ty)
        makeBaseType' (AppT t1 t2) = AppT (makeBaseType' t1) (makeBaseType' t2)
        makeBaseType' (AppKindT ty kn) = AppKindT (makeBaseType' ty) (makeBaseType' kn)
        makeBaseType' (SigT ty kn) = SigT (makeBaseType' ty) (makeBaseType' kn)
        makeBaseType' (VarT name) = VarT name
        makeBaseType' (ConT name)
            | name == orig = VarT recn
            | otherwise = ConT name
        makeBaseType' (PromotedT name)
            | name `elem` conns = PromotedT (fName name)
            | otherwise = PromotedT name
        makeBaseType' (InfixT ty1 name ty2) = InfixT (makeBaseType' ty1) name (makeBaseType' ty2)
        makeBaseType' (UInfixT ty1 name ty2) = UInfixT (makeBaseType' ty1) name (makeBaseType' ty2)
        makeBaseType' (ParensT ty) = ParensT (makeBaseType' ty)
        makeBaseType' o@(TupleT _) = o
        makeBaseType' o@(UnboxedTupleT _) = o
        makeBaseType' o@(UnboxedSumT _) = o
        makeBaseType' ArrowT = ArrowT
        makeBaseType' EqualityT = EqualityT
        makeBaseType' ListT = ListT
        makeBaseType' o@(PromotedTupleT _) = o
        makeBaseType' PromotedNilT = PromotedNilT
        makeBaseType' PromotedConsT = PromotedConsT
        makeBaseType' StarT = StarT
        makeBaseType' ConstraintT = ConstraintT
        makeBaseType' o@(LitT _) = o
        makeBaseType' WildCardT = WildCardT
        makeBaseType' (ImplicitParamT s ty) = ImplicitParamT s (makeBaseType' ty)

    makeBaseBangType :: Name -> Name -> [Name] -> BangType -> BangType
    makeBaseBangType orig recn conns (bng, ty) = (bng, makeBaseType orig recn conns ty)

    makeBaseVarBangType :: Name -> Name -> [Name] -> VarBangType -> VarBangType
    makeBaseVarBangType orig recn conns (var, bng, ty) = (fName var, bng, makeBaseType orig recn conns ty)

    fixGadtTy :: Int -> Name -> Name -> [Name] -> Name -> Type -> Type
    fixGadtTy 0   orig recn conns name o          = AppT (fixGadtTy (-1)      orig recn conns name o) (VarT recn)
    fixGadtTy pos orig recn conns name (AppT a b) = AppT (fixGadtTy (pos - 1) orig recn conns name a) (makeBaseType orig recn conns b)
    fixGadtTy _   _    _    _     name _          = ConT (fName name)

    makeBaseConstructor :: Int -> Name -> Name -> [Name] -> Con -> Con
    makeBaseConstructor pos orig recn conns (NormalC name tys) = NormalC (fName name) (map (makeBaseBangType orig recn conns) tys)
    makeBaseConstructor pos orig recn conns (RecC name tys) = RecC (fName name) (map (makeBaseVarBangType orig recn conns) tys)
    makeBaseConstructor pos orig recn conns (InfixC ty1 name ty2) = InfixC (makeBaseBangType orig recn conns ty1) (fName name) (makeBaseBangType orig recn conns ty2)
    makeBaseConstructor pos orig recn conns (ForallC vars ctx con) = ForallC vars (map (makeBaseType orig recn conns) ctx) (makeBaseConstructor pos orig recn conns con)
    makeBaseConstructor pos orig recn conns (GadtC names tys ty) = GadtC (map fName names) (map (makeBaseBangType orig recn conns) tys) (fixGadtTy pos orig recn conns orig ty)
    makeBaseConstructor pos orig recn conns (RecGadtC names tys ty) = RecGadtC (map fName names) (map (makeBaseVarBangType orig recn conns) tys) (fixGadtTy pos orig recn conns orig ty)
