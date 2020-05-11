{-# Options -Wall -Wname-shadowing -Wno-deprecations #-}
{-# LANGUAGE FlexibleInstances #-}

module TypeChecker where


import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.Error
import Data.Maybe


import Definitions
import Printer







--------------------------------------------- TYPES AND STRUCTURES ------------------------------------------------

class Types a where
    freeVars :: a -> S.Set String
    apply :: Subst -> a -> a

instance Types Type where
    -- Function returns free type variables
    freeVars (TypeVar n) = S.singleton n
    freeVars TypeInt = S.empty
    freeVars TypeBool = S.empty
    freeVars (TypeList typ) = freeVars typ
    freeVars (TypeFunc t1 t2) = freeVars t1 `S.union` freeVars t2

    -- Function applies given substitutions to given type.
    apply s (TypeVar n) = fromMaybe (TypeVar n) (M.lookup n s)
    apply s (TypeFunc t1 t2) = TypeFunc (apply s t1) (apply s t2)
    apply s (TypeList t) = TypeList $ apply s t
    apply _ t = t



instance Types Scheme where
    freeVars (Scheme vars t) = freeVars t `S.difference` S.fromList vars
    apply s (Scheme vars t) = Scheme vars (apply (foldr M.delete s vars) t)

instance Types a => Types [a] where
    apply s  =  map (apply s)
    freeVars = foldr (S.union . freeVars ) S.empty



-- Type (map) storeing substitutions for given
-- polymorphic type
type Subst = M.Map String Type

-- Return empty substitution
nullSubst :: Subst
nullSubst  =   M.empty

-- Composes two substitutions in one
compose :: Subst -> Subst -> Subst
compose s1 s2   = M.map (apply s1) s2 `M.union` s1



type TypeEnv = M.Map String Scheme

instance Types TypeEnv where
    freeVars env = freeVars (M.elems env)
    apply s = M.map (apply s)

generalize :: TypeEnv -> Type -> Scheme
generalize env t =
    let vars = S.toList (freeVars t `S.difference` freeVars env)
    in Scheme vars t


data Scheme  =  Scheme [String] Type

data TIEnv = TIEnv {}

type TypeM = ErrorT String (ReaderT TIEnv (State Int))






--------------------------------------------- TYPE INFERENCE FUNCTIONS ------------------------------------------------

-- Gives new char (eg. a01, a02 ...) for polimorphic types
newTyVar :: String -> TypeM Type
newTyVar prefix = do
    s <- state (\n -> (n, n + 1))
    return (TypeVar (prefix ++ show s))



-- substitute all forall vars with fresh ones
instantiate :: Scheme -> TypeM Type
instantiate (Scheme vars t) = do
    nvars <- mapM (\ _ -> newTyVar "scheme") vars
    let s = M.fromList (zip vars nvars)
    return (apply s t)



-- Function used for type unification. It returns
-- substitutions, which needs to be done in order to
-- match two given types.
unify :: FilePosition -> Type -> Type -> TypeM Subst
unify pos (TypeFunc l r) (TypeFunc l' r') = do
    s1 <- unify pos l l'
    s2 <- unify pos (apply s1 r) (apply s1 r')
    return (s1 `compose` s2)
unify pos (TypeVar u) t = varBind pos u t
unify pos t (TypeVar u) = varBind pos u t
unify _ TypeInt TypeInt = return nullSubst
unify _ TypeBool TypeBool = return nullSubst
unify pos (TypeList t1) (TypeList t2) = unify pos t1 t2
unify pos t1 t2 = throwError $ (addPosToError pos) ++ 
                            "Given types do not unify: type " 
                            ++ show t1 ++ " vs expected: " ++ show t2



-- Function tries to bind variables
varBind :: FilePosition -> String -> Type -> TypeM Subst
varBind pos u t | t == TypeVar u = return nullSubst
                | u `S.member` freeVars t = 
                    throwError $ (addPosToError pos) ++ 
                        "Occurs check fails: " ++ u ++ " vs. " ++ show t
                | otherwise = return (M.singleton u t)



-- Function gives new types names for primitive functions.
-- Since their types ("a") are strictly coded (file BuiltinPrimiFuncs.hs)
-- we need to give each primitive different "a". Otherwise they are 
-- "ruining each one's substitutions".
type PolyMap = (M.Map String Type)
getNewPolymorphicNames :: PolyMap -> Type -> TypeM (PolyMap, Type)
getNewPolymorphicNames m (TypeVar v) = do
    case M.lookup v m of
        Nothing -> do
            polyType <- newTyVar "var"
            return (M.insert v polyType m, polyType)
        Just polyType -> return (m, polyType)
getNewPolymorphicNames m (TypeList l) = do
    (newMap, newL) <- getNewPolymorphicNames m l
    return (newMap, TypeList newL)
getNewPolymorphicNames m (TypeFunc t1 t2) = do
    (t1Map, newt1) <- getNewPolymorphicNames m t1
    (t2Map, newt2) <- getNewPolymorphicNames t1Map t2
    return (t2Map, TypeFunc newt1 newt2)
getNewPolymorphicNames m t = return (m, t)



-- Type inference function. Inferences types of given
-- programm elements. For given parse tree it returns
-- inferenced type and all substitutions made during
-- inference process.
inferType :: TypeEnv -> ParseTree -> TypeM (Subst, Type)
inferType env (TData pos l) = case l of
    DInt _ -> return (nullSubst, TypeInt)
    DBool _ -> return (nullSubst, TypeBool)
    DList [] -> do
        typeName <- newTyVar "a"
        return (nullSubst, TypeList typeName)
    DList (h:t) -> do
        (firstsSub, firstType) <- inferType env h
        newSub <- (foldM (\sub nextType -> do
                    (nextTypeSub, newNextType) <- inferType (apply sub env) nextType
                    --traceM $ " newNextType = " ++ show newNextType
                    --traceM $ " newNextType z apply = " ++ show (apply sT' newNextType)
                    --traceM $ " typefirstType z apply = " ++ show (apply sT' firstType)
                    allSubs <- unify pos (apply nextTypeSub newNextType) 
                                    (apply nextTypeSub firstType)
                    return (sub `compose` allSubs))
                firstsSub
                t)
        return (newSub, TypeList (apply newSub firstType))
    DPrimi (PrimitiveFunc _ t _ _) -> do
        (_, newType) <- getNewPolymorphicNames M.empty t
        return (nullSubst, newType)
    _ -> undefined
inferType env (TVar pos n) =
    case M.lookup n env of
        Nothing -> 
            throwError $ (addPosToError pos) ++ 
             "Could't bind given varible. Unknown identifier: " ++ n
        Just poly -> do
            t <- instantiate poly
            return (nullSubst, t)
inferType env (TFAppl pos e1 e2) = do
    polyType <- newTyVar "ret"
    (s1, t1) <- inferType env e1
    (s2, t2) <- inferType (apply s1 env) e2
    s3 <- unify pos (apply s2 t1) (TypeFunc t2 polyType)
    return (s3 `compose` s2 `compose` s1, apply s3 polyType)
inferType env (TFunc _ n e) = do
    polyType <- newTyVar "arg"
    let newEnv = M.insert n (Scheme [] polyType) env
    (s1, t1) <- inferType newEnv e
    return (s1, TypeFunc (apply s1 polyType) t1)



-- Function adds new declarations types to types environment.
addToEnv :: TypeEnv -> (String, ParseTree) -> TypeM TypeEnv
addToEnv env (n, _) = do
    polyType <- newTyVar "a"
    return (M.insert n (Scheme [] polyType) env)



-- Function tries to infer type of given program declaration.
infereDeclarations :: (Subst, TypeEnv) -> (String, ParseTree) -> TypeM (Subst, TypeEnv)
infereDeclarations (sub, env) (name, tree) = do
    (sub2, t) <- inferType env tree
    let newEnv = apply sub2 env
    polyType <- instantiate (fromJust (M.lookup name newEnv))
    sub3 <- unify (getPos tree) t polyType
    return (sub3 `compose` sub2 `compose` sub, apply sub3 newEnv)



-- Function generalizes given program declaration.
generalizeDeclaration :: (TypeEnv, TypeEnv) -> (String, ParseTree) -> TypeM (TypeEnv, TypeEnv)
generalizeDeclaration (acc, env) (name, _) = do
    polyType <- instantiate (fromJust (M.lookup name acc))
    let genRes = generalize env polyType
    return (M.insert name genRes acc, env)



-- Function prepears types environment and all substitutions done during
-- adding all declarations to types environment.
getEnvAndSubst :: TypeEnv -> [(String, ParseTree)] -> TypeM (TypeEnv, Subst)
getEnvAndSubst env decls = do
    newEnv <- foldM addToEnv env decls
    (sub, envInfer) <- foldM infereDeclarations (nullSubst, newEnv) decls
    (envGen, _) <- foldM generalizeDeclaration (envInfer, apply sub env) decls
    return (envGen, sub)



-- Function returns all declarations (def and fun instructions)
-- which occure in program. 
-- Return list contains paris: (func or var name, its body).
getProgDeclarations :: Programm -> [(String, ParseTree)]
getProgDeclarations [] = []
getProgDeclarations ((PEExpr _):rest) = getProgDeclarations rest
getProgDeclarations ((PEDef _ n t):rest) = (n, t) : getProgDeclarations rest
getProgDeclarations ((PEFunc _ n t):rest) = (n, t) : getProgDeclarations rest



-- Function calls type inference function on each program
-- expression. Each time it is called in environment with
-- applied declarations substitutions.
inferenceProgElem :: TypeEnv -> ProgElem -> TypeM TypeEnv
inferenceProgElem env (PEDef _ _ _) = return env
inferenceProgElem env (PEFunc _ _ _) = return env
inferenceProgElem env (PEExpr tree) = do
    (_, _) <- inferType env tree
    return env



-- Main typechecker function
typeCheck :: Programm -> TypeM ()
typeCheck program =
    let decls = getProgDeclarations program
    in do
        (env, sub) <- getEnvAndSubst M.empty decls
        foldM_ inferenceProgElem (apply sub env) program


