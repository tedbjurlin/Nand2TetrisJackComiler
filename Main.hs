{-# LANGUAGE GADTs #-}
module Main where

import Types
    ( insertVar,
      lookupVar,
      nTable,
      BinaryOp(..),
      Body(..),
      Class(..),
      ClassBody(..),
      ClassVar(..),
      Expression(..),
      Ident,
      Keyword(..),
      Kind(..),
      Parameter(..),
      SVar(..),
      Statement(..),
      Subroutine(..),
      SubroutineCall(..),
      SymbolTable,
      UnaryOp(..),
      VType (TClass) )
import Parser ( parseJack )
import System.FilePath.Posix (dropExtension, (<.>), (</>))
import System.Environment (getArgs)
import System.Directory (doesFileExist, listDirectory)
import Data.Char (ord)

cExpression :: Ident -> SymbolTable -> SymbolTable -> Expression -> String
cExpression cn ct st t = case t of
    (Binary op e1 e2)   -> cExpression cn ct st e1 ++ cExpression cn ct st e2 ++ cBOp op
    (Unary op e)        -> cExpression cn ct st e ++ cUOp op
    (IntegerConstant i) -> "push constant " ++ show i ++ "\n"
    (KeywordConstant k) -> cKeywordConstant k
    (Variable v)        -> case lookupVar ct v of
        Just (_, kind, idx) -> "push " ++ cKind kind ++ " " ++ show idx ++ "\n"
        Nothing -> case lookupVar st v of
            Just (_, kind, idx) -> "push " ++ cKind kind ++ " " ++ show idx ++ "\n"
            Nothing -> error $ "invalid variable " ++ v
    (TermSubroutine sRtn) -> cSubroutineCall cn ct st sRtn
    (StringConstant s) -> cStringConstant s
    (Array idx e) -> cArrayLookup cn ct st idx e

cArrayLookup :: Ident -> SymbolTable -> SymbolTable -> Ident -> Expression -> String
cArrayLookup cn ct st idx e = p
    where
        e' = cExpression cn ct st e
        i1 = case lookupVar ct idx of
            Just (_, kind, idx') -> "push " ++ cKind kind ++ " " ++ show idx' ++ "\n"
            Nothing -> case lookupVar st idx of
                Just (_, kind, idx') -> "push " ++ cKind kind ++ " " ++ show idx' ++ "\n"
                Nothing -> error $ "invalid variable " ++ idx
        p = i1 ++ e' ++ "add\npop pointer 1\npush that 0\n"

cStringConstant :: String -> String
cStringConstant s = "push constant " ++ (show . length) s ++ "\n"
    ++ "call String.new 1\n" ++ concatMap (\c -> "push constant " ++ cCode c ++ "\ncall String.appendChar 2\n") s

cCode :: Char -> String
cCode = show . ord

cKeywordConstant :: Keyword -> String
cKeywordConstant k = case k of
    KTrue -> "push constant 0\nnot\n"
    KFalse -> "push constant 0\n"
    KNull -> "push constant 0\n"
    KThis -> "push pointer 0\n"

cKind :: Kind -> String
cKind Argument = "argument"
cKind Var      = "local"
cKind Static   = "static"
cKind Field    = "this"

cBOp :: BinaryOp -> String
cBOp Add = "add\n"
cBOp Sub = "sub\n"
cBOp Mul = "call Math.multiply 2\n"
cBOp Div = "call Math.divide 2\n"
cBOp And = "and\n"
cBOp Or  = "or\n"
cBOp Lt  = "lt\n"
cBOp Gt  = "gt\n"
cBOp Eq  = "eq\n"

cUOp :: UnaryOp -> String
cUOp UNeg = "neg\n"
cUOp UNot = "not\n"

cSubroutineCall :: Ident -> SymbolTable -> SymbolTable -> SubroutineCall -> String
cSubroutineCall cn ct st (UnQualified n es) = "push pointer 0\n" ++ cExpressionList cn ct st es
    ++ "call " ++ cn ++ "." ++ n ++ " " ++ show (length es + 1) ++ "\n"
cSubroutineCall cn ct st (Qualified c n es) = case lookupVar ct c of
    Just (TClass idx, k, i) -> "push " ++ cKind k ++ " " ++ show i ++ "\n"
        ++ cExpressionList cn ct st es
        ++ "call " ++ idx ++ "." ++ n ++ " " ++ show (length es + 1) ++ "\n"
    _ -> case lookupVar st c of
        Just (TClass idx, k, i) -> "push " ++ cKind k ++ " " ++ show i ++ "\n"
            ++ cExpressionList cn ct st es
            ++ "call " ++ idx ++ "." ++ n ++ " " ++ show (length es + 1) ++ "\n"
        _ -> cExpressionList cn ct st es
            ++ "call " ++ c ++ "." ++ n ++ " " ++ show (length es) ++ "\n"

cExpressionList :: Ident -> SymbolTable -> SymbolTable -> [Expression] -> String
cExpressionList cn ct st = concatMap (cExpression cn ct st)

cClass :: Class -> String
cClass (C idx cb) = cClassBody idx cb

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

cClassBody :: Ident -> ClassBody -> String
cClassBody idx (CBody vars rtns) = snd $ concatMapIS (cSubroutine (length vars) (foldr (uncurry3 insertVar . cClassVar) nTable (reverse vars)) idx) 0 rtns

cClassVar :: ClassVar -> (String, VType, Kind)
cClassVar (CStatic v i) = (i, v, Static)
cClassVar (CField v i)  = (i, v, Field)

cSubroutine :: Int -> SymbolTable -> Ident -> Int -> Subroutine -> (Int, String)
cSubroutine i ct cn lidx (Constructor _ n ps b) =
    let st = foldr (uncurry3 insertVar . cParam) nTable (reverse ps) in
        let (nvars, (lidx', bs)) = cBody cn True lidx ct st b in
            (lidx', "function " ++ cn ++ "." ++ n ++ " " ++ show nvars
                ++ "\npush constant " ++ show i ++ "\ncall Memory.alloc 1\npop pointer 0\n" ++ bs)
cSubroutine _ ct cn lidx (Function _ n ps b) =
    let st = foldr (uncurry3 insertVar . cParam) nTable (reverse ps) in
        let (nvars, (lidx', bs)) = cBody cn False lidx ct st b in
            (lidx', "function " ++ cn ++ "." ++ n ++ " " ++ show nvars ++ "\n" ++ bs)
cSubroutine _ ct cn lidx (Method _ n ps b) =
    let st = foldr (uncurry3 insertVar . cParam) st' (reverse ps) in
        let (nvars, (lidx', bs)) = cBody cn False lidx ct st b in
            (lidx', "function " ++ cn ++ "." ++ n ++ " " ++ show nvars
                ++ "\npush argument 0\npop pointer 0\n" ++ bs)
        where
            st' = insertVar "this" (TClass cn) Argument nTable

cParam :: Parameter -> (String, VType, Kind)
cParam (P t i) = (i, t, Argument)

cBody :: Ident -> Bool -> Int -> SymbolTable -> SymbolTable -> Body -> (Int, (Int, String))
cBody cn b lidx ct st (B vars stmts)= (length vars, concatMapIS (cStatement cn b ct (foldr (uncurry3 insertVar . cSVar) st (reverse vars))) lidx stmts)

cSVar :: SVar -> (String, VType, Kind)
cSVar (SVar t i) = (i, t, Var)

cStatement :: Ident -> Bool -> SymbolTable -> SymbolTable -> Int -> Statement -> (Int, String)
cStatement cn b ct st lidx stmt = case stmt of
    (Let idx e) -> cLet cn ct st lidx idx e
    (LetArray idx e s) -> (lidx, cLetArray cn ct st idx e s)
    (If e stmts) -> cIf cn b ct st lidx e stmts
    (IfElse e stmts1 stmts2) -> cIfElse cn b ct st lidx e stmts1 stmts2
    (While e stmts) -> cWhile cn b ct st lidx e stmts
    (Do sRtn) -> (lidx, cSubroutineCall cn ct st sRtn ++ "pop temp 0\n")
    (Return e) -> (lidx, cExpression cn ct st e ++ "return\n")
    ReturnV -> (lidx, if b
        then "push pointer 0\nreturn\n"
        else "push constant 0\nreturn\n")

cLetArray :: Ident -> SymbolTable -> SymbolTable -> Ident -> Expression -> Expression -> String
cLetArray cn ct st idx e1 e2 = p
    where
        e1' = cExpression cn ct st e1
        e2' = cExpression cn ct st e2
        i1 = case lookupVar ct idx of
            Just (_, kind, idx') -> "push " ++ cKind kind ++ " " ++ show idx' ++ "\n"
            Nothing -> case lookupVar st idx of
                Just (_, kind, idx') -> "push " ++ cKind kind ++ " " ++ show idx' ++ "\n"
                Nothing -> error $ "invalid variable " ++ idx
        p = i1 ++ e1' ++ "add\n" ++ e2' ++ "pop temp 0\npop pointer 1\npush temp 0\npop that 0\n"

cLet :: Ident -> SymbolTable -> SymbolTable -> Int -> Ident -> Expression -> (Int, String)
cLet cn ct st lidx idx e = case lookupVar st idx of
        Just (_, k, i) -> (lidx, ce k i)
        Nothing        -> case lookupVar ct idx of
            Just (_, k, i) -> (lidx, ce k i)
            Nothing -> error $ "invalid variable " ++ idx
        where
            ce k i = cExpression cn ct st e ++ "pop " ++ cKind k ++ " " ++ show i ++ "\n"

cIf :: Ident -> Bool -> SymbolTable -> SymbolTable -> Int -> Expression -> [Statement] -> (Int, String)
cIf cn b ct st lidx e stmts = (lidx', p $ cExpression cn ct st e)
            where
                (lidx', s') = concatMapIS (cStatement cn b ct st) (lidx + 1) stmts
                p e' = e' ++ "if-goto IF_TRUE" ++ show lidx
                    ++ "\ngoto IF_FALSE" ++ show lidx ++ "\nlabel IF_TRUE"
                    ++ show lidx ++ "\n" ++ s' ++ "label IF_FALSE" ++ show lidx ++ "\n"

cIfElse :: Ident -> Bool -> SymbolTable -> SymbolTable -> Int -> Expression -> [Statement] -> [Statement] -> (Int, String)
cIfElse cn b ct st lidx e s1 s2 = (lidx'', p $ cExpression cn ct st e)
            where
                (lidx', s1') = concatMapIS (cStatement cn b ct st) (lidx + 1) s1
                (lidx'', s2') = concatMapIS (cStatement cn b ct st) (lidx' + 1) s2
                p e' = e' ++ "not\nif-goto IF_FALSE$" ++ show lidx ++ "\n"
                    ++ s1' ++ "goto IF_END$" ++ show lidx ++ "\nlabel IF_FALSE$" ++ show lidx
                    ++ "\n" ++ s2' ++ "label IF_END$" ++ show lidx ++ "\n"

cWhile :: Ident -> Bool -> SymbolTable -> SymbolTable -> Int -> Expression -> [Statement] -> (Int, String)
cWhile cn b ct st lidx e s = (lidx', p $ cExpression cn ct st e)
        where
            (lidx', s') = concatMapIS (cStatement cn b ct st) (lidx + 1) s
            p e' = "label WHILE_EXP" ++ show lidx ++ "\n" ++ e' ++ "not\nif-goto WHILE_END"
                ++ show lidx ++ "\n" ++ s' ++ "goto WHILE_EXP" ++ show lidx
                ++ "\nlabel WHILE_END" ++ show lidx ++ "\n"

concatMapIS :: (Int -> a -> (Int, String)) -> Int -> [a] -> (Int, String)
concatMapIS _ i [] = (i, "")
concatMapIS f i [a] = f i a
concatMapIS f i (a:as) = let (i', s') = f i a in
    let (i'', s'') = concatMapIS f i' as in (i'', s'++s'')

process :: String -> IO ()
process filepath = do
    contents <- readFile filepath
    let ast = parseJack contents
    case ast of
        (Left err) -> putStrLn err
        (Right c) -> do
            let results = cClass c
            name <- changeName filepath
            writeFile name results
            putStrLn $ "written file to " ++ name

isJack :: String -> Bool
isJack f = take 5 (reverse f) == "kcaj."

processFromPath :: String -> IO ()
processFromPath filepath = do
  con <- doesFileExist filepath
  if con
    then do
      process filepath
    else do
      files <- listDirectory filepath
      print files
      foldMap (process . (filepath </>)) (filter isJack files)

changeName :: String -> IO String
changeName s = do
    let base = dropExtension s
    return $ base <.> "vm"

main :: IO ()
main = do
    args <- getArgs
    processFromPath $ head args

-- two symbol tables: one global for static and field variables
-- one local for local and argument variables

-- need to store type, kind, & index