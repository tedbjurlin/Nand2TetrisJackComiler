{-# LANGUAGE GADTs #-}
module Types where
import qualified Data.Map as M
import Data.Map (Map)


data Class where
    C :: Ident -> ClassBody -> Class
    deriving Show

data ClassBody where
    CBody :: [ClassVar] -> [Subroutine] -> ClassBody
    deriving Show

data ClassVar where
    CStatic :: VType -> Ident -> ClassVar
    CField  :: VType -> Ident -> ClassVar
    deriving Show

data VType where
    TInt   :: VType
    TChar  :: VType
    TBool  :: VType
    TArray :: VType
    TClass :: Ident -> VType
    deriving Show

type Ident = String

data Subroutine where
    Constructor :: CType -> Ident -> [Parameter] -> Body -> Subroutine
    Function    :: CType -> Ident -> [Parameter] -> Body -> Subroutine
    Method      :: CType -> Ident -> [Parameter] -> Body -> Subroutine
    deriving Show

data CType where
    Void :: CType
    V    :: VType -> CType
    deriving Show

data Parameter where
    P :: VType -> Ident -> Parameter
    deriving Show

data Body where
    B :: [SVar] -> [Statement] -> Body
    deriving Show

data SVar where
    SVar :: VType -> Ident -> SVar
    deriving Show

data Statement where
    Let      :: Ident -> Expression -> Statement
    LetArray :: Ident -> Expression -> Expression -> Statement
    If       :: Expression -> [Statement] -> Statement
    IfElse   :: Expression -> [Statement] -> [Statement] -> Statement
    While    :: Expression -> [Statement] -> Statement
    Do       :: SubroutineCall -> Statement
    Return   :: Expression -> Statement
    ReturnV  :: Statement
    deriving Show

data BinaryOp = Add | Sub |  Mul |  Div |  And |  Or |  Lt |  Gt |  Eq
    deriving (Show, Eq)

data Expression where
    Binary          :: BinaryOp -> Expression -> Expression -> Expression
    Unary           :: UnaryOp -> Expression -> Expression
    IntegerConstant :: Integer -> Expression
    StringConstant  :: String  -> Expression
    KeywordConstant :: Keyword -> Expression
    Variable        :: Ident -> Expression
    Array           :: Ident -> Expression -> Expression
    TermSubroutine  :: SubroutineCall -> Expression
    deriving Show

data SubroutineCall where
    UnQualified :: Ident -> [Expression] -> SubroutineCall
    Qualified   :: Ident -> Ident -> [Expression] -> SubroutineCall
    deriving Show

data Keyword where
    KTrue  :: Keyword
    KFalse :: Keyword
    KNull  :: Keyword
    KThis  :: Keyword
    deriving Show

data UnaryOp where
    UNeg :: UnaryOp
    UNot :: UnaryOp
    deriving Show

data Kind where
    Field    :: Kind
    Static   :: Kind
    Argument :: Kind
    Var      :: Kind
    deriving Show

type Table = Map String (VType, Kind, Int)

data SymbolTable where
    ST :: Table -> Int -> Int -> SymbolTable
    deriving Show

nTable :: SymbolTable
nTable = ST M.empty 0 0

insertVar :: String -> VType -> Kind -> SymbolTable -> SymbolTable
insertVar k t l (ST st i1 i2) = case l of
    Field    -> ST (M.insert k (t, l, i1) st) (i1+1) i2
    Argument -> ST (M.insert k (t, l, i1) st) (i1+1) i2
    Static   -> ST (M.insert k (t, l, i2) st) i1 (i2+1)
    Var      -> ST (M.insert k (t, l, i2) st) i1 (i2+1)

lookupVar :: SymbolTable -> String -> Maybe (VType, Kind, Int)
lookupVar (ST st _ _) s = M.lookup s st