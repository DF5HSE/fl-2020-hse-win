module PrologAst where

data Token = TIdent String
           | TVar String
           | Comma -- ',' -- conjuction
           | Semi -- semicolon -- ';' -- disjuction
           | Lbr
           | Rbr
           | Dot
           | Cork
           deriving (Eq, Show)

data PrologProgram = Program {
        pModule :: Maybe String
      , types   :: [TypeDef]
      , rels    :: [Relation]
      }

data TypeDef = TypeDef String Type

data Type = Var String
          | TAtom Atom
          | TBr Type
          | Arrow Type Type

data Atom = ID String | IDTAIL String Tail

data Tail = ATOM Atom | ATOMWITHBR AtomBr | ATOMBRTAIL AtomBr Tail | VAR String | VARTAIL String Tail

data AtomBr = ATOMINBR Atom | ATOMBR AtomBr | VARBR String

data Relation = HEAD Atom | BODY Atom RelationBody

data RelationBody = RAtom Atom
                  | Conj RelationBody RelationBody
                  | Disj RelationBody RelationBody
                  | RBodyBr RelationBody







instance Show PrologProgram where
  show pp = showModule (pModule pp) ++ "\n\n" ++ myShowList (types pp) ++ "\n\n" ++ myShowList (rels pp)
showModule Nothing = ""
showModule (Just mod) = "MODULE (" ++ show mod ++ ")"

myShowList [] = ""
myShowList (x:xs) = show x ++ "\n" ++ myShowList xs

instance Show Relation where
  show (HEAD a) = "REL (" ++ show a ++ ")"
  show (BODY a b) = "REL (" ++ show a ++ ") (" ++ show b ++ ")"

instance Show RelationBody where
  show (RAtom a) = show a
  show (Conj l r) = "CONJ (" ++ show l ++ ") (" ++ show r ++ ")"
  show (Disj l r) = "DISJ (" ++ show l ++ ") (" ++ show r ++ ")"
  show (RBodyBr rb) = show rb

instance Show Atom where
  show (ID s) = "ATOM (ID " ++ show s ++ ")"
  show (IDTAIL s t) = "ATOM (ID " ++ show s ++ " " ++ show t ++ ")"

instance Show Tail where
  show (ATOM a) = show a
  show (ATOMWITHBR abr) = show abr
  show (ATOMBRTAIL ab t) = show ab ++ " " ++ show t
  show (VAR v) = "VAR " ++ show v
  show (VARTAIL v t) = "VAR " ++ show v ++ " " ++ show t

instance Show AtomBr where
  show (ATOMINBR a) = show a
  show (ATOMBR abr) = show abr
  show (VARBR v) = "VAR " ++ show v

instance Show TypeDef where
  show (TypeDef s t) = "TYPE " ++ show s ++ " " ++ show t

instance Show Type where
  show (Var s) = "VAR " ++ show s
  show (TAtom a) = show a
  show (TBr br) = show br --"(" ++ show br ++ ")"
  show (Arrow l r) = "ARROW (" ++ show l ++ ") (" ++ show r ++ ")"


