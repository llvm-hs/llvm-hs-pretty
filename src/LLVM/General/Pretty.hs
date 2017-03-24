{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module LLVM.General.Pretty (
  ppllvm,
) where

import Prelude hiding ((<$>))
import GHC.Word

import LLVM.General.Typed

import LLVM.General.AST
import LLVM.General.AST.Global
import LLVM.General.AST.Type

import LLVM.General.AST.Attribute
import qualified LLVM.General.AST.Linkage as L
import qualified LLVM.General.AST.Visibility as V
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.FloatingPointPredicate as FP
import qualified LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.AST.AddrSpace as AS
import qualified LLVM.General.AST.Float as F
import LLVM.General.AST.FunctionAttribute

import Data.String

import Text.Printf
import Data.Text.Lazy (Text, pack, unpack)
import Text.PrettyPrint.Leijen.Text

import Data.Char (chr, ord, isControl, isLetter, isDigit)
import Data.List (intersperse)
import Numeric (showHex)

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

commas :: [Doc] -> Doc
commas  = hsep . punctuate (char ',')

colons :: [Doc] -> Doc
colons  = hcat . intersperse (char ':')

hlinecat :: [Doc] -> Doc
hlinecat = vcat . intersperse softbreak

wrapbraces :: Doc -> Doc -> Doc
wrapbraces leadIn x = (leadIn <> char '{') <$> x <$> char '}'

local :: Doc -> Doc
local a = "%" <> a

global :: Doc -> Doc
global a = "@" <> a

label :: Doc -> Doc
label a = "label" <+> "%" <> a

isFunctionPtr (PointerType FunctionType {..} _) = True
isFunctionPtr _ = False

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

class PP p where
  pp :: p -> Doc

ppMaybe (Just x) = pp x
ppMaybe Nothing = empty

instance PP Word32 where
  pp x = int (fromIntegral x)

instance PP Word64 where
  pp x = int (fromIntegral x)

instance PP Integer where
  pp = integer

instance PP Name where
  pp (Name []) = dquotes empty
  pp (Name name@(first:_))
    | isFirst first && all isRest name = text (pack name)
    | otherwise = dquotes . hcat . map escape $ name
    where
        isFirst c = isLetter c || c == '-' || c == '_'
        isRest c = isDigit c || isFirst c
  pp (UnName x) = int (fromIntegral x)

instance PP Parameter where
  pp (Parameter ty name attrs) = pp ty <+> local (pp name)

instance PP ([Parameter], Bool) where
  pp (params, False) = commas (fmap pp params)

instance PP (Operand, [ParameterAttribute]) where
  pp (op, attrs) = ppTyped op

instance PP Type where
  pp (IntegerType width) = "i" <> pp width
  pp (FloatingPointType 16    IEEE) = "half"
  pp (FloatingPointType 32    IEEE) = "float"
  pp (FloatingPointType 64    IEEE) = "double"
  pp (FloatingPointType width IEEE) = "fp" <> pp width
  pp (FloatingPointType width DoubleExtended) = "x86_fp" <> pp width
  pp (FloatingPointType width PairOfFloats)   = "ppc_fp" <> pp width

  pp VoidType = "void"
  pp (PointerType ref@(FunctionType {}) addr) = pp ref
  pp (PointerType ref addr) = pp ref <> "*"
  pp ft@(FunctionType {..}) = pp resultType <+> ppFunctionArgumentTypes ft
  pp (VectorType {..}) = "<" <> pp nVectorElements <+> "x" <+> pp elementType <> ">"
  pp (StructureType {..}) = if isPacked
                               then "<{" <> (commas $ fmap pp elementTypes ) <> "}>"
                               else  "{" <> (commas $ fmap pp elementTypes ) <> "}"
  pp (ArrayType {..}) = brackets $ pp nArrayElements <+> "x" <+> pp elementType
  pp (NamedTypeReference name) = "%" <> pp name

instance PP Global where
  pp (Function {..}) =
      case basicBlocks of
        [] ->
          ("declare" <+> pp returnType <+> global (pp name) <> ppParams (pp . typeOf) parameters)

        -- single unnamed block is special cased, and won't parse otherwise... yeah good times
        [b@(BasicBlock (UnName _) _ _)] ->
            ("define" <+> pp returnType <+> global (pp name) <> ppParams pp parameters)
            `wrapbraces` (indent 2 $ ppSingleBlock b)

        bs ->
          ("define" <+> pp returnType <+> global (pp name) <> ppParams pp parameters)
           `wrapbraces` (vcat $ fmap pp bs)

  pp (GlobalVariable {..}) = global (pp name) <+> "=" <+> "global" <+> pp type' <+> ppMaybe initializer


instance PP Definition where
  pp (GlobalDefinition x) = pp x
  pp (TypeDefinition nm ty) = pp nm
  pp (FunctionAttributes gid attrs) = "attributes" <+> pp gid <+> "=" <+> braces (hsep (fmap pp attrs))
  pp (NamedMetadataDefinition nm meta) = text (pack nm)
  pp (MetadataNodeDefinition node meta) = pp node

instance PP FunctionAttribute where
  pp x = case x of
   NoReturn            -> "noreturn"
   NoUnwind            -> "nounwind"
   ReadNone            -> "readnone"
   ReadOnly            -> "readonly"
   NoInline            -> "noinline"
   AlwaysInline        -> "alwaysinline"
   MinimizeSize        -> "minimizesize"
   OptimizeForSize     -> "optimizeforsize"
   OptimizeNone        -> "optimizenone"
   StackProtect        -> "stackprotect"
   StackProtectReq     -> "stackprotectreq"
   StackProtectStrong  -> "stackprotectstrong"
   NoRedZone           -> "noredzone"
   NoImplicitFloat     -> "noimplicitfloat"
   Naked               -> "naked"
   InlineHint          -> "inlinehint"
   StackAlignment n    -> "stackalign"
   ReturnsTwice        -> "TODO"
   UWTable             -> "uwtable"
   NonLazyBind         -> "TODO"
   Builtin             -> "TODO"
   NoBuiltin           -> "TODO"
   Cold                -> "TODO"
   JumpTable           -> "TODO"
   NoDuplicate         -> "TODO"
   SanitizeAddress     -> "TODO"
   SanitizeThread      -> "TODO"
   SanitizeMemory      -> "TODO"
   StringAttribute k v -> dquotes (text (pack k)) <> "=" <> dquotes (text (pack v))

instance PP MetadataNodeID where
  pp (MetadataNodeID x) = "#" <> int (fromIntegral x)

instance PP GroupID where
  pp (GroupID x) = "#" <> int (fromIntegral x)

instance PP BasicBlock where
  pp (BasicBlock nm instrs term) =
    pp nm <> ":" <$> indent 2 (vcat $ (fmap pp instrs) ++ [pp term])

instance PP Terminator where
  pp (Br dest meta) = "br" <+> label (pp dest)
  pp (Ret val meta) = "ret" <+> maybe "void" ppTyped val
  pp (CondBr cond tdest fdest meta) =
      "br" <+> ppTyped cond
    <> "," <+> label (pp tdest)
    <> "," <+> label (pp fdest)

instance PP Instruction where
  pp x = case x of
    Mul {..}    -> "mul" <+> ppTyped operand0 <> "," <+> pp operand1
    Add {..}    -> "add" <+> ppTyped operand0 <> "," <+> pp operand1
    Sub {..}    -> "sub" <+> ppTyped operand0 <> "," <+> pp operand1
    FSub {..}   -> "fsub" <+> ppTyped operand0 <> "," <+> pp operand1
    FMul {..}   -> "fmul" <+> ppTyped operand0 <> "," <+> pp operand1

    FAdd {..}   -> "fadd" <+> ppTyped operand0 <> "," <+> pp operand1
    FCmp {..}   -> "fcmp" <+> pp fpPredicate <+> ppTyped operand0 <> "," <+> pp operand1

    Alloca {..} -> "alloca" <+> pp allocatedType
    Store {..}  -> "store" <+> ppTyped value <> "," <+> ppTyped address
    Load {..}   -> "load" <+> ppTyped address
    Phi {..}    -> "phi" <+> pp type' <+> commas (fmap phiIncoming incomingValues)

    ICmp {..}   -> "icmp" <+> pp iPredicate <+> ppTyped operand0 <> "," <+> pp operand1

    Call {..}   -> ppCall x
    Select {..} -> "select" <+> pp condition' <+> pp trueValue <+> pp falseValue
    SExt {..}   -> "sext" <+> ppTyped operand0 <+> "to" <+> pp type'
    Trunc {..}  -> "sext" <+> ppTyped operand0 <+> "to" <+> pp type'

    GetElementPtr {..} -> "getelementptr" <+> bounds inBounds <+> commas (fmap ppTyped (address:indices))

    x -> error (show x)

    where
      bounds True = "inbounds"
      bounds False = empty

instance PP CallableOperand where
  pp (Left asm) = error "CallableOperand"
  pp (Right op) = pp op

instance PP Operand where
  pp (LocalReference _ nm) = local (pp nm)
  pp (ConstantOperand con) = pp con


instance PP C.Constant where
  pp (C.Int width val) = pp val
  pp (C.Float (F.Double val)) = text $ pack $ printf "%6.6e" val
  pp (C.Float (F.Single val)) = text $ pack $ printf "%6.6e" val
  pp (C.GlobalReference ty nm) = "@" <> pp nm

  pp C.Array {..}
    | memberType == (IntegerType 8) = "c" <> (dquotes $ hcat [ppIntAsChar val | C.Int _ val <- memberValues])
    | otherwise = brackets $ commas $ fmap ppTyped memberValues


  pp C.GetElementPtr {..} = "getelementptr" <+> bounds inBounds <+> parens (commas (fmap ppTyped (address:indices)))
    where
      bounds True = "inbounds"
      bounds False = empty

instance PP a => PP (Named a) where
  pp (nm := a) = "%" <> pp nm <+> "=" <+> pp a
  pp (Do a) = pp a

instance PP Module where
  pp Module {..} =
    let header = printf "; ModuleID = '%s'" moduleName in
    hlinecat (fromString header : (fmap pp moduleDefinitions))

instance PP FP.FloatingPointPredicate where
  pp op = case op of
   FP.False -> "false"
   FP.OEQ   -> "oeq"
   FP.OGT   -> "ogt"
   FP.OGE   -> "oge"
   FP.OLT   -> "olt"
   FP.OLE   -> "ole"
   FP.ONE   -> "one"
   FP.ORD   -> "ord"
   FP.UEQ   -> "ueq"
   FP.UGT   -> "ugt"
   FP.UGE   -> "uge"
   FP.ULT   -> "ult"
   FP.ULE   -> "ule"
   FP.UNE   -> "une"
   FP.UNO   -> "uno"
   FP.True  -> "true"

instance PP IP.IntegerPredicate where
  pp op = case op of
   IP.EQ  -> "eq"
   IP.NE  -> "ne"
   IP.UGT -> "ugt"
   IP.UGE -> "uge"
   IP.ULT -> "ult"
   IP.ULE -> "ule"
   IP.SGT -> "sgt"
   IP.SGE -> "sge"
   IP.SLT -> "slt"
   IP.SLE -> "sle"

-------------------------------------------------------------------------------
-- Special Case Hacks
-------------------------------------------------------------------------------

escape :: Char -> Doc
escape '"'  = "\\\""
escape '\\' = "\\\\"
escape c    = if isControl c
              then "\\" <> hex c
              else char c
    where
        hex :: Char -> Doc
        hex = pad0 . ($ []) . showHex . ord
        pad0 :: String -> Doc
        pad0 [] = "00"
        pad0 [x] = "0" <> char x
        pad0 xs = text (pack xs)

ppIntAsChar :: Integral a => a -> Doc
ppIntAsChar = escape . chr . fromIntegral


-- print an operand and its type
ppTyped :: (PP a, Typed a) => a -> Doc
ppTyped a = pp (typeOf a) <+> pp a

phiIncoming :: (Operand, Name) -> Doc
phiIncoming (op, nm) = brackets (pp op <> "," <+> (local (pp nm)))

ppParams :: (a -> Doc) -> ([a], Bool) -> Doc
ppParams ppParam (ps, varrg) = parens . commas $ fmap ppParam ps ++ vargs
    where
        vargs = if varrg then ["..."] else []

ppFunctionArgumentTypes :: Type -> Doc
ppFunctionArgumentTypes FunctionType {..} = ppParams pp (argumentTypes, isVarArg)

ppCall :: Instruction -> Doc
ppCall Call { function = Right f,..}
  = tail <+> "call" <+> pp resultType <+> ftype <+> pp f <> parens (commas $ fmap pp arguments)
    where
      tail = if isTailCall tailCallKind then "tail" else empty
      (functionType@FunctionType {..}) = referencedType (typeOf f)
      ftype = if isVarArg || isFunctionPtr resultType
              then ppFunctionArgumentTypes functionType <> "*"
              else empty
      referencedType (PointerType t _) = referencedType t
      referencedType t                 = t

      -- xxx: tail call kind hack
      isTailCall Nothing = False
      isTailCall (Just a) = True

ppCall x = error (show x)

ppSingleBlock :: BasicBlock -> Doc
ppSingleBlock (BasicBlock nm instrs term) = (vcat $ (fmap pp instrs) ++ [pp term])

ppllvm :: Module -> Text
ppllvm = displayT . renderPretty 0.4 100 . pp
