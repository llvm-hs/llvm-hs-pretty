{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module LLVM.Pretty (
  ppllvm,
) where

import Prelude hiding ((<$>))
import GHC.Word

import LLVM.Typed

import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.Type

import LLVM.AST.Attribute
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Visibility as V
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.AddrSpace as AS
import qualified LLVM.AST.Float as F
import LLVM.AST.FunctionAttribute

import Data.String
import qualified Data.ByteString.Short as SBF
import qualified Data.ByteString.Char8 as BF
import Data.ByteString.Internal(w2c)

import Data.Text.Format
import Text.Printf
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding
import Text.PrettyPrint.Leijen.Text

import Data.Char (chr, ord, isAscii, isControl, isLetter, isDigit)
import Data.List (intersperse)
import Data.Maybe (isJust)
import Numeric (showHex)
import Debug.Trace

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

spacedbraces :: Doc -> Doc
spacedbraces x = char '{' <+> x <+> char '}'

local :: Doc -> Doc
local a = "%" <> a

global :: Doc -> Doc
global a = "@" <> a

label :: Doc -> Doc
label a = "label" <+> "%" <> a

isFunctionPtr (PointerType FunctionType {..} _) = True
isFunctionPtr _ = False

cma :: Doc -> Doc -> Doc -- <,> does not work :(
a `cma` b = a <> "," <+> b

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
  pp (Name name)| SBF.null name = dquotes empty
                | isFirst (w2c $ SBF.index name 0) && BF.all isRest (SBF.fromShort name) = (textStrict . decodeShortUtf8) name
                | otherwise = dquotes . hcat . BF.foldl (\acc c -> acc ++ [escape c]) [] $ (SBF.fromShort name)
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
  pp (FloatingPointType HalfFP) = "half"
  pp (FloatingPointType FloatFP) = "float"
  pp (FloatingPointType DoubleFP) = "double"
  pp (FloatingPointType FP128FP) = "fp128"
  pp (FloatingPointType X86_FP80FP) = "x86_fp80"
  pp (FloatingPointType PPC_FP128FP)   = "ppc_fp128"

  pp VoidType = "void"
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
          ("declare" <+> pp linkage <+> pp returnType <+> global (pp name) <> ppParams (pp . typeOf) parameters)

        -- single unnamed block is special cased, and won't parse otherwise... yeah good times
        [b@(BasicBlock (UnName _) _ _)] ->
            ("define" <+> pp linkage <+> pp returnType <+> global (pp name) <> ppParams pp parameters)
            `wrapbraces` (indent 2 $ ppSingleBlock b)

        bs ->
          ("define" <+> pp linkage <+> pp returnType <+> global (pp name) <> ppParams pp parameters)
           `wrapbraces` (vcat $ fmap pp bs)

  pp (GlobalVariable {..}) = global (pp name) <+> "=" <+> ppLinkage hasInitializer linkage <+> kind <+> pp type' <+> ppMaybe initializer
    where
      hasInitializer = isJust initializer
      kind | isConstant = "constant"
           | otherwise  = "global"

  pp (GlobalAlias {..}) = global (pp name) <+> "=" <+> pp linkage <+> "alias" <+> pp typ `cma` ppTyped aliasee
    where
      typ = getElementType type'

decodeShortUtf8 = decodeUtf8 . SBF.fromShort

instance PP Definition where
  pp (GlobalDefinition x) = pp x
  pp (TypeDefinition nm ty) = local (pp nm) <+> "=" <+> "type" <+> maybe "opaque" pp ty
  pp (FunctionAttributes gid attrs) = "attributes" <+> pp gid <+> "=" <+> braces (hsep (fmap pp attrs))
  pp (NamedMetadataDefinition nm meta) = textStrict (decodeShortUtf8 nm)
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
   StringAttribute k v -> dquotes (textStrict (decodeShortUtf8 k)) <> "=" <> dquotes (textStrict (decodeShortUtf8 v))

instance PP L.Linkage where
    pp = ppLinkage False

ppLinkage omitExternal x = case x of
   L.External | omitExternal -> empty
              | otherwise    -> "external"
   L.Private                 -> "private"
   L.Internal                -> "internal"
   L.ExternWeak              -> "extern_weak"

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
    `cma` label (pp tdest)
    `cma` label (pp fdest)
  pp (Switch {..}) = "switch" <+> ppTyped operand0'
                   `cma` label (pp defaultDest)
                   <+> brackets (hsep [ ppTyped v `cma` label (pp l) | (v,l) <- dests ])

  pp x = error (show x)

instance PP Instruction where
  pp x = case x of
    Add {..}    -> "add"  <+> ppTyped operand0 `cma` pp operand1
    Sub {..}    -> "sub"  <+> ppTyped operand0 `cma` pp operand1
    Mul {..}    -> "mul"  <+> ppTyped operand0 `cma` pp operand1
    Shl {..}    -> "shl"  <+> ppTyped operand0 `cma` pp operand1
    AShr {..}   -> "ashr" <+> ppTyped operand0 `cma` pp operand1
    And {..}    -> "and"  <+> ppTyped operand0 `cma` pp operand1
    SDiv {..}   -> "sdiv"  <+> ppTyped operand0 `cma` pp operand1
    UDiv {..}   -> "udiv"  <+> ppTyped operand0 `cma` pp operand1
    SRem {..}   -> "srem"  <+> ppTyped operand0 `cma` pp operand1
    URem {..}   -> "urem"  <+> ppTyped operand0 `cma` pp operand1

    FAdd {..}   -> "fadd" <+> ppTyped operand0 `cma` pp operand1
    FSub {..}   -> "fsub" <+> ppTyped operand0 `cma` pp operand1
    FMul {..}   -> "fmul" <+> ppTyped operand0 `cma` pp operand1

    FCmp {..}   -> "fcmp" <+> pp fpPredicate <+> ppTyped operand0 `cma` pp operand1

    Alloca {..} -> "alloca" <+> pp allocatedType <> num <> align
      where num   = case numElements of Nothing -> empty
                                        Just o -> "," <+> ppTyped o
            align | alignment == 0 = empty
                  | otherwise      = "," <+> pp alignment
    Store {..}  -> "store" <+> ppTyped value `cma` ppTyped address
    Load {..}   -> "load" <+> pp argTy `cma` ppTyped address
      where PointerType argTy _ = typeOf address
    Phi {..}    -> "phi" <+> pp type' <+> commas (fmap phiIncoming incomingValues)

    ICmp {..}   -> "icmp" <+> pp iPredicate <+> ppTyped operand0 `cma` pp operand1

    Call {..}   -> ppCall x
    Select {..} -> "select" <+> pp condition' <+> pp trueValue <+> pp falseValue
    SExt {..}   -> "sext" <+> ppTyped operand0 <+> "to" <+> pp type'
    ZExt {..}   -> "zext" <+> ppTyped operand0 <+> "to" <+> pp type'
    Trunc {..}  -> "trunc" <+> ppTyped operand0 <+> "to" <+> pp type'

    GetElementPtr {..} -> "getelementptr" <+> bounds inBounds <+> commas (pp argTy : fmap ppTyped (address:indices))
      where argTy = getElementType $ typeOf address
    ExtractValue {..} -> "extractvalue" <+> commas (ppTyped aggregate : fmap pp indices')

    BitCast {..} -> "bitcast" <+> ppTyped operand0 <+> "to" <+> pp type'
    PtrToInt {..} -> "ptrtoint" <+> ppTyped operand0 <+> "to" <+> pp type'
    IntToPtr {..} -> "inttoptr" <+> ppTyped operand0 <+> "to" <+> pp type'

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
  pp (C.Float (F.Double val)) = textStrict $ pack $ printf "%6.6e" val
  pp (C.Float (F.Single val)) = textStrict $ pack $ printf "%6.6e" val
  pp (C.GlobalReference ty nm) = "@" <> pp nm

  pp (C.Struct _ _ elems) = spacedbraces $ commas $ fmap ppTyped elems
  pp (C.Null {}) = "null"

  pp C.Array {..}
    | memberType == (IntegerType 8) = "c" <> (dquotes $ hcat [ppIntAsChar val | C.Int _ val <- memberValues])
    | otherwise = brackets $ commas $ fmap ppTyped memberValues

  pp C.GetElementPtr {..} = "getelementptr" <+> bounds inBounds <+> parens (commas (pp argTy : fmap ppTyped (address:indices)))
    where
      argTy = getElementType $ typeOf address
      bounds True = "inbounds"
      bounds False = empty

  pp C.BitCast {..} = "bitcast" <+> parens (ppTyped operand0 <+> "to" <+> pp type')
  pp C.PtrToInt {..} = "ptrtoint" <+> parens (ppTyped operand0 <+> "to" <+> pp type')
  pp C.IntToPtr {..} = "inttoptr" <+> parens (ppTyped operand0 <+> "to" <+> pp type')

  pp x = error (show x)

instance PP a => PP (Named a) where
  pp (nm := a) = "%" <> pp nm <+> "=" <+> pp a
  pp (Do a) = pp a

instance PP Module where
  pp Module {..} =
    let header = format "; ModuleID = '{}'" (Only $ decodeShortUtf8 moduleName) in
    hlinecat ((text header) : (fmap pp moduleDefinitions))

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
escape '"'  = "\\22"
escape '\\' = "\\\\"
escape c    = if isAscii c && not (isControl c)
              then char c
              else "\\" <> hex c
    where
        hex :: Char -> Doc
        hex = pad0 . ($ []) . showHex . ord
        pad0 :: String -> Doc
        pad0 [] = "00"
        pad0 [x] = "0" <> char x
        pad0 xs = textStrict (pack xs)

ppIntAsChar :: Integral a => a -> Doc
ppIntAsChar = escape . chr . fromIntegral


-- print an operand and its type
ppTyped :: (PP a, Typed a) => a -> Doc
ppTyped a = pp (typeOf a) <+> pp a

ppCommaTyped :: (PP a, Typed a) => a -> Doc
ppCommaTyped a = pp (typeOf a) `cma` pp a

phiIncoming :: (Operand, Name) -> Doc
phiIncoming (op, nm) = brackets (pp op `cma` (local (pp nm)))

ppParams :: (a -> Doc) -> ([a], Bool) -> Doc
ppParams ppParam (ps, varrg) = parens . commas $ fmap ppParam ps ++ vargs
    where
        vargs = if varrg then ["..."] else []

ppFunctionArgumentTypes :: Type -> Doc
ppFunctionArgumentTypes FunctionType {..} = ppParams pp (argumentTypes, isVarArg)

ppCall :: Instruction -> Doc
ppCall inst@(Call { function = Right f,..})
  = tail <+> "call" <+> pp resultType <+> ftype <+> pp f <> parens (commas $ fmap pp arguments)
    where
      (functionType@FunctionType {..}) = referencedType (typeOf f)
      ftype = if isVarArg || isFunctionPtr resultType
              then ppFunctionArgumentTypes functionType <> "*"
              else empty
      referencedType (PointerType t _) = referencedType t
      referencedType t                 = t

      tail = case tailCallKind of
        Just Tail -> "tail"
        Just MustTail -> "musttail"
        Nothing -> empty

ppCall x = error (show x)

ppSingleBlock :: BasicBlock -> Doc
ppSingleBlock (BasicBlock nm instrs term) = (vcat $ (fmap pp instrs) ++ [pp term])

ppllvm :: Module -> Text
ppllvm = displayTStrict . renderPretty 0.4 100 . pp
