{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}

module LLVM.Pretty (
  ppllvm,
  ppll,
) where

import Prelude hiding ((<$>))
import GHC.Word

import LLVM.Pretty.Typed

import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.Type

import LLVM.DataLayout
import LLVM.AST.Attribute
import LLVM.AST.DataLayout
import LLVM.AST.COMDAT
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Visibility as V
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.InlineAssembly as IA
import qualified LLVM.AST.AddrSpace as AS
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.RMWOperation as RMW
import LLVM.AST.Operand hiding (DIGLobalVariable(..), GlobalVariable, Module, NoReturn, PointerType)
import qualified LLVM.AST.Operand as O
import LLVM.AST.ParameterAttribute as PA
import LLVM.AST.FunctionAttribute as FA

import Data.String

import Text.Printf
import Data.Text.Lazy.Encoding
import Data.Text.Lazy (Text, pack, unpack)
import qualified Data.ByteString.Short as SBF
import qualified Data.ByteString.Lazy.Char8 as BF
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Internal (w2c)
import Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc as P
import Data.Text.Prettyprint.Doc.Render.Text

import qualified Data.ByteString.Char8 as BL
import qualified Data.ByteString.Short as BS
import Data.Char (chr, ord, isAscii, isControl, isLetter, isDigit)
import Data.Foldable (toList)
import Data.Int
import Data.List (intersperse)
import Data.Maybe (isJust, mapMaybe)
-- import Data.Monoid ((<>))
import Numeric (showHex)

import Data.Array.Unsafe
import Data.Array.MArray
import Data.Array.ST
import Control.Monad.ST

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

parensIf ::  Bool -> Doc ann -> Doc ann
parensIf True = parens
parensIf False = id

commas :: [Doc ann] -> Doc ann
commas  = hsep . punctuate (pretty ',')

colons :: [Doc ann] -> Doc ann
colons  = hcat . intersperse (pretty ':')

hlinecat :: [Doc ann] -> Doc ann
hlinecat = vcat . intersperse softbreak
  where
    softbreak = group hardline

wrapbraces :: Doc ann -> Doc ann -> Doc ann
wrapbraces leadIn x = (leadIn <> pretty '{') <> line' <> x <> line' <> pretty '}'

angleBrackets :: Doc ann -> Doc ann
angleBrackets x = pretty '<' <> x <> pretty '>'

spacedbraces :: Doc ann -> Doc ann
spacedbraces x = pretty '{' <+> x <+> pretty '}'

local' :: Doc ann -> Doc ann
local' a = "%" <> a

global :: Doc ann -> Doc ann
global a = "@" <> a

label :: Doc ann -> Doc ann
label a = "label" <+> "%" <> a

cma :: Doc ann -> Doc ann -> Doc ann -- <,> does not work :(
a `cma` b = a <> "," <+> b

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

-- class Pretty p where
--   pretty :: p -> Doc

ppMaybe :: Pretty a => Maybe a -> Doc ann
ppMaybe (Just x) = pretty x
ppMaybe Nothing = mempty

ppBool :: Doc ann -> Bool -> Doc ann
ppBool x True = x
ppBool x False = mempty

-- XXX: horrible hack
unShort :: BS.ShortByteString -> [Char]
unShort xs = fmap (toEnum . fromIntegral) $ BS.unpack xs

short :: BS.ShortByteString -> Doc ann
short x = pretty (pack (unShort x))

decodeShortUtf8 :: SBF.ShortByteString -> Text
decodeShortUtf8 = decodeUtf8 . fromStrict . SBF.fromShort

-- instance Pretty Word8 where
--   pretty x = int (fromIntegral x)

-- instance Pretty Word16 where
--   pretty x = int (fromIntegral x)

-- instance Pretty Word32 where
--   pretty x = int (fromIntegral x)

-- instance Pretty Word64 where
--   pretty x = int (fromIntegral x)

-- instance Pretty Int32 where
--   pretty x = int (fromIntegral x)

-- instance Pretty Int64 where
--   pretty x = int (fromIntegral x)

-- instance Pretty Integer where
--   pretty = integer

instance Pretty BS.ShortByteString where
  pretty = pretty . unShort

-- instance Pretty [Char] where
--   pretty = text . pack

-- instance Pretty Bool where
--   pretty True = "true"
--   pretty False = "false"

ppBoolean :: Bool -> Doc ann
ppBoolean True = "true"
ppBoolean False = "false"

instance Pretty Name where
  pretty (Name nm)
    | BS.null nm = dquotes mempty
    | isFirst first && all isRest name = pretty (pack name)
    | otherwise = dquotes . hcat . map escape $ name
    where
        name = unShort nm
        first = head name
        isFirst c = isLetter c || c == '-' || c == '_' || c == '$' || c == '.'
        isRest c = isDigit c || isFirst c
  pretty (UnName x) = pretty ( (fromIntegral x) :: Int)

instance Pretty Parameter where
  pretty (Parameter ty (UnName _) attrs) = pretty ty <+> ppParamAttributes attrs
  pretty (Parameter ty name attrs) = pretty ty <+> ppParamAttributes attrs <+> local' (pretty name)

ppParamAttributes :: [ParameterAttribute] -> Doc ann
ppParamAttributes pas = hsep $ fmap pretty pas

-- TODO: Auto instance
-- instance Pretty [ParameterAttribute] where
--   pretty x = hsep $ fmap pretty x

-- instance Pretty ([Parameter], Bool) where
--   pretty (params, False) = commas (fmap pretty params)
--   pretty (params, True) = "TODO" XXX: variadic case

-- instance Pretty (Operand, [ParameterAttribute]) where
--   pretty (op, attrs) = pretty (typeOf op) <+> pretty attrs <+> pretty op

ppArguments :: (Operand, [ParameterAttribute]) -> Doc ann
ppArguments (op, attrs) = pretty (typeOf op) <+> ppParamAttributes attrs <+> pretty op

instance Pretty UnnamedAddr where
  pretty LocalAddr = "local_unnamed_addr"
  pretty GlobalAddr = "unnamed_addr"

instance Pretty Type where
  pretty (IntegerType width) = "i" <> pretty width
  pretty (FloatingPointType HalfFP)      = "half"
  pretty (FloatingPointType FloatFP )    = "float"
  pretty (FloatingPointType DoubleFP)    = "double"
  pretty (FloatingPointType FP128FP)     = "fp128"
  pretty (FloatingPointType X86_FP80FP)  = "x86_fp80"
  pretty (FloatingPointType PPC_FP128FP) = "ppc_fp128"

  pretty VoidType = "void"
  pretty (PointerType ref (AS.AddrSpace addr))
    | addr == 0 = pretty ref <> "*"
    | otherwise = pretty ref <+> "addrspace" <> parens (pretty addr) <> "*"
  pretty ft@(FunctionType {..}) = pretty resultType <+> ppFunctionArgumentTypes ft
  pretty (VectorType {..}) = "<" <> pretty nVectorElements <+> "x" <+> pretty elementType <> ">"
  pretty (StructureType {..}) = if isPacked
                               then "<{" <> (commas $ fmap pretty elementTypes ) <> "}>"
                               else  "{" <> (commas $ fmap pretty elementTypes ) <> "}"
  pretty (ArrayType {..}) = brackets $ pretty nArrayElements <+> "x" <+> pretty elementType
  pretty (NamedTypeReference name) = "%" <> pretty name
  pretty MetadataType = "metadata"
  pretty TokenType = "token"
  pretty LabelType = "label"

instance Pretty Global where
  pretty Function {..} =
      case basicBlocks of
        [] ->
          ("declare" <+> pretty linkage <+> pretty callingConvention
            <+> ppReturnAttributes returnAttributes <+> pretty returnType <+> global (pretty name)
            <> ppParams (pretty . typeOf) parameters <+> ppFunctionAttributes functionAttributes <+> align <+> gcName <+> pre)

        -- single unnamed block is special cased, and won't parse otherwise... yeah good times
        [b@(BasicBlock (UnName _) _ _)] ->
            ("define" <+> pretty linkage <+> pretty callingConvention
              <+> ppReturnAttributes returnAttributes <+> pretty returnType <+> global (pretty name)
              <> ppParams pretty parameters <+> ppFunctionAttributes functionAttributes <+> align <+> gcName <+> pre)
            `wrapbraces` (indent 2 $ ppSingleBlock b)

        bs ->
          ("define" <+> pretty linkage <+> pretty callingConvention
            <+> ppReturnAttributes returnAttributes <+> pretty returnType <+> global (pretty name)
            <> ppParams pretty parameters <+> ppFunctionAttributes functionAttributes <+> align <+> gcName <+> pre)
          `wrapbraces` (vcat $ fmap pretty bs)
    where
      pre = case prefix of
              Nothing  -> mempty
              Just con -> "prefix" <+> ppTyped con
      align | alignment == 0    = mempty
            | otherwise = "align" <+> pretty alignment
      gcName = maybe mempty (\n -> "gc" <+> dquotes (pretty $ pack n)) (fmap unShort garbageCollectorName)

  pretty GlobalVariable {..} = global (pretty name) <+> "=" <+> ppLinkage hasInitializer linkage <+> ppMaybe unnamedAddr
                             <+> addrSpace' <+> kind <+> pretty type' <+> ppMaybe initializer <> ppAlign alignment
    where
      hasInitializer = isJust initializer
      addrSpace' =
        case addrSpace of
          AS.AddrSpace addr
            | addr == 0 -> mempty
            | otherwise -> "addrspace" <> parens (pretty addr)
      kind | isConstant = "constant"
           | otherwise  = "global"

  pretty GlobalAlias {..} = global (pretty name) <+> "=" <+> pretty linkage <+> ppMaybe unnamedAddr <+> "alias" <+> pretty type' `cma` ppTyped aliasee

ppFunctionAttribute :: Either GroupID FunctionAttribute -> Doc ann
ppFunctionAttribute (Left grpId) = pretty grpId
ppFunctionAttribute (Right fA) = pretty fA

ppFunctionAttributes :: [Either GroupID FunctionAttribute] -> Doc ann
ppFunctionAttributes attribs = hsep $ fmap ppFunctionAttribute attribs

ppMetadata :: Maybe Metadata -> Doc ann
ppMetadata Nothing = "null"
ppMetadata (Just m) = pretty m

instance Pretty Definition where
  pretty (GlobalDefinition x) = pretty x
  pretty (TypeDefinition nm ty) = local' (pretty nm) <+> "=" <+> "type" <+> maybe "opaque" pretty ty
  pretty (FunctionAttributes gid attrs) = "attributes" <+> pretty gid <+> "=" <+> braces (hsep (fmap ppAttrInGroup attrs))
  pretty (NamedMetadataDefinition nm meta) = "!" <> short nm <+> "=" <+> "!" <> braces (commas (fmap pretty meta))
  pretty (MetadataNodeDefinition node meta) = pretty node <+> "=" <+> pretty meta
  pretty (ModuleInlineAssembly asm) = "module asm" <+> dquotes (pretty (pack (BL.unpack asm)))
  pretty (COMDAT name selKind) = "$" <> short name <+> "=" <+> "comdat" <+> pretty selKind

instance Pretty SelectionKind where
  pretty Any = "any"
  pretty ExactMatch = "exactmatch"
  pretty Largest = "largest"
  pretty NoDuplicates = "noduplicates"
  pretty SameSize = "samesize"

ppAttrInGroup :: FunctionAttribute -> Doc ann
ppAttrInGroup = \case
  StackAlignment n -> "alignstack=" <> pretty n
  attr -> pretty attr

instance Pretty FunctionAttribute where
  pretty = \case
   NoReturn            -> "noreturn"
   NoUnwind            -> "nounwind"
   FA.ReadNone         -> "readnone"
   FA.ReadOnly         -> "readonly"
   FA.WriteOnly        -> "writeonly"
   NoInline            -> "noinline"
   AlwaysInline        -> "alwaysinline"
   MinimizeSize        -> "minsize"
   OptimizeForSize     -> "optsize"
   OptimizeNone        -> "optnone"
   SafeStack           -> "safestack"
   StackProtect        -> "ssp"
   StackProtectReq     -> "sspreq"
   StackProtectStrong  -> "sspstrong"
   NoRedZone           -> "noredzone"
   NoImplicitFloat     -> "noimplicitfloat"
   Naked               -> "naked"
   InlineHint          -> "inlinehint"
   StackAlignment n    -> "alignstack" <> parens (pretty n)
   ReturnsTwice        -> "returns_twice"
   UWTable             -> "uwtable"
   NonLazyBind         -> "nonlazybind"
   Builtin             -> "builtin"
   NoBuiltin           -> "nobuiltin"
   Cold                -> "cold"
   JumpTable           -> "jumptable"
   NoDuplicate         -> "noduplicate"
   SanitizeAddress     -> "sanitize_address"
   SanitizeThread      -> "sanitize_thread"
   SanitizeMemory      -> "sanitize_memory"
   SanitizeHWAddress   -> "sanitize_hwaddress"
   NoRecurse           -> "norecurse"
   Convergent          -> "convergent"
   ArgMemOnly          -> "argmemonly"
   InaccessibleMemOnly -> "inaccessiblememonly"
   AllocSize a Nothing -> "allocsize" <> parens (pretty a)
   AllocSize a (Just b) -> "allocsize" <> parens (commas [pretty a, pretty b])
   InaccessibleMemOrArgMemOnly -> "inaccessiblemem_or_argmemonly"
   FA.StringAttribute k v -> dquotes (short k) <> "=" <> dquotes (short v)
   Speculatable        -> "speculatable"
   StrictFP            -> "strictfp"

instance Pretty ParameterAttribute where
  pretty = \case
    ZeroExt                    -> "zeroext"
    SignExt                    -> "signext"
    InReg                      -> "inreg"
    SRet                       -> "sret"
    Alignment word             -> "align" <+> pretty word
    NoAlias                    -> "noalias"
    ByVal                      -> "byval"
    NoCapture                  -> "nocapture"
    Nest                       -> "nest"
    PA.ReadNone                -> "readnone"
    PA.ReadOnly                -> "readonly"
    PA.WriteOnly               -> "writeonly"
    InAlloca                   -> "inalloca"
    NonNull                    -> "nonnull"
    Dereferenceable word       -> "dereferenceable" <> parens (pretty word)
    DereferenceableOrNull word -> "dereferenceable_or_null" <> parens (pretty word)
    Returned                   -> "returned"
    SwiftSelf                  -> "swiftself"
    SwiftError                 -> "swifterror"
    ImmArg                     -> "imgarg"
    PA.StringAttribute k v -> dquotes (short k) <> "=" <> dquotes (short v)

instance Pretty CC.CallingConvention where
  pretty = \case
   CC.Numbered word -> "cc" <+> pretty word
   CC.C             -> "ccc"
   CC.Fast          -> "fastcc"
   CC.Cold          -> "coldcc"
   CC.GHC           -> "cc 10"
   CC.HiPE          -> "cc 11"
   CC.WebKit_JS     -> "webkit_jscc"
   CC.AnyReg        -> "anyregcc"
   CC.PreserveMost  -> "preserve_mostcc"
   CC.PreserveAll   -> "preserve_allcc"
   CC.Swift         -> "swiftcc"
   CC.CXX_FastTLS   -> "cxx_fast_tlscc"
   CC.X86_StdCall   -> "cc 64"
   CC.X86_FastCall  -> "cc 65"
   CC.ARM_APCS      -> "cc 66"
   CC.ARM_AAPCS     -> "cc 67"
   CC.ARM_AAPCS_VFP -> "cc 68"
   CC.MSP430_INTR   -> "cc 69"
   CC.X86_ThisCall  -> "cc 70"
   CC.PTX_Kernel    -> "cc 71"
   CC.PTX_Device    -> "cc 72"
   CC.SPIR_FUNC     -> "cc 75"
   CC.SPIR_KERNEL   -> "cc 76"
   CC.Intel_OCL_BI  -> "cc 77"
   CC.X86_64_SysV   -> "cc 78"
   CC.Win64         -> "cc 79"
   CC.X86_Intr      -> "x86_intrcc"
   CC.X86_RegCall   -> "x86_regcallcc"
   CC.X86_VectorCall -> "x86_vectorcallcc"
   CC.AVR_Intr      -> "avr_intrcc"
   CC.AVR_Signal    -> "avr_signalcc"
   CC.AVR_Builtin   -> "cc 86"
   CC.HHVM          -> "hhvmcc"
   CC.HHVM_C        -> "hhvm_ccc"
   CC.AMDGPU_VS     -> "amdgpu_vs"
   CC.AMDGPU_GS     -> "amdgpu_gs"
   CC.AMDGPU_PS     -> "amdgpu_ps"
   CC.AMDGPU_CS     -> "amdgpu_cs"
   CC.AMDGPU_HS     -> "amdgpu_hs"
   CC.AMDGPU_Kernel -> "amdgpu_kernel"
   CC.MSP430_Builtin -> "msp430"

instance Pretty L.Linkage where
    pretty = ppLinkage False

ppLinkage :: Bool -> L.Linkage -> Doc ann
ppLinkage omitExternal = \case
   L.External | omitExternal -> mempty
              | otherwise    -> "external"
   L.Private                 -> "private"
   L.Internal                -> "internal"
   L.ExternWeak              -> "extern_weak"
   L.AvailableExternally     -> "available_externally"
   L.LinkOnce                -> "linkonce"
   L.Weak                    -> "weak"
   L.Common                  -> "common"
   L.Appending               -> "appending"
   L.LinkOnceODR             -> "linkonce_odr"
   L.WeakODR                 -> "weak_odr"

ppInstructionMetadata :: InstructionMetadata -> Doc ann
ppInstructionMetadata meta = commas ["!" <> short x <+> pretty y | (x,y) <- meta]

instance Pretty MetadataNodeID where
  pretty (MetadataNodeID x) = "!" <> pretty ((fromIntegral x) :: Int)

instance Pretty GroupID where
  pretty (GroupID x) = "#" <> pretty ((fromIntegral x) :: Int)

instance Pretty BasicBlock where
  pretty (BasicBlock nm instrs term) =
    label <> P.line <> indent 2 (vcat $ (fmap pretty instrs) ++ [pretty term])
    where
      label = case nm of
        UnName _ -> "; <label>:" <> pretty nm <> ":"
        _ -> pretty nm <> ":"

instance Pretty Terminator where
  pretty = \case
    Br dest meta -> "br" <+> label (pretty dest) <+> ppInstrMeta meta

    Ret val meta -> "ret" <+> maybe "void" ppTyped val <+> ppInstrMeta meta

    CondBr cond tdest fdest meta ->
     "br" <+> ppTyped cond
     `cma` label (pretty tdest)
     `cma` label (pretty fdest)
     <+> ppInstrMeta meta

    Switch {..} -> "switch" <+> ppTyped operand0'
                 `cma` label (pretty defaultDest)
                 <+> brackets (hsep [ ppTyped v `cma` label (pretty l) | (v,l) <- dests ])
                 <+> ppInstrMeta metadata'

    Unreachable {..} -> "unreachable" <+> ppInstrMeta metadata'

    IndirectBr op dests meta -> "indirectbr" <+> ppTyped op `cma`
     brackets (hsep [ label (pretty l) | l <- dests ])
     <+> ppInstrMeta meta

    e@Invoke {..} ->
     ppInvoke e
     <+> "to" <+> label (pretty returnDest)
     <+> "unwind" <+> label (pretty exceptionDest)
     <+> ppInstrMeta metadata'

    Resume op meta -> "resume "<+> ppTyped op <+> ppInstrMeta meta

    CleanupRet pad dest meta ->
      "cleanupret" <+> "from" <+> pretty pad <+> "unwind" <+> maybe "to caller" (label . pretty) dest
      <+> ppInstrMeta meta

    CatchRet catchPad succ meta ->
      "catchret" <+> "from" <+> pretty catchPad <+> "to" <+> label (pretty succ)
      <+> ppInstrMeta meta

    CatchSwitch {..} ->
      "catchswitch" <+> "within" <+> pretty parentPad' <+>
      brackets (commas (map (label . pretty) (toList catchHandlers))) <+>
      "unwind" <+> "to" <+> maybe "caller" pretty defaultUnwindDest
      <+> ppInstrMeta metadata'

instance Pretty Instruction where
  pretty = \case
    Add {..}    -> ppInstrWithNuwNsw "add" nuw nsw operand0 operand1 metadata
    Sub {..}    -> ppInstrWithNuwNsw "sub" nuw nsw operand0 operand1 metadata
    Mul {..}    -> ppInstrWithNuwNsw "mul" nuw nsw operand0 operand1 metadata
    Shl {..}    -> ppInstrWithNuwNsw "shl" nuw nsw operand0 operand1 metadata
    AShr {..}   -> ppInstrWithExact "ashr" exact operand0 operand1 metadata
    LShr {..}   -> ppInstrWithExact "lshr" exact operand0 operand1 metadata

    And {..}    -> "and"  <+> ppTyped operand0 `cma` pretty operand1 <+> ppInstrMeta metadata
    Or {..}     -> "or"   <+> ppTyped operand0 `cma` pretty operand1 <+> ppInstrMeta metadata
    Xor {..}    -> "xor"  <+> ppTyped operand0 `cma` pretty operand1 <+> ppInstrMeta metadata
    SDiv {..}   -> ppInstrWithExact "sdiv" exact operand0 operand1 metadata
    UDiv {..}   -> ppInstrWithExact "udiv" exact operand0 operand1 metadata
    SRem {..}   -> "srem"  <+> ppTyped operand0 `cma` pretty operand1 <+> ppInstrMeta metadata
    URem {..}   -> "urem"  <+> ppTyped operand0 `cma` pretty operand1 <+> ppInstrMeta metadata

    FAdd {..}   -> "fadd" <+> (pretty fastMathFlags) <+> ppTyped operand0 `cma` pretty operand1 <+> ppInstrMeta metadata
    FSub {..}   -> "fsub" <+> (pretty fastMathFlags) <+> ppTyped operand0 `cma` pretty operand1 <+> ppInstrMeta metadata
    FMul {..}   -> "fmul" <+> (pretty fastMathFlags) <+> ppTyped operand0 `cma` pretty operand1 <+> ppInstrMeta metadata
    FDiv {..}   -> "fdiv" <+> (pretty fastMathFlags) <+> ppTyped operand0 `cma` pretty operand1 <+> ppInstrMeta metadata
    FRem {..}   -> "frem" <+> (pretty fastMathFlags) <+> ppTyped operand0 `cma` pretty operand1 <+> ppInstrMeta metadata
    FCmp {..}   -> "fcmp" <+> pretty fpPredicate <+> ppTyped operand0 `cma` pretty operand1 <+> ppInstrMeta metadata

    Alloca {..} -> "alloca" <+> pretty allocatedType <> num <> ppAlign alignment <+> ppInstrMeta metadata
      where num   = case numElements of Nothing -> mempty
                                        Just o -> "," <+> ppTyped o
    Store {..}  -> "store" <+> ppMAtomicity maybeAtomicity <+> ppVolatile volatile <+> ppTyped value `cma` ppTyped address <+> ppMOrdering maybeAtomicity <> ppAlign alignment <+> ppInstrMeta metadata
    Load {..}   -> "load" <+> ppMAtomicity maybeAtomicity <+> ppVolatile volatile <+> pretty argTy `cma` ppTyped address <+> ppMOrdering maybeAtomicity <> ppAlign alignment <+> ppInstrMeta metadata
      where
        argTy = case typeOf address of
          PointerType argTy_ _ -> argTy_
          _ -> error "invalid load of non-pointer type. (Malformed AST)"
    Phi {..}    -> "phi" <+> pretty type' <+> commas (fmap phiIncoming incomingValues) <+> ppInstrMeta metadata

    ICmp {..}   -> "icmp" <+> pretty iPredicate <+> ppTyped operand0 `cma` pretty operand1 <+> ppInstrMeta metadata

    c@Call {..} -> ppCall c  <+> ppInstrMeta metadata
    Select {..} -> "select" <+> commas [ppTyped condition', ppTyped trueValue, ppTyped falseValue] <+> ppInstrMeta metadata
    SExt {..}   -> "sext" <+> ppTyped operand0 <+> "to" <+> pretty type' <+> ppInstrMeta metadata <+> ppInstrMeta metadata
    ZExt {..}   -> "zext" <+> ppTyped operand0 <+> "to" <+> pretty type' <+> ppInstrMeta metadata <+> ppInstrMeta metadata
    FPExt {..}   -> "fpext" <+> ppTyped operand0 <+> "to" <+> pretty type' <+> ppInstrMeta metadata <+> ppInstrMeta metadata
    Trunc {..}  -> "trunc" <+> ppTyped operand0 <+> "to" <+> pretty type' <+> ppInstrMeta metadata <+> ppInstrMeta metadata
    FPTrunc {..}  -> "fptrunc" <+> ppTyped operand0 <+> "to" <+> pretty type' <+> ppInstrMeta metadata <+> ppInstrMeta metadata

    GetElementPtr {..} -> "getelementptr" <+> bounds inBounds <+> commas (pretty argTy : fmap ppTyped (address:indices)) <+> ppInstrMeta metadata
      where argTy = getElementType $ typeOf address
    ExtractValue {..} -> "extractvalue" <+> commas (ppTyped aggregate : fmap pretty indices') <+> ppInstrMeta metadata

    BitCast {..} -> "bitcast" <+> ppTyped operand0 <+> "to" <+> pretty type' <+> ppInstrMeta metadata
    FPToUI {..} -> "fptoui" <+> ppTyped operand0 <+> "to" <+> pretty type' <+> ppInstrMeta metadata
    FPToSI {..} -> "fptosi" <+> ppTyped operand0 <+> "to" <+> pretty type' <+> ppInstrMeta metadata
    UIToFP {..} -> "uitofp" <+> ppTyped operand0 <+> "to" <+> pretty type' <+> ppInstrMeta metadata
    SIToFP {..} -> "sitofp" <+> ppTyped operand0 <+> "to" <+> pretty type' <+> ppInstrMeta metadata
    PtrToInt {..} -> "ptrtoint" <+> ppTyped operand0 <+> "to" <+> pretty type' <+> ppInstrMeta metadata
    IntToPtr {..} -> "inttoptr" <+> ppTyped operand0 <+> "to" <+> pretty type' <+> ppInstrMeta metadata

    InsertElement {..} -> "insertelement" <+> commas [ppTyped vector, ppTyped element, ppTyped index] <+> ppInstrMeta metadata
    ShuffleVector {..} -> "shufflevector" <+> commas [ppTyped operand0, ppTyped operand1, ppTyped mask] <+> ppInstrMeta metadata
    ExtractElement {..} -> "extractelement" <+> commas [ppTyped vector, ppTyped index] <+> ppInstrMeta metadata
    InsertValue {..} -> "insertvalue" <+> commas (ppTyped aggregate : ppTyped element : fmap pretty indices') <+> ppInstrMeta metadata

    Fence {..} -> "fence" <+> ppAtomicity atomicity <+> ppInstrMeta metadata
    AtomicRMW {..} -> "atomicrmw" <+> ppVolatile volatile <+> pretty rmwOperation <+> ppTyped address `cma` ppTyped value <+> ppAtomicity atomicity  <+> ppInstrMeta metadata
    CmpXchg {..} -> "cmpxchg" <+> ppVolatile volatile <+> ppTyped address `cma` ppTyped expected `cma` ppTyped replacement
      <+> ppAtomicity atomicity <+> pretty failureMemoryOrdering <+> ppInstrMeta metadata

    AddrSpaceCast {..} -> "addrspacecast" <+> ppTyped operand0 <+> "to" <+> pretty type' <+> ppInstrMeta metadata
    VAArg {..} -> "va_arg" <+> ppTyped argList `cma` pretty type' <+> ppInstrMeta metadata

    LandingPad {..} ->
      "landingpad" <+> pretty type' <+> ppBool "cleanup" cleanup <+> ppInstrMeta metadata
      <+> commas (fmap pretty clauses)
    CatchPad {..} -> "catchpad" <+> "within" <+> pretty catchSwitch <+> brackets (commas (map ppTyped args)) <+> ppInstrMeta metadata
    CleanupPad {..} -> "cleanuppad" <+> "within" <+> pretty parentPad <+> brackets (commas (map ppTyped args)) <+> ppInstrMeta metadata

    where
      bounds True = "inbounds"
      bounds False = mempty

      ppInstrWithNuwNsw :: Doc ann -> Bool -> Bool -> Operand -> Operand -> InstructionMetadata -> Doc ann
      ppInstrWithNuwNsw name nuw nsw op0 op1 metadata =
        name
        <+> ppBool "nuw" nuw
        <+> ppBool "nsw" nsw
        <+> ppTyped op0
        `cma` pretty op1
        <+> ppInstrMeta metadata

      ppInstrWithExact :: Doc ann -> Bool -> Operand -> Operand -> InstructionMetadata -> Doc ann
      ppInstrWithExact name exact op0 op1 metadata =
        name
        <+> ppBool "exact" exact
        <+> ppTyped op0
        `cma` pretty op1
        <+> ppInstrMeta metadata

instance Pretty CallableOperand where
  pretty (Left asm) = error "CallableOperand"
  pretty (Right op) = pretty op

instance Pretty LandingPadClause where
  pretty = \case
    Catch c  -> "catch" <+> ppTyped c
    Filter c -> "filter" <+> ppTyped c

-- instance Pretty [Either GroupID FunctionAttribute] where
--   pretty x = hsep $ fmap pretty x

instance Pretty (Either GroupID FunctionAttribute) where
  pretty (Left gid) = pretty gid
  pretty (Right fattr) = pretty fattr

instance Pretty Operand where
  pretty (LocalReference _ nm) = local' (pretty nm)
  pretty (ConstantOperand con) = pretty con
  pretty (MetadataOperand mdata) = pretty mdata

instance Pretty Metadata where
  pretty (MDString str) = "!" <> dquotes (pretty (decodeShortUtf8 str))
  pretty (MDNode node) = pretty node
  pretty (MDValue operand) = ppTyped operand

ppDINode :: [Char] -> [([Char], Maybe (Doc ann))] -> Doc ann
ppDINode name attrs = "!" <> pretty name <> parens (commas (mapMaybe (\(k, mayV) -> fmap (\v -> pretty k <> ":" <+> v) mayV) attrs))

ppDIArray :: [Doc ann] -> Maybe (Doc ann)
ppDIArray [] = Nothing
ppDIArray xs = Just ("!" <> braces (commas xs))

instance Pretty a => Pretty (MDRef a) where
  pretty (MDInline a) = pretty a
  pretty (MDRef ref) = pretty ref

instance Pretty MDNode where
  pretty (MDTuple xs) = "!" <> braces (commas (map ppMetadata xs))
  pretty (DIExpression e) = pretty e
  pretty (DIGlobalVariableExpression e) = pretty e
  pretty (DILocation l) = pretty l
  pretty (DIMacroNode m) = pretty m
  pretty (DINode n) = pretty n

instance Pretty DIExpression where
  pretty (Expression os) = "!DIExpression" <> parens (commas (concatMap ppDWOp os))

ppDWOp :: DWOp -> [Doc ann]
ppDWOp o = case o of
  DwOpFragment DW_OP_LLVM_Fragment {..} -> ["DW_OP_LLVM_fragment", pretty offset, pretty size]
  DW_OP_StackValue -> ["DW_OP_stack_value"]
  DW_OP_Swap -> ["DW_OP_swap"]
  DW_OP_ConstU c -> ["DW_OP_constu", pretty c]
  DW_OP_PlusUConst c -> ["DW_OP_plus_uconst", pretty c]
  DW_OP_Plus -> ["DW_OP_plus"]
  DW_OP_Minus -> ["DW_OP_minus"]
  DW_OP_Mul -> ["DW_OP_mul"]
  DW_OP_Deref -> ["DW_OP_deref"]
  DW_OP_XDeref -> ["DW_OP_xderef"]
  DW_OP_Lit0 -> ["DW_OP_Lit0"]
  DW_OP_Div -> ["DW_OP_Div"]
  DW_OP_Mod -> ["DW_OP_Mod"]
  DW_OP_Not -> ["DW_OP_Not"]
  DW_OP_Or -> ["DW_OP_Or"]
  DW_OP_Xor -> ["DW_OP_Xor"]
  DW_OP_And -> ["DW_OP_And"]
  DW_OP_Shr -> ["DW_OP_Shr"]
  DW_OP_Shra -> ["DW_OP_Shra"]
  DW_OP_Shl -> ["DW_OP_Shl"]
  DW_OP_Dup -> ["DW_OP_Dup"]

instance Pretty DIGlobalVariableExpression where
  pretty e = ppDINode "DIGlobalVariableExpression"
    [ ("var", Just (pretty (var e)))
    , ("expr", Just (pretty (expr e)))
    ]

instance Pretty DILocation where
  pretty (Location line col scope) =
    ppDINode "DILocation" [("line", Just (pretty line)), ("column", Just (pretty col)), ("scope", Just (pretty scope))]

instance Pretty DIMacroNode where
  pretty DIMacro {..} = ppDINode "DIMacro"
    [("type", Just (pretty info)), ("line", Just (pretty line)), ("name", ppSbs name), ("value", ppSbs value)]
  pretty DIMacroFile {..} = ppDINode "DIMacroFile"
    [ ("line", Just (pretty line))
    , ("file", Just (pretty file))
    , ("nodes", ppDIArray (map pretty elements))
    ]

instance Pretty DIMacroInfo where
  pretty Define = "DW_MACINFO_define"
  pretty Undef = "DW_MACINFO_undef"

instance Pretty DINode where
  pretty (DIEnumerator e) = pretty e
  pretty (DIImportedEntity e) = pretty e
  pretty (DIObjCProperty p) = pretty p
  pretty (DIScope s) = pretty s
  pretty (DISubrange r) = pretty r
  pretty (DITemplateParameter p) = pretty p
  pretty (DIVariable v) = pretty v

instance Pretty DILocalScope where
  pretty (DILexicalBlockBase b) = pretty b
  pretty (DISubprogram p) = pretty p

instance Pretty DIEnumerator where
  pretty (Enumerator val unsigned name) =
    ppDINode "DIEnumerator"
      [ ("name", ppSbs name)
      , ("isUnsigned", if unsigned then Just "true" else Nothing)
      , ("value", Just (pretty val))]

instance Pretty DIImportedEntity where
  pretty ImportedEntity {..} = ppDINode "DIImportedEntity"
    [ ("tag", Just (pretty tag))
    , ("scope", Just (pretty scope))
    , ("name", ppSbs name)
    , ("entity", fmap pretty entity)
    , ("file", fmap pretty file)
    , ("line", Just (pretty line))
    ]

instance Pretty ImportedEntityTag where
  pretty ImportedModule = "DW_TAG_imported_module"
  pretty ImportedDeclaration = "DW_TAG_imported_declaration"

instance Pretty DIObjCProperty where
  pretty ObjCProperty {..} = ppDINode "DIObjCProperty"
    [ ("name", ppSbs name)
    , ("file", fmap pretty file)
    , ("line", Just (pretty line))
    , ("setter", ppSbs getterName)
    , ("getter", ppSbs setterName)
    , ("attributes", Just (pretty attributes))
    , ("type", fmap pretty type')
    ]

instance Pretty DIScope where
  pretty (DICompileUnit cu) = pretty cu
  pretty (DIFile f) = pretty f
  pretty (DILocalScope s) = pretty s
  pretty (DIModule m) = pretty m
  pretty (DINamespace ns) = pretty ns
  pretty (DIType t) = pretty t

instance Pretty DISubrange where
  pretty Subrange {..} = ppDINode "DISubrange" [("count", Just (pretty count)), ("lowerBound", Just (pretty lowerBound))]

instance Pretty DICount where
  pretty (DICountConstant c) = pretty c
  pretty (DICountVariable v) = pretty v

instance Pretty DITemplateParameter where
  pretty DITemplateTypeParameter {..} = ppDINode "DITemplateTypeParameter"
    [ ("name", ppSbs name), ("type", Just (pretty type')) ]
  pretty DITemplateValueParameter {..} = ppDINode "DITemplateValueParameter"
   [ ("tag", ppTemplateValueParameterTag tag)
   , ("name", ppSbs name)
   , ("type", Just (pretty type'))
   , ("value", Just (pretty value))
   ]

ppTemplateValueParameterTag :: TemplateValueParameterTag -> Maybe (Doc ann)
ppTemplateValueParameterTag TemplateValueParameter = Nothing
ppTemplateValueParameterTag GNUTemplateTemplateParam = Just "DW_TAG_GNU_template_template_param"
ppTemplateValueParameterTag GNUTemplateParameterPack = Just "DW_TAG_GNU_template_parameter_pack"

instance Pretty DIVariable where
  pretty (DIGlobalVariable v) = pretty v
  pretty (DILocalVariable v) = pretty v

instance Pretty DICompileUnit where
  pretty cu@CompileUnit {..} = "distinct" <+> ppDINode "DICompileUnit"
    [ ("language", Just (pretty language))
    , ("file", Just (pretty file))
    , ("producer", ppSbs producer)
    , ("isOptimized", Just (ppBoolean optimized))
    , ("flags", ppSbs flags)
    , ("runtimeVersion", Just (pretty runtimeVersion))
    , ("splitDebugFileName", ppSbs splitDebugFileName)
    , ("emissionKind", Just (pretty emissionKind))
    , ("enums", ppDIArray (map pretty enums))
    , ("retainedTypes", ppDIArray (map ppEither retainedTypes))
    , ("globals", ppDIArray (map pretty globals))
    , ("imports", ppDIArray (map pretty imports))
    , ("macros", ppDIArray (map pretty macros))
    , ("dwoId", Just (pretty dWOId))
    , ("splitDebugInlining", Just (ppBoolean splitDebugInlining))
    , ("debugInfoForProfiling", Just (ppBoolean debugInfoForProfiling))
    , ("nameTableKind", Just (pretty nameTableKind))
    , ("debugBaseAddress", Just (ppBoolean debugBaseAddress))
    ]

instance Pretty DebugEmissionKind where
  pretty NoDebug = "NoDebug"
  pretty FullDebug = "FullDebug"
  pretty LineTablesOnly = "LineTablesOnly"

instance Pretty DIFile where
  pretty (File {..}) = ppDINode "DIFile" $
    [ ("filename", Just (dquotes (pretty filename)))
    , ("directory", Just (dquotes (pretty directory)))
    ]
    <> ppDIChecksum checksum

ppDIChecksum :: Maybe ChecksumInfo -> [([Char], Maybe (Doc ann))]
ppDIChecksum Nothing = []
ppDIChecksum (Just (ChecksumInfo kind val)) = [("checksumkind", Just (pretty kind)), ("checksum", ppSbs val)]

instance Pretty DIModule where
  pretty O.Module {..} = ppDINode "DIModule"
    [ ("scope", Just (maybe "null" pretty scope))
    , ("name", ppSbs name)
    , ("configMacros", ppSbs configurationMacros)
    , ("includePath", ppSbs includePath)
    , ("isysroot", ppSbs isysRoot)
    ]

instance Pretty DINamespace where
  pretty Namespace {..} = ppDINode "DINamespace"
    [ ("name", ppSbs name)
    , ("scope", Just (maybe "null" pretty scope))
    , ("exportSymbols", Just (ppBoolean exportSymbols))
    ]

instance Pretty DIType where
  pretty (DIBasicType t) = pretty t
  pretty (DICompositeType t) = pretty t
  pretty (DIDerivedType t) = pretty t
  pretty (DISubroutineType t) = pretty t

instance Pretty DILexicalBlockBase where
  pretty DILexicalBlock {..} = ppDINode "DILexicalBlock"
    [ ("scope", Just (pretty scope))
    , ("file", fmap pretty file)
    , ("line", Just (pretty line))
    , ("column", Just (pretty column))
    ]
  pretty DILexicalBlockFile {..} = ppDINode "DILexicalBlockFile"
    [ ("scope", Just (pretty scope)), ("file", fmap pretty file), ("discriminator", Just (pretty discriminator)) ]

ppSbs :: BS.ShortByteString -> Maybe (Doc ann)
ppSbs s
  | SBF.null s = Nothing
  | otherwise = Just (dquotes (pretty s))

instance Pretty DISubprogram where
  pretty Subprogram {..} = ppMaybe (if definition then Just ("distinct " :: [Char]) else Nothing) <>
   ppDINode "DISubprogram"
   [ ("name", ppSbs name)
   , ("linkageName", ppSbs linkageName)
   , ("scope", fmap pretty scope)
   , ("file", fmap pretty file)
   , ("line", Just (pretty line))
   , ("type", fmap pretty type')
   , ("isLocal", Just (ppBoolean localToUnit))
   , ("isDefinition", Just (ppBoolean definition))
   , ("scopeLine", Just (pretty scopeLine))
   , ("containingType", fmap pretty containingType)
   , ("virtuality", ppVirtuality virtuality)
   , ("virtualIndex", Just (pretty virtualityIndex))
   , ("thisAdjustment", Just (pretty thisAdjustment))
   , ("flags", ppDIFlags flags)
   , ("isOptimized", Just (ppBoolean optimized))
   , ("unit", fmap pretty unit)
   , ("templateParams", ppDIArray (map pretty templateParams))
   , ("declaration", fmap pretty declaration)
   , ("retainedNodes", ppDIArray (map pretty retainedNodes))
   , ("thrownTypes", ppDIArray (map pretty thrownTypes))
   ]

ppVirtuality :: Virtuality -> Maybe (Doc ann)
ppVirtuality NoVirtuality = Nothing
ppVirtuality Virtual = Just "DW_VIRTUALITY_virtual"
ppVirtuality PureVirtual = Just "DW_VIRTUALITY_pure_virtual"

instance Pretty ChecksumKind where
  pretty MD5 = "CSK_MD5"
  pretty SHA1 = "CSK_SHA1"

instance Pretty DIBasicType where
  pretty (BasicType {..}) = ppDINode "DIBasicType"
    [ ("tag", Just (pretty tag))
    , ("name", ppSbs name)
    , ("size", Just (pretty sizeInBits))
    , ("align", Just (pretty alignInBits))
    , ("encoding", fmap pretty encoding)
    ]

instance Pretty BasicTypeTag where
  pretty BaseType = "DW_TAG_base_type"
  pretty UnspecifiedType = "DW_TAG_unspecified_type"

instance Pretty Encoding where
  pretty e = case e of
    AddressEncoding -> "DW_ATE_address"
    BooleanEncoding -> "DW_ATE_boolean"
    UTFEncoding -> "DW_ATE_UTF"
    FloatEncoding -> "DW_ATE_float"
    SignedEncoding -> "DW_ATE_signed"
    SignedCharEncoding -> "DW_ATE_signed_char"
    UnsignedEncoding -> "DW_ATE_unsigned"
    UnsignedCharEncoding -> "DW_ATE_unsigned_char"

ppDIFlags :: [DIFlag] -> Maybe (Doc ann)
ppDIFlags [] = Nothing
ppDIFlags flags = Just (hsep (punctuate (pretty '|') (map pretty flags)))

instance Pretty DIFlag where
  pretty flag = "DIFlag" <> fromString (flagName flag)
    where
      flagName (Accessibility f) = show f
      flagName (InheritanceFlag f) = show f
      flagName VirtualFlag = "Virtual"
      flagName f = show f


ppEither :: (Pretty a, Pretty b) => MDRef (Either a b) -> Doc ann
ppEither (MDRef r) = pretty r
ppEither (MDInline e) = either pretty pretty e

instance Pretty DICompositeType where
  pretty DIArrayType {..} = ppDINode "DICompositeType"
    [ ("tag", Just "DW_TAG_array_type")
    , ("elements", ppDIArray (map pretty subscripts))
    , ("baseType", fmap pretty elementTy)
    , ("size", Just (pretty sizeInBits))
    , ("align", Just (pretty alignInBits))
    , ("flags", ppDIFlags flags)
    ]
  pretty DIClassType {..} = ppDINode "DICompositeType"
    [ ("tag", Just "DW_TAG_class_type")
    , ("scope", fmap pretty scope)
    , ("name", ppSbs name)
    , ("file", fmap pretty file)
    , ("line", Just (pretty line))
    , ("flags", ppDIFlags flags)
    , ("baseType", fmap pretty derivedFrom)
    , ("elements", ppDIArray (map ppEither elements))
    , ("vtableHolder", fmap pretty vtableHolder)
    , ("templateParams", ppDIArray (map pretty templateParams))
    , ("identifier", ppSbs identifier)
    , ("size", Just (pretty sizeInBits))
    , ("align", Just (pretty alignInBits))
    ]
  pretty DIEnumerationType {..} = ppDINode "DICompositeType"
    [ ("tag", Just "DW_TAG_enumeration_type")
    , ("name", ppSbs name)
    , ("file", fmap pretty file)
    , ("line", Just (pretty line))
    , ("size", Just (pretty sizeInBits))
    , ("align", Just (pretty alignInBits))
    , ("elements", Just ("!" <> braces (commas (map pretty values))))
    , ("scope", fmap pretty scope)
    , ("identifier", ppSbs identifier)
    , ("baseType", fmap pretty baseType)
    ]
  pretty DIStructureType {..} = ppDINode "DICompositeType"
    [ ("tag", Just "DW_TAG_structure_type")
    , ("scope", fmap pretty scope)
    , ("name", ppSbs name)
    , ("file", fmap pretty file)
    , ("line", Just (pretty line))
    , ("flags", ppDIFlags flags)
    , ("baseType", fmap pretty derivedFrom)
    , ("elements", ppDIArray (map ppEither elements))
    , ("runtimeLang", Just (pretty runtimeLang))
    , ("vtableHolder", fmap pretty vtableHolder)
    , ("identifier", ppSbs identifier)
    , ("size", Just (pretty sizeInBits))
    , ("align", Just (pretty alignInBits))
    ]
  pretty DIUnionType {..} = ppDINode "DICompositeType"
    [ ("tag", Just "DW_TAG_union_type")
    , ("name", ppSbs name)
    , ("file", fmap pretty file)
    , ("line", Just (pretty line))
    , ("flags", ppDIFlags flags)
    , ("elements", ppDIArray (map ppEither elements))
    , ("runtimeLang", Just (pretty runtimeLang))
    , ("identifier", ppSbs identifier)
    , ("size", Just (pretty sizeInBits))
    , ("align", Just (pretty alignInBits))
    ]

instance Pretty DIDerivedType where
  pretty DerivedType {..} = ppDINode "DIDerivedType"
    [ ("tag", Just (pretty tag))
    , ("name", ppSbs name)
    , ("file", fmap pretty file)
    , ("line", Just (pretty line))
    , ("scope", fmap pretty scope)
    , ("baseType", Just (pretty baseType))
    , ("size", Just (pretty sizeInBits))
    , ("align", Just (pretty alignInBits))
    , ("offset", Just (pretty offsetInBits))
    , ("flags", ppDIFlags flags)
    , ("dwarfAddressSpace", fmap pretty addressSpace)
    ]

instance Pretty DerivedTypeTag where
  pretty t =
    case t of
      Typedef -> "DW_TAG_typedef"
      O.PointerType -> "DW_TAG_pointer_type"
      PtrToMemberType -> "DW_TAG_ptr_to_member_type"
      ReferenceType -> "DW_TAG_reference_type"
      RValueReferenceType -> "DW_TAG_rvalue_reference_type"
      ConstType -> "DW_TAG_const_type"
      VolatileType -> "DW_TAG_volatile_type"
      RestrictType -> "DW_TAG_restrict_type"
      AtomicType -> "DW_TAG_atomic_type"
      Member -> "DW_TAG_member"
      Inheritance -> "DW_TAG_inheritance"
      Friend -> "DW_TAG_friend"

instance Pretty DISubroutineType where
  pretty SubroutineType {..} = ppDINode "DISubroutineType"
    [ ("flags", ppDIFlags flags)
    , ("cc", Just (pretty cc))
    , ("types", Just ("!" <> braces (commas (map ppTy typeArray))))
    ]
    where ppTy Nothing = "null"
          ppTy (Just t) = pretty t

instance Pretty DIGlobalVariable where
  pretty O.GlobalVariable {..} = ppDINode "DIGlobalVariable"
    [ ("name", ppSbs name)
    , ("scope", fmap pretty scope)
    , ("linkageName", ppSbs linkageName)
    , ("file", fmap pretty file)
    , ("line", Just (pretty line))
    , ("type", fmap pretty type')
    , ("isLocal", Just (ppBoolean local))
    , ("isDefinition", Just (ppBoolean definition))
    , ("declaration", fmap pretty staticDataMemberDeclaration)
    , ("align", Just (pretty alignInBits))
    ]

instance Pretty DILocalVariable where
  pretty LocalVariable {..} = ppDINode "DILocalVariable"
    [ ("name", ppSbs name)
    , ("scope", Just (pretty scope))
    , ("file", fmap pretty file)
    , ("line", Just (pretty line))
    , ("type", fmap pretty type')
    , ("flags", ppDIFlags flags)
    , ("arg", Just (pretty arg))
    , ("align", Just (pretty alignInBits))
    ]

instance Pretty C.Constant where
  pretty (C.Int width val) = pretty val
  pretty (C.Float (F.Double val))      =
    if specialFP val
      then "0x" <> (pretty . pack) (showHex (doubleToWord val) "")
      else pretty $ pack $ printf "%6.6e" val
  pretty (C.Float (F.Single val))      =
    if specialFP val
      then "0x" <> (pretty . pack) (showHex (floatToWord val) "")
      else pretty $ pack $ printf "%6.6e" val
  pretty (C.Float (F.Half val))        = pretty $ pack $ printf "%6.6e" val
  pretty (C.Float (F.Quadruple val _)) = pretty $ pack $ printf "%6.6e" val
  pretty (C.Float (F.X86_FP80 val _))  = pretty $ pack $ printf "%6.6e" val
  pretty (C.Float (F.PPC_FP128 val _)) = pretty $ pack $ printf "%6.6e" val

  pretty (C.GlobalReference ty nm) = "@" <> pretty nm
  pretty (C.Vector args) = "<" <+> commas (fmap ppTyped args) <+> ">"

  pretty (C.Add {..})    = "add"  <+> ppTyped operand0 `cma` pretty operand1
  pretty (C.Sub {..})    = "sub"  <+> ppTyped operand0 `cma` pretty operand1
  pretty (C.Mul {..})    = "mul"  <+> ppTyped operand0 `cma` pretty operand1
  pretty (C.Shl {..})    = "shl"  <+> ppTyped operand0 `cma` pretty operand1
  pretty (C.AShr {..})   = "ashr" <+> ppTyped operand0 `cma` pretty operand1
  pretty (C.LShr {..})   = "lshr" <+> ppTyped operand0 `cma` pretty operand1
  pretty (C.And {..})    = "and"  <+> ppTyped operand0 `cma` pretty operand1
  pretty (C.Or {..})     = "or"   <+> ppTyped operand0 `cma` pretty operand1
  pretty (C.Xor {..})    = "xor"  <+> ppTyped operand0 `cma` pretty operand1
  pretty (C.SDiv {..})   = "sdiv"  <+> ppTyped operand0 `cma` pretty operand1
  pretty (C.UDiv {..})   = "udiv"  <+> ppTyped operand0 `cma` pretty operand1
  pretty (C.SRem {..})   = "srem"  <+> ppTyped operand0 `cma` pretty operand1
  pretty (C.URem {..})   = "urem"  <+> ppTyped operand0 `cma` pretty operand1

  pretty (C.FAdd {..})   = "fadd" <+> ppTyped operand0 `cma` pretty operand1
  pretty (C.FSub {..})   = "fsub" <+> ppTyped operand0 `cma` pretty operand1
  pretty (C.FMul {..})   = "fmul" <+> ppTyped operand0 `cma` pretty operand1
  pretty (C.FDiv {..})   = "fdiv" <+> ppTyped operand0 `cma` pretty operand1
  pretty (C.FRem {..})   = "frem" <+> ppTyped operand0 `cma` pretty operand1
  pretty (C.FCmp {..})   = "fcmp" <+> pretty fpPredicate <+> ppTyped operand0 `cma` pretty operand1
  pretty C.ICmp {..}     = "icmp" <+> pretty iPredicate <+> ppTyped operand0 `cma` pretty operand1

  pretty (C.Select {..})  = "select" <+> commas [ppTyped condition', ppTyped trueValue, ppTyped falseValue]
  pretty (C.SExt {..})    = "sext" <+> ppTyped operand0 <+> "to" <+> pretty type'
  pretty (C.ZExt {..})    = "zext" <+> ppTyped operand0 <+> "to" <+> pretty type'
  pretty (C.FPExt {..})   = "fpext" <+> ppTyped operand0 <+> "to" <+> pretty type'
  pretty (C.Trunc {..})   = "trunc" <+> ppTyped operand0 <+> "to" <+> pretty type'
  pretty (C.FPTrunc {..}) = "fptrunc" <+> ppTyped operand0 <+> "to" <+> pretty type'

  pretty C.FPToUI {..} = "fptoui" <+> ppTyped operand0 <+> "to" <+> pretty type'
  pretty C.FPToSI {..} = "fptosi" <+> ppTyped operand0 <+> "to" <+> pretty type'
  pretty C.UIToFP {..} = "uitofp" <+> ppTyped operand0 <+> "to" <+> pretty type'
  pretty C.SIToFP {..} = "sitofp" <+> ppTyped operand0 <+> "to" <+> pretty type'

  pretty (C.Struct _ packed elems) =
    let struct = spacedbraces $ commas $ fmap ppTyped elems
    in if packed
         then angleBrackets struct
         else struct

  pretty (C.Null constantType) = ppNullInitializer constantType

#if MIN_VERSION_llvm_hs_pure(5,1,3)
  pretty (C.AggregateZero constantType) = "zeroinitializer"
#endif

  pretty (C.Undef {}) = "undef"
  pretty (C.TokenNone {}) = "none"
  pretty (C.BlockAddress fn blk) = "blockaddress" <> parens (commas [ global (pretty fn), local' (pretty blk) ])

  pretty C.Array {..}
    | memberType == (IntegerType 8) = "c" <> (dquotes $ hcat [ppIntAsChar val | C.Int _ val <- memberValues])
    | otherwise = brackets $ commas $ fmap ppTyped memberValues

  pretty C.GetElementPtr {..} = "getelementptr" <+> bounds inBounds <+> parens (commas (pretty argTy : fmap ppTyped (address:indices)))
    where
      argTy = case typeOf address of
        PointerType argTy_ _ -> argTy_
        _ -> error "invalid load of non-pointer type. (Malformed AST)"
      bounds True = "inbounds"
      bounds False = mempty

  pretty C.BitCast {..} = "bitcast" <+> parens (ppTyped operand0 <+> "to" <+> pretty type')
  pretty C.PtrToInt {..} = "ptrtoint" <+> parens (ppTyped operand0 <+> "to" <+> pretty type')
  pretty C.IntToPtr {..} = "inttoptr" <+> parens (ppTyped operand0 <+> "to" <+> pretty type')
  pretty C.AddrSpaceCast {..} = "addrspacecast" <+> parens (ppTyped operand0 <+> "to" <+> pretty type')
  pretty _ = error "Non-function argument. (Malformed AST)"

instance Pretty a => Pretty (Named a) where
  pretty (nm := a) = "%" <> pretty nm <+> "=" <+> pretty a
  pretty (Do a) = pretty a

instance Pretty Module where
  pretty Module {..} =
    let header = printf "; ModuleID = '%s'" (unShort moduleName) in
    let target = case moduleTargetTriple of
                      Nothing -> mempty
                      Just target -> "target triple =" <+> dquotes (short target) in
    let layout = case moduleDataLayout of
                      Nothing     -> mempty
                      Just layout -> "target datalayout =" <+> dquotes (pretty layout) in
    hlinecat (fromString header : (layout <> softline <> target) : (fmap pretty moduleDefinitions))

instance Pretty FP.FloatingPointPredicate where
  pretty op = case op of
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

instance Pretty IP.IntegerPredicate where
  pretty op = case op of
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

-- instance Pretty Atomicity where
--   pretty (scope, order) =
--     pretty scope <+> pretty order

ppAtomicity :: Atomicity -> Doc ann
ppAtomicity (scope, order) = pretty scope <+> pretty order

ppMAtomicity :: Maybe Atomicity -> Doc ann
ppMAtomicity (Just m) = "atomic"
ppMAtomicity Nothing = mempty

ppMOrdering :: Maybe Atomicity -> Doc ann
ppMOrdering (Just (scope, order)) = pretty order
ppMOrdering Nothing = mempty

instance Pretty SynchronizationScope where
  pretty = \case
    SingleThread -> "syncscope(\"singlethread\")"
    System -> mempty

instance Pretty MemoryOrdering where
  pretty = \case
    Unordered              -> "unordered"
    Monotonic              -> "monotonic"
    Acquire                -> "acquire"
    Release                -> "release"
    AcquireRelease         -> "acq_rel"
    SequentiallyConsistent -> "seq_cst"

instance Pretty RMW.RMWOperation where
  pretty = \case
    RMW.Xchg -> "xchg"
    RMW.Add -> "add"
    RMW.Sub -> "sub"
    RMW.And -> "and"
    RMW.Nand -> "nand"
    RMW.Or -> "or"
    RMW.Xor -> "xor"
    RMW.Max -> "max"
    RMW.Min -> "min"
    RMW.UMax -> "umax"
    RMW.UMin -> "umin"

instance Pretty DataLayout where
  pretty x = pretty (BL.unpack (dataLayoutToString x))

instance Pretty DebugNameTableKind where
  pretty = \case
    NameTableKindDefault -> "Default"
    NameTableKindGNU -> "GNU"
    NameTableKindNone -> "None"

instance Pretty FastMathFlags where
  pretty FastMathFlags {..} =
        if allowReassoc then "reassoc" else ""
    <+> if noNaNs then "nnan" else ""
    <+> if noInfs then "ninf" else ""
    <+> if noSignedZeros then "nsz" else ""
    <+> if allowReciprocal then "arcp" else ""
    <+> if allowContract then "contract" else ""
    <+> if approxFunc then "afn" else ""


-- -------------------------------------------------------------------------------
-- -- Special Case Hacks
-- -------------------------------------------------------------------------------

escape :: Char -> Doc ann
escape '"'  = pretty ("\\22" :: String)
escape '\\' = pretty ("\\\\" :: String)
escape c    = if isAscii c && not (isControl c)
              then pretty c
              else pretty ("\\" :: String) <> hex c
    where
        hex :: Char -> Doc ann
        hex = pad0 . ($ []) . showHex . ord
        pad0 :: String -> Doc ann
        pad0 [] = "00"
        pad0 [x] = "0" <> pretty x
        pad0 xs = pretty (pack xs)

ppVolatile :: Bool -> Doc ann
ppVolatile True = "volatile"
ppVolatile False = mempty

ppIntAsChar :: Integral a => a -> Doc ann
ppIntAsChar = escape . chr . fromIntegral

ppAlign :: Word32 -> Doc ann
ppAlign x | x == 0    = mempty
          | otherwise = ", align" <+> pretty x

-- print an operand and its type
ppTyped :: (Pretty a, Typed a) => a -> Doc ann
ppTyped a = pretty (typeOf a) <+> pretty a

ppCommaTyped :: (Pretty a, Typed a) => a -> Doc ann
ppCommaTyped a = pretty (typeOf a) `cma` pretty a

phiIncoming :: (Operand, Name) -> Doc ann
phiIncoming (op, nm) = brackets (pretty op `cma` (local' (pretty nm)))

ppParams :: (a -> Doc ann) -> ([a], Bool) -> Doc ann
ppParams ppParam (ps, varrg) = parens . commas $ fmap ppParam ps ++ vargs
    where
        vargs = if varrg then ["..."] else []

ppFunctionArgumentTypes :: Type -> Doc ann
ppFunctionArgumentTypes FunctionType {..} = ppParams pretty (argumentTypes, isVarArg)
ppFunctionArgumentTypes _ = error "Non-function argument. (Malformed AST)"

ppNullInitializer :: Type -> Doc ann
ppNullInitializer PointerType {..} = "zeroinitializer"
ppNullInitializer StructureType {..} = "zeroinitializer"
ppNullInitializer FunctionType {..} = "zeroinitializer"
ppNullInitializer ArrayType {..} = "zeroinitializer"
ppNullInitializer _ = error "Non-pointer argument. (Malformed AST)"

ppCall :: Instruction -> Doc ann
ppCall Call { function = Right f,..}
  = tail <+> "call" <+> pretty callingConvention <+> ppReturnAttributes returnAttributes <+> pretty resultType <+> ftype
    <+> pretty f <> parens (commas $ fmap ppArguments arguments) <+> ppFunctionAttributes functionAttributes
    where
      (functionType@FunctionType {..}) = case (referencedType (typeOf f)) of
                                           fty@FunctionType {..} -> fty
                                           _ -> error "Calling non function type. (Malformed AST)"
      ftype = if isVarArg
              then ppFunctionArgumentTypes functionType
              else mempty
      referencedType (PointerType t _) = referencedType t
      referencedType t                 = t

      tail = case tailCallKind of
        Just Tail -> "tail"
        Just MustTail -> "musttail"
        Just NoTail -> "notail"
        Nothing -> mempty
ppCall Call { function = Left (IA.InlineAssembly {..}), ..}
  = tail <+> "call" <+> pretty callingConvention <+> ppReturnAttributes returnAttributes <+> pretty type'
    <+> "asm" <+> sideeffect' <+> align' <+> dialect' <+> dquotes (pretty (pack (BL.unpack assembly))) <> ","
    <+> dquotes (pretty constraints) <> parens (commas $ fmap ppArguments arguments) <+> ppFunctionAttributes functionAttributes
    where
      tail = case tailCallKind of
        Just Tail -> "tail"
        Just MustTail -> "musttail"
        Just NoTail -> "notail"
        Nothing -> mempty
      -- If multiple keywords appear the ‘sideeffect‘ keyword must come first,
      -- the ‘alignstack‘ keyword second and the ‘inteldialect‘ keyword last.
      sideeffect' = if hasSideEffects then "sideeffect" else ""
      align' = if alignStack then "alignstack" else ""
      -- ATTDialect is assumed if not specified
      dialect' = case dialect of IA.ATTDialect -> ""; IA.IntelDialect -> "inteldialect"
ppCall x = error "Non-callable argument. (Malformed AST)"

ppReturnAttributes :: [ParameterAttribute] -> Doc ann
ppReturnAttributes pas = hsep $ fmap pretty pas

-- Differs from Call in record name conventions only so needs a seperate almost
-- identical function. :(
ppInvoke :: Terminator -> Doc ann
ppInvoke Invoke { function' = Right f,..}
  = "invoke" <+> pretty callingConvention' <+> pretty resultType <+> ftype
    <+> pretty f <> parens (commas $ fmap ppArguments arguments') <+> ppFunctionAttributes functionAttributes'
    where
      (functionType@FunctionType {..}) =
        case referencedType (typeOf f) of
          fty@FunctionType{..} -> fty
          _ -> error "Invoking non-function type. (Malformed AST)"
      ftype = if isVarArg
              then ppFunctionArgumentTypes functionType
              else mempty
      referencedType (PointerType t _) = referencedType t
      referencedType t                 = t
ppInvoke x = error "Non-callable argument. (Malformed AST)"

ppSingleBlock :: BasicBlock -> Doc ann
ppSingleBlock (BasicBlock nm instrs term) = (vcat $ (fmap pretty instrs) ++ [pretty term])

-- According to <https://stackoverflow.com/a/7002812/3877993> this is
-- the best way to cast floats to words.

cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)

floatToWord :: Float -> Word32
floatToWord x = runST (cast x)

specialFP :: RealFloat a => a -> Bool
specialFP f = isNaN f || f == 1 / 0 || f == - 1 / 0

ppInstrMeta :: InstructionMetadata -> Doc ann
ppInstrMeta [] = mempty
ppInstrMeta xs = "," <> ppInstructionMetadata xs

ppLayoutOptions :: LayoutOptions
ppLayoutOptions = LayoutOptions (AvailablePerLine 100 0.5)

-- -------------------------------------------------------------------------------
-- -- Toplevel
-- -------------------------------------------------------------------------------

-- | Pretty print a LLVM module
ppllvm :: Module -> Text
ppllvm = renderLazy . layoutPretty ppLayoutOptions . pretty

-- | Pretty print a printable LLVM expression
ppll :: Pretty a => a -> Text
ppll = renderLazy . layoutPretty ppLayoutOptions . pretty
