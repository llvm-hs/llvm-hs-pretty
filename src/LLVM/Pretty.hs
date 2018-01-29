{-# LANGUAGE CPP #-}
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

import LLVM.AST.Typed

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
import qualified LLVM.AST.AddrSpace as AS
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.RMWOperation as RMW
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
import Text.PrettyPrint.Leijen.Text

import qualified Data.ByteString.Char8 as BL
import qualified Data.ByteString.Short as BS
import Data.Char (chr, ord, isAscii, isControl, isLetter, isDigit)
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Maybe (isJust)
import Numeric (showHex)

import Data.Array.Unsafe
import Data.Array.MArray
import Data.Array.ST
import Control.Monad.ST

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

angleBrackets :: Doc -> Doc
angleBrackets x = char '<' <> x <> char '>'

spacedbraces :: Doc -> Doc
spacedbraces x = char '{' <+> x <+> char '}'

local :: Doc -> Doc
local a = "%" <> a

global :: Doc -> Doc
global a = "@" <> a

label :: Doc -> Doc
label a = "label" <+> "%" <> a

cma :: Doc -> Doc -> Doc -- <,> does not work :(
a `cma` b = a <> "," <+> b

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

class PP p where
  pp :: p -> Doc

ppMaybe :: PP a => Maybe a -> Doc
ppMaybe (Just x) = pp x
ppMaybe Nothing = empty

ppBool :: Doc -> Bool -> Doc
ppBool x True = x
ppBool x False = empty

-- XXX: horrible hack
unShort :: BS.ShortByteString -> [Char]
unShort xs = fmap (toEnum . fromIntegral) $ BS.unpack xs

short :: BS.ShortByteString -> Doc
short x = string (pack (unShort x))

decodeShortUtf8 :: SBF.ShortByteString -> Text
decodeShortUtf8 = decodeUtf8 . fromStrict . SBF.fromShort

instance PP Word32 where
  pp x = int (fromIntegral x)

instance PP Word64 where
  pp x = int (fromIntegral x)

instance PP Integer where
  pp = integer

instance PP BS.ShortByteString where
  pp = pp . unShort

instance PP [Char] where
  pp = text . pack

instance PP Name where
  pp (Name nm)
   | BS.null nm = dquotes empty
    | isFirst first && all isRest name = text (pack name)
    | otherwise = dquotes . hcat . map escape $ name
    where
        name = unShort nm
        first = head name
        isFirst c = isLetter c || c == '-' || c == '_' || c == '$' || c == '.'
        isRest c = isDigit c || isFirst c
  pp (UnName x) = int (fromIntegral x)

instance PP Parameter where
  pp (Parameter ty (UnName _) attrs) = pp ty <+> pp attrs
  pp (Parameter ty name attrs) = pp ty <+> pp attrs <+> local (pp name)

instance PP [ParameterAttribute] where
  pp x = hsep $ fmap pp x

instance PP ([Parameter], Bool) where
  pp (params, False) = commas (fmap pp params)
  pp (params, True) = "TODO" -- XXX: variadic case

instance PP (Operand, [ParameterAttribute]) where
  pp (op, attrs) = pp (typeOf op) <+> pp attrs <+> pp op

instance PP UnnamedAddr where
  pp LocalAddr = "local_unnamed_addr"
  pp GlobalAddr = "unnamed_addr"

instance PP Type where
  pp (IntegerType width) = "i" <> pp width
  pp (FloatingPointType HalfFP)      = "half"
  pp (FloatingPointType FloatFP )    = "float"
  pp (FloatingPointType DoubleFP)    = "double"
  pp (FloatingPointType FP128FP)     = "fp128"
  pp (FloatingPointType X86_FP80FP)  = "x86_fp80"
  pp (FloatingPointType PPC_FP128FP) = "ppc_fp128"

  pp VoidType = "void"
  pp (PointerType ref (AS.AddrSpace addr))
    | addr == 0 = pp ref <> "*"
    | otherwise = pp ref <+> "addrspace" <> parens (pp addr) <> "*"
  pp ft@(FunctionType {..}) = pp resultType <+> ppFunctionArgumentTypes ft
  pp (VectorType {..}) = "<" <> pp nVectorElements <+> "x" <+> pp elementType <> ">"
  pp (StructureType {..}) = if isPacked
                               then "<{" <> (commas $ fmap pp elementTypes ) <> "}>"
                               else  "{" <> (commas $ fmap pp elementTypes ) <> "}"
  pp (ArrayType {..}) = brackets $ pp nArrayElements <+> "x" <+> pp elementType
  pp (NamedTypeReference name) = "%" <> pp name
  pp MetadataType = "metadata"
  pp TokenType = "token"
  pp LabelType = "label"

instance PP Global where
  pp Function {..} =
      case basicBlocks of
        [] ->
          ("declare" <+> pp linkage <+> pp callingConvention
            <+> pp returnAttributes <+> pp returnType <+> global (pp name)
            <> ppParams (pp . typeOf) parameters <+> pp functionAttributes <+> align <+> gcName)

        -- single unnamed block is special cased, and won't parse otherwise... yeah good times
        [b@(BasicBlock (UnName _) _ _)] ->
            ("define" <+> pp linkage <+> pp callingConvention
              <+> pp returnAttributes <+> pp returnType <+> global (pp name)
              <> ppParams pp parameters <+> pp functionAttributes <+> align <+> gcName)
            `wrapbraces` (indent 2 $ ppSingleBlock b)

        bs ->
          ("define" <+> pp linkage <+> pp callingConvention
            <+> pp returnAttributes <+> pp returnType <+> global (pp name)
            <> ppParams pp parameters <+> pp functionAttributes <+> align <+> gcName)
          `wrapbraces` (vcat $ fmap pp bs)
    where
      align | alignment == 0    = empty
            | otherwise = "align" <+> pp alignment
      gcName = maybe empty (\n -> "gc" <+> dquotes (text $ pack n)) (fmap unShort garbageCollectorName)

  pp GlobalVariable {..} = global (pp name) <+> "=" <+> ppLinkage hasInitializer linkage <+> ppMaybe unnamedAddr
                             <+> addrSpace' <+> kind <+> pp type' <+> ppMaybe initializer <> ppAlign alignment
    where
      hasInitializer = isJust initializer
      addrSpace' =
        case addrSpace of
          AS.AddrSpace addr
            | addr == 0 -> mempty
            | otherwise -> "addrspace" <> parens (pp addr)
      kind | isConstant = "constant"
           | otherwise  = "global"

  pp GlobalAlias {..} = global (pp name) <+> "=" <+> pp linkage <+> ppMaybe unnamedAddr <+> "alias" <+> pp typ `cma` ppTyped aliasee
    where
      typ = getElementType type'

ppMetadata :: Maybe Metadata -> Doc
ppMetadata Nothing = "null"
ppMetadata (Just m) = pp m

instance PP Definition where
  pp (GlobalDefinition x) = pp x
  pp (TypeDefinition nm ty) = local (pp nm) <+> "=" <+> "type" <+> maybe "opaque" pp ty
  pp (FunctionAttributes gid attrs) = "attributes" <+> pp gid <+> "=" <+> braces (hsep (fmap ppAttrInGroup attrs))
  pp (NamedMetadataDefinition nm meta) = "!" <> short nm <+> "=" <+> "!" <> braces (commas (fmap pp meta))
  pp (MetadataNodeDefinition node meta) = pp node <+> "=" <+> "!" <> braces (commas (fmap ppMetadata meta))
  pp (ModuleInlineAssembly asm) = "module asm" <+> dquotes (text (pack (BL.unpack asm)))
  pp (COMDAT name selKind) = "$" <> short name <+> "=" <+> "comdat" <+> pp selKind

instance PP SelectionKind where
  pp Any = "any"
  pp ExactMatch = "exactmatch"
  pp Largest = "largest"
  pp NoDuplicates = "noduplicates"
  pp SameSize = "samesize"

ppAttrInGroup :: FunctionAttribute -> Doc
ppAttrInGroup = \case
  StackAlignment n -> "alignstack=" <> pp n
  attr -> pp attr

instance PP FunctionAttribute where
  pp = \case
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
   StackAlignment n    -> "alignstack" <> parens (pp n)
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
   NoRecurse           -> "norecurse"
   Convergent          -> "convergent"
   ArgMemOnly          -> "argmemonly"
   InaccessibleMemOnly -> "inaccessiblememonly"
   AllocSize a Nothing -> "allocsize" <> parens (pp a)
   AllocSize a (Just b) -> "allocsize" <> parens (commas [pp a, pp b])
   InaccessibleMemOrArgMemOnly -> "inaccessiblemem_or_argmemonly"
   FA.StringAttribute k v -> dquotes (short k) <> "=" <> dquotes (short v)
   Speculatable        -> "speculatable"

instance PP ParameterAttribute where
  pp = \case
    ZeroExt                    -> "zeroext"
    SignExt                    -> "signext"
    InReg                      -> "inreg"
    SRet                       -> "sret"
    Alignment word             -> "align" <+> pp word
    NoAlias                    -> "noalias"
    ByVal                      -> "byval"
    NoCapture                  -> "nocapture"
    Nest                       -> "nest"
    PA.ReadNone                -> "readnone"
    PA.ReadOnly                -> "readonly"
    PA.WriteOnly               -> "writeonly"
    InAlloca                   -> "inalloca"
    NonNull                    -> "nonnull"
    Dereferenceable word       -> "dereferenceable" <> parens (pp word)
    DereferenceableOrNull word -> "dereferenceable_or_null" <> parens (pp word)
    Returned                   -> "returned"
    SwiftSelf                  -> "swiftself"
    SwiftError                 -> "swifterror"
    PA.StringAttribute k v -> dquotes (short k) <> "=" <> dquotes (short v)

instance PP CC.CallingConvention where
  pp = \case
   CC.Numbered word -> "cc" <+> pp word
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

instance PP L.Linkage where
    pp = ppLinkage False

ppLinkage :: Bool -> L.Linkage -> Doc
ppLinkage omitExternal = \case
   L.External | omitExternal -> empty
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

instance PP InstructionMetadata where
  pp meta = commas ["!" <> pp x <> "!" <> ("{" <> pp y <> "}") | (x,y) <- meta]

instance PP MetadataNodeID where
  pp (MetadataNodeID x) = "!" <> int (fromIntegral x)

instance PP GroupID where
  pp (GroupID x) = "#" <> int (fromIntegral x)

instance PP BasicBlock where
  pp (BasicBlock nm instrs term) =
    label <$> indent 2 (vcat $ (fmap pp instrs) ++ [pp term])
    where
      label = case nm of
        UnName _ -> "; <label>:" <> pp nm <> ":"
        _ -> pp nm <> ":"

instance PP Terminator where
  pp = \case
    Br dest meta -> "br" <+> label (pp dest) <+> ppInstrMeta meta

    Ret val meta -> "ret" <+> maybe "void" ppTyped val <+> ppInstrMeta meta

    CondBr cond tdest fdest meta ->
     "br" <+> ppTyped cond
     `cma` label (pp tdest)
     `cma` label (pp fdest)
     <+> ppInstrMeta meta

    Switch {..} -> "switch" <+> ppTyped operand0'
                 `cma` label (pp defaultDest)
                 <+> brackets (hsep [ ppTyped v `cma` label (pp l) | (v,l) <- dests ])
                 <+> ppInstrMeta metadata'

    Unreachable {..} -> "unreachable" <+> ppInstrMeta metadata'

    IndirectBr op dests meta -> "indirectbr" <+> ppTyped op `cma`
     brackets (hsep [ label (pp l) | l <- dests ])
     <+> ppInstrMeta meta

    e @ Invoke {..} ->
     ppInvoke e
     <+> "to" <+> label (pp returnDest)
     <+> "unwind" <+> label (pp exceptionDest)
     <+> ppInstrMeta metadata'

    Resume op meta -> "resume "<+> ppTyped op <+> ppInstrMeta meta

    CleanupRet pad dest meta ->
      "cleanupret" <+> "from" <+> pp pad <+> "unwind" <+> maybe "to caller" (label . pp) dest
      <+> ppInstrMeta meta

    CatchRet catchPad succ meta ->
      "catchret" <+> "from" <+> pp catchPad <+> "to" <+> label (pp succ)
      <+> ppInstrMeta meta

    CatchSwitch {..} ->
      "catchswitch" <+> "within" <+> pp parentPad' <+>
      brackets (commas (map (label . pp) (toList catchHandlers))) <+>
      "unwind" <+> "to" <+> maybe "caller" pp defaultUnwindDest
      <+> ppInstrMeta metadata'

instance PP Instruction where
  pp = \case
    Add {..}    -> "add"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    Sub {..}    -> "sub"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    Mul {..}    -> "mul"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    Shl {..}    -> "shl"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    AShr {..}   -> "ashr" <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    LShr {..}   -> "lshr" <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    And {..}    -> "and"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    Or {..}     -> "or"   <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    Xor {..}    -> "xor"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    SDiv {..}   -> "sdiv"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    UDiv {..}   -> "udiv"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    SRem {..}   -> "srem"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    URem {..}   -> "urem"  <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata

    FAdd {..}   -> "fadd" <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    FSub {..}   -> "fsub" <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    FMul {..}   -> "fmul" <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    FDiv {..}   -> "fdiv" <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    FRem {..}   -> "frem" <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata
    FCmp {..}   -> "fcmp" <+> pp fpPredicate <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata

    Alloca {..} -> "alloca" <+> pp allocatedType <> num <> ppAlign alignment <+> ppInstrMeta metadata
      where num   = case numElements of Nothing -> empty
                                        Just o -> "," <+> ppTyped o
    Store {..}  -> "store" <+> ppTyped value `cma` ppTyped address <> ppAlign alignment
    Load {..}   -> "load" <+> pp argTy `cma` ppTyped address <> ppAlign alignment <+> ppInstrMeta metadata
      where PointerType argTy _ = typeOf address
    Phi {..}    -> "phi" <+> pp type' <+> commas (fmap phiIncoming incomingValues) <+> ppInstrMeta metadata

    ICmp {..}   -> "icmp" <+> pp iPredicate <+> ppTyped operand0 `cma` pp operand1 <+> ppInstrMeta metadata

    c@Call {..} -> ppCall c  <+> ppInstrMeta metadata
    Select {..} -> "select" <+> commas [ppTyped condition', ppTyped trueValue, ppTyped falseValue] <+> ppInstrMeta metadata
    SExt {..}   -> "sext" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata <+> ppInstrMeta metadata
    ZExt {..}   -> "zext" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata <+> ppInstrMeta metadata
    FPExt {..}   -> "fpext" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata <+> ppInstrMeta metadata
    Trunc {..}  -> "trunc" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata <+> ppInstrMeta metadata
    FPTrunc {..}  -> "fptrunc" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata <+> ppInstrMeta metadata

    GetElementPtr {..} -> "getelementptr" <+> bounds inBounds <+> commas (pp argTy : fmap ppTyped (address:indices)) <+> ppInstrMeta metadata
      where argTy = getElementType $ typeOf address
    ExtractValue {..} -> "extractvalue" <+> commas (ppTyped aggregate : fmap pp indices') <+> ppInstrMeta metadata

    BitCast {..} -> "bitcast" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata
    FPToUI {..} -> "fptoui" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata
    FPToSI {..} -> "fptosi" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata
    UIToFP {..} -> "uitofp" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata
    SIToFP {..} -> "sitofp" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata
    PtrToInt {..} -> "ptrtoint" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata
    IntToPtr {..} -> "inttoptr" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata

    InsertElement {..} -> "insertelement" <+> commas [ppTyped vector, ppTyped element, ppTyped index] <+> ppInstrMeta metadata
    ShuffleVector {..} -> "shufflevector" <+> commas [ppTyped operand0, ppTyped operand1, ppTyped mask] <+> ppInstrMeta metadata
    ExtractElement {..} -> "extractelement" <+> commas [ppTyped vector, ppTyped index] <+> ppInstrMeta metadata
    InsertValue {..} -> "insertvalue" <+> commas (ppTyped aggregate : ppTyped element : fmap pp indices') <+> ppInstrMeta metadata

    Fence {..} -> "fence" <+> pp atomicity <+> ppInstrMeta metadata
    AtomicRMW {..} -> "atomicrmw" <+> ppVolatile volatile <+> pp rmwOperation <+> ppTyped address `cma` ppTyped value <+> pp atomicity  <+> ppInstrMeta metadata
    CmpXchg {..} -> "cmpxchg" <+> ppVolatile volatile <+> ppTyped address `cma` ppTyped expected `cma` ppTyped replacement
      <+> pp atomicity <+> pp failureMemoryOrdering <+> ppInstrMeta metadata

    AddrSpaceCast {..} -> "addrspacecast" <+> ppTyped operand0 <+> "to" <+> pp type' <+> ppInstrMeta metadata
    VAArg {..} -> "va_arg" <+> ppTyped argList `cma` pp type' <+> ppInstrMeta metadata

    LandingPad {..} ->
      "landingpad" <+> pp type' <+> ppBool "cleanup" cleanup <+> ppInstrMeta metadata
      <+> commas (fmap pp clauses)
    CatchPad {..} -> "catchpad" <+> "within" <+> pp catchSwitch <+> brackets (commas (map ppTyped args)) <+> ppInstrMeta metadata
    CleanupPad {..} -> "cleanuppad" <+> "within" <+> pp parentPad <+> brackets (commas (map ppTyped args)) <+> ppInstrMeta metadata

    where
      bounds True = "inbounds"
      bounds False = empty

instance PP CallableOperand where
  pp (Left asm) = error "CallableOperand"
  pp (Right op) = pp op

instance PP LandingPadClause where
  pp = \case
    Catch c  -> "catch" <+> ppTyped c
    Filter c -> "filter" <+> ppTyped c

instance PP [Either GroupID FunctionAttribute] where
  pp x = hsep $ fmap pp x

instance PP (Either GroupID FunctionAttribute) where
  pp (Left gid) = pp gid
  pp (Right fattr) = pp fattr

instance PP Operand where
  pp (LocalReference _ nm) = local (pp nm)
  pp (ConstantOperand con) = pp con
  pp (MetadataOperand mdata) = pp mdata

instance PP Metadata where
  pp (MDString str) = "!" <> dquotes (text (decodeShortUtf8 str))
  pp (MDNode node) = pp node
  pp (MDValue operand) = pp operand

instance PP MetadataNode where
  pp (MetadataNode xs) = "!" <> braces (commas (fmap ppMetadata xs))
  pp (MetadataNodeReference ref) = pp ref

instance PP C.Constant where
  pp (C.Int width val) = pp val
  pp (C.Float (F.Double val))      =
    if specialFP val
      then "0x" <> (text . pack) (showHex (doubleToWord val) "")
      else text $ pack $ printf "%6.6e" val
  pp (C.Float (F.Single val))      =
    if specialFP val
      then "0x" <> (text . pack) (showHex (floatToWord val) "")
      else text $ pack $ printf "%6.6e" val
  pp (C.Float (F.Half val))        = text $ pack $ printf "%6.6e" val
  pp (C.Float (F.Quadruple val _)) = text $ pack $ printf "%6.6e" val
  pp (C.Float (F.X86_FP80 val _))  = text $ pack $ printf "%6.6e" val
  pp (C.Float (F.PPC_FP128 val _)) = text $ pack $ printf "%6.6e" val

  pp (C.GlobalReference ty nm) = "@" <> pp nm
  pp (C.Vector args) = "<" <+> commas (fmap ppTyped args) <+> ">"

  pp (C.Add {..})    = "add"  <+> ppTyped operand0 `cma` pp operand1
  pp (C.Sub {..})    = "sub"  <+> ppTyped operand0 `cma` pp operand1
  pp (C.Mul {..})    = "mul"  <+> ppTyped operand0 `cma` pp operand1
  pp (C.Shl {..})    = "shl"  <+> ppTyped operand0 `cma` pp operand1
  pp (C.AShr {..})   = "ashr" <+> ppTyped operand0 `cma` pp operand1
  pp (C.LShr {..})   = "lshr" <+> ppTyped operand0 `cma` pp operand1
  pp (C.And {..})    = "and"  <+> ppTyped operand0 `cma` pp operand1
  pp (C.Or {..})     = "or"   <+> ppTyped operand0 `cma` pp operand1
  pp (C.Xor {..})    = "xor"  <+> ppTyped operand0 `cma` pp operand1
  pp (C.SDiv {..})   = "sdiv"  <+> ppTyped operand0 `cma` pp operand1
  pp (C.UDiv {..})   = "udiv"  <+> ppTyped operand0 `cma` pp operand1
  pp (C.SRem {..})   = "srem"  <+> ppTyped operand0 `cma` pp operand1
  pp (C.URem {..})   = "urem"  <+> ppTyped operand0 `cma` pp operand1

  pp (C.FAdd {..})   = "fadd" <+> ppTyped operand0 `cma` pp operand1
  pp (C.FSub {..})   = "fsub" <+> ppTyped operand0 `cma` pp operand1
  pp (C.FMul {..})   = "fmul" <+> ppTyped operand0 `cma` pp operand1
  pp (C.FDiv {..})   = "fdiv" <+> ppTyped operand0 `cma` pp operand1
  pp (C.FRem {..})   = "frem" <+> ppTyped operand0 `cma` pp operand1
  pp (C.FCmp {..})   = "fcmp" <+> pp fpPredicate <+> ppTyped operand0 `cma` pp operand1
  pp C.ICmp {..}     = "icmp" <+> pp iPredicate <+> ppTyped operand0 `cma` pp operand1

  pp (C.Select {..})  = "select" <+> commas [ppTyped condition', ppTyped trueValue, ppTyped falseValue]
  pp (C.SExt {..})    = "sext" <+> ppTyped operand0 <+> "to" <+> pp type'
  pp (C.ZExt {..})    = "zext" <+> ppTyped operand0 <+> "to" <+> pp type'
  pp (C.FPExt {..})   = "fpext" <+> ppTyped operand0 <+> "to" <+> pp type'
  pp (C.Trunc {..})   = "trunc" <+> ppTyped operand0 <+> "to" <+> pp type'
  pp (C.FPTrunc {..}) = "fptrunc" <+> ppTyped operand0 <+> "to" <+> pp type'

  pp C.FPToUI {..} = "fptoui" <+> ppTyped operand0 <+> "to" <+> pp type'
  pp C.FPToSI {..} = "fptosi" <+> ppTyped operand0 <+> "to" <+> pp type'
  pp C.UIToFP {..} = "uitofp" <+> ppTyped operand0 <+> "to" <+> pp type'
  pp C.SIToFP {..} = "sitofp" <+> ppTyped operand0 <+> "to" <+> pp type'

  pp (C.Struct _ packed elems) =
    let struct = spacedbraces $ commas $ fmap ppTyped elems
    in if packed
         then angleBrackets struct
         else struct

  pp (C.Null constantType) = ppNullInitializer constantType

#if MIN_VERSION_llvm_hs_pure(5,1,3)
  pp (C.AggregateZero constantType) = "zeroinitializer"
#endif

  pp (C.Undef {}) = "undef"
  pp (C.TokenNone {}) = "none"
  pp (C.BlockAddress fn blk) = "blockaddress" <> parens (commas (fmap pp [fn, blk]))

  pp C.Array {..}
    | memberType == (IntegerType 8) = "c" <> (dquotes $ hcat [ppIntAsChar val | C.Int _ val <- memberValues])
    | otherwise = brackets $ commas $ fmap ppTyped memberValues

  pp C.GetElementPtr {..} = "getelementptr" <+> bounds inBounds <+> parens (commas (pp argTy : fmap ppTyped (address:indices)))
    where
      PointerType argTy _ = typeOf address
      bounds True = "inbounds"
      bounds False = empty

  pp C.BitCast {..} = "bitcast" <+> parens (ppTyped operand0 <+> "to" <+> pp type')
  pp C.PtrToInt {..} = "ptrtoint" <+> parens (ppTyped operand0 <+> "to" <+> pp type')
  pp C.IntToPtr {..} = "inttoptr" <+> parens (ppTyped operand0 <+> "to" <+> pp type')
  pp C.AddrSpaceCast {..} = "addrspacecast" <+> parens (ppTyped operand0 <+> "to" <+> pp type')

instance PP a => PP (Named a) where
  pp (nm := a) = "%" <> pp nm <+> "=" <+> pp a
  pp (Do a) = pp a

instance PP Module where
  pp Module {..} =
    let header = printf "; ModuleID = '%s'" (unShort moduleName) in
    let target = case moduleTargetTriple of
                      Nothing -> mempty
                      Just target -> "target triple =" <+> dquotes (pp target) in
    let layout = case moduleDataLayout of
                      Nothing     -> mempty
                      Just layout -> "target datalayout =" <+> dquotes (pp layout) in
    hlinecat (fromString header : (layout </> target) : (fmap pp moduleDefinitions))

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

instance PP Atomicity where
  pp (scope, order) =
    pp scope <+> pp order

instance PP SynchronizationScope where
  pp = \case
    SingleThread -> "syncscope(\"singlethreaded\")"
    System -> mempty

instance PP MemoryOrdering where
  pp = \case
    Unordered              -> "unordered"
    Monotonic              -> "monotonic"
    Acquire                -> "acquire"
    Release                -> "release"
    AcquireRelease         -> "acq_rel"
    SequentiallyConsistent -> "seq_cst"

instance PP RMW.RMWOperation where
  pp = \case
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

instance PP DataLayout where
  pp x = pp (BL.unpack (dataLayoutToString x))

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
        pad0 xs = text (pack xs)

ppVolatile :: Bool -> Doc
ppVolatile True = "volatile"
ppVolatile False = mempty

ppIntAsChar :: Integral a => a -> Doc
ppIntAsChar = escape . chr . fromIntegral

ppAlign :: Word32 -> Doc
ppAlign x | x == 0    = empty
          | otherwise = ", align" <+> pp x

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
ppFunctionArgumentTypes _ = error "Non-function argument. (Malformed AST)"

ppNullInitializer :: Type -> Doc
ppNullInitializer PointerType {..} = "zeroinitializer"
ppNullInitializer StructureType {..} = "zeroinitializer"
ppNullInitializer FunctionType {..} = "zeroinitializer"
ppNullInitializer ArrayType {..} = "zeroinitializer"
ppNullInitializer _ = error "Non-pointer argument. (Malformed AST)"

ppCall :: Instruction -> Doc
ppCall Call { function = Right f,..}
  = tail <+> "call" <+> pp callingConvention <+> pp returnAttributes <+> pp resultType <+> ftype
    <+> pp f <> parens (commas $ fmap pp arguments) <+> pp functionAttributes
    where
      (functionType@FunctionType {..}) = referencedType (typeOf f)
      ftype = if isVarArg
              then ppFunctionArgumentTypes functionType
              else empty
      referencedType (PointerType t _) = referencedType t
      referencedType t                 = t

      tail = case tailCallKind of
        Just Tail -> "tail"
        Just MustTail -> "musttail"
        Just NoTail -> "notail"
        Nothing -> empty
ppCall x = error "Non-callable argument. (Malformed AST)"

-- Differs from Call in record name conventions only so needs a seperate almost
-- identical function. :(
ppInvoke :: Terminator -> Doc
ppInvoke Invoke { function' = Right f,..}
  = "invoke" <+> pp callingConvention' <+> pp resultType <+> ftype
    <+> pp f <> parens (commas $ fmap pp arguments') <+> pp functionAttributes'
    where
      (functionType@FunctionType {..}) = referencedType (typeOf f)
      ftype = if isVarArg
              then ppFunctionArgumentTypes functionType
              else empty
      referencedType (PointerType t _) = referencedType t
      referencedType t                 = t
ppInvoke x = error "Non-callable argument. (Malformed AST)"

ppSingleBlock :: BasicBlock -> Doc
ppSingleBlock (BasicBlock nm instrs term) = (vcat $ (fmap pp instrs) ++ [pp term])

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

ppInstrMeta :: InstructionMetadata -> Doc
ppInstrMeta [] = mempty
ppInstrMeta xs = "," <> pp xs

-------------------------------------------------------------------------------
-- Toplevel
-------------------------------------------------------------------------------

-- | Pretty print a LLVM module
ppllvm :: Module -> Text
ppllvm = displayT . renderPretty 0.4 100 . pp

-- | Pretty print a printable LLVM expression
ppll :: PP a => a -> Text
ppll = displayT . renderPretty 0.4 100 . pp
