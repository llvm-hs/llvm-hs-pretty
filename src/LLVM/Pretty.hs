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

import LLVM.Typed

import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.Type

import LLVM.AST.Attribute
import LLVM.AST.COMDAT
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Visibility as V
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.AddrSpace as AS
import qualified LLVM.AST.Float as F
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
import Data.List (intersperse)
import Data.Maybe (isJust)
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

ppMaybe (Just x) = pp x
ppMaybe Nothing = empty

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

instance PP Name where
  pp (Name nm)
   | BS.null nm = dquotes empty
    | isFirst first && all isRest name = text (pack name)
    | otherwise = dquotes . hcat . map escape $ name
    where
        name = unShort nm
        first = head name
        isFirst c = isLetter c || c == '-' || c == '_'
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
  pp (FloatingPointType PPC_FP128FP) = "ppc_f128p"

  pp VoidType = "void"
  pp (PointerType ref addr) = pp ref <> "*"
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
                             <+> kind <+> pp type' <+> ppMaybe initializer <> ppAlign alignment
    where
      hasInitializer = isJust initializer
      kind | isConstant = "constant"
           | otherwise  = "global"

  pp GlobalAlias {..} = global (pp name) <+> "=" <+> pp linkage <+> ppMaybe unnamedAddr <+> "alias" <+> pp typ `cma` ppTyped aliasee
    where
      typ = getElementType type'

instance PP Definition where
  pp (GlobalDefinition x) = pp x
  pp (TypeDefinition nm ty) = local (pp nm) <+> "=" <+> "type" <+> maybe "opaque" pp ty
  pp (FunctionAttributes gid attrs) = "attributes" <+> pp gid <+> "=" <+> braces (hsep (fmap pp attrs))
  pp (NamedMetadataDefinition nm meta) = short nm
  pp (MetadataNodeDefinition node meta) = pp node
  pp (ModuleInlineAssembly asm) = "module asm" <+> dquotes (text (pack (BL.unpack asm)))
  pp (COMDAT name selKind) = "$" <> short name <+> "=" <+> "comdat" <+> pp selKind

instance PP SelectionKind where
  pp Any = "any"
  pp ExactMatch = "exactmatch"
  pp Largest = "largest"
  pp NoDuplicates = "noduplicates"
  pp SameSize = "samesize"

instance PP FunctionAttribute where
  pp = \case
   NoReturn            -> "noreturn"
   NoUnwind            -> "nounwind"
   FA.ReadNone         -> "readnone"
   FA.ReadOnly         -> "readonly"
   FA.WriteOnly        -> "writeonly"
   NoInline            -> "noinline"
   AlwaysInline        -> "alwaysinline"
   MinimizeSize        -> "minimizesize"
   OptimizeForSize     -> "optimizeforsize"
   OptimizeNone        -> "optimizenone"
   SafeStack           -> "safestack"
   StackProtect        -> "ssp"
   StackProtectReq     -> "sspreq"
   StackProtectStrong  -> "sspstrong"
   NoRedZone           -> "noredzone"
   NoImplicitFloat     -> "noimplicitfloat"
   Naked               -> "naked"
   InlineHint          -> "inlinehint"
   StackAlignment n    -> "stackalign"
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
   StringAttribute k v -> dquotes (short k) <> "=" <> dquotes (short v)
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

instance PP MetadataNodeID where
  pp (MetadataNodeID x) = "#" <> int (fromIntegral x)

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
  pp (Br dest meta) = "br" <+> label (pp dest)
  pp (Ret val meta) = "ret" <+> maybe "void" ppTyped val
  pp (CondBr cond tdest fdest meta) =
      "br" <+> ppTyped cond
    `cma` label (pp tdest)
    `cma` label (pp fdest)
  pp (Switch {..}) = "switch" <+> ppTyped operand0'
                   `cma` label (pp defaultDest)
                   <+> brackets (hsep [ ppTyped v `cma` label (pp l) | (v,l) <- dests ])
  pp (Unreachable _) = "unreachable"
  pp (IndirectBr {..}) = error "Not Implemented"
  pp (Invoke {..}) = error "Not Implemented"
  pp (Resume {..}) = error "Not Implemented"
  pp (CleanupRet {..}) = error "Not Implemented"
  pp (CatchRet {..}) = error "Not Implemented"
  pp (CatchSwitch {..}) = error "Not Implemented"

instance PP Instruction where
  pp = \case
    Add {..}    -> "add"  <+> ppTyped operand0 `cma` pp operand1
    Sub {..}    -> "sub"  <+> ppTyped operand0 `cma` pp operand1
    Mul {..}    -> "mul"  <+> ppTyped operand0 `cma` pp operand1
    Shl {..}    -> "shl"  <+> ppTyped operand0 `cma` pp operand1
    AShr {..}   -> "ashr" <+> ppTyped operand0 `cma` pp operand1
    LShr {..}   -> "lshr" <+> ppTyped operand0 `cma` pp operand1
    And {..}    -> "and"  <+> ppTyped operand0 `cma` pp operand1
    Or {..}     -> "or"   <+> ppTyped operand0 `cma` pp operand1
    Xor {..}    -> "xor"  <+> ppTyped operand0 `cma` pp operand1
    SDiv {..}   -> "sdiv"  <+> ppTyped operand0 `cma` pp operand1
    UDiv {..}   -> "udiv"  <+> ppTyped operand0 `cma` pp operand1
    SRem {..}   -> "srem"  <+> ppTyped operand0 `cma` pp operand1
    URem {..}   -> "urem"  <+> ppTyped operand0 `cma` pp operand1

    FAdd {..}   -> "fadd" <+> ppTyped operand0 `cma` pp operand1
    FSub {..}   -> "fsub" <+> ppTyped operand0 `cma` pp operand1
    FMul {..}   -> "fmul" <+> ppTyped operand0 `cma` pp operand1
    FDiv {..}   -> "fdiv" <+> ppTyped operand0 `cma` pp operand1
    FRem {..}   -> "frem" <+> ppTyped operand0 `cma` pp operand1
    FCmp {..}   -> "fcmp" <+> pp fpPredicate <+> ppTyped operand0 `cma` pp operand1

    Alloca {..} -> "alloca" <+> pp allocatedType <> num <> ppAlign alignment
      where num   = case numElements of Nothing -> empty
                                        Just o -> "," <+> ppTyped o
    Store {..}  -> "store" <+> ppTyped value `cma` ppTyped address <> ppAlign alignment
    Load {..}   -> "load" <+> pp argTy `cma` ppTyped address <> ppAlign alignment
      where PointerType argTy _ = typeOf address
    Phi {..}    -> "phi" <+> pp type' <+> commas (fmap phiIncoming incomingValues)

    ICmp {..}   -> "icmp" <+> pp iPredicate <+> ppTyped operand0 `cma` pp operand1

    c@Call {..} -> ppCall c
    Select {..} -> "select" <+> pp condition' <+> pp trueValue <+> pp falseValue
    SExt {..}   -> "sext" <+> ppTyped operand0 <+> "to" <+> pp type'
    ZExt {..}   -> "zext" <+> ppTyped operand0 <+> "to" <+> pp type'
    FPExt {..}   -> "fpext" <+> ppTyped operand0 <+> "to" <+> pp type'
    Trunc {..}  -> "trunc" <+> ppTyped operand0 <+> "to" <+> pp type'
    FPTrunc {..}  -> "fptrunc" <+> ppTyped operand0 <+> "to" <+> pp type'

    GetElementPtr {..} -> "getelementptr" <+> bounds inBounds <+> commas (pp argTy : fmap ppTyped (address:indices))
      where argTy = getElementType $ typeOf address
    ExtractValue {..} -> "extractvalue" <+> commas (ppTyped aggregate : fmap pp indices')

    BitCast {..} -> "bitcast" <+> ppTyped operand0 <+> "to" <+> pp type'
    FPToUI {..} -> "fptoui" <+> ppTyped operand0 <+> "to" <+> pp type'
    FPToSI {..} -> "fptosi" <+> ppTyped operand0 <+> "to" <+> pp type'
    UIToFP {..} -> "uitofp" <+> ppTyped operand0 <+> "to" <+> pp type'
    SIToFP {..} -> "sitofp" <+> ppTyped operand0 <+> "to" <+> pp type'
    PtrToInt {..} -> "ptrtoint" <+> ppTyped operand0 <+> "to" <+> pp type'
    IntToPtr {..} -> "inttoptr" <+> ppTyped operand0 <+> "to" <+> pp type'

    InsertElement {..} -> error "Not implemeneted"
    ShuffleVector {..} -> error "Not implemeneted"
    ExtractElement {..} -> error "Not implemeneted"
    InsertValue {..} -> "insertvalue" <+> commas (ppTyped aggregate : ppTyped element : fmap pp indices')

    Fence {..} -> error "Not implemeneted"
    AtomicRMW {..} -> error "Not implemeneted"
    CmpXchg {..} -> error "Not implemeneted"
    AddrSpaceCast {..} -> error "Not implemeneted"
    VAArg {..} -> error "Not implemeneted"

    LandingPad {..} -> error "Not implemeneted"
    CatchPad {..} -> error "Not implemeneted"
    CleanupPad {..} -> error "Not implemeneted"

    where
      bounds True = "inbounds"
      bounds False = empty

instance PP CallableOperand where
  pp (Left asm) = error "CallableOperand"
  pp (Right op) = pp op

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
  pp (MDString str) = text (decodeShortUtf8 str)
  pp (MDNode node) = pp node
  pp (MDValue operand) = pp operand

instance PP MetadataNode where
  pp (MetadataNode xs) = error "TODO"
  pp (MetadataNodeReference _) = error "TODO"

instance PP C.Constant where
  pp (C.Int width val) = pp val
  pp (C.Float (F.Double val))      = text $ pack $ printf "%6.6e" val
  pp (C.Float (F.Single val))      = text $ pack $ printf "%6.6e" val
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

  pp (C.Select {..})  = "select" <+> pp condition' <+> pp trueValue <+> pp falseValue
  pp (C.SExt {..})    = "sext" <+> ppTyped operand0 <+> "to" <+> pp type'
  pp (C.ZExt {..})    = "zext" <+> ppTyped operand0 <+> "to" <+> pp type'
  pp (C.FPExt {..})   = "fpext" <+> ppTyped operand0 <+> "to" <+> pp type'
  pp (C.Trunc {..})   = "trunc" <+> ppTyped operand0 <+> "to" <+> pp type'
  pp (C.FPTrunc {..}) = "fptrunc" <+> ppTyped operand0 <+> "to" <+> pp type'

  pp C.FPToUI {..} = "fptoui" <+> ppTyped operand0 <+> "to" <+> pp type'
  pp C.FPToSI {..} = "fptosi" <+> ppTyped operand0 <+> "to" <+> pp type'
  pp C.UIToFP {..} = "uitofp" <+> ppTyped operand0 <+> "to" <+> pp type'
  pp C.SIToFP {..} = "sitofp" <+> ppTyped operand0 <+> "to" <+> pp type'

  pp (C.Struct _ _ elems) = spacedbraces $ commas $ fmap ppTyped elems
  pp (C.Null {}) = "zeroinitializer"
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

instance PP a => PP (Named a) where
  pp (nm := a) = "%" <> pp nm <+> "=" <+> pp a
  pp (Do a) = pp a

instance PP Module where
  pp Module {..} =
    let header = printf "; ModuleID = '%s'" (unShort moduleName) in
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
        Nothing -> empty
ppCall x = error "Non-callable argument. (Malformed AST)"

ppSingleBlock :: BasicBlock -> Doc
ppSingleBlock (BasicBlock nm instrs term) = (vcat $ (fmap pp instrs) ++ [pp term])

ppllvm :: Module -> Text
ppllvm = displayT . renderPretty 0.4 100 . pp

ppll :: PP a => a -> Text
ppll = displayT . renderPretty 0.4 100 . pp
