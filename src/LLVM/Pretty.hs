{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}

module LLVM.Pretty (
  renderll, ppllvm, ppll, ppGlobal, ppDefinition, ppBasicBlock, ppTerminator,
  ppInstruction, ppCallableOperand, ppOperand, ppMetadata, ppConstant, ppModule
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
import qualified LLVM.AST.InlineAssembly as IA
import qualified LLVM.AST.AddrSpace as AS
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.RMWOperation as RMW
import LLVM.AST.Operand hiding (DIGLobalVariable(..), GlobalVariable, Module, NoReturn, PointerType)
import qualified LLVM.AST.Operand as O
import LLVM.AST.ParameterAttribute as PA
import LLVM.AST.FunctionAttribute as FA
import LLVM.IRBuilder.Module hiding (global)

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
import Control.Monad (liftM)
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

ppMaybe :: Pretty a => Maybe a -> Doc ann
ppMaybe (Just x) = pretty x
ppMaybe Nothing = mempty

ppEither :: (Pretty a, Pretty b) => MDRef (Either a b) -> Doc ann
ppEither (MDRef r) = pretty r
ppEither (MDInline e) = either pretty pretty e

ppEitherM :: MonadModuleBuilder m => (a -> m (Doc ann)) -> (b -> m (Doc ann)) -> MDRef (Either a b) -> m (Doc ann)
ppEitherM ppA ppB (MDRef r) = return $ pretty r
ppEitherM ppA ppB (MDInline (Left x)) = ppA x
ppEitherM ppA ppB (MDInline (Right x)) = ppB x

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

instance Pretty BS.ShortByteString where
  pretty = pretty . unShort

ppBoolean :: Bool -> Doc ann
ppBoolean True = "true"
ppBoolean False = "false"

-- print an operand and its type
ppTyped :: (MonadModuleBuilder m, Pretty a, Typed a) => a -> m (Doc ann)
ppTyped a = do
  ta <- typeOf a
  return $ pretty ta <+> pretty a

ppTypedM :: (MonadModuleBuilder m, Typed a) => (a -> m (Doc ann)) -> a -> m (Doc ann)
ppTypedM pp a = do
  ta <- typeOf a
  prettyA <- pp a
  return $ pretty ta <+> prettyA

ppCommaTyped :: (MonadModuleBuilder m, Pretty a, Typed a) => a -> m (Doc ann)
ppCommaTyped a = do
  ta <- typeOf a
  return $ pretty ta `cma` pretty a

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

ppArguments :: MonadModuleBuilder m => (Operand, [ParameterAttribute]) -> m (Doc ann)
ppArguments (op, attrs) = do
  tOp <- typeOf op
  prettyOp <- ppOperand op
  return $ pretty tOp <+> ppParamAttributes attrs <+> prettyOp

ppParams :: (a -> Doc ann) -> ([a], Bool) -> Doc ann
ppParams ppParam (ps, varrg) = parens . commas $ fmap ppParam ps ++ vargs
  where
    vargs = if varrg then ["..."] else []

ppParamsM :: MonadModuleBuilder m => (a -> m (Doc ann)) -> ([a], Bool) -> m (Doc ann)
ppParamsM ppParam (ps, varrg) = do
  prettyParams <- mapM ppParam ps
  return $ parens $ commas $ prettyParams ++ vargs
  where
    vargs = if varrg then ["..."] else []

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

ppGlobal :: MonadModuleBuilder m => Global -> m (Doc ann)
ppGlobal g
  | Function {..} <- g = do
      pre <- case prefix of
               Nothing  -> pure mempty
               Just con -> do
                 ppCon <- ppTypedM ppConstant con
                 return $ "prefix" <+> ppCon
      let align = if alignment == 0 then mempty
                  else "align" <+> pretty alignment
      let gcName = maybe mempty (\n -> "gc" <+> dquotes (pretty $ pack n)) (fmap unShort garbageCollectorName)
      case basicBlocks of
        [] -> do
          prettyParams <- ppParamsM (liftM pretty . typeOf) parameters
          return $ ("declare" <+> pretty linkage <+> pretty callingConvention
            <+> ppReturnAttributes returnAttributes <+> pretty returnType <+> global (pretty name)
            <> prettyParams <+> ppFunctionAttributes functionAttributes <+> align <+> gcName <+> pre)

        -- single unnamed block is special cased, and won't parse otherwise... yeah good times
        [b@(BasicBlock (UnName _) _ _)] -> do
          prettyBB <- ppSingleBlock b
          return $ (("define" <+> pretty linkage <+> pretty callingConvention
                              <+> ppReturnAttributes returnAttributes <+> pretty returnType <+> global (pretty name)
                              <> ppParams pretty parameters <+> ppFunctionAttributes functionAttributes <+> align <+> gcName <+> pre)
                    `wrapbraces` (indent 2 prettyBB))

        bs -> do
          prettyBBs <- mapM ppBasicBlock bs
          return $ (("define" <+> pretty linkage <+> pretty callingConvention
                              <+> ppReturnAttributes returnAttributes <+> pretty returnType <+> global (pretty name)
                              <> ppParams pretty parameters <+> ppFunctionAttributes functionAttributes <+> align <+> gcName <+> pre)
                    `wrapbraces` (vcat prettyBBs))

  | GlobalVariable {..} <- g = do
      let hasInitializer = isJust initializer
      let addrSpace' = case addrSpace of
                         AS.AddrSpace addr
                           | addr == 0 -> mempty
                           | otherwise -> "addrspace" <> parens (pretty addr)
      let kind = if isConstant then "constant" else "global"
      prettyInit <- case initializer of { Just x -> ppConstant x; Nothing -> pure mempty }
      return $ global (pretty name) <+> "=" <+> ppLinkage hasInitializer linkage <+> ppMaybe unnamedAddr
               <+> addrSpace' <+> kind <+> pretty type' <+> prettyInit <> ppAlign alignment

  | GlobalAlias {} <- g = do
    let typ = getElementType $ LLVM.AST.Global.type' g
    prettyAliasee <- ppConstant $ LLVM.AST.Global.aliasee g
    return $ global (pretty $ LLVM.AST.Global.name g) <+> "=" <+> pretty (linkage g) <+> ppMaybe (unnamedAddr g) <+> "alias" <+> pretty typ `cma` prettyAliasee

ppFunctionAttribute :: Either GroupID FunctionAttribute -> Doc ann
ppFunctionAttribute (Left grpId) = pretty grpId
ppFunctionAttribute (Right fA) = pretty fA

ppFunctionAttributes :: [Either GroupID FunctionAttribute] -> Doc ann
ppFunctionAttributes attribs = hsep $ fmap ppFunctionAttribute attribs

ppMaybeMetadata :: MonadModuleBuilder m => Maybe Metadata -> m (Doc ann)
ppMaybeMetadata Nothing = pure "null"
ppMaybeMetadata (Just m) = ppMetadata m

ppDefinition :: MonadModuleBuilder m => Definition -> m (Doc ann)
ppDefinition d
  | GlobalDefinition x <- d = ppGlobal x
  | TypeDefinition nm ty <- d = return $ local' (pretty nm) <+> "=" <+> "type" <+> maybe "opaque" pretty ty
  | FunctionAttributes gid attrs <- d = return $ "attributes" <+> pretty gid <+> "=" <+> braces (hsep (fmap ppAttrInGroup attrs))
  | NamedMetadataDefinition nm meta <- d = return $ "!" <> short nm <+> "=" <+> "!" <> braces (commas (fmap pretty meta))
  | MetadataNodeDefinition node meta <- d = do
      prettyMeta <- ppMDNode meta
      return $ pretty node <+> "=" <+> prettyMeta
  | ModuleInlineAssembly asm <- d = return $ "module asm" <+> dquotes (pretty (pack (BL.unpack asm)))
  | COMDAT name selKind <- d = return $ "$" <> short name <+> "=" <+> "comdat" <+> pretty selKind

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
   FA.NoFree           -> "nofree"
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
    PA.NoFree                  -> "nofree"
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

instance Pretty MetadataNodeID where
  pretty (MetadataNodeID x) = "!" <> pretty ((fromIntegral x) :: Int)

instance Pretty GroupID where
  pretty (GroupID x) = "#" <> pretty ((fromIntegral x) :: Int)

ppBasicBlock :: MonadModuleBuilder m => BasicBlock -> m (Doc ann)
ppBasicBlock (BasicBlock nm instrs term) = do
  prettyInstrs <- mapM (ppNamed ppInstruction) instrs
  prettyTerm <- ppNamed ppTerminator term
  return $ label <> P.line <> indent 2 (vcat $ prettyInstrs ++ [prettyTerm])
  where
    label = case nm of
      UnName _ -> "; <label>:" <> pretty nm <> ":"
      _ -> pretty nm <> ":"

ppTerminator :: MonadModuleBuilder m => Terminator -> m (Doc ann)
ppTerminator t
  | Br dest meta <- t = do
      prettyMeta <- ppInstrMeta meta
      return $ "br" <+> label (pretty dest) <+> prettyMeta

  | Ret val meta <- t = do
      prettyVal <- maybe (pure "void") (ppTypedM ppOperand) val
      prettyMeta <- ppInstrMeta meta
      return $ "ret" <+> prettyVal <+> prettyMeta

  | CondBr cond tdest fdest meta <- t = do
      prettyCond <- ppTypedM ppOperand cond
      prettyMeta <- ppInstrMeta meta
      return $ "br" <+> prettyCond
               `cma` label (pretty tdest)
               `cma` label (pretty fdest)
               <+> prettyMeta

  | Switch {..} <- t = do
      prettyOperand0' <- ppTypedM ppOperand operand0'
      prettyDests <- mapM (\(v,l) -> do prettyV <- ppTypedM ppConstant v
                                        return $ prettyV `cma` label (pretty l)) dests
      prettyMeta <- ppInstrMeta metadata'
      return $ "switch" <+> prettyOperand0'
               `cma` label (pretty defaultDest)
               <+> brackets (hsep prettyDests)
               <+> prettyMeta

  | Unreachable {..} <- t = do
      prettyMeta <- ppInstrMeta metadata'
      return $ "unreachable" <+> prettyMeta

  | IndirectBr op dests meta <- t = do
      prettyOp <- ppTypedM ppOperand op
      prettyMeta <- ppInstrMeta meta
      return $ "indirectbr" <+> prettyOp `cma`
               brackets (hsep [ label (pretty l) | l <- dests ])
               <+> prettyMeta

  | Invoke {..} <- t = do
      prettyInv <- ppInvoke t
      prettyMeta <- ppInstrMeta metadata'
      return $ prettyInv
               <+> "to" <+> label (pretty returnDest)
               <+> "unwind" <+> label (pretty exceptionDest)
               <+> prettyMeta

  | Resume op meta <- t = do
      prettyOp <- ppTypedM ppOperand op
      prettyMeta <- ppInstrMeta meta
      return $ "resume "<+> prettyOp <+> prettyMeta

  | CleanupRet pad dest meta <- t = do
      prettyPad <- ppOperand pad
      prettyMeta <- ppInstrMeta meta
      return $ "cleanupret" <+> "from"
                            <+> prettyPad <+> "unwind"
                            <+> maybe "to caller" (label . pretty) dest
                            <+> prettyMeta

  | CatchRet catchPad succ meta <- t = do
      prettyPad <- ppOperand catchPad
      prettyMeta <- ppInstrMeta meta
      return $ "catchret" <+> "from"
                          <+> prettyPad
                          <+> "to" <+> label (pretty succ)
                          <+> prettyMeta

  | CatchSwitch {..} <- t = do
      prettyPad <- ppOperand parentPad'
      prettyMeta <- ppInstrMeta metadata'
      return $ "catchswitch" <+> "within" <+> prettyPad
                             <+> brackets (commas (map (label . pretty) (toList catchHandlers)))
                             <+> "unwind" <+> "to"
                             <+> maybe "caller" pretty defaultUnwindDest
                             <+> prettyMeta

ppInstruction :: MonadModuleBuilder m => Instruction -> m (Doc ann)
ppInstruction i
  | Add {..}    <- i = ppInstrWithNuwNsw "add" nuw nsw operand0 operand1 metadata
  | Sub {..}    <- i = ppInstrWithNuwNsw "sub" nuw nsw operand0 operand1 metadata
  | Mul {..}    <- i = ppInstrWithNuwNsw "mul" nuw nsw operand0 operand1 metadata
  | Shl {..}    <- i = ppInstrWithNuwNsw "shl" nuw nsw operand0 operand1 metadata
  | AShr {..}   <- i = ppInstrWithExact "ashr" exact operand0 operand1 metadata
  | LShr {..}   <- i = ppInstrWithExact "lshr" exact operand0 operand1 metadata

  | And {..}    <- i = do { prettyMeta <- ppInstrMeta metadata; prettyOp0 <- ppTypedM ppOperand operand0; prettyOp1 <- ppOperand operand1; return $ "and"  <+> prettyOp0 `cma` prettyOp1 <+> prettyMeta }
  | Or {..}     <- i = do { prettyMeta <- ppInstrMeta metadata; prettyOp0 <- ppTypedM ppOperand operand0; prettyOp1 <- ppOperand operand1; return $ "or"   <+> prettyOp0 `cma` prettyOp1 <+> prettyMeta }
  | Xor {..}    <- i = do { prettyMeta <- ppInstrMeta metadata; prettyOp0 <- ppTypedM ppOperand operand0; prettyOp1 <- ppOperand operand1; return $ "xor"  <+> prettyOp0 `cma` prettyOp1 <+> prettyMeta }
  | SDiv {..}   <- i = ppInstrWithExact "sdiv" exact operand0 operand1 metadata
  | UDiv {..}   <- i = ppInstrWithExact "udiv" exact operand0 operand1 metadata
  | SRem {..}   <- i = do { prettyMeta <- ppInstrMeta metadata; prettyOp0 <- ppTypedM ppOperand operand0; prettyOp1 <- ppOperand operand1; return $ "srem"  <+> prettyOp0 `cma` prettyOp1 <+> prettyMeta }
  | URem {..}   <- i = do { prettyMeta <- ppInstrMeta metadata; prettyOp0 <- ppTypedM ppOperand operand0; prettyOp1 <- ppOperand operand1; return $ "urem"  <+> prettyOp0 `cma` prettyOp1 <+> prettyMeta }

  | FAdd {..}   <- i = do { prettyMeta <- ppInstrMeta metadata; prettyOp0 <- ppTypedM ppOperand operand0; prettyOp1 <- ppOperand operand1; return $ "fadd" <+> (pretty fastMathFlags) <+> prettyOp0 `cma` prettyOp1 <+> prettyMeta }
  | FSub {..}   <- i = do { prettyMeta <- ppInstrMeta metadata; prettyOp0 <- ppTypedM ppOperand operand0; prettyOp1 <- ppOperand operand1; return $ "fsub" <+> (pretty fastMathFlags) <+> prettyOp0 `cma` prettyOp1 <+> prettyMeta }
  | FMul {..}   <- i = do { prettyMeta <- ppInstrMeta metadata; prettyOp0 <- ppTypedM ppOperand operand0; prettyOp1 <- ppOperand operand1; return $ "fmul" <+> (pretty fastMathFlags) <+> prettyOp0 `cma` prettyOp1 <+> prettyMeta }
  | FDiv {..}   <- i = do { prettyMeta <- ppInstrMeta metadata; prettyOp0 <- ppTypedM ppOperand operand0; prettyOp1 <- ppOperand operand1; return $ "fdiv" <+> (pretty fastMathFlags) <+> prettyOp0 `cma` prettyOp1 <+> prettyMeta }
  | FRem {..}   <- i = do { prettyMeta <- ppInstrMeta metadata; prettyOp0 <- ppTypedM ppOperand operand0; prettyOp1 <- ppOperand operand1; return $ "frem" <+> (pretty fastMathFlags) <+> prettyOp0 `cma` prettyOp1 <+> prettyMeta }
  | FCmp {..}   <- i = do { prettyMeta <- ppInstrMeta metadata; prettyOp0 <- ppTypedM ppOperand operand0; prettyOp1 <- ppOperand operand1; return $ "fcmp" <+> pretty fpPredicate <+> prettyOp0 `cma` prettyOp1 <+> prettyMeta }

  | Alloca {..} <- i = do
      num <- case numElements of Nothing -> pure mempty
                                 Just o -> do { prettyO <- ppTypedM ppOperand o; return $ "," <+> prettyO }
      prettyMeta <- ppInstrMeta metadata
      return $ "alloca" <+> pretty allocatedType <> num <> ppAlign alignment <+> prettyMeta
  | Store {..}  <- i = do
      prettyValue <- ppTypedM ppOperand value
      prettyAddr <- ppTypedM ppOperand address
      prettyMeta <- ppInstrMeta metadata
      return $ "store" <+> ppMAtomicity maybeAtomicity <+> ppVolatile volatile <+> prettyValue `cma` prettyAddr <+> ppMOrdering maybeAtomicity <> ppAlign alignment <+> prettyMeta
  | Load {..}   <- i = do
      ta <-typeOf address
      let argTy = case ta of
                    PointerType {} -> pointerReferent argTy
                    _ -> error "invalid load of non-pointer type. (Malformed AST)"
      prettyAddr <- ppTypedM ppOperand address
      prettyMeta <- ppInstrMeta metadata
      return $ "load" <+> ppMAtomicity maybeAtomicity <+> ppVolatile volatile <+> pretty argTy `cma` prettyAddr <+> ppMOrdering maybeAtomicity <> ppAlign alignment <+> prettyMeta

  | Phi {..}    <- i = do
      prettyInc <- mapM phiIncoming incomingValues
      prettyMeta <- ppInstrMeta metadata
      return $ "phi" <+> pretty type' <+> commas prettyInc <+> prettyMeta

  | ICmp {..}   <- i = do { prettyMeta <- ppInstrMeta metadata; prettyOp0 <- ppTypedM ppOperand operand0; prettyOp1 <- ppOperand operand1; return $ "icmp" <+> pretty iPredicate <+> prettyOp0 `cma` prettyOp1 <+> prettyMeta }

  | Call {..} <- i = do
      prettyCall <- ppCall i
      prettyMeta <- ppInstrMeta metadata
      return $ prettyCall <+> prettyMeta
  | Select {..} <- i = do
      prettyCond  <- ppTypedM ppOperand condition'
      prettyTrue  <- ppTypedM ppOperand trueValue
      prettyFalse <- ppTypedM ppOperand falseValue
      prettyMeta <- ppInstrMeta metadata
      return $ "select" <+> commas [prettyCond, prettyTrue, prettyFalse] <+> prettyMeta
  | SExt {..}     <- i = do { prettyMeta <- ppInstrMeta metadata; prettyOp0 <- ppTypedM ppOperand operand0; return $ "sext" <+> prettyOp0 <+> "to" <+> pretty type' <+> prettyMeta }
  | ZExt {..}     <- i = do { prettyMeta <- ppInstrMeta metadata; prettyOp0 <- ppTypedM ppOperand operand0; return $ "zext" <+> prettyOp0 <+> "to" <+> pretty type' <+> prettyMeta }
  | FPExt {..}    <- i = do { prettyMeta <- ppInstrMeta metadata; prettyOp0 <- ppTypedM ppOperand operand0; return $ "fpext" <+> prettyOp0 <+> "to" <+> pretty type' <+> prettyMeta }
  | Trunc {..}    <- i = do { prettyMeta <- ppInstrMeta metadata; prettyOp0 <- ppTypedM ppOperand operand0; return $ "trunc" <+> prettyOp0 <+> "to" <+> pretty type' <+> prettyMeta }
  | FPTrunc {..}  <- i = do { prettyMeta <- ppInstrMeta metadata; prettyOp0 <- ppTypedM ppOperand operand0; return $ "fptrunc" <+> prettyOp0 <+> "to" <+> pretty type' <+> prettyMeta }

  | GetElementPtr {..} <- i = do
      ta <-typeOf address
      let argTy = getElementType ta
      prettyAIs <- mapM (ppTypedM ppOperand) (address:indices)
      prettyMeta <- ppInstrMeta metadata
      return $ "getelementptr" <+> bounds inBounds <+> commas (pretty argTy : prettyAIs) <+> prettyMeta

  | ExtractValue {..} <- i = do
      prettyAgg <- ppTypedM ppOperand aggregate
      prettyMeta <- ppInstrMeta metadata
      return $ "extractvalue" <+> commas (prettyAgg : fmap pretty indices') <+> prettyMeta

  | BitCast {..}  <- i = do { prettyMeta <- ppInstrMeta metadata; prettyOp0 <- ppTypedM ppOperand operand0; return $ "bitcast" <+> prettyOp0 <+> "to" <+> pretty type' <+> prettyMeta }
  | FPToUI {..}   <- i = do { prettyMeta <- ppInstrMeta metadata; prettyOp0 <- ppTypedM ppOperand operand0; return $ "fptoui" <+> prettyOp0 <+> "to" <+> pretty type' <+> prettyMeta }
  | FPToSI {..}   <- i = do { prettyMeta <- ppInstrMeta metadata; prettyOp0 <- ppTypedM ppOperand operand0; return $ "fptosi" <+> prettyOp0 <+> "to" <+> pretty type' <+> prettyMeta }
  | UIToFP {..}   <- i = do { prettyMeta <- ppInstrMeta metadata; prettyOp0 <- ppTypedM ppOperand operand0; return $ "uitofp" <+> prettyOp0 <+> "to" <+> pretty type' <+> prettyMeta }
  | SIToFP {..}   <- i = do { prettyMeta <- ppInstrMeta metadata; prettyOp0 <- ppTypedM ppOperand operand0; return $ "sitofp" <+> prettyOp0 <+> "to" <+> pretty type' <+> prettyMeta }
  | PtrToInt {..} <- i = do { prettyMeta <- ppInstrMeta metadata; prettyOp0 <- ppTypedM ppOperand operand0; return $ "ptrtoint" <+> prettyOp0 <+> "to" <+> pretty type' <+> prettyMeta }
  | IntToPtr {..} <- i = do { prettyMeta <- ppInstrMeta metadata; prettyOp0 <- ppTypedM ppOperand operand0; return $ "inttoptr" <+> prettyOp0 <+> "to" <+> pretty type' <+> prettyMeta }

  | InsertElement {..} <- i = do
      prettyVec <- ppTypedM ppOperand vector
      prettyElt <- ppTypedM ppOperand element
      prettyInd <- ppTypedM ppOperand index
      prettyMeta <- ppInstrMeta metadata
      return $ "insertelement" <+> commas [prettyVec, prettyElt, prettyInd] <+> prettyMeta
  | ShuffleVector {..} <- i = do
      prettyOp0 <- ppTypedM ppOperand operand0
      prettyOp1 <- ppTypedM ppOperand operand1
      prettyMask <- ppTypedM ppConstant mask
      prettyMeta <- ppInstrMeta metadata
      return $ "shufflevector" <+> commas [prettyOp0, prettyOp1, prettyMask] <+> prettyMeta
  | ExtractElement {..} <- i = do
      prettyVec <- ppTypedM ppOperand vector
      prettyInd <- ppTypedM ppOperand index
      prettyMeta <- ppInstrMeta metadata
      return $ "extractelement" <+> commas [prettyVec, prettyInd] <+> prettyMeta
  | InsertValue {..} <- i = do
      prettyAgg <- ppTypedM ppOperand aggregate
      prettyElt <- ppTypedM ppOperand element
      prettyMeta <- ppInstrMeta metadata
      return $ "insertvalue" <+> commas (prettyAgg : prettyElt : fmap pretty indices') <+> prettyMeta

  | Fence {..} <- i = do
      prettyMeta <- ppInstrMeta metadata
      return $ "fence" <+> ppAtomicity atomicity <+> prettyMeta
  | AtomicRMW {..} <- i = do
      prettyAddr <- ppTypedM ppOperand address
      prettyVal <- ppTypedM ppOperand value
      prettyMeta <- ppInstrMeta metadata
      return $ "atomicrmw" <+> ppVolatile volatile <+> pretty rmwOperation <+> prettyAddr `cma` prettyVal <+> ppAtomicity atomicity  <+> prettyMeta
  | CmpXchg {..} <- i = do
      prettyAddr <- ppTypedM ppOperand address
      prettyExp <- ppTypedM ppOperand expected
      prettyRep <- ppTypedM ppOperand replacement
      prettyMeta <- ppInstrMeta metadata
      return $ "cmpxchg" <+> ppVolatile volatile <+> prettyAddr `cma` prettyExp `cma` prettyRep
               <+> ppAtomicity atomicity <+> pretty failureMemoryOrdering <+> prettyMeta

  | AddrSpaceCast {..} <- i = do
      prettyOp0 <- ppTypedM ppOperand operand0
      prettyMeta <- ppInstrMeta metadata
      return $ "addrspacecast" <+> prettyOp0 <+> "to" <+> pretty type' <+> prettyMeta
  | VAArg {..} <- i = do
      prettyArgList <- ppTypedM ppOperand argList
      prettyMeta <- ppInstrMeta metadata
      return $ "va_arg" <+> prettyArgList `cma` pretty type' <+> prettyMeta

  | LandingPad {..} <- i = do
      prettyClauses <- mapM ppLandingPadClause clauses
      prettyMeta <- ppInstrMeta metadata
      return $ "landingpad" <+> pretty type' <+> ppBool "cleanup" cleanup <+> prettyMeta
                            <+> commas prettyClauses
  | CatchPad {..} <- i = do
      prettyArgs <- mapM (ppTypedM ppOperand) args
      prettyCS <- ppOperand catchSwitch
      prettyMeta <- ppInstrMeta metadata
      return $ "catchpad" <+> "within" <+> prettyCS <+> brackets (commas prettyArgs) <+> prettyMeta
  | CleanupPad {..} <- i = do
      prettyArgs <- mapM (ppTypedM ppOperand) args
      prettyPP <- ppOperand parentPad
      prettyMeta <- ppInstrMeta metadata
      return $ "cleanuppad" <+> "within" <+> prettyPP <+> brackets (commas prettyArgs) <+> prettyMeta

  where
    bounds True = "inbounds"
    bounds False = mempty

    ppInstrWithNuwNsw :: MonadModuleBuilder m => Doc ann -> Bool -> Bool -> Operand -> Operand -> InstructionMetadata -> m (Doc ann)
    ppInstrWithNuwNsw name nuw nsw op0 op1 metadata = do
      prettyOp0 <- ppTypedM ppOperand op0
      prettyOp1 <- ppOperand op1
      prettyMeta <- ppInstrMeta metadata
      return $ name
               <+> ppBool "nuw" nuw
               <+> ppBool "nsw" nsw
               <+> prettyOp0
               `cma` prettyOp1
               <+> prettyMeta

    ppInstrWithExact :: MonadModuleBuilder m => Doc ann -> Bool -> Operand -> Operand -> InstructionMetadata -> m (Doc ann)
    ppInstrWithExact name exact op0 op1 metadata = do
      prettyOp0 <- ppTypedM ppOperand op0
      prettyOp1 <- ppOperand op1
      prettyMeta <- ppInstrMeta metadata
      return $ name
               <+> ppBool "exact" exact
               <+> prettyOp0
               `cma` prettyOp1
               <+> prettyMeta

ppCallableOperand :: MonadModuleBuilder m => CallableOperand -> m (Doc ann)
ppCallableOperand (Left asm) = error "Inline assembly is not a valid CallableOperand (Malformed AST)"
ppCallableOperand (Right op) = ppOperand op

ppLandingPadClause :: MonadModuleBuilder m => LandingPadClause -> m (Doc ann)
ppLandingPadClause l
  | Catch c  <- l = do { prettyC <- ppTypedM ppConstant c; return $ "catch" <+> prettyC }
  | Filter c <- l = do { prettyC <- ppTypedM ppConstant c; return $ "filter" <+> prettyC }

instance Pretty (Either GroupID FunctionAttribute) where
  pretty (Left gid) = pretty gid
  pretty (Right fattr) = pretty fattr

ppOperand :: MonadModuleBuilder m => Operand -> m (Doc ann)
ppOperand (LocalReference _ nm) = return $ local' (pretty nm)
ppOperand (ConstantOperand con) = ppConstant con
ppOperand (MetadataOperand mdata) = ppMetadata mdata

ppMetadata :: MonadModuleBuilder m => Metadata -> m (Doc ann)
ppMetadata (MDString str) = return $ "!" <> dquotes (pretty (decodeShortUtf8 str))
ppMetadata (MDNode node) = ppMDRefM node ppMDNode
ppMetadata (MDValue operand) = ppTypedM ppOperand operand

ppDINode :: [Char] -> [([Char], Maybe (Doc ann))] -> Doc ann
ppDINode name attrs = "!" <> pretty name <> parens (commas (mapMaybe (\(k, mayV) -> fmap (\v -> pretty k <> ":" <+> v) mayV) attrs))

ppDIArray :: [Doc ann] -> Maybe (Doc ann)
ppDIArray [] = Nothing
ppDIArray xs = Just ("!" <> braces (commas xs))

instance Pretty a => Pretty (MDRef a) where
  pretty (MDInline a) = pretty a
  pretty (MDRef ref) = pretty ref

ppMDRefM :: MonadModuleBuilder m => MDRef a -> (a -> m (Doc ann)) -> m (Doc ann)
ppMDRefM (MDInline a) f = f a
ppMDRefM (MDRef ref) _ = return $ pretty ref

ppMDNode :: MonadModuleBuilder m => MDNode -> m (Doc ann)
ppMDNode (MDTuple xs) = do
  prettyMeta <- mapM ppMaybeMetadata xs
  return $ "!" <> braces (commas prettyMeta)
ppMDNode (DIExpression e) = return $ pretty e
ppMDNode (DIGlobalVariableExpression e) = ppDIGlobalVariableExpression e
ppMDNode (DILocation l) = ppDILocation l
ppMDNode (DIMacroNode m) = return $ pretty m
ppMDNode (DINode n) = ppDINodeM n

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

ppDIGlobalVariableExpression :: MonadModuleBuilder m => DIGlobalVariableExpression -> m (Doc ann)
ppDIGlobalVariableExpression e = do
  prettyV <- ppMDRefM (var e) ppDIGlobalVariable
  prettyE <- ppMDRefM (expr e) (pure . pretty)
  return $ ppDINode "DIGlobalVariableExpression"
    [ ("var", Just prettyV)
    , ("expr", Just prettyE)
    ]

ppDILocation :: MonadModuleBuilder m => DILocation -> m (Doc ann)
ppDILocation (Location line col scope) = do
  prettyScope <- ppMDRefM scope ppDILocalScope
  return $ ppDINode "DILocation" [("line", Just (pretty line)), ("column", Just (pretty col)), ("scope", Just prettyScope)]

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

ppDINodeM :: MonadModuleBuilder m => DINode -> m (Doc ann)
ppDINodeM (DIEnumerator e) = return $ pretty e
ppDINodeM (DIImportedEntity e) = ppDIImportedEntity e
ppDINodeM (DIObjCProperty p) = ppDIObjCProperty p
ppDINodeM (DIScope s) = ppDIScope s
ppDINodeM (DISubrange r) = ppDISubrange r
ppDINodeM (DITemplateParameter p) = ppDITemplateParameter p
ppDINodeM (DIVariable v) = ppDIVariable v

ppDILocalScope :: MonadModuleBuilder m => DILocalScope -> m (Doc ann)
ppDILocalScope (DILexicalBlockBase b) = ppDILexicalBlockBase b
ppDILocalScope (DISubprogram p) = ppDISubprogram p

instance Pretty DIEnumerator where
  pretty (Enumerator val unsigned name) =
    ppDINode "DIEnumerator"
      [ ("name", ppSbs name)
      , ("isUnsigned", if unsigned then Just "true" else Nothing)
      , ("value", Just (pretty val))]

ppDIImportedEntity :: MonadModuleBuilder m => DIImportedEntity -> m (Doc ann)
ppDIImportedEntity (ImportedEntity {..}) = do
  prettyEntity <- case entity of {Just x -> do { x' <- ppMDRefM x ppDINodeM; return $ Just x' }; Nothing -> pure Nothing }
  prettyScope <- ppMDRefM scope ppDIScope
  return $ ppDINode "DIImportedEntity"
    [ ("tag", Just (pretty tag))
    , ("scope", Just prettyScope)
    , ("name", ppSbs name)
    , ("entity", prettyEntity)
    , ("file", fmap pretty file)
    , ("line", Just (pretty line))
    ]

instance Pretty ImportedEntityTag where
  pretty ImportedModule = "DW_TAG_imported_module"
  pretty ImportedDeclaration = "DW_TAG_imported_declaration"

ppDIObjCProperty :: MonadModuleBuilder m => DIObjCProperty -> m (Doc ann)
ppDIObjCProperty ObjCProperty {..} = do
  prettyType <- case type' of {Just x -> do { x' <- ppMDRefM x ppDIType; return $ Just x' }; Nothing -> pure Nothing }
  return $ ppDINode "DIObjCProperty"
    [ ("name", ppSbs name)
    , ("file", fmap pretty file)
    , ("line", Just (pretty line))
    , ("setter", ppSbs getterName)
    , ("getter", ppSbs setterName)
    , ("attributes", Just (pretty attributes))
    , ("type", prettyType)
    ]

ppDIScope :: MonadModuleBuilder m => DIScope -> m (Doc ann)
ppDIScope (DICompileUnit cu) = ppDICompileUnit cu
ppDIScope (DIFile f) = return $ pretty f
ppDIScope (DILocalScope s) = ppDILocalScope s
ppDIScope (DIModule m) = ppDIModule m
ppDIScope (DINamespace ns) = ppDINamespace ns
ppDIScope (DIType t) = ppDIType t

ppDISubrange :: MonadModuleBuilder m => DISubrange -> m (Doc ann)
ppDISubrange Subrange {..} = do
  prettyCount <- ppDICount count
  return $ ppDINode "DISubrange" [("count", Just prettyCount), ("lowerBound", Just (pretty lowerBound))]

ppDICount :: MonadModuleBuilder m => DICount -> m (Doc ann)
ppDICount (DICountConstant c) = pure $ pretty c
ppDICount (DICountVariable v) = ppMDRefM v ppDIVariable

ppDITemplateParameter :: MonadModuleBuilder m => DITemplateParameter -> m (Doc ann)
ppDITemplateParameter (DITemplateTypeParameter {..}) = do
  prettyType <- ppMDRefM (case type' of { (Just t) -> t; Nothing -> error "type field in DITemplateTypeParameter is required (malformed AST)" }) ppDIType
  return $ ppDINode "DITemplateTypeParameter" [ ("name", ppSbs name), ("type", Just prettyType) ]
ppDITemplateParameter (DITemplateValueParameter {..}) = do
  prettyType <- case type' of {Just x -> do { x' <- ppMDRefM x ppDIType; return $ Just x' }; Nothing -> pure Nothing }
  prettyValue <- ppMaybeMetadata value
  return $ ppDINode "DITemplateValueParameter"
    [ ("tag", ppTemplateValueParameterTag tag)
    , ("name", ppSbs name)
    , ("type", prettyType)
    , ("value", Just prettyValue)
    ]

ppTemplateValueParameterTag :: TemplateValueParameterTag -> Maybe (Doc ann)
ppTemplateValueParameterTag TemplateValueParameter = Nothing
ppTemplateValueParameterTag GNUTemplateTemplateParam = Just "DW_TAG_GNU_template_template_param"
ppTemplateValueParameterTag GNUTemplateParameterPack = Just "DW_TAG_GNU_template_parameter_pack"

ppDIVariable :: MonadModuleBuilder m => DIVariable -> m (Doc ann)
ppDIVariable (DIGlobalVariable v) = ppDIGlobalVariable v
ppDIVariable (DILocalVariable v) = ppDILocalVariable v

ppDICompileUnit :: MonadModuleBuilder m => DICompileUnit -> m (Doc ann)
ppDICompileUnit cu@CompileUnit {..} = do
  prettyImports <- mapM (flip ppMDRefM ppDIImportedEntity) imports
  prettyEnums <- mapM (flip ppMDRefM ppDICompositeType) enums
  prettyRTs <- mapM (ppEitherM ppDIType ppDISubprogram) retainedTypes
  prettyGlobals <- mapM (flip ppMDRefM ppDIGlobalVariableExpression) globals
  return $ "distinct" <+> ppDINode "DICompileUnit"
                      [ ("language", Just (pretty language))
                      , ("file", Just (pretty file))
                      , ("producer", ppSbs producer)
                      , ("isOptimized", Just (ppBoolean optimized))
                      , ("flags", ppSbs flags)
                      , ("runtimeVersion", Just (pretty runtimeVersion))
                      , ("splitDebugFileName", ppSbs splitDebugFileName)
                      , ("emissionKind", Just (pretty emissionKind))
                      , ("enums", ppDIArray prettyEnums)
                      , ("retainedTypes", ppDIArray prettyRTs)
                      , ("globals", ppDIArray prettyGlobals)
                      , ("imports", ppDIArray prettyImports)
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

ppDIModule :: MonadModuleBuilder m => DIModule -> m (Doc ann)
ppDIModule O.Module {..} = do
  prettyScope <- case scope of { Just x -> ppMDRefM x ppDIScope; Nothing -> pure "null" }
  return $ ppDINode "DIModule"
    [ ("scope", Just prettyScope)
    , ("name", ppSbs name)
    , ("configMacros", ppSbs configurationMacros)
    , ("includePath", ppSbs includePath)
    , ("isysroot", ppSbs isysRoot)
    ]

ppDINamespace :: MonadModuleBuilder m => DINamespace -> m (Doc ann)
ppDINamespace Namespace {..} = do
  prettyScope <- case scope of { Just x -> ppMDRefM x ppDIScope; Nothing -> pure "null" }
  return $ ppDINode "DINamespace"
    [ ("name", ppSbs name)
    , ("scope", Just prettyScope)
    , ("exportSymbols", Just (ppBoolean exportSymbols))
    ]

ppDIType :: MonadModuleBuilder m => DIType -> m (Doc ann)
ppDIType (DIBasicType t) = return $ pretty t
ppDIType (DICompositeType t) = ppDICompositeType t
ppDIType (DIDerivedType t) = ppDIDerivedType t
ppDIType (DISubroutineType t) = ppDISubroutineType t

ppDILexicalBlockBase :: MonadModuleBuilder m => DILexicalBlockBase -> m (Doc ann)
ppDILexicalBlockBase DILexicalBlock {..} = do
  prettyScope <- ppMDRefM scope ppDILocalScope
  return $ ppDINode "DILexicalBlock"
    [ ("scope", Just prettyScope)
    , ("file", fmap pretty file)
    , ("line", Just (pretty line))
    , ("column", Just (pretty column))
    ]
ppDILexicalBlockBase DILexicalBlockFile {..} = do
  prettyScope <- ppMDRefM scope ppDILocalScope
  return $ ppDINode "DILexicalBlockFile"
    [ ("scope", Just prettyScope), ("file", fmap pretty file), ("discriminator", Just (pretty discriminator)) ]

ppSbs :: BS.ShortByteString -> Maybe (Doc ann)
ppSbs s
  | SBF.null s = Nothing
  | otherwise = Just (dquotes (pretty s))

ppDISubprogram :: MonadModuleBuilder m => DISubprogram -> m (Doc ann)
ppDISubprogram Subprogram {..} = do
  prettyScope <- case scope of { Just x -> do { p <- ppMDRefM x ppDIScope; return $ Just p }; Nothing -> pure Nothing }
  prettyType <- case type' of { Just x -> do { p <- ppMDRefM x ppDISubroutineType; return $ Just p }; Nothing -> pure Nothing }
  prettyCT <- case containingType of { Just x -> do { p <- ppMDRefM x ppDIType; return $ Just p }; Nothing -> pure Nothing }
  prettyUnit <- case unit of { Just x -> do { p <- ppMDRefM x ppDICompileUnit; return $ Just p }; Nothing -> pure Nothing }
  prettyTParams <- mapM (flip ppMDRefM ppDITemplateParameter) templateParams
  prettyDecl <- case declaration of { Just x -> do { p <- ppMDRefM x ppDISubprogram; return $ Just p }; Nothing -> pure Nothing }
  prettyRNs <- mapM (flip ppMDRefM ppDILocalVariable) retainedNodes
  prettyTTs <- mapM (flip ppMDRefM ppDIType) thrownTypes
  let header = ppMaybe (if definition then Just ("distinct " :: [Char]) else Nothing)
  return $ header <>
           ppDINode "DISubprogram"
           [ ("name", ppSbs name)
           , ("linkageName", ppSbs linkageName)
           , ("scope", prettyScope)
           , ("file", fmap pretty file)
           , ("line", Just (pretty line))
           , ("type", prettyType)
           , ("isLocal", Just (ppBoolean localToUnit))
           , ("isDefinition", Just (ppBoolean definition))
           , ("scopeLine", Just (pretty scopeLine))
           , ("containingType", prettyCT)
           , ("virtuality", ppVirtuality virtuality)
           , ("virtualIndex", Just (pretty virtualityIndex))
           , ("thisAdjustment", Just (pretty thisAdjustment))
           , ("flags", ppDIFlags flags)
           , ("isOptimized", Just (ppBoolean optimized))
           , ("unit", prettyUnit)
           , ("templateParams", ppDIArray prettyTParams)
           , ("declaration", prettyDecl)
           , ("retainedNodes", ppDIArray prettyRNs)
           , ("thrownTypes", ppDIArray prettyTTs)
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

ppDICompositeType :: MonadModuleBuilder m => DICompositeType -> m (Doc ann)
ppDICompositeType DIArrayType {..} = do
  prettyEltTy <- case elementTy of {Just x -> do { x' <- ppMDRefM x ppDIType; return $ Just x' }; Nothing -> pure Nothing }
  prettySubs <- mapM ppDISubrange subscripts
  return $ ppDINode "DICompositeType"
    [ ("tag", Just "DW_TAG_array_type")
    , ("elements", ppDIArray prettySubs)
    , ("baseType", prettyEltTy)
    , ("size", Just (pretty sizeInBits))
    , ("align", Just (pretty alignInBits))
    , ("flags", ppDIFlags flags)
    ]
ppDICompositeType DIClassType {..} = do
  prettyScope <- case scope of { Just x -> do { p <- ppMDRefM x ppDIScope; return $ Just p }; Nothing -> pure Nothing }
  prettyBaseTy <- case derivedFrom of {Just x -> do { x' <- ppMDRefM x ppDIType; return $ Just x' }; Nothing -> pure Nothing }
  prettyElements <- mapM (ppEitherM ppDIDerivedType ppDISubprogram) elements
  prettyVTHolder <- case vtableHolder of {Just x -> do { x' <- ppMDRefM x ppDIType; return $ Just x' }; Nothing -> pure Nothing }
  prettyTParams <- mapM ppDITemplateParameter templateParams
  return $ ppDINode "DICompositeType"
    [ ("tag", Just "DW_TAG_class_type")
    , ("scope", prettyScope)
    , ("name", ppSbs name)
    , ("file", fmap pretty file)
    , ("line", Just (pretty line))
    , ("flags", ppDIFlags flags)
    , ("baseType", prettyBaseTy)
    , ("elements", ppDIArray prettyElements)
    , ("vtableHolder", prettyVTHolder)
    , ("templateParams", ppDIArray prettyTParams)
    , ("identifier", ppSbs identifier)
    , ("size", Just (pretty sizeInBits))
    , ("align", Just (pretty alignInBits))
    ]
ppDICompositeType DIEnumerationType {..} = do
  prettyScope <- case scope of { Just x -> do { p <- ppMDRefM x ppDIScope; return $ Just p }; Nothing -> pure Nothing }
  prettyBaseTy <- case baseType of {Just x -> do { x' <- ppMDRefM x ppDIType; return $ Just x' }; Nothing -> pure Nothing }
  return $ ppDINode "DICompositeType"
    [ ("tag", Just "DW_TAG_enumeration_type")
    , ("name", ppSbs name)
    , ("file", fmap pretty file)
    , ("line", Just (pretty line))
    , ("size", Just (pretty sizeInBits))
    , ("align", Just (pretty alignInBits))
    , ("elements", Just ("!" <> braces (commas (map pretty values))))
    , ("scope", prettyScope)
    , ("identifier", ppSbs identifier)
    , ("baseType", prettyBaseTy)
    ]
ppDICompositeType DIStructureType {..} = do
  prettyScope <- case scope of { Just x -> do { p <- ppMDRefM x ppDIScope; return $ Just p }; Nothing -> pure Nothing }
  prettyBaseTy <- case derivedFrom of {Just x -> do { x' <- ppMDRefM x ppDIType; return $ Just x' }; Nothing -> pure Nothing }
  prettyElements <- mapM (ppEitherM ppDIDerivedType ppDISubprogram) elements
  prettyVTHolder <- case vtableHolder of {Just x -> do { x' <- ppMDRefM x ppDIType; return $ Just x' }; Nothing -> pure Nothing }
  return $ ppDINode "DICompositeType"
    [ ("tag", Just "DW_TAG_structure_type")
    , ("scope", prettyScope)
    , ("name", ppSbs name)
    , ("file", fmap pretty file)
    , ("line", Just (pretty line))
    , ("flags", ppDIFlags flags)
    , ("baseType", prettyBaseTy)
    , ("elements", ppDIArray prettyElements)
    , ("runtimeLang", Just (pretty runtimeLang))
    , ("vtableHolder", prettyVTHolder)
    , ("identifier", ppSbs identifier)
    , ("size", Just (pretty sizeInBits))
    , ("align", Just (pretty alignInBits))
    ]
ppDICompositeType DIUnionType {..} = do
  prettyElements <- mapM (ppEitherM ppDIDerivedType ppDISubprogram) elements
  return $ ppDINode "DICompositeType"
    [ ("tag", Just "DW_TAG_union_type")
    , ("name", ppSbs name)
    , ("file", fmap pretty file)
    , ("line", Just (pretty line))
    , ("flags", ppDIFlags flags)
    , ("elements", ppDIArray prettyElements)
    , ("runtimeLang", Just (pretty runtimeLang))
    , ("identifier", ppSbs identifier)
    , ("size", Just (pretty sizeInBits))
    , ("align", Just (pretty alignInBits))
    ]

ppDIDerivedType :: MonadModuleBuilder m => DIDerivedType -> m (Doc ann)
ppDIDerivedType DerivedType {..} = do
  prettyScope <- case scope of { Just x -> do { p <- ppMDRefM x ppDIScope; return $ Just p }; Nothing -> pure Nothing }
  prettyBaseTy <- case baseType of { Just x -> do { x' <- ppMDRefM x ppDIType; return $ Just x' }; Nothing -> pure $ Just "null" }
  return $ ppDINode "DIDerivedType"
    [ ("tag", Just (pretty tag))
    , ("name", ppSbs name)
    , ("file", fmap pretty file)
    , ("line", Just (pretty line))
    , ("scope", prettyScope)
    , ("baseType", prettyBaseTy)
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

ppDISubroutineType :: MonadModuleBuilder m => DISubroutineType -> m (Doc ann)
ppDISubroutineType SubroutineType {..} = do
  prettyTyArr <- mapM ppTy typeArray
  return $ ppDINode "DISubroutineType"
    [ ("flags", ppDIFlags flags)
    , ("cc", Just (pretty cc))
    , ("types", Just ("!" <> braces (commas prettyTyArr)))
    ]
  where ppTy Nothing = pure "null"
        ppTy (Just t) = ppMDRefM t ppDIType

ppDIGlobalVariable :: MonadModuleBuilder m => DIGlobalVariable -> m (Doc ann)
ppDIGlobalVariable O.GlobalVariable {..} = do
  prettyScope <- case scope of { Just x -> do { p <- ppMDRefM x ppDIScope; return $ Just p }; Nothing -> pure Nothing }
  prettyType <- case type' of { Just x -> do { p <- ppMDRefM x ppDIType; return $ Just p }; Nothing -> pure Nothing }
  prettyDecl <- case staticDataMemberDeclaration of { Just x -> do { p <- ppMDRefM x ppDIDerivedType; return $ Just p }; Nothing -> pure Nothing }
  return $ ppDINode "DIGlobalVariable"
    [ ("name", ppSbs name)
    , ("scope", prettyScope)
    , ("linkageName", ppSbs linkageName)
    , ("file", fmap pretty file)
    , ("line", Just (pretty line))
    , ("type", prettyType)
    , ("isLocal", Just (ppBoolean local))
    , ("isDefinition", Just (ppBoolean definition))
    , ("declaration", prettyDecl)
    , ("align", Just (pretty alignInBits))
    ]

ppDILocalVariable :: MonadModuleBuilder m => DILocalVariable -> m (Doc ann)
ppDILocalVariable LocalVariable {..} = do
  prettyScope <- ppMDRefM scope ppDIScope
  prettyType <- case type' of { Just x -> do { p <- ppMDRefM x ppDIType; return $ Just p }; Nothing -> pure Nothing }
  return $ ppDINode "DILocalVariable"
    [ ("name", ppSbs name)
    , ("scope", Just prettyScope)
    , ("file", fmap pretty file)
    , ("line", Just (pretty line))
    , ("type", prettyType)
    , ("flags", ppDIFlags flags)
    , ("arg", Just (pretty arg))
    , ("align", Just (pretty alignInBits))
    ]

ppConstant :: MonadModuleBuilder m => C.Constant -> m (Doc ann)
ppConstant c
  | C.Int width val <- c = return $ pretty val
  | C.Float (F.Double val) <- c = return $
      if specialFP val
      then "0x" <> (pretty . pack) (showHex (doubleToWord val) "")
      else pretty $ pack $ printf "%6.6e" val
  | C.Float (F.Single val) <- c = return $
      if specialFP val
      then "0x" <> (pretty . pack) (showHex (floatToWord val) "")
      else pretty $ pack $ printf "%6.6e" val
  | C.Float (F.Half val) <- c = return $ pretty $ pack $ printf "%6.6e" val
  | C.Float (F.Quadruple val _) <- c = return $ pretty $ pack $ printf "%6.6e" val
  | C.Float (F.X86_FP80 val _) <- c = return $ pretty $ pack $ printf "%6.6e" val
  | C.Float (F.PPC_FP128 val _) <- c = return $ pretty $ pack $ printf "%6.6e" val

  | C.GlobalReference ty nm <- c = return $ "@" <> pretty nm
  | C.Vector args <- c = do
      prettyArgs <- mapM (ppTypedM ppConstant) args
      return $ "<" <+> commas prettyArgs <+> ">"

  | C.Add {..}  <- c = do { prettyOp0 <- ppConstant operand0; prettyOp1 <- ppConstant operand1; return $ "add"  <+> prettyOp0 `cma` prettyOp1 }
  | C.Sub {..}  <- c = do { prettyOp0 <- ppConstant operand0; prettyOp1 <- ppConstant operand1; return $ "sub"  <+> prettyOp0 `cma` prettyOp1 }
  | C.Mul {..}  <- c = do { prettyOp0 <- ppConstant operand0; prettyOp1 <- ppConstant operand1; return $ "mul"  <+> prettyOp0 `cma` prettyOp1 }
  | C.Shl {..}  <- c = do { prettyOp0 <- ppConstant operand0; prettyOp1 <- ppConstant operand1; return $ "shl"  <+> prettyOp0 `cma` prettyOp1 }
  | C.AShr {..} <- c = do { prettyOp0 <- ppConstant operand0; prettyOp1 <- ppConstant operand1; return $ "ashr" <+> prettyOp0 `cma` prettyOp1 }
  | C.LShr {..} <- c = do { prettyOp0 <- ppConstant operand0; prettyOp1 <- ppConstant operand1; return $ "lshr" <+> prettyOp0 `cma` prettyOp1 }
  | C.And {..}  <- c = do { prettyOp0 <- ppConstant operand0; prettyOp1 <- ppConstant operand1; return $ "and"  <+> prettyOp0 `cma` prettyOp1 }
  | C.Or {..}   <- c = do { prettyOp0 <- ppConstant operand0; prettyOp1 <- ppConstant operand1; return $ "or"   <+> prettyOp0 `cma` prettyOp1 }
  | C.Xor {..}  <- c = do { prettyOp0 <- ppConstant operand0; prettyOp1 <- ppConstant operand1; return $ "xor"  <+> prettyOp0 `cma` prettyOp1 }
  | C.SDiv {..} <- c = do { prettyOp0 <- ppConstant operand0; prettyOp1 <- ppConstant operand1; return $ "sdiv" <+> prettyOp0 `cma` prettyOp1 }
  | C.UDiv {..} <- c = do { prettyOp0 <- ppConstant operand0; prettyOp1 <- ppConstant operand1; return $ "udiv" <+> prettyOp0 `cma` prettyOp1 }
  | C.SRem {..} <- c = do { prettyOp0 <- ppConstant operand0; prettyOp1 <- ppConstant operand1; return $ "srem" <+> prettyOp0 `cma` prettyOp1 }
  | C.URem {..} <- c = do { prettyOp0 <- ppConstant operand0; prettyOp1 <- ppConstant operand1; return $ "urem" <+> prettyOp0 `cma` prettyOp1 }

  | C.FAdd {..} <- c = do { prettyOp0 <- ppConstant operand0; prettyOp1 <- ppConstant operand1; return $ "fadd" <+> prettyOp0 `cma` prettyOp1 }
  | C.FSub {..} <- c = do { prettyOp0 <- ppConstant operand0; prettyOp1 <- ppConstant operand1; return $ "fsub" <+> prettyOp0 `cma` prettyOp1 }
  | C.FMul {..} <- c = do { prettyOp0 <- ppConstant operand0; prettyOp1 <- ppConstant operand1; return $ "fmul" <+> prettyOp0 `cma` prettyOp1 }
  | C.FDiv {..} <- c = do { prettyOp0 <- ppConstant operand0; prettyOp1 <- ppConstant operand1; return $ "fdiv" <+> prettyOp0 `cma` prettyOp1 }
  | C.FRem {..} <- c = do { prettyOp0 <- ppConstant operand0; prettyOp1 <- ppConstant operand1; return $ "frem" <+> prettyOp0 `cma` prettyOp1 }
  | C.FCmp {..} <- c = do { prettyOp0 <- ppConstant operand0; prettyOp1 <- ppConstant operand1; return $ "fcmp" <+> pretty fpPredicate <+> prettyOp0 `cma` prettyOp1 }
  | C.ICmp {..} <- c = do { prettyOp0 <- ppConstant operand0; prettyOp1 <- ppConstant operand1; return $ "icmp" <+> pretty iPredicate <+> prettyOp0 `cma` prettyOp1 }

  | C.Select {..} <- c = do
    prettyCond <- ppConstant condition'
    prettyTrue <- ppConstant trueValue
    prettyFalse <- ppConstant falseValue
    return $ "select" <+> commas [prettyCond, prettyTrue, prettyFalse]

  | C.SExt {..}    <- c = do { prettyOp0 <- ppConstant operand0; return $ "sext" <+> prettyOp0 <+> "to" <+> pretty type' }
  | C.ZExt {..}    <- c = do { prettyOp0 <- ppConstant operand0; return $ "zext" <+> prettyOp0 <+> "to" <+> pretty type' }
  | C.FPExt {..}   <- c = do { prettyOp0 <- ppConstant operand0; return $ "fpext" <+> prettyOp0 <+> "to" <+> pretty type' }
  | C.Trunc {..}   <- c = do { prettyOp0 <- ppConstant operand0; return $ "trunc" <+> prettyOp0 <+> "to" <+> pretty type' }
  | C.FPTrunc {..} <- c = do { prettyOp0 <- ppConstant operand0; return $ "fptrunc" <+> prettyOp0 <+> "to" <+> pretty type' }

  | C.FPToUI {..} <- c = do { prettyOp0 <- ppConstant operand0; return $ "fptoui" <+> prettyOp0 <+> "to" <+> pretty type' }
  | C.FPToSI {..} <- c = do { prettyOp0 <- ppConstant operand0; return $ "fptosi" <+> prettyOp0 <+> "to" <+> pretty type' }
  | C.UIToFP {..} <- c = do { prettyOp0 <- ppConstant operand0; return $ "uitofp" <+> prettyOp0 <+> "to" <+> pretty type' }
  | C.SIToFP {..} <- c = do { prettyOp0 <- ppConstant operand0; return $ "sitofp" <+> prettyOp0 <+> "to" <+> pretty type' }

  | C.Struct _ packed elems <- c = do
      prettyElems <- mapM (ppTypedM ppConstant) elems
      let struct = spacedbraces $ commas prettyElems
      if packed
      then return $ angleBrackets struct
      else return struct

  | C.Null constantType <- c = return $ ppNullInitializer constantType

#if MIN_VERSION_llvm_hs_pure(5,1,3)
  | C.AggregateZero constantType <- c = return "zeroinitializer"
#endif

  | C.Undef {} <- c = return "undef"
  | C.TokenNone {} <- c = return "none"
  | C.BlockAddress fn blk <- c = return $ "blockaddress" <> parens (commas [ global (pretty fn), local' (pretty blk) ])

  | C.Array {..} <- c,  memberType == (IntegerType 8) = return $ "c" <> (dquotes $ hcat [ppIntAsChar val | C.Int _ val <- memberValues])
  | C.Array {..} <- c = do
      prettyMembs <- mapM (ppTypedM ppConstant) memberValues
      return $ brackets $ commas prettyMembs

  | C.GetElementPtr {..} <- c = do
      ta <- typeOf address
      let argTy = getElementType ta
      prettyAIs <- mapM (ppTypedM ppConstant) (address:indices)
      let prettyBounds = case inBounds of { True -> "inbounds"; False -> mempty }
      return $ "getelementptr" <+> prettyBounds <+> parens (commas (pretty argTy : prettyAIs))

  | C.BitCast {..}        <- c = do { prettyOp0 <- ppTypedM ppConstant operand0; return $ "bitcast" <+> parens (prettyOp0 <+> "to" <+> pretty type') }
  | C.PtrToInt {..}       <- c = do { prettyOp0 <- ppTypedM ppConstant operand0; return $ "ptrtoint" <+> parens (prettyOp0 <+> "to" <+> pretty type') }
  | C.IntToPtr {..}       <- c = do { prettyOp0 <- ppTypedM ppConstant operand0; return $ "inttoptr" <+> parens (prettyOp0 <+> "to" <+> pretty type') }
  | C.AddrSpaceCast {..}  <- c = do { prettyOp0 <- ppTypedM ppConstant operand0; return $ "addrspacecast" <+> parens (prettyOp0 <+> "to" <+> pretty type') }
  | otherwise = error "Non-function argument. (Malformed AST)"

instance Pretty a => Pretty (Named a) where
  pretty (nm := a) = "%" <> pretty nm <+> "=" <+> pretty a
  pretty (Do a) = pretty a

ppNamed :: MonadModuleBuilder m => (a -> m (Doc ann)) -> Named a -> m (Doc ann)
ppNamed pp (nm := a) = do
  prettyA <- pp a
  return $ "%" <> pretty nm <+> "=" <+> prettyA
ppNamed pp (Do a) = pp a

ppModule :: MonadModuleBuilder m => Module -> m (Doc ann)
ppModule Module {..} = do
  prettyDefs <- mapM ppDefinition moduleDefinitions
  let header = printf "; ModuleID = '%s'" (unShort moduleName)
  let target = case moduleTargetTriple of
                    Nothing -> mempty
                    Just target -> "target triple =" <+> dquotes (short target)
  let layout = case moduleDataLayout of
                    Nothing     -> mempty
                    Just layout -> "target datalayout =" <+> dquotes (pretty layout)
  return $ hlinecat (fromString header : (layout <> softline <> target) : prettyDefs)

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

phiIncoming :: MonadModuleBuilder m => (Operand, Name) -> m (Doc ann)
phiIncoming (op, nm) = do
  prettyOp <- ppOperand op
  return $ brackets (prettyOp `cma` (local' (pretty nm)))

ppFunctionArgumentTypes :: Type -> Doc ann
ppFunctionArgumentTypes FunctionType {..} = ppParams pretty (argumentTypes, isVarArg)
ppFunctionArgumentTypes _ = error "Non-function argument. (Malformed AST)"

ppNullInitializer :: Type -> Doc ann
ppNullInitializer PointerType {..} = "zeroinitializer"
ppNullInitializer StructureType {..} = "zeroinitializer"
ppNullInitializer FunctionType {..} = "zeroinitializer"
ppNullInitializer ArrayType {..} = "zeroinitializer"
ppNullInitializer _ = error "Non-pointer argument. (Malformed AST)"

ppCall :: MonadModuleBuilder m => Instruction -> m (Doc ann)
ppCall (Call { function = Right f,..}) = do
  typeOfF <- typeOf f
  let (functionType@FunctionType {..}) = case (referencedType typeOfF) of
                                           fty@FunctionType {..} -> fty
                                           _ -> error "Calling non function type. (Malformed AST)"
  let ftype = if isVarArg
              then ppFunctionArgumentTypes functionType
              else mempty

  let tail = case tailCallKind of
               Just Tail -> "tail"
               Just MustTail -> "musttail"
               Just NoTail -> "notail"
               Nothing -> mempty
  prettyArgs <- mapM ppArguments arguments
  prettyF <- ppOperand f
  return $ tail <+> "call" <+> pretty callingConvention <+> ppReturnAttributes returnAttributes <+> pretty resultType <+> ftype
                <+> prettyF <> parens (commas prettyArgs) <+> ppFunctionAttributes functionAttributes

ppCall (Call { function = Left (IA.InlineAssembly {..}), ..}) = do
  let tail = case tailCallKind of
               Just Tail -> "tail"
               Just MustTail -> "musttail"
               Just NoTail -> "notail"
               Nothing -> mempty
  -- If multiple keywords appear the ‘sideeffect‘ keyword must come first,
  -- the ‘alignstack‘ keyword second and the ‘inteldialect‘ keyword last.
  let sideeffect' = if hasSideEffects then "sideeffect" else ""
  let align' = if alignStack then "alignstack" else ""
  -- ATTDialect is assumed if not specified
  let dialect' = case dialect of IA.ATTDialect -> ""; IA.IntelDialect -> "inteldialect"
  prettyArgs <- mapM ppArguments arguments
  return $ tail <+> "call" <+> pretty callingConvention <+> ppReturnAttributes returnAttributes <+> pretty type'
                <+> "asm" <+> sideeffect' <+> align' <+> dialect' <+> dquotes (pretty (pack (BL.unpack assembly))) <> ","
                <+> dquotes (pretty constraints) <> parens (commas prettyArgs) <+> ppFunctionAttributes functionAttributes

ppCall x = error "Non-callable argument. (Malformed AST)"

referencedType :: Type -> Type
referencedType (PointerType t _) = referencedType t
referencedType t                 = t

ppReturnAttributes :: [ParameterAttribute] -> Doc ann
ppReturnAttributes pas = hsep $ fmap pretty pas

-- Differs from Call in record name conventions only so needs a seperate almost
-- identical function. :(
ppInvoke :: MonadModuleBuilder m => Terminator -> m (Doc ann)
ppInvoke (Invoke { function' = Right f,..}) = do
  typeOfF <- typeOf f
  let (functionType@FunctionType {..}) = case referencedType typeOfF of
                                           fty@FunctionType{..} -> fty
                                           _ -> error "Invoking non-function type. (Malformed AST)"
  let ftype = if isVarArg
              then ppFunctionArgumentTypes functionType
              else mempty
  prettyArgs <- mapM ppArguments arguments'
  prettyF <- ppOperand f
  return $ "invoke" <+> pretty callingConvention' <+> pretty resultType <+> ftype
                    <+> prettyF <> parens (commas prettyArgs) <+> ppFunctionAttributes functionAttributes'

ppInvoke x = error "Non-callable argument. (Malformed AST)"

ppSingleBlock :: MonadModuleBuilder m => BasicBlock -> m (Doc ann)
ppSingleBlock (BasicBlock nm instrs term) = do
  prettyInsts <- mapM (ppNamed ppInstruction) instrs
  prettyTerm <- ppNamed ppTerminator term
  return $ (vcat $ prettyInsts ++ [prettyTerm])

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

ppInstrMeta :: MonadModuleBuilder m => InstructionMetadata -> m (Doc ann)
ppInstrMeta [] = pure mempty
ppInstrMeta xs = do
  prettyMeta <- ppInstructionMetadata xs
  return $ "," <> prettyMeta

ppInstructionMetadata :: MonadModuleBuilder m => InstructionMetadata -> m (Doc ann)
ppInstructionMetadata meta = do
  prettyMeta <- mapM (\(x,y) -> do { y' <- ppMDRefM y ppMDNode; return (x, y') }) meta
  return $ commas ["!" <> short x <+> y | (x,y) <- prettyMeta]

ppLayoutOptions :: LayoutOptions
ppLayoutOptions = LayoutOptions (AvailablePerLine 100 0.5)

-- -------------------------------------------------------------------------------
-- -- Toplevel
-- -------------------------------------------------------------------------------

-- | Render the result of pretty-printing to text
renderll :: Doc ann -> Text
renderll = renderLazy . layoutPretty ppLayoutOptions

-- | Pretty print a LLVM module
ppllvm :: MonadModuleBuilder m => Module -> m Text
ppllvm m = do
  prettyMod <- ppModule m
  return $ renderll prettyMod

-- | Pretty print a printable LLVM expression
ppll :: Pretty a => a -> Text
ppll = renderll . pretty
