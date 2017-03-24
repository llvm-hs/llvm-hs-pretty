{-# LANGUAGE RecordWildCards #-}

module LLVM.General.Typed (
  Typed(..),
) where

import LLVM.General.AST
import LLVM.General.AST.Global
import LLVM.General.AST.Type

import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F

-----
-- Reasoning about types
-----

class Typed a where
    typeOf :: a -> Type

instance Typed Operand where
    typeOf (LocalReference t _) = t
    typeOf (ConstantOperand c)  = typeOf c
    typeOf _                    = MetadataType

instance Typed C.Constant where
    typeOf (C.Int bits _)  = IntegerType bits
    typeOf (C.Float float) = typeOf float
    typeOf (C.Null t)      = t
    typeOf (C.Struct {..}) = StructureType isPacked (map typeOf memberValues)
    typeOf (C.Array {..})  = ArrayType (fromIntegral $ length memberValues) memberType
    typeOf (C.Vector {..}) = VectorType (fromIntegral $ length memberValues) $
                                case memberValues of
                                    []    -> VoidType {- error "Vectors of size zero are not allowed" -}
                                    (x:_) -> typeOf x
    typeOf (C.Undef t)     = t
    typeOf (C.BlockAddress {..})   = ptr i8
    typeOf (C.GlobalReference t _) = t
    typeOf (C.Add {..})     = typeOf operand0
    typeOf (C.FAdd {..})    = typeOf operand0
    typeOf (C.Sub {..})     = typeOf operand0
    typeOf (C.FSub {..})    = typeOf operand0
    typeOf (C.Mul {..})     = typeOf operand0
    typeOf (C.FMul {..})    = typeOf operand0
    typeOf (C.UDiv {..})    = typeOf operand0
    typeOf (C.SDiv {..})    = typeOf operand0
    typeOf (C.URem {..})    = typeOf operand0
    typeOf (C.SRem {..})    = typeOf operand0
    typeOf (C.Shl {..})     = typeOf operand0
    typeOf (C.LShr {..})    = typeOf operand0
    typeOf (C.AShr {..})    = typeOf operand0
    typeOf (C.And {..})     = typeOf operand0
    typeOf (C.Or  {..})     = typeOf operand0
    typeOf (C.Xor {..})     = typeOf operand0
    typeOf (C.GetElementPtr {..}) = getElementPtrType (typeOf address) indices
    typeOf (C.Trunc {..})   = type'
    typeOf (C.ZExt {..})    = type'
    typeOf (C.SExt {..})    = type'
    typeOf (C.FPToUI {..})  = type'
    typeOf (C.FPToSI {..})  = type'
    typeOf (C.UIToFP {..})  = type'
    typeOf (C.SIToFP {..})  = type'
    typeOf (C.FPTrunc {..}) = type'
    typeOf (C.FPExt {..})   = type'
    typeOf (C.PtrToInt {..})    = type'
    typeOf (C.BitCast {..})     = type'
    typeOf (C.ICmp {..})    = case (typeOf operand0) of
                                (VectorType n _) -> VectorType n i1
                                _ -> i1
    typeOf (C.FCmp {..})    = case (typeOf operand0) of
                                (VectorType n _) -> VectorType n i1
                                _ -> i1
    typeOf (C.Select {..})  = typeOf trueValue
    typeOf (C.ExtractElement {..})  = case typeOf vector of
                                        (VectorType _ t) -> t
                                        _ -> VoidType {- error "The first operand of an ‘extractelement‘ instruction is a value of vector type." -}
    typeOf (C.InsertElement {..})   = typeOf vector
    typeOf (C.ShuffleVector {..})   = case (typeOf operand0, typeOf mask) of
                                        (VectorType _ t, VectorType m _) -> VectorType m t
                                        _ -> VoidType {- error -}
    typeOf (C.ExtractValue {..})    = extractValueType (typeOf aggregate) indices
    typeOf (C.InsertValue {..})     = typeOf aggregate

getElementPtrType :: Type -> [C.Constant] -> Type
getElementPtrType ty cons = ptr i8 -- XXX

extractValueType = error "extract"

instance Typed F.SomeFloat where
    typeOf (F.Half _)          = FloatingPointType 16  IEEE
    typeOf (F.Single _)        = FloatingPointType 32  IEEE
    typeOf (F.Double _)        = FloatingPointType 64  IEEE
    typeOf (F.Quadruple _ _)   = FloatingPointType 128 IEEE
    typeOf (F.X86_FP80 _ _)    = FloatingPointType 80  DoubleExtended
    typeOf (F.PPC_FP128 _ _)   = FloatingPointType 128 PairOfFloats

instance Typed Global where
    typeOf (GlobalVariable {..}) = type'
    typeOf (GlobalAlias {..})    = type'
    typeOf (Function {..})       = let (params, isVarArg) = parameters
                                   in FunctionType returnType (map typeOf params) isVarArg
instance Typed Parameter where
    typeOf (Parameter t _ _) = t
