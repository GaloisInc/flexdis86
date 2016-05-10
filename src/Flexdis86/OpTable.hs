{- |
Module      :  $Header$
Description :  Declares parser for optable.xml file.
Copyright   :  (c) Galois, Inc 2013
Maintainer  :  jhendrix@galois.com

This declares the parser for optable.xml file.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
module Flexdis86.OpTable
  ( -- * Primitive types.
    CPURequirement(..)
  , Vendor(..)
  , ModeLimit(..)
    -- * Def
  , Def
  , defMnemonic
  , defMnemonicSynonyms
  , defVendor
  , defCPUReq
  , modeLimit
  , Mode(..)
  , defMode
  , reqAddrSize
  , reqOpSize
  , defPrefix
  , requiredPrefix
  , defOpcodes
  , requiredMod
  , requiredReg
  , requiredRM
  , x87ModRM
  , defOperands
  , x64Compatible
    -- * Parsing defs
  , parseOpTable
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Bits ((.&.), shiftR)
import qualified Data.ByteString as BS
import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Word

import Prelude

import Numeric (readDec, readHex)
import Text.XML.Light

import Flexdis86.Operand
import Flexdis86.Register
import Flexdis86.Segment
import Flexdis86.Sizes

qname :: String -> QName
qname nm = QName { qName = nm, qURI = Nothing, qPrefix = Nothing }

ppQName :: QName -> String
ppQName qnm = qName qnm

data ElemState = ElemState { esName :: QName
                           , esContent :: [Content]
                           , esLine :: Maybe Line
                           }

mkElemState :: Element -> ElemState
mkElemState e = ElemState { esName = elName e
                          , esContent = elContent e
                          , esLine = elLine e
                          }

newtype ElemParser a = EP { unEP :: ElemState -> Either String (a,ElemState) }

instance Functor ElemParser where
  fmap f m = EP (\s -> case unEP m s of
                         Left e -> Left e
                         Right (v,s') -> Right (f v, s'))

instance Applicative ElemParser where
  pure v = EP $ \s -> Right (v,s)
  m <*> h = EP $ \s -> do
    (f,s1) <- unEP m s
    (x,s2) <- unEP h s1
    return (f x, s2)

instance Monad ElemParser where
  return v = EP $ \s -> Right (v,s)
  m >>= h = EP $ \s -> do (v,s') <- unEP m s
                          unEP (h v) s'
  fail e = EP $ \s -> Left $ show (esLine s) ++ ": " ++  e

instance MonadState ElemState ElemParser where
  get = EP (\s -> Right (s,s))
  put v = EP (\_ -> Right ((),v))

runWithElement :: ElemParser a -> Element -> ElemParser a
runWithElement m e = do
  s <- get
  put (mkElemState e)
  v <- m
  put s
  return v

runElemParser :: ElemParser a -> Element -> Either String a
runElemParser m e = fmap fst (unEP m (mkElemState e))

checkTag :: String -> ElemParser ()
checkTag nm = do
  enm <- gets esName
  when (enm /= qname nm) $
    fail $ "Unexpected tag " ++ ppQName enm ++ " in element."

getNextElt :: ElemParser (Maybe Element)
getNextElt = do
  s <- get
  case esContent s of
    [] -> return Nothing
    (h:r) -> do
      put s { esContent = r }
      case h of
        Elem e -> return (Just e)
        Text d | dropWhile isSpace (cdData d) == "" -> getNextElt
        t -> fail $ "Unexpected non-whitespace text: " ++ show t

pushElt :: Element -> ElemParser ()
pushElt e = do
  s <- get
  put s { esContent = Elem e : esContent s }

next :: ElemParser a -> ElemParser a
next p = do
  me <- getNextElt
  case me of
    Just e -> runWithElement p e
    Nothing -> fail "Failed to find next element."

opt :: String -> ElemParser a -> ElemParser (Maybe a)
opt nm p = do
  me <- getNextElt
  case me of
    Nothing -> return Nothing
    Just e
      | elName e == qname nm ->
        fmap Just $ runWithElement p e
      | otherwise -> pushElt e >> return Nothing

checkEnd :: ElemParser ()
checkEnd = do
  me <- getNextElt
  case me of
    Nothing -> return ()
    Just e -> pushElt e >> fail "Expected end of elements."

remainingElts :: ElemParser a -> ElemParser [a]
remainingElts p = go []
  where go r = do
          me <- getNextElt
          case me of
            Nothing -> return (reverse r)
            Just e -> do
              v <- runWithElement p e
              go (v:r)

asText :: ElemParser String
asText = do
  s <- get
  case esContent s of
    [Text d] | cdVerbatim d == CDataText -> return (cdData d)
    _ -> fail "asText found non-text."

required_text :: String -> ElemParser String
required_text nm = next $ checkTag nm >> asText

------------------------------------------------------------------------
-- Misc

-- | Mode effect on instruction semantecs.
data Mode
     -- | Default operand size is 64 bits in x64 mode.
   = Default64
     -- | Instruction is invalid in x64 mode.
   | Invalid64
  deriving (Eq,Ord,Show)

-- | Parse a mode for the instruction.
parse_mode :: ElemParser (Maybe Mode)
parse_mode = opt "mode" $ do
  txt <- asText
  case txt of
    "def64" -> return Default64
    "inv64" -> return Invalid64
    _ -> fail $ "Invalid mode: " ++ show txt

-- | Defines which features are required for instruction
-- to be supported.
data CPURequirement
     -- | This is a base instruction.
   = Base
     -- | This uses the X87 architecture.
   | X87
     -- | This is an undocumented X86 instruction.
   | X87_undocumented
     -- | Safer mode extensions
   | SMX
     -- | AMD 3D now definition.
   | AMD_3DNOW
   | SSE
   | SSE2
   | SSE3
     -- | The movbe instruction which is unique to the atom.
   | SSE3_atom
   | SSE4_1
   | SSE4_2
     -- | AES new instructions.
   | AESNI
  deriving (Eq,Ord, Show)

insClassMap :: Map.Map String CPURequirement
insClassMap = Map.fromList
  [ (,) "X87"   X87
  , (,) "X87 UNDOC" X87_undocumented
  , (,) "smx"   SMX
  , (,) "sse"   SSE
  , (,) "sse2"  SSE2
  , (,) "sse3"   SSE3
  , (,) "sse3 atom" SSE3_atom
  , (,) "sse4.1" SSE4_1
  , (,) "sse4.2" SSE4_2
  , (,) "aesni" AESNI
  ]

-- | Returns a CPU requirement, possible given a requirement from the outer
-- constext.
parse_CPURequirement :: CPURequirement -> ElemParser CPURequirement
parse_CPURequirement c = do
  fmap (fromMaybe c) $ opt "class" $ do
    txt <- asText
    case Map.lookup txt insClassMap of
      Nothing -> fail $ "Unknown instruction class: " ++ show txt
      Just v -> do
        when (c /= Base) $ do
          fail $ "Instruction class already defined " ++ show (c,v)
        return v

-- | Defines whether instuction is vendor specific.
data Vendor = AMD | Intel
  deriving (Eq,Ord, Show)

parse_vendor :: Maybe Vendor -> ElemParser (Maybe Vendor)
parse_vendor v = do
  fmap (maybe v Just) $ opt "vendor" $ do
    when (isJust v) $ do
      fail $ "Vendor already defined."
    txt <- asText
    case txt of
      "amd" -> return $ AMD
      "intel" -> return $ Intel
      _ -> fail $ "Unknown vendor: " ++ show txt

-- | Indicates restrictions on which mode instruction may run in.
data ModeLimit
   = AnyMode
   | Only32
   | Only64
   | Not64
  deriving (Eq, Show)

valid64 :: ModeLimit -> Bool
valid64 m = m == AnyMode || m == Only64

------------------------------------------------------------------------
-- Instruction


-- | The definition of an instruction.
data Def = Def  { _defMnemonic :: String
                  -- ^ Canonical mnemonic
                , _defMnemonicSynonyms :: [String]
                  -- ^ Additional mnemonics, not including
                  -- the canonical mnemonic. Used e.g. by
                  -- jump instructions.
                , _defCPUReq :: CPURequirement
                , _defVendor :: Maybe Vendor
                , _modeLimit :: ModeLimit
                , _defMode   :: Maybe Mode
                , _reqAddrSize :: Maybe SizeConstraint
                , _reqOpSize :: Maybe SizeConstraint
                , _defPrefix :: [String]
                , _requiredPrefix :: Maybe Word8
                , _defOpcodes :: [Word8]
                , _requiredMod :: Maybe ModConstraint
                , _requiredReg :: Maybe Fin8
                , _requiredRM :: Maybe Fin8
                , _x87ModRM    :: Maybe Fin64
                , _defOperands  :: [OperandType]
                } deriving (Eq, Show)

-- | Canonical mnemonic for definition.
defMnemonic :: Simple Lens Def String
defMnemonic = lens _defMnemonic (\s v -> s { _defMnemonic = v })

-- | Additional mnemonics, not including the canonical mnemonic.
--
-- Used e.g. by jump instructions.
defMnemonicSynonyms :: Simple Lens Def [String]
defMnemonicSynonyms =
  lens _defMnemonicSynonyms (\s v -> s { _defMnemonicSynonyms = v })

-- | CPU requirements on the definition.
defCPUReq :: Simple Lens Def CPURequirement
defCPUReq = lens _defCPUReq (\s v -> s { _defCPUReq = v })

-- | Vendor requirements on the definition.
defVendor :: Simple Lens Def (Maybe Vendor)
defVendor = lens _defVendor (\s v -> s { _defVendor = v })

-- | Restrictions on the mode of the CPU.
modeLimit :: Simple Lens Def ModeLimit
modeLimit = lens _modeLimit (\s v -> s { _modeLimit = v })

-- | Modifications to x64 mode.
defMode :: Simple Lens Def (Maybe Mode)
defMode = lens _defMode (\s v -> s { _defMode = v })

-- | Expected address size for instruction.
reqAddrSize :: Simple Lens Def (Maybe SizeConstraint)
reqAddrSize = lens _reqAddrSize (\s v -> s { _reqAddrSize = v })

-- | Expected operand size for instruction.
reqOpSize :: Simple Lens Def (Maybe SizeConstraint)
reqOpSize = lens _reqOpSize (\s v -> s { _reqOpSize = v })

-- | Prefixes allowed on instruction.
defPrefix :: Simple Lens Def [String]
defPrefix = lens _defPrefix (\s v -> s { _defPrefix = v })

-- | Prefixe required by an instruction, if any.
requiredPrefix :: Simple Lens Def (Maybe Word8)
requiredPrefix = lens _requiredPrefix (\s v -> s { _requiredPrefix = v })

-- | Opcodes on instruction.
defOpcodes :: Simple Lens Def [Word8]
defOpcodes = lens _defOpcodes (\s v -> s { _defOpcodes = v })

-- | Constraint on the modRM.mod value.
requiredMod :: Simple Lens Def (Maybe ModConstraint)
requiredMod = lens _requiredMod (\s v -> s { _requiredMod = v })

-- | Indicates if instruction must have ModR/M value with the
-- given value in the reg field.
requiredReg :: Simple Lens Def (Maybe Fin8)
requiredReg = lens _requiredReg (\s v -> s { _requiredReg = v })

-- | Indicates if instruction must have ModR/M value with the
-- given value in the rm field.
requiredRM :: Simple Lens Def (Maybe Fin8)
requiredRM = lens _requiredRM (\s v -> s { _requiredRM = v })

-- | An x87 FPU opcode expected in the low 6-bits of a ModRM byte
-- following instruction.
x87ModRM :: Simple Lens Def (Maybe Fin64)
x87ModRM = lens _x87ModRM (\s v -> s { _x87ModRM = v })

-- | Operand descriptions.
defOperands :: Simple Lens Def [OperandType]
defOperands = lens _defOperands (\s v -> s { _defOperands = v })

-- | Parse a definition.
parse_def ::
  String -> [String] -> CPURequirement -> Maybe Vendor -> ElemParser Def
parse_def nm syns creq v = do
  checkTag "def"
  let parse_prefix = fromMaybe [] . fmap words <$> opt "pfx" asText
  prefix <- parse_prefix
  opc_text <- required_text "opc"
  oprnds <- fromMaybe [] . fmap words <$> opt "opr" asText
  mode <- parse_mode
  creq' <- parse_CPURequirement creq
  v' <- parse_vendor v
  checkEnd
  let d0 = Def { _defMnemonic = nm
               , _defMnemonicSynonyms = syns
               , _defCPUReq = creq'
               , _defVendor = v'
               , _modeLimit = AnyMode
               , _defMode = mode
               , _reqAddrSize = Nothing
               , _reqOpSize = Nothing
               , _defPrefix = prefix
               , _requiredPrefix = Nothing
               , _defOpcodes = []
               , _requiredMod = Nothing
               , _requiredReg = Nothing
               , _requiredRM  = Nothing
               , _x87ModRM = Nothing
               , _defOperands = map (lookupOperandType "") oprnds
               }
  flip execStateT d0 $ do
    mapM_ parse_opcode (words opc_text)

addOpcode :: MonadState Def m => Word8 -> m ()
addOpcode c = defOpcodes %= (++ [c])

setDefCPUReq :: MonadState Def m => CPURequirement -> m ()
setDefCPUReq r = do
  creq <- use defCPUReq
  when (creq == Base) $ defCPUReq .= r

-- | Parse opcode value
parse_opcode :: MonadState Def m => String -> m ()
parse_opcode nm = do
  case readHex nm of
    [(v,"")] -> addOpcode v
    _ | Just r <- stripPrefix "/a=" nm
      -> case r of
           "16" -> reqAddrSize ?= Size16
           "32" -> reqAddrSize ?= Size32
           "64" -> reqAddrSize ?= Size64
           _ -> fail $ "Unexpected address size: " ++ r
    _ | Just r <- stripPrefix "/m=" nm
      -> case r of
           "32"  -> modeLimit .= Only32
           "64"  -> modeLimit .= Only64
           "!64" -> modeLimit .= Not64
           _ -> fail $ "Unexpected mode limit: " ++ r
    _ | Just r <- stripPrefix "/mod=" nm
      -> case r of
           "11"  -> requiredMod ?= OnlyReg
           "!11" -> requiredMod ?= OnlyMem
           _ -> fail $ "Unexpected mod constraint: " ++ r
    _ | Just r <- stripPrefix "/o=" nm
      , [(b,"")] <- readDec r
      , b `elem` [16, 32, 64::Int]
      -> case r of
           "16" -> reqOpSize ?= Size16
           "32" -> reqOpSize ?= Size32
           "64" -> reqOpSize ?= Size64
           _ -> fail $ "Unexpected operand size: " ++ r

    _ | Just r <- stripPrefix "/reg=" nm
      , [(b,"")] <- readHex r
      , 0 <= b && b < 8
      -> requiredReg ?= Fin8 b
    _ | Just r <- stripPrefix "/rm=" nm
      , [(b,"")] <- readHex r
      , 0 <= b && b < 8
      -> requiredRM ?= Fin8 b
    _ | Just r <- stripPrefix "/3dnow=" nm
      , [(b,"")] <- readHex r
      -> do setDefCPUReq AMD_3DNOW
            addOpcode b -- We don't use requiredPrefix here because it is a suffix
    _ | Just r <- stripPrefix "/sse=" nm
      , [(b,"")] <- readHex r
      -> do setDefCPUReq SSE
            requiredPrefix ?= b
    _ | Just r <- stripPrefix "/x87=" nm
      , [(b,"")] <- readHex r
      , 0 <= b && b < 64
      -> do setDefCPUReq X87
            x87ModRM ?= Fin64 b
            -- FIXME: sjw: HACK to avoid making the parser more complex.  Basically, we
            -- pretend we want both Reg and R/M
            requiredRM  ?= Fin8 (b .&. 0x7) -- bottom 3 bits
            requiredReg ?= Fin8 ((b `shiftR` 3) .&. 0x7)
    _  ->  fail $ "Unexpected opcode: " ++ show nm

------------------------------------------------------------------------
-- Instruction

-- | Return instuction if instruction can be used with x64.
x64Compatible :: Def -> Bool
x64Compatible d =
  case d^.defOpcodes of
    [b] | b .&. 0xF0 == 0x40 -> False
    _ -> valid64 (d^.modeLimit)

-- | Recognizes form for mnemonics
isMnemonic :: String -> Bool
isMnemonic (h:r) = (isLower h || h == '_') && all isLowerOrDigit r
isMnemonic [] = False

isLowerOrDigit :: Char -> Bool
isLowerOrDigit c = isLower c || isDigit c || (c == '_')

parse_instruction :: ElemParser [Def]
parse_instruction = do
  checkTag "instruction"
  (mnem, syns) <- parse_mnemonics
  cpuReq <- parse_CPURequirement Base
  v <- parse_vendor Nothing
  remainingElts (parse_def mnem syns cpuReq v)

parse_mnemonics :: ElemParser (String, [String])
parse_mnemonics = do
  mnems <- words <$> required_text "mnemonic"
  when (null mnems) $
    fail "Empty mnemonic element!"
  forM_ mnems $ \mnem -> do
    unless (isMnemonic mnem) $
      fail $ "Invalid mnemonic: " ++ show mnem
  return (head mnems, tail mnems)

parse_x86_optable :: ElemParser [Def]
parse_x86_optable = do
  checkTag "x86optable"
  concat <$> remainingElts parse_instruction

parseOpTable :: BS.ByteString -> Either String [Def]
parseOpTable bs = do
  case parseXMLDoc bs of
    Nothing -> Left "Not an XML document"
    Just elt -> runElemParser parse_x86_optable elt

operandHandlerMap :: Map.Map String OperandType
operandHandlerMap = Map.fromList
  [ -- Fixed values implicitly derived from opcode.
    (,) "AL"  $ OpType (Reg_fixed 0) BSize
  , (,) "eAX" $ OpType (Reg_fixed 0) ZSize
  , (,) "rAX" $ OpType (Reg_fixed 0) VSize
  , (,) "CL"  $ OpType (Reg_fixed 1) BSize
  , (,) "DX"  $ OpType (Reg_fixed 2) WSize

  , (,) "MIdb" $ M_Implicit ES RDI BSize
  , (,) "MIdw" $ M_Implicit ES RDI WSize
  , (,) "MIdd" $ M_Implicit ES RDI DSize
  , (,) "MIdq" $ M_Implicit ES RDI QSize
  , (,) "MIsb" $ M_Implicit DS RSI BSize
  , (,) "MIsw" $ M_Implicit DS RSI WSize
  , (,) "MIsd" $ M_Implicit DS RSI DSize
  , (,) "MIsq" $ M_Implicit DS RSI QSize

    -- Fixed segment registers.
  , (,) "FS"  $ SEG FS
  , (,) "GS"  $ SEG GS

    -- Register values stored in opcode that also depend on REX.b
  , (,) "R0b" $ OpType (Opcode_reg 0) BSize
  , (,) "R1b" $ OpType (Opcode_reg 1) BSize
  , (,) "R2b" $ OpType (Opcode_reg 2) BSize
  , (,) "R3b" $ OpType (Opcode_reg 3) BSize
  , (,) "R4b" $ OpType (Opcode_reg 4) BSize
  , (,) "R5b" $ OpType (Opcode_reg 5) BSize
  , (,) "R6b" $ OpType (Opcode_reg 6) BSize
  , (,) "R7b" $ OpType (Opcode_reg 7) BSize
  , (,) "R0v" $ OpType (Opcode_reg 0) VSize
  , (,) "R1v" $ OpType (Opcode_reg 1) VSize
  , (,) "R2v" $ OpType (Opcode_reg 2) VSize
  , (,) "R3v" $ OpType (Opcode_reg 3) VSize
  , (,) "R4v" $ OpType (Opcode_reg 4) VSize
  , (,) "R5v" $ OpType (Opcode_reg 5) VSize
  , (,) "R6v" $ OpType (Opcode_reg 6) VSize
  , (,) "R7v" $ OpType (Opcode_reg 7) VSize
  , (,) "R0y" $ OpType (Opcode_reg 0) YSize
  , (,) "R1y" $ OpType (Opcode_reg 1) YSize
  , (,) "R2y" $ OpType (Opcode_reg 2) YSize
  , (,) "R3y" $ OpType (Opcode_reg 3) YSize
  , (,) "R4y" $ OpType (Opcode_reg 4) YSize
  , (,) "R5y" $ OpType (Opcode_reg 5) YSize
  , (,) "R6y" $ OpType (Opcode_reg 6) YSize
  , (,) "R7y" $ OpType (Opcode_reg 7) YSize

    -- Register values stored in ModRM.reg.
  , (,) "Gb"  $ OpType ModRM_reg BSize
  , (,) "Gd"  $ OpType ModRM_reg DSize
  , (,) "Gq"  $ OpType ModRM_reg QSize
  , (,) "Gv"  $ OpType ModRM_reg VSize
  , (,) "Gy"  $ OpType ModRM_reg YSize

    -- Control register read from ModRM.reg.
  , (,) "C"   $ RG_C
    -- Debug register read from ModRM.reg.
  , (,) "D"   $ RG_dbg

    -- MMX register stored in ModRM.reg
  , (,) "P"    $ RG_MMX_reg

    -- MMX register stored in ModRM.rm
    -- (ModRM.mod must equal 3).
  , (,) "N"    $ RG_MMX_rm
   -- Register operand stored in ModRM.rm (ModRM.mod must equal 3)
  , (,) "R"    $ OpType ModRM_rm_mod3 RDQSize

    -- RM fields are read from ModRM.rm
  , (,) "Eb"  $ OpType ModRM_rm BSize
  , (,) "Ew"  $ OpType ModRM_rm WSize
  , (,) "Ed"  $ OpType ModRM_rm DSize
  , (,) "Eq"  $ OpType ModRM_rm QSize
  , (,) "Ev"  $ OpType ModRM_rm VSize
  , (,) "Ey"  $ OpType ModRM_rm YSize

    -- Memory or register value stored in ModRM.rm
    -- As a  register has size VSize, and as memory has size WSize.
  , (,) "MwRv" $ MXRX VSize WSize
    -- As a  register has size DSize, and as memory has size BSize.
  , (,) "MbRd" $ MXRX BSize DSize
  , (,) "MwRy" $ MXRX WSize YSize

    -- Far Pointer (which is an address) stored in ModRM.rm
    -- (ModRM.mod must not equal 3)
  , (,) "Fv"   $ M_FP
    -- Memory value stored in ModRM.rm
    -- (ModRM.mod must not equal 3)
  , (,) "M"    $ M

    -- Memory value pointing to floating point value
  , (,) "M32fp" $ M_FloatingPoint FPSize32
  , (,) "M64fp" $ M_FloatingPoint FPSize64
  , (,) "M80fp" $ M_FloatingPoint FPSize80


    -- FP Register indicies
  , (,) "ST0"   $ RG_ST 0
  , (,) "ST1"   $ RG_ST 1
  , (,) "ST2"   $ RG_ST 2
  , (,) "ST3"   $ RG_ST 3
  , (,) "ST4"   $ RG_ST 4
  , (,) "ST5"   $ RG_ST 5
  , (,) "ST6"   $ RG_ST 6
  , (,) "ST7"   $ RG_ST 7

    -- Memory value pointing to 64-bit integer stored in ModRM.rm
    -- (ModRM.mod must not equal 3).
  , (,) "Mq"  $ M_X Size64
  , (,) "Md"  $ M_X Size32
  , (,) "Mw"  $ M_X Size16

  , (,) "S"   $ RG_S

  , (,) "Q"    $ RM_MMX

    -- Immediate values with different sizes.
    -- An immediate byte that does not need to be extended.
  , (,) "Ib"  $ OpType ImmediateSource BSize
    -- An immediate word that does not need to be extended.
  , (,) "Iw"  $ OpType ImmediateSource WSize
    -- A size v value that does not need to be extended.
  , (,) "Iv"  $ OpType ImmediateSource VSize
    -- A size z value that does not need to be extended.
  , (,) "Iz"  $ OpType ImmediateSource ZSize

    -- Constant one
  , (,) "I1"  $ IM_1

  , (,) "Jb"   $ OpType JumpImmediate  BSize
  , (,) "Jz"   $ OpType JumpImmediate  ZSize

  , (,) "Ob"   $ OpType OffsetSource BSize
  , (,) "Ov"   $ OpType OffsetSource VSize

    -- An immediate byte that is sign extended to an operator size.
  , (,) "sIb" $ IM_SB
    -- An immediate ZSize value that is sign extended to the operator
    -- size.
  , (,) "sIz" $ IM_SZ

    -- XMM
  , (,) "U"   $ RG_XMM_rm
  , (,) "V"   $ RG_XMM_reg
  , (,) "W"   $ RM_XMM
  ]

lookupOperandType :: String -> String -> OperandType
lookupOperandType i nm =
  case Map.lookup nm operandHandlerMap of
    Just h -> h
    Nothing -> error $ "Unknown operand: " ++ i ++ " " ++ show nm
