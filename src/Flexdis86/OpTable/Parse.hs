{- |
Module      : Flexdis86.OpTable.Parse
Copyright   : (c) Galois, Inc 2026
Description : Parse @optable.xml@ into a list of 'Def's
Maintainer  : langston@galois.com

This module is used only at compile time (via a Template Haskell splice in
"Flexdis86.DefaultParser"); it is not needed at runtime.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}

module Flexdis86.OpTable.Parse
  ( parseOpTable
  ) where

import qualified Control.Monad.Fail as MF
import           Control.Monad (forM_, unless, when)
import           Control.Monad.State ( MonadState(..)
                                     , execStateT
                                     , gets
                                     )
import           Data.Bits ((.|.), shiftL, shiftR)
import qualified Data.Binary as Bin
import           Data.Binary.Put (Put, runPut)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import           Data.Char (isDigit, isSpace)
import           Data.Containers.ListUtils (nubOrd)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Word (Word8)
import           Numeric (readDec, readHex)
import           Text.XML.Light ( Content(..)
                                , Element(..)
                                , Line
                                , QName(..)
                                , cdData
                                , cdVerbatim
                                , elContent
                                , elLine
                                , elName
                                , parseXMLDoc
                                , CDataKind(..)
                                )

import           Lens.Micro.Mtl ((%=), (.=), (?=), use)

import           Flexdis86.OpTable
import           Flexdis86.Sizes (SizeConstraint(..), ModConstraint(..), asFin8, asFin64, maskFin8)

------------------------------------------------------------------------
-- ElemParser

qname :: String -> QName
qname nm = QName { qName = nm, qURI = Nothing, qPrefix = Nothing }

ppQName :: QName -> String
ppQName qnm = qName qnm

data ElemState = ElemState { esName    :: QName
                           , esContent :: [Content]
                           , esLine    :: Maybe Line
                           }

mkElemState :: Element -> ElemState
mkElemState e = ElemState { esName    = elName e
                          , esContent = elContent e
                          , esLine    = elLine e
                          }

-- | Full parser state: current element context plus streaming
-- 'Binary'-serialized Def accumulator ('Put'). The accumulator is never
-- saved/restored by 'runWithElement'.
data ParseState = ParseState { psElem    :: !ElemState
                             , psBuilder :: !Put
                             }

newtype ElemParser a = EP { unEP :: ParseState -> Either String (a, ParseState) }

instance Functor ElemParser where
  fmap f m = EP (\s -> case unEP m s of
                         Left e       -> Left e
                         Right (v,s') -> Right (f v, s'))

instance Applicative ElemParser where
  pure v    = EP $ \s -> Right (v, s)
  m <*> h   = EP $ \s -> do
    (f, s1) <- unEP m s
    (x, s2) <- unEP h s1
    return (f x, s2)

instance Monad ElemParser where
  return = pure
  m >>= h = EP $ \s -> do
    (v, s') <- unEP m s
    unEP (h v) s'
#if !(MIN_VERSION_base(4,13,0))
  fail = MF.fail
#endif

instance MF.MonadFail ElemParser where
  fail e = EP $ \s -> Left $ show (esLine (psElem s)) ++ ": " ++ e

instance MonadState ElemState ElemParser where
  get   = EP (\s -> Right (psElem s, s))
  put v = EP (\s -> Right ((), s { psElem = v }))

-- | Run a sub-parser on a child element, restoring the element context
-- afterwards while preserving any Defs emitted into the accumulator.
runWithElement :: ElemParser a -> Element -> ElemParser a
runWithElement m e = EP $ \s ->
  let s' = s { psElem = mkElemState e }
  in case unEP m s' of
       Left err       -> Left err
       Right (v, s'') -> Right (v, s'' { psElem = psElem s })

runElemParser :: ElemParser () -> Element -> Either String Put
runElemParser m e =
  case unEP m (ParseState (mkElemState e) mempty) of
    Left err     -> Left err
    Right (_, s) -> Right (psBuilder s)

-- | Emit a 'Def' into the streaming Binary accumulator.
emitDef :: Def -> ElemParser ()
emitDef d = EP $ \s -> Right
  ( ()
  , s { psBuilder = psBuilder s <> Bin.put d }
  )

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
        Elem e                                    -> return (Just e)
        Text d | dropWhile isSpace (cdData d) == "" -> getNextElt
        t                                         -> fail $ "Unexpected non-whitespace text: " ++ show t

pushElt :: Element -> ElemParser ()
pushElt e = do
  s <- get
  put s { esContent = Elem e : esContent s }

next :: ElemParser a -> ElemParser a
next p = do
  me <- getNextElt
  case me of
    Just e  -> runWithElement p e
    Nothing -> fail "Failed to find next element."

opt :: String -> ElemParser a -> ElemParser (Maybe a)
opt nm p = do
  me <- getNextElt
  case me of
    Nothing -> return Nothing
    Just e
      | elName e == qname nm -> fmap Just $ runWithElement p e
      | otherwise            -> pushElt e >> return Nothing

checkEnd :: ElemParser ()
checkEnd = do
  me <- getNextElt
  case me of
    Nothing -> return ()
    Just e  -> pushElt e >> fail "Expected end of elements."

remainingElts_ :: ElemParser () -> ElemParser ()
remainingElts_ p = do
  me <- getNextElt
  case me of
    Nothing -> return ()
    Just e  -> runWithElement p e >> remainingElts_ p

asText :: ElemParser String
asText = do
  s <- get
  case esContent s of
    [Text d] | cdVerbatim d == CDataText -> return (cdData d)
    x -> fail (unlines [ "asText found non-text.", show x])

required_text :: String -> ElemParser String
required_text nm = next $ checkTag nm >> asText

------------------------------------------------------------------------
-- Mode / CPURequirement / Vendor parsers

parse_mode :: ElemParser (Maybe Mode)
parse_mode = opt "mode" $ do
  txt <- asText
  case txt of
    "def64" -> return Default64
    "inv64" -> return Invalid64
    _       -> fail $ "Invalid mode: " ++ show txt

insClassMap :: Map.Map String CPURequirement
insClassMap = Map.fromList
  [ (,) "X87"        X87
  , (,) "X87 UNDOC"  X87_undocumented
  , (,) "smx"        SMX
  , (,) "sse"        SSE
  , (,) "sse2"       SSE2
  , (,) "sse3"       SSE3
  , (,) "sse3 atom"  SSE3_atom
  , (,) "sse4.1"     SSE4_1
  , (,) "sse4.2"     SSE4_2
  , (,) "aesni"      AESNI
  , (,) "sha"        SHA
  , (,) "avx"        AVX
  , (,) "bmi2"       BMI2
  , (,) "adx"        ADX
  ]

parse_CPURequirement :: CPURequirement -> ElemParser CPURequirement
parse_CPURequirement c = do
  fmap (fromMaybe c) $ opt "class" $ do
    txt <- asText
    case Map.lookup txt insClassMap of
      Nothing -> fail $ "Unknown instruction class: " ++ show txt
      Just v  -> do
        when (c /= Base) $
          fail $ "Instruction class already defined " ++ show (c, v)
        return v

parse_vendor :: Maybe Vendor -> ElemParser (Maybe Vendor)
parse_vendor v = do
  fmap (maybe v Just) $ opt "vendor" $ do
    when (maybe False (const True) v) $
      fail "Vendor already defined."
    txt <- asText
    case txt of
      "amd"   -> return AMD
      "intel" -> return Intel
      _       -> fail $ "Unknown vendor: " ++ show txt

------------------------------------------------------------------------
-- VEX prefix helpers (used only during parsing)

data VexSpec = VexSpec
  { vexUseVVVV    :: Maybe VexUseVVVV
  , vexSize       :: VexSize
  , vexImpSimd    :: Maybe VexImpSimd
  , vexImpOpc     :: Maybe VexImpOpc
  , vexW          :: Maybe VexW
  , vexAllowShort :: Bool
  } deriving (Eq, Show)

data VexUseVVVV = VEX_NDS | VEX_NDD | VEX_DDS      deriving (Eq, Show)
data VexSize    = Vex128 | Vex256                   deriving (Eq, Show)
data VexImpSimd = Imp0x66 | Imp0xF3 | Imp0xF2       deriving (Eq, Show)
data VexImpOpc  = Imp0x0F | Imp0x0F38 | Imp0x0F3A   deriving (Eq, Show)
data VexW       = VexW0 | VexW1                     deriving (Eq, Show)

parseVex :: String -> VexSpec
parseVex inp =
  VexSpec { vexUseVVVV    = vvvv
          , vexSize       = size
          , vexImpSimd    = impS
          , vexImpOpc     = impO
          , vexW          = w
          , vexAllowShort = short
          }
  where
  ts0 = chunks inp

  (vvvv, ts1) =
    case ts0 of
      f : fs | f == "NDS" -> (Just VEX_NDS, fs)
             | f == "NDD" -> (Just VEX_NDD, fs)
             | f == "DDS" -> (Just VEX_DDS, fs)
      _                   -> (Nothing, ts0)

  (size, ts2) =
    case ts1 of
      f : fs | f == "128" -> (Vex128, fs)
             | f == "256" -> (Vex256, fs)
      _ -> error "VEX specification is missing its size field"

  (impS, ts3) =
    case ts2 of
      f : fs | f == "66" -> (Just Imp0x66, fs)
             | f == "F2" -> (Just Imp0xF2, fs)
             | f == "F3" -> (Just Imp0xF3, fs)
      _                  -> (Nothing, ts2)

  (impO, ts4) =
    case ts3 of
      f : fs | f == "0F"   -> (Just Imp0x0F,   fs)
             | f == "0F3A" -> (Just Imp0x0F3A, fs)
             | f == "0F38" -> (Just Imp0x0F38, fs)
      _                    -> (Nothing, ts3)

  (w, ts5) =
    case ts4 of
      f : fs | f == "W0" -> (Just VexW0, fs)
             | f == "W1" -> (Just VexW1, fs)
      _                  -> (Nothing, ts4)

  short =
    case ts5 of
      f : _ | f == "WIG" -> True
      _                  -> False

  chunks x = if null x then []
                       else case break (== '.') x of
                              (as, _:bs) -> as : chunks bs
                              _          -> [x]

vexMayBeShort :: VexSpec -> Bool
vexMayBeShort vp =
  case vexImpOpc vp of
    Just Imp0x0F38  -> False
    Just Imp0x0F3A  -> False
    _ -> case vexW vp of
           Just VexW1 -> False
           _          -> vexAllowShort vp

vexToBytes :: VexSpec -> [[Word8]]
vexToBytes vp = short ++ long
  where
  long  = [ [0xC4, b1, b2] | b1 <- longByte1, b2 <- longByte2 ]
  short = if vexMayBeShort vp then [ [0xC5, b] | b <- shortByte ] else []

  shortByte = nubOrd
              [ field 7 r .|. field 3 vvvv .|. field 2 l .|. pp
              | r <- [0, 1], vvvv <- vvvvVals, l <- lVals, pp <- ppVals ]
  longByte1 = nubOrd
              [ field 5 rxb .|. m
              | rxb <- [0 .. 7], m <- mmmmVals ]
  longByte2 = nubOrd
              [ field 7 ww .|. field 3 vvvv .|. field 2 l .|. pp
              | ww <- wVals, vvvv <- vvvvVals, l <- lVals, pp <- ppVals ]

  field amt x = shiftL x amt

  vvvvVals = case vexUseVVVV vp of
               Nothing -> [15]
               Just _  -> [0 .. 15]

  wVals    = case vexW vp of
               Nothing    -> [0, 1]
               Just VexW0 -> [0]
               Just VexW1 -> [1]

  lVals    = case vexSize vp of
               Vex128 -> [0]
               Vex256 -> [1]

  ppVals   = case vexImpSimd vp of
               Nothing      -> [0]
               Just Imp0x66 -> [1]
               Just Imp0xF3 -> [2]
               Just Imp0xF2 -> [3]

  mmmmVals = case vexImpOpc vp of
               Nothing  -> []
               Just imp ->
                 case imp of
                   Imp0x0F   -> [1]
                   Imp0x0F38 -> [2]
                   Imp0x0F3A -> [3]

------------------------------------------------------------------------
-- Opcode / mnemonic parsers

addOpcode :: MonadState Def m => Word8 -> m ()
addOpcode c = defOpcodes %= (++ [c])

setDefCPUReq :: MonadState Def m => CPURequirement -> m ()
setDefCPUReq r = do
  creq <- use defCPUReq
  when (creq == Base) $ defCPUReq .= r

parse_opcode :: (MonadState Def m, MF.MonadFail m) => String -> m ()
parse_opcode nm = do
  case readHex nm of
    [(v, "")] -> addOpcode v
    _ | Just r <- List.stripPrefix "/a=" nm
      -> case r of
           "16" -> reqAddrSize ?= Size16
           "32" -> reqAddrSize ?= Size32
           "64" -> reqAddrSize ?= Size64
           _    -> fail $ "Unexpected address size: " ++ r
    _ | Just r <- List.stripPrefix "/m=" nm
      -> case r of
           "32"  -> modeLimit .= Only32
           "64"  -> modeLimit .= Only64
           "!64" -> modeLimit .= Not64
           _     -> fail $ "Unexpected mode limit: " ++ r
    _ | Just r <- List.stripPrefix "/mod=" nm
      -> case r of
           "11"  -> requiredMod ?= OnlyReg
           "!11" -> requiredMod ?= OnlyMem
           _     -> fail $ "Unexpected mod constraint: " ++ r
    _ | Just r <- List.stripPrefix "/o=" nm
      , [(b, "")] <- readDec r
      , b `elem` [16, 32, 64 :: Int]
      -> case r of
           "16" -> reqOpSize ?= OpSize16
           "32" -> reqOpSize ?= OpSize32
           "64" -> reqOpSize ?= OpSize64
           _    -> fail $ "Unexpected operand size: " ++ r
    _ | Just r <- List.stripPrefix "/reg=" nm
      , [(b, "")] <- readHex r
      , Just v    <- asFin8 b
      -> requiredReg ?= v
    _ | Just r <- List.stripPrefix "/rm=" nm
      , [(b, "")] <- readHex r
      , Just v    <- asFin8 b
      -> requiredRM ?= v
    _ | Just r <- List.stripPrefix "/3dnow=" nm
      , [(b, "")] <- readHex r
      -> do setDefCPUReq AMD_3DNOW
            addOpcode b
    _ | Just r <- List.stripPrefix "/sse=" nm
      , [(b, "")] <- readHex r
      -> do setDefCPUReq SSE
            requiredPrefix ?= b
    _ | Just r <- List.stripPrefix "/x87=" nm
      , [(b, "")] <- readHex r
      , Just modRM <- asFin64 b
      -> do setDefCPUReq X87
            x87ModRM ?= modRM
            -- FIXME: sjw: HACK to avoid making the parser more complex.  Basically, we
            -- pretend we want both Reg and R/M
            requiredRM  ?= maskFin8 b -- bottom 3 bits
            requiredReg ?= maskFin8 (b `shiftR` 3)
    -- This is a special hack used for the endbr32 and endbr64 instructions.
    -- The first byte in their opcodes is parsed as a REP prefix, so we make
    -- sure that it is present by marking it as a required prefix. See
    -- Note [x86_64 disassembly] in Flexdis86.Disassembler for more details.
    _ | Just r <- List.stripPrefix "/reqpfx=" nm
      , [(b, "")] <- readHex r
      -> requiredPrefix ?= b
    _ | Just r <- List.stripPrefix "/vex=" nm
      -> do setDefCPUReq AVX
            vexPrefixes .= vexToBytes (parseVex r)
    _ -> fail $ "Unexpected opcode: " ++ show nm

isMnemonic :: String -> Bool
isMnemonic (h:r) = ('a' <= h && h <= 'z' || h == '_') && all isLowerOrDigit r
isMnemonic []    = False

isLowerOrDigit :: Char -> Bool
isLowerOrDigit c = 'a' <= c && c <= 'z' || isDigit c || c == '_'

parse_mnemonics :: ElemParser (BS.ByteString, [BS.ByteString])
parse_mnemonics = do
  mnems <- words <$> required_text "mnemonic"
  forM_ mnems $ \mnem -> do
    unless (isMnemonic mnem) $
      fail $ "Invalid mnemonic: " ++ show mnem
  case BSC.pack <$> mnems of
    []    -> fail "Empty mnemonic element!"
    (h:t) -> return (h, t)

------------------------------------------------------------------------
-- Def parser

strictReverse :: [a] -> [a]
strictReverse = go []
  where go :: [a] -> [a] -> [a]
        go r []    = r
        go r (a:l) = (go $! (a:r)) l

strictMapList :: Monad m => (a -> m b) -> [a] -> m [b]
strictMapList = go []
  where go :: Monad m => [b] -> (a -> m b) -> [a] -> m [b]
        go r _ []    = pure $! strictReverse r
        go r f (a:l) = do b <- f a
                          seq b $ go (b:r) f l

parse_def ::
  BS.ByteString ->
  [BS.ByteString] ->
  CPURequirement ->
  Maybe Vendor ->
  ElemParser ()
parse_def nm syns creq v = do
  checkTag "def"
  let parse_prefix = fromMaybe [] . fmap words <$> opt "pfx" asText
  prefix     <- parse_prefix
  opc_text   <- required_text "opc"
  oprndNames <- fromMaybe [] . fmap words <$> opt "opr" asText
  mode       <- parse_mode
  creq'      <- parse_CPURequirement creq
  v'         <- parse_vendor v
  checkEnd
  when (creq' `elem` (Base : supportedCPUReqs)) $ do
    oprnds <- strictMapList (lookupOperandType nm) oprndNames
    let d0 = Def { _defMnemonic        = nm
                 , _defMnemonicSynonyms = syns
                 , _defCPUReq          = creq'
                 , _defVendor          = v'
                 , _modeLimit          = AnyMode
                 , _defMode            = mode
                 , _reqAddrSize        = Nothing
                 , _reqOpSize          = Nothing
                 , _defPrefix          = prefix
                 , _requiredPrefix     = Nothing
                 , _defOpcodes         = []
                 , _requiredMod        = Nothing
                 , _requiredReg        = Nothing
                 , _requiredRM         = Nothing
                 , _x87ModRM           = Nothing
                 , _vexPrefixes        = []
                 , _defOperands        = oprnds
                 }
    d <- seq d0 $ flip execStateT d0 $ mapM_ parse_opcode (words opc_text)
    if defSupported d then emitDef d else return ()

skipRemainingElts :: ElemParser ()
skipRemainingElts = do
  me <- getNextElt
  case me of
    Nothing -> return ()
    Just _  -> skipRemainingElts

parse_instruction :: ElemParser ()
parse_instruction = do
  checkTag "instruction"
  (mnem, syns) <- parse_mnemonics
  cpuReq       <- parse_CPURequirement Base
  v            <- parse_vendor Nothing
  if cpuReq `notElem` (Base : supportedCPUReqs)
    then skipRemainingElts
    else remainingElts_ (parse_def mnem syns cpuReq v)

parse_x86_optable :: ElemParser ()
parse_x86_optable = do
  checkTag "x86optable"
  remainingElts_ parse_instruction

-- | Parse the optable.xml file, returning only the 'Def's as a 'Data.Binary'-encoded bytestring.
--
-- Only returns 'Def's supported by flexdis86 (i.e., those for which
-- 'defSupported' returns 'True').
parseOpTable :: BS.ByteString -> Either String LBS.ByteString
parseOpTable bs =
  case parseXMLDoc bs of
    Nothing  -> Left "Not an XML document"
    Just elt -> case runElemParser parse_x86_optable elt of
      Left err     -> Left err
      Right putAcc -> Right $ runPut putAcc
