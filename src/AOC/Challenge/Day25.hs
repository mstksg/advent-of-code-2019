{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : AOC.Challenge.Day25
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 25.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day25 (
    day25a
  ) where

import           AOC.Common                         (Dir(..))
import           AOC.Common.Conduino
import           AOC.Common.Intcode
import           AOC.Common.Search                  (aStar)
import           AOC.Common.Subset
import           AOC.Prelude hiding                 (many, noneOf)
import           AOC.Solver                         ((:~>)(..))
import           AOC.Util                           (firstJust)
import           Control.DeepSeq                    (NFData)
import           Control.Lens                       (foldOf, folded, itraversed, iforOf)
import           Control.Monad                      (ap, guard)
import           Data.Char                          (isDigit)
import           Data.Foldable                      (fold, toList)
import           Data.Generics.Labels               ()
import           Data.Group
import           Data.List.NonEmpty                 (NonEmpty(..))
import           Data.Map                           (Map)
import           Data.Map.NonEmpty                  (NEMap)
import           Data.Set                           (Set)
import           Data.Text                          (Text)
import           Data.Void                          (Void)
import           GHC.Generics                       (Generic)
import           Text.Megaparsec
import           Text.Read                          (readMaybe)
import qualified Control.Monad.Combinators.NonEmpty as NE
import qualified Data.Map                           as M
import qualified Data.Map.NonEmpty                  as NEM
import qualified Data.Set                           as S
import qualified Data.Text                          as T

data Room n a = R
    { rTitle  :: !Text
    , rDesc   :: !Text
    , rDoors  :: !(NEMap Dir a)
    , rItems  :: !(Set Text)
    , rNote   :: !(Maybe n)
    }
  deriving (Show, Eq, Ord, Foldable, Functor, Traversable, Generic)
instance (NFData n, NFData a) => NFData (Room n a)

data ShipMap = SM
    { smMap        :: !(Map Text (Room Void Text))
    , smStart      :: !Text
    , smPressure   :: !Dir
    }
  deriving (Show, Eq, Ord, Generic)

dirCmd :: Dir -> Text
dirCmd = \case
    North -> "north"
    East  -> "east"
    South -> "south"
    West  -> "west"

explore :: Memory -> Maybe ShipMap
explore m = do
    (mp, startName) <- go Nothing $
           toAsciiVM $ untilHalt (stepN @VMErr 100000 m)
    checkpoint  <- M.lookup "Security Checkpoint" mp
    pressureDir <- firstJust (\(d, p) -> d <$ guard (p == "Pressure-Sensitive Floor")) $
                        NEM.toList (rDoors checkpoint)
    pure SM
      { smMap      = mp
      , smStart    = startName
      , smPressure = pressureDir
      }
  where
    go  :: Maybe (Dir, Text)        -- ^ where did we come from
        -> AsciiVM (Either IErr) ()
        -> Maybe (Map Text (Room Void Text), Text)  -- ^ room name, graph
    go comeFrom bot = do
      (room, next) <- explorePoint bot
      (newRooms, room') <- sequenceA . fmap sequenceA . iforOf (#rDoors . itraversed) room $ \d () ->
            case comeFrom of
              Just (c, t)
                | c == d -> pure (Just t)
              _ -> case go (Just (invert d, rTitle room)) (next d) of
                Nothing -> pure Nothing
                Just (rest, rname) -> (rest, Just rname)
      pure ( M.singleton (rTitle room) room' <> newRooms
           , rTitle room
           )
    explorePoint
        :: AsciiVM (Either IErr) ()
        -> Maybe (Room Void (), Dir -> AsciiVM (Either IErr) ())
    explorePoint bot = do
      (out, goThere) <- case squeezePipe bot of
            Left e        -> error $ show e
            Right (os, r) -> case r of
              Left  next -> pure (T.unlines os, next . Right)
              Right _    -> Nothing
      room :| _ <- case runParser (NE.some parseRoomDesc) "" out of
        Left e -> error $ errorBundlePretty e
        Right x -> pure x
      let goodItems = flip S.filter (rItems room) $ \item ->
            case squeezePipe (goThere ("take " <> item)) of
              Left e -> error $ show e
              Right (_ , Left next) ->
                let (testDoor, _) = NEM.findMin (rDoors room)
                in  case squeezePipe (next $ Right (dirCmd testDoor)) of
                      Left e -> error $ show e
                      Right (os, Left  _) -> not $ "stuck" `T.isInfixOf` T.unlines os
                      Right (_ , Right _) -> False
              Right (_ , Right _) -> False
      pure ( room { rItems = goodItems, rNote = Nothing }
           , goThere . dirCmd
           )

data BotState = BS { bsLoc   :: !Text
                   , bsItems :: !(Set Text)
                   }
  deriving (Show, Eq, Ord, Generic)

checkpointRoute
    :: ShipMap
    -> Maybe [Text]
checkpointRoute sm@SM{..} = do
    stateRoute <- snd <$> aStar bHeuristic
                            stepBot
                            (BS smStart S.empty)
                            ((== 0) . bHeuristic)
    pure $ concatMap (uncurry toRoute) $ (zip`ap`tail) stateRoute
  where
    allItems = foldOf (#smMap . folded . #rItems) sm
    bHeuristic BS{..} = S.size (allItems `S.difference` bsItems)
                              + if bsLoc == "Security Checkpoint"
                                  then 0 else 1
    stepBot BS{..} = M.fromList
        [ (BS{bsLoc = nextRoom, bsItems = bsItems `S.union` rItems }, 1)
        | bsLoc /= "Security Checkpoint"
        , let R{..} = smMap M.! bsLoc
        , nextRoom <- toList rDoors
        , nextRoom /= "Pressure-Sensitive Floor"
        ]
    toRoute bs0 bs1 = S.toList getItems ++ [dirCmd dir]
      where
        R{..} = smMap M.! bsLoc bs0
        Just dir = firstJust (\(d, n) -> d <$ guard (n == bsLoc bs1)) . NEM.toList $ rDoors
        getItems = S.map ("take " <>) $ rItems `S.difference` bsItems bs0

getToCheckpoint
    :: Memory
    -> ShipMap
    -> Maybe (Text -> AsciiVM (Either IErr) ())
getToCheckpoint mem sm = do
    route <- checkpointRoute sm
    case feedPipe route initVM of
      Left e -> error $ show e
      Right (_, Left next) -> pure next
      Right (_, Right _  ) -> empty
  where
    initVM :: AsciiVM (Either IErr) ()
    initVM = toAsciiVM $ untilHalt (stepForever @VMErr mem)

_testSearch
    :: Memory
    -> IO ()
_testSearch mem = do
    Just !sm <- pure $ explore mem
    putStrLn "Done exploring"
    Just bot <- pure $ getToCheckpoint mem sm
    putStrLn "Here we go"
    interactAsciiVM (bot "inv")

searchCheckpoint
    :: (Text -> AsciiVM (Either IErr) ())   -- ^ bot at checkpoint with all items
    -> ShipMap
    -> Maybe (Set Text, Text)
searchCheckpoint bot sm = do
    goodSet      <- join $ findSubset (fmap fst . testSet) True allItems
    (EQ, outRes) <- testSet goodSet
    pure (goodSet, outRes)
  where
    allItems = foldOf (#smMap . folded . #rItems) sm
    testSet xs = case feedPipe inps (bot i0) of
        Left e -> error $ show e
        Right (T.unlines->os, _) -> case runParser parseResp "" os of
          Left _ -> pure (EQ, os)
          Right (r :| _) -> case rNote r of
            Nothing -> Nothing
            Just n
              | "heavier" `T.isInfixOf` n -> pure (LT, os)
              | "lighter" `T.isInfixOf` n -> pure (GT, os)
              | otherwise                 -> error $ "no parse: " ++ T.unpack n
      where
        drops   = S.map ("drop " <>) (allItems `S.difference` xs)
        i0:inps = S.toList drops ++ [dirCmd (smPressure sm)]
    parseResp = do
      skipMany (noneOf ['='])
      NE.some parseRoomDesc

_bruteForceCheckpoint
    :: (Text -> AsciiVM (Either IErr) ())   -- ^ bot at checkpoint with all items
    -> ShipMap
    -> Maybe (Set Text, Text)
_bruteForceCheckpoint bot sm = flip firstJust (S.powerSet allItems) $ \is ->
    let drops = S.map ("drop " <>) is
        i0:inps = S.toList drops ++ [dirCmd (smPressure sm)]
        heldItems = allItems `S.difference` is
    in  case feedPipe inps (bot i0) of
          Left e -> error $ show e
          Right (os, _) -> case runParser parseResp "" (T.unlines os) of
            Left _ -> pure (heldItems, T.unlines os)
            Right (r :| _) -> case rNote r of
              Nothing -> empty
              Just _  -> empty
  where
    allItems = foldOf (#smMap . folded . #rItems) sm
    parseResp = do
      skipMany (noneOf ['='])
      NE.some parseRoomDesc

day25a :: Memory :~> Int
day25a = MkSol
    { sParse = parseMem
    , sShow  = show
    , sSolve = \m -> do
        sm       <- explore m
        bot      <- getToCheckpoint m sm
        -- (_, out) <- bruteForceCheckpoint bot sm
        (_, out) <- searchCheckpoint bot sm
        readMaybe . filter isDigit $ T.unpack out
    }

type Parser = Parsec Void Text

parseRoomDesc :: Parser (Room Text ())
parseRoomDesc = do
    skipMany "\n"
    rTitle <- T.strip . T.pack <$> between "==" "==" (many (noneOf ['=']))
    skipMany "\n"
    rDesc  <- T.strip . T.pack <$> manyTill anySingle "\n"
    skipMany "\n"
    _ <- "Doors here lead:\n"
    NEM.IsNonEmpty rDoors <- M.fromSet (const ()) <$> parseList dirParser
    skipMany "\n"
    rItems <- fmap fold . optional $ do
        _ <- "Items here:\n"
        parseList itemParser
    skipMany "\n"
    rNote <- optional $ do
      _ <- "A loud, robotic voice says "
      n <- T.pack <$> between "\"" "\"" (many (noneOf ['"']))
      " and you are ejected back to the checkpoint."
      pure n
    skipMany "\n"
    pure R{..}
  where
    dirParser = choice
        [ North <$ "north"
        , East  <$ "east"
        , South <$ "south"
        , West  <$ "west"
        ]
    itemParser = T.pack <$> many (noneOf ['\n'])
    parseList p = S.fromList <$> many (between "- " "\n" p)



