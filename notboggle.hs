{-# LANGUAGE DoAndIfThenElse #-}
-- http://www.boardgamegeek.com/thread/300565
import           Control.Applicative ((<$>))
import qualified Control.Monad       as M
import           Control.Monad.State (State)
import qualified Control.Monad.State as ST
import qualified Data.Char           as C
import qualified Data.List           as L
import           Data.Set            (Set)
import qualified Data.Set            as S

import qualified ListRandom          as LR

-- TODO use Text instead of String
-- FIXME Qu die
type Die = String
type Dice = [Die]
type Board = (String, Int)
type Dictionary = Set String
type Game = (Dictionary, Board)

-- FIXME Qu die
englishOld :: Dice
englishOld = ["AACIOT", "AHMORS", "EGKLUY", "ABILTY", "ACDEMP", "EGINTV", "GILRUW", "ELPSTU", "DENOSW", "ACELRS", "ABJMOQ", "EEFHIY", "EHINPS", "DKNOTU", "ADENVZ", "BIFORX"]

englishNew :: Dice
englishNew = ["AAEEGN", "ELRTTY", "AOOTTW", "ABBJOO", "EHRTVW", "CIMOTV", "DISTTY", "EIOSST", "DELRVY", "ACHOPS", "HIMNQU", "EEINSU", "EEGHNW", "AFFKPS", "HLNNRZ", "DEILRX"]

bigEnglish :: Dice
bigEnglish = ["AAAFRS", "AAEEEE", "AAFIRS", "ADENNN", "AEEEEM", "AEEGMU", "AEGMNN", "AFIRSY", "BJKQXZ", "CCENST", "CEIILT", "CEILPT", "CEIPST", "DDHNOT", "DHHLOR", "DHLNOR", "DHLNOR", "EIIITT", "EMOTTT", "ENSSSU", "FIPRSY", "GORRVW", "IPRRRY", "NOOTUW", "OOOTTU"]

-- For use with (english[Old|New]|bigEnglish)
englishChallengeCube :: Die
englishChallengeCube = "IKLMQU"

german :: Dice
german = ["PTESUL", "ENTVIG", "PEDCAM", "RESCAL", "VANZED", "RILWEU", "FEESIH", "TONKEU", "RESNIH", ",TAAEIO", "ENTSOD", "BOQJAM", "ROSMAI", "YUNGLE", "FOXRAI", "BARTIL"]

prettyPrint :: String -> String
prettyPrint [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p] =
        [a,b,c,d,'\n',e,f,g,h,'\n',i,j,k,l,'\n',m,n,o,p]
prettyPrint [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y] =
        [a,b,c,d,e,'\n',f,g,h,i,j,'\n',k,l,m,n,o,'\n',p,q,r,s,t,'\n',u,v,w,x,y]
prettyPrint _ = error "Not yet implemented."

main :: IO ()
main = do dice <- LR.shuffle englishNew
          rolledDice <- mapM LR.pick dice
          putStrLn $ prettyPrint rolledDice
          d <- wordDictionary
          --print =<< solveBoard (d, (rolledDice, 4))
          print $ solveBoardST (d, (rolledDice, 4))

wordDictionary :: IO (Set String)
wordDictionary =
  (S.fromList . filter (\s -> not (null s) && (not . isPossesive) s && (not . isProperNoun) s) . lines) <$> readFile "/usr/share/dict/words"
  where isPossesive s = "'s" `L.isSuffixOf` s
        isProperNoun = C.isUpper . head

lowerCase :: String -> String
lowerCase = map C.toLower

isWord :: Dictionary -> String -> Bool
isWord dict s = S.member (lowerCase s) dict

indicesAround :: Int -> Int -> Int -> [(Int, Int)]
indicesAround maxIndex x0 y0 =
  [{-# SCC "tuple" #-} (x1, y1)
    | x1 <- {-# SCC "x_list" #-} [x0 - 1, x0, x0 + 1],
      y1 <- {-# SCC "y_list" #-} [y0 - 1, y0, y0 + 1],
      x1 >= 0, y1 >= 0,
      x1 <= maxIndex, y1 <= maxIndex,
      {-# SCC "not_same" #-} (x1, y1) /= (x0, y0)]

indexBoard :: Board -> Int -> Int -> Char
indexBoard (s, boardLength) x y = s !! (y * boardLength + x)

findWordsAtST :: Game -> Int -> Int -> Set String
findWordsAtST (dictionary, board@(_, boardLength)) x y = ST.execState (findWordsAtHelp x y S.empty []) S.empty
  where findWordsAtHelp :: Int -> Int -> Set (Int, Int) -> String -> State (Set String) ()
        findWordsAtHelp x0 y0 nodesVisited0 wordSoFar =
          if alreadyVisitedNode then
            return ()
          else do
            M.when (isWord dictionary currentWord) (ST.modify (S.insert currentWord))
            mapM_ (\(x1, y1) -> findWordsAtHelp x1 y1 nodesVisited1 currentWord) nextSteps
          where c = indexBoard board x0 y0
                currentWord = wordSoFar ++ [c]
                nodesVisited1 = S.insert (x0, y0) nodesVisited0
                maxIndex = boardLength - 1
                nextSteps = indicesAround maxIndex x0 y0
                alreadyVisitedNode = S.member (x0, y0) nodesVisited0

findWordsAt :: Game -> Int -> Int -> IO (Set String)
findWordsAt (dictionary, board@(_, boardLength)) x y = findWordsAtHelp x y S.empty S.empty []
  where findWordsAtHelp :: Int -> Int -> Set (Int, Int) -> Set String -> String -> IO (Set String)
        findWordsAtHelp x0 y0 nodesVisited0 wordsSoFar wordSoFar =
          if alreadyVisitedNode then
            return wordsSoFar
          else if isWord dictionary currentWord then
            S.unions <$> mapM (\(x1, y1) -> findWordsAtHelp x1 y1 nodesVisited1 (S.insert currentWord wordsSoFar) currentWord) nextSteps
          else
            --(putStrLn $ "not a word: " ++ currentWord) >>
            S.unions <$> mapM (\(x1, y1) -> findWordsAtHelp x1 y1 nodesVisited1 wordsSoFar currentWord) nextSteps
          where c = indexBoard board x0 y0
                currentWord = wordSoFar ++ [c]
                nodesVisited1 = S.insert (x0, y0) nodesVisited0
                maxIndex = boardLength - 1
                nextSteps = indicesAround maxIndex x0 y0
                alreadyVisitedNode = S.member (x0, y0) nodesVisited0

solveBoard :: Game -> IO (Set String)
solveBoard game@(_, (_, boardLength)) = S.unions <$> mapM (\(x, y) -> findWordsAt game x y) [(x, y) | x <- [0 .. boardLength - 1], y <- [0 .. boardLength - 1]]

solveBoardST :: Game -> Set String
solveBoardST game@(_, (_, boardLength)) = S.unions $ map (\(x, y) -> findWordsAtST game x y) [(x, y) | x <- [0 .. boardLength - 1], y <- [0 .. boardLength - 1]]
