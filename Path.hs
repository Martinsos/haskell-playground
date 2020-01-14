module Path where

import System.FilePath ((</>))

-- Experiment based on inspired by https://hackage.haskell.org/package/path .

{-
Is it absolute?
Is it relative?
If it is relative, what is it relative to?
Is it file or directory or smth else? We could just say that everything else is also file.
When I see filepath in code, I would like to know answers to questions above.

One thing is when data has value that says it is realtive or absolute. I want it on type lvl: I want type that can't be anything but relative!
-}


------

-- newtype Path entryType relativity = Path FilePath
-- newtype File = File
-- newtype Dir = Dir
-- newtype Rel = Rel
-- newtype Abs = Abs

-- makeRelFilePath :: FilePath -> Path File Rel
-- makeRelFilePath = Path

-- makeAbsFilePath :: FilePath -> Path File Abs
-- makeAbsFilePath = Path

-- makeRelDirPath :: FilePath -> Path Dir Rel
-- makeRelDirPath = Path

-- makeAbsDirPath :: FilePath -> Path Dir Abs
-- makeAbsDirPath = Path

-------

newtype RelFilePath = RelFilePath FilePath deriving (Show, Eq)
newtype AbsFilePath = AbsFilePath FilePath deriving (Show, Eq)
newtype RelDirPath = RelDirPath FilePath deriving (Show, Eq)
newtype AbsDirPath = AbsDirPath FilePath deriving (Show, Eq)

-- TODO: add validations, so wrong file path can't be constructed.
makeRelFilePath :: FilePath -> RelFilePath
makeRelFilePath = RelFilePath

makeAbsFilePath :: FilePath -> AbsFilePath
makeAbsFilePath = AbsFilePath

makeRelDirPath :: FilePath -> RelDirPath
makeRelDirPath = RelDirPath

makeAbsDirPath :: FilePath -> AbsDirPath
makeAbsDirPath = AbsDirPath

class Path p where
    toFilePath :: p -> FilePath
    mapFilePath :: p -> (FilePath -> FilePath) -> p

instance Path RelFilePath where
    toFilePath (RelFilePath path) = path
    mapFilePath p f = RelFilePath $ f $ toFilePath p

instance Path AbsFilePath where
    toFilePath (AbsFilePath path) = path
    mapFilePath p f = AbsFilePath $ f $ toFilePath p

instance Path RelDirPath where
    toFilePath (RelDirPath path) = path
    mapFilePath p f = RelDirPath $ f $ toFilePath p

instance Path AbsDirPath where
    toFilePath (AbsDirPath path) = path
    mapFilePath p f = AbsDirPath $ f $ toFilePath p
 
-- * <ab+cd> style operators below explained:
--     - a and c positions => '/' stands for absolute, '.' stands for relative.
--     - b and d positions => '/' stands for directory, '.' stands for file.
--   So <//+./> reads as "absolute directory + relative directory".

(<//+..>) :: AbsDirPath -> RelFilePath -> AbsFilePath
(AbsDirPath p1) <//+..> (RelFilePath p2) = AbsFilePath (p1 </> p2)

(<//+./>) :: AbsDirPath -> RelDirPath -> AbsDirPath
(AbsDirPath p1) <//+./> (RelDirPath p2) = AbsDirPath (p1 </> p2)

(<./+..>) :: RelDirPath -> RelFilePath -> RelFilePath
(RelDirPath p1) <./+..> (RelFilePath p2) = RelFilePath (p1 </> p2)

(<./+./>) :: RelDirPath -> RelDirPath -> RelDirPath
(RelDirPath p1) <./+./> (RelDirPath p2) = RelDirPath (p1 </> p2)


