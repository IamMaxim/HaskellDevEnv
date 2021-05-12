{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}

module LastLecture where

-- MONADS --

type Var = String

vars :: [(String, Int)]
vars =
  [ ("x", 3),
    ("y", 4)
  ]

lookupVar :: Var -> Maybe Int
lookupVar var = lookup var vars

-- x + y = ?
addVars :: String -> String -> Maybe Int
addVars x y = lookupVar x `addMaybeInts` lookupVar y

addMaybeInts :: Maybe Int -> Maybe Int -> Maybe Int
addMaybeInts (Just x) (Just y) = Just (x + y)
addMaybeInts _ _ = Nothing

uMaybe ::
  (a -> b) ->
  Maybe a ->
  Maybe b
uMaybe f (Just x) = Just (f x)
uMaybe _ _ = Nothing

addMaybe ::
  (a -> b -> c) ->
  Maybe a ->
  Maybe b ->
  Maybe c
addMaybe f (Just x) (Just y) = Just (f x y)
addMaybe _ _ _ = Nothing

g :: Int -> Int -> Int -> Int
g x y z = x + y * z

apMaybe ::
  Maybe (a -> b) ->
  Maybe a ->
  Maybe b
apMaybe (Just f) (Just x) = Just (f x)
apMaybe _ _ = Nothing

mx :: Maybe Int
mx = lookupVar "x"

my :: Maybe Int
my = lookupVar "y"

mz :: Maybe Int
mz = lookupVar "z"

notOptional :: a -> Maybe a
notOptional = Just

------------------------------

data Result e a
  = Failure e
  | Success a
  deriving (Show, Functor)

lookupVarR :: Var -> Result String Int
lookupVarR var =
  case lookupVar var of
    Nothing -> Failure ("undefined variable: " ++ var)
    Just x -> Success x

success :: a -> Result e a
success = Success

apResult ::
  Semigroup e =>
  Result e (a -> b) ->
  Result e a ->
  Result e b
apResult (Success f) (Success x) = Success (f x)
apResult (Failure err1) (Failure err2) = Failure (err1 <> err2)
apResult (Failure err) _ = Failure err
apResult _ (Failure err) = Failure err

rx = lookupVarR "x"

ry = lookupVarR "y"

rz = lookupVarR "z"

-------

type Person = String

type TableName = String

data DB = DB [(TableName, [(String, String)])]

db :: DB
db =
  DB
    { fathers =
        [ ("Nick", "Jack"),
          ("Anna", "John"),
          ("John", "Jack")
        ],
      mothers =
        [ ("Nick", "Elizabeth"),
          ("Anna", "Michele"),
          ("John", "Elizabeth")
        ]
    }

data DBResult a
  = TableNotFound String
  | NoData
  | Ok a

---

mothers :: DB -> Maybe [(String, String)]
mothers (DB tables) =
  case lookup "mothers" tables of
    Nothing -> TableNotFound "mothers"
    Just x -> Ok x

fathers :: DB -> Maybe [(String, String)]
fathers (DB tables) =
  case lookup "fathers" tables of
    Nothing -> TableNotFound "fathers"
    Just x -> Ok x

---

motherOf :: Person -> Person
motherOf name = lookup name <$> mothers db

fatherOf :: Person -> Person
fatherOf name = lookup name <$> fathers db

noData (Ok (Just x)) = Ok x
noData NoData = NoData
noData (TableNotFound name) = TableNotFound name

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _ = Nothing
bindMaybe (Just x) f = f x

ex1 :: Maybe Int
ex1 =
  lookupVar "x" `bindMaybe` \x ->
    lookupVar "y" `bindMaybe` \y ->
      lookupVar "z" `bindMaybe` \z ->
        Just (x + y * z)

---

bindResult :: Result e a -> (a -> Result e a) -> Result e b
bindResult (Success x) f = f x
bindResult (Failure err) _ = Failure err

ex2 :: Result String Int
ex2 =
  lookupVar "x" `bindResult` \x ->
    lookupVar "y" `bindResult` \y ->
      lookupVar "z" `bindResult` \z ->
        Success (x + y * z)

bindDBResult :: DBResult a -> (a -> DBResult b) -> DBResult b
bindDBResult NoData _ = NoData
bindDBResult (TableNotFound name) _ = TableNotFound name

grandfathers :: Person -> DBResult [Person]
grandfathers name =
  motherOf name `bindDBResult` \mother ->
  fatherOf mother `bindDBResult` \grandfather1 ->
  fatherOf name `bindDBResult` \father ->
  fatherOf father `bindDBResult` \grandfather2 ->
  Ok [grandfather1, grandfather2]
