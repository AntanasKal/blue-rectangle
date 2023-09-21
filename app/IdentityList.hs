{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- Adding Identity List according to this paper:
-- https://www.researchgate.net/publication/228785589_The_Yampa_Arcade

module IdentityList where

type ILKey = Int
data IL a = IL
  { ilNextKey :: ILKey
  , ilAssocs :: [(ILKey, a)]
  }

emptyIL :: IL a
emptyIL = IL 0 []

insertIL_ :: a -> IL a -> IL a
insertIL_ elem ilist@(IL nextKey assocs) = IL (nextKey + 1) ((nextKey, elem) : assocs)

listToIL :: [a] -> IL a
listToIL = foldr insertIL_ emptyIL

elemsIL :: IL a -> [a]
elemsIL ilist@(IL _ assocs) = map snd assocs

assocsIL :: IL a -> [(ILKey, a)]
assocsIL ilist@(IL _ assocs) = assocs

deleteIL :: ILKey -> IL a -> IL a
deleteIL key ilist@(IL nextKey assocs) = IL nextKey (filter (\assoc -> fst assoc /= key) assocs)

replaceIL :: ILKey -> a -> IL a -> IL a
replaceIL key newVal ilist@(IL nextKey assocs) = IL nextKey (map (\(k, val) -> (k, if key == k then newVal else val)) assocs)

mapIL :: ((ILKey, a) -> b) -> IL a -> IL b
mapIL f ilist@(IL nextKey assocs) = IL nextKey (map (\(key, elem) -> (key, f (key, elem))) assocs)

instance Functor IL where
  fmap f = mapIL (f . snd)

instance Foldable IL where
  foldr f init ilist@(IL _ assocs) = foldr f init (fmap snd assocs)

instance Traversable IL where
  traverse :: (Applicative f) => (a -> f b) -> IL a -> f (IL b)
  traverse f arr = listToIL <$> traverse (f . snd) (assocsIL arr)
