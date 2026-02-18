{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Core categorical definitions for the hidden variable debates analysis.
-- Implements objects, morphisms, functors, natural transformations,
-- and the Yoneda embedding in the measurement category framework.
module Category where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | A category is specified by its objects and morphisms.
-- We represent small categories concretely as directed graphs with composition.
data Category obj mor = Category
  { catObjects   :: [obj]
  , catMorphisms :: [(obj, obj, mor)]
  , catCompose   :: mor -> mor -> Maybe mor
  , catIdentity  :: obj -> mor
  }

-- | A functor between categories.
data Functor' obj1 mor1 obj2 mor2 = Functor'
  { fObj :: obj1 -> obj2
  , fMor :: mor1 -> mor2
  }

-- | A natural transformation between two functors.
-- Components indexed by objects of the source category.
data NatTrans obj1 mor1 obj2 mor2 = NatTrans
  { ntSource :: Functor' obj1 mor1 obj2 mor2
  , ntTarget :: Functor' obj1 mor1 obj2 mor2
  , ntComponent :: obj1 -> mor2
  }

-- | The representable presheaf Hom(A, -) for object A.
-- Given A, returns a function that maps each object B to the set Hom(A, B).
representablePresheaf
  :: (Eq obj, Eq mor)
  => Category obj mor
  -> obj
  -> obj
  -> [mor]
representablePresheaf cat a b =
  [ m | (s, t, m) <- catMorphisms cat, s == a, t == b ]

-- | The Yoneda embedding: sends an object A to its representable presheaf.
-- y(A) = Hom(A, -) : C -> Set
yonedaEmbedding
  :: (Eq obj, Eq mor)
  => Category obj mor
  -> obj
  -> (obj -> [mor])
yonedaEmbedding cat a = representablePresheaf cat a

-- | Check if two objects are isomorphic by comparing their representable presheaves.
-- By the Yoneda lemma, y(A) = y(B) implies A = B (up to iso).
yonedaEquivalent
  :: (Eq obj, Eq mor)
  => Category obj mor
  -> obj
  -> obj
  -> Bool
yonedaEquivalent cat a b =
  let ya = map (\o -> (o, representablePresheaf cat a o)) (catObjects cat)
      yb = map (\o -> (o, representablePresheaf cat b o)) (catObjects cat)
  in ya == yb

-- | Check if a functor is faithful (injective on morphisms).
isFaithful
  :: (Eq mor2)
  => [(mor1, mor2)]
  -> Bool
isFaithful mappings =
  let images = map snd mappings
  in length images == length (nub' images)
  where
    nub' [] = []
    nub' (x:xs) = x : nub' (filter (/= x) xs)

-- | Check if a functor is full (surjective on hom-sets).
isFull
  :: (Eq obj1, Eq obj2, Eq mor2)
  => Category obj1 mor1
  -> Category obj2 mor2
  -> Functor' obj1 mor1 obj2 mor2
  -> Bool
isFull src tgt f =
  all (\(s, t) ->
    let targetMors = [ m | (s', t', m) <- catMorphisms tgt
                         , s' == fObj f s, t' == fObj f t ]
        sourceMors = [ fMor f m | (s', t', m) <- catMorphisms src
                                , s' == s, t' == t ]
    in all (`elem` sourceMors) targetMors
  ) [(s, t) | s <- catObjects src, t <- catObjects src]

-- | Inclusion functor J: C_S -> C (the accessible subcategory into the full category).
inclusionFunctor
  :: Category obj mor
  -> Category obj mor
  -> Functor' obj mor obj mor
inclusionFunctor _sub _full = Functor' id id

-- | Left Kan extension (simplified discrete version).
-- Lan_J(F)(d) = colim_{(c, J(c) -> d)} F(c)
-- For finite categories, this is a union/join of images.
leftKanExtension
  :: (Eq obj, Eq mor, Ord obj)
  => Category obj mor           -- ^ Source category C
  -> Category obj mor           -- ^ Target category D
  -> (obj -> [a])               -- ^ Functor F: C -> Set
  -> obj                        -- ^ Object d in D
  -> [a]
leftKanExtension src tgt f d =
  concatMap (\c ->
    if any (\(s, t, _) -> s == c && t == d) (catMorphisms tgt)
    then f c
    else []
  ) (catObjects src)

-- | Extension deficit: measures the gap between Lan_J(F) and the total functor.
extensionDeficit
  :: (Eq obj, Eq mor, Eq a, Ord obj)
  => Category obj mor           -- ^ Subcategory (observer's accessible part)
  -> Category obj mor           -- ^ Full category
  -> (obj -> [a])               -- ^ Observer's functor
  -> (obj -> [a])               -- ^ Total functor (ground truth)
  -> obj                        -- ^ Object to evaluate at
  -> [a]                        -- ^ Elements in the deficit
extensionDeficit sub full fLocal fTotal d =
  let lanResult = leftKanExtension sub full fLocal d
      totalResult = fTotal d
  in filter (`notElem` lanResult) totalResult
