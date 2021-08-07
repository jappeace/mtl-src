{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}

-- I made this blogpost compile: https://blog.cofree.coffee/2021-08-05-a-brief-intro-to-monad-transformers/
-- I claim fair use since I changed it to mtl style.
module Lib
  ( main
  )
where

import qualified Data.Map as M
import qualified Data.HashSet as S
import Control.Monad.Except
import Control.Monad.State
                 --
data AST a = Leaf a | Node (AST a) (AST a)
  deriving (Show, Functor, Foldable, Traversable)

type VariableName = String
type Variables = S.HashSet VariableName

assignIndexToVariables ::
  MonadError String m =>
  MonadState (M.Map VariableName Int) m =>
  AST VariableName -> Variables -> m (AST Int)
assignIndexToVariables ast variables = forM ast $ \var -> do
  unless (var `S.member` variables) $
    throwError $ "Unknown Variable " <> var
  cache <- get
  case M.lookup var cache of
    Just index -> pure index
    Nothing -> do
      let index = M.size cache
      put $ M.insert var index cache
      pure index

main :: IO ()
main =
  let vars = S.fromList ["a", "b", "c"]
      ast = Node (Leaf "a") (Node (Leaf "b") (Node (Leaf "a") (Leaf "c")))
  in do
    print $ flip evalState mempty $ runExceptT $ assignIndexToVariables ast vars
    print $ runExcept $ flip evalStateT mempty $ assignIndexToVariables ast vars
    print (eitherFive, maybeFive)

