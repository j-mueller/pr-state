{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
module PrState (runMain) where

import           Control.Concurrent.ParallelIO.Global (parallel)
import           Control.Monad.Except                 (liftEither, runExceptT)
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Reader                 (runReaderT)
import           Data.Set                             (Set)
import qualified Data.Set                             as Set
import           Data.String                          (IsString (..))
import           GitHub                               (Owner, Repo,
                                                       SimplePullRequest (..),
                                                       SimpleUser (..))
import qualified GitHub
import           GitHub.Data.Name                     (Name (..))
import           Prettyprinter                        (Pretty (..),
                                                       defaultLayoutOptions,
                                                       layoutPretty, pretty)
import           Prettyprinter.Render.String          (renderString)
import qualified PrState.API                          as API
import           PrState.Report                       (mkPrReport, repoPRReport)
import           System.Environment                   (getArgs)
import           System.Exit                          (exitFailure)

loadArgs :: IO (Name Owner, Name Repo, Set (Name GitHub.User))
loadArgs = getArgs >>= \case
  owner:repo:users -> pure (fromString owner, fromString repo, Set.fromList $ fmap fromString users)
  _ -> do
    putStrLn "usage: pr-state <owner> <repo> (user)*"
    exitFailure

runMain :: IO ()
runMain = do
  (owner, repo, allowedUsers) <- loadArgs
  env <- API.gitHubEnv owner repo
  result <- runExceptT $ flip runReaderT env $ do
    prs <- API.listOpenPRs
    let isRelevant SimplePullRequest{simplePullRequestUser=SimpleUser{simpleUserLogin}} = Set.null allowedUsers || simpleUserLogin `Set.member` allowedUsers
    reports <- liftIO
      $ parallel
      $ runExceptT . flip runReaderT env . mkPrReport . simplePullRequestNumber
      <$> filter isRelevant prs
    traverse liftEither reports >>=
      liftIO . putStrLn . render . foldMap repoPRReport
  case result of
    Left err -> do
      print err
      exitFailure
    _ -> pure ()

render :: Pretty a => a -> String
render = renderString . layoutPretty defaultLayoutOptions . pretty
