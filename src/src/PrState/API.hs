{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-| Conveniences for calling the Github API
-}
module PrState.API(
  GitHubEnv(..),
  GithubApi,
  gitHubEnv,
  listOpenPRs,
  loadPR,
  loadReviews
) where

import           Control.Monad.Except   (MonadError (throwError))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader (ask))
import           Data.Foldable          (toList)
import           Data.String            (IsString (..))
import           GitHub                 (AuthMethod (..), IssueNumber (..),
                                         Name, Owner, PullRequest, Repo,
                                         SimplePullRequest (..), github)
import qualified GitHub
import           System.Environment     (lookupEnv)
import GitHub.Data.Reviews (Review)

instance AuthMethod (Maybe GitHub.Auth) where
  endpoint = maybe (endpoint ()) endpoint
  setAuthRequest = maybe (setAuthRequest ()) setAuthRequest

{-| Github environment for API calls
-}
data GitHubEnv =
  GitHubEnv
    { gheOwner :: Name Owner
    , gheRepo  :: Name Repo
    , gheAuth  :: Maybe GitHub.Auth
    }

{-| Construct a GitHubEnv using the @GITHUB_TOKEN@ variable for authentication (if provided)
-}
gitHubEnv :: Name Owner -> Name Repo -> IO GitHubEnv
gitHubEnv owner repo =
  GitHubEnv owner repo
    <$> fmap (fmap (GitHub.OAuth . fromString)) (lookupEnv "GITHUB_TOKEN")

newtype APIError =
  APIError GitHub.Error
  deriving stock Show

type GithubApi m = (MonadReader GitHubEnv m, MonadError APIError m, MonadIO m)

{-| Get all open pull requests
-}
listOpenPRs :: (MonadReader GitHubEnv m, MonadError APIError m, MonadIO m) => m [SimplePullRequest]
listOpenPRs = do
  GitHubEnv{gheOwner, gheRepo, gheAuth} <- ask
  liftIO (github gheAuth GitHub.pullRequestsForR gheOwner gheRepo GitHub.stateOpen GitHub.FetchAll) >>= \case
    Left err -> throwError (APIError err)
    Right x -> pure (toList x)

{-| Load a PR
-}
loadPR :: (MonadReader GitHubEnv m, MonadError APIError m, MonadIO m) => IssueNumber -> m PullRequest
loadPR issueNumber = do
  GitHubEnv{gheOwner, gheRepo, gheAuth} <- ask
  liftIO (github gheAuth GitHub.pullRequestR gheOwner gheRepo issueNumber) >>= \case
    Left err -> throwError (APIError err)
    Right x -> pure x

{-| Load all reviews of a PR
-}
loadReviews :: (MonadReader GitHubEnv m, MonadError APIError m, MonadIO m) => IssueNumber -> m [Review]
loadReviews issueNumber = do
  GitHubEnv{gheOwner, gheRepo, gheAuth} <- ask
  liftIO (github gheAuth GitHub.pullRequestReviewsR gheOwner gheRepo issueNumber GitHub.FetchAll) >>= \case
    Left err -> throwError (APIError err)
    Right x -> pure (toList x)
