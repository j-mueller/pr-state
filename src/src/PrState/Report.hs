{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
module PrState.Report(
  PRReport(..),
  PRStatus(..),
  mkPrReport,
  repoPRReport
) where

import           Data.Foldable    (toList)
import           Data.Map         (Map)
import qualified Data.Map         as Map
import qualified Data.Set         as Set
import           Data.Text        (Text)
import           GitHub           (IssueNumber (..), MergeableState (..),
                                   PullRequest (..), SimpleUser (..))
import qualified GitHub
import           GitHub.Data.Name (Name (..))
import           Prettyprinter    (Doc, Pretty (..), colon, hang, parens,
                                   pretty, vsep, (<+>))
import qualified PrState.API      as API

{-| Status of a PR
-}
data PRStatus =
  StatusDraft
  | StatusNeedsReviewers -- ^ No reviewers assigned, ready for review
  | WaitingForReview SimpleUser [SimpleUser]
  | Rework -- ^ Changes requested, unresolved changes
  | Approved -- ^ Ready to merge when formal requirements are met (might need rebasing etc.)
  | Closed -- ^ Closed or merged
  deriving stock Show

instance Pretty PRStatus where
  pretty = \case
    StatusDraft -> "Draft"
    StatusNeedsReviewers -> "Needs reviewers"
    WaitingForReview author _reviewers -> "Awaiting review. Authored by" <+> prettyUser author
    Rework -> "Needs rework"
    Approved -> "Approved"
    Closed -> "Closed"

pullRequestStatus :: API.GithubApi m => PullRequest -> m PRStatus
pullRequestStatus PullRequest{pullRequestMergeableState, pullRequestRequestedReviewers, pullRequestNumber, pullRequestUser} = case pullRequestMergeableState of
  StateDraft -> pure StatusDraft
  _ -> case toList pullRequestRequestedReviewers of
        []         -> pure StatusNeedsReviewers
        reviewers -> do
          reviews <- API.loadReviews pullRequestNumber
          let isChangesRequested = \case
                GitHub.ReviewStateChangesRequested -> True
                _ -> False
              reqChanges = filter (isChangesRequested . GitHub.reviewState) reviews
          case reqChanges of
            [] -> pure $ WaitingForReview pullRequestUser reviewers
            _  -> pure Rework

prettyUser :: SimpleUser -> Doc ann
prettyUser SimpleUser{simpleUserLogin = N n} = pretty n

data PRReport =
  PRReport
    { prAuthor       :: !SimpleUser
    , prIssue        :: !IssueNumber
    , prTitle        :: !Text
    , prStatus       :: !PRStatus
    , prChangedFiles :: !Int
    }
    deriving stock Show

responsibleUser :: PRReport -> [SimpleUser]
responsibleUser PRReport{prAuthor, prStatus} = case prStatus of
  WaitingForReview _ users -> users
  _                        -> [prAuthor]

mkPrReport :: API.GithubApi m => IssueNumber -> m PRReport
mkPrReport prIssue = do
  pr@PullRequest{pullRequestUser, pullRequestTitle, pullRequestChangedFiles} <- API.loadPR prIssue
  prStatus <- pullRequestStatus pr
  pure $
    PRReport
      { prAuthor = pullRequestUser
      , prIssue
      , prTitle  = pullRequestTitle
      , prStatus
      , prChangedFiles = pullRequestChangedFiles
      }

instance Pretty PRReport where
  pretty PRReport{prIssue=IssueNumber n, prTitle, prStatus, prChangedFiles} =
    hang 2 $ vsep [ "PR" <> pretty n <+> "-" <+> pretty prTitle <+> parens (pretty prChangedFiles <+> "files")
                  , pretty prStatus]

data RepoPRReport =
  RepoPRReport
    { rprOpenPrs   :: !Int -- ^ Total number of open PRs
    , prTaskByUser :: Map SimpleUser [PRReport]
    }
  deriving stock Show

instance Pretty RepoPRReport where
  pretty RepoPRReport{rprOpenPrs, prTaskByUser} =
    let userSection (user, reports) =
          hang 2 $ vsep (prettyUser user <> colon : fmap pretty reports)
    in vsep (pretty rprOpenPrs <+> "open PRs" : fmap userSection (Map.toList prTaskByUser))

instance Semigroup RepoPRReport where
  l <> r =
    RepoPRReport
      { rprOpenPrs = rprOpenPrs l + rprOpenPrs r
      , prTaskByUser = Map.unionWith (<>) (prTaskByUser l) (prTaskByUser r)
      }

instance Monoid RepoPRReport where
  mempty =
    RepoPRReport
      { rprOpenPrs = 0
      , prTaskByUser = Map.empty
      }

{-| Generate a PR report from a list of PRs
-}
repoPRReport :: PRReport -> RepoPRReport
repoPRReport rep = do
  RepoPRReport
    { rprOpenPrs = 1
    , prTaskByUser = Map.fromSet (const [rep]) (Set.fromList $ responsibleUser rep)
    }
