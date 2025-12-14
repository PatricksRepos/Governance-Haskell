{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as M
import System.IO
import System.Process (readProcess)
import Data.Char (toUpper)
import Data.List (isPrefixOf)
import Text.Printf (printf)

-- ============================================================
-- CONFIG
-- ============================================================

epochSize :: Int
epochSize = 10

minVoteWindow :: Int
minVoteWindow = 3

minRevealWindow :: Int
minRevealWindow = 2

defaultQuorum :: Int
defaultQuorum = 2

baseDeposit :: Int
baseDeposit = 100

faucetAmt :: Int
faucetAmt = 1000

initialTreasury :: Int
initialTreasury = 100000

-- ============================================================
-- ANSI / ASCII
-- ============================================================

esc = "\ESC["
reset = esc ++ "0m"
cyan  = esc ++ "36m"
green = esc ++ "32m"
red   = esc ++ "31m"
yellow = esc ++ "33m"
gray  = esc ++ "90m"
white = esc ++ "37m"

paint c s = c ++ s ++ reset
clear = putStr "\ESC[2J\ESC[H"

-- ============================================================
-- HASH
-- ============================================================

hash256 :: String -> IO String
hash256 s = do
  o <- readProcess "bash"
        ["-c","printf '%s' \"$1\" | openssl dgst -sha256","--",s] ""
  return (last (words o))

short :: String -> String
short h = take 10 h ++ "..."

-- ============================================================
-- TYPES
-- ============================================================

type WalletId   = Int
type ProposalId = String
type Slot       = Int
type Epoch      = Int

data Vote = YES | NO deriving (Eq,Show)
data Status = POpen | PClosed | PFinalized deriving (Eq,Show)
data Category = Funding | Policy | Info deriving (Eq,Show)
data VoteMode = OnePersonOneVote | StakeWeighted deriving (Eq,Show)
data Visibility = Public | Private deriving (Eq,Show)

data Wallet = Wallet
  { wId     :: WalletId
  , wName   :: String
  , wSecret :: String
  , wBal    :: Int
  , wStake  :: Int
  , wLastProposalEpoch :: Epoch
  } deriving Show

data Proposal = Proposal
  { pId        :: ProposalId
  , pTitle     :: String
  , pCategory  :: Category
  , pMode      :: VoteMode
  , pVis       :: Visibility
  , pDeposit   :: Int
  , pProposer  :: WalletId
  , pStart     :: Slot
  , pEnd       :: Slot
  , pRevealEnd :: Slot
  , pQuorum    :: Int
  , pFunding   :: Maybe Int
  , pStatus    :: Status
  , pCommits   :: M.Map WalletId String
  , pReveals   :: M.Map WalletId Vote
  } deriving Show

data Certification = Certification
  { cPid    :: ProposalId
  , cYes    :: Int
  , cNo     :: Int
  , cPart   :: Int
  , cQuorum :: Int
  , cPassed :: Bool
  , cAudit  :: String
  } deriving Show

data Audit = Audit
  { aMsg  :: String
  , aHash :: String
  } deriving Show

data Chain = Chain
  { csSlot     :: Slot
  , csWallets  :: M.Map WalletId Wallet
  , csProps    :: M.Map ProposalId Proposal
  , csNextWid  :: WalletId
  , csTreasury :: Int
  , csBurned   :: Int
  , csCerts    :: [Certification]
  , csAudit    :: [Audit]
  }

-- ============================================================
-- INIT
-- ============================================================

initChain :: Chain
initChain = Chain
  { csSlot = 0
  , csWallets = M.empty
  , csProps = M.empty
  , csNextWid = 1
  , csTreasury = initialTreasury
  , csBurned = 0
  , csCerts = []
  , csAudit = []
  }

-- ============================================================
-- HELPERS
-- ============================================================

currentEpoch :: Chain -> Epoch
currentEpoch st = csSlot st `div` epochSize

prompt :: String -> IO String
prompt s = putStr s >> hFlush stdout >> getLine

promptInt :: String -> IO Int
promptInt s = do
  v <- prompt s
  case reads v of
    [(n,"")] -> return n
    _ -> promptInt s

commitHash :: ProposalId -> String -> Vote -> String -> IO String
commitHash pid secret v salt =
  hash256 (pid ++ secret ++ show v ++ salt)

appendAudit :: String -> Chain -> IO Chain
appendAudit msg st = do
  let prev = case csAudit st of
               [] -> replicate 64 '0'
               (a:_) -> aHash a
  h <- hash256 (prev ++ msg)
  return st{csAudit = Audit msg h : csAudit st}

-- ============================================================
-- UI
-- ============================================================

ui :: Chain -> IO ()
ui st = do
  clear
  putStrLn (paint cyan "======================================================================")
  putStrLn (paint white " GOVERNANCE L1  (Commit–Reveal • Stake • Treasury • Audit)")
  putStrLn (paint cyan "======================================================================")
  printf " Slot: %d | Epoch: %d | Wallets: %d | Proposals: %d | Treasury: %d | Burned: %d\n"
    (csSlot st) (currentEpoch st) (M.size (csWallets st)) (M.size (csProps st))
    (csTreasury st) (csBurned st)
  putStrLn (paint cyan "----------------------------------------------------------------------")
  putStrLn " 1 Advance Slot        6 List Proposals"
  putStrLn " 2 Create Wallet       7 Commit Vote"
  putStrLn " 3 Faucet Wallet       8 Reveal Vote"
  putStrLn " 4 List Wallets        9 Close Proposal"
  putStrLn " 5 Stake Tokens       10 Finalize Proposal"
  putStrLn "11 Create Proposal    12 View Certifications"
  putStrLn "13 View Audit Log      0 Exit"
  putStrLn (paint cyan "======================================================================")

-- ============================================================
-- MAIN LOOP
-- ============================================================

main :: IO ()
main = loop initChain

loop :: Chain -> IO ()
loop st = do
  ui st
  c <- prompt "> "
  case c of

    "1" -> loop st{csSlot = csSlot st + 1}

    "2" -> do
      name <- prompt "Wallet name: "
      sec  <- prompt "Wallet secret: "
      let w = Wallet (csNextWid st) name sec 0 0 (-1)
      loop st{ csNextWid = csNextWid st + 1
             , csWallets = M.insert (wId w) w (csWallets st) }

    "3" -> do
      wid <- promptInt "Wallet id: "
      case M.lookup wid (csWallets st) of
        Just w -> loop st{csWallets = M.insert wid w{wBal=wBal w+faucetAmt} (csWallets st)}
        _ -> loop st

    "4" -> do
      clear
      mapM_ print (M.elems (csWallets st))
      _ <- prompt "Enter..."
      loop st

    "5" -> do
      wid <- promptInt "Wallet id: "
      amt <- promptInt "Stake amount: "
      case M.lookup wid (csWallets st) of
        Just w | wBal w >= amt ->
          let w' = w{wBal=wBal w-amt, wStake=wStake w+amt}
          in loop st{csWallets = M.insert wid w' (csWallets st)}
        _ -> loop st

    "11" -> do
      wid <- promptInt "Proposer wallet id: "
      title <- prompt "Proposal title: "
      putStrLn "Category: 1 Funding | 2 Policy | 3 Info"
      catN <- promptInt "Choose: "
      putStrLn "Vote mode: 1 1p1v | 2 stake"
      modeN <- promptInt "Choose: "
      putStrLn "Visibility: 1 public | 2 private"
      visN <- promptInt "Choose: "
      fund <- promptInt "Funding amount (0 for none): "
      case M.lookup wid (csWallets st) of
        Just w | wBal w >= baseDeposit
               , wLastProposalEpoch w /= currentEpoch st -> do
            pid <- hash256 (title ++ show (csSlot st))
            let start = csSlot st + 1
                end   = start + minVoteWindow
                rev   = end + minRevealWindow
                dep   = baseDeposit * (1 + length (filter (\p -> pProposer p==wid) (M.elems (csProps st))))
                p = Proposal pid title
                      (if catN==1 then Funding else if catN==2 then Policy else Info)
                      (if modeN==2 then StakeWeighted else OnePersonOneVote)
                      (if visN==2 then Private else Public)
                      dep wid start end rev defaultQuorum
                      (if fund>0 then Just fund else Nothing)
                      POpen M.empty M.empty
                w' = w{wBal=wBal w-dep, wLastProposalEpoch=currentEpoch st}
                st1 = st{ csWallets = M.insert wid w' (csWallets st)
                        , csProps = M.insert pid p (csProps st)
                        }
            st2 <- appendAudit ("Proposal created: "++title) st1
            loop st2
        _ -> loop st

    "6" -> do
      clear
      mapM_ (\p ->
        printf "%s | %-20s | %s | vote %d→%d | reveals=%d\n"
          (short (pId p)) (pTitle p) (show (pStatus p))
          (pStart p) (pEnd p) (M.size (pReveals p))
        ) (M.elems (csProps st))
      _ <- prompt "Enter..."
      loop st

    "7" -> do
      pidp <- prompt "Proposal id prefix: "
      wid  <- promptInt "Wallet id: "
      vStr <- fmap (map toUpper) (prompt "YES or NO: ")
      salt <- prompt "Salt: "
      case (M.lookup wid (csWallets st),
            filter (\(k,_) -> pidp `isPrefixOf` k) (M.toList (csProps st))) of
        (Just w,(pid,p):_) | csSlot st>=pStart p && csSlot st<=pEnd p -> do
          h <- commitHash pid (wSecret w)
                (if vStr=="YES" then YES else NO) salt
          loop st{csProps = M.insert pid p{pCommits=M.insert wid h (pCommits p)} (csProps st)}
        _ -> loop st

    "8" -> do
      pidp <- prompt "Proposal id prefix: "
      wid  <- promptInt "Wallet id: "
      vStr <- fmap (map toUpper) (prompt "YES or NO: ")
      salt <- prompt "Salt: "
      case (M.lookup wid (csWallets st),
            filter (\(k,_) -> pidp `isPrefixOf` k) (M.toList (csProps st))) of
        (Just w,(pid,p):_) | csSlot st>pEnd p && csSlot st<=pRevealEnd p -> do
          h <- commitHash pid (wSecret w)
                (if vStr=="YES" then YES else NO) salt
          if M.lookup wid (pCommits p)==Just h
            then loop st{csProps = M.insert pid p{pReveals=M.insert wid (if vStr=="YES" then YES else NO) (pReveals p)} (csProps st)}
            else loop st
        _ -> loop st

    "9" -> do
      pidp <- prompt "Proposal id prefix: "
      case filter (\(k,_) -> pidp `isPrefixOf` k) (M.toList (csProps st)) of
        ((pid,p):_) -> loop st{csProps=M.insert pid p{pStatus=PClosed} (csProps st)}
        _ -> loop st

    "10" -> do
      pidp <- prompt "Proposal id prefix: "
      case filter (\(k,_) -> pidp `isPrefixOf` k) (M.toList (csProps st)) of
        ((pid,p):_) -> do
          let weights w = if pMode p==StakeWeighted then wStake w else 1
              yes = sum [weights w | (wid,YES)<-M.toList (pReveals p), let Just w=M.lookup wid (csWallets st)]
              no  = sum [weights w | (wid,NO )<-M.toList (pReveals p), let Just w=M.lookup wid (csWallets st)]
              part = yes+no
              passed = part>=pQuorum p && yes>no
              st1 = if passed
                      then st{csTreasury=csTreasury st - maybe 0 id (pFunding p)}
                      else st{csBurned=csBurned st + pDeposit p}
          st2 <- appendAudit ("Finalized: "++pTitle p++" result="++show passed) st1
          let cert = Certification pid yes no part (pQuorum p) passed (aHash (head (csAudit st2)))
          clear
          print cert
          _ <- prompt "Enter..."
          loop st2{ csProps=M.insert pid p{pStatus=PFinalized} (csProps st2)
                  , csCerts=cert:csCerts st2 }
        _ -> loop st

    "12" -> do
      clear
      mapM_ print (csCerts st)
      _ <- prompt "Enter..."
      loop st

    "13" -> do
      clear
      mapM_ print (csAudit st)
      _ <- prompt "Enter..."
      loop st

    "0" -> putStrLn "Exiting."
    _   -> loop st
