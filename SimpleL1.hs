{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as M
import System.IO (hFlush, stdout)
import System.Process (readProcess)
import Data.Char (toUpper)

-- ============================================================
-- CONFIG
-- ============================================================

epochSize :: Int
epochSize = 10

baseProposalDeposit :: Int
baseProposalDeposit = 100

faucetAmount :: Int
faucetAmount = 1000

burnFailurePct :: Int
burnFailurePct = 50  -- burn 50% of deposit if proposal fails (incl quorum fail)

initialTreasury :: Int
initialTreasury = 100000

-- ============================================================
-- ANSI / UI
-- ============================================================

esc :: String
esc = "\ESC["

reset, red, green, yellow, cyan, magenta, gray, white :: String
reset   = esc ++ "0m"
red     = esc ++ "31m"
green   = esc ++ "32m"
yellow  = esc ++ "33m"
cyan    = esc ++ "36m"
magenta = esc ++ "35m"
gray    = esc ++ "90m"
white   = esc ++ "37m"

paint :: String -> String -> String
paint c s = c ++ s ++ reset

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

-- ============================================================
-- INPUT HELPERS
-- ============================================================

prompt :: String -> IO String
prompt s = putStr s >> hFlush stdout >> getLine

promptInt :: String -> IO Int
promptInt s = do
  v <- prompt s
  case reads v of
    [(n,"")] -> return n
    _ -> putStrLn (paint red "Invalid number") >> promptInt s

promptPct :: String -> IO Int
promptPct s = do
  n <- promptInt s
  if n < 0 || n > 100 then putStrLn (paint red "Enter 0..100") >> promptPct s else return n

pause :: IO ()
pause = do
  _ <- prompt (paint gray "Press Enter...")
  return ()

-- ============================================================
-- HASH (OpenSSL SHA256)
-- ============================================================

hash256 :: String -> IO String
hash256 msg = do
  out <- readProcess
    "bash"
    ["-c","printf '%s' \"$1\" | openssl dgst -sha256","--",msg]
    ""
  return (last (words out))

short :: String -> String
short h = take 10 h ++ "..."

-- ============================================================
-- TYPES
-- ============================================================

type WalletId = Int
type ProposalId = String
type SlotNo = Int
type Epoch = Int

data Vote = YES | NO deriving (Eq, Show)

data VoteMode = OnePersonOneVote | StakeWeighted deriving (Eq, Show)

data Category = Funding | Policy | Info deriving (Eq, Show)

data ProposalStatus = POpen | PClosed | PFinalized deriving (Eq, Show)

-- Commit–Reveal
type Commitment = String

data Proposal = Proposal
  { pId            :: ProposalId
  , pTitle         :: String
  , pDesc          :: String
  , pCategory      :: Category
  , pMode          :: VoteMode
  , pProposer      :: WalletId
  , pDeposit       :: Int

  , pStart         :: SlotNo
  , pEnd           :: SlotNo
  , pRevealEnd     :: SlotNo            -- reveals allowed until this slot (inclusive)

  , pQuorumPct     :: Int               -- 0..100
  , pEligSnap      :: Int               -- eligible wallets snapshot (for 1p1v)
  , pStakeSnap     :: Int               -- total staked snapshot (for stake mode)

  , pFundingTarget :: Maybe (WalletId, Int) -- (recipient, amount) if Funding

  , pStatus        :: ProposalStatus

  -- Commit–Reveal storage
  , pCommits       :: M.Map WalletId (Commitment, Int) -- commitment + weight snapshot
  , pReveals       :: M.Map WalletId Vote              -- revealed vote (validated)

  -- Results (set on finalize)
  , pResult        :: Maybe Bool
  , pYesTotal      :: Int
  , pNoTotal       :: Int
  , pPartTotal     :: Int
  } deriving Show

data Wallet = Wallet
  { wId      :: WalletId
  , wName    :: String
  , wSecret  :: String
  , wBalance :: Int
  , wStake   :: Int
  } deriving Show

-- ============================================================
-- AUDIT LOG (hash chained)
-- ============================================================

data Event
  = EAdvanceSlot SlotNo
  | ECreateWallet WalletId
  | EFaucet WalletId Int
  | EStake WalletId Int
  | ECreateProposal ProposalId WalletId Int Category VoteMode SlotNo SlotNo SlotNo Int
  | ECommitVote ProposalId WalletId Commitment Int
  | ERevealVote ProposalId WalletId Vote Int
  | EClose ProposalId
  | EFinalize ProposalId Bool Int Int Int
  | ETreasuryTransfer ProposalId WalletId Int
  | EDepositRefund ProposalId WalletId Int
  | EDepositBurn ProposalId Int
  deriving (Eq, Show)

data LogEntry = LogEntry
  { leIx      :: Int
  , leSlot    :: SlotNo
  , leEvent   :: Event
  , lePrev    :: String
  , leHash    :: String
  } deriving Show

-- ============================================================
-- CHAIN
-- ============================================================

data Chain = Chain
  { csSlot       :: SlotNo
  , csNextWid    :: WalletId
  , csWallets    :: M.Map WalletId Wallet
  , csProposals  :: M.Map ProposalId Proposal

  -- economics
  , csPropCount  :: M.Map (WalletId, Epoch) Int
  , csTreasury   :: Int
  , csBurned     :: Int

  -- audit
  , csEvents     :: [LogEntry]
  , csNextEIx    :: Int
  , csLastHash   :: String
  }

initChain :: Chain
initChain =
  Chain 0 1 M.empty M.empty M.empty initialTreasury 0 [] 0 (replicate 64 '0')

-- ============================================================
-- GOVERNANCE / ECONOMICS
-- ============================================================

currentEpoch :: Chain -> Epoch
currentEpoch st = csSlot st `div` epochSize

totalStake :: Chain -> Int
totalStake st = sum (map wStake (M.elems (csWallets st)))

proposalCount :: WalletId -> Chain -> Int
proposalCount wid st =
  M.findWithDefault 0 (wid, currentEpoch st) (csPropCount st)

proposalCost :: WalletId -> Chain -> Int
proposalCost wid st =
  baseProposalDeposit * (2 ^ proposalCount wid st)

-- ============================================================
-- AUDIT HELPERS
-- ============================================================

appendEvent :: Chain -> Event -> IO Chain
appendEvent st ev = do
  let ix   = csNextEIx st
      slot = csSlot st
      prev = csLastHash st
      payload = show ix ++ "|" ++ show slot ++ "|" ++ show ev ++ "|" ++ prev
  h <- hash256 payload
  let entry = LogEntry ix slot ev prev h
  return st
    { csEvents   = csEvents st ++ [entry]
    , csNextEIx  = ix + 1
    , csLastHash = h
    }

verifyAudit :: Chain -> IO Bool
verifyAudit st = go (replicate 64 '0') (csEvents st)
  where
    go _ [] = return True
    go expectedPrev (e:es) = do
      let payload = show (leIx e) ++ "|" ++ show (leSlot e) ++ "|" ++ show (leEvent e) ++ "|" ++ expectedPrev
      h <- hash256 payload
      if lePrev e /= expectedPrev then return False
      else if leHash e /= h then return False
      else go (leHash e) es

-- ============================================================
-- QUORUM + TALLY (counts REVEALS)
-- ============================================================

ceilingDiv :: Int -> Int -> Int
ceilingDiv a b = (a + b - 1) `div` b

requiredParticipation :: Proposal -> Int
requiredParticipation p =
  case pMode p of
    OnePersonOneVote -> ceilingDiv (pQuorumPct p * pEligSnap p) 100
    StakeWeighted    -> ceilingDiv (pQuorumPct p * pStakeSnap p) 100

computeTally :: Proposal -> (Int, Int, Int)
computeTally p =
  case pMode p of
    OnePersonOneVote ->
      let vs = M.elems (pReveals p)
          yes = length [() | YES <- vs]
          no  = length [() | NO  <- vs]
          part = length vs
      in (yes, no, part)
    StakeWeighted ->
      let -- weight snapshot comes from commits (validated reveals must have a commit)
          yes = sum [ w | (wid, YES) <- M.toList (pReveals p)
                        , Just (_, w) <- [M.lookup wid (pCommits p)] ]
          no  = sum [ w | (wid, NO)  <- M.toList (pReveals p)
                        , Just (_, w) <- [M.lookup wid (pCommits p)] ]
          part = sum [ w | (wid, _) <- M.toList (pReveals p)
                         , Just (_, w) <- [M.lookup wid (pCommits p)] ]
      in (yes, no, part)

-- ============================================================
-- COMMITMENT (simple tutorial version)
-- commitment = SHA256(proposalId|walletId|VOTE|salt)
-- ============================================================

mkCommitment :: ProposalId -> WalletId -> Vote -> String -> IO Commitment
mkCommitment pid wid v salt =
  hash256 (pid ++ "|" ++ show wid ++ "|" ++ show v ++ "|" ++ salt)

-- ============================================================
-- UI RENDER
-- ============================================================

renderUI :: Chain -> IO ()
renderUI st = do
  clearScreen
  putStrLn (paint cyan "======================================================================")
  putStrLn (paint magenta "   GOVERNANCE L1  (Commit–Reveal + Quorum + Stake + Audit + Treasury)")
  putStrLn (paint cyan "======================================================================")
  putStrLn $
    paint green (" Slot: " ++ show (csSlot st)) ++
    paint gray  (" | Epoch: " ++ show (currentEpoch st)) ++
    paint yellow (" | Wallets: " ++ show (M.size (csWallets st))) ++
    paint yellow (" | Proposals: " ++ show (M.size (csProposals st))) ++
    paint cyan (" | Treasury: " ++ show (csTreasury st)) ++
    paint gray (" | Burned: " ++ show (csBurned st)) ++
    paint gray (" | Ledger: " ++ short (csLastHash st))
  putStrLn (paint cyan "----------------------------------------------------------------------")
  putStrLn (paint white " 1 Advance Slot      6 List Proposals")
  putStrLn (paint white " 2 Create Wallet     7 Commit Vote (select)")
  putStrLn (paint white " 3 Faucet Wallet     8 Reveal Vote (select)")
  putStrLn (paint white " 4 List Wallets      9 Close Proposal (select)")
  putStrLn (paint white " 5 Stake Tokens     10 Finalize Proposal (select)")
  putStrLn (paint white "12 Create Proposal  13 View Audit Log")
  putStrLn (paint white "14 Verify Audit Chain")
  putStrLn (paint red   " 0 Exit")
  putStrLn (paint cyan "======================================================================")

printWallets :: Chain -> IO ()
printWallets st = do
  clearScreen
  putStrLn (paint cyan "======================================================================")
  putStrLn (paint magenta "  WALLETS")
  putStrLn (paint cyan "======================================================================")
  if M.null (csWallets st)
    then putStrLn (paint gray "  (none)")
    else mapM_ pp (M.elems (csWallets st))
  putStrLn (paint cyan "======================================================================")
  where
    pp w =
      putStrLn $
        paint white ("  #" ++ show (wId w) ++ "  " ++ wName w) ++
        paint gray (" | bal=" ++ show (wBalance w) ++ " GOV") ++
        paint yellow (" | stake=" ++ show (wStake w))

printProposals :: Chain -> IO ()
printProposals st = do
  clearScreen
  putStrLn (paint cyan "======================================================================")
  putStrLn (paint magenta "  PROPOSALS")
  putStrLn (paint cyan "======================================================================")
  if M.null (csProposals st)
    then putStrLn (paint gray "  (none)")
    else mapM_ pp (M.elems (csProposals st))
  putStrLn (paint cyan "======================================================================")
  where
    statusCol POpen      = green
    statusCol PClosed    = yellow
    statusCol PFinalized = gray

    modeTxt OnePersonOneVote = "1p1v"
    modeTxt StakeWeighted    = "stake"

    catTxt Funding = "Funding"
    catTxt Policy  = "Policy"
    catTxt Info    = "Info"

    pp p = do
      let col = statusCol (pStatus p)
          win = show (pStart p) ++ "→" ++ show (pEnd p)
          rwin = show (pEnd p + 1) ++ "→" ++ show (pRevealEnd p)
          q = "quorum=" ++ show (pQuorumPct p) ++ "%"
          commitsN = M.size (pCommits p)
          revealsN = M.size (pReveals p)
          extra =
            case pFundingTarget p of
              Nothing -> ""
              Just (to, amt) -> " -> #" ++ show to ++ " amt=" ++ show amt
          finalTxt =
            case pStatus p of
              PFinalized ->
                " YES=" ++ show (pYesTotal p) ++ " NO=" ++ show (pNoTotal p)
                ++ " part=" ++ show (pPartTotal p)
                ++ " result=" ++ show (pResult p)
              _ -> ""
      putStrLn $
        paint white ("  " ++ short (pId p)) ++ " " ++
        paint col ("[" ++ show (pStatus p) ++ "] ") ++
        paint cyan (pTitle p) ++
        paint gray ("  cat=" ++ catTxt (pCategory p)
                   ++ " mode=" ++ modeTxt (pMode p)
                   ++ " " ++ q
                   ++ " vote=" ++ win
                   ++ " reveal=" ++ rwin
                   ++ " dep=" ++ show (pDeposit p)
                   ++ " prop=#" ++ show (pProposer p)
                   ++ extra
                   ++ " commits=" ++ show commitsN
                   ++ " reveals=" ++ show revealsN
                   ++ finalTxt)

-- ============================================================
-- PROPOSAL SELECTION
-- ============================================================

data SelectFilter
  = CommitableNow
  | RevealableNow
  | AnyOpen
  | FinalizableNow

selectProposal :: Chain -> SelectFilter -> IO (Maybe Proposal)
selectProposal st filt = do
  let psAll = M.elems (csProposals st)
      ps = case filt of
             CommitableNow -> filter isCommitable psAll
             RevealableNow -> filter isRevealable psAll
             AnyOpen       -> filter (\p -> pStatus p == POpen) psAll
             FinalizableNow-> filter isFinalizable psAll

  if null ps
    then do
      case filt of
        CommitableNow  -> putStrLn (paint yellow "No proposals currently accepting COMMIT votes.")
        RevealableNow  -> putStrLn (paint yellow "No proposals currently accepting REVEALS.")
        AnyOpen        -> putStrLn (paint gray "No open proposals.")
        FinalizableNow -> putStrLn (paint gray "No proposals ready to finalize (must be closed and reveal window ended).")
      explainWhy st filt
      pause
      return Nothing
    else do
      putStrLn (paint cyan "------------------------------------------------------------------")
      mapM_ (ppLine st filt) (zip [1..] ps)
      putStrLn (paint cyan "------------------------------------------------------------------")
      n <- promptInt "Select proposal number: "
      if n < 1 || n > length ps
        then putStrLn (paint red "Invalid selection.") >> pause >> return Nothing
        else return (Just (ps !! (n - 1)))

  where
    isCommitable p =
      pStatus p == POpen && csSlot st >= pStart p && csSlot st <= pEnd p

    isRevealable p =
      pStatus p == PClosed && csSlot st >= (pEnd p + 1) && csSlot st <= pRevealEnd p

    isFinalizable p =
      pStatus p == PClosed && csSlot st > pRevealEnd p

ppLine :: Chain -> SelectFilter -> (Int, Proposal) -> IO ()
ppLine st filt (i, p) = do
  let voteWin = show (pStart p) ++ "→" ++ show (pEnd p)
      revWin  = show (pEnd p + 1) ++ "→" ++ show (pRevealEnd p)
      tag =
        case filt of
          CommitableNow  -> if csSlot st >= pStart p && csSlot st <= pEnd p then paint green "COMMIT NOW" else paint gray "not commitable"
          RevealableNow  -> if csSlot st >= (pEnd p + 1) && csSlot st <= pRevealEnd p then paint green "REVEAL NOW" else paint gray "not revealable"
          FinalizableNow -> if csSlot st > pRevealEnd p then paint green "FINALIZE" else paint gray "not finalizable"
          AnyOpen        -> paint gray "open"
  putStrLn $
    paint white ("[" ++ show i ++ "] ") ++
    paint cyan (pTitle p) ++
    paint gray (" vote=" ++ voteWin ++ " reveal=" ++ revWin) ++
    paint gray (" commits=" ++ show (M.size (pCommits p)) ++ " reveals=" ++ show (M.size (pReveals p))) ++
    "  " ++ tag

explainWhy :: Chain -> SelectFilter -> IO ()
explainWhy st filt = do
  let ps = M.elems (csProposals st)
      openPs = filter (\p -> pStatus p == POpen) ps
      closedPs = filter (\p -> pStatus p == PClosed) ps
  case filt of
    CommitableNow ->
      if null openPs then putStrLn (paint gray "Reason: no open proposals.")
      else do
        putStrLn (paint gray "Open proposals exist, but current slot may be outside vote window:")
        mapM_ (\p -> putStrLn (paint gray (" - " ++ pTitle p ++ ": vote " ++ show (pStart p) ++ "→" ++ show (pEnd p) ++ ", current " ++ show (csSlot st)))) openPs
    RevealableNow ->
      if null closedPs then putStrLn (paint gray "Reason: no closed proposals (close one first).")
      else do
        putStrLn (paint gray "Closed proposals exist, but current slot may be outside reveal window:")
        mapM_ (\p -> putStrLn (paint gray (" - " ++ pTitle p ++ ": reveal " ++ show (pEnd p + 1) ++ "→" ++ show (pRevealEnd p) ++ ", current " ++ show (csSlot st)))) closedPs
    FinalizableNow ->
      if null closedPs then putStrLn (paint gray "Reason: no closed proposals.")
      else do
        putStrLn (paint gray "Closed proposals exist, but reveal window may not be finished:")
        mapM_ (\p -> putStrLn (paint gray (" - " ++ pTitle p ++ ": finalize after slot " ++ show (pRevealEnd p) ++ ", current " ++ show (csSlot st)))) closedPs
    AnyOpen -> return ()

-- ============================================================
-- MAIN
-- ============================================================

main :: IO ()
main = do
  st1 <- appendEvent initChain (EAdvanceSlot 0)
  loop st1

loop :: Chain -> IO ()
loop st = do
  renderUI st
  c <- prompt "> "
  case c of

    "1" -> do
      let st' = st { csSlot = csSlot st + 1 }
      st'' <- appendEvent st' (EAdvanceSlot (csSlot st'))
      loop st''

    "2" -> do
      name <- prompt "Wallet name: "
      sec  <- prompt "Wallet secret (keep private): "
      let wid = csNextWid st
          w = Wallet wid name sec 0 0
          st' = st { csNextWid = wid + 1, csWallets = M.insert wid w (csWallets st) }
      st'' <- appendEvent st' (ECreateWallet wid)
      loop st''

    "3" -> do
      wid <- promptInt "Wallet id to faucet: "
      case M.lookup wid (csWallets st) of
        Nothing -> putStrLn (paint red "Wallet not found.") >> pause >> loop st
        Just w -> do
          let w' = w { wBalance = wBalance w + faucetAmount }
              st' = st { csWallets = M.insert wid w' (csWallets st) }
          st'' <- appendEvent st' (EFaucet wid faucetAmount)
          loop st''

    "4" -> printWallets st >> pause >> loop st

    "5" -> do
      wid <- promptInt "Wallet id to stake from: "
      amt <- promptInt "Amount to stake: "
      case M.lookup wid (csWallets st) of
        Nothing -> putStrLn (paint red "Wallet not found.") >> pause >> loop st
        Just w ->
          if amt <= 0 then putStrLn (paint red "Amount must be > 0.") >> pause >> loop st
          else if wBalance w < amt then putStrLn (paint red "Insufficient balance.") >> pause >> loop st
          else do
            let w' = w { wBalance = wBalance w - amt, wStake = wStake w + amt }
                st' = st { csWallets = M.insert wid w' (csWallets st) }
            st'' <- appendEvent st' (EStake wid amt)
            loop st''

    "6" -> printProposals st >> pause >> loop st

    -- 7 COMMIT vote
    "7" -> do
      mp <- selectProposal st CommitableNow
      case mp of
        Nothing -> loop st
        Just p -> do
          wid <- promptInt "Wallet id committing: "
          case M.lookup wid (csWallets st) of
            Nothing -> putStrLn (paint red "Wallet not found.") >> pause >> loop st
            Just w -> do
              if M.member wid (pCommits p)
                then putStrLn (paint red "Already committed for this proposal.") >> pause >> loop st
                else do
                  vStr <- fmap (map toUpper) (prompt "Commit vote YES or NO (hidden later): ")
                  let mv = if vStr == "YES" then Just YES else if vStr == "NO" then Just NO else Nothing
                  case mv of
                    Nothing -> putStrLn (paint red "Invalid vote.") >> pause >> loop st
                    Just v -> do
                      salt <- prompt "Salt (any secret string, keep it to reveal later): "
                      let weight =
                            case pMode p of
                              OnePersonOneVote -> 1
                              StakeWeighted    -> wStake w
                      if weight <= 0
                        then putStrLn (paint red "No voting power (stake is 0).") >> pause >> loop st
                        else do
                          com <- mkCommitment (pId p) wid v salt
                          let p' = p { pCommits = M.insert wid (com, weight) (pCommits p) }
                              st' = st { csProposals = M.insert (pId p) p' (csProposals st) }
                          st'' <- appendEvent st' (ECommitVote (pId p) wid com weight)
                          putStrLn (paint green ("Committed! commitment=" ++ short com))
                          pause
                          loop st''

    -- 8 REVEAL vote
    "8" -> do
      mp <- selectProposal st RevealableNow
      case mp of
        Nothing -> loop st
        Just p -> do
          wid <- promptInt "Wallet id revealing: "
          case M.lookup wid (pCommits p) of
            Nothing -> putStrLn (paint red "No commitment found for this wallet.") >> pause >> loop st
            Just (storedCom, weight) -> do
              if M.member wid (pReveals p)
                then putStrLn (paint red "Already revealed for this proposal.") >> pause >> loop st
                else do
                  vStr <- fmap (map toUpper) (prompt "Reveal vote YES or NO: ")
                  let mv = if vStr == "YES" then Just YES else if vStr == "NO" then Just NO else Nothing
                  case mv of
                    Nothing -> putStrLn (paint red "Invalid vote.") >> pause >> loop st
                    Just v -> do
                      salt <- prompt "Salt used in commit: "
                      com <- mkCommitment (pId p) wid v salt
                      if com /= storedCom
                        then putStrLn (paint red "Reveal does not match commitment (invalid).") >> pause >> loop st
                        else do
                          let p' = p { pReveals = M.insert wid v (pReveals p) }
                              st' = st { csProposals = M.insert (pId p) p' (csProposals st) }
                          st'' <- appendEvent st' (ERevealVote (pId p) wid v weight)
                          putStrLn (paint green "Reveal accepted ✅")
                          pause
                          loop st''

    -- 9 close proposal (ends commits; enables reveal phase)
    "9" -> do
      mp <- selectProposal st AnyOpen
      case mp of
        Nothing -> loop st
        Just p -> do
          let p' = p { pStatus = PClosed }
              st' = st { csProposals = M.insert (pId p) p' (csProposals st) }
          st'' <- appendEvent st' (EClose (pId p))
          putStrLn (paint yellow "Proposal closed. Reveal phase begins next slot.")
          pause
          loop st''

    -- 10 finalize (only after reveal window ends)
    "10" -> do
      mp <- selectProposal st FinalizableNow
      case mp of
        Nothing -> loop st
        Just p -> do
          let (yesT, noT, partT) = computeTally p
              req = requiredParticipation p
              quorumMet = partT >= req
              passed = quorumMet && yesT > noT

              proposerId = pProposer p
              proposer = csWallets st M.! proposerId

              burnAmt = if passed then 0 else (pDeposit p * burnFailurePct) `div` 100
              refundAmt = pDeposit p - burnAmt
              proposer' = proposer { wBalance = wBalance proposer + refundAmt }

              stA = st { csWallets = M.insert proposerId proposer' (csWallets st)
                       , csBurned  = csBurned st + burnAmt }

          stB <- appendEvent stA (EFinalize (pId p) passed yesT noT partT)
          stC <- if refundAmt > 0 then appendEvent stB (EDepositRefund (pId p) proposerId refundAmt) else return stB
          stD <- if burnAmt > 0 then appendEvent stC (EDepositBurn (pId p) burnAmt) else return stC

          stE <- case (passed, pCategory p, pFundingTarget p) of
                   (True, Funding, Just (toWid, amt)) ->
                     if csTreasury stD >= amt
                       then case M.lookup toWid (csWallets stD) of
                              Nothing -> return stD
                              Just recv -> do
                                let recv' = recv { wBalance = wBalance recv + amt }
                                    stX = stD { csTreasury = csTreasury stD - amt
                                              , csWallets  = M.insert toWid recv' (csWallets stD) }
                                appendEvent stX (ETreasuryTransfer (pId p) toWid amt)
                       else return stD
                   _ -> return stD

          let p' = p { pStatus = PFinalized
                     , pResult = Just passed
                     , pYesTotal = yesT
                     , pNoTotal = noT
                     , pPartTotal = partT
                     }
              stF = stE { csProposals = M.insert (pId p) p' (csProposals stE) }

          clearScreen
          putStrLn (paint cyan "======================================================================")
          putStrLn (paint magenta "  FINALIZE RESULT (Commit–Reveal)")
          putStrLn (paint cyan "======================================================================")
          putStrLn (paint white ("Proposal: " ++ pTitle p))
          putStrLn (paint gray ("Mode: " ++ show (pMode p) ++ " | Category: " ++ show (pCategory p)))
          putStrLn (paint gray ("Quorum required: " ++ show req ++ " | Participation: " ++ show partT ++ " | Met: " ++ show quorumMet))
          putStrLn (paint gray ("YES: " ++ show yesT ++ " | NO: " ++ show noT))
          putStrLn (paint (if passed then green else red) ("PASSED? " ++ show passed))
          putStrLn (paint gray ("Deposit refund: " ++ show refundAmt ++ " | Burned: " ++ show burnAmt))
          putStrLn (paint cyan "======================================================================")
          pause
          loop stF

    "12" -> do
      wid <- promptInt "Proposer wallet id: "
      case M.lookup wid (csWallets st) of
        Nothing -> putStrLn (paint red "Wallet not found.") >> pause >> loop st
        Just w -> do
          let cost = proposalCost wid st
          putStrLn (paint gray ("Next proposal deposit (epoch " ++ show (currentEpoch st) ++ "): " ++ show cost))
          if wBalance w < cost
            then putStrLn (paint red "Insufficient balance for deposit.") >> pause >> loop st
            else do
              t <- prompt "Title: "
              d <- prompt "Description: "

              putStrLn (paint cyan "Category: 1=Funding  2=Policy  3=Info")
              catN <- promptInt "Choose: "
              let cat = case catN of
                          1 -> Funding
                          2 -> Policy
                          _ -> Info

              putStrLn (paint cyan "Vote mode: 1=1p1v  2=stake-weighted")
              modeN <- promptInt "Choose: "
              let mode = if modeN == 2 then StakeWeighted else OnePersonOneVote

              qpct <- promptPct "Quorum % (0..100): "
              s <- promptInt "Vote start slot: "
              e <- promptInt "Vote end slot: "
              rEnd <- promptInt "Reveal end slot (must be >= end+1): "

              let rMin = e + 1
              if rEnd < rMin
                then putStrLn (paint red ("Reveal end must be >= " ++ show rMin)) >> pause >> loop st
                else do
                  fTarget <- case cat of
                               Funding -> do
                                 toWid <- promptInt "Funding recipient wallet id: "
                                 amt  <- promptInt "Funding amount from TREASURY: "
                                 return (Just (toWid, amt))
                               _ -> return Nothing

                  pid <- hash256 (show wid ++ "|" ++ t ++ "|" ++ show (csSlot st))

                  let eligSnap = M.size (csWallets st)
                      stakeSnap = totalStake st

                      p = Proposal
                            pid t d cat mode wid cost
                            s e rEnd
                            qpct eligSnap stakeSnap
                            fTarget
                            POpen
                            M.empty M.empty
                            Nothing 0 0 0

                      w' = w { wBalance = wBalance w - cost }
                      key = (wid, currentEpoch st)
                      newCount = proposalCount wid st + 1

                      st' = st { csWallets   = M.insert wid w' (csWallets st)
                               , csProposals = M.insert pid p (csProposals st)
                               , csPropCount = M.insert key newCount (csPropCount st) }

                  st'' <- appendEvent st' (ECreateProposal pid wid cost cat mode s e rEnd qpct)
                  loop st''

    "13" -> do
      clearScreen
      putStrLn (paint cyan "======================================================================")
      putStrLn (paint magenta "  AUDIT LOG (Hash-Chained)")
      putStrLn (paint cyan "======================================================================")
      if null (csEvents st) then putStrLn (paint gray "  (no events)") else mapM_ pp (csEvents st)
      putStrLn (paint cyan "======================================================================")
      pause
      loop st
      where
        pp e =
          putStrLn $
            paint white (" #" ++ show (leIx e)) ++
            paint gray (" slot=" ++ show (leSlot e)) ++
            paint cyan (" " ++ short (leHash e)) ++
            paint gray (" prev=" ++ short (lePrev e)) ++
            paint white (" " ++ show (leEvent e))

    "14" -> do
      ok <- verifyAudit st
      clearScreen
      putStrLn (paint cyan "======================================================================")
      putStrLn (paint magenta "  AUDIT VERIFY")
      putStrLn (paint cyan "======================================================================")
      putStrLn (if ok then paint green "Audit hash-chain: VALID ✅" else paint red "Audit hash-chain: INVALID ❌")
      putStrLn (paint gray ("Events: " ++ show (length (csEvents st)) ++ " | Head: " ++ short (csLastHash st)))
      putStrLn (paint cyan "======================================================================")
      pause
      loop st

    "0" -> putStrLn "Bye."
    _   -> loop st
