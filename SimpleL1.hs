{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as M
import System.IO (hFlush, stdout)
import System.Process (readProcess)
import Data.Char (toUpper)
import Data.List (foldl')

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
burnFailurePct = 50  -- burn 50% of deposit if proposal fails (incl. quorum fail)

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
  if n < 0 || n > 100
    then putStrLn (paint red "Enter 0..100") >> promptPct s
    else return n

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
  , pQuorumPct     :: Int              -- 0..100
  , pEligSnap      :: Int              -- eligible wallets snapshot (for 1p1v)
  , pStakeSnap     :: Int              -- total staked snapshot (for stake mode)
  , pFundingTarget :: Maybe (WalletId, Int) -- (recipient, amount) if Funding
  , pStatus        :: ProposalStatus
  , pVotes         :: M.Map WalletId (Vote, Int) -- stores vote + weight at time of vote
  , pResult        :: Maybe Bool        -- Just True/False when finalized
  , pYesTotal      :: Int
  , pNoTotal       :: Int
  , pPartTotal     :: Int               -- participation total used for quorum
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
  | ECreateProposal ProposalId WalletId Int Category VoteMode SlotNo SlotNo Int
  | EVote ProposalId WalletId Vote Int
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
  Chain
    { csSlot      = 0
    , csNextWid   = 1
    , csWallets   = M.empty
    , csProposals = M.empty

    , csPropCount = M.empty
    , csTreasury  = initialTreasury
    , csBurned    = 0

    , csEvents    = []
    , csNextEIx   = 0
    , csLastHash  = replicate 64 '0'
    }

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
verifyAudit st = do
  let entries = csEvents st
  let go _ [] = return True
      go expectedPrev (e:es) = do
        let payload = show (leIx e) ++ "|" ++ show (leSlot e) ++ "|" ++ show (leEvent e) ++ "|" ++ expectedPrev
        h <- hash256 payload
        if lePrev e /= expectedPrev then return False
        else if leHash e /= h then return False
        else go (leHash e) es
  go (replicate 64 '0') entries

-- ============================================================
-- UI RENDER
-- ============================================================

renderUI :: Chain -> IO ()
renderUI st = do
  clearScreen
  putStrLn (paint cyan "======================================================================")
  putStrLn (paint magenta "   CARDANO-LIKE GOVERNANCE L1  (Quorum + Stake + Audit + Categories)")
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
  putStrLn (paint white " | CHAIN               | WALLETS                     | GOVERNANCE     |")
  putStrLn (paint cyan  " |---------------------+-----------------------------+----------------|")
  putStrLn (paint white " | 1 Advance Slot      | 2 Create Wallet (secret)    | 6 List Proposals")
  putStrLn (paint white " |                     | 3 Faucet Wallet             | 7 Vote (select)")
  putStrLn (paint white " |                     | 4 List Wallets              | 8 Close (select)")
  putStrLn (paint white " |                     | 5 Stake Tokens              | 9 Finalize (select)")
  putStrLn (paint cyan  " |---------------------+-----------------------------+----------------|")
  putStrLn (paint white " | AUDIT               |                             |                |")
  putStrLn (paint cyan  " |---------------------+-----------------------------+----------------|")
  putStrLn (paint white " | 10 View Audit Log   | 11 Verify Audit Hash-Chain  | 12 Create Proposal")
  putStrLn (paint cyan  "----------------------------------------------------------------------")
  putStrLn (paint red   " | 0 Exit                                                             |")
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
          window = show (pStart p) ++ "→" ++ show (pEnd p)
          q = "quorum=" ++ show (pQuorumPct p) ++ "%"
          tally =
            case pStatus p of
              PFinalized ->
                " YES=" ++ show (pYesTotal p) ++ " NO=" ++ show (pNoTotal p) ++ " part=" ++ show (pPartTotal p)
                ++ " result=" ++ show (pResult p)
              _ -> " votes=" ++ show (M.size (pVotes p))
          extra =
            case pFundingTarget p of
              Nothing -> ""
              Just (to, amt) -> " -> #" ++ show to ++ " amt=" ++ show amt
      putStrLn $
        paint white ("  " ++ short (pId p)) ++ " " ++
        paint col ("[" ++ show (pStatus p) ++ "] ") ++
        paint cyan (pTitle p) ++
        paint gray ("  cat=" ++ catTxt (pCategory p)
                   ++ " mode=" ++ modeTxt (pMode p)
                   ++ " " ++ q
                   ++ " win=" ++ window
                   ++ " dep=" ++ show (pDeposit p)
                   ++ " prop=#" ++ show (pProposer p)
                   ++ extra
                   ++ tally)

-- ============================================================
-- PROPOSAL SELECTION (no hash typing)
-- ============================================================

data SelectFilter
  = VoteableNow
  | AnyOpen
  | ClosedOnly

selectProposal :: Chain -> SelectFilter -> IO (Maybe Proposal)
selectProposal st filt = do
  let psAll = M.elems (csProposals st)
      ps = case filt of
             VoteableNow -> filter isVoteable psAll
             AnyOpen     -> filter (\p -> pStatus p == POpen) psAll
             ClosedOnly  -> filter (\p -> pStatus p == PClosed) psAll

  if null ps
    then do
      case filt of
        VoteableNow -> do
          putStrLn (paint yellow "No proposals currently accepting votes.")
          explainWhy st
        AnyOpen     -> putStrLn (paint gray "No open proposals.")
        ClosedOnly  -> putStrLn (paint gray "No closed proposals ready to finalize.")
      pause
      return Nothing
    else do
      putStrLn (paint cyan "------------------------------------------------------------------")
      mapM_ (ppLine st) (zip [1..] ps)
      putStrLn (paint cyan "------------------------------------------------------------------")
      n <- promptInt "Select proposal number: "
      if n < 1 || n > length ps
        then do
          putStrLn (paint red "Invalid selection.")
          pause
          return Nothing
        else return (Just (ps !! (n - 1)))

  where
    isVoteable p =
      pStatus p == POpen
      && csSlot st >= pStart p
      && csSlot st <= pEnd p

ppLine :: Chain -> (Int, Proposal) -> IO ()
ppLine st (i, p) = do
  let win = show (pStart p) ++ "→" ++ show (pEnd p)
      modeTxt OnePersonOneVote = "1p1v"
      modeTxt StakeWeighted    = "stake"
      catTxt Funding = "Funding"
      catTxt Policy  = "Policy"
      catTxt Info    = "Info"
      voteable = (pStatus p == POpen && csSlot st >= pStart p && csSlot st <= pEnd p)
      tag = if voteable then paint green "VOTE NOW" else paint gray "not voteable"
  putStrLn $
    paint white ("[" ++ show i ++ "] ") ++
    paint cyan (pTitle p) ++
    paint gray ("  (" ++ catTxt (pCategory p) ++ "/" ++ modeTxt (pMode p) ++ ")") ++
    paint gray ("  win=" ++ win) ++
    paint gray ("  votes=" ++ show (M.size (pVotes p))) ++
    "  " ++ tag

explainWhy :: Chain -> IO ()
explainWhy st = do
  let ps = M.elems (csProposals st)
      openPs = filter (\p -> pStatus p == POpen) ps
  if null openPs
    then putStrLn (paint gray "Reason: there are no open proposals.")
    else do
      putStrLn (paint gray "Open proposals exist, but voting may be outside the window:")
      mapM_ (\p ->
        putStrLn $
          paint gray (" - " ++ pTitle p ++ ": window " ++ show (pStart p) ++ "→" ++ show (pEnd p)
            ++ ", current slot " ++ show (csSlot st))
        ) openPs

-- ============================================================
-- QUORUM + TALLY
-- ============================================================

requiredParticipation :: Proposal -> Int
requiredParticipation p =
  case pMode p of
    OnePersonOneVote ->
      ceilingDiv (pQuorumPct p * pEligSnap p) 100
    StakeWeighted ->
      ceilingDiv (pQuorumPct p * pStakeSnap p) 100

ceilingDiv :: Int -> Int -> Int
ceilingDiv a b = (a + b - 1) `div` b

computeTally :: Proposal -> (Int, Int, Int)
computeTally p =
  case pMode p of
    OnePersonOneVote ->
      let votes = M.elems (pVotes p)
          yes = length [() | (YES,_) <- votes]
          no  = length [() | (NO,_)  <- votes]
          part = length votes
      in (yes, no, part)
    StakeWeighted ->
      let votes = M.elems (pVotes p)
          yes = sum [w | (YES,w) <- votes]
          no  = sum [w | (NO,w)  <- votes]
          part = sum [w | (_,w) <- votes]
      in (yes, no, part)

-- ============================================================
-- MAIN
-- ============================================================

main :: IO ()
main = do
  -- create an initial audit entry for genesis (optional but nice)
  st1 <- appendEvent initChain (EAdvanceSlot 0)
  loop st1

loop :: Chain -> IO ()
loop st = do
  renderUI st
  c <- prompt "> "
  case c of

    "1" -> do
      let st' = st { csSlot = csSlot st + 1 }
      st'' <- appendEvent st' (EAdvanceSlot (csSlot st' ))
      loop st''

    "2" -> do
      name <- prompt "Wallet name: "
      sec  <- prompt "Wallet secret (keep private): "
      let wid = csNextWid st
          w = Wallet wid name sec 0 0
          st' = st { csNextWid = wid + 1
                   , csWallets = M.insert wid w (csWallets st) }
      st'' <- appendEvent st' (ECreateWallet wid)
      loop st''

    "3" -> do
      wid <- promptInt "Wallet id to faucet: "
      case M.lookup wid (csWallets st) of
        Nothing -> do
          putStrLn (paint red "Wallet not found.")
          pause
          loop st
        Just w -> do
          let w' = w { wBalance = wBalance w + faucetAmount }
              st' = st { csWallets = M.insert wid w' (csWallets st) }
          st'' <- appendEvent st' (EFaucet wid faucetAmount)
          loop st''

    "4" -> do
      printWallets st
      pause
      loop st

    "5" -> do
      wid <- promptInt "Wallet id to stake from: "
      amt <- promptInt "Amount to stake: "
      case M.lookup wid (csWallets st) of
        Nothing -> do
          putStrLn (paint red "Wallet not found.")
          pause
          loop st
        Just w -> do
          if amt <= 0
            then do putStrLn (paint red "Amount must be > 0.") >> pause >> loop st
            else if wBalance w < amt
              then do putStrLn (paint red "Insufficient balance.") >> pause >> loop st
              else do
                let w' = w { wBalance = wBalance w - amt, wStake = wStake w + amt }
                    st' = st { csWallets = M.insert wid w' (csWallets st) }
                st'' <- appendEvent st' (EStake wid amt)
                loop st''

    "6" -> do
      printProposals st
      pause
      loop st

    "7" -> do
      mp <- selectProposal st VoteableNow
      case mp of
        Nothing -> loop st
        Just p -> do
          wid <- promptInt "Wallet id voting: "
          case M.lookup wid (csWallets st) of
            Nothing -> do
              putStrLn (paint red "Wallet not found.")
              pause
              loop st
            Just w -> do
              v <- fmap (map toUpper) (prompt "Vote YES or NO: ")
              let mv = if v == "YES" then Just YES else if v == "NO" then Just NO else Nothing
              case mv of
                Nothing -> do
                  putStrLn (paint red "Invalid vote.")
                  pause
                  loop st
                Just vote -> do
                  let weight =
                        case pMode p of
                          OnePersonOneVote -> 1
                          StakeWeighted    -> wStake w
                  if weight <= 0
                    then do
                      putStrLn (paint red "No voting power. Stake is 0.")
                      pause
                      loop st
                    else do
                      let p' = p { pVotes = M.insert wid (vote, weight) (pVotes p) }
                          st' = st { csProposals = M.insert (pId p) p' (csProposals st) }
                      st'' <- appendEvent st' (EVote (pId p) wid vote weight)
                      loop st''

    "8" -> do
      mp <- selectProposal st AnyOpen
      case mp of
        Nothing -> loop st
        Just p -> do
          let p' = p { pStatus = PClosed }
              st' = st { csProposals = M.insert (pId p) p' (csProposals st) }
          st'' <- appendEvent st' (EClose (pId p))
          loop st''

    "9" -> do
      mp <- selectProposal st ClosedOnly
      case mp of
        Nothing -> loop st
        Just p -> do
          let (yesT, noT, partT) = computeTally p
              req = requiredParticipation p
              quorumMet = partT >= req
              passed = quorumMet && yesT > noT

              proposerId = pProposer p
              proposer = csWallets st M.! proposerId

              -- deposit settlement
              burnAmt = if passed then 0 else (pDeposit p * burnFailurePct) `div` 100
              refundAmt = pDeposit p - burnAmt

              proposer' = proposer { wBalance = wBalance proposer + refundAmt }

              stA = st { csWallets = M.insert proposerId proposer' (csWallets st)
                       , csBurned = csBurned st + burnAmt }

          stB <- appendEvent stA (EFinalize (pId p) passed yesT noT partT)
          stC <- if refundAmt > 0
                 then appendEvent stB (EDepositRefund (pId p) proposerId refundAmt)
                 else return stB
          stD <- if burnAmt > 0
                 then appendEvent stC (EDepositBurn (pId p) burnAmt)
                 else return stC

          -- funding execution if passed and category funding
          stE <- case (passed, pCategory p, pFundingTarget p) of
                   (True, Funding, Just (toWid, amt)) ->
                     if csTreasury stD >= amt
                       then case M.lookup toWid (csWallets stD) of
                              Nothing -> do
                                -- if recipient missing, treat as no-op (still finalizes)
                                return stD
                              Just recv -> do
                                let recv' = recv { wBalance = wBalance recv + amt }
                                    stX = stD { csTreasury = csTreasury stD - amt
                                              , csWallets  = M.insert toWid recv' (csWallets stD) }
                                stY <- appendEvent stX (ETreasuryTransfer (pId p) toWid amt)
                                return stY
                       else do
                         -- treasury insufficient: still finalize, but no transfer
                         return stD
                   _ -> return stD

          let p' = p { pStatus = PFinalized
                     , pResult = Just passed
                     , pYesTotal = yesT
                     , pNoTotal = noT
                     , pPartTotal = partT
                     }
              stF = stE { csProposals = M.insert (pId p) p' (csProposals stE) }

          -- show result
          clearScreen
          putStrLn (paint cyan "======================================================================")
          putStrLn (paint magenta "  FINALIZE RESULT")
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

    "10" -> do
      clearScreen
      putStrLn (paint cyan "======================================================================")
      putStrLn (paint magenta "  AUDIT LOG (Hash-Chained)")
      putStrLn (paint cyan "======================================================================")
      if null (csEvents st)
        then putStrLn (paint gray "  (no events)")
        else mapM_ pp (csEvents st)
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

    "11" -> do
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

    "12" -> do
      wid <- promptInt "Proposer wallet id: "
      case M.lookup wid (csWallets st) of
        Nothing -> do
          putStrLn (paint red "Wallet not found.")
          pause
          loop st
        Just w -> do
          let cost = proposalCost wid st
          putStrLn (paint gray ("Next proposal deposit for this wallet (epoch " ++ show (currentEpoch st) ++ "): " ++ show cost))
          if wBalance w < cost
            then do
              putStrLn (paint red "Insufficient balance for deposit.")
              pause
              loop st
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
              s <- promptInt "Start slot: "
              e <- promptInt "End slot: "

              -- funding details if needed
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
                        { pId = pid
                        , pTitle = t
                        , pDesc = d
                        , pCategory = cat
                        , pMode = mode
                        , pProposer = wid
                        , pDeposit = cost
                        , pStart = s
                        , pEnd = e
                        , pQuorumPct = qpct
                        , pEligSnap = eligSnap
                        , pStakeSnap = stakeSnap
                        , pFundingTarget = fTarget
                        , pStatus = POpen
                        , pVotes = M.empty
                        , pResult = Nothing
                        , pYesTotal = 0
                        , pNoTotal = 0
                        , pPartTotal = 0
                        }

                  w' = w { wBalance = wBalance w - cost }
                  key = (wid, currentEpoch st)
                  newCount = proposalCount wid st + 1

                  st' = st { csWallets   = M.insert wid w' (csWallets st)
                           , csProposals = M.insert pid p (csProposals st)
                           , csPropCount = M.insert key newCount (csPropCount st) }

              st'' <- appendEvent st' (ECreateProposal pid wid cost cat mode s e qpct)
              loop st''

    "0" -> putStrLn "Bye."
    _ -> loop st
