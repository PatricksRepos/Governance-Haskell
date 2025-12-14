# Cardano-Like Governance L1 (Haskell)

This project is a **console-based simulation of a Cardano-style Layer-1 governance system**, written in Haskell.

It is **not a cryptocurrency**, but a deterministic governance ledger that models how modern proof-of-stake blockchains (like Cardano) handle proposals, voting, quorum, deposits, treasury funding, and auditability.

The goal is **education, experimentation, and protocol design exploration**.

---

## ✨ Features

### 🧾 Wallets
- Create wallets with a private secret
- Faucet to mint governance tokens
- Stake tokens for stake-weighted voting
- Wallet balances + stake tracked explicitly

---

### 🗳️ Governance Proposals
- Proposal **categories**:
  - **Funding** (treasury → recipient on pass)
  - **Policy** (parameter / rule changes)
  - **Info** (signaling only)
- Proposal **deposit required**
  - Deposit cost increases **exponentially per wallet per epoch**
- Voting window defined by **slot range**
- One proposal can be created per wallet per slot, economically throttled

---

### 🗳️ Voting Modes
- **1-person-1-vote**
- **Stake-weighted voting**
- Vote weights are snapshotted at vote time
- Proposals can only be voted on **inside their voting window**

---

### 📊 Quorum & Tally
- Minimum **participation quorum** (percentage-based)
- Proposal passes only if:
  - quorum is met **and**
  - YES votes > NO votes
- Automatic tally on finalization

---

### 💰 Economic Rules
- Proposal **deposit is refunded** if proposal passes
- If proposal fails (or quorum fails):
  - A percentage of the deposit is **burned**
- Funding proposals pay from a **shared treasury**
- Burned tokens are tracked separately

---

### 🔐 Tamper-Evident Audit Log
- Every action emits an **event**
- Events are **hash-chained (SHA-256)**:
  - each event includes the previous hash
- Full append-only audit trail:
  - create wallet
  - faucet
  - stake
  - create proposal
  - vote
  - close
  - finalize
- Ledger integrity can be verified at any time

---

### 🖥️ Console UI
- Colored ASCII dashboard
- Numbered selection (no hash typing)
- Contextual proposal selection
- Human-friendly explanations when actions are unavailable
- Designed to feel like a real governance CLI, not a dev REPL

---

## 🧠 Conceptual Model

This project mirrors real blockchain governance:

| On-Chain (simulated) | Off-Chain (real world) |
|---------------------|------------------------|
| Votes recorded       | Humans act on outcomes |
| Treasury transfers   | Funds actually spent   |
| Finalized proposals  | Policy enforcement     |

The ledger **records authorization and consensus**, not real-world enforcement.

---

## ▶️ Running the Project

### Requirements
- Linux or macOS
- `openssl`
- GHC / `runhaskell`

### Run
```bash
runhaskell SimpleL1.hs
