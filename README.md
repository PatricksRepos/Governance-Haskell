# Governance L1 – Commit–Reveal Blockchain Voting (Haskell)

This repository contains a **console-based governance Layer-1 simulation**, written in Haskell, inspired by modern blockchain governance systems (Cardano, Polkadot, Cosmos).

The project demonstrates how **fair, auditable, privacy-preserving digital voting** can be implemented **without digital identity systems, KYC, or social credit frameworks**.

It is designed as:
- a technical tutorial
- a governance research prototype
- a demonstration that digital voting can be verifiable and fair

---

## Scope and Goals

This system models **on-chain governance**, not a cryptocurrency network.

It focuses on:
- proposal lifecycle
- voting rules
- quorum enforcement
- economic spam resistance
- auditability
- treasury governance

The ledger records **authorization and consensus**, not real-world enforcement.

---

## Core Features

### Wallets
- Create wallets with private secrets
- Faucet to mint governance tokens
- Stake tokens for stake-weighted voting
- Explicit balance and stake tracking

---

### Governance Proposals
- Proposal categories:
  - **Funding** – authorize treasury transfers
  - **Policy** – governance decisions without funds
  - **Info** – signaling proposals
- Proposal deposit required
- Deposit cost increases **exponentially per wallet per epoch**
- One proposal per wallet per slot (economic throttling)
- Vote window and reveal window defined in slots

---

### Voting Modes
- **One-person-one-vote**
- **Stake-weighted voting**
- Voting power snapshotted at commit time
- Vote weight immutable once committed

---

### Commit–Reveal Voting
- Votes are **private during the voting phase**
- Voters submit cryptographic commitments
- Votes are revealed later with verification
- Prevents vote buying, coercion, and strategic signaling
- Strict time windows enforced

Voting phases:
1. Commit phase
2. Reveal phase
3. Finalization

Votes revealed outside the reveal window are rejected.

---

### Quorum and Tally
- Minimum participation quorum (percentage-based)
- Proposal passes only if:
  - quorum is met
  - YES votes exceed NO votes
- Tallies computed deterministically at finalization

---

### Treasury Governance
- Treasury balance tracked globally
- Funding proposals specify:
  - recipient wallet
  - funding amount
- Treasury funds move **only if a funding proposal passes**
- Proposal deposits:
  - refunded on success
  - partially burned on failure
- Burned tokens tracked explicitly

---

### Audit Log (Tamper-Evident)
- Every action emits an event
- Events are **hash-chained**
- Full append-only audit trail
- Verifiable at any time
- Includes:
  - wallet creation
  - staking
  - proposal creation
  - vote commits
  - vote reveals
  - finalization
  - treasury transfers
  - deposit burns/refunds

---

### Console Interface
- Colored ASCII UI
- Numbered proposal selection (no hash typing)
- Proposal status clearly displayed:
  - commit counts
  - reveal counts
  - voting windows
- Designed for walkthroughs and screenshots

---

## Running the Project

### Requirements
- Linux or macOS
- `openssl`
- GHC with `runhaskell`

### Run
```bash
runhaskell SimpleL1.hs
