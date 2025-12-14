# Governance L1 – Commit–Reveal Voting Engine

This repository contains a **single-file Haskell implementation of a Cardano-inspired Layer-1 governance system** designed to demonstrate **fair, auditable, and censorship-resistant digital voting**.

The project is intentionally console-based and self-contained so it can be:
- audited easily
- learned step-by-step
- extended into production systems later

No digital identity, social credit system, or centralized authority is required.

---

## Core Concepts

This system models real blockchain governance primitives:

- Slot / epoch–based time
- Economic incentives and penalties
- Commit–reveal voting
- Stake-weighted or 1-person-1-vote modes
- Treasury and proposal deposits
- Hash-chained audit logs
- Deterministic, replayable results

The design is inspired by **Cardano governance, on-chain voting research, and cryptographic voting systems**, simplified for clarity and education.

---

## Why You Cannot See Votes Immediately

This system uses **commit–reveal voting**.

That means:

- During the **commit phase**, votes are intentionally hidden
- Only cryptographic commitments (hashes) are stored
- No one can see YES / NO totals early
- This prevents coercion, bribery, and bandwagon effects

Votes become visible **only after**:
1. the reveal window opens
2. voters reveal their vote + salt
3. the proposal is closed
4. the proposal is finalized

This behavior is **by design** and mirrors how secure governance systems work.

---

## Features

### Wallets
- Wallet creation
- Faucet funding
- Token balances
- Token staking (locked stake)

### Proposals
- Created by wallets (not admins)
- Deposit required (economic throttle)
- Deposit refunded on pass
- Deposit burned on failure
- Proposal categories:
  - Funding
  - Policy
  - Information
- Treasury funding proposals supported

### Voting
- Commit–reveal scheme
- Vote windows enforced by slot
- Reveal windows enforced by slot
- Public or private voting modes
- One-person-one-vote
- Stake-weighted voting
- Quorum enforcement
- Double-vote prevention

### Governance & Economics
- Treasury balance
- Burn tracking
- Economic throttling via deposits
- Slot-based lifecycle
- Epoch awareness

### Audit & Integrity
- Event log
- Hash-chained audit trail
- Proposal certification records
- Deterministic finalization
- Verifiable results

### Console Interface
- ANSI-colored output
- Compact ASCII layout
- Clear lifecycle phases
- Proposal listings with:
  - short IDs
  - status
  - vote & reveal windows
  - participation counts

---

## Typical Workflow

1. Advance slots to move time forward
2. Create wallets
3. Faucet tokens into wallets
4. (Optional) Stake tokens
5. Create a proposal (deposit is locked)
6. Advance into the commit window
7. Commit votes (vote + salt → hash)
8. Advance into the reveal window
9. Reveal votes (vote + salt verified)
10. Close the proposal
11. Finalize the proposal
12. Review certification and audit logs

Results are **only shown after finalization**.

---

## What This Is (and Is Not)

### This IS:
- A realistic governance simulation
- A teaching tool for blockchain voting
- A foundation for enterprise or organizational governance
- A system designed for transparency without surveillance

### This is NOT:
- A production blockchain
- A replacement for cryptographic libraries
- A finished ZK implementation (but it is ZK-ready)

---

## ZK Readiness

The commit–reveal structure is intentionally designed so that:
- Commitments can later be replaced with zero-knowledge proofs
- Tallies can be verified without revealing individual votes
- Privacy can be increased without changing governance logic

This makes the system suitable for future work on:
- ZK vote tallying
- anonymous credentials
- confidential governance

---

## License & Use

This project is provided for:
- education
- research
- experimentation
- governance prototyping

It is intentionally simple, readable, and extensible.

---

## Next Steps (Planned / Optional)

- Zero-knowledge tally proofs
- JSON export of certifications
- Proposal selection menus (no ID typing)
- Persistent storage
- Networked multi-node simulation
- Documentation walkthrough / tutorial mode

---

## Philosophy

Digital voting **does not require**:
- biometric IDs
- centralized databases
- social credit systems
- surveillance infrastructure

It requires:
- cryptography
- economic incentives
- transparency
- verifiability
- time-based finality

This project exists to demonstrate that clearly.
