# 🏢 Supplier Reputation Ledger

A decentralized reputation management system for suppliers built on the Stacks blockchain using Clarity smart contracts.

## 🌟 Features

- **Supplier Registration** 📝 - Register as a supplier with stake requirements
- **Reputation System** ⭐ - Rate suppliers from 1-5 stars with comments
- **Escrow Transactions** 💰 - Secure payment system with buyer protection
- **Dispute Resolution** ⚖️ - Admin-mediated dispute resolution process
- **Stake Management** 💎 - Suppliers stake STX tokens to participate
- **Platform Fees** 💳 - Configurable platform fees for transactions

## 🚀 Quick Start

### Prerequisites

- [Clarinet](https://docs.hiro.so/stacks/clarinet) installed
- Stacks wallet for testing

### Installation

```bash
git clone <repository-url>
cd Supplier-Reputation-Ledger
clarinet check
```

## 📖 Usage

### For Suppliers

#### 1. Register as a Supplier 📋
```clarity
(contract-call? .Supplier-Reputation-Ledger register-supplier "My Company" "Electronics")
```

#### 2. Update Supplier Information ✏️
```clarity
(contract-call? .Supplier-Reputation-Ledger update-supplier-info "Updated Company Name" "Technology")
```

#### 3. Increase Stake 📈
```clarity
(contract-call? .Supplier-Reputation-Ledger increase-stake u500000)
```

#### 4. Deactivate Account 🛑
```clarity
(contract-call? .Supplier-Reputation-Ledger deactivate-supplier)
```

### For Buyers

#### 1. Rate a Supplier ⭐
```clarity
(contract-call? .Supplier-Reputation-Ledger rate-supplier u1 u5 "Excellent service and quality!")
```

#### 2. Create Transaction 💸
```clarity
(contract-call? .Supplier-Reputation-Ledger create-transaction u1 u1000000)
```

#### 3. Complete Transaction ✅
```clarity
(contract-call? .Supplier-Reputation-Ledger complete-transaction u1)
```

#### 4. Dispute Transaction ⚠️
```clarity
(contract-call? .Supplier-Reputation-Ledger dispute-transaction u1)
```

### Read-Only Functions 🔍

#### Get Supplier Information
```clarity
(contract-call? .Supplier-Reputation-Ledger get-supplier u1)
```

#### Get Supplier by Owner
```clarity
(contract-call? .Supplier-Reputation-Ledger get-supplier-by-owner 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

#### Get Supplier Reputation
```clarity
(contract-call? .Supplier-Reputation-Ledger get-supplier-reputation u1)
```

#### Check Rating
```clarity
(contract-call? .Supplier-Reputation-Ledger get-rating u1 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

## 🏗️ Contract Architecture

### Data Structures

- **Suppliers**: Core supplier information with reputation metrics
- **Ratings**: Individual supplier ratings and reviews
- **Transactions**: Escrow-based payment system
- **Escrow Funds**: Locked funds during pending transactions

### Key Parameters

- **Minimum Stake**: Default 1 STX (1,000,000 microSTX)
- **Platform Fee**: Default 0.5% (50 basis points)
- **Rating Scale**: 1-5 stars
- **Transaction Timeout**: 144 blocks (~24 hours)

## 🛡️ Security Features

- **Stake Requirements**: Suppliers must stake STX to participate
- **Escrow System**: Funds held in escrow until transaction completion
- **Dispute Resolution**: Admin-mediated dispute resolution
- **Access Controls**: Owner-only administrative functions
- **Input Validation**: Comprehensive parameter validation

## 🧪 Testing

```bash
npm install
npm test
```

## 📊 Error Codes

| Code | Description |
|------|-------------|
| u100 | Unauthorized access |
| u101 | Supplier not found |
| u102 | Invalid rating (must be 1-5) |
| u103 | Already rated this supplier |
| u104 | Insufficient funds |
| u105 | Supplier already exists |
| u106 | Invalid parameters |

## 🔧 Admin Functions

Only the contract owner can:
- Set platform fees
- Set minimum stake requirements
- Resolve disputes
- Emergency pause suppliers
- Withdraw platform fees

## 📈 Reputation Calculation

Average rating = (Total rating points × 100) ÷ Number of ratings

This provides a percentage-based score where:
- 500 = 5-star average (100%)
- 400 = 4-star average (80%)
- 300 = 3-star average (60%)


## 📄 License

MIT License - see LICENSE file for details
