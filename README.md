# Pay-per-View with Smart Contracts

A decentralized pay-per-view system built on Stacks blockchain that enables content creators to monetize their content directly through smart contracts.

## 🌟 Features

- Content creators can publish and price their content
- Users can purchase access to content
- Automatic revenue tracking per creator
- View count tracking
- Content management system
- Direct creator payments

## 📚 Contract Functions

### For Content Creators

- `add-content`: Publish new content with title and price
- `update-content-price`: Modify content price
- `deactivate-content`: Remove content from active listing

### For Users

- `purchase-content`: Buy access to specific content
- `get-content`: View content details
- `get-purchase-status`: Check purchase history
- `get-creator-stats`: View creator statistics

## 🚀 Usage

1. Deploy the contract to Stacks blockchain
2. Content creators can add content using `add-content`
3. Users can purchase access using `purchase-content`
4. Track statistics and manage content through provided functions

## 💡 Example

```clarity
;; Add new content
(contract-call? .pay-per-view add-content "My Amazing Video" u100)

;; Purchase content
(contract-call? .pay-per-view purchase-content u1)
```

## ⚖️ License

MIT License
```
