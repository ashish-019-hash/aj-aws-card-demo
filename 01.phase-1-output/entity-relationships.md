# Entity Relationship Mappings - CardDemo COBOL Application

## Overview
This document provides detailed mapping of relationships between business entities in the CardDemo COBOL application, including cardinality, foreign key relationships, and business process flows.

## Primary Entity Relationships

### 1. Customer-Account-Card Hierarchy

```
Customer (1) ──→ Card Cross-Reference (N) ──→ Account (1)
    │                                              │
    └──────────→ Card Cross-Reference (N) ──→ Card (1)
                                                   │
                                                   └──→ Transaction (N)
```

**Relationship Details:**
- **Customer to Card Cross-Reference**: 1:N
  - Foreign Key: XREF-CUST-ID references CUST-ID
  - Business Rule: One customer can have multiple card cross-references
  
- **Card Cross-Reference to Account**: N:1
  - Foreign Key: XREF-ACCT-ID references ACCT-ID
  - Business Rule: Multiple cross-references can point to same account
  
- **Card Cross-Reference to Card**: 1:1
  - Foreign Key: XREF-CARD-NUM references CARD-NUM
  - Business Rule: Each cross-reference maps to exactly one card

### 2. Account-Card-Transaction Flow

```
Account (1) ──→ Card (N) ──→ Transaction (N)
    │                           │
    │                           └──→ Daily Transaction (N)
    │
    └──→ Transaction Category Balance (N)
```

**Relationship Details:**
- **Account to Card**: 1:N
  - Foreign Key: CARD-ACCT-ID references ACCT-ID
  - Business Rule: One account can have multiple cards
  
- **Card to Transaction**: 1:N
  - Foreign Key: TRAN-CARD-NUM references CARD-NUM
  - Business Rule: One card can have multiple transactions
  
- **Card to Daily Transaction**: 1:N
  - Foreign Key: DALYTRAN-CARD-NUM references CARD-NUM
  - Business Rule: Daily processing creates separate transaction records

### 3. Transaction Classification Hierarchy

```
Transaction Type (1) ──→ Transaction Category (N) ──→ Transaction (N)
         │                        │                       │
         │                        │                       └──→ Daily Transaction (N)
         │                        │
         │                        └──→ Transaction Category Balance (N)
         │
         └──→ Transaction (N)
         │
         └──→ Daily Transaction (N)
         │
         └──→ Transaction Category Balance (N)
```

**Relationship Details:**
- **Transaction Type to Transaction Category**: 1:N
  - Foreign Key: TRAN-TYPE-CD in TRAN-CAT-RECORD references TRAN-TYPE
  - Business Rule: Each transaction type can have multiple categories
  
- **Transaction Type to Transaction**: 1:N
  - Foreign Key: TRAN-TYPE-CD references TRAN-TYPE
  - Business Rule: Each transaction must have a valid type
  
- **Transaction Category to Transaction**: 1:N
  - Foreign Key: TRAN-CAT-CD references TRAN-CAT-CD in TRAN-CAT-RECORD
  - Business Rule: Each transaction must have a valid category within its type

### 4. Account Balance Tracking

```
Account (1) ──→ Transaction Category Balance (N)
                         │
                         ├──→ Transaction Type (1)
                         └──→ Transaction Category (1)
```

**Relationship Details:**
- **Account to Transaction Category Balance**: 1:N
  - Foreign Key: TRANCAT-ACCT-ID references ACCT-ID
  - Business Rule: Each account tracks balances by transaction category
  
- **Transaction Category Balance to Transaction Type**: N:1
  - Foreign Key: TRANCAT-TYPE-CD references TRAN-TYPE
  - Business Rule: Balance tracking requires valid transaction type
  
- **Transaction Category Balance to Transaction Category**: N:1
  - Foreign Key: TRANCAT-TYPE-CD + TRANCAT-CD references TRAN-CAT-KEY
  - Business Rule: Balance tracking requires valid transaction category

### 5. Configuration and Disclosure Management

```
Account Group (1) ──→ Account (N)
         │
         └──→ Disclosure Group (N)
                    │
                    ├──→ Transaction Type (1)
                    └──→ Transaction Category (1)
```

**Relationship Details:**
- **Account Group to Account**: 1:N
  - Foreign Key: ACCT-GROUP-ID in ACCOUNT-RECORD
  - Business Rule: Accounts are grouped for configuration purposes
  
- **Account Group to Disclosure Group**: 1:N
  - Foreign Key: DIS-ACCT-GROUP-ID references account group
  - Business Rule: Each account group can have multiple disclosure configurations
  
- **Disclosure Group to Transaction Type**: N:1
  - Foreign Key: DIS-TRAN-TYPE-CD references TRAN-TYPE
  - Business Rule: Disclosure rates are defined per transaction type
  
- **Disclosure Group to Transaction Category**: N:1
  - Foreign Key: DIS-TRAN-TYPE-CD + DIS-TRAN-CAT-CD references TRAN-CAT-KEY
  - Business Rule: Disclosure rates are defined per transaction category

## Cardinality Matrix

| Parent Entity | Child Entity | Cardinality | Foreign Key Field | Business Rule |
|---------------|--------------|-------------|-------------------|---------------|
| Customer | Card Cross-Reference | 1:N | XREF-CUST-ID | One customer, multiple cards |
| Account | Card | 1:N | CARD-ACCT-ID | One account, multiple cards |
| Account | Transaction Category Balance | 1:N | TRANCAT-ACCT-ID | Balance tracking per category |
| Card | Transaction | 1:N | TRAN-CARD-NUM | Card usage generates transactions |
| Card | Daily Transaction | 1:N | DALYTRAN-CARD-NUM | Daily processing of transactions |
| Transaction Type | Transaction Category | 1:N | TRAN-TYPE-CD | Type contains multiple categories |
| Transaction Type | Transaction | 1:N | TRAN-TYPE-CD | All transactions have a type |
| Transaction Type | Daily Transaction | 1:N | DALYTRAN-TYPE-CD | Daily transactions have types |
| Transaction Type | Transaction Category Balance | 1:N | TRANCAT-TYPE-CD | Balance tracking by type |
| Transaction Type | Disclosure Group | 1:N | DIS-TRAN-TYPE-CD | Disclosure rates by type |
| Transaction Category | Transaction | 1:N | TRAN-CAT-CD | All transactions have a category |
| Transaction Category | Daily Transaction | 1:N | DALYTRAN-CAT-CD | Daily transactions have categories |
| Transaction Category | Transaction Category Balance | 1:N | TRANCAT-CD | Balance tracking by category |
| Transaction Category | Disclosure Group | 1:N | DIS-TRAN-CAT-CD | Disclosure rates by category |
| Account Group | Account | 1:N | ACCT-GROUP-ID | Accounts grouped for configuration |
| Account Group | Disclosure Group | 1:N | DIS-ACCT-GROUP-ID | Group-specific disclosure rates |

## Business Process Flows

### 1. Customer Onboarding Process
```
1. Create Customer Record (CUSTOMER-RECORD)
2. Create Account Record (ACCOUNT-RECORD) 
3. Create Card Record (CARD-RECORD)
4. Create Card Cross-Reference (CARD-XREF-RECORD)
5. Initialize Transaction Category Balances (TRAN-CAT-BAL-RECORD)
```

### 2. Transaction Processing Flow
```
1. Validate Card (CARD-RECORD)
2. Validate Transaction Type (TRAN-TYPE-RECORD)
3. Validate Transaction Category (TRAN-CAT-RECORD)
4. Create Transaction (TRAN-RECORD)
5. Update Account Balance (ACCOUNT-RECORD)
6. Update Category Balance (TRAN-CAT-BAL-RECORD)
7. Apply Interest Rates (DISCLOSURE-GROUP-RECORD)
```

### 3. Daily Processing Flow
```
1. Process Transactions (TRAN-RECORD)
2. Create Daily Transaction Records (DALYTRAN-RECORD)
3. Update Category Balances (TRAN-CAT-BAL-RECORD)
4. Generate Reports (TRANSACTION-DETAIL-REPORT)
5. Archive/Purge as needed
```

### 4. Interest Calculation Flow
```
1. Identify Account Group (ACCOUNT-RECORD.ACCT-GROUP-ID)
2. Lookup Disclosure Rates (DISCLOSURE-GROUP-RECORD)
3. Apply Rates by Transaction Category
4. Update Account Balances
5. Generate Disclosure Statements
```

## Data Integrity Rules

### Referential Integrity
1. **Customer-Account-Card**: All cards must have valid accounts, all accounts must have valid customers via cross-reference
2. **Transaction Classification**: All transactions must have valid type and category codes
3. **Balance Consistency**: Transaction category balances must reconcile with transaction details
4. **Configuration Validity**: All disclosure groups must reference valid account groups and transaction categories

### Business Rules
1. **Card Activation**: Cards must be active to process transactions
2. **Account Status**: Accounts must be active for new transactions
3. **Credit Limits**: Transaction amounts cannot exceed account credit limits
4. **Category Limits**: Some transaction categories may have specific limits
5. **Interest Application**: Interest rates are applied based on account group and transaction category

### Data Validation
1. **Primary Key Uniqueness**: All entity primary keys must be unique
2. **Foreign Key Validity**: All foreign key references must exist in parent tables
3. **Date Consistency**: Transaction dates must be logical (orig-ts <= proc-ts)
4. **Amount Validation**: Transaction amounts must be within reasonable ranges
5. **Status Consistency**: Entity statuses must be consistent across related records

## Performance Considerations

### Indexing Strategy
1. **Primary Keys**: All primary keys should be indexed for fast lookups
2. **Foreign Keys**: All foreign key fields should be indexed for join performance
3. **Date Fields**: Transaction date fields should be indexed for reporting
4. **Status Fields**: Active status fields should be indexed for filtering

### Access Patterns
1. **Customer Lookup**: Frequently accessed via CUST-ID
2. **Account Access**: Accessed via ACCT-ID and ACCT-GROUP-ID
3. **Card Validation**: Real-time access via CARD-NUM
4. **Transaction Processing**: High-volume inserts and balance updates
5. **Reporting**: Date-range queries on transaction entities

### Optimization Opportunities
1. **Denormalization**: Consider denormalizing frequently accessed customer/account data
2. **Partitioning**: Partition transaction tables by date for better performance
3. **Archiving**: Implement archiving strategy for old transaction data
4. **Caching**: Cache master data (transaction types, categories) for faster access
