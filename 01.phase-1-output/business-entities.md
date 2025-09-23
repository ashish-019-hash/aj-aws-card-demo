# Business Entity Catalog - CardDemo COBOL Application

## Overview
This document catalogs the business entities extracted from the CardDemo COBOL application codebase. The analysis follows the Business Entity Playbook methodology, focusing on true business data structures that represent real-world business concepts rather than technical implementation details.

## Entity Summary
- **Total Entities Identified**: 10
- **Master Data Entities**: 3 (Customer, Transaction Type, Transaction Category)
- **Transactional Entities**: 3 (Transaction, Daily Transaction, Transaction Category Balance)
- **Relationship Entities**: 2 (Account, Card Cross-Reference)
- **Configuration Entities**: 2 (Card, Disclosure Group)

---

### ENTITY-001: Customer Record

**Entity Type**: Master
**Description**: Core customer master data containing personal and contact information for cardholders
**Source**: CUSTREC.cpy, lines 1-21

**Business Attributes**:
- Primary Key: CUST-ID (9 digits)
- Core Attributes: Name, address, phone, email, government ID
- Foreign Keys: None (master entity)
- Status Fields: None explicitly defined

**Data Structure**:
| Field Name | Data Type | Description | Key Type |
|------------|-----------|-------------|----------|
| CUST-ID | PIC 9(09) | Customer unique identifier | Primary Key |
| CUST-FIRST-NAME | PIC X(25) | Customer first name | |
| CUST-MIDDLE-NAME | PIC X(25) | Customer middle name | |
| CUST-LAST-NAME | PIC X(25) | Customer last name | |
| CUST-ADDR-LINE-1 | PIC X(50) | Address line 1 | |
| CUST-ADDR-LINE-2 | PIC X(50) | Address line 2 | |
| CUST-ADDR-LINE-3 | PIC X(50) | Address line 3 | |
| CUST-ADDR-STATE-CD | PIC X(02) | State code | |
| CUST-ADDR-COUNTRY-CD | PIC X(03) | Country code | |
| CUST-ADDR-ZIP | PIC X(10) | ZIP/postal code | |
| CUST-PHONE-NUM-1 | PIC X(15) | Primary phone number | |
| CUST-PHONE-NUM-2 | PIC X(15) | Secondary phone number | |
| CUST-SSN | PIC 9(09) | Social Security Number | |
| CUST-GOVT-ISSUED-ID | PIC X(20) | Government issued ID | |
| CUST-DOB-YYYY-MM-DD | PIC X(10) | Date of birth | |
| CUST-EFT-ACCOUNT-ID | PIC X(10) | EFT account identifier | |
| CUST-PRI-CARD-IND | PIC X(01) | Primary card indicator | |
| CUST-FICO-CREDIT-SCORE | PIC 9(03) | FICO credit score | |

**Relationships**:
- Parent: None (top-level master entity)
- Children: Account (1:N via CUST-ID), Card Cross-Reference (1:N via CUST-ID)
- Associates: Card (via Card Cross-Reference)

**Usage Context**:
- Programs: Customer management, account opening, card issuance
- Business Functions: Customer onboarding, KYC compliance, credit assessment

---

### ENTITY-002: Account Record

**Entity Type**: Relationship
**Description**: Financial account entity linking customers to cards and transactions, containing balance and credit limit information
**Source**: CVACT01Y.cpy, lines 1-21

**Business Attributes**:
- Primary Key: ACCT-ID (11 digits)
- Core Attributes: Balance, credit limits, dates, status
- Foreign Keys: ACCT-GROUP-ID (references account grouping)
- Status Fields: ACCT-ACTIVE-STATUS

**Data Structure**:
| Field Name | Data Type | Description | Key Type |
|------------|-----------|-------------|----------|
| ACCT-ID | PIC 9(11) | Account unique identifier | Primary Key |
| ACCT-ACTIVE-STATUS | PIC X(01) | Account active status flag | |
| ACCT-CURR-BAL | PIC S9(10)V99 | Current account balance | |
| ACCT-CREDIT-LIMIT | PIC S9(10)V99 | Credit limit amount | |
| ACCT-CASH-CREDIT-LIMIT | PIC S9(10)V99 | Cash advance credit limit | |
| ACCT-OPEN-DATE | PIC X(10) | Account opening date | |
| ACCT-EXPIRAION-DATE | PIC X(10) | Account expiration date | |
| ACCT-REISSUE-DATE | PIC X(10) | Account reissue date | |
| ACCT-CURR-CYC-CREDIT | PIC S9(10)V99 | Current cycle credit amount | |
| ACCT-CURR-CYC-DEBIT | PIC S9(10)V99 | Current cycle debit amount | |
| ACCT-ADDR-ZIP | PIC X(10) | Account address ZIP code | |
| ACCT-GROUP-ID | PIC X(10) | Account group identifier | Foreign Key |

**Relationships**:
- Parent: Customer (N:1 via CUST-ID in Card Cross-Reference)
- Children: Card (1:N via ACCT-ID), Transaction Category Balance (1:N via ACCT-ID)
- Associates: Transaction (via Card), Disclosure Group (via ACCT-GROUP-ID)

**Usage Context**:
- Programs: Account management, billing, credit limit management
- Business Functions: Account lifecycle management, credit monitoring, billing cycles

---

### ENTITY-003: Card Record

**Entity Type**: Configuration
**Description**: Physical/virtual card entity containing card-specific information and linking to accounts
**Source**: CVACT02Y.cpy, lines 1-15

**Business Attributes**:
- Primary Key: CARD-NUM (16 characters)
- Core Attributes: CVV, embossed name, expiration date, status
- Foreign Keys: CARD-ACCT-ID (references Account)
- Status Fields: CARD-ACTIVE-STATUS

**Data Structure**:
| Field Name | Data Type | Description | Key Type |
|------------|-----------|-------------|----------|
| CARD-NUM | PIC X(16) | Card number | Primary Key |
| CARD-ACCT-ID | PIC 9(11) | Associated account ID | Foreign Key |
| CARD-CVV-CD | PIC 9(03) | Card verification value | |
| CARD-EMBOSSED-NAME | PIC X(50) | Name embossed on card | |
| CARD-EXPIRAION-DATE | PIC X(10) | Card expiration date | |
| CARD-ACTIVE-STATUS | PIC X(01) | Card active status flag | |

**Relationships**:
- Parent: Account (N:1 via CARD-ACCT-ID)
- Children: Transaction (1:N via CARD-NUM)
- Associates: Customer (via Card Cross-Reference), Transaction (direct usage)

**Usage Context**:
- Programs: Card issuance, card management, transaction processing
- Business Functions: Card lifecycle management, fraud prevention, transaction authorization

---

### ENTITY-004: Card Cross-Reference Record

**Entity Type**: Relationship
**Description**: Junction entity establishing many-to-many relationships between customers, accounts, and cards
**Source**: CVACT03Y.cpy, lines 1-12

**Business Attributes**:
- Primary Key: XREF-CARD-NUM (16 characters)
- Core Attributes: Cross-reference mappings
- Foreign Keys: XREF-CUST-ID, XREF-ACCT-ID, XREF-CARD-NUM
- Status Fields: None

**Data Structure**:
| Field Name | Data Type | Description | Key Type |
|------------|-----------|-------------|----------|
| XREF-CARD-NUM | PIC X(16) | Card number reference | Primary Key / Foreign Key |
| XREF-CUST-ID | PIC 9(09) | Customer ID reference | Foreign Key |
| XREF-ACCT-ID | PIC 9(11) | Account ID reference | Foreign Key |

**Relationships**:
- Parent: Customer (N:1 via XREF-CUST-ID), Account (N:1 via XREF-ACCT-ID), Card (1:1 via XREF-CARD-NUM)
- Children: None (junction entity)
- Associates: Enables Customer-Account-Card many-to-many relationships

**Usage Context**:
- Programs: Customer-account-card association management
- Business Functions: Multi-card accounts, authorized user management, account sharing

---

### ENTITY-005: Transaction Record

**Entity Type**: Transactional
**Description**: Core transaction entity capturing all transaction details including merchant information and amounts
**Source**: CVTRA05Y.cpy, lines 1-22

**Business Attributes**:
- Primary Key: TRAN-ID (16 characters)
- Core Attributes: Amount, description, merchant details, timestamps
- Foreign Keys: TRAN-TYPE-CD, TRAN-CAT-CD, TRAN-CARD-NUM
- Status Fields: None explicitly defined

**Data Structure**:
| Field Name | Data Type | Description | Key Type |
|------------|-----------|-------------|----------|
| TRAN-ID | PIC X(16) | Transaction unique identifier | Primary Key |
| TRAN-TYPE-CD | PIC X(02) | Transaction type code | Foreign Key |
| TRAN-CAT-CD | PIC 9(04) | Transaction category code | Foreign Key |
| TRAN-SOURCE | PIC X(10) | Transaction source system | |
| TRAN-DESC | PIC X(100) | Transaction description | |
| TRAN-AMT | PIC S9(09)V99 | Transaction amount | |
| TRAN-MERCHANT-ID | PIC 9(09) | Merchant identifier | |
| TRAN-MERCHANT-NAME | PIC X(50) | Merchant name | |
| TRAN-MERCHANT-CITY | PIC X(50) | Merchant city | |
| TRAN-MERCHANT-ZIP | PIC X(10) | Merchant ZIP code | |
| TRAN-CARD-NUM | PIC X(16) | Card number used | Foreign Key |
| TRAN-ORIG-TS | PIC X(26) | Original transaction timestamp | |
| TRAN-PROC-TS | PIC X(26) | Processing timestamp | |

**Relationships**:
- Parent: Card (N:1 via TRAN-CARD-NUM), Transaction Type (N:1 via TRAN-TYPE-CD), Transaction Category (N:1 via TRAN-CAT-CD)
- Children: None (leaf transactional entity)
- Associates: Account (via Card), Customer (via Card Cross-Reference)

**Usage Context**:
- Programs: Transaction processing, authorization, settlement
- Business Functions: Payment processing, merchant management, transaction reporting

---

### ENTITY-006: Daily Transaction Record

**Entity Type**: Transactional
**Description**: Daily aggregated or processed transaction data with identical structure to main transaction record
**Source**: CVTRA06Y.cpy, lines 1-22

**Business Attributes**:
- Primary Key: DALYTRAN-ID (16 characters)
- Core Attributes: Same as Transaction Record with DALYTRAN prefix
- Foreign Keys: DALYTRAN-TYPE-CD, DALYTRAN-CAT-CD, DALYTRAN-CARD-NUM
- Status Fields: None explicitly defined

**Data Structure**:
| Field Name | Data Type | Description | Key Type |
|------------|-----------|-------------|----------|
| DALYTRAN-ID | PIC X(16) | Daily transaction identifier | Primary Key |
| DALYTRAN-TYPE-CD | PIC X(02) | Transaction type code | Foreign Key |
| DALYTRAN-CAT-CD | PIC 9(04) | Transaction category code | Foreign Key |
| DALYTRAN-SOURCE | PIC X(10) | Transaction source system | |
| DALYTRAN-DESC | PIC X(100) | Transaction description | |
| DALYTRAN-AMT | PIC S9(09)V99 | Transaction amount | |
| DALYTRAN-MERCHANT-ID | PIC 9(09) | Merchant identifier | |
| DALYTRAN-MERCHANT-NAME | PIC X(50) | Merchant name | |
| DALYTRAN-MERCHANT-CITY | PIC X(50) | Merchant city | |
| DALYTRAN-MERCHANT-ZIP | PIC X(10) | Merchant ZIP code | |
| DALYTRAN-CARD-NUM | PIC X(16) | Card number used | Foreign Key |
| DALYTRAN-ORIG-TS | PIC X(26) | Original transaction timestamp | |
| DALYTRAN-PROC-TS | PIC X(26) | Processing timestamp | |

**Relationships**:
- Parent: Card (N:1 via DALYTRAN-CARD-NUM), Transaction Type (N:1 via DALYTRAN-TYPE-CD), Transaction Category (N:1 via DALYTRAN-CAT-CD)
- Children: None (leaf transactional entity)
- Associates: Account (via Card), Customer (via Card Cross-Reference)

**Usage Context**:
- Programs: Daily transaction processing, batch settlement, reporting
- Business Functions: End-of-day processing, daily reconciliation, transaction reporting

---

### ENTITY-007: Transaction Category Balance Record

**Entity Type**: Transactional
**Description**: Account-level balance tracking by transaction category for spending analysis and limits
**Source**: CVTRA01Y.cpy, lines 1-14

**Business Attributes**:
- Primary Key: TRAN-CAT-KEY (composite: TRANCAT-ACCT-ID + TRANCAT-TYPE-CD + TRANCAT-CD)
- Core Attributes: Category balance amount
- Foreign Keys: TRANCAT-ACCT-ID, TRANCAT-TYPE-CD, TRANCAT-CD
- Status Fields: None

**Data Structure**:
| Field Name | Data Type | Description | Key Type |
|------------|-----------|-------------|----------|
| TRANCAT-ACCT-ID | PIC 9(11) | Account identifier | Primary Key / Foreign Key |
| TRANCAT-TYPE-CD | PIC X(02) | Transaction type code | Primary Key / Foreign Key |
| TRANCAT-CD | PIC 9(04) | Transaction category code | Primary Key / Foreign Key |
| TRAN-CAT-BAL | PIC S9(09)V99 | Category balance amount | |

**Relationships**:
- Parent: Account (N:1 via TRANCAT-ACCT-ID), Transaction Type (N:1 via TRANCAT-TYPE-CD), Transaction Category (N:1 via TRANCAT-CD)
- Children: None (aggregated transactional entity)
- Associates: Transaction (via category codes)

**Usage Context**:
- Programs: Balance tracking, spending analysis, category limit enforcement
- Business Functions: Spending category management, budget tracking, financial reporting

---

### ENTITY-008: Transaction Type Record

**Entity Type**: Master
**Description**: Master data entity defining transaction types with codes and descriptions
**Source**: CVTRA03Y.cpy, lines 1-11

**Business Attributes**:
- Primary Key: TRAN-TYPE (2 characters)
- Core Attributes: Type code and description
- Foreign Keys: None (master reference data)
- Status Fields: None

**Data Structure**:
| Field Name | Data Type | Description | Key Type |
|------------|-----------|-------------|----------|
| TRAN-TYPE | PIC X(02) | Transaction type code | Primary Key |
| TRAN-TYPE-DESC | PIC X(50) | Transaction type description | |

**Relationships**:
- Parent: None (master reference entity)
- Children: Transaction (1:N via TRAN-TYPE-CD), Daily Transaction (1:N via DALYTRAN-TYPE-CD), Transaction Category Balance (1:N via TRANCAT-TYPE-CD), Transaction Category (1:N via TRAN-TYPE-CD)
- Associates: All transaction-related entities

**Usage Context**:
- Programs: Transaction classification, validation, reporting
- Business Functions: Transaction categorization, business rule enforcement, regulatory reporting

---

### ENTITY-009: Transaction Category Record

**Entity Type**: Master
**Description**: Master data entity defining transaction categories within transaction types for detailed classification
**Source**: CVTRA04Y.cpy, lines 1-13

**Business Attributes**:
- Primary Key: TRAN-CAT-KEY (composite: TRAN-TYPE-CD + TRAN-CAT-CD)
- Core Attributes: Category code and description
- Foreign Keys: TRAN-TYPE-CD (references Transaction Type)
- Status Fields: None

**Data Structure**:
| Field Name | Data Type | Description | Key Type |
|------------|-----------|-------------|----------|
| TRAN-TYPE-CD | PIC X(02) | Transaction type code | Primary Key / Foreign Key |
| TRAN-CAT-CD | PIC 9(04) | Transaction category code | Primary Key |
| TRAN-CAT-TYPE-DESC | PIC X(50) | Category description | |

**Relationships**:
- Parent: Transaction Type (N:1 via TRAN-TYPE-CD)
- Children: Transaction (1:N via TRAN-CAT-CD), Daily Transaction (1:N via DALYTRAN-CAT-CD), Transaction Category Balance (1:N via TRANCAT-CD)
- Associates: All transaction-related entities

**Usage Context**:
- Programs: Transaction sub-classification, detailed reporting, spending analysis
- Business Functions: Granular transaction categorization, merchant category codes, spending pattern analysis

---

### ENTITY-010: Disclosure Group Record

**Entity Type**: Configuration
**Description**: Configuration entity defining interest rates and disclosure information by account group and transaction category
**Source**: CVTRA02Y.cpy, lines 1-14

**Business Attributes**:
- Primary Key: DIS-GROUP-KEY (composite: DIS-ACCT-GROUP-ID + DIS-TRAN-TYPE-CD + DIS-TRAN-CAT-CD)
- Core Attributes: Interest rate
- Foreign Keys: DIS-ACCT-GROUP-ID, DIS-TRAN-TYPE-CD, DIS-TRAN-CAT-CD
- Status Fields: None

**Data Structure**:
| Field Name | Data Type | Description | Key Type |
|------------|-----------|-------------|----------|
| DIS-ACCT-GROUP-ID | PIC X(10) | Account group identifier | Primary Key / Foreign Key |
| DIS-TRAN-TYPE-CD | PIC X(02) | Transaction type code | Primary Key / Foreign Key |
| DIS-TRAN-CAT-CD | PIC 9(04) | Transaction category code | Primary Key / Foreign Key |
| DIS-INT-RATE | PIC S9(04)V99 | Interest rate percentage | |

**Relationships**:
- Parent: Account Group (N:1 via DIS-ACCT-GROUP-ID), Transaction Type (N:1 via DIS-TRAN-TYPE-CD), Transaction Category (N:1 via DIS-TRAN-CAT-CD)
- Children: None (configuration entity)
- Associates: Account (via ACCT-GROUP-ID)

**Usage Context**:
- Programs: Interest calculation, disclosure generation, rate management
- Business Functions: Interest rate management, regulatory compliance, customer disclosures

---

## Entity Relationship Summary

### Primary Relationships
1. **Customer → Account → Card → Transaction**: Core hierarchical flow
2. **Transaction Type → Transaction Category**: Master data hierarchy
3. **Account → Transaction Category Balance**: Balance aggregation by category
4. **Card Cross-Reference**: Enables many-to-many Customer-Account-Card relationships

### Cardinality Mappings
- Customer (1) : Account (N)
- Account (1) : Card (N) 
- Card (1) : Transaction (N)
- Transaction Type (1) : Transaction Category (N)
- Transaction Type (1) : Transaction (N)
- Transaction Category (1) : Transaction (N)
- Account (1) : Transaction Category Balance (N)
- Account Group (1) : Disclosure Group (N)

### Business Process Flow
1. **Customer Onboarding**: Customer → Account → Card issuance
2. **Transaction Processing**: Card → Transaction → Category Balance update
3. **Interest Calculation**: Account Group + Transaction Category → Disclosure Group rates
4. **Reporting**: All entities contribute to comprehensive transaction and balance reporting

## Architecture Notes
- **File-based System**: No DCLGEN database structures found; uses VSAM file organization
- **Master-Detail Pattern**: Clear separation of master data (Customer, Transaction Type) and transactional data
- **Audit Trail**: Transaction timestamps support audit and reconciliation requirements
- **Flexible Categorization**: Two-level transaction classification (Type → Category) supports detailed analysis
- **Multi-tenancy Support**: Account grouping enables different rate structures and configurations
