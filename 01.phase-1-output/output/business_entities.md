# Business Entities Analysis - COBOL Legacy System

## Overview
This document contains the extracted business entities from the COBOL codebase analysis, focusing on real-world, business-relevant entities essential for domain modeling and modernization.

## Business Entities

### 1. Customer Entity
**Source**: `CUSTREC.cpy` - Customer Record Structure (RECLN 500)

| Attribute | Data Type | Description |
|-----------|-----------|-------------|
| CUST-ID | PIC 9(09) | Unique customer identifier |
| CUST-FIRST-NAME | PIC X(25) | Customer first name |
| CUST-MIDDLE-NAME | PIC X(25) | Customer middle name |
| CUST-LAST-NAME | PIC X(25) | Customer last name |
| CUST-ADDR-LINE-1 | PIC X(50) | Primary address line |
| CUST-ADDR-LINE-2 | PIC X(50) | Secondary address line |
| CUST-ADDR-LINE-3 | PIC X(50) | Tertiary address line |
| CUST-ADDR-STATE-CD | PIC X(02) | State code |
| CUST-ADDR-COUNTRY-CD | PIC X(03) | Country code |
| CUST-ADDR-ZIP | PIC X(10) | ZIP/postal code |
| CUST-PHONE-NUM-1 | PIC X(15) | Primary phone number |
| CUST-PHONE-NUM-2 | PIC X(15) | Secondary phone number |
| CUST-SSN | PIC 9(09) | Social Security Number |
| CUST-GOVT-ISSUED-ID | PIC X(20) | Government issued ID |
| CUST-DOB-YYYYMMDD | PIC X(10) | Date of birth |
| CUST-EFT-ACCOUNT-ID | PIC X(10) | Electronic funds transfer account |
| CUST-PRI-CARD-HOLDER-IND | PIC X(01) | Primary card holder indicator |
| CUST-FICO-CREDIT-SCORE | PIC 9(03) | FICO credit score |

### 2. Account Entity
**Source**: `CVACT01Y.cpy` - Account Record Structure (RECLN 300)

| Attribute | Data Type | Description |
|-----------|-----------|-------------|
| ACCT-ID | PIC 9(11) | Unique account identifier |
| ACCT-ACTIVE-STATUS | PIC X(01) | Account active status |
| ACCT-CURR-BAL | PIC S9(10)V99 | Current account balance |
| ACCT-CREDIT-LIMIT | PIC S9(10)V99 | Credit limit |
| ACCT-CASH-CREDIT-LIMIT | PIC S9(10)V99 | Cash advance credit limit |
| ACCT-OPEN-DATE | PIC X(10) | Account opening date |
| ACCT-EXPIRAION-DATE | PIC X(10) | Account expiration date |
| ACCT-REISSUE-DATE | PIC X(10) | Account reissue date |
| ACCT-CURR-CYC-CREDIT | PIC S9(10)V99 | Current cycle credit |
| ACCT-CURR-CYC-DEBIT | PIC S9(10)V99 | Current cycle debit |
| ACCT-ADDR-ZIP | PIC X(10) | Account address ZIP code |
| ACCT-GROUP-ID | PIC X(10) | Account group identifier |

### 3. Card Entity
**Source**: `CVACT02Y.cpy` - Card Record Structure (RECLN 150)

| Attribute | Data Type | Description |
|-----------|-----------|-------------|
| CARD-NUM | PIC X(16) | Credit card number |
| CARD-ACCT-ID | PIC 9(11) | Associated account ID |
| CARD-CVV-CD | PIC 9(03) | Card verification value |
| CARD-EMBOSSED-NAME | PIC X(50) | Name embossed on card |
| CARD-EXPIRAION-DATE | PIC X(10) | Card expiration date |
| CARD-ACTIVE-STATUS | PIC X(01) | Card active status |

### 4. Transaction Entity
**Source**: `CVTRA05Y.cpy` - Transaction Record Structure (RECLN 350)

| Attribute | Data Type | Description |
|-----------|-----------|-------------|
| TRAN-ID | PIC X(16) | Unique transaction identifier |
| TRAN-TYPE-CD | PIC X(02) | Transaction type code |
| TRAN-CAT-CD | PIC 9(04) | Transaction category code |
| TRAN-SOURCE | PIC X(10) | Transaction source |
| TRAN-DESC | PIC X(100) | Transaction description |
| TRAN-AMT | PIC S9(09)V99 | Transaction amount |
| TRAN-MERCHANT-ID | PIC 9(09) | Merchant identifier |
| TRAN-MERCHANT-NAME | PIC X(50) | Merchant name |
| TRAN-MERCHANT-CITY | PIC X(50) | Merchant city |
| TRAN-MERCHANT-ZIP | PIC X(10) | Merchant ZIP code |
| TRAN-CARD-NUM | PIC X(16) | Associated card number |
| TRAN-ORIG-TS | PIC X(26) | Original timestamp |
| TRAN-PROC-TS | PIC X(26) | Processing timestamp |

### 5. User Entity
**Source**: `CSUSR01Y.cpy` - Security User Data Structure

| Attribute | Data Type | Description |
|-----------|-----------|-------------|
| SEC-USR-ID | PIC X(08) | User identifier |
| SEC-USR-FNAME | PIC X(20) | User first name |
| SEC-USR-LNAME | PIC X(20) | User last name |
| SEC-USR-PWD | PIC X(08) | User password |
| SEC-USR-TYPE | PIC X(01) | User type (Admin/User) |

### 6. Card Cross-Reference Entity
**Source**: `CVACT03Y.cpy` - Card Cross-Reference Structure (RECLN 50)

| Attribute | Data Type | Description |
|-----------|-----------|-------------|
| XREF-CARD-NUM | PIC X(16) | Card number |
| XREF-CUST-ID | PIC 9(09) | Customer identifier |
| XREF-ACCT-ID | PIC 9(11) | Account identifier |

### 7. Transaction Type Entity
**Source**: `CVTRA03Y.cpy` - Transaction Type Structure (RECLN 60)

| Attribute | Data Type | Description |
|-----------|-----------|-------------|
| TRAN-TYPE | PIC X(02) | Transaction type code |
| TRAN-TYPE-DESC | PIC X(50) | Transaction type description |

### 8. Transaction Category Entity
**Source**: `CVTRA04Y.cpy` - Transaction Category Structure (RECLN 60)

| Attribute | Data Type | Description |
|-----------|-----------|-------------|
| TRAN-TYPE-CD | PIC X(02) | Transaction type code |
| TRAN-CAT-CD | PIC 9(04) | Transaction category code |
| TRAN-CAT-TYPE-DESC | PIC X(50) | Category description |

### 9. Transaction Category Balance Entity
**Source**: `CVTRA01Y.cpy` - Transaction Category Balance Structure (RECLN 50)

| Attribute | Data Type | Description |
|-----------|-----------|-------------|
| TRANCAT-ACCT-ID | PIC 9(11) | Account identifier |
| TRANCAT-TYPE-CD | PIC X(02) | Transaction type code |
| TRANCAT-CD | PIC 9(04) | Transaction category code |
| TRAN-CAT-BAL | PIC S9(09)V99 | Category balance |

### 10. Disclosure Group Entity
**Source**: `CVTRA02Y.cpy` - Disclosure Group Structure (RECLN 50)

| Attribute | Data Type | Description |
|-----------|-----------|-------------|
| DIS-ACCT-GROUP-ID | PIC X(10) | Account group identifier |
| DIS-TRAN-TYPE-CD | PIC X(02) | Transaction type code |
| DIS-TRAN-CAT-CD | PIC 9(04) | Transaction category code |
| DIS-INT-RATE | PIC S9(04)V99 | Interest rate |

## Entity Relationships

### Primary Relationships

1. **Customer ↔ Account** (1:N)
   - One customer can have multiple accounts
   - Linked via CUST-ID in communication area

2. **Account ↔ Card** (1:N)
   - One account can have multiple cards
   - Linked via ACCT-ID (CARD-ACCT-ID → ACCT-ID)

3. **Card ↔ Transaction** (1:N)
   - One card can have multiple transactions
   - Linked via CARD-NUM (TRAN-CARD-NUM → CARD-NUM)

4. **Customer ↔ Card** (1:N via Cross-Reference)
   - One customer can have multiple cards
   - Linked via Card Cross-Reference entity (XREF-CUST-ID → CUST-ID, XREF-CARD-NUM → CARD-NUM)

5. **Account ↔ Transaction Category Balance** (1:N)
   - One account can have multiple category balances
   - Linked via ACCT-ID (TRANCAT-ACCT-ID → ACCT-ID)

6. **Transaction ↔ Transaction Type** (N:1)
   - Multiple transactions can have the same type
   - Linked via TRAN-TYPE-CD

7. **Transaction ↔ Transaction Category** (N:1)
   - Multiple transactions can have the same category
   - Linked via TRAN-CAT-CD

8. **Account ↔ Disclosure Group** (N:1)
   - Multiple accounts can belong to the same disclosure group
   - Linked via ACCT-GROUP-ID (DIS-ACCT-GROUP-ID)

### Cross-Reference Relationships

The Card Cross-Reference entity serves as a junction table that establishes the many-to-many relationship between customers and their cards, while also maintaining the direct relationship to accounts.

## Entity Relationship Diagram

![Entity Relationships](entity_relationships.svg)

## Notes

- All entities exclude UI fields, helper variables (WS-, TEMP-, FILLER, FLAGS), control fields, counters, and COBOL structure-only items
- Focus is on real-world business entities essential for domain modeling and modernization
- Relationships are derived from cross-reference structures and communication areas in the COBOL programs
- Data types follow COBOL PIC clause conventions where S indicates signed numeric, V indicates decimal point, and X indicates alphanumeric
