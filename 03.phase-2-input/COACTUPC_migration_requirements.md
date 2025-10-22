# COACTUPC Account Update - Migration Requirements

**Program:** COACTUPC | **Transaction:** CAUP | **Map:** CACTUPA  
**Source:** 00.phase-1-input/app/cbl/COACTUPC.cbl (4237 lines)

## 1. Frontend Requirements

### 1.1 Screen Layout
- **Title:** ACCOUNT UPDATE  
- **Header:** Transaction ID, Program Name, Date (MM/DD/YY), Time (HH:MM:SS)  
- **Account Section (Lines 96-234):** 11 fields at rows 6-12  
- **Customer Section (Lines 236-478):** 24 fields at rows 14-20  
- **Messages:** Info (row 22), Error (row 23, red)  
- **Function Keys (Lines 493-507):** ENTER, F3 (Exit), F5 (Save-conditional), F12 (Cancel-conditional)

### 1.2 Field Details

| Field | Label | Type | Len | Editable | Source | Validation | Format |
|-------|-------|------|-----|----------|--------|------------|--------|
| ACCTSID | Account Number | Numeric | 11 | Initial only | Account | Required, 11 digits, non-zero | 99999999999 |
| ACSTTUS | Status | Alpha | 1 | Yes | Account | Y or N | Y/N |
| OPNYEAR/MON/DAY | Open Date | Numeric | 4/2/2 | Yes | Account | 1900-2099, valid date | YYYY/MM/DD |
| ACRDLIM | Credit Limit | Currency | 13 | Yes | Account | Required, signed decimal | -,---,---,--9.99 |
| EXPYEAR/MON/DAY | Expiry Date | Numeric | 4/2/2 | Yes | Account | 1900-2099, valid date | YYYY/MM/DD |
| ACSHLIM | Cash Credit Limit | Currency | 13 | Yes | Account | Required, signed decimal | -,---,---,--9.99 |
| RISYEAR/MON/DAY | Reissue Date | Numeric | 4/2/2 | Yes | Account | 1900-2099, valid date | YYYY/MM/DD |
| ACURBAL | Current Balance | Currency | 13 | Yes | Account | Required, signed decimal | -,---,---,--9.99 |
| ACRCYCR | Cycle Credit | Currency | 13 | Yes | Account | Required, signed decimal | -,---,---,--9.99 |
| ACRCYDB | Cycle Debit | Currency | 13 | Yes | Account | Required, signed decimal | -,---,---,--9.99 |
| AADDGRP | Account Group | Alphanumeric | 10 | Yes | Account | Optional | Text |
| ACSTNUM | Customer ID | Numeric | 9 | No | Customer | Display only (from xref) | 999999999 |
| ACTSSN1/2/3 | SSN Parts | Numeric | 3/2/4 | Yes | Customer | Required, 3-part, not 000/666/900-999 | 999-99-9999 |
| DOBYEAR/MON/DAY | DOB | Numeric | 4/2/2 | Yes | Customer | 1900-2099, not future | YYYY/MM/DD |
| ACSTFCO | FICO Score | Numeric | 3 | Yes | Customer | Required, 300-850 | 999 |
| ACSFNAM | First Name | Alpha | 25 | Yes | Customer | Required, alpha only | Text |
| ACSMNAM | Middle Name | Alpha | 25 | Yes | Customer | Optional, alpha only | Text |
| ACSLNAM | Last Name | Alpha | 25 | Yes | Customer | Required, alpha only | Text |
| ACSADL1 | Address Line 1 | Alphanumeric | 50 | Yes | Customer | Required | Text |
| ACSADL2 | Address Line 2 | Alphanumeric | 50 | Yes | Customer | Optional | Text |
| ACSCITY | City | Alphanumeric | 50 | Yes | Customer | Optional | Text |
| ACSSTTE | State | Alpha | 2 | Yes | Customer | Required, valid US state | XX |
| ACSZIPC | Zip Code | Alphanumeric | 10 | Yes | Customer | Required, valid for state | 99999-9999 |
| ACSCTRY | Country | Alpha | 3 | No | Customer | Fixed to USA | XXX |
| ACSPH1A/B/C | Phone 1 | Numeric | 3/3/4 | Yes | Customer | Optional (all or none), valid area code | (999)999-9999 |
| ACSPH2A/B/C | Phone 2 | Numeric | 3/3/4 | Yes | Customer | Optional (all or none) | (999)999-9999 |
| ACSEFTC | EFT Account | Alphanumeric | 10 | Yes | Customer | Required, 10 digits, non-zero | 9999999999 |
| ACSGOVT | Government ID | Alphanumeric | 20 | Yes | Customer | Optional | Text |
| ACSPFLG | Primary Holder | Alpha | 1 | Yes | Customer | Required, Y or N | Y/N |

**Line References:** Field receiving (1047-1427), Display logic (2698-2950), Attribute setup (3200-3580)

### 1.3 Client-Side Validation

**Account Number (Lines 1783-1818):**
```javascript
if (!accountNumber || !isNumeric(accountNumber) || accountNumber.length !== 11 || parseInt(accountNumber) === 0)
  return {valid: false, error: 'Account Number must be an 11 digit Non-Zero Number'};
```

**Status (Lines 1472-1476):** Must be 'Y' or 'N'

**Date (Lines 1478-1507):**
```javascript
if (year < 1900 || year > 2099 || month < 1 || month > 12 || !isValidDay(day, month, year))
  return {valid: false, error: '{fieldName}: Invalid date'};
```

**DOB Additional Check:** Cannot be in future

**SSN (Lines 2431-2490):**
```javascript
// Part 1: 3 digits, not 000/666/900-999
// Part 2: 2 digits, not 00
// Part 3: 4 digits, not 0000
```

**Phone (Lines 2300-2427):**
```javascript
// If any part provided, all must be provided
// Area code must be valid US area code
// Prefix cannot be 000
// Line cannot be 0000
```

**FICO (Lines 2514-2533):** 300-850 range

**Names (Lines 1969-2059):** First/Last required (alpha only), Middle optional (alpha only)

**State/Zip Cross-Validation (Lines 2536-2560):** Zip prefix must be valid for state

### 1.4 UI State Management

**States (Lines 35-44):**
1. **ACUP-DETAILS-NOT-FETCHED:** Initial, account number entry
2. **ACUP-SHOW-DETAILS:** Data displayed, editable
3. **ACUP-CHANGES-MADE:** User edited fields
4. **ACUP-CHANGES-NOT-OK:** Validation errors
5. **ACUP-CHANGES-OK-NOT-CONFIRMED:** Valid, awaiting F5
6. **ACUP-CHANGES-OKAYED-AND-DONE:** Update succeeded
7. **ACUP-CHANGES-FAILED:** Update failed
8. **ACUP-CHANGES-OKAYED-LOCK-ERROR:** Lock failure
9. **ACUP-CHANGES-OKAYED-BUT-FAILED:** Concurrent update detected

**State Transitions:**
```
Initial → NOT-FETCHED → [ENTER] → SHOW-DETAILS → [Edit+ENTER] → 
  CHANGES-MADE → [Validate] → {CHANGES-NOT-OK | CHANGES-OK-NOT-CONFIRMED} →
  [F5] → {OKAYED-AND-DONE | LOCK-ERROR | FAILED | Concurrent}
```

**Button Behaviors:**
- **ENTER:** Always enabled, action depends on state
- **F3 (Exit):** Always enabled, returns to caller
- **F5 (Save):** Enabled only in CHANGES-OK-NOT-CONFIRMED state
- **F12 (Cancel):** Enabled after data fetched

### 1.5 Data Formatting
- **Dates:** Display YYYY / MM / DD, Store YYYY-MM-DD
- **Phone:** Display (999)999-9999, Store (999)999-9999
- **SSN:** Display 999-99-9999, Store 999999999 (numeric)
- **Currency:** Display -,---,---,--9.99, Store S9(10)V99

---

## 2. Backend Requirements

### 2.1 API Endpoints

**GET /api/accounts/{accountId}**
- Fetch account and customer data
- Returns: account object, customer object, version info
- Errors: 404 (not found), 500 (database error)
- Implementation: Lines 3608-3800

**PUT /api/accounts/{accountId}**
- Update account and customer with optimistic locking
- Request: account, customer, oldValues
- Returns: success message
- Errors: 400 (validation), 409 (concurrent update/lock), 500 (update failed)
- Implementation: Lines 3870-4107

**GET /api/navigation/context**
- Retrieve session navigation context
- Returns: user info, navigation breadcrumb
- Implementation: Lines 880-893

### 2.2 Business Logic (Chronological Execution Order)

**Step 1: Program Initialization (Lines 858-919)**
```
1. Setup abend handler (862-864)
2. Initialize working storage (866-868)
3. Restore COMMAREA session state (880-893):
   - If EIBCALEN=0: Fresh entry, initialize all
   - Else: Restore navigation and program state
4. Process PF key mapping (898-899)
5. Validate AID key (905-916): ENTER/PF03/PF05/PF12
```

**Step 2: Main Decision Router (Lines 921-1004)**
```
EVALUATE current_state:
  WHEN PF03: Exit to calling program (927-959)
  WHEN Fresh entry: Send initial screen (964-973)
  WHEN Update complete: Reset for next account (979-989)
  WHEN Other: Process inputs (996-1003)
```

**Step 3: Input Processing (Lines 1025-1427)**
```
1. Receive map data from screen (1039-1045): EXEC CICS RECEIVE MAP
2. Copy all fields from screen to working storage (1047-1427):
   - Account fields (1048-1218): Handle '*' or SPACES as LOW-VALUES
   - Customer fields (1220-1424): Handle '*' or SPACES as LOW-VALUES
```

**Step 4: Validation Orchestration (Lines 1429-1679)**
```
IF state=DETAILS-NOT-FETCHED (1433-1446):
  Validate account number only (1435-1436)
ELSE data already fetched (1447-1676):
  1. Compare old vs new (1460-1461) → Lines 1681-1779
  2. If no changes: Exit
  3. Set CHANGES-NOT-OK state (1470)
  4. Validate ALL fields in order (1472-1662):
     - Account Status (Y/N)
     - Open Date, Credit Limit, Expiry Date
     - Cash Credit Limit, Reissue Date, Balance
     - Cycle Credit, Cycle Debit
     - SSN (3 parts), DOB, FICO
     - First/Middle/Last Names
     - Address, State, Zip
     - Phone 1, Phone 2
     - Government ID, EFT, Primary Holder
  5. Cross-validate State/Zip (1665-1669)
  6. If all valid: Set CHANGES-OK-NOT-CONFIRMED (1674)
```

**Step 5: Change Detection (Lines 1681-1779)**
```
Compare ALL fields (case-insensitive for text, exact for numeric):
  - Account: ID, Status, Balance, Limits, Dates, Cycles, Group
  - Customer: ID, Names, Address, State, Zip, Phones, SSN, DOB, FICO, EFT, Holder
If any mismatch: Set CHANGE-HAS-OCCURRED
Else: Set NO-CHANGES-DETECTED
```

**Step 6: Action Decision (Lines 2562-2645)**
```
EVALUATE state:
  WHEN DETAILS-NOT-FETCHED: Prepare to fetch
  WHEN PF12: Cancel, reload original (2572-2580)
  WHEN SHOW-DETAILS: Check for changes/errors (2585-2591)
  WHEN CHANGES-NOT-OK: Stay in error state (2596-2597)
  WHEN CHANGES-OK + PF05: Perform update (2602-2615)
  WHEN CHANGES-OKAYED-AND-DONE: Reset for next (2625-2632)
```

**Step 7: Data Fetch Sequence (Lines 3608-3800)**
```
1. Read CARDDAT by account ID via CARDAIX (3654-3662) → Get customer ID
2. Read ACCTDAT by account ID (3703-3711) → Get account data
3. Read CUSTDAT by customer ID (3753-3761) → Get customer data
4. Store in ACUP-OLD-DETAILS (9500-STORE-FETCHED-DATA)
Errors: 404 if not found, 500 if read error
```

**Step 8: Update Processing with Optimistic Locking (Lines 3870-4107)**
```
1. Lock account: READ ACCTDAT with UPDATE (3894-3903)
   If fail: Set COULD-NOT-LOCK-ACCT, return
2. Lock customer: READ CUSTDAT with UPDATE (3921-3930)
   If fail: Set COULD-NOT-LOCK-CUST, return
3. Optimistic lock check (3946-3947) → Lines 4109-4195:
   Compare locked records with old values
   If mismatch: Set DATA-WAS-CHANGED-BEFORE-UPDATE, return
4. Build account update record (3963-4002):
   - Assemble dates: STRING year '-' month '-' day
5. Build customer update record (4007-4059):
   - Assemble phones: STRING '(' area ')' prefix '-' line
   - Assemble DOB: STRING year '-' month '-' day
6. REWRITE ACCTDAT (4065-4081)
   If fail: Set LOCKED-BUT-UPDATE-FAILED, return
7. REWRITE CUSTDAT (4085-4103)
   If fail: SYNCPOINT ROLLBACK, Set LOCKED-BUT-UPDATE-FAILED, return
8. Success: Locks released on next SYNCPOINT
```

**Step 9: Screen Display (Lines 2649-3605)**
```
1. Initialize screen header (3100-SCREEN-INIT): Title, transaction, date, time
2. Setup screen variables (3200-SETUP-SCREEN-VARS):
   EVALUATE state:
     WHEN DETAILS-NOT-FETCHED: Show blank fields
     WHEN SHOW-DETAILS: Show original values
     WHEN CHANGES-MADE: Show updated values
3. Setup info message (3250-SETUP-INFOMSG)
4. Setup field attributes (3300-SETUP-SCREEN-ATTRS): Error highlighting
5. Setup function key attributes (3390-SETUP-INFOMSG-ATTRS): Enable F5/F12
6. Send map to terminal (3400-SEND-SCREEN): EXEC CICS SEND MAP
```

**Step 10: Program Termination (Lines 1007-1020)**
```
1. Store error message in COMMAREA
2. Merge navigation and program state into WS-COMMAREA
3. EXEC CICS RETURN TRANSID('CAUP') COMMAREA(WS-COMMAREA)
   → Pseudo-conversational: program terminates, state preserved
```

### 2.3 Data Transformations

**Date Assembly (Lines 3983-4000):**
```sql
STRING year '-' month '-' day → 'YYYY-MM-DD'
```

**Phone Assembly (Lines 4027-4041):**
```sql
STRING '(' area ')' prefix '-' line → '(999)999-9999'
```

**SSN Assembly (Line 4044):**
```sql
9-digit numeric from 3 parts
```

### 2.4 Transaction Integrity

**Two-Phase Commit (Lines 4065-4103):**
1. REWRITE ACCTDAT → If fail: return error
2. REWRITE CUSTDAT → If fail: SYNCPOINT ROLLBACK, return error
3. Success: Both committed atomically

**Atomic Rule:** Either BOTH account AND customer updated, OR NEITHER

### 2.5 Concurrency Control

**Optimistic Locking (Lines 4109-4195):**
1. Fetch: Store old values in ACUP-OLD-DETAILS
2. Lock: READ with UPDATE (gets current values)
3. Compare: locked values vs old values
4. If match: Proceed with REWRITE
5. If mismatch: Reject with CONCURRENT_UPDATE error, return current data

---

## 3. Database Requirements (Repository Pattern)

### 3.1 Card Cross-Reference Repository

**Purpose:** Links cards to customers and accounts (many-to-many)  
**Source:** CVACT03Y.cpy  
**Access:** READ via alternate index CARDAIX by account_id (Line 3654)

```sql
CREATE TABLE card_cross_reference (
    card_number VARCHAR(16) PRIMARY KEY,
    customer_id INTEGER NOT NULL,
    account_id VARCHAR(11) NOT NULL,
    created_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (customer_id) REFERENCES customers(customer_id),
    FOREIGN KEY (account_id) REFERENCES accounts(account_id),
    CHECK (card_number ~ '^[0-9]{16}$'),
    CHECK (customer_id > 0),
    CHECK (account_id ~ '^[0-9]{11}$')
);

CREATE INDEX idx_card_xref_account_id ON card_cross_reference(account_id);
CREATE INDEX idx_card_xref_customer_id ON card_cross_reference(customer_id);
```

**Repository Interface:**
```typescript
interface CardCrossReferenceRepository {
    findByAccountId(accountId: string): Promise<CardCrossReference | null>;
    findByCardNumber(cardNumber: string): Promise<CardCrossReference | null>;
    findByCustomerId(customerId: number): Promise<CardCrossReference[]>;
}
```

### 3.2 Account Repository

**Purpose:** Account master data (limits, balances, dates)  
**Source:** CVACT01Y.cpy  
**Access:** READ by account_id (Line 3703), READ with UPDATE (Line 3894), REWRITE (Line 4066)

```sql
CREATE TABLE accounts (
    account_id VARCHAR(11) PRIMARY KEY,
    active_status CHAR(1) NOT NULL DEFAULT 'Y',
    current_balance DECIMAL(12,2) NOT NULL DEFAULT 0.00,
    credit_limit DECIMAL(12,2) NOT NULL,
    cash_credit_limit DECIMAL(12,2) NOT NULL,
    open_date DATE NOT NULL,
    expiration_date DATE NOT NULL,
    reissue_date DATE,
    current_cycle_credit DECIMAL(12,2) NOT NULL DEFAULT 0.00,
    current_cycle_debit DECIMAL(12,2) NOT NULL DEFAULT 0.00,
    account_group_id VARCHAR(10),
    zip_code VARCHAR(10),
    created_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    version INTEGER NOT NULL DEFAULT 0,
    CHECK (account_id ~ '^[0-9]{11}$'),
    CHECK (active_status IN ('Y', 'N')),
    CHECK (open_date >= '1900-01-01'),
    CHECK (expiration_date >= open_date)
);

CREATE INDEX idx_accounts_active ON accounts(active_status);
CREATE INDEX idx_accounts_group ON accounts(account_group_id);
```

**Repository Interface:**
```typescript
interface AccountRepository {
    findById(accountId: string): Promise<Account | null>;
    update(accountId: string, data: Partial<Account>): Promise<void>;
    lockForUpdate(accountId: string): Promise<Account>;
}
```

### 3.3 Customer Repository

**Purpose:** Customer master data (personal, address, contact)  
**Source:** CVCUS01Y.cpy  
**Access:** READ by customer_id (Line 3753), READ with UPDATE (Line 3921), REWRITE (Line 4086)

```sql
CREATE TABLE customers (
    customer_id INTEGER PRIMARY KEY,
    first_name VARCHAR(25) NOT NULL,
    middle_name VARCHAR(25),
    last_name VARCHAR(25) NOT NULL,
    address_line1 VARCHAR(50),
    address_line2 VARCHAR(50),
    address_line3 VARCHAR(50),
    state_code CHAR(2),
    country_code CHAR(3),
    zip_code VARCHAR(10),
    phone_number1 VARCHAR(15),
    phone_number2 VARCHAR(15),
    ssn INTEGER,
    govt_issued_id VARCHAR(20),
    date_of_birth DATE,
    eft_account_id VARCHAR(10),
    primary_card_holder_ind CHAR(1),
    fico_credit_score SMALLINT,
    created_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    version INTEGER NOT NULL DEFAULT 0,
    CHECK (customer_id > 0),
    CHECK (fico_credit_score BETWEEN 300 AND 850),
    CHECK (primary_card_holder_ind IN ('Y', 'N')),
    CHECK (state_code ~ '^[A-Z]{2}$'),
    CHECK (date_of_birth >= '1900-01-01' AND date_of_birth <= CURRENT_DATE)
);

CREATE INDEX idx_customers_name ON customers(last_name, first_name);
CREATE INDEX idx_customers_ssn ON customers(ssn);
```

**Repository Interface:**
```typescript
interface CustomerRepository {
    findById(customerId: number): Promise<Customer | null>;
    update(customerId: number, data: Partial<Customer>): Promise<void>;
    lockForUpdate(customerId: number): Promise<Customer>;
}
```

### 3.4 Entity Relationships

```
Card Cross-Reference (Junction Table)
    ├── Many-to-One → Customer
    └── Many-to-One → Account

Account (Independent Entity)
    └── One-to-Many ← Card Cross-Reference

Customer (Independent Entity)
    └── One-to-Many ← Card Cross-Reference
```

**Relationship Notes:**
- One account can have many cards (via card_cross_reference)
- One customer can have many cards (via card_cross_reference)
- One card belongs to one customer and one account
- Account and Customer are linked through card_cross_reference (many-to-many)

---

## 4. Integration & Session Management

### 4.1 COMMAREA Structure (Lines 880-893)

**Navigation Context:**
- `CDEMO-FROM-PROGRAM`: Calling program name
- `CDEMO-FROM-TRANID`: Calling transaction ID
- `CDEMO-TO-PROGRAM`: Destination program name
- `CDEMO-TO-TRANID`: Destination transaction ID
- `CDEMO-LAST-MAPSET`: Current mapset (COACTUP)
- `CDEMO-LAST-MAP`: Current map (CACTUPA)

**Program-Specific State:**
- `CDEMO-ACCT-ID`: Current account ID
- `CDEMO-CUST-ID`: Associated customer ID
- `CDEMO-CARD-NUM`: Card number
- `ACUP-OLD-DETAILS`: Original fetched data
- `ACUP-NEW-DETAILS`: Modified data
- State flags (35-44): Current UI state

### 4.2 Entry Points

**Fresh Entry (EIBCALEN=0):**
- From main menu (COMEN01C)
- Initialize all context
- Display blank screen

**Re-entry (COMMAREA passed):**
- From another transaction
- Restore session state
- May have pre-filled account ID

### 4.3 Exit Points

**F3 Exit (Lines 927-959):**
```
1. Determine destination: CDEMO-FROM-PROGRAM or COMEN01C
2. Update navigation context
3. EXEC CICS SYNCPOINT
4. EXEC CICS XCTL to destination
```

**Normal Completion:**
- Reset state to DETAILS-NOT-FETCHED
- Prompt for next account
- User can F3 to exit

### 4.4 Pseudo-Conversational Pattern (Lines 1015-1019)

```
EXEC CICS RETURN TRANSID('CAUP') COMMAREA(WS-COMMAREA)
```
- Program terminates after each user interaction
- State preserved in COMMAREA
- Next interaction starts new program instance
- Efficient resource usage (no long-running processes)

---

## 5. Error Handling

### 5.1 Error Scenarios

**Record Not Found (Lines 3668-3685):**
```json
{"error": "NOT_FOUND", "message": "Account not found in Cross ref file"}
```

**Validation Errors (Lines 1429-1679):**
```json
{"error": "VALIDATION_ERROR", "fields": {"creditLimit": "must be supplied"}}
```

**Concurrent Update (Lines 4109-4195):**
```json
{"error": "CONCURRENT_UPDATE", "message": "Data modified by another user", "currentData": {...}}
```

**Record Locked (Lines 3905-3915):**
```json
{"error": "RECORD_LOCKED", "message": "Account being updated by another user"}
```

**Update Failed (Lines 4095-4103):**
```json
{"error": "UPDATE_FAILED", "message": "Customer update failed. Account update rolled back"}
```

### 5.2 Error Message Display

- **Error field highlighting:** Red color (DFHRED), asterisk if blank (Lines 3207-3450)
- **Error message area:** Row 23, 78 characters, bright red (Line 489)
- **Cursor positioning:** First error field
- **Info messages:** Row 22, neutral color (Line 480)

---

## 6. Verification Checklist

✅ **Frontend Team:** Can build UI without COBOL code?
- Complete screen layout with field positions
- All validation rules with examples
- UI state machine with transitions
- Data formatting specifications
- Button behaviors and navigation flow

✅ **Backend Team:** Can implement business logic without COBOL code?
- API endpoints with request/response formats
- Complete chronological business flow (10 steps)
- All validation rules for server-side
- Data transformation logic with examples
- Transaction integrity patterns
- Concurrency control implementation

✅ **Database Team:** Can design schema without COBOL code?
- Repository pattern for 3 entities
- Complete SQL DDL with constraints
- Entity relationships diagram
- Access patterns and indexes
- Optimistic locking support (version column)

✅ **Integration Team:** Understand communication patterns?
- COMMAREA structure and contents
- Entry/exit points with navigation
- Pseudo-conversational pattern
- Session state preservation
- Inter-module communication flow

---

## 7. Key Line Number References

- **Initialization:** 858-919
- **Main Router:** 921-1004
- **Input Processing:** 1025-1427
- **Validation:** 1429-1679, 1783-2560
- **Change Detection:** 1681-1779
- **Action Decision:** 2562-2645
- **Screen Display:** 2649-3605
- **Data Fetch:** 3608-3800 (CARDDAT: 3654, ACCTDAT: 3703, CUSTDAT: 3753)
- **Update Processing:** 3870-4107
- **Optimistic Lock Check:** 4109-4195
- **Termination:** 1007-1020

---

**Document Version:** 1.0  
**Generated:** Based on COACTUPC.cbl analysis following program-extract-summary-prompt.md methodology  
**Completeness:** All teams can implement independently without COBOL source consultation
