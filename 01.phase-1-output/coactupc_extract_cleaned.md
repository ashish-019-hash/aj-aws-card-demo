# COACTUPC - Account Update Program - Cleaned Documentation

## Program Identification

**Program Name:** COACTUPC  
**Transaction ID:** CAUP  
**Program Type:** Online Transaction - Account Update  
**Source File:** COACTUPC.cbl

---

## Business Purpose

The COACTUPC program provides online account update functionality for the CardDemo credit card management application. The program allows authorized users to view and modify both account-level and customer-level information for existing credit card accounts.

**Primary Functions:**
1. **Account Lookup** - Accept account number and retrieve associated account and customer data
2. **Data Display** - Display all account financial information and customer demographic information
3. **Data Validation** - Validate all user modifications against business rules
4. **Data Update** - Save validated changes to both account and customer master files with transaction integrity
5. **Concurrency Control** - Detect and prevent concurrent updates by multiple users

**Key Business Rules:**
- Account and customer data must be updated atomically (both or neither)
- Concurrent updates are detected and rejected (optimistic locking)
- All field modifications are validated before save
- Changes require explicit confirmation (save or cancel)
- Navigation context is preserved for return to calling program

---

## Data Structures

### Account Master Record

```cobol
01  ACCOUNT-RECORD.
    05  ACCT-ID                     PIC X(11).
    05  ACCT-ACTIVE-STATUS          PIC X(01).
    05  ACCT-CURR-BAL               PIC S9(10)V99 COMP-3.
    05  ACCT-CREDIT-LIMIT           PIC S9(10)V99 COMP-3.
    05  ACCT-CASH-CREDIT-LIMIT      PIC S9(10)V99 COMP-3.
    05  ACCT-OPEN-DATE              PIC X(10).
    05  ACCT-EXPIRAION-DATE         PIC X(10).
    05  ACCT-REISSUE-DATE           PIC X(10).
    05  ACCT-CURR-CYC-CREDIT        PIC S9(10)V99 COMP-3.
    05  ACCT-CURR-CYC-DEBIT         PIC S9(10)V99 COMP-3.
    05  ACCT-GROUP-ID               PIC X(10).
    05  FILLER                      PIC X(178).
```

**Purpose:** Stores account financial information including balances, limits, and important dates. Packed decimal fields (COMP-3) optimize storage for monetary values.

**Key Fields:**
- `ACCT-ID` - 11-character account identifier (primary key)
- `ACCT-ACTIVE-STATUS` - Account status ('Y'=Active, 'N'=Inactive)
- `ACCT-CURR-BAL` - Current balance (signed, 2 decimal places)
- `ACCT-CREDIT-LIMIT` - Credit limit (signed, 2 decimal places)
- `ACCT-CASH-CREDIT-LIMIT` - Cash advance limit (signed, 2 decimal places)
- `ACCT-OPEN-DATE` - Account open date (YYYY-MM-DD format)
- `ACCT-EXPIRAION-DATE` - Card expiration date (YYYY-MM-DD format)
- `ACCT-REISSUE-DATE` - Card reissue date (YYYY-MM-DD format)
- `ACCT-CURR-CYC-CREDIT` - Current cycle credits (signed, 2 decimal places)
- `ACCT-CURR-CYC-DEBIT` - Current cycle debits (signed, 2 decimal places)
- `ACCT-GROUP-ID` - Account group identifier

### Customer Master Record

```cobol
01  CUSTOMER-RECORD.
    05  CUST-ID                     PIC 9(09) COMP.
    05  CUST-FIRST-NAME             PIC X(25).
    05  CUST-MIDDLE-NAME            PIC X(25).
    05  CUST-LAST-NAME              PIC X(25).
    05  CUST-ADDR-LINE-1            PIC X(50).
    05  CUST-ADDR-LINE-2            PIC X(50).
    05  CUST-ADDR-LINE-3            PIC X(50).
    05  CUST-ADDR-STATE-CD          PIC X(02).
    05  CUST-ADDR-COUNTRY-CD        PIC X(03).
    05  CUST-ADDR-ZIP               PIC X(10).
    05  CUST-PHONE-NUM-1            PIC X(15).
    05  CUST-PHONE-NUM-2            PIC X(15).
    05  CUST-SSN                    PIC 9(09) COMP.
    05  CUST-GOVT-ISSUED-ID         PIC X(20).
    05  CUST-DOB-YYYY-MM-DD         PIC X(10).
    05  CUST-EFT-ACCOUNT-ID         PIC X(10).
    05  CUST-PRI-CARD-HOLDER-IND    PIC X(01).
    05  CUST-FICO-CREDIT-SCORE      PIC 9(03) COMP.
    05  FILLER                      PIC X(168).
```

**Purpose:** Stores customer demographic and contact information.

**Key Fields:**
- `CUST-ID` - 9-digit customer identifier (primary key)
- `CUST-FIRST-NAME` - Customer first name
- `CUST-MIDDLE-NAME` - Customer middle name
- `CUST-LAST-NAME` - Customer last name
- `CUST-ADDR-LINE-1` through `CUST-ADDR-LINE-3` - Customer address
- `CUST-ADDR-STATE-CD` - Two-character US state code
- `CUST-ADDR-COUNTRY-CD` - Three-character country code
- `CUST-ADDR-ZIP` - ZIP/postal code
- `CUST-PHONE-NUM-1` - Primary phone number (format: (999)999-9999)
- `CUST-PHONE-NUM-2` - Secondary phone number (format: (999)999-9999)
- `CUST-SSN` - Social Security Number (9 digits)
- `CUST-GOVT-ISSUED-ID` - Government-issued ID number
- `CUST-DOB-YYYY-MM-DD` - Date of birth (YYYY-MM-DD format)
- `CUST-EFT-ACCOUNT-ID` - Electronic funds transfer account ID
- `CUST-PRI-CARD-HOLDER-IND` - Primary cardholder indicator
- `CUST-FICO-CREDIT-SCORE` - FICO credit score (300-850)

### Card Cross-Reference Record

```cobol
01  CARD-XREF-RECORD.
    05  XREF-CARD-NUM               PIC X(16).
    05  XREF-CUST-ID                PIC 9(09) COMP.
    05  XREF-ACCT-ID                PIC X(11).
    05  FILLER                      PIC X(219).
```

**Purpose:** Cross-reference file linking card numbers to accounts and customers. Allows lookup by account ID to retrieve customer ID.

**Key Fields:**
- `XREF-CARD-NUM` - 16-digit card number (primary key)
- `XREF-CUST-ID` - Customer ID (links to CUSTOMER-RECORD)
- `XREF-ACCT-ID` - Account ID (alternate index, links to ACCOUNT-RECORD)

### Inter-Program Communication Area

```cobol
01  CARDDEMO-COMMAREA.
    05  CDEMO-FROM-TRANID           PIC X(4).
    05  CDEMO-FROM-PROGRAM          PIC X(8).
    05  CDEMO-TO-TRANID             PIC X(4).
    05  CDEMO-TO-PROGRAM            PIC X(8).
    05  CDEMO-USER-ID               PIC X(8).
    05  CDEMO-USER-TYPE             PIC X(1).
    05  CDEMO-PGM-CONTEXT           PIC X(1).
    05  CDEMO-ACCT-ID               PIC 9(11) COMP.
    05  CDEMO-CARD-NUM              PIC X(16).
    05  CDEMO-CUST-ID               PIC 9(09) COMP.
    05  CDEMO-CUST-FNAME            PIC X(25).
    05  CDEMO-CUST-MNAME            PIC X(25).
    05  CDEMO-CUST-LNAME            PIC X(25).
    05  CDEMO-ACCT-STATUS           PIC X(1).
    05  CDEMO-LAST-MAPSET           PIC X(8).
    05  CDEMO-LAST-MAP              PIC X(7).
    05  CDEMO-LAST-AID              PIC X(1).
    05  FILLER                      PIC X(300).
```

**Purpose:** Shared communication area used across all CardDemo programs for navigation context, user session data, and current account/customer context.

**Navigation Fields:**
- `CDEMO-FROM-TRANID` - Transaction ID of calling program
- `CDEMO-FROM-PROGRAM` - Program name that invoked this program
- `CDEMO-TO-TRANID` - Transaction ID to transfer to on exit
- `CDEMO-TO-PROGRAM` - Program name to transfer to on exit

**Session Fields:**
- `CDEMO-USER-ID` - Logged-in user identifier
- `CDEMO-USER-TYPE` - User type (admin vs. regular)
- `CDEMO-PGM-CONTEXT` - Entry vs. re-entry state

**Context Fields:**
- `CDEMO-ACCT-ID` - Current account number
- `CDEMO-CARD-NUM` - Current card number
- `CDEMO-CUST-ID` - Current customer ID
- `CDEMO-CUST-FNAME`, `CDEMO-CUST-MNAME`, `CDEMO-CUST-LNAME` - Customer name
- `CDEMO-ACCT-STATUS` - Account status

### Program-Specific State Area

```cobol
01  WS-THIS-PROGCOMMAREA.
    05  ACUP-OLD-DETAILS.
        10  ACUP-OLD-ACCT-DATA.
            15  ACUP-OLD-ACCT-ID            PIC 9(11).
            15  ACUP-OLD-ACTIVE-STATUS      PIC X(01).
            15  ACUP-OLD-CURR-BAL-N         PIC S9(10)V99.
            15  ACUP-OLD-CREDIT-LIMIT-N     PIC S9(10)V99.
            15  ACUP-OLD-CASH-CREDIT-LIMIT-N PIC S9(10)V99.
            15  ACUP-OLD-OPEN-YEAR          PIC 9(04).
            15  ACUP-OLD-OPEN-MON           PIC 9(02).
            15  ACUP-OLD-OPEN-DAY           PIC 9(02).
            15  ACUP-OLD-EXPIRY-YEAR        PIC 9(04).
            15  ACUP-OLD-EXPIRY-MON         PIC 9(02).
            15  ACUP-OLD-EXPIRY-DAY         PIC 9(02).
            15  ACUP-OLD-REISSUE-YEAR       PIC 9(04).
            15  ACUP-OLD-REISSUE-MON        PIC 9(02).
            15  ACUP-OLD-REISSUE-DAY        PIC 9(02).
            15  ACUP-OLD-CURR-CYC-CREDIT-N  PIC S9(10)V99.
            15  ACUP-OLD-CURR-CYC-DEBIT-N   PIC S9(10)V99.
            15  ACUP-OLD-GROUP-ID           PIC X(10).
        10  ACUP-OLD-CUST-DATA.
            15  ACUP-OLD-CUST-ID            PIC 9(09).
            15  ACUP-OLD-CUST-SSN           PIC 9(09).
            15  ACUP-OLD-CUST-FIRST-NAME    PIC X(25).
            15  ACUP-OLD-CUST-MIDDLE-NAME   PIC X(25).
            15  ACUP-OLD-CUST-LAST-NAME     PIC X(25).
            15  ACUP-OLD-CUST-ADDR-LINE-1   PIC X(50).
            15  ACUP-OLD-CUST-ADDR-LINE-2   PIC X(50).
            15  ACUP-OLD-CUST-ADDR-LINE-3   PIC X(50).
            15  ACUP-OLD-CUST-ADDR-STATE-CD PIC X(02).
            15  ACUP-OLD-CUST-ADDR-COUNTRY-CD PIC X(03).
            15  ACUP-OLD-CUST-ADDR-ZIP      PIC X(10).
            15  ACUP-OLD-CUST-PHONE-NUM-1A  PIC 9(03).
            15  ACUP-OLD-CUST-PHONE-NUM-1B  PIC 9(03).
            15  ACUP-OLD-CUST-PHONE-NUM-1C  PIC 9(04).
            15  ACUP-OLD-CUST-PHONE-NUM-2A  PIC 9(03).
            15  ACUP-OLD-CUST-PHONE-NUM-2B  PIC 9(03).
            15  ACUP-OLD-CUST-PHONE-NUM-2C  PIC 9(04).
            15  ACUP-OLD-CUST-GOVT-ISSUED-ID PIC X(20).
            15  ACUP-OLD-CUST-DOB-YEAR      PIC 9(04).
            15  ACUP-OLD-CUST-DOB-MON       PIC 9(02).
            15  ACUP-OLD-CUST-DOB-DAY       PIC 9(02).
            15  ACUP-OLD-CUST-FICO-CREDIT-SCORE PIC 9(03).
            15  ACUP-OLD-CUST-EFT-ACCOUNT-ID PIC X(10).
    05  ACUP-NEW-DETAILS.
        10  ACUP-NEW-ACCT-DATA.
            15  ACUP-NEW-ACCT-ID-X          PIC X(11).
            15  ACUP-NEW-ACTIVE-STATUS      PIC X(01).
            15  ACUP-NEW-CREDIT-LIMIT-X     PIC X(17).
            15  ACUP-NEW-CREDIT-LIMIT-N     PIC S9(10)V99.
            15  ACUP-NEW-CURR-BAL-X         PIC X(17).
            15  ACUP-NEW-CURR-BAL-N         PIC S9(10)V99.
            15  ACUP-NEW-CASH-CREDIT-LIMIT-X PIC X(17).
            15  ACUP-NEW-CASH-CREDIT-LIMIT-N PIC S9(10)V99.
            15  ACUP-NEW-OPEN-YEAR          PIC X(04).
            15  ACUP-NEW-OPEN-MON           PIC X(02).
            15  ACUP-NEW-OPEN-DAY           PIC X(02).
            15  ACUP-NEW-EXPIRY-YEAR        PIC X(04).
            15  ACUP-NEW-EXPIRY-MON         PIC X(02).
            15  ACUP-NEW-EXPIRY-DAY         PIC X(02).
            15  ACUP-NEW-REISSUE-YEAR       PIC X(04).
            15  ACUP-NEW-REISSUE-MON        PIC X(02).
            15  ACUP-NEW-REISSUE-DAY        PIC X(02).
            15  ACUP-NEW-CURR-CYC-CREDIT-X  PIC X(17).
            15  ACUP-NEW-CURR-CYC-CREDIT-N  PIC S9(10)V99.
            15  ACUP-NEW-CURR-CYC-DEBIT-X   PIC X(17).
            15  ACUP-NEW-CURR-CYC-DEBIT-N   PIC S9(10)V99.
            15  ACUP-NEW-GROUP-ID           PIC X(10).
        10  ACUP-NEW-CUST-DATA.
            15  ACUP-NEW-CUST-ID            PIC 9(09).
            15  ACUP-NEW-CUST-SSN-N         PIC 9(09).
            15  ACUP-NEW-CUST-SSN-1         PIC X(03).
            15  ACUP-NEW-CUST-SSN-2         PIC X(02).
            15  ACUP-NEW-CUST-SSN-3         PIC X(04).
            15  ACUP-NEW-CUST-FIRST-NAME    PIC X(25).
            15  ACUP-NEW-CUST-MIDDLE-NAME   PIC X(25).
            15  ACUP-NEW-CUST-LAST-NAME     PIC X(25).
            15  ACUP-NEW-CUST-ADDR-LINE-1   PIC X(50).
            15  ACUP-NEW-CUST-ADDR-LINE-2   PIC X(50).
            15  ACUP-NEW-CUST-ADDR-LINE-3   PIC X(50).
            15  ACUP-NEW-CUST-ADDR-STATE-CD PIC X(02).
            15  ACUP-NEW-CUST-ADDR-COUNTRY-CD PIC X(03).
            15  ACUP-NEW-CUST-ADDR-ZIP      PIC X(10).
            15  ACUP-NEW-CUST-PHONE-NUM-1A  PIC X(03).
            15  ACUP-NEW-CUST-PHONE-NUM-1B  PIC X(03).
            15  ACUP-NEW-CUST-PHONE-NUM-1C  PIC X(04).
            15  ACUP-NEW-CUST-PHONE-NUM-2A  PIC X(03).
            15  ACUP-NEW-CUST-PHONE-NUM-2B  PIC X(03).
            15  ACUP-NEW-CUST-PHONE-NUM-2C  PIC X(04).
            15  ACUP-NEW-CUST-GOVT-ISSUED-ID PIC X(20).
            15  ACUP-NEW-CUST-DOB-YEAR      PIC X(04).
            15  ACUP-NEW-CUST-DOB-MON       PIC X(02).
            15  ACUP-NEW-CUST-DOB-DAY       PIC X(02).
            15  ACUP-NEW-CUST-FICO-CREDIT-SCORE PIC X(03).
            15  ACUP-NEW-CUST-EFT-ACCOUNT-ID PIC X(10).
    05  WS-NON-KEY-FLAGS.
        10  WS-EDIT-ACCT-STATUS             PIC X(01).
        10  WS-EDIT-OPEN-DATE-FLGS.
            15  FLG-OPEN-YEAR-ISVALID       PIC X(01).
            15  FLG-OPEN-MONTH-ISVALID      PIC X(01).
            15  FLG-OPEN-DAY-ISVALID        PIC X(01).
        10  WS-EDIT-EXPIRY-DATE-FLGS.
            15  FLG-EXPIRY-YEAR-ISVALID     PIC X(01).
            15  FLG-EXPIRY-MONTH-ISVALID    PIC X(01).
            15  FLG-EXPIRY-DAY-ISVALID      PIC X(01).
        10  WS-EDIT-REISSUE-DATE-FLGS.
            15  FLG-REISSUE-YEAR-ISVALID    PIC X(01).
            15  FLG-REISSUE-MONTH-ISVALID   PIC X(01).
            15  FLG-REISSUE-DAY-ISVALID     PIC X(01).
```

**Purpose:** Preserves original fetched data (ACUP-OLD-*), user modifications (ACUP-NEW-*), and validation results across session interactions.

### State Management Flags

```cobol
01  WS-MISC-STORAGE.
    05  ACUP-DETAILS-FETCH-FLAG     PIC X(01).
        88  ACUP-DETAILS-NOT-FETCHED    VALUE 'N'.
        88  ACUP-DETAILS-FETCHED        VALUE 'Y'.
    05  ACUP-CHANGE-STATUS          PIC X(01).
        88  ACUP-SHOW-DETAILS           VALUE 'S'.
        88  ACUP-CHANGES-MADE           VALUE 'C'.
        88  ACUP-CHANGES-NOT-OK         VALUE 'E'.
        88  ACUP-CHANGES-OK-NOT-CONFIRMED VALUE 'V'.
        88  ACUP-CHANGES-OKAYED-AND-DONE  VALUE 'D'.
        88  ACUP-CHANGES-FAILED           VALUE 'F'.
        88  ACUP-CHANGES-OKAYED-LOCK-ERROR VALUE 'L'.
        88  ACUP-CHANGES-OKAYED-BUT-FAILED VALUE 'U'.
```

**Purpose:** 88-level condition names control program flow through session interactions, tracking whether data has been fetched and the validation/save state.

**State Values:**
- `ACUP-DETAILS-NOT-FETCHED` - Initial state, awaiting account number
- `ACUP-SHOW-DETAILS` - Account data fetched and displayed
- `ACUP-CHANGES-MADE` - User has modified one or more fields
- `ACUP-CHANGES-NOT-OK` - Validation failed
- `ACUP-CHANGES-OK-NOT-CONFIRMED` - Validation passed, awaiting save confirmation
- `ACUP-CHANGES-OKAYED-AND-DONE` - Update completed successfully
- `ACUP-CHANGES-FAILED` - Update failed
- `ACUP-CHANGES-OKAYED-LOCK-ERROR` - Could not lock records for update
- `ACUP-CHANGES-OKAYED-BUT-FAILED` - Update operation failed

---

## Field Mappings

### Account Fields

| Screen Field | Database Field | Data Type | Description |
|-------------|----------------|-----------|-------------|
| ACCTSID | ACCTDAT.ACCT-ID | X(11) | Account Number |
| ACSTTUS | ACCTDAT.ACCT-ACTIVE-STATUS | X(01) | Account Status (Y/N) |
| ACRDLIM | ACCTDAT.ACCT-CREDIT-LIMIT | S9(10)V99 | Credit Limit |
| ACURBAL | ACCTDAT.ACCT-CURR-BAL | S9(10)V99 | Current Balance |
| ACSHLIM | ACCTDAT.ACCT-CASH-CREDIT-LIMIT | S9(10)V99 | Cash Credit Limit |
| OPNYEAR / OPNMON / OPNDAY | ACCTDAT.ACCT-OPEN-DATE | X(10) | Account Open Date (YYYY-MM-DD) |
| EXPYEAR / EXPMON / EXPDAY | ACCTDAT.ACCT-EXPIRAION-DATE | X(10) | Card Expiration Date (YYYY-MM-DD) |
| REIYEAR / REIMON / REIDAY | ACCTDAT.ACCT-REISSUE-DATE | X(10) | Card Reissue Date (YYYY-MM-DD) |
| CURSORG | ACCTDAT.ACCT-CURR-CYC-CREDIT | S9(10)V99 | Current Cycle Credit |
| CURDUPD | ACCTDAT.ACCT-CURR-CYC-DEBIT | S9(10)V99 | Current Cycle Debit |
| AGRPID | ACCTDAT.ACCT-GROUP-ID | X(10) | Account Group ID |

### Customer Fields

| Screen Field | Database Field | Data Type | Description |
|-------------|----------------|-----------|-------------|
| CSFNAME | CUSTDAT.CUST-FIRST-NAME | X(25) | Customer First Name |
| CSMNAME | CUSTDAT.CUST-MIDDLE-NAME | X(25) | Customer Middle Name |
| CSLNAME | CUSTDAT.CUST-LAST-NAME | X(25) | Customer Last Name |
| CSADL1 | CUSTDAT.CUST-ADDR-LINE-1 | X(50) | Address Line 1 |
| CSADL2 | CUSTDAT.CUST-ADDR-LINE-2 | X(50) | Address Line 2 |
| CSADL3 | CUSTDAT.CUST-ADDR-LINE-3 | X(50) | Address Line 3 |
| CSSTATE | CUSTDAT.CUST-ADDR-STATE-CD | X(02) | State Code |
| CSCNTRY | CUSTDAT.CUST-ADDR-COUNTRY-CD | X(03) | Country Code |
| CSZIP | CUSTDAT.CUST-ADDR-ZIP | X(10) | ZIP/Postal Code |
| CSPH1A / CSPH1B / CSPH1C | CUSTDAT.CUST-PHONE-NUM-1 | X(15) | Phone Number 1 (Area/Prefix/Line) |
| CSPH2A / CSPH2B / CSPH2C | CUSTDAT.CUST-PHONE-NUM-2 | X(15) | Phone Number 2 (Area/Prefix/Line) |
| CSSSN1 / CSSSN2 / CSSSN3 | CUSTDAT.CUST-SSN | 9(09) | Social Security Number (3 parts) |
| CSGID | CUSTDAT.CUST-GOVT-ISSUED-ID | X(20) | Government Issued ID |
| CSDOBY / CSDOBM / CSDOBD | CUSTDAT.CUST-DOB-YYYY-MM-DD | X(10) | Date of Birth (Year/Month/Day) |
| CSFICO | CUSTDAT.CUST-FICO-CREDIT-SCORE | 9(03) | FICO Credit Score |
| CSEFTID | CUSTDAT.CUST-EFT-ACCOUNT-ID | X(10) | EFT Account ID |

---

## Files and Tables

### ACCTDAT - Account Master File

**Description:** Stores account financial information including balances, limits, and important dates.

**Key Field:** ACCT-ID (PIC X(11))

**Operations:** Read, Lock for Update, Update

**Contents:**
- Account identification (ACCT-ID)
- Account status (ACCT-ACTIVE-STATUS)
- Financial data (balances, limits)
- Important dates (open, expiration, reissue)
- Cycle activity (credits, debits)
- Group association (ACCT-GROUP-ID)

### CUSTDAT - Customer Master File

**Description:** Stores customer demographic and contact information.

**Key Field:** CUST-ID (PIC 9(09) COMP)

**Operations:** Read, Lock for Update, Update

**Contents:**
- Customer identification (CUST-ID)
- Customer name (first, middle, last)
- Address (3 lines, state, country, zip)
- Contact information (2 phone numbers)
- Personal identifiers (SSN, government ID)
- Additional data (date of birth, FICO score, EFT account)

### CARDDAT - Card Cross-Reference File

**Description:** Cross-reference file linking card numbers to accounts and customers. Provides mapping from Account ID to Customer ID and Card Number.

**Primary Key:** XREF-CARD-NUM (PIC X(16))

**Alternate Index:** XREF-ACCT-ID (used by this program for account-based lookup)

**Operations:** Read (via alternate index)

**Contents:**
- Card number (XREF-CARD-NUM)
- Customer ID (XREF-CUST-ID)
- Account ID (XREF-ACCT-ID)

**Relationship:** When given an Account ID, the program uses the alternate index to retrieve the associated Customer ID and Card Number.

---

## Business Logic and Program Flow

### Program Initialization

When the program begins execution, it establishes error handling, initializes working storage areas (CC-WORK-AREA, WS-MISC-STORAGE, WS-COMMAREA), stores transaction context (transaction ID 'CAUP', program name), and clears error message flags.

### State Management and Session Handling

The program employs a stateful session design pattern. On each invocation, it examines the communication area to determine its current state.

**Case 1: Initial Entry** - When there is no communication area or entry from menu program with fresh context, the program initializes both CARDDEMO-COMMAREA and WS-THIS-PROGCOMMAREA, sets state to initial entry (CDEMO-PGM-ENTER) and data not fetched (ACUP-DETAILS-NOT-FETCHED), indicating no data has been loaded yet.

**Case 2: Re-entry** - When the user has taken an action (pressed a key), the program restores the previous state by retrieving the communication area. This preserves all previously fetched data, validation flags, and state indicators across session interactions.

### User Action Processing

After restoring state, the program validates which actions are acceptable in the current state. It initializes the action as invalid, then checks if the action is one of the allowed actions: ENTER (always valid), F3/EXIT (always valid), F5/SAVE (valid only when changes validated and awaiting confirmation), or F12/CANCEL (valid only when data has been fetched). If the action is valid, processing continues; otherwise invalid actions are treated as ENTER to prevent user confusion.

### Main Processing Logic

The program uses conditional logic to route processing based on the current state and user action.

#### Exit Processing

When the user exits (F3), the program performs a clean exit. It first checks if a calling program exists by examining the navigation context. If empty, it defaults to the main menu program (COMEN01C with transaction CMEN). Otherwise, it returns to the calling program using the stored program and transaction identifiers.

The program then updates the navigation context by recording the current program and transaction as the source, setting user type and entry flags, and recording the last screen displayed. It commits any pending changes, then transfers control passing the updated communication area. This preserves navigation context, allowing the target program to know where the user came from.

#### Initial Display and Reset Scenarios

When displaying the initial entry screen or resetting after completion, the program branches based on state. For initial entry (data not fetched with initial entry state) or fresh entry from menu, it initializes the program-specific area, sends a blank screen, sets the re-entry flag, and returns.

For terminal states (update completed successfully or update failed), the program initializes all working storage areas and the account identifier, sets entry state, sends a blank screen, sets re-entry flag, and returns. These scenarios display a blank screen prompting for a new account number.

#### Normal Processing Flow

For all other cases, the program follows the standard processing path:
1. **Process Inputs** - Receive and validate input
2. **Decide Action** - Determine what action to take based on state and input
3. **Send Screen** - Display the updated screen
4. **Return** - Return to session manager with preserved state

This is the main processing path involving input reception, validation, decision logic, and screen display.

### Input Processing

The input processing section coordinates receiving and validating input. It retrieves screen data, validates all inputs, and prepares navigation context by moving the return message to the communication area error field and setting identifiers to reference this program, ensuring the user stays on this screen.

#### Receive Input Data

The program retrieves screen data and systematically moves each field from the input screen to working storage. For each field, it checks if the input indicates no change (asterisk or spaces). If so, it marks the field as unchanged. Otherwise, it captures the actual input value.

For the account number field, the program checks for changes and moves the value to both working storage fields (CC-ACCT-ID and ACUP-NEW-ACCT-ID-X). If details haven't been fetched yet (ACUP-DETAILS-NOT-FETCHED), it exits immediately since only the account number is needed for initial search.

For numeric fields like credit limit, the program performs additional validation. It moves the input to the display format field (ACUP-NEW-CREDIT-LIMIT-X), then verifies the input is numeric using built-in numeric validation. If valid, it converts the value and stores it in the numeric format field (ACUP-NEW-CREDIT-LIMIT-N).

For date fields, the program splits the input into separate components: year (4 digits), month (2 digits), and day (2 digits), storing each component in its own working storage field (ACUP-NEW-OPEN-YEAR, ACUP-NEW-OPEN-MON, ACUP-NEW-OPEN-DAY) for independent validation later.

The process repeats for all screen fields including account status, expiration date, reissue date, current balance, cash credit limit, current cycle credit/debit, group ID, customer name components, three address lines, state code, country code, zip code, two phone numbers, SSN components, government ID, date of birth components, FICO score, and EFT account ID. The routine systematically captures all user inputs while handling blank/unchanged fields appropriately.

#### Validate Input Data

The validation section performs comprehensive validation. It first initializes validation status to valid.

**If Account Not Yet Fetched** - When data has not been fetched (ACUP-DETAILS-NOT-FETCHED is true), the program only validates the account number by checking that it is 11 digits, numeric, and non-zero. If the account number is blank, it sets a flag indicating no search criteria was received. The routine then initializes the old account data area to empty and exits early since no other fields are relevant yet.

**If Account Already Fetched** - The program sets various found flags (account data found, account in master, account filter valid, customer in master, customer filter valid), then compares every field between old and new values.

The comparison routine performs a field-by-field comparison between original (ACUP-OLD-*) and modified (ACUP-NEW-*) values. For each field, it checks if the values differ. If any field has changed, it sets the changes-found flag and updates a corresponding field-level change flag. This comparison covers all account fields (status, balance, credit limits, dates, cycle amounts, group ID) and all customer fields (name components, address lines, state, country, zip, phone numbers, SSN, government ID, date of birth, FICO score, EFT account).

If no changes are found, or if changes were already validated or completed, the program clears validation flags and exits validation early.

If changes are detected, the program validates each modified field by calling specialized validation routines. For example, it prepares the variable name and value, then performs the appropriate validation routine such as yes/no validation for account status, date validation for dates, signed numeric validation for currency fields, required alphabetic validation for customer names, alphabetic or space validation for address fields, phone number validation, SSN validation, state code validation, zip/state combination validation, and numeric validation for FICO score.

Each validation routine stores its result in corresponding working storage flags. If any validation fails, the input error flag is set.

At the end of validation, if all validations passed, the program sets the state to changes validated and awaiting confirmation (ACUP-CHANGES-OK-NOT-CONFIRMED) to enable the save action and prompt the user to confirm the save operation.

### Decision Logic

The decision logic section determines actions based on the current state.

**First Search - Fetch Account Data** - When data has not been fetched (ACUP-DETAILS-NOT-FETCHED is true) and the account filter is valid (FLG-ACCTFILTER-ISVALID), the program reads data from the three files (card cross-reference, account master, customer master). If the account is found (FOUND-ACCOUNT-DATA), it sets the state to show details (ACUP-SHOW-DETAILS) to display the fetched data on screen.

**Changes Made and Valid** - When changes have been made (ACUP-CHANGES-MADE is true) and all inputs are valid (INPUT-OK), the program sets state to changes validated awaiting confirmation (ACUP-CHANGES-OK-NOT-CONFIRMED) and sets a flag to display a message asking the user to confirm save or cancel.

**User Confirms Save** - When in the confirm state (ACUP-CHANGES-OK-NOT-CONFIRMED) and save action is triggered, the program performs update processing. Based on the results, it evaluates four possible outcomes:
1. If could not lock account → sets state to lock error (ACUP-CHANGES-OKAYED-LOCK-ERROR)
2. If locked but update failed → sets state to update failed (ACUP-CHANGES-OKAYED-BUT-FAILED)
3. If data was changed before update → another user modified the data, so sets state back to show details (ACUP-SHOW-DETAILS) to display current values
4. Otherwise update succeeded → sets state to completed (ACUP-CHANGES-OKAYED-AND-DONE)

**Unexpected State** - Any unhandled scenario triggers an error condition by recording the program name, error code ('0001'), and error message ('UNEXPECTED DATA SCENARIO'), then performing the error routine.

### Account Data Fetch Process

The account data fetch process orchestrates reading from three files. It begins by initializing the old details area (ACUP-OLD-DETAILS), setting the no-info-message flag, and moving the account ID to working storage fields (ACUP-OLD-ACCT-ID, WS-CARD-RID-ACCT-ID). It then performs three sequential read operations.

**Step 1: Read Card Cross-Reference** - The program reads from the card cross-reference file using the account ID as the search key. This accesses the alternate index to get Customer ID from Account ID. The response is checked; if not found, the program sets error flags and exits. If successful, it proceeds to read the account master.

**Step 2: Read Account Master** - The program reads from the account master file using the account ID as the key. This retrieves account financial data including balances, credit limits, and dates. If not found, it sets error flags and exits. If successful, it continues to read the customer master.

**Step 3: Read Customer Master** - The program first moves the customer ID from the previous read (CDEMO-CUST-ID) to the read key field (WS-CARD-RID-CUST-ID), then reads from the customer master file. This retrieves customer demographics including name, address, phone numbers, SSN, date of birth, government ID, FICO score, and EFT account ID. If not found, it sets error flags and exits.

**Step 4: Store Fetched Data** - The store fetched data routine moves all fields from the three file records into the ACUP-OLD-* working storage fields. This preserves the original values for later comparison during updates. Account fields are moved to ACUP-OLD-ACCT-DATA and customer fields to ACUP-OLD-CUST-DATA. For date fields, the program splits the YYYY-MM-DD format into separate year, month, and day components. For phone numbers, it parses the (999)999-9999 format into separate area code, prefix, and line number components.

### Update Processing

The update processing section implements the database update with proper locking and transaction integrity.

**Locking Phase** - The program first attempts to lock both records for exclusive update.

For the account record, it reads the record with an update lock using the account ID as key. If the response is not successful, it sets the could-not-lock flag (COULD-NOT-LOCK-ACCT-FOR-UPDATE), displays an error message, and exits without updating.

For the customer record, it reads the record with an update lock using the customer ID as key. If locking fails, it sets the lock error flag and exits.

**Optimistic Concurrency Check** - The program performs a concurrent change detection check which compares every field in the locked records against the ACUP-OLD-* values that were originally fetched. This implements optimistic locking to detect concurrent updates.

The concurrent change check performs field-by-field comparison. For each field, it checks if the value in the locked record differs from the original value. If any field has changed, another user modified the data concurrently, so it sets the data-was-changed-before-update flag (DATA-WAS-CHANGED-BEFORE-UPDATE), displays an error message, and exits. If no concurrent changes are detected, the update can proceed safely.

**Prepare Update Records** - The program builds the update records by moving values from ACUP-NEW-* working storage to the update record structures (ACCT-UPDATE-RECORD and CUST-UPDATE-RECORD). For date fields, it reassembles the components using string concatenation. For example, the open date is assembled by combining the year, a dash, the month, another dash, and the day into the target date field. Phone numbers are similarly reassembled by combining the area code in parentheses, prefix, dash, and line number into the (999)999-9999 format.

**Update Account Record** - The program updates the account record in the account master file. If the response is not successful, it sets the update failed flag (LOCKED-BUT-UPDATE-FAILED) and exits.

**Update Customer Record with Rollback** - The program updates the customer record in the customer master file.

After the update, the program checks the response. If successful, both updates succeeded and the transaction commits automatically. However, if the customer update fails after the account update already succeeded, this creates a data inconsistency. To handle this, the program issues a transaction rollback command.

This undoes the account update, ensuring transaction integrity - either both files are updated atomically or neither is updated. The program sets the update failed flag and exits.

### Program Return

Every processing path ends at the common return section. The program first moves the return message to the error message field in the communication area (CCARD-ERROR-MSG). It then packs both communication areas into a single structure: the shared area (CARDDEMO-COMMAREA) goes into the first portion of the combined area (WS-COMMAREA), and the program-specific area (WS-THIS-PROGCOMMAREA) goes into the second portion.

Finally, it returns control with transaction ID 'CAUP', passing the packed communication area (2000 bytes total) to preserve state across session interactions. The program releases all resources and waits for the next user input. When the user takes an action, the session manager automatically restarts the program with the same transaction ID, restores the communication area, and processing continues from the beginning with the preserved state intact.

---

## Validation Rules

### Yes/No Fields

**Validation:** Must be 'Y' or 'N'

**Error Messages:**
- "[Field name] must be supplied"
- "[Field name] must be Y or N"

**Applied To:** ACCT-ACTIVE-STATUS

### Date Fields

**Year Validation:**
- Must be 4 digits
- Must be numeric
- Range: 1900-2099

**Month Validation:**
- Must be 2 digits
- Must be numeric
- Range: 01-12

**Day Validation:**
- Must be 2 digits
- Must be numeric
- Valid for the specified month (handles leap years)

**Date of Birth Special Rule:**
- Must not be in the future

**Error Messages:**
- "Year must be supplied"
- "Year must be 4 digits"
- "Year must be numeric"
- "Year must be between 1900 and 2099"
- "Month must be supplied"
- "Month must be 2 digits"
- "Month must be numeric"
- "Month must be between 01 and 12"
- "Day must be supplied"
- "Day must be 2 digits"
- "Day must be numeric"
- "Day must be valid for the month"
- "Date of birth cannot be in the future"

**Applied To:**
- ACCT-OPEN-DATE
- ACCT-EXPIRAION-DATE
- ACCT-REISSUE-DATE
- CUST-DOB-YYYY-MM-DD

### Currency/Numeric Fields

**Validation:**
- Must pass numeric validation with optional sign and decimal point
- Can be negative
- Must have exactly two decimal places

**Error Messages:**
- "[Field name] must be supplied"
- "[Field name] is not valid"

**Applied To:**
- ACCT-CURR-BAL
- ACCT-CREDIT-LIMIT
- ACCT-CASH-CREDIT-LIMIT
- ACCT-CURR-CYC-CREDIT
- ACCT-CURR-CYC-DEBIT

### Simple Numeric Fields

**Validation:**
- Must be all numeric
- Must not be zero (if required)

**FICO Score Special Validation:**
- Must be numeric
- Range: 300-850

**Error Messages:**
- "[Field name] must be supplied"
- "[Field name] must be numeric"
- "[Field name] must not be zero"
- "FICO Score must be between 300 and 850"

**Applied To:**
- CUST-FICO-CREDIT-SCORE

### Required Alphabetic Fields

**Validation:**
- Only alphabetic characters and spaces allowed
- Non-alphabetic characters are removed
- Result must not be blank after removal

**Error Messages:**
- "[Field name] must be supplied"
- "[Field name] can have alphabets only"

**Applied To:**
- CUST-FIRST-NAME
- CUST-MIDDLE-NAME
- CUST-LAST-NAME

### Optional Alphabetic or Space Fields

**Validation:**
- Can be blank (optional)
- If provided, only alphabetic characters and spaces allowed

**Error Messages:**
- "[Field name] can have alphabets only"

**Applied To:**
- CUST-ADDR-LINE-1
- CUST-ADDR-LINE-2
- CUST-ADDR-LINE-3

### Social Security Number

**Part 1 Validation:**
- Must be 3 digits
- Must be numeric
- Must not be 000

**Part 2 Validation:**
- Must be 2 digits
- Must be numeric
- Must not be 00

**Part 3 Validation:**
- Must be 4 digits
- Must be numeric
- Must not be 0000

**Error Messages:**
- "SSN Part 1 must be supplied"
- "SSN Part 1 must be 3 digits"
- "SSN Part 1 must be numeric"
- "SSN Part 1 cannot be 000"
- "SSN Part 2 must be supplied"
- "SSN Part 2 must be 2 digits"
- "SSN Part 2 must be numeric"
- "SSN Part 2 cannot be 00"
- "SSN Part 3 must be supplied"
- "SSN Part 3 must be 4 digits"
- "SSN Part 3 must be numeric"
- "SSN Part 3 cannot be 0000"

**Applied To:** CUST-SSN (validated in 3 parts)

### State Code

**Validation:**
- Must be 2 alphabetic characters
- Must be a valid US state code

**Error Messages:**
- "State code must be supplied"
- "Invalid state code"

**Applied To:** CUST-ADDR-STATE-CD

### Phone Numbers

**Validation:**
- Optional: Can be completely blank
- If any part provided, all parts required
- Area code: 3 digits, valid North American area code
- Prefix: 3 digits, not 000
- Line: 4 digits

**Error Messages:**
- "Phone area code must be supplied"
- "Phone area code must be 3 digits"
- "Phone area code must be numeric"
- "Invalid phone area code"
- "Phone prefix must be supplied"
- "Phone prefix must be 3 digits"
- "Phone prefix must be numeric"
- "Phone prefix cannot be 000"
- "Phone line must be supplied"
- "Phone line must be 4 digits"
- "Phone line must be numeric"

**Applied To:**
- CUST-PHONE-NUM-1
- CUST-PHONE-NUM-2

### Cross-Field Validation

**Zip Code and State Validation:**
- Zip code must be valid for the specified state code

**Error Message:**
- "Zip code is not valid for state [XX]"

**Applied To:** CUST-ADDR-ZIP and CUST-ADDR-STATE-CD combination

---

## Error Conditions and Recovery

### Record Not Found Errors

**Account Not in Card Cross-Reference:**
- **Error:** Account number does not exist in the card cross-reference file
- **Recovery:** Display error message, return to entry screen for new account number

**Account Not in Account Master:**
- **Error:** Account number exists in cross-reference but not in account master file
- **Recovery:** Display error message, return to entry screen for new account number

**Customer Not in Customer Master:**
- **Error:** Customer ID from cross-reference does not exist in customer master file
- **Recovery:** Display error message, return to entry screen for new account number

### Record Locking Failures

**Cannot Lock Account Record:**
- **Error:** Another user or process has locked the account record for update
- **Message:** "Could not lock account for update"
- **Recovery:** Do not proceed with update, return to display screen, user must retry later

**Cannot Lock Customer Record:**
- **Error:** Another user or process has locked the customer record for update
- **Message:** "Could not lock customer for update"
- **Recovery:** Do not proceed with update, return to display screen, user must retry later

### Update Failures

**Account Update Fails:**
- **Error:** Update operation on account master file failed
- **Message:** "Update failed"
- **Recovery:** Do not proceed with customer update, return to display screen

**Customer Update Fails After Account Success:**
- **Error:** Customer update failed but account update already succeeded
- **Action:** Issue transaction rollback to undo account update
- **Message:** "Update failed"
- **Recovery:** Both updates are rolled back, data remains unchanged, return to display screen

### Concurrent Update Detection

**Optimistic Locking Check:**
- **Process:** Compare original fetched values (ACUP-OLD-*) against locked record values
- **Error:** Any field has changed since original fetch
- **Cause:** Another user modified the account or customer data concurrently
- **Message:** "Data was changed by another user. Please review and retry."
- **Recovery:** Release locks, display current (changed) data, user must review and re-enter changes

### Field Validation Errors

**General Pattern:**
- Field attribute set to error indicator
- Cursor positioned at first error field
- Error message displayed in error message area
- Field value may show error indicator
- User must correct and resubmit

**Specific Validation Failures:** See Validation Rules section for detailed error messages and conditions.

### No Search Criteria

**Error:** User submitted blank account number
- **Flag:** NO-SEARCH-CRITERIA-RECEIVED
- **Recovery:** Display error message prompting for account number

### Unexpected Program State

**Error:** Program reaches unhandled state combination
- **Cause:** Logic error or data corruption
- **Action:** Record program name, error code ('0001'), message ('UNEXPECTED DATA SCENARIO')
- **Recovery:** Execute error routine, terminate transaction

---

## Inter-Program Communication

### Communication Area Structure

**Total Length:** 2000 bytes

**Component 1: Shared Communication Area (CARDDEMO-COMMAREA)**
- Size: ~500 bytes
- Scope: Shared across all CardDemo programs
- Contains: Navigation context, user session data, current account/customer context

**Component 2: Program-Specific State (WS-THIS-PROGCOMMAREA)**
- Size: ~1500 bytes
- Scope: COACTUPC program only
- Contains: Original fetched data (ACUP-OLD-*), user modifications (ACUP-NEW-*), validation flags, state indicators

### Navigation Context

**Calling Program Information:**
- `CDEMO-FROM-TRANID` - Transaction ID of the program that invoked COACTUPC
- `CDEMO-FROM-PROGRAM` - Program name that invoked COACTUPC

**Target Program Information:**
- `CDEMO-TO-TRANID` - Transaction ID to transfer to on exit
- `CDEMO-TO-PROGRAM` - Program name to transfer to on exit

**Default Exit Target:**
- Program: COMEN01C (main menu)
- Transaction: CMEN
- Used when: No calling program specified or F3 pressed without prior navigation context

### Session Information

**User Context:**
- `CDEMO-USER-ID` - Logged-in user identifier
- `CDEMO-USER-TYPE` - User type indicator (admin vs. regular)
- `CDEMO-PGM-CONTEXT` - Program context (entry vs. re-entry)

**Last Screen Displayed:**
- `CDEMO-LAST-MAPSET` - Last mapset displayed
- `CDEMO-LAST-MAP` - Last map displayed
- `CDEMO-LAST-AID` - Last attention identifier (key pressed)

### Data Context Passed Between Programs

**Account/Customer Context:**
- `CDEMO-ACCT-ID` - Current account number being processed
- `CDEMO-CARD-NUM` - Current card number
- `CDEMO-CUST-ID` - Current customer ID
- `CDEMO-CUST-FNAME`, `CDEMO-CUST-MNAME`, `CDEMO-CUST-LNAME` - Customer name components
- `CDEMO-ACCT-STATUS` - Account status

### Data Received FROM Calling Program

When COACTUPC is invoked by another program:
- Calling program/transaction identification (FROM fields)
- Optional pre-filled account number in `CDEMO-ACCT-ID`
- User session information (user ID, user type)
- Navigation context for return path

**Auto-Fetch Behavior:** If `CDEMO-ACCT-ID` contains an account number, the program automatically fetches and displays the account data without requiring the user to enter the account number.

### Data Passed TO Target Program on Exit

When COACTUPC transfers control (F3 exit):
- Updated account/customer context (IDs, names, status)
- Navigation context with COACTUPC recorded as FROM program
- Session information preserved
- Last screen information updated

**Navigation Breadcrumb:** The program updates the FROM fields to record COACTUPC as the calling program, enabling the target program to return if needed.

### External Program Dependencies

**COMEN01C - Main Menu Program**
- **Relationship:** Default exit target when F3 pressed and no calling program specified
- **Transaction:** CMEN
- **Data Passed:** CARDDEMO-COMMAREA with navigation context
- **Purpose:** Returns user to main menu after account update operations

**Dynamic Calling Program**
- **Source:** CDEMO-FROM-PROGRAM in communication area
- **Relationship:** Program that invoked COACTUPC
- **Exit Behavior:** When F3 pressed, returns to calling program if specified
- **Data Passed:** Updated CARDDEMO-COMMAREA with account/customer context
- **Purpose:** Enables flexible navigation, allowing integration into various business workflows

---

## Program State Transitions

### State Flow Diagram

```
[Initial Entry]
     |
     | User enters account number
     v
[Fetch Account Data]
     |
     ├─ Account Not Found → Return to [Initial Entry]
     |
     | Account Found
     v
[Show Details]
     |
     ├─ F3 → Exit to calling program/menu
     ├─ F12 → Return to [Initial Entry]
     |
     | User modifies fields, presses ENTER
     v
[Validate Changes]
     |
     ├─ Validation Fails → Return to [Show Details] with errors
     |
     | Validation Passes
     v
[Changes OK - Awaiting Confirmation]
     |
     ├─ F12 → Cancel changes, return to [Show Details]
     |
     | F5 → Save
     v
[Lock Records]
     |
     ├─ Cannot Lock → [Lock Error] → Return to [Show Details]
     |
     | Records Locked
     v
[Check Concurrent Changes]
     |
     ├─ Data Changed → Release locks, return to [Show Details] with current data
     |
     | No Concurrent Changes
     v
[Update Records]
     |
     ├─ Account Update Fails → [Update Failed] → Return to [Show Details]
     ├─ Customer Update Fails → Rollback, [Update Failed] → Return to [Show Details]
     |
     | Both Updates Succeed
     v
[Update Complete]
     |
     v
Return to [Initial Entry] (ready for next account)
```

### State Descriptions

**ACUP-DETAILS-NOT-FETCHED (Initial State)**
- Program is awaiting account number input
- Screen displays blank form with account number field enabled
- Only account number validation is performed
- Valid action: Enter account number and submit

**ACUP-SHOW-DETAILS (Display State)**
- Account and customer data has been fetched and displayed
- All fields are enabled for modification
- User can view current data or make changes
- Valid actions: Modify fields and submit, F3 (exit), F12 (cancel/new account)

**ACUP-CHANGES-MADE (Modification State)**
- User has modified one or more fields
- Changes have been submitted but not validated yet
- Program will perform field validation
- Transitions to: ACUP-CHANGES-OK-NOT-CONFIRMED (if valid) or ACUP-CHANGES-NOT-OK (if invalid)

**ACUP-CHANGES-NOT-OK (Validation Error State)**
- One or more validation errors detected
- Error messages displayed for invalid fields
- User must correct errors and resubmit
- Valid actions: Correct errors and resubmit, F12 (cancel changes)

**ACUP-CHANGES-OK-NOT-CONFIRMED (Confirmation State)**
- All changes validated successfully
- Program prompts user to confirm save operation
- Message: "Press F5 to save or F12 to cancel"
- Valid actions: F5 (save), F12 (cancel changes)

**ACUP-CHANGES-OKAYED-LOCK-ERROR (Lock Failure State)**
- User confirmed save but could not lock records
- Another user/process has the record locked
- Error message displayed
- Valid actions: Retry later, F12 (cancel changes)

**ACUP-CHANGES-OKAYED-BUT-FAILED (Update Failure State)**
- Records were locked but update operation failed
- Database operation encountered an error
- Any partial updates have been rolled back
- Valid actions: Review data, retry, or F12 (cancel)

**ACUP-CHANGES-OKAYED-AND-DONE (Success State)**
- Both account and customer records updated successfully
- Success message displayed
- Program resets to initial state for next account
- Automatic transition: Return to ACUP-DETAILS-NOT-FETCHED

**ACUP-CHANGES-FAILED (General Failure State)**
- Update process failed for unspecified reason
- Error message displayed
- Program resets to initial state
- Automatic transition: Return to ACUP-DETAILS-NOT-FETCHED

---

## Data Transformations

### Date Handling

**Storage Format:** YYYY-MM-DD (10 bytes, e.g., "2024-03-15")

**Screen Display Format:** Separated components (YYYY / MM / DD)

**Validation:** Year (4 digits), Month (2 digits), Day (2 digits) validated separately

**Assembly for Update:** Date components are reassembled using string concatenation:
```
Concatenate: Year + '-' + Month + '-' + Day → "YYYY-MM-DD"
```

**Disassembly for Display:** Date string is parsed into components:
```
"YYYY-MM-DD" → Year (4 chars) + Month (2 chars) + Day (2 chars)
```

**Applied To:**
- ACCT-OPEN-DATE
- ACCT-EXPIRAION-DATE
- ACCT-REISSUE-DATE
- CUST-DOB-YYYY-MM-DD

### Phone Number Formatting

**Input Format:** Three separate fields
- Area code (3 digits)
- Prefix (3 digits)
- Line number (4 digits)

**Storage Format:** (999)999-9999 (15 bytes, e.g., "(415)555-1234")

**Assembly for Update:** Phone components are concatenated:
```
Concatenate: '(' + AreaCode + ')' + Prefix + '-' + LineNumber → "(999)999-9999"
```

**Disassembly for Display:** Phone string is parsed:
```
"(999)999-9999" → AreaCode (3 digits) + Prefix (3 digits) + LineNumber (4 digits)
```

**Applied To:**
- CUST-PHONE-NUM-1
- CUST-PHONE-NUM-2

### Currency Formatting

**Display Format:** -,---,---,--9.99 (with commas, sign, and 2 decimal places)

**Storage Format:** S9(10)V99 COMP-3 (packed decimal)

**Conversion for Input:** Numeric validation with optional sign and decimal point, converted to packed decimal

**Conversion for Display:** Packed decimal formatted with commas, sign, and decimal point

**Applied To:**
- ACCT-CURR-BAL
- ACCT-CREDIT-LIMIT
- ACCT-CASH-CREDIT-LIMIT
- ACCT-CURR-CYC-CREDIT
- ACCT-CURR-CYC-DEBIT

### SSN Handling

**Input Format:** Three separate fields
- Part 1 (3 digits)
- Part 2 (2 digits)
- Part 3 (4 digits)

**Storage Format:** 9(09) COMP (9-digit numeric, e.g., 123456789)

**Assembly for Update:** SSN components are concatenated as numeric value:
```
Numeric concatenation: Part1 (3 digits) + Part2 (2 digits) + Part3 (4 digits) → 9-digit number
```

**Disassembly for Display:** 9-digit number is split:
```
123456789 → Part1 (123) + Part2 (45) + Part3 (6789)
```

**Applied To:** CUST-SSN

---

## Transaction Integrity

### Two-Phase Update Pattern

The program ensures that account and customer data are updated atomically - either both succeed or neither is updated.

**Phase 1: Lock Both Records**
1. Lock account record for exclusive update
2. Lock customer record for exclusive update
3. If either lock fails, release any acquired locks and exit with error

**Phase 2: Validate and Update**
1. Validate optimistic locking (detect concurrent changes)
2. Update account record
3. Update customer record
4. If customer update fails: Issue transaction rollback to undo account update
5. If both succeed: Transaction commits automatically

**Rollback Scenario:**
If the customer record update fails after the account record was already updated successfully, the program issues a rollback command. This undoes the account update, ensuring data consistency. The error message "Update failed" is displayed and the user must retry the operation.

### Optimistic Locking Implementation

**Purpose:** Detect concurrent updates by multiple users to prevent lost updates.

**Pattern:**
1. User views data at time T1 (stored in ACUP-OLD-*)
2. User modifies data (stored in ACUP-NEW-*)
3. On save at time T2, lock records with exclusive update lock
4. Compare locked record values against ACUP-OLD-* values
5. If any field changed between T1 and T2, another user modified the data
6. Reject update and show current data
7. If unchanged, proceed with update using ACUP-NEW-* values

**Concurrent Change Detection:**
The program compares every field in both account and customer records. If any field differs from the original fetched value, the DATA-WAS-CHANGED-BEFORE-UPDATE flag is set, locks are released, current data is displayed, and the user is notified: "Data was changed by another user. Please review and retry."

This prevents the classic "lost update" problem where two users edit the same record concurrently and the second user's changes overwrite the first user's changes without awareness.

### Commit and Rollback Behavior

**Automatic Commit:**
- When the program returns successfully after both updates, pending changes commit automatically
- No explicit commit command needed for success case

**Explicit Commit:**
- Before transferring control to another program (F3 exit), pending changes are explicitly committed
- Ensures data is saved before navigation to another program

**Explicit Rollback:**
- When customer update fails after account update succeeded
- Undoes the account update to maintain atomicity
- Leaves database in original state as if no update was attempted

---

## End of Cleaned Documentation

This cleaned documentation preserves all business logic, data structures, validation rules, and program flow information needed for migrating the COACTUPC account update program to a modern platform. All mainframe-specific implementation details (CICS commands, VSAM operations, BMS map specifications) have been removed while retaining the essential business requirements and data definitions.
