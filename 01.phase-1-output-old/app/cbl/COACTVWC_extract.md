# COBOL CICS Program Analysis: COACTVWC

**Program ID:** COACTVWC  
**Transaction ID:** CAVW  
**Mapset:** COACTVW  
**Map:** CACTVWA  
**Purpose:** Accept and process Account View request - Display account and customer details (READ-ONLY)

---

## 1. Screen Visualization

```
┌────────────────────────────────────────────────────────────────────────────┐
│Tran:CAVW        CardDemo Account View                        Date:mm/dd/yy │
│Prog:COACTVWC    Secure Credit Card System                    Time:hh:mm:ss │
│                                                                              │
│                             View Account                                     │
│                  Account Number : ___________   Active Y/N: _               │
│       Opened :__________                        Credit Limit        :       │
│       Expiry :__________                                   +ZZZ,ZZZ,ZZZ.99  │
│       Reissue:__________                        Cash credit Limit   :       │
│                                                            +ZZZ,ZZZ,ZZZ.99  │
│       Account Group:__________                  Current Balance     :       │
│                                                            +ZZZ,ZZZ,ZZZ.99  │
│                                                 Current Cycle Credit:       │
│                                                            +ZZZ,ZZZ,ZZZ.99  │
│                                                 Current Cycle Debit :       │
│                                                            +ZZZ,ZZZ,ZZZ.99  │
│                            Customer Details                                  │
│       Customer id  :_________            SSN:____________                   │
│       Date of birth:__________           FICO Score:___                     │
│       First Name             Middle Name:            Last Name :            │
│       _________________________  _________________________  ________________│
│       Address:__________________________________________________  State __   │
│                 __________________________________________________  Zip _____│
│       City __________________________________________________  Country ___   │
│       Phone 1:_____________       Government Issued Id Ref    : ____________│
│       Phone 2:_____________       EFT Account Id: __________                │
│                                   Primary Card Holder Y/N:_                 │
│                                                                              │
│                         Enter or update id of account to display            │
│ ERROR MESSAGE AREA                                                          │
│  F3=Exit                                                                    │
└────────────────────────────────────────────────────────────────────────────┘
```

### Screen Layout Details

The screen is a standard 24-row by 80-column 3270 terminal display. Key characteristics:

- **Header Section (Lines 1-2)**: Transaction ID, screen title, program name, date, and time
- **Title Section (Line 4)**: "View Account" centered
- **Input Section (Line 5)**: Single input field for Account Number (11 digits, MUSTFILL validation)
- **Account Information (Lines 5-10)**: Display-only fields for account status, dates, limits, and balances
- **Customer Information (Lines 11-20)**: Display-only fields for customer demographics and contact information
- **Message Area (Line 22)**: Informational messages (45 characters)
- **Error Area (Line 23)**: Error messages in red text (78 characters)
- **Navigation (Line 24)**: Function key indicators

---

## 2. Field Details Table

### Screen Fields Mapping

| Line | Column | Field Name | Type | Length | Data Source | Attribute |
|------|--------|------------|------|--------|-------------|-----------|
| 1 | 1 | TRNNAME | Display | 4 | Transaction ID 'CAVW' | ASKIP, FSET, NORM, BLUE |
| 1 | 21 | TITLE01 | Display | 40 | Screen Title | ASKIP, NORM, YELLOW |
| 1 | 71 | CURDATE | Display | 8 | Current Date | ASKIP, NORM, BLUE |
| 2 | 7 | PGMNAME | Display | 8 | Program Name 'COACTVWC' | ASKIP, NORM, BLUE |
| 2 | 21 | TITLE02 | Display | 40 | Screen Subtitle | ASKIP, NORM, YELLOW |
| 2 | 71 | CURTIME | Display | 8 | Current Time | ASKIP, NORM, BLUE |
| 5 | 38 | ACCTSID | Input | 11 | Account Number | FSET, IC, NORM, UNPROT, GREEN, UNDERLINE, MUSTFILL |
| 5 | 70 | ACSTTUS | Display | 1 | Account Active Status | ASKIP, UNDERLINE |
| 6 | 17 | ADTOPEN | Display | 10 | Account Open Date | UNDERLINE |
| 6 | 61 | ACRDLIM | Display | 15 | Credit Limit | UNDERLINE, RIGHT, PICOUT |
| 7 | 17 | AEXPDT | Display | 10 | Account Expiry Date | UNDERLINE |
| 7 | 61 | ACSHLIM | Display | 15 | Cash Credit Limit | UNDERLINE, RIGHT, PICOUT |
| 8 | 17 | AREISDT | Display | 10 | Account Reissue Date | UNDERLINE |
| 8 | 61 | ACURBAL | Display | 15 | Current Balance | UNDERLINE, RIGHT, PICOUT |
| 9 | 61 | ACRCYCR | Display | 15 | Current Cycle Credit | UNDERLINE, RIGHT, PICOUT |
| 10 | 23 | AADDGRP | Display | 10 | Account Group ID | UNDERLINE |
| 10 | 61 | ACRCYDB | Display | 15 | Current Cycle Debit | UNDERLINE, RIGHT, PICOUT |
| 12 | 23 | ACSTNUM | Display | 9 | Customer ID | UNDERLINE, TURQUOISE |
| 12 | 54 | ACSTSSN | Display | 12 | Customer SSN | UNDERLINE, TURQUOISE |
| 13 | 23 | ACSTDOB | Display | 10 | Customer Date of Birth | UNDERLINE, TURQUOISE |
| 13 | 61 | ACSTFCO | Display | 3 | Customer FICO Score | UNDERLINE, TURQUOISE |
| 15 | 1 | ACSFNAM | Display | 25 | Customer First Name | UNDERLINE, TURQUOISE |
| 15 | 28 | ACSMNAM | Display | 25 | Customer Middle Name | UNDERLINE, TURQUOISE |
| 15 | 55 | ACSLNAM | Display | 25 | Customer Last Name | UNDERLINE, TURQUOISE |
| 16 | 10 | ACSADL1 | Display | 50 | Address Line 1 | UNDERLINE, TURQUOISE |
| 16 | 73 | ACSSTTE | Display | 2 | State Code | UNDERLINE, TURQUOISE |
| 17 | 10 | ACSADL2 | Display | 50 | Address Line 2 | UNDERLINE, TURQUOISE |
| 17 | 73 | ACSZIPC | Display | 5 | Zip Code | UNDERLINE, RIGHT, TURQUOISE |
| 18 | 10 | ACSCITY | Display | 50 | City | UNDERLINE, TURQUOISE |
| 18 | 73 | ACSCTRY | Display | 3 | Country Code | UNDERLINE, TURQUOISE |
| 19 | 10 | ACSPHN1 | Display | 13 | Phone Number 1 | UNDERLINE, TURQUOISE |
| 19 | 58 | ACSGOVT | Display | 20 | Government Issued ID | UNDERLINE, TURQUOISE |
| 20 | 10 | ACSPHN2 | Display | 13 | Phone Number 2 | UNDERLINE, TURQUOISE |
| 20 | 41 | ACSEFTC | Display | 10 | EFT Account ID | UNDERLINE, TURQUOISE |
| 20 | 78 | ACSPFLG | Display | 1 | Primary Card Holder Flag | UNDERLINE, TURQUOISE |
| 22 | 23 | INFOMSG | Display | 45 | Information Message | PROT, NEUTRAL |
| 23 | 1 | ERRMSG | Display | 78 | Error Message | ASKIP, BRT, FSET, RED |

### Field Validation Rules

**ACCTSID (Account Number Input)**
- Required field (MUSTFILL attribute)
- Must be numeric
- Must be 11 digits
- Must be non-zero
- Validation performed in paragraph 2210-EDIT-ACCOUNT

**Display Fields**
- All display fields are protected (ASKIP attribute)
- Populated from ACCTDAT (Account Master) and CUSTDAT (Customer Master) files
- Formatted using PICOUT attributes where applicable (monetary amounts)
- SSN displayed with formatting (XXX-XX-XXXX)

---

## 3. Program Structure

### Program Identification

```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID.
    COACTVWC.
```

**Program Name:** COACTVWC  
**Transaction ID:** CAVW (Line 146)  
**Mapset:** COACTVW (Line 148)  
**Map:** CACTVWA (Line 150)  
**Program Type:** Online CICS Transaction Processing (READ-ONLY)  
**Business Function:** Credit Card Account View - Display account and customer information

### Copybooks Used

The program uses 15 copybooks organized by functionality:

**1. CVCRD01Y (Line 207)**
- Card-related working storage variables
- Common card data structures

**2. COCOM01Y (Line 211)**
- Application COMMAREA structure
- Inter-program communication area
- Contains calling program context, user type, navigation state

**3. DFHBMSCA (Line 221)**
- IBM-supplied BMS attribute definitions
- Field attribute constants (ASKIP, UNPROT, FSET, etc.)
- Color definitions (BLUE, RED, GREEN, etc.)

**4. DFHAID (Line 222)**
- IBM-supplied AID (Attention Identifier) key definitions
- Function key constants (ENTER, PF03, etc.)

**5. COTTL01Y (Line 226)**
- Common screen title definitions
- Standard header/footer text

**6. COACTVW (Line 229)**
- BMS map copybook for CACTVWA screen
- Defines input and output symbolic map structures

**7. CSDAT01Y (Line 232)**
- Current date/time handling structures
- Date formatting utilities

**8. CSMSG01Y (Line 235)**
- Common message definitions
- Standard error messages

**9. CSMSG02Y (Line 238)**
- Abend handling variables
- Error tracking structures

**10. CSUSR01Y (Line 241)**
- Signed-on user data
- User session information

**11. CVACT01Y (Line 244)**
- Account record layout (ACCTDAT file)
- Account Master file structure

**12. CVACT02Y (Line 248)**
- Secondary account record structures
- Additional account-related layouts

**13. CVACT03Y (Line 251)**
- Card cross-reference record layout
- CARDDAT/CXACAIX alternate index structure

**14. CVCUS01Y (Line 254)**
- Customer record layout (CUSTDAT file)
- Customer Master file structure

**15. CSSTRPFY (Line 913)**
- PF key storage and remapping logic
- Common PF key handling routine

### Working Storage Variables

**CICS Processing Variables (Lines 39-45)**
```cobol
05 WS-CICS-PROCESSNG-VARS.
   07 WS-RESP-CD                PIC S9(09) COMP VALUE ZEROS.
   07 WS-REAS-CD                PIC S9(09) COMP VALUE ZEROS.
   07 WS-TRANID                 PIC X(4)    VALUE SPACES.
```

**Input Validation Flags (Lines 50-65)**
```cobol
05  WS-INPUT-FLAG               PIC X(1).
  88  INPUT-OK                  VALUE '0'.
  88  INPUT-ERROR               VALUE '1'.
05  WS-EDIT-ACCT-FLAG           PIC X(1).
  88  FLG-ACCTFILTER-NOT-OK     VALUE '0'.
  88  FLG-ACCTFILTER-ISVALID    VALUE '1'.
  88  FLG-ACCTFILTER-BLANK      VALUE ' '.
05  WS-EDIT-CUST-FLAG           PIC X(1).
  88  FLG-CUSTFILTER-NOT-OK     VALUE '0'.
  88  FLG-CUSTFILTER-ISVALID    VALUE '1'.
  88  FLG-CUSTFILTER-BLANK      VALUE ' '.
```

**File I/O Structures (Lines 73-85)**
```cobol
05  WS-XREF-RID.
  10  WS-CARD-RID-CARDNUM       PIC X(16).
  10  WS-CARD-RID-CUST-ID       PIC 9(09).
  10  WS-CARD-RID-ACCT-ID       PIC 9(11).
05  WS-FILE-READ-FLAGS.
  10 WS-ACCOUNT-MASTER-READ-FLAG  PIC X(1).
     88 FOUND-ACCT-IN-MASTER      VALUE '1'.
  10 WS-CUST-MASTER-READ-FLAG     PIC X(1).
     88 FOUND-CUST-IN-MASTER      VALUE '1'.
```

**Message Variables (Lines 109-138)**
```cobol
05  WS-LONG-MSG                   PIC X(500).
05  WS-INFO-MSG                   PIC X(40).
  88  WS-NO-INFO-MESSAGE          VALUES SPACES LOW-VALUES.
  88  WS-PROMPT-FOR-INPUT         VALUE 'Enter or update id of account to display'.
  88  WS-INFORM-OUTPUT            VALUE 'Displaying details of given Account'.
05  WS-RETURN-MSG                 PIC X(75).
  88  WS-RETURN-MSG-OFF           VALUE SPACES.
  88  WS-PROMPT-FOR-ACCT          VALUE 'Account number not provided'.
  88  NO-SEARCH-CRITERIA-RECEIVED VALUE 'No input received'.
  88  SEARCHED-ACCT-ZEROES        VALUE 'Account number must be a non zero 11 digit number'.
  88  SEARCHED-ACCT-NOT-NUMERIC   VALUE 'Account number must be a non zero 11 digit number'.
  88  DID-NOT-FIND-ACCT-IN-CARDXREF  VALUE 'Did not find this account in account card xref file'.
  88  DID-NOT-FIND-ACCT-IN-ACCTDAT   VALUE 'Did not find this account in account master file'.
  88  DID-NOT-FIND-CUST-IN-CUSTDAT   VALUE 'Did not find associated customer in master file'.
```

**Program Constants (Lines 142-203)**
```cobol
01 WS-LITERALS.
   05 LIT-THISPGM                PIC X(8)  VALUE 'COACTVWC'.
   05 LIT-THISTRANID             PIC X(4)  VALUE 'CAVW'.
   05 LIT-THISMAPSET             PIC X(8)  VALUE 'COACTVW '.
   05 LIT-THISMAP                PIC X(7)  VALUE 'CACTVWA'.
   05 LIT-ACCTFILENAME           PIC X(8)  VALUE 'ACCTDAT '.
   05 LIT-CARDFILENAME           PIC X(8)  VALUE 'CARDDAT '.
   05 LIT-CUSTFILENAME           PIC X(8)  VALUE 'CUSTDAT '.
   05 LIT-CARDXREFNAME-ACCT-PATH PIC X(8)  VALUE 'CXACAIX '.
```

### Paragraph Structure

**Main Processing Flow**
- 0000-MAIN (Line 262): Main program entry point
- COMMON-RETURN (Line 394): Common return logic
- ABEND-ROUTINE (Line 916): Abnormal termination handler

**Screen Management**
- 1000-SEND-MAP (Line 416): Main screen sending orchestrator
- 1100-SCREEN-INIT (Line 431): Initialize screen fields
- 1200-SETUP-SCREEN-VARS (Line 460): Populate screen data from files
- 1300-SETUP-SCREEN-ATTRS (Line 541): Set field attributes and cursor position
- 1400-SEND-SCREEN (Line 577): Execute SEND MAP command

**Input Processing**
- 2000-PROCESS-INPUTS (Line 596): Main input processing orchestrator
- 2100-RECEIVE-MAP (Line 610): Receive screen input
- 2200-EDIT-MAP-INPUTS (Line 622): Validate all inputs
- 2210-EDIT-ACCOUNT (Line 649): Validate account number field

**File Access**
- 9000-READ-ACCT (Line 687): Main file read orchestrator
- 9200-GETCARDXREF-BYACCT (Line 723): Read card cross-reference by account
- 9300-GETACCTDATA-BYACCT (Line 774): Read account master by account
- 9400-GETCUSTDATA-BYCUST (Line 825): Read customer master by customer

**Utility Routines**
- SEND-PLAIN-TEXT (Line 877): Send plain text message (debugging)
- SEND-LONG-TEXT (Line 896): Send long text message (debugging)
- YYYY-STORE-PFKEY (from CSSTRPFY copybook): PF key storage routine

---

## 4. CICS Commands

### 4.1 HANDLE ABEND (Line 264)

**Purpose:** Establish abend handling routine for unexpected errors

**Code:**
```cobol
EXEC CICS HANDLE ABEND
          LABEL(ABEND-ROUTINE)
END-EXEC
```

**Parameters:**
- LABEL: Points to ABEND-ROUTINE paragraph (line 916)

**Usage Context:**
- Executed at program initialization in 0000-MAIN
- Ensures all abends are caught and handled gracefully
- Allows custom error message display before termination

**Migration Notes:**
- Modern frameworks should implement global exception handlers
- Consider centralized error logging and user-friendly error pages

---

### 4.2 XCTL (Line 349)

**Purpose:** Transfer control to calling program or main menu

**Code:**
```cobol
EXEC CICS XCTL
          PROGRAM (CDEMO-TO-PROGRAM)
          COMMAREA(CARDDEMO-COMMAREA)
END-EXEC
```

**Parameters:**
- PROGRAM: Dynamic program name (CDEMO-TO-PROGRAM from COMMAREA)
- COMMAREA: Application communication area

**Usage Context:**
- Executed when user presses F3 (PFK03)
- Transfers to calling program if available, otherwise to main menu (COMEN01C)
- Preserves COMMAREA for maintaining application context

**Migration Notes:**
- Replace with navigation routing/state management
- Implement proper session management
- Consider RESTful navigation patterns

---

### 4.3 RETURN (Line 402)

**Purpose:** Return control to CICS with pseudo-conversational design

**Code:**
```cobol
EXEC CICS RETURN
     TRANSID (LIT-THISTRANID)
     COMMAREA (WS-COMMAREA)
     LENGTH(LENGTH OF WS-COMMAREA)
END-EXEC
```

**Parameters:**
- TRANSID: Transaction ID 'CAVW' - indicates next transaction to invoke
- COMMAREA: Working storage communication area (2000 bytes)
- LENGTH: Length of COMMAREA

**Usage Context:**
- Executed at COMMON-RETURN (line 394) after processing
- Implements pseudo-conversational pattern
- Preserves program state between user interactions
- Releases resources while waiting for user input

**Additional RETURN Commands:**
- Line 885: RETURN without TRANSID (terminal return in SEND-PLAIN-TEXT)
- Line 904: RETURN without TRANSID (terminal return in SEND-LONG-TEXT)

**Migration Notes:**
- Replace with stateless HTTP request/response model
- Use session management for state preservation
- Consider JWT tokens or session cookies for context
- Implement proper resource cleanup

---

### 4.4 SEND MAP (Line 583)

**Purpose:** Send formatted screen to terminal

**Code:**
```cobol
EXEC CICS SEND MAP(CCARD-NEXT-MAP)
              MAPSET(CCARD-NEXT-MAPSET)
              FROM(CACTVWAO)
              CURSOR
              ERASE
              FREEKB
              RESP(WS-RESP-CD)
END-EXEC
```

**Parameters:**
- MAP: Map name 'CACTVWA'
- MAPSET: Mapset name 'COACTVW'
- FROM: Output symbolic map structure (CACTVWAO)
- CURSOR: Position cursor based on field attributes
- ERASE: Clear screen before sending
- FREEKB: Free keyboard for input
- RESP: Response code

**Usage Context:**
- Executed in 1400-SEND-SCREEN paragraph (line 577)
- Called after populating screen fields in 1200-SETUP-SCREEN-VARS
- Displays account and customer information
- Positions cursor at ACCTSID field

**Migration Notes:**
- Replace with HTML template rendering
- Use responsive web design frameworks
- Implement field-level validation on client side
- Consider accessibility requirements (WCAG standards)

---

### 4.5 RECEIVE MAP (Line 611)

**Purpose:** Receive user input from terminal

**Code:**
```cobol
EXEC CICS RECEIVE MAP(LIT-THISMAP)
          MAPSET(LIT-THISMAPSET)
          INTO(CACTVWAI)
          RESP(WS-RESP-CD)
          RESP2(WS-REAS-CD)
END-EXEC
```

**Parameters:**
- MAP: Map name 'CACTVWA'
- MAPSET: Mapset name 'COACTVW'
- INTO: Input symbolic map structure (CACTVWAI)
- RESP: Response code
- RESP2: Reason code

**Usage Context:**
- Executed in 2100-RECEIVE-MAP paragraph (line 610)
- Called when user presses ENTER or other AID key
- Retrieves account number input from ACCTSID field
- Part of pseudo-conversational flow

**Migration Notes:**
- Replace with HTTP POST/GET request handling
- Parse form data or JSON payload
- Implement server-side validation
- Use framework-specific request binding

---

### 4.6 READ - Card Cross-Reference (Line 727)

**Purpose:** Read card cross-reference file using account ID as key

**Code:**
```cobol
EXEC CICS READ
     DATASET   (LIT-CARDXREFNAME-ACCT-PATH)
     RIDFLD    (WS-CARD-RID-ACCT-ID-X)
     KEYLENGTH (LENGTH OF WS-CARD-RID-ACCT-ID-X)
     INTO      (CARD-XREF-RECORD)
     LENGTH    (LENGTH OF CARD-XREF-RECORD)
     RESP      (WS-RESP-CD)
     RESP2     (WS-REAS-CD)
END-EXEC
```

**Parameters:**
- DATASET: 'CXACAIX' - Card cross-reference alternate index
- RIDFLD: Account ID (11 digits)
- KEYLENGTH: 11 bytes
- INTO: CARD-XREF-RECORD structure
- LENGTH: Record length
- RESP/RESP2: Response and reason codes

**Usage Context:**
- Executed in 9200-GETCARDXREF-BYACCT paragraph (line 723)
- Uses alternate index path to access CARDDAT by account ID
- Retrieves customer ID and card number for the account
- First step in the read sequence (card xref → account → customer)

**Response Handling:**
- DFHRESP(NORMAL): Record found, extract customer ID and card number
- DFHRESP(NOTFND): Account not in cross-reference file
- OTHER: File access error

**Migration Notes:**
- Replace with database query on card cross-reference table
- Use SQL: SELECT cust_id, card_num FROM card_xref WHERE acct_id = ?
- Implement proper connection pooling
- Add database exception handling

---

### 4.7 READ - Account Master (Line 776)

**Purpose:** Read account master file by account ID

**Code:**
```cobol
EXEC CICS READ
     DATASET   (LIT-ACCTFILENAME)
     RIDFLD    (WS-CARD-RID-ACCT-ID-X)
     KEYLENGTH (LENGTH OF WS-CARD-RID-ACCT-ID-X)
     INTO      (ACCOUNT-RECORD)
     LENGTH    (LENGTH OF ACCOUNT-RECORD)
     RESP      (WS-RESP-CD)
     RESP2     (WS-REAS-CD)
END-EXEC
```

**Parameters:**
- DATASET: 'ACCTDAT' - Account Master file
- RIDFLD: Account ID (11 digits)
- KEYLENGTH: 11 bytes
- INTO: ACCOUNT-RECORD structure
- LENGTH: Record length
- RESP/RESP2: Response and reason codes

**Usage Context:**
- Executed in 9300-GETACCTDATA-BYACCT paragraph (line 774)
- Reads complete account record including:
  * Account status
  * Open/expiry/reissue dates
  * Credit limits (regular and cash)
  * Current balance
  * Current cycle credit/debit
  * Account group ID
- Second step in read sequence

**Response Handling:**
- DFHRESP(NORMAL): Record found, set FOUND-ACCT-IN-MASTER flag
- DFHRESP(NOTFND): Account not in master file
- OTHER: File access error

**Migration Notes:**
- Replace with database query on account table
- Use SQL: SELECT * FROM accounts WHERE acct_id = ?
- Map COBOL structure to database columns
- Consider using ORM frameworks

---

### 4.8 READ - Customer Master (Line 826)

**Purpose:** Read customer master file by customer ID

**Code:**
```cobol
EXEC CICS READ
     DATASET   (LIT-CUSTFILENAME)
     RIDFLD    (WS-CARD-RID-CUST-ID-X)
     KEYLENGTH (LENGTH OF WS-CARD-RID-CUST-ID-X)
     INTO      (CUSTOMER-RECORD)
     LENGTH    (LENGTH OF CUSTOMER-RECORD)
     RESP      (WS-RESP-CD)
     RESP2     (WS-REAS-CD)
END-EXEC
```

**Parameters:**
- DATASET: 'CUSTDAT' - Customer Master file
- RIDFLD: Customer ID (9 digits)
- KEYLENGTH: 9 bytes
- INTO: CUSTOMER-RECORD structure
- LENGTH: Record length
- RESP/RESP2: Response and reason codes

**Usage Context:**
- Executed in 9400-GETCUSTDATA-BYCUST paragraph (line 825)
- Reads complete customer record including:
  * Personal information (name, SSN, DOB)
  * Contact information (phone, address)
  * Financial information (FICO score, EFT account)
  * Primary card holder indicator
- Final step in read sequence (after card xref and account reads)
- Customer ID obtained from XREF-CUST-ID in previous read

**Response Handling:**
- DFHRESP(NORMAL): Record found, set FOUND-CUST-IN-MASTER flag
- DFHRESP(NOTFND): Customer not in master file
- OTHER: File access error

**Migration Notes:**
- Replace with database query on customer table
- Use SQL: SELECT * FROM customers WHERE cust_id = ?
- Consider JOIN queries to fetch all related data in one call
- Implement caching strategy for frequently accessed customer data

---

### 4.9 SEND TEXT (Line 878)

**Purpose:** Send plain text message for debugging/error display

**Code:**
```cobol
EXEC CICS SEND TEXT
          FROM(WS-RETURN-MSG)
          LENGTH(LENGTH OF WS-RETURN-MSG)
          ERASE
          FREEKB
END-EXEC
```

**Parameters:**
- FROM: Error message text (75 bytes)
- LENGTH: Message length
- ERASE: Clear screen
- FREEKB: Free keyboard

**Usage Context:**
- Executed in SEND-PLAIN-TEXT paragraph (line 877)
- Used for debugging and unexpected error scenarios
- Followed by RETURN without TRANSID (line 885)
- Displays plain text without BMS map formatting

**Additional SEND TEXT:**
- Line 897: SEND-LONG-TEXT for longer messages (500 bytes)

**Migration Notes:**
- Replace with error page rendering
- Use HTTP error codes appropriately
- Implement consistent error message formatting
- Consider error logging to separate system

---

### 4.10 SEND (Line 924) - Abend Data

**Purpose:** Send abend information before abnormal termination

**Code:**
```cobol
EXEC CICS SEND
              FROM (ABEND-DATA)
              LENGTH(LENGTH OF ABEND-DATA)
              NOHANDLE
END-EXEC
```

**Parameters:**
- FROM: ABEND-DATA structure (contains error details)
- LENGTH: Data length
- NOHANDLE: Suppress error handling

**Usage Context:**
- Executed in ABEND-ROUTINE paragraph (line 916)
- Displays error information to user before abend
- Uses NOHANDLE to prevent recursive error handling
- Followed by HANDLE ABEND CANCEL and ABEND commands

**Migration Notes:**
- Replace with exception handling and error logging
- Display user-friendly error page
- Log technical details to monitoring system
- Implement graceful degradation

---

### 4.11 HANDLE ABEND CANCEL (Line 930)

**Purpose:** Cancel abend handling before issuing ABEND command

**Code:**
```cobol
EXEC CICS HANDLE ABEND
     CANCEL
END-EXEC
```

**Usage Context:**
- Executed in ABEND-ROUTINE after displaying error
- Prevents recursive abend handling
- Prepares for controlled abnormal termination

**Migration Notes:**
- Not directly applicable in modern frameworks
- Ensure exception handlers don't cause infinite loops

---

### 4.12 ABEND (Line 934)

**Purpose:** Force abnormal termination with specific abend code

**Code:**
```cobol
EXEC CICS ABEND
     ABCODE('9999')
END-EXEC
```

**Parameters:**
- ABCODE: Abend code '9999' (application-defined error)

**Usage Context:**
- Executed in ABEND-ROUTINE after error display
- Controlled termination for unrecoverable errors
- Code '9999' indicates application-detected error

**Migration Notes:**
- Replace with proper exception throwing
- Use HTTP 500 status for server errors
- Implement circuit breaker patterns for fault tolerance

---

### Summary of CICS Commands

| Command | Location | Purpose | Parameters |
|---------|----------|---------|------------|
| HANDLE ABEND | Line 264 | Set up abend handler | LABEL |
| XCTL | Line 349 | Transfer to another program | PROGRAM, COMMAREA |
| RETURN | Line 402 | Pseudo-conversational return | TRANSID, COMMAREA, LENGTH |
| SEND MAP | Line 583 | Display screen | MAP, MAPSET, FROM, CURSOR, ERASE, FREEKB |
| RECEIVE MAP | Line 611 | Get user input | MAP, MAPSET, INTO |
| READ | Line 727 | Read card xref by account | DATASET, RIDFLD, INTO |
| READ | Line 776 | Read account master | DATASET, RIDFLD, INTO |
| READ | Line 826 | Read customer master | DATASET, RIDFLD, INTO |
| SEND TEXT | Line 878 | Display text message | FROM, LENGTH, ERASE, FREEKB |
| SEND | Line 924 | Send abend data | FROM, LENGTH, NOHANDLE |
| HANDLE ABEND | Line 930 | Cancel abend handling | CANCEL |
| ABEND | Line 934 | Force abnormal termination | ABCODE |

---

## 5. Navigational Details

### Function Keys

**ENTER Key**
- **Action:** Process account number input and display account details
- **Flow:**
  1. Validate account number (must be 11-digit non-zero numeric)
  2. Read card cross-reference to get customer ID
  3. Read account master to get account details
  4. Read customer master to get customer details
  5. Populate and display screen with all information
- **Error Handling:** Display validation or file not found errors

**F3 (PFK03)**
- **Action:** Exit to calling program or main menu
- **Flow:**
  1. Check CDEMO-FROM-TRANID in COMMAREA
  2. If available, XCTL to calling program
  3. Otherwise, XCTL to main menu (COMEN01C/CM00)
  4. Preserve COMMAREA context for navigation

### Program State Management

**State Indicators:**
```cobol
CDEMO-PGM-ENTER     - Initial entry into program
CDEMO-PGM-REENTER   - Returning to program after user input
```

**Navigation Context in COMMAREA:**
```cobol
CDEMO-FROM-PROGRAM  - Calling program name
CDEMO-FROM-TRANID   - Calling transaction ID
CDEMO-TO-PROGRAM    - Target program for XCTL
CDEMO-TO-TRANID     - Target transaction ID
CDEMO-LAST-MAPSET   - Last mapset displayed
CDEMO-LAST-MAP      - Last map displayed
```

### Screen Flow Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                     Program Entry Point                      │
│                      (Transaction CAVW)                      │
└───────────────────────┬─────────────────────────────────────┘
                        │
                        ▼
            ┌───────────────────────┐
            │   Check COMMAREA      │
            │   EIBCALEN = 0?       │
            └───────┬───────────────┘
                    │
        ┌───────────┴──────────────┐
        │ Yes                      │ No
        ▼                          ▼
┌───────────────┐         ┌────────────────────┐
│ CDEMO-PGM-    │         │ CDEMO-PGM-REENTER? │
│ ENTER = TRUE  │         └─────┬──────────────┘
└───────┬───────┘               │
        │                        │
        │                ┌───────┴────────┐
        │                │ Yes            │ No (from Menu)
        │                ▼                ▼
        │         ┌──────────────┐  ┌──────────────┐
        │         │ Process User │  │ Initialize   │
        │         │ Input (ENTER)│  │ COMMAREA     │
        │         └──────┬───────┘  └──────┬───────┘
        │                │                 │
        │                ▼                 │
        │         ┌──────────────┐        │
        │         │ Validate Acct│        │
        │         │ Number       │        │
        │         └──────┬───────┘        │
        │                │                 │
        │         ┌──────┴────────┐       │
        │         │ Valid?        │       │
        │         └───┬───────────┘       │
        │             │                   │
        │      ┌──────┴──────┐            │
        │      │ Yes         │ No         │
        │      ▼             ▼            │
        │  ┌────────┐  ┌─────────┐       │
        │  │ Read   │  │ Display │       │
        │  │ Files  │  │ Error   │       │
        │  └────┬───┘  └────┬────┘       │
        │       │           │             │
        │       ▼           │             │
        │  ┌────────────┐   │             │
        │  │ Card Xref  │   │             │
        │  │ → Account  │   │             │
        │  │ → Customer │   │             │
        │  └─────┬──────┘   │             │
        │        │           │             │
        └────────┴───────────┴─────────────┘
                 │
                 ▼
        ┌────────────────┐
        │ Send Screen    │
        │ (SEND MAP)     │
        └────────┬───────┘
                 │
                 ▼
        ┌────────────────┐
        │ Return to CICS │
        │ (Pseudo-conv)  │
        └────────┬───────┘
                 │
        ┌────────┴────────┐
        │                 │
        ▼                 ▼
    ┌────────┐      ┌─────────┐
    │ User   │      │  F3     │
    │ Enters │      │ Pressed │
    │ ENTER  │      └────┬────┘
    └────┬───┘           │
         │               ▼
         │        ┌──────────────┐
         │        │ XCTL to      │
         │        │ Calling Pgm  │
         │        │ or Menu      │
         │        └──────────────┘
         │
         └───► (Returns to Process User Input)
```

### Related Programs

**Menu Program (COMEN01C)**
- Transaction ID: CM00
- Mapset: COMEN01
- Map: COMEN1A
- Called on F3 exit when no calling program context

**Card List Program (COCRDLIC)**
- Transaction ID: CCLI
- Mapset: COCRDLI
- Map: CCRDSLA
- Referenced but not directly called from this program

**Card Update Program (COCRDUPC)**
- Transaction ID: CCUP
- Mapset: COCRDUP
- Map: CCRDUPA
- Referenced but not directly called from this program

**Card Detail Program (COCRDSLC)**
- Transaction ID: CCDL
- Mapset: COCRDSL
- Map: CCRDSLA
- Referenced but not directly called from this program

### Navigation Patterns

**1. Initial Entry Flow:**
- User invokes transaction CAVW from menu or command line
- Program initializes with empty COMMAREA
- Displays blank screen with prompt: "Enter or update id of account to display"
- Cursor positioned at Account Number input field

**2. Account View Flow:**
- User enters 11-digit account number
- User presses ENTER
- Program validates input
- If valid, reads three files sequentially (card xref → account → customer)
- Displays complete account and customer information
- User can view details and press F3 to exit

**3. Error Flow:**
- If validation fails or files not found:
  * Display error message in red at line 23
  * Position cursor at Account Number field
  * Allow user to re-enter or press F3 to exit

**4. Exit Flow:**
- User presses F3 at any time
- Program transfers control (XCTL) to:
  * Calling program if CDEMO-FROM-PROGRAM is set
  * Main menu (COMEN01C) if no calling program
- COMMAREA preserved for context

---

## 6. Business Logic and Program Execution Flow

### Program Initialization (0000-MAIN, Line 262)

**Step 1: Error Handler Setup**
```cobol
EXEC CICS HANDLE ABEND
      LABEL(ABEND-ROUTINE)
END-EXEC
```
Establishes global abend handler to catch any unexpected errors during execution.

**Step 2: Initialize Working Storage**
```cobol
INITIALIZE CC-WORK-AREA
           WS-MISC-STORAGE
           WS-COMMAREA
```
Clears all working storage areas to ensure clean state for processing.

**Step 3: Store Transaction Context**
```cobol
MOVE LIT-THISTRANID TO WS-TRANID
```
Sets transaction ID 'CAVW' for reference throughout execution.

**Step 4: Clear Error Messages**
```cobol
SET WS-RETURN-MSG-OFF TO TRUE
```
Ensures no leftover error messages from previous invocations.

### COMMAREA Management (Lines 282-293)

**Decision Logic:**
```cobol
IF EIBCALEN IS EQUAL TO 0
OR (CDEMO-FROM-PROGRAM = LIT-MENUPGM
    AND NOT CDEMO-PGM-REENTER)
   INITIALIZE CARDDEMO-COMMAREA
              WS-THIS-PROGCOMMAREA
ELSE
   MOVE DFHCOMMAREA (1:LENGTH OF CARDDEMO-COMMAREA) TO
                     CARDDEMO-COMMAREA
   MOVE DFHCOMMAREA(LENGTH OF CARDDEMO-COMMAREA + 1:
                    LENGTH OF WS-THIS-PROGCOMMAREA) TO
                     WS-THIS-PROGCOMMAREA
END-IF
```

**Scenarios:**
1. **First-time invocation (EIBCALEN = 0):**
   - No COMMAREA passed
   - Initialize all communication areas
   - Program state = PGM-ENTER

2. **From menu without re-entry:**
   - Called from menu (COMEN01C)
   - Not a return from user input
   - Initialize COMMAREA
   - Program state = PGM-ENTER

3. **Pseudo-conversational return:**
   - COMMAREA contains previous state
   - Restore application context
   - Restore program-specific context
   - Program state = PGM-REENTER

### PF Key Remapping (Lines 299-300)

```cobol
PERFORM YYYY-STORE-PFKEY
   THRU YYYY-STORE-PFKEY-EXIT
```

Maps physical PF keys to logical function keys and stores in COMMAREA for consistent handling across the application.

### AID Key Validation (Lines 306-314)

```cobol
SET PFK-INVALID TO TRUE
IF CCARD-AID-ENTER OR
   CCARD-AID-PFK03
   SET PFK-VALID TO TRUE
END-IF

IF PFK-INVALID
   SET CCARD-AID-ENTER TO TRUE
END-IF
```

**Valid Keys:**
- ENTER: Process account view request
- F3 (PFK03): Exit to calling program or menu

**Invalid Keys:**
- Any other key press is treated as ENTER
- Ensures program always responds to user input

### Main Decision Logic (Lines 323-383)

**EVALUATE TRUE Structure:**

**Case 1: F3 Key Pressed (Lines 324-352)**
```cobol
WHEN CCARD-AID-PFK03
   IF CDEMO-FROM-TRANID EQUAL LOW-VALUES
   OR CDEMO-FROM-TRANID EQUAL SPACES
      MOVE LIT-MENUTRANID TO CDEMO-TO-TRANID
   ELSE
      MOVE CDEMO-FROM-TRANID TO CDEMO-TO-TRANID
   END-IF
   
   IF CDEMO-FROM-PROGRAM EQUAL LOW-VALUES
   OR CDEMO-FROM-PROGRAM EQUAL SPACES
      MOVE LIT-MENUPGM TO CDEMO-TO-PROGRAM
   ELSE
      MOVE CDEMO-FROM-PROGRAM TO CDEMO-TO-PROGRAM
   END-IF
   
   MOVE LIT-THISTRANID TO CDEMO-FROM-TRANID
   MOVE LIT-THISPGM TO CDEMO-FROM-PROGRAM
   
   SET CDEMO-USRTYP-USER TO TRUE
   SET CDEMO-PGM-ENTER TO TRUE
   MOVE LIT-THISMAPSET TO CDEMO-LAST-MAPSET
   MOVE LIT-THISMAP TO CDEMO-LAST-MAP
   
   EXEC CICS XCTL
             PROGRAM (CDEMO-TO-PROGRAM)
             COMMAREA(CARDDEMO-COMMAREA)
   END-EXEC
```

**Business Logic:**
- User wants to exit current screen
- Determine target program:
  * Use calling program if available in COMMAREA
  * Default to main menu (COMEN01C) if no calling program
- Update COMMAREA with navigation context
- Transfer control via XCTL (does not return)

**Case 2: Initial Entry (Lines 353-360)**
```cobol
WHEN CDEMO-PGM-ENTER
   PERFORM 1000-SEND-MAP THRU
           1000-SEND-MAP-EXIT
   GO TO COMMON-RETURN
```

**Business Logic:**
- First time displaying this screen
- Send blank map with prompt
- Return to CICS to wait for user input

**Case 3: User Re-entry (Lines 361-374)**
```cobol
WHEN CDEMO-PGM-REENTER
   PERFORM 2000-PROCESS-INPUTS
      THRU 2000-PROCESS-INPUTS-EXIT
   IF INPUT-ERROR
      PERFORM 1000-SEND-MAP
         THRU 1000-SEND-MAP-EXIT
      GO TO COMMON-RETURN
   ELSE
      PERFORM 9000-READ-ACCT
         THRU 9000-READ-ACCT-EXIT
      PERFORM 1000-SEND-MAP
         THRU 1000-SEND-MAP-EXIT
      GO TO COMMON-RETURN
   END-IF
```

**Business Logic:**
- User entered account number and pressed ENTER
- Process and validate input
- If validation fails:
  * Display error message
  * Re-send map for correction
  * Return to CICS
- If validation succeeds:
  * Read account data from files
  * Populate screen fields
  * Send map with data
  * Return to CICS

**Case 4: Unexpected Scenario (Lines 375-382)**
```cobol
WHEN OTHER
   MOVE LIT-THISPGM TO ABEND-CULPRIT
   MOVE '0001' TO ABEND-CODE
   MOVE SPACES TO ABEND-REASON
   MOVE 'UNEXPECTED DATA SCENARIO' TO WS-RETURN-MSG
   PERFORM SEND-PLAIN-TEXT
      THRU SEND-PLAIN-TEXT-EXIT
```

**Business Logic:**
- Should never happen in normal operation
- Defensive programming for data corruption
- Display error message and terminate gracefully

### Input Processing Flow (2000-PROCESS-INPUTS, Line 596)

**Step 1: Receive Map (2100-RECEIVE-MAP, Line 610)**
```cobol
EXEC CICS RECEIVE MAP(LIT-THISMAP)
          MAPSET(LIT-THISMAPSET)
          INTO(CACTVWAI)
          RESP(WS-RESP-CD)
          RESP2(WS-REAS-CD)
END-EXEC
```

Retrieves user input from terminal into input symbolic map structure.

**Step 2: Edit Map Inputs (2200-EDIT-MAP-INPUTS, Line 622)**

**Initialize Validation Flags:**
```cobol
SET INPUT-OK TO TRUE
SET FLG-ACCTFILTER-ISVALID TO TRUE
```

**Handle Wildcard/Blank Input:**
```cobol
IF ACCTSIDI OF CACTVWAI = '*'
OR ACCTSIDI OF CACTVWAI = SPACES
   MOVE LOW-VALUES TO CC-ACCT-ID
ELSE
   MOVE ACCTSIDI OF CACTVWAI TO CC-ACCT-ID
END-IF
```

**Step 3: Validate Account Number (2210-EDIT-ACCOUNT, Line 649)**

**Validation Rule 1: Required Field**
```cobol
IF CC-ACCT-ID EQUAL LOW-VALUES
OR CC-ACCT-ID EQUAL SPACES
   SET INPUT-ERROR TO TRUE
   SET FLG-ACCTFILTER-BLANK TO TRUE
   IF WS-RETURN-MSG-OFF
      SET WS-PROMPT-FOR-ACCT TO TRUE
   END-IF
   MOVE ZEROES TO CDEMO-ACCT-ID
   GO TO 2210-EDIT-ACCOUNT-EXIT
END-IF
```
- Account number must be provided
- Cannot be blank or asterisk
- Error message: "Account number not provided"

**Validation Rule 2: Numeric and Non-Zero**
```cobol
IF CC-ACCT-ID IS NOT NUMERIC
OR CC-ACCT-ID EQUAL ZEROES
   SET INPUT-ERROR TO TRUE
   SET FLG-ACCTFILTER-NOT-OK TO TRUE
   IF WS-RETURN-MSG-OFF
      MOVE 'Account Filter must be a non-zero 11 digit number'
           TO WS-RETURN-MSG
   END-IF
   MOVE ZERO TO CDEMO-ACCT-ID
   GO TO 2210-EDIT-ACCOUNT-EXIT
ELSE
   MOVE CC-ACCT-ID TO CDEMO-ACCT-ID
   SET FLG-ACCTFILTER-ISVALID TO TRUE
END-IF
```
- Must be numeric (0-9 only)
- Must be non-zero
- Must be 11 digits (enforced by field length)
- Error message: "Account Filter must be a non-zero 11 digit number"

**Step 4: Cross-Field Validation**
```cobol
IF FLG-ACCTFILTER-BLANK
   SET NO-SEARCH-CRITERIA-RECEIVED TO TRUE
END-IF
```
Ensures at least one search criterion provided (only account number in this program).

### File Access Flow (9000-READ-ACCT, Line 687)

**Step 1: Clear Info Message**
```cobol
SET WS-NO-INFO-MESSAGE TO TRUE
MOVE CDEMO-ACCT-ID TO WS-CARD-RID-ACCT-ID
```

**Step 2: Read Card Cross-Reference (9200-GETCARDXREF-BYACCT, Line 723)**

```cobol
EXEC CICS READ
     DATASET   (LIT-CARDXREFNAME-ACCT-PATH)
     RIDFLD    (WS-CARD-RID-ACCT-ID-X)
     KEYLENGTH (LENGTH OF WS-CARD-RID-ACCT-ID-X)
     INTO      (CARD-XREF-RECORD)
     LENGTH    (LENGTH OF CARD-XREF-RECORD)
     RESP      (WS-RESP-CD)
     RESP2     (WS-REAS-CD)
END-EXEC

EVALUATE WS-RESP-CD
   WHEN DFHRESP(NORMAL)
      MOVE XREF-CUST-ID TO CDEMO-CUST-ID
      MOVE XREF-CARD-NUM TO CDEMO-CARD-NUM
   WHEN DFHRESP(NOTFND)
      SET INPUT-ERROR TO TRUE
      SET FLG-ACCTFILTER-NOT-OK TO TRUE
      STRING 'Account:' WS-CARD-RID-ACCT-ID-X
             ' not found in Cross ref file. Resp:'
             ERROR-RESP ' Reas:' ERROR-RESP2
             DELIMITED BY SIZE
             INTO WS-RETURN-MSG
      END-STRING
   WHEN OTHER
      SET INPUT-ERROR TO TRUE
      SET FLG-ACCTFILTER-NOT-OK TO TRUE
      MOVE 'READ' TO ERROR-OPNAME
      MOVE LIT-CARDXREFNAME-ACCT-PATH TO ERROR-FILE
      MOVE WS-FILE-ERROR-MESSAGE TO WS-RETURN-MSG
END-EVALUATE
```

**Business Logic:**
- Uses alternate index (CXACAIX) to access CARDDAT by account ID
- Retrieves customer ID associated with the account
- If not found, account doesn't exist or has no cards
- If file error, report technical issue

**Step 3: Read Account Master (9300-GETACCTDATA-BYACCT, Line 774)**

```cobol
EXEC CICS READ
     DATASET   (LIT-ACCTFILENAME)
     RIDFLD    (WS-CARD-RID-ACCT-ID-X)
     KEYLENGTH (LENGTH OF WS-CARD-RID-ACCT-ID-X)
     INTO      (ACCOUNT-RECORD)
     LENGTH    (LENGTH OF ACCOUNT-RECORD)
     RESP      (WS-RESP-CD)
     RESP2     (WS-REAS-CD)
END-EXEC

EVALUATE WS-RESP-CD
   WHEN DFHRESP(NORMAL)
      SET FOUND-ACCT-IN-MASTER TO TRUE
   WHEN DFHRESP(NOTFND)
      SET INPUT-ERROR TO TRUE
      SET FLG-ACCTFILTER-NOT-OK TO TRUE
      STRING 'Account:' WS-CARD-RID-ACCT-ID-X
             ' not found in Acct Master file.Resp:'
             ERROR-RESP ' Reas:' ERROR-RESP2
             DELIMITED BY SIZE
             INTO WS-RETURN-MSG
      END-STRING
   WHEN OTHER
      SET INPUT-ERROR TO TRUE
      SET FLG-ACCTFILTER-NOT-OK TO TRUE
      MOVE 'READ' TO ERROR-OPNAME
      MOVE LIT-ACCTFILENAME TO ERROR-FILE
      MOVE WS-FILE-ERROR-MESSAGE TO WS-RETURN-MSG
END-EVALUATE
```

**Business Logic:**
- Direct read of ACCTDAT using account ID as primary key
- Loads complete account record into ACCOUNT-RECORD structure
- Account data includes:
  * Active status (Y/N)
  * Open, expiry, reissue dates
  * Credit limits (regular and cash)
  * Current balance and cycle amounts
  * Account group ID

**Step 4: Read Customer Master (9400-GETCUSTDATA-BYCUST, Line 825)**

```cobol
EXEC CICS READ
     DATASET   (LIT-CUSTFILENAME)
     RIDFLD    (WS-CARD-RID-CUST-ID-X)
     KEYLENGTH (LENGTH OF WS-CARD-RID-CUST-ID-X)
     INTO      (CUSTOMER-RECORD)
     LENGTH    (LENGTH OF CUSTOMER-RECORD)
     RESP      (WS-RESP-CD)
     RESP2     (WS-REAS-CD)
END-EXEC

EVALUATE WS-RESP-CD
   WHEN DFHRESP(NORMAL)
      SET FOUND-CUST-IN-MASTER TO TRUE
   WHEN DFHRESP(NOTFND)
      SET INPUT-ERROR TO TRUE
      SET FLG-CUSTFILTER-NOT-OK TO TRUE
      STRING 'CustId:' WS-CARD-RID-CUST-ID-X
             ' not found in customer master.Resp: '
             ERROR-RESP ' REAS:' ERROR-RESP2
             DELIMITED BY SIZE
             INTO WS-RETURN-MSG
      END-STRING
   WHEN OTHER
      SET INPUT-ERROR TO TRUE
      SET FLG-CUSTFILTER-NOT-OK TO TRUE
      MOVE 'READ' TO ERROR-OPNAME
      MOVE LIT-CUSTFILENAME TO ERROR-FILE
      MOVE WS-FILE-ERROR-MESSAGE TO WS-RETURN-MSG
END-EVALUATE
```

**Business Logic:**
- Uses customer ID obtained from card cross-reference
- Loads complete customer record into CUSTOMER-RECORD structure
- Customer data includes:
  * Personal information (name, SSN, DOB, FICO score)
  * Address (line 1, line 2, city, state, zip, country)
  * Contact information (phone 1, phone 2)
  * Financial data (government ID, EFT account)
  * Primary card holder indicator

### Screen Display Flow (1000-SEND-MAP, Line 416)

**Step 1: Screen Initialization (1100-SCREEN-INIT, Line 431)**

```cobol
MOVE LOW-VALUES TO CACTVWAO
MOVE FUNCTION CURRENT-DATE TO WS-CURDATE-DATA

MOVE CCDA-TITLE01 TO TITLE01O OF CACTVWAO
MOVE CCDA-TITLE02 TO TITLE02O OF CACTVWAO
MOVE LIT-THISTRANID TO TRNNAMEO OF CACTVWAO
MOVE LIT-THISPGM TO PGMNAMEO OF CACTVWAO

MOVE WS-CURDATE-MM-DD-YY TO CURDATEO OF CACTVWAO
MOVE WS-CURTIME-HH-MM-SS TO CURTIMEO OF CACTVWAO
```

**Business Logic:**
- Clear output map to low-values (all fields blank)
- Get current system date/time
- Populate header fields:
  * Transaction ID: CAVW
  * Program name: COACTVWC
  * Screen titles from COTTL01Y copybook
  * Current date in MM/DD/YY format
  * Current time in HH:MM:SS format

**Step 2: Setup Screen Variables (1200-SETUP-SCREEN-VARS, Line 460)**

**Initial Entry Logic:**
```cobol
IF EIBCALEN = 0
   SET WS-PROMPT-FOR-INPUT TO TRUE
ELSE
   IF FLG-ACCTFILTER-BLANK
      MOVE LOW-VALUES TO ACCTSIDO OF CACTVWAO
   ELSE
      MOVE CC-ACCT-ID TO ACCTSIDO OF CACTVWAO
   END-IF
```

**Populate Account Fields (if found):**
```cobol
IF FOUND-ACCT-IN-MASTER OR FOUND-CUST-IN-MASTER
   MOVE ACCT-ACTIVE-STATUS TO ACSTTUSO OF CACTVWAO
   MOVE ACCT-CURR-BAL TO ACURBALO OF CACTVWAO
   MOVE ACCT-CREDIT-LIMIT TO ACRDLIMO OF CACTVWAO
   MOVE ACCT-CASH-CREDIT-LIMIT TO ACSHLIMO OF CACTVWAO
   MOVE ACCT-CURR-CYC-CREDIT TO ACRCYCRO OF CACTVWAO
   MOVE ACCT-CURR-CYC-DEBIT TO ACRCYDBO OF CACTVWAO
   MOVE ACCT-OPEN-DATE TO ADTOPENO OF CACTVWAO
   MOVE ACCT-EXPIRAION-DATE TO AEXPDTO OF CACTVWAO
   MOVE ACCT-REISSUE-DATE TO AREISDTO OF CACTVWAO
   MOVE ACCT-GROUP-ID TO AADDGRPO OF CACTVWAO
END-IF
```

**Populate Customer Fields (if found):**
```cobol
IF FOUND-CUST-IN-MASTER
   MOVE CUST-ID TO ACSTNUMO OF CACTVWAO
   
   STRING CUST-SSN(1:3) '-' CUST-SSN(4:2) '-' CUST-SSN(6:4)
          DELIMITED BY SIZE
          INTO ACSTSSNO OF CACTVWAO
   END-STRING
   
   MOVE CUST-FICO-CREDIT-SCORE TO ACSTFCOO OF CACTVWAO
   MOVE CUST-DOB-YYYY-MM-DD TO ACSTDOBO OF CACTVWAO
   MOVE CUST-FIRST-NAME TO ACSFNAMO OF CACTVWAO
   MOVE CUST-MIDDLE-NAME TO ACSMNAMO OF CACTVWAO
   MOVE CUST-LAST-NAME TO ACSLNAMO OF CACTVWAO
   MOVE CUST-ADDR-LINE-1 TO ACSADL1O OF CACTVWAO
   MOVE CUST-ADDR-LINE-2 TO ACSADL2O OF CACTVWAO
   MOVE CUST-ADDR-LINE-3 TO ACSCITYO OF CACTVWAO
   MOVE CUST-ADDR-STATE-CD TO ACSSTTEO OF CACTVWAO
   MOVE CUST-ADDR-ZIP TO ACSZIPCO OF CACTVWAO
   MOVE CUST-ADDR-COUNTRY-CD TO ACSCTRYO OF CACTVWAO
   MOVE CUST-PHONE-NUM-1 TO ACSPHN1O OF CACTVWAO
   MOVE CUST-PHONE-NUM-2 TO ACSPHN2O OF CACTVWAO
   MOVE CUST-GOVT-ISSUED-ID TO ACSGOVTO OF CACTVWAO
   MOVE CUST-EFT-ACCOUNT-ID TO ACSEFTCO OF CACTVWAO
   MOVE CUST-PRI-CARD-HOLDER-IND TO ACSPFLGO OF CACTVWAO
END-IF
```

**Business Logic:**
- If initial entry, set prompt message
- If re-entry with input, display account number
- If files read successfully:
  * Populate all account fields from ACCOUNT-RECORD
  * Populate all customer fields from CUSTOMER-RECORD
  * Format SSN with dashes (XXX-XX-XXXX)
  * Monetary amounts use BMS PICOUT for formatting

**Setup Messages:**
```cobol
IF WS-NO-INFO-MESSAGE
   SET WS-PROMPT-FOR-INPUT TO TRUE
END-IF

MOVE WS-RETURN-MSG TO ERRMSGO OF CACTVWAO
MOVE WS-INFO-MSG TO INFOMSGO OF CACTVWAO
```

**Step 3: Setup Screen Attributes (1300-SETUP-SCREEN-ATTRS, Line 541)**

```cobol
MOVE DFHBMFSE TO ACCTSIDA OF CACTVWAI

EVALUATE TRUE
   WHEN FLG-ACCTFILTER-NOT-OK
   WHEN FLG-ACCTFILTER-BLANK
      MOVE -1 TO ACCTSIDL OF CACTVWAI
   WHEN OTHER
      MOVE -1 TO ACCTSIDL OF CACTVWAI
END-EVALUATE

MOVE DFHDFCOL TO ACCTSIDC OF CACTVWAO

IF FLG-ACCTFILTER-NOT-OK
   MOVE DFHRED TO ACCTSIDC OF CACTVWAO
END-IF

IF FLG-ACCTFILTER-BLANK AND CDEMO-PGM-REENTER
   MOVE '*' TO ACCTSIDO OF CACTVWAO
   MOVE DFHRED TO ACCTSIDC OF CACTVWAO
END-IF

IF WS-NO-INFO-MESSAGE
   MOVE DFHBMDAR TO INFOMSGC OF CACTVWAO
ELSE
   MOVE DFHNEUTR TO INFOMSGC OF CACTVWAO
END-IF
```

**Business Logic:**
- Set cursor position to account number field (ACCTSIDL = -1)
- Apply field attributes:
  * DFHBMFSE: Field separator (start of entry)
  * DFHDFCOL: Default color
  * DFHRED: Red color for errors
  * DFHBMDAR: Dark attribute (hidden)
  * DFHNEUTR: Neutral color
- Highlight errors in red
- Show asterisk if field was blank on re-entry

**Step 4: Send Screen (1400-SEND-SCREEN, Line 577)**

```cobol
MOVE LIT-THISMAPSET TO CCARD-NEXT-MAPSET
MOVE LIT-THISMAP TO CCARD-NEXT-MAP
SET CDEMO-PGM-REENTER TO TRUE

EXEC CICS SEND MAP(CCARD-NEXT-MAP)
              MAPSET(CCARD-NEXT-MAPSET)
              FROM(CACTVWAO)
              CURSOR
              ERASE
              FREEKB
              RESP(WS-RESP-CD)
END-EXEC
```

**Business Logic:**
- Update COMMAREA with current map context
- Set program state to PGM-REENTER for next invocation
- Execute SEND MAP:
  * CURSOR: Position based on attributes
  * ERASE: Clear screen before displaying
  * FREEKB: Free keyboard for user input
  * FROM: Output symbolic map with populated data

### Common Return Logic (COMMON-RETURN, Line 394)

```cobol
MOVE WS-RETURN-MSG TO CCARD-ERROR-MSG

MOVE CARDDEMO-COMMAREA TO WS-COMMAREA
MOVE WS-THIS-PROGCOMMAREA TO
     WS-COMMAREA(LENGTH OF CARDDEMO-COMMAREA + 1:
                  LENGTH OF WS-THIS-PROGCOMMAREA)

EXEC CICS RETURN
     TRANSID (LIT-THISTRANID)
     COMMAREA (WS-COMMAREA)
     LENGTH(LENGTH OF WS-COMMAREA)
END-EXEC
```

**Business Logic:**
- Save error message in COMMAREA
- Combine application COMMAREA and program-specific COMMAREA
- Return to CICS with:
  * TRANSID 'CAVW': Next transaction to invoke
  * COMMAREA: Preserved state (2000 bytes)
- Releases all resources
- Waits for user input (pseudo-conversational)

### Abend Handling (ABEND-ROUTINE, Line 916)

```cobol
IF ABEND-MSG EQUAL LOW-VALUES
   MOVE 'UNEXPECTED ABEND OCCURRED.' TO ABEND-MSG
END-IF

MOVE LIT-THISPGM TO ABEND-CULPRIT

EXEC CICS SEND
              FROM (ABEND-DATA)
              LENGTH(LENGTH OF ABEND-DATA)
              NOHANDLE
END-EXEC

EXEC CICS HANDLE ABEND
     CANCEL
END-EXEC

EXEC CICS ABEND
     ABCODE('9999')
END-EXEC
```

**Business Logic:**
- Display error information to user
- Identify culprit program (COACTVWC)
- Cancel abend handling to avoid recursion
- Issue controlled abend with code '9999'
- Transaction terminates abnormally

---

## 7. Data Structures and Sources

### File Structures

**1. ACCTDAT - Account Master File**

Primary Key: ACCT-ID (11 digits)

Record Layout (from CVACT01Y copybook):
```cobol
01 ACCOUNT-RECORD.
   05 ACCT-ID                    PIC 9(11).
   05 ACCT-ACTIVE-STATUS         PIC X(01).
   05 ACCT-CURR-BAL              PIC S9(10)V99 COMP-3.
   05 ACCT-CREDIT-LIMIT          PIC S9(10)V99 COMP-3.
   05 ACCT-CASH-CREDIT-LIMIT     PIC S9(10)V99 COMP-3.
   05 ACCT-OPEN-DATE             PIC X(10).
   05 ACCT-EXPIRAION-DATE        PIC X(10).
   05 ACCT-REISSUE-DATE          PIC X(10).
   05 ACCT-CURR-CYC-CREDIT       PIC S9(10)V99 COMP-3.
   05 ACCT-CURR-CYC-DEBIT        PIC S9(10)V99 COMP-3.
   05 ACCT-GROUP-ID              PIC X(10).
```

**Business Purpose:**
- Stores credit card account information
- Tracks account status, balances, and limits
- Records account lifecycle dates
- Groups accounts for reporting

**Access Method:** VSAM KSDS (Key Sequenced Data Set)

**2. CUSTDAT - Customer Master File**

Primary Key: CUST-ID (9 digits)

Record Layout (from CVCUS01Y copybook):
```cobol
01 CUSTOMER-RECORD.
   05 CUST-ID                    PIC 9(09).
   05 CUST-FIRST-NAME            PIC X(25).
   05 CUST-MIDDLE-NAME           PIC X(25).
   05 CUST-LAST-NAME             PIC X(25).
   05 CUST-ADDR-LINE-1           PIC X(50).
   05 CUST-ADDR-LINE-2           PIC X(50).
   05 CUST-ADDR-LINE-3           PIC X(50).
   05 CUST-ADDR-STATE-CD         PIC X(02).
   05 CUST-ADDR-COUNTRY-CD       PIC X(03).
   05 CUST-ADDR-ZIP              PIC X(05).
   05 CUST-PHONE-NUM-1           PIC X(13).
   05 CUST-PHONE-NUM-2           PIC X(13).
   05 CUST-SSN                   PIC 9(09).
   05 CUST-GOVT-ISSUED-ID        PIC X(20).
   05 CUST-DOB-YYYY-MM-DD        PIC X(10).
   05 CUST-EFT-ACCOUNT-ID        PIC X(10).
   05 CUST-PRI-CARD-HOLDER-IND   PIC X(01).
   05 CUST-FICO-CREDIT-SCORE     PIC 9(03).
```

**Business Purpose:**
- Stores customer demographic information
- Maintains contact details for communication
- Records financial attributes (SSN, FICO)
- Tracks primary card holder designation

**Access Method:** VSAM KSDS (Key Sequenced Data Set)

**3. CARDDAT/CXACAIX - Card Cross-Reference File**

Primary Key: CARD-NUM (16 digits)
Alternate Index: XREF-ACCT-ID (11 digits) - Used by this program

Record Layout (from CVACT03Y copybook):
```cobol
01 CARD-XREF-RECORD.
   05 XREF-CARD-NUM              PIC X(16).
   05 XREF-CUST-ID               PIC 9(09).
   05 XREF-ACCT-ID               PIC 9(11).
```

**Business Purpose:**
- Links cards to accounts and customers
- Enables account-based queries (via alternate index)
- Supports card-to-customer association
- Facilitates cross-reference lookups

**Access Method:** VSAM KSDS with Alternate Index (CXACAIX)

### COMMAREA Structure

**Application-Level COMMAREA (from COCOM01Y copybook)**

```cobol
01 CARDDEMO-COMMAREA.
   05 CDEMO-FROM-TRANID          PIC X(04).
   05 CDEMO-FROM-PROGRAM         PIC X(08).
   05 CDEMO-TO-TRANID            PIC X(04).
   05 CDEMO-TO-PROGRAM           PIC X(08).
   05 CDEMO-USER-ID              PIC X(08).
   05 CDEMO-USER-TYPE            PIC X(01).
      88 CDEMO-USRTYP-USER       VALUE 'U'.
      88 CDEMO-USRTYP-ADMIN      VALUE 'A'.
   05 CDEMO-PGM-CONTEXT          PIC X(01).
      88 CDEMO-PGM-ENTER         VALUE 'E'.
      88 CDEMO-PGM-REENTER       VALUE 'R'.
   05 CDEMO-ACCT-ID              PIC 9(11).
   05 CDEMO-CUST-ID              PIC 9(09).
   05 CDEMO-CARD-NUM             PIC X(16).
   05 CDEMO-LAST-MAPSET          PIC X(07).
   05 CDEMO-LAST-MAP             PIC X(07).
   05 CCARD-AID-ENTER            PIC X(01).
   05 CCARD-AID-PFK03            PIC X(01).
   05 CCARD-NEXT-PROG            PIC X(08).
   05 CCARD-NEXT-MAPSET          PIC X(07).
   05 CCARD-NEXT-MAP             PIC X(07).
   05 CCARD-ERROR-MSG            PIC X(75).
```

**Business Purpose:**
- Maintains navigation context across programs
- Preserves user identity and type
- Stores current data keys (account, customer, card)
- Tracks screen flow and AID keys
- Carries error messages

**Program-Specific COMMAREA**

```cobol
01 WS-THIS-PROGCOMMAREA.
   05 CA-CALL-CONTEXT.
      10 CA-FROM-PROGRAM         PIC X(08).
      10 CA-FROM-TRANID          PIC X(04).
```

**Combined COMMAREA:** 2000 bytes total
- Application COMMAREA
- Program-specific context
- Preserved across pseudo-conversational calls

### BMS Map Structures

**Input Symbolic Map (CACTVWAI)**

```cobol
01 CACTVWAI.
   02 ACCTSIDL    PIC S9(4) COMP.
   02 ACCTSIDI    PIC X(11).
```

**Output Symbolic Map (CACTVWAO)**

```cobol
01 CACTVWAO.
   02 TRNNAMEO    PIC X(4).
   02 TITLE01O    PIC X(40).
   02 CURDATEO    PIC X(8).
   02 PGMNAMEO    PIC X(8).
   02 TITLE02O    PIC X(40).
   02 CURTIMEO    PIC X(8).
   02 ACCTSIDO    PIC X(11).
   02 ACSTTUSO    PIC X(1).
   02 ADTOPENO    PIC X(10).
   02 ACRDLIMO    PIC X(15).
   02 AEXPDTO     PIC X(10).
   02 ACSHLIMO    PIC X(15).
   02 AREISDTO    PIC X(10).
   02 ACURBALO    PIC X(15).
   02 ACRCYCRO    PIC X(15).
   02 AADDGRPO    PIC X(10).
   02 ACRCYDBO    PIC X(15).
   02 ACSTNUMO    PIC X(9).
   02 ACSTSSNO    PIC X(12).
   02 ACSTDOBO    PIC X(10).
   02 ACSTFCOO    PIC X(3).
   02 ACSFNAMO    PIC X(25).
   02 ACSMNAMO    PIC X(25).
   02 ACSLNAMO    PIC X(25).
   02 ACSADL1O    PIC X(50).
   02 ACSSTTEO    PIC X(2).
   02 ACSADL2O    PIC X(50).
   02 ACSZIPCO    PIC X(5).
   02 ACSCITYO    PIC X(50).
   02 ACSCTRYO    PIC X(3).
   02 ACSPHN1O    PIC X(13).
   02 ACSGOVTO    PIC X(20).
   02 ACSPHN2O    PIC X(13).
   02 ACSEFTCO    PIC X(10).
   02 ACSPFLGO    PIC X(1).
   02 INFOMSGO    PIC X(45).
   02 ERRMSGO     PIC X(78).
```

**Attribute Map Fields**

```cobol
02 ACCTSIDA    PIC X(1).    - Field attribute byte
02 ACCTSIDC    PIC X(1).    - Color attribute
02 ACCTSIDH    PIC X(1).    - Highlighting
02 INFOMSGC    PIC X(1).    - Info message color
```

### Data Relationships

```
┌─────────────────────────────────────────────────────────────┐
│                      Data Flow Diagram                       │
└─────────────────────────────────────────────────────────────┘

User Input (ACCTSID)
        │
        ▼
┌───────────────────┐
│ Account Number    │──────────────┐
│ (11 digits)       │              │
└───────────────────┘              │
                                   │
                                   ▼
                        ┌──────────────────────┐
                        │ CARDDAT/CXACAIX      │
                        │ (Alternate Index)    │
                        │ Key: XREF-ACCT-ID    │
                        └──────────┬───────────┘
                                   │
                                   │ Returns:
                                   │ - XREF-CUST-ID
                                   │ - XREF-CARD-NUM
                                   │
                    ┌──────────────┴──────────────┐
                    │                             │
                    ▼                             ▼
        ┌───────────────────┐         ┌───────────────────┐
        │ ACCTDAT           │         │ CUSTDAT           │
        │ Key: ACCT-ID      │         │ Key: CUST-ID      │
        └──────┬────────────┘         └──────┬────────────┘
               │                             │
               │ Returns:                    │ Returns:
               │ - Account Details           │ - Customer Details
               │ - Status, Dates             │ - Demographics
               │ - Limits, Balances          │ - Contact Info
               │                             │
               └──────────────┬──────────────┘
                              │
                              ▼
                     ┌─────────────────┐
                     │ Screen Display  │
                     │ (CACTVWAO)      │
                     └─────────────────┘
```

---

## 8. Dependencies

### External Programs

**1. COMEN01C (Main Menu Program)**
- **Transaction ID:** CM00
- **Mapset:** COMEN01
- **Map:** COMEN1A
- **Relationship:** Default exit destination when F3 pressed without calling program context
- **Communication:** XCTL with CARDDEMO-COMMAREA
- **Purpose:** Application main menu for navigation

**2. Calling Program (Dynamic)**
- **Program:** CDEMO-FROM-PROGRAM (from COMMAREA)
- **Transaction:** CDEMO-FROM-TRANID (from COMMAREA)
- **Relationship:** Program that invoked COACTVWC
- **Communication:** XCTL with CARDDEMO-COMMAREA on F3
- **Purpose:** Return to previous context in application flow

**Referenced Programs (Not Directly Called):**

**3. COCRDLIC (Card List Program)**
- **Transaction ID:** CCLI
- **Mapset:** COCRDLI
- **Map:** CCRDSLA
- **Purpose:** Display list of credit cards

**4. COCRDUPC (Card Update Program)**
- **Transaction ID:** CCUP
- **Mapset:** COCRDUP
- **Map:** CCRDUPA
- **Purpose:** Update credit card information

**5. COCRDSLC (Card Detail Program)**
- **Transaction ID:** CCDL
- **Mapset:** COCRDSL
- **Map:** CCRDSLA
- **Purpose:** Display credit card details

### VSAM Files

**1. ACCTDAT (Account Master)**
- **Type:** VSAM KSDS (Key Sequenced Data Set)
- **Primary Key:** ACCT-ID (11 bytes)
- **Access Mode:** Random by account ID
- **Operations:** READ only
- **Purpose:** Store account information including status, dates, limits, and balances

**2. CUSTDAT (Customer Master)**
- **Type:** VSAM KSDS
- **Primary Key:** CUST-ID (9 bytes)
- **Access Mode:** Random by customer ID
- **Operations:** READ only
- **Purpose:** Store customer demographic and contact information

**3. CARDDAT/CXACAIX (Card Cross-Reference)**
- **Type:** VSAM KSDS with Alternate Index
- **Primary Key:** XREF-CARD-NUM (16 bytes)
- **Alternate Index:** XREF-ACCT-ID (11 bytes) - Used by this program
- **Access Mode:** Random by account ID via alternate index
- **Operations:** READ only
- **Purpose:** Link cards to accounts and customers

### Copybooks

**System Copybooks:**
1. **DFHBMSCA** - CICS BMS attribute definitions
2. **DFHAID** - CICS attention identifier constants

**Application Copybooks:**
1. **CVCRD01Y** - Card-related working storage
2. **COCOM01Y** - Application COMMAREA structure
3. **COTTL01Y** - Screen title definitions
4. **COACTVW** - BMS map symbolic description
5. **CSDAT01Y** - Date/time handling structures
6. **CSMSG01Y** - Common message definitions
7. **CSMSG02Y** - Abend handling variables
8. **CSUSR01Y** - User session data
9. **CVACT01Y** - Account record layout
10. **CVACT02Y** - Additional account structures
11. **CVACT03Y** - Card cross-reference layout
12. **CVCUS01Y** - Customer record layout
13. **CSSTRPFY** - PF key storage routine

### CICS Resources

**Transaction:**
- **TRANID:** CAVW
- **Program:** COACTVWC
- **Type:** Conversational interface

**Terminal:**
- **Type:** 3270 display terminal
- **Screen Size:** 24 rows × 80 columns
- **Color Support:** Required (RED, GREEN, BLUE, YELLOW, TURQUOISE, NEUTRAL)
- **Extended Attributes:** Highlighting, validation

**Temporary Storage:**
- **None:** Program uses COMMAREA for state management

**Transient Data:**
- **None:** No TD queue access

### Migration Dependencies

**Database Migration:**
- Convert VSAM files to relational database tables
- Maintain referential integrity (account → customer via card xref)
- Implement indexes equivalent to VSAM keys and alternate indexes

**UI Framework:**
- HTML/CSS for screen layout
- JavaScript for client-side validation
- Responsive design for multiple devices

**Application Server:**
- Session management for pseudo-conversational pattern
- RESTful API for data access
- Connection pooling for database

**Security:**
- Authentication/authorization framework
- Secure data transmission (HTTPS)
- PII protection (SSN, account numbers)

---

## 9. Error Handling and Recovery

### Validation Errors

**1. Account Number Not Provided**

**Condition:**
```cobol
IF CC-ACCT-ID EQUAL LOW-VALUES
OR CC-ACCT-ID EQUAL SPACES
```

**Error Message:** "Account number not provided"

**Recovery:**
- Set INPUT-ERROR flag
- Set FLG-ACCTFILTER-BLANK flag
- Position cursor at ACCTSID field
- Display error in red
- Allow user to re-enter

**User Action Required:** Enter valid account number

---

**2. Account Number Invalid Format**

**Condition:**
```cobol
IF CC-ACCT-ID IS NOT NUMERIC
OR CC-ACCT-ID EQUAL ZEROES
```

**Error Message:** "Account Filter must be a non-zero 11 digit number"

**Recovery:**
- Set INPUT-ERROR flag
- Set FLG-ACCTFILTER-NOT-OK flag
- Position cursor at ACCTSID field
- Highlight field in red
- Allow user to re-enter

**User Action Required:** Enter 11-digit numeric non-zero value

---

### File Access Errors

**3. Account Not Found in Card Cross-Reference**

**Condition:**
```cobol
EXEC CICS READ DATASET(CXACAIX) ...
RESP(WS-RESP-CD)
END-EXEC

WHEN DFHRESP(NOTFND)
```

**Error Message:** "Account: [account-number] not found in Cross ref file. Resp: [resp] Reas: [reas]"

**Recovery:**
- Set INPUT-ERROR flag
- Set FLG-ACCTFILTER-NOT-OK flag
- Display detailed error message
- Position cursor at ACCTSID field
- Allow user to re-enter or exit

**Business Reason:** Account doesn't exist or has no associated cards

**User Action Required:** Verify account number or contact support

---

**4. Account Not Found in Account Master**

**Condition:**
```cobol
EXEC CICS READ DATASET(ACCTDAT) ...
RESP(WS-RESP-CD)
END-EXEC

WHEN DFHRESP(NOTFND)
```

**Error Message:** "Account: [account-number] not found in Acct Master file.Resp: [resp] Reas: [reas]"

**Recovery:**
- Set INPUT-ERROR flag
- Set FLG-ACCTFILTER-NOT-OK flag
- Display detailed error message
- Position cursor at ACCTSID field
- Allow user to re-enter or exit

**Business Reason:** Account record missing (data integrity issue)

**User Action Required:** Contact IT support for data investigation

---

**5. Customer Not Found in Customer Master**

**Condition:**
```cobol
EXEC CICS READ DATASET(CUSTDAT) ...
RESP(WS-RESP-CD)
END-EXEC

WHEN DFHRESP(NOTFND)
```

**Error Message:** "CustId: [customer-id] not found in customer master.Resp: [resp] REAS: [reas]"

**Recovery:**
- Set INPUT-ERROR flag
- Set FLG-CUSTFILTER-NOT-OK flag
- Display detailed error message
- Cannot display full account information
- Allow user to exit or search different account

**Business Reason:** Customer record missing (data integrity issue)

**User Action Required:** Contact IT support for data investigation

---

**6. File I/O Error**

**Condition:**
```cobol
EXEC CICS READ ...
RESP(WS-RESP-CD)
END-EXEC

WHEN OTHER
```

**Error Message:** "File Error: [operation] on [filename] returned RESP [resp-code], RESP2 [reas-code]"

**Recovery:**
- Set INPUT-ERROR flag
- Build detailed error message with:
  * Operation (READ)
  * File name (ACCTDAT, CUSTDAT, CXACAIX)
  * RESP code
  * RESP2 code
- Display technical error information
- Allow user to retry or exit

**Technical Reasons:**
- File not available (closed, disabled)
- I/O error (hardware, VSAM corruption)
- Security violation (unauthorized access)
- Resource shortage (VSAM buffers)

**User Action Required:** Contact IT support; may need system administrator intervention

---

### System Errors

**7. Unexpected Data Scenario**

**Condition:**
```cobol
WHEN OTHER
   MOVE 'UNEXPECTED DATA SCENARIO' TO WS-RETURN-MSG
```

**Error Message:** "UNEXPECTED DATA SCENARIO"

**Recovery:**
- Capture abend culprit (COACTVWC)
- Capture abend code ('0001')
- Display plain text error
- Terminate session
- Log to CICS error handling

**Technical Reason:** Program state corruption or logic error

**User Action Required:** Contact IT support with transaction details

---

**8. Abnormal Termination (ABEND)**

**Condition:**
- Any unhandled CICS exception
- Program check
- Storage violation
- Timeout

**Handler:**
```cobol
ABEND-ROUTINE.
   IF ABEND-MSG EQUAL LOW-VALUES
      MOVE 'UNEXPECTED ABEND OCCURRED.' TO ABEND-MSG
   END-IF
   
   MOVE LIT-THISPGM TO ABEND-CULPRIT
   
   EXEC CICS SEND FROM(ABEND-DATA) LENGTH(LENGTH OF ABEND-DATA)
                  NOHANDLE
   END-EXEC
   
   EXEC CICS HANDLE ABEND CANCEL END-EXEC
   
   EXEC CICS ABEND ABCODE('9999') END-EXEC
```

**Recovery:**
- Display abend information to user
- Identify program (COACTVWC)
- Cancel recursive abend handling
- Issue controlled abend with code '9999'
- Transaction logs available for debugging

**User Action Required:** Contact IT support; transaction will be logged

---

### Error Response Codes

**CICS Response Codes Handled:**

| Response | Meaning | Handled By |
|----------|---------|------------|
| NORMAL | Successful operation | All READ operations |
| NOTFND | Record not found | All READ operations |
| MAPFAIL | Map receive failed | RECEIVE MAP (not explicitly handled) |
| OTHER | Any other error | All operations |

**Error Severity Levels:**

| Level | Description | Examples | Recovery |
|-------|-------------|----------|----------|
| INFO | Informational | Prompt for input | Display message |
| WARNING | Validation failed | Invalid account format | Allow correction |
| ERROR | Business error | Account not found | Display error, allow retry |
| SEVERE | System error | File I/O failure | Display error, contact support |
| FATAL | Program failure | Abend | Log and terminate |

---

### Error Display Patterns

**Screen-Based Errors:**
- Display in ERRMSG field (line 23, 78 characters)
- Use RED color for visibility
- Position cursor at error field
- Allow correction without losing context

**Text-Based Errors:**
- Use SEND TEXT for serious errors
- Display technical details for IT support
- Clear screen before message
- Require support intervention

**Abend Errors:**
- Capture program context
- Send abend data to user
- Log to CICS error facilities
- Terminate with abend code '9999'

---

### Error Prevention Strategies

**Input Validation:**
- Field-level validation (account number format)
- MUSTFILL attribute for required fields
- Numeric validation before file access
- Clear error messages for correction

**File Access:**
- Sequential read pattern (xref → account → customer)
- Check each RESP code immediately
- Set flags for successful reads
- Display partial data if available

**State Management:**
- Validate COMMAREA on entry
- Initialize variables properly
- Set appropriate flags
- Preserve context across calls

**Resource Management:**
- Use pseudo-conversational design
- Release resources with RETURN
- No long-running transactions
- Efficient file access patterns

---

### Migration Considerations for Error Handling

**Modern Error Handling:**
- Replace RESP codes with exception handling
- Use try-catch blocks for file operations
- Implement global error handlers
- Return appropriate HTTP status codes

**Logging:**
- Replace CICS logging with application logging
- Use structured logging (JSON, XML)
- Include correlation IDs for tracing
- Implement log levels (DEBUG, INFO, ERROR)

**User Experience:**
- Friendly error messages for end users
- Technical details in logs only
- Inline validation with AJAX
- Toast notifications for errors

**Monitoring:**
- Implement application performance monitoring
- Track error rates and patterns
- Alert on threshold violations
- Create error dashboards

**Recovery:**
- Implement retry logic for transient errors
- Circuit breaker pattern for service calls
- Graceful degradation for failures
- Data integrity verification

---

## 10. Additional Technical Details

### Pseudo-Conversational Design

**Pattern Implementation:**

The program follows CICS pseudo-conversational design pattern for optimal resource usage:

**Initial Invocation (EIBCALEN = 0):**
```cobol
IF EIBCALEN IS EQUAL TO 0
   INITIALIZE CARDDEMO-COMMAREA
              WS-THIS-PROGCOMMAREA
```
- First time transaction is invoked
- No COMMAREA passed
- Program initializes all data structures
- Sets state to CDEMO-PGM-ENTER

**Subsequent Invocations (EIBCALEN > 0):**
```cobol
ELSE
   MOVE DFHCOMMAREA (1:LENGTH OF CARDDEMO-COMMAREA) TO
                     CARDDEMO-COMMAREA
   MOVE DFHCOMMAREA(LENGTH OF CARDDEMO-COMMAREA + 1:
                    LENGTH OF WS-THIS-PROGCOMMAREA) TO
                     WS-THIS-PROGCOMMAREA
END-IF
```
- Returns from user input
- Restores state from COMMAREA
- Sets state to CDEMO-PGM-REENTER

**Return to CICS:**
```cobol
EXEC CICS RETURN
     TRANSID (LIT-THISTRANID)
     COMMAREA (WS-COMMAREA)
     LENGTH(LENGTH OF WS-COMMAREA)
END-EXEC
```
- Saves state in COMMAREA (2000 bytes)
- Specifies next transaction (CAVW)
- Releases all resources
- Task terminates, waits for user input

**Benefits:**
- Minimal resource consumption
- Scales to thousands of concurrent users
- Resources only held during active processing
- No timeout issues for slow users

**Migration Notes:**
- Replace with stateless HTTP request/response
- Use session management for state preservation
- Consider JWT tokens or session cookies
- Implement proper session timeout handling

---

### COMMAREA Management

**Structure:**
- Application-level COMMAREA (CARDDEMO-COMMAREA)
- Program-specific COMMAREA (WS-THIS-PROGCOMMAREA)
- Combined size: 2000 bytes

**Information Preserved:**
- Navigation context (calling program, transaction)
- User identity and type
- Current data keys (account, customer, card)
- Screen flow state (mapset, map)
- AID key information
- Error messages

**COMMAREA Assembly:**
```cobol
MOVE  CARDDEMO-COMMAREA    TO WS-COMMAREA
MOVE  WS-THIS-PROGCOMMAREA TO
      WS-COMMAREA(LENGTH OF CARDDEMO-COMMAREA + 1:
                   LENGTH OF WS-THIS-PROGCOMMAREA)
```

**Migration Notes:**
- Replace with session objects
- Use distributed cache (Redis) for scalability
- Consider microservices context propagation
- Implement correlation IDs for tracing

---

### Date and Time Handling

**Current Date/Time Retrieval:**
```cobol
MOVE FUNCTION CURRENT-DATE TO WS-CURDATE-DATA
```

**Date Formatting (MM/DD/YY):**
```cobol
MOVE WS-CURDATE-MONTH       TO WS-CURDATE-MM
MOVE WS-CURDATE-DAY         TO WS-CURDATE-DD
MOVE WS-CURDATE-YEAR(3:2)   TO WS-CURDATE-YY
MOVE WS-CURDATE-MM-DD-YY    TO CURDATEO OF CACTVWAO
```

**Time Formatting (HH:MM:SS):**
```cobol
MOVE WS-CURTIME-HOURS       TO WS-CURTIME-HH
MOVE WS-CURTIME-MINUTE      TO WS-CURTIME-MM
MOVE WS-CURTIME-SECOND      TO WS-CURTIME-SS
MOVE WS-CURTIME-HH-MM-SS    TO CURTIMEO OF CACTVWAO
```

**Date Display from Files:**
- Account dates (open, expiry, reissue) displayed as-is
- Customer DOB displayed in YYYY-MM-DD format
- No date arithmetic in this program (view only)

**Migration Notes:**
- Use modern date/time libraries (Java Time API, Python datetime)
- Handle time zones explicitly
- Use ISO 8601 format for storage
- Localize date/time display for users

---

### BMS Attribute Management

**Field Attribute Manipulation:**

**Cursor Positioning:**
```cobol
MOVE -1 TO ACCTSIDL OF CACTVWAI
```
- Sets cursor to ACCTSID field
- Value of -1 indicates cursor position

**Color Attributes:**
```cobol
MOVE DFHDFCOL TO ACCTSIDC OF CACTVWAO  - Default color
MOVE DFHRED   TO ACCTSIDC OF CACTVWAO  - Red for errors
```

**Field Attributes:**
```cobol
MOVE DFHBMFSE  TO ACCTSIDA OF CACTVWAI  - Field separator
MOVE DFHBMDAR  TO INFOMSGC OF CACTVWAO  - Dark (hidden)
MOVE DFHNEUTR  TO INFOMSGC OF CACTVWAO  - Neutral color
```

**Attribute Constants (from DFHBMSCA):**
- ASKIP: Auto-skip (protected, no cursor stop)
- UNPROT: Unprotected (input allowed)
- PROT: Protected (display only)
- FSET: Field set (modified data tag)
- BRT: Bright intensity
- NORM: Normal intensity
- IC: Initial cursor position

**Migration Notes:**
- Replace with CSS classes for styling
- Use HTML5 input attributes for validation
- Implement focus management with JavaScript
- Consider accessibility (ARIA attributes)

---

### Screen Management Patterns

**Three-Phase Screen Sending:**

**Phase 1: Initialize (1100-SCREEN-INIT)**
- Clear output map
- Set header information
- Format date/time
- Set constant fields

**Phase 2: Populate Data (1200-SETUP-SCREEN-VARS)**
- Move file data to screen fields
- Format monetary amounts (PICOUT)
- Format SSN with dashes
- Set messages

**Phase 3: Set Attributes (1300-SETUP-SCREEN-ATTRS)**
- Position cursor based on context
- Set field colors (error highlighting)
- Set field protection
- Configure message display

**Phase 4: Send (1400-SEND-SCREEN)**
- Execute SEND MAP command
- CURSOR: Use attribute positioning
- ERASE: Clear screen first
- FREEKB: Free keyboard for input

**Migration Notes:**
- Use MVC pattern (Model-View-Controller)
- Implement view models for data binding
- Use template engines (Thymeleaf, Jinja2)
- Separate presentation logic from business logic

---

### Read-Only Nature

**Key Characteristic:** This program performs only READ operations, no updates.

**Implications:**

**No Transaction Atomicity Concerns:**
- No SYNCPOINT or SYNCPOINT ROLLBACK
- No concurrent update detection needed
- No data locking required
- Simpler error handling

**File Access Pattern:**
- Three sequential READ operations
- No interdependencies between reads
- Partial results acceptable
- Display what's available

**Comparison with COACTUPC (Update Program):**

| Feature | COACTVWC (View) | COACTUPC (Update) |
|---------|-----------------|-------------------|
| File Operations | READ only | READ UPDATE, REWRITE |
| Transaction Control | None | SYNCPOINT, ROLLBACK |
| Concurrent Updates | N/A | Detection and handling |
| Data Validation | Input only | Input and change detection |
| Complexity | Simple | Complex |

**Migration Notes:**
- Implement as GET endpoints (REST)
- Use read replicas for scalability
- Cache frequently accessed data
- No transaction management needed
- Simple error handling sufficient

---

### Performance Characteristics

**Resource Usage:**
- Minimal CPU per transaction
- Three file I/O operations (reads only)
- Small COMMAREA (2000 bytes)
- Standard BMS map operations

**Scalability:**
- Pseudo-conversational design enables high concurrency
- No long-running transactions
- Resources released between user interactions
- Efficient for interactive queries

**File Access Optimization:**
- Sequential read pattern (xref → account → customer)
- Direct key access (no browsing)
- Minimal data transfer
- No update conflicts

**Migration Considerations:**
- Implement caching strategy
- Use connection pooling
- Consider read replicas
- Implement query optimization
- Add pagination for large result sets

---

### Security Considerations

**Data Protection:**
- SSN displayed (requires PCI/PII compliance)
- Account numbers visible
- Customer financial data exposed
- No built-in encryption

**Access Control:**
- Transaction-level security (RACF/TopSecret)
- User type in COMMAREA (USER vs ADMIN)
- No field-level security
- Terminal-based access control

**Audit Trail:**
- CICS logging for transaction invocation
- No application-level audit log
- File access logged by VSAM
- Abends logged to CICS error facilities

**Migration Requirements:**
- Implement authentication/authorization
- Use HTTPS for data transmission
- Encrypt sensitive data at rest
- Implement field-level security
- Add comprehensive audit logging
- Mask sensitive data (SSN, account numbers)
- Implement role-based access control (RBAC)
- Add session management with timeout

---

### Cloud Migration Priorities

**High Priority:**
1. Convert VSAM files to relational database
2. Implement RESTful API for data access
3. Replace BMS screens with web UI
4. Add authentication/authorization
5. Implement session management

**Medium Priority:**
1. Add caching layer (Redis)
2. Implement logging framework
3. Add monitoring/alerting
4. Implement CI/CD pipeline
5. Add automated testing

**Low Priority:**
1. Implement pagination
2. Add search functionality
3. Implement data export
4. Add reporting features
5. Optimize database queries

**Technical Stack Recommendations:**
- Backend: Java Spring Boot, Python FastAPI, or Node.js Express
- Frontend: React, Angular, or Vue.js
- Database: PostgreSQL, MySQL, or Oracle
- Cache: Redis or Memcached
- Authentication: OAuth2, JWT
- API: RESTful with OpenAPI/Swagger
- Monitoring: Prometheus, Grafana
- Logging: ELK Stack (Elasticsearch, Logstash, Kibana)

---

### Program Strengths

1. **Simple and Focused:** Single responsibility (view account data)
2. **Efficient Design:** Pseudo-conversational pattern for scalability
3. **Good Error Handling:** Comprehensive error checking and user feedback
4. **Clear Structure:** Well-organized paragraphs and flow
5. **READ-Only:** No data integrity concerns, simpler logic
6. **User-Friendly:** Clear prompts and error messages

### Program Limitations

1. **Single Account View:** Cannot view multiple accounts
2. **No Search:** Must know exact account number
3. **No Export:** Cannot save or print data
4. **Limited Navigation:** Must exit to access other functions
5. **No Audit:** No logging of who viewed what
6. **3270 Dependency:** Tied to mainframe terminal

### Recommended Enhancements for Migration

1. **Add Search Capability:** Search by customer name, SSN, partial account
2. **Implement Pagination:** Handle large result sets
3. **Add Export:** PDF, Excel, CSV formats
4. **Enhance Security:** Mask sensitive data, audit viewing
5. **Improve Navigation:** Direct links to related functions
6. **Add Filtering:** Filter by account status, date ranges
7. **Implement Caching:** Cache frequently accessed accounts
8. **Add Mobile Support:** Responsive design for multiple devices

---

## Summary

The COACTVWC program is a well-structured, efficient COBOL CICS application for viewing credit card account information. It demonstrates proper use of pseudo-conversational design, comprehensive error handling, and clear separation of concerns through well-organized paragraphs. As a READ-ONLY program, it has simpler logic than update programs but provides essential functionality for account inquiry.

**Key Takeaways:**
- 942 lines of COBOL implementing read-only account view
- Uses pseudo-conversational pattern for scalability
- Accesses three VSAM files (card xref, account, customer)
- Single input field with comprehensive validation
- Displays 30+ fields of account and customer data
- Well-structured error handling with user-friendly messages
- Good candidate for initial cloud migration (simple, no updates)
- Demonstrates mainframe best practices for online transaction processing

This comprehensive analysis provides all the information needed to understand the program's functionality and successfully migrate it to a modern cloud-based architecture while preserving its business logic and user experience.
