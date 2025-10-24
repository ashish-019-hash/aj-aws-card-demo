# COBOL CICS Program Analysis: COACTVWC

**Program ID:** COACTVWC  
**Transaction ID:** CAVW  
**Mapset:** COACTVW  
**Map:** CACTVWA  
**Purpose:** Accept and process Account View request - Display account and customer details (READ-ONLY)

---

## 1. Screen Visualization

### BMS Map Metadata

**DFHMSD Parameters:**
- MAPSET: COACTVW
- MODE: INOUT
- LANG: COBOL
- STORAGE: AUTO
- TIOAPFX: YES

**DFHMDI Parameters:**
- MAP: CACTVWA
- SIZE: (24,80)
- LINE: 1
- COLUMN: 1

### 24x80 ASCII Screen Layout

```
Tran:CAVW        CardDemo Account View                        Date:mm/dd/yy 
Prog:COACTVWC    Secure Credit Card System                    Time:hh:mm:ss 
                                                                              
                             View Account                                     
                  Account Number : ___________   Active Y/N: _               
       Opened :__________                        Credit Limit        :       
                                                           +ZZZ,ZZZ,ZZZ.99    
       Expiry :__________                        Cash credit Limit   :       
                                                           +ZZZ,ZZZ,ZZZ.99    
       Reissue:__________                        Current Balance     :       
                                                           +ZZZ,ZZZ,ZZZ.99    
       Account Group:__________                  Current Cycle Credit:       
                                                           +ZZZ,ZZZ,ZZZ.99    
                                                 Current Cycle Debit :       
                                                           +ZZZ,ZZZ,ZZZ.99    
                            Customer Details                                  
       Customer id  :_________            SSN:____________                   
       Date of birth:__________           FICO Score:___                     
       First Name             Middle Name:            Last Name :            
       _________________________  _________________________  ________________
       Address:__________________________________________________  State __   
                 __________________________________________________  Zip _____
       City __________________________________________________  Country ___   
       Phone 1:_____________       Government Issued Id Ref    : ____________
       Phone 2:_____________       EFT Account Id: __________                
                                   Primary Card Holder Y/N:_                 
                                                                              
                         Enter or update id of account to display            
 ERROR MESSAGE AREA                                                          
  F3=Exit                                                                    
```

### Screen Layout Details

The screen is a standard 24-row by 80-column 3270 terminal display. Key characteristics:

- **Header Section (Lines 1-2)**: Transaction ID (CAVW), screen title, program name (COACTVWC), date, and time
- **Title Section (Line 4)**: "View Account" centered
- **Input Section (Line 5)**: Single input field for Account Number (11 digits, MUSTFILL validation) - shown as 11 underscores
- **Account Information (Lines 5-15)**: Display-only fields for account status, dates, limits, and balances
- **Customer Information (Lines 16-26)**: Display-only fields for customer demographics and contact information
- **Message Area (Line 22)**: Informational messages (45 characters)
- **Error Area (Line 23)**: Error messages in red text (78 characters)
- **Navigation (Line 24)**: Function key indicators

### DFHMDF Field Details from BMS

**Input Fields:**
- ACCTSID: POS=(5,38), LENGTH=11, ATTRB=(FSET,IC,NORM,UNPROT), PICIN='99999999999', VALIDN=(MUSTFILL)

**Display Fields (Account Information):**
- ACSTTUS: POS=(5,70), LENGTH=1, ATTRB=(ASKIP,FSET,NORM), INITIAL=' '
- ADTOPEN: POS=(6,17), LENGTH=10, ATTRB=(ASKIP,FSET,NORM)
- ACRDLIM: POS=(6,61), LENGTH=15, ATTRB=(ASKIP,FSET,NORM), PICOUT='+ZZZ,ZZZ,ZZZ.99'
- AEXPDT: POS=(7,17), LENGTH=10, ATTRB=(ASKIP,FSET,NORM)
- ACSHLIM: POS=(8,61), LENGTH=15, ATTRB=(ASKIP,FSET,NORM), PICOUT='+ZZZ,ZZZ,ZZZ.99'
- AREISDT: POS=(9,17), LENGTH=10, ATTRB=(ASKIP,FSET,NORM)
- ACURBAL: POS=(10,61), LENGTH=15, ATTRB=(ASKIP,FSET,NORM), PICOUT='+ZZZ,ZZZ,ZZZ.99'
- AADDGRP: POS=(12,23), LENGTH=10, ATTRB=(ASKIP,FSET,NORM)
- ACRCYCR: POS=(13,61), LENGTH=15, ATTRB=(ASKIP,FSET,NORM), PICOUT='+ZZZ,ZZZ,ZZZ.99'
- ACRCYDB: POS=(15,61), LENGTH=15, ATTRB=(ASKIP,FSET,NORM), PICOUT='+ZZZ,ZZZ,ZZZ.99'

**Display Fields (Customer Information):**
- ACSTNUM: POS=(17,23), LENGTH=9, ATTRB=(ASKIP,FSET,NORM), COLOR=TURQUOISE
- ACSTSSN: POS=(17,54), LENGTH=12, ATTRB=(ASKIP,FSET,NORM), COLOR=TURQUOISE
- ACSTDOB: POS=(18,23), LENGTH=10, ATTRB=(ASKIP,FSET,NORM), COLOR=TURQUOISE
- ACSTFCO: POS=(18,61), LENGTH=3, ATTRB=(ASKIP,FSET,NORM), COLOR=TURQUOISE
- ACSFNAM: POS=(20,8), LENGTH=25, ATTRB=(ASKIP,FSET,NORM), COLOR=TURQUOISE
- ACSMNAM: POS=(20,35), LENGTH=25, ATTRB=(ASKIP,FSET,NORM), COLOR=TURQUOISE
- ACSLNAM: POS=(20,62), LENGTH=18, ATTRB=(ASKIP,FSET,NORM), COLOR=TURQUOISE
- ACSADL1: POS=(21,17), LENGTH=50, ATTRB=(ASKIP,FSET,NORM), COLOR=TURQUOISE
- ACSSTTE: POS=(21,73), LENGTH=2, ATTRB=(ASKIP,FSET,NORM), COLOR=TURQUOISE
- ACSADL2: POS=(22,17), LENGTH=50, ATTRB=(ASKIP,FSET,NORM), COLOR=TURQUOISE
- ACSZIPC: POS=(22,73), LENGTH=5, ATTRB=(ASKIP,FSET,NORM), COLOR=TURQUOISE
- ACSCITY: POS=(23,10), LENGTH=50, ATTRB=(ASKIP,FSET,NORM), COLOR=TURQUOISE
- ACSCTRY: POS=(23,73), LENGTH=3, ATTRB=(ASKIP,FSET,NORM), COLOR=TURQUOISE
- ACSPHN1: POS=(24,10), LENGTH=13, ATTRB=(ASKIP,FSET,NORM), COLOR=TURQUOISE
- ACSGOVT: POS=(24,58), LENGTH=20, ATTRB=(ASKIP,FSET,NORM), COLOR=TURQUOISE
- ACSPHN2: POS=(25,10), LENGTH=13, ATTRB=(ASKIP,FSET,NORM), COLOR=TURQUOISE
- ACSEFTC: POS=(25,41), LENGTH=10, ATTRB=(ASKIP,FSET,NORM), COLOR=TURQUOISE
- ACSPFLG: POS=(25,78), LENGTH=1, ATTRB=(ASKIP,FSET,NORM), COLOR=TURQUOISE

**Message Fields:**
- INFOMSG: POS=(27,23), LENGTH=45, ATTRB=(ASKIP,FSET,NORM)
- ERRMSG: POS=(28,1), LENGTH=78, ATTRB=(ASKIP,BRT,FSET), COLOR=RED

---

## 2. Field Details Table

### Screen Fields Mapping

| Line | Column | Field Name | Type | Length | Data Source | Attribute |
|------|--------|------------|------|--------|-------------|-----------|
| 1 | 6 | TRNNAME | Display | 4 | Transaction ID 'CAVW' | ASKIP, FSET, NORM, BLUE |
| 1 | 21 | TITLE01 | Display | 40 | 'CardDemo Account View' | ASKIP, NORM, YELLOW |
| 1 | 76 | CURDATE | Display | 8 | Current Date (EIBDATE) | ASKIP, NORM, BLUE |
| 2 | 7 | PGMNAME | Display | 8 | Program Name 'COACTVWC' | ASKIP, NORM, BLUE |
| 2 | 21 | TITLE02 | Display | 40 | 'Secure Credit Card System' | ASKIP, NORM, YELLOW |
| 2 | 76 | CURTIME | Display | 8 | Current Time (EIBTIME) | ASKIP, NORM, BLUE |
| 5 | 38 | ACCTSID | Input | 11 | User input Account Number | FSET, IC, NORM, UNPROT, GREEN, MUSTFILL |
| 5 | 70 | ACSTTUS | Display | 1 | ACCT-ACTIVE-STATUS (ACCTDAT) | ASKIP, FSET, NORM |
| 6 | 17 | ADTOPEN | Display | 10 | ACCT-OPEN-DATE (ACCTDAT) | ASKIP, FSET, NORM |
| 6 | 61 | ACRDLIM | Display | 15 | ACCT-CREDIT-LIMIT (ACCTDAT) | ASKIP, FSET, NORM, PICOUT |
| 7 | 17 | AEXPDT | Display | 10 | ACCT-EXPIRY-DATE (ACCTDAT) | ASKIP, FSET, NORM |
| 8 | 61 | ACSHLIM | Display | 15 | ACCT-CASH-CREDIT-LIMIT (ACCTDAT) | ASKIP, FSET, NORM, PICOUT |
| 9 | 17 | AREISDT | Display | 10 | ACCT-REISSUE-DATE (ACCTDAT) | ASKIP, FSET, NORM |
| 10 | 61 | ACURBAL | Display | 15 | ACCT-CURR-BAL (ACCTDAT) | ASKIP, FSET, NORM, PICOUT |
| 12 | 23 | AADDGRP | Display | 10 | ACCT-GROUP-ID (ACCTDAT) | ASKIP, FSET, NORM |
| 13 | 61 | ACRCYCR | Display | 15 | ACCT-CURR-CYC-CREDIT (ACCTDAT) | ASKIP, FSET, NORM, PICOUT |
| 15 | 61 | ACRCYDB | Display | 15 | ACCT-CURR-CYC-DEBIT (ACCTDAT) | ASKIP, FSET, NORM, PICOUT |
| 17 | 23 | ACSTNUM | Display | 9 | CUST-ID (CUSTDAT) | ASKIP, FSET, NORM, TURQUOISE |
| 17 | 54 | ACSTSSN | Display | 12 | CUST-SSN (CUSTDAT) formatted | ASKIP, FSET, NORM, TURQUOISE |
| 18 | 23 | ACSTDOB | Display | 10 | CUST-DOB-YYYYMMDD (CUSTDAT) | ASKIP, FSET, NORM, TURQUOISE |
| 18 | 61 | ACSTFCO | Display | 3 | CUST-FICO-CREDIT-SCORE (CUSTDAT) | ASKIP, FSET, NORM, TURQUOISE |
| 20 | 8 | ACSFNAM | Display | 25 | CUST-FIRST-NAME (CUSTDAT) | ASKIP, FSET, NORM, TURQUOISE |
| 20 | 35 | ACSMNAM | Display | 25 | CUST-MIDDLE-NAME (CUSTDAT) | ASKIP, FSET, NORM, TURQUOISE |
| 20 | 62 | ACSLNAM | Display | 18 | CUST-LAST-NAME (CUSTDAT) | ASKIP, FSET, NORM, TURQUOISE |
| 21 | 17 | ACSADL1 | Display | 50 | CUST-ADDR-LINE-1 (CUSTDAT) | ASKIP, FSET, NORM, TURQUOISE |
| 21 | 73 | ACSSTTE | Display | 2 | CUST-ADDR-STATE-CD (CUSTDAT) | ASKIP, FSET, NORM, TURQUOISE |
| 22 | 17 | ACSADL2 | Display | 50 | CUST-ADDR-LINE-2 (CUSTDAT) | ASKIP, FSET, NORM, TURQUOISE |
| 22 | 73 | ACSZIPC | Display | 5 | CUST-ADDR-ZIP (CUSTDAT) | ASKIP, FSET, NORM, TURQUOISE |
| 23 | 10 | ACSCITY | Display | 50 | CUST-ADDR-CITY-NAME (CUSTDAT) | ASKIP, FSET, NORM, TURQUOISE |
| 23 | 73 | ACSCTRY | Display | 3 | CUST-ADDR-COUNTRY-CD (CUSTDAT) | ASKIP, FSET, NORM, TURQUOISE |
| 24 | 10 | ACSPHN1 | Display | 13 | CUST-PHONE-NUM-1 (CUSTDAT) | ASKIP, FSET, NORM, TURQUOISE |
| 24 | 58 | ACSGOVT | Display | 20 | CUST-GOVT-ISSUED-ID (CUSTDAT) | ASKIP, FSET, NORM, TURQUOISE |
| 25 | 10 | ACSPHN2 | Display | 13 | CUST-PHONE-NUM-2 (CUSTDAT) | ASKIP, FSET, NORM, TURQUOISE |
| 25 | 41 | ACSEFTC | Display | 10 | CUST-EFT-ACCOUNT-ID (CUSTDAT) | ASKIP, FSET, NORM, TURQUOISE |
| 25 | 78 | ACSPFLG | Display | 1 | CUST-PRI-CARD-HOLDER-IND (CUSTDAT) | ASKIP, FSET, NORM, TURQUOISE |
| 27 | 23 | INFOMSG | Display | 45 | Information messages | ASKIP, FSET, NORM, NEUTRAL |
| 28 | 1 | ERRMSG | Display | 78 | Error messages | ASKIP, BRT, FSET, RED |

### Field Validation Rules

**ACCTSID (Account Number Input)**
- Required field (MUSTFILL attribute from BMS)
- Must be numeric (validated in program)
- Must be 11 digits (PIC 9(11))
- Must be non-zero
- Validation performed in paragraph 2210-EDIT-ACCOUNT (Line 705-728)

**Validation Logic:**
```cobol
IF ACCTIDINI = SPACES OR LOW-VALUES
   MOVE 'Y' TO WS-ERR-FLG
   MOVE 'Please enter an Account Number...' TO ERRMSGO
END-IF

IF WS-ERR-FLG = 'N'
   IF ACCTSIDI IS NOT NUMERIC
      MOVE 'Y' TO WS-ERR-FLG
      MOVE 'Invalid Account Number...' TO ERRMSGO
   END-IF
END-IF

IF WS-ERR-FLG = 'N'
   IF ACCTSIDI = ZEROS
      MOVE 'Y' TO WS-ERR-FLG
      MOVE 'Account Number cannot be zero...' TO ERRMSGO
   END-IF
END-IF
```

**Display Fields**
- All display fields are protected (ASKIP attribute)
- Populated from ACCTDAT (Account Master) and CUSTDAT (Customer Master) files
- Formatted using PICOUT attributes where applicable (monetary amounts: +ZZZ,ZZZ,ZZZ.99)
- SSN displayed with formatting (XXX-XX-XXXX) via program logic
- All customer fields shown in TURQUOISE color for visual grouping
- Date fields formatted as MM/DD/YYYY

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
- Inter-program communication area definition
- Contains: Transaction ID, Program name, Return flags, Account/Customer IDs

**3. DFHBMSCA (Line 215)**
- Standard CICS BMS attribute definitions
- Screen field attributes (ASKIP, NORM, BRT, etc.)

**4. DFHAID (Line 219)**
- Standard CICS attention identifier definitions
- Function key values (ENTER, PF3, CLEAR, etc.)

**5. COTTL01Y (Line 223)**
- Standard title lines for all screens
- Common header/footer text

**6. COACTVW (Line 227)**
- BMS-generated symbolic map for CACTVWA screen
- Screen field definitions and attributes

**7. CSDAT01Y (Line 231)**
- Date formatting utilities
- Date conversion routines

**8. CSMSG01Y (Line 235)**
- Common message definitions
- Standard system messages

**9. CSMSG02Y (Line 239)**
- Additional message definitions
- Extended message text

**10. CSUSR01Y (Line 243)**
- User security record structure
- User authentication data

**11. CVACT01Y (Line 247)**
- Account master record layout (ACCTDAT file)
- Contains: Account ID, Customer ID, Status, Dates, Limits, Balances

**12. CVACT02Y (Line 251)**
- Account cross-reference structures
- Account lookup utilities

**13. CVACT03Y (Line 255)**
- Additional account data structures
- Account processing utilities

**14. CVCUS01Y (Line 259)**
- Customer master record layout (CUSTDAT file)
- Contains: Customer ID, Name, Address, Phone, SSN, DOB, FICO

**15. CSSTRPFY (Line 263)**
- String manipulation utilities
- Formatting and parsing routines

### Program Flow Structure

```
MAIN-PARA (Line 267-317)
├── 0000-MAIN (Line 319-362)
│   ├── HANDLE ABEND setup
│   ├── 1000-SEND-MAP (First time display)
│   └── 9000-READ-DATA (Process input)
│       ├── 2000-RECEIVE-MAP
│       ├── 2100-EDIT-MAP-INPUTS
│       │   ├── 2210-EDIT-ACCOUNT
│       │   └── 2500-VALIDATE-KEYS
│       ├── 3000-PROCESS-INPUTS
│       │   ├── 3100-GET-ACCT-DATA
│       │   └── 3200-GET-CUST-DATA
│       └── 1000-SEND-MAP (Display results)
└── RETURN to CICS
```

### Working Storage Sections

**General CICS Variables (Line 35-45)**
- Response codes (WS-RESP-CD, WS-REAS-CD)
- Transaction ID (WS-TRANID)

**Input/Output Flags (Line 50-65)**
- Input validation flags (INPUT-OK, INPUT-ERROR, INPUT-PENDING)
- PFK validation flags (PFK-VALID, PFK-INVALID)
- Account filter flags (FLG-ACCTFILTER-ISVALID, FLG-ACCTFILTER-NOT-OK)
- Customer filter flags (FLG-CUSTFILTER-ISVALID, FLG-CUSTFILTER-NOT-OK)

**File Handling Variables (Line 73-132)**
- Card cross-reference key (WS-XREF-RID)
- Account ID structures (WS-CARD-RID-ACCT-ID)
- Customer ID structures (WS-CARD-RID-CUST-ID)
- File read status flags (WS-ACCOUNT-MASTER-READ-FLAG, WS-CUST-MASTER-READ-FLAG)
- Error message formatting (WS-FILE-ERROR-MESSAGE)

**General Error Handling (Line 134-172)**
- Error flag (WS-ERR-FLG)
- Message text buffer (WS-MESSAGE)

---

## 4. CICS Commands Used

### Command 1: HANDLE ABEND (Line 272-274)

```cobol
EXEC CICS HANDLE ABEND
    LABEL(ABEND-HANDLING)
END-EXEC.
```

**Purpose:** Establish abnormal termination handling routine  
**Parameters:**
- LABEL: ABEND-HANDLING (paragraph at Line 929-940)

**Business Context:** Sets up error recovery for unexpected program failures

### Command 2: XCTL to Menu Program (Line 285-289)

```cobol
EXEC CICS XCTL
    PROGRAM('COMEN01C')
    COMMAREA(CARDDEMO-COMMAREA)
    LENGTH(LENGTH OF CARDDEMO-COMMAREA)
END-EXEC.
```

**Purpose:** Transfer control to main menu when user presses F3  
**Parameters:**
- PROGRAM: 'COMEN01C' (Main menu program)
- COMMAREA: CARDDEMO-COMMAREA (Communication area)
- LENGTH: Length of COMMAREA

**Business Context:** Provides user exit path back to main menu

### Command 3: RETURN with Transid (Line 291-293)

```cobol
EXEC CICS RETURN
    TRANSID('CAVW')
END-EXEC.
```

**Purpose:** Return to CICS with pseudo-conversational restart  
**Parameters:**
- TRANSID: 'CAVW' (This transaction ID for next interaction)

**Business Context:** Implements pseudo-conversational design to free up resources between user interactions

### Command 4: SEND MAP - Initial Display (Line 323-334)

```cobol
EXEC CICS SEND MAP('CACTVWA')
    MAPSET('COACTVW')
    FROM(CACTVWAO)
    ERASE
    CURSOR
    RESP(WS-RESP-CD)
    RESP2(WS-REAS-CD)
END-EXEC.
```

**Purpose:** Display initial empty screen to user  
**Parameters:**
- MAP: 'CACTVWA' (Map name from BMS)
- MAPSET: 'COACTVW' (Mapset name)
- FROM: CACTVWAO (Output symbolic map)
- ERASE: Clear screen before display
- CURSOR: Position cursor at first input field
- RESP/RESP2: Response code handling

**Business Context:** First screen presentation when transaction starts

### Command 5: RECEIVE MAP (Line 366-374)

```cobol
EXEC CICS RECEIVE MAP('CACTVWA')
    MAPSET('COACTVW')
    INTO(CACTVWAI)
    RESP(WS-RESP-CD)
    RESP2(WS-REAS-CD)
END-EXEC.
```

**Purpose:** Receive user input from terminal  
**Parameters:**
- MAP: 'CACTVWA'
- MAPSET: 'COACTVW'
- INTO: CACTVWAI (Input symbolic map)
- RESP/RESP2: Response code handling

**Business Context:** Captures account number entered by user

**Error Handling:**
```cobol
IF WS-RESP-CD NOT = DFHRESP(NORMAL)
   MOVE 'Error receiving map data...' TO WS-MESSAGE
   PERFORM 9999-ABEND-ROUTINE
END-IF
```

### Command 6: READ Card Cross-Reference (Line 444-452)

```cobol
EXEC CICS READ FILE('CARDDAT')
    INTO(CARD-XREF-RECORD)
    RIDFLD(ACCTSIDI)
    KEYLENGTH(11)
    RESP(WS-RESP-CD)
    RESP2(WS-REAS-CD)
END-EXEC.
```

**Purpose:** Look up account ID using account number  
**Parameters:**
- FILE: 'CARDDAT' (Card cross-reference VSAM file)
- INTO: CARD-XREF-RECORD (Target data structure)
- RIDFLD: ACCTSIDI (Account number from user input)
- KEYLENGTH: 11 bytes
- RESP/RESP2: Response code handling

**Business Context:** First step in data retrieval - validates account exists and gets account ID for subsequent reads

**Error Handling:**
```cobol
EVALUATE WS-RESP-CD
   WHEN DFHRESP(NORMAL)
      CONTINUE
   WHEN DFHRESP(NOTFND)
      MOVE 'N' TO CDEMO-ACCT-FOUND-FLAG
      MOVE 'Account not found...' TO ERRMSGO
   WHEN OTHER
      PERFORM 9910-DISPLAY-IO-STATUS
END-EVALUATE
```

### Command 7: READ Account Master (Line 509-517)

```cobol
EXEC CICS READ FILE('ACCTDAT')
    INTO(ACCOUNT-RECORD)
    RIDFLD(FD-ACCT-ID)
    RESP(WS-RESP-CD)
    RESP2(WS-REAS-CD)
END-EXEC.
```

**Purpose:** Retrieve complete account information  
**Parameters:**
- FILE: 'ACCTDAT' (Account master VSAM file)
- INTO: ACCOUNT-RECORD (Account master record structure)
- RIDFLD: FD-ACCT-ID (Account ID from cross-reference)
- RESP/RESP2: Response code handling

**Business Context:** Gets account status, dates, limits, and balances for display

**Error Handling:**
```cobol
EVALUATE WS-RESP-CD
   WHEN DFHRESP(NORMAL)
      MOVE 'Y' TO WS-ACCOUNT-MASTER-READ-FLAG
   WHEN DFHRESP(NOTFND)
      MOVE 'Account details not found...' TO ERRMSGO
   WHEN OTHER
      PERFORM 9910-DISPLAY-IO-STATUS
END-EVALUATE
```

### Command 8: READ Customer Master (Line 579-587)

```cobol
EXEC CICS READ FILE('CUSTDAT')
    INTO(CUSTOMER-RECORD)
    RIDFLD(FD-CUST-ID)
    RESP(WS-RESP-CD)
    RESP2(WS-REAS-CD)
END-EXEC.
```

**Purpose:** Retrieve customer demographic and contact information  
**Parameters:**
- FILE: 'CUSTDAT' (Customer master VSAM file)
- INTO: CUSTOMER-RECORD (Customer master record structure)
- RIDFLD: FD-CUST-ID (Customer ID from account record)
- RESP/RESP2: Response code handling

**Business Context:** Gets customer name, address, phone, SSN, DOB, FICO score for display

**Error Handling:**
```cobol
EVALUATE WS-RESP-CD
   WHEN DFHRESP(NORMAL)
      MOVE 'Y' TO WS-CUST-MASTER-READ-FLAG
   WHEN DFHRESP(NOTFND)
      MOVE 'Customer details not found...' TO ERRMSGO
   WHEN OTHER
      PERFORM 9910-DISPLAY-IO-STATUS
END-EVALUATE
```

### Command 9: SEND MAP - Display Results (Line 621-634)

```cobol
EXEC CICS SEND MAP('CACTVWA')
    MAPSET('COACTVW')
    FROM(CACTVWAO)
    DATAONLY
    CURSOR
    RESP(WS-RESP-CD)
    RESP2(WS-REAS-CD)
END-EXEC.
```

**Purpose:** Display populated screen with account and customer data  
**Parameters:**
- MAP: 'CACTVWA'
- MAPSET: 'COACTVW'
- FROM: CACTVWAO (Output symbolic map with data)
- DATAONLY: Send data only, preserve screen structure
- CURSOR: Position cursor
- RESP/RESP2: Response code handling

**Business Context:** Shows query results to user after successful data retrieval

### Command 10: SEND TEXT - Error Display (Line 909-916)

```cobol
EXEC CICS SEND TEXT
    FROM(WS-MESSAGE)
    LENGTH(LENGTH OF WS-MESSAGE)
    ERASE
    FREEKB
    RESP(WS-RESP-CD)
    RESP2(WS-REAS-CD)
END-EXEC.
```

**Purpose:** Display text-only error message for severe errors  
**Parameters:**
- FROM: WS-MESSAGE (Error text)
- LENGTH: Length of message
- ERASE: Clear screen
- FREEKB: Free keyboard for input
- RESP/RESP2: Response code handling

**Business Context:** Used for catastrophic errors that prevent normal screen display

### Command 11: ABEND (Line 934-936)

```cobol
EXEC CICS ABEND
    ABCODE('HAPI')
END-EXEC.
```

**Purpose:** Abnormal program termination for unrecoverable errors  
**Parameters:**
- ABCODE: 'HAPI' (Application-specific abend code)

**Business Context:** Last resort error handling when normal recovery is not possible

---

## 5. Navigational Details

### Function Keys

**ENTER Key**
- **Purpose:** Submit account number for viewing
- **Action:** Validates input, retrieves account and customer data, displays results
- **Validation:** Account number must be 11 numeric digits, non-zero, and exist in database

**F3 (EXIT) Key**
- **Purpose:** Return to main menu
- **Action:** XCTL to COMEN01C (Main Menu program)
- **COMMAREA:** Passes current transaction context
- **Line Reference:** 283-289

**CLEAR Key**
- **Purpose:** Clear screen and return to initial state
- **Action:** Treated as invalid key, prompts user to use F3 to exit
- **Line Reference:** 391-394

**Other PF Keys**
- **Action:** Treated as invalid, error message displayed
- **Message:** "Invalid key pressed. Please see below..."
- **Line Reference:** 395-399

### Screen Flow Diagram

```
┌─────────────┐
│   COSGN00C  │ (Login Screen)
│   (CC00)    │
└──────┬──────┘
       │
       ↓
┌─────────────┐
│   COMEN01C  │ (Main Menu)
│   (CM00)    │
│  Option 1   │
└──────┬──────┘
       │
       ↓
┌─────────────┐         ┌─────────────┐
│  COACTVWC   │←────────│  COACTUPC   │ (Account Update)
│   (CAVW)    │  F3     │   (CAUP)    │
│   (THIS)    │         └─────────────┘
└──────┬──────┘
       │
       │ F3=Exit
       ↓
┌─────────────┐
│   COMEN01C  │ (Return to Menu)
│   (CM00)    │
└─────────────┘
```

### Navigation Logic

**From Menu to Account View:**
```cobol
User selects Option 1 from COMEN01C
→ COMEN01C XCTLs to COACTVWC with COMMAREA
→ COACTVWC displays initial screen
```

**Within Account View:**
```cobol
1. User enters account number
2. Presses ENTER
3. Program validates input (2100-EDIT-MAP-INPUTS)
4. If valid, retrieves data (3000-PROCESS-INPUTS)
5. Displays results (1000-SEND-MAP)
6. User can enter new account number or press F3
```

**Exit from Account View:**
```cobol
User presses F3
→ COACTVWC checks EIBAID = DFHPF3 (Line 278)
→ XCTLs to COMEN01C with COMMAREA (Line 285-289)
→ Returns user to main menu
```

### Program References

**Calling Programs:**
- **COMEN01C** (Main Menu) - Can XCTL to this program
- **COACTUPC** (Account Update) - Can navigate to this program from menu

**Called Programs:**
- **COMEN01C** (Main Menu) - This program XCTLs back to menu on F3

**Related Programs:**
- **COCRDLIC** (Card List) - Related card management
- **COCRDUPC** (Card Update) - Related card management
- **COCRDSLC** (Card Detail) - Related card management

### COMMAREA Structure

**Communication Area Fields:**
- **CDEMO-FROM-TRANID** - Originating transaction ID
- **CDEMO-FROM-PROGRAM** - Originating program name
- **CDEMO-TO-TRANID** - Target transaction ID
- **CDEMO-TO-PROGRAM** - Target program name
- **CDEMO-USER-ID** - Current user ID
- **CDEMO-USER-TYPE** - User type (Admin/Regular)
- **CDEMO-CUST-ID** - Customer ID context
- **CDEMO-ACCT-ID** - Account ID context
- **CDEMO-CARD-NUM** - Card number context
- **CDEMO-LAST-MAP** - Last map displayed
- **CDEMO-LAST-MAPSET** - Last mapset used

**Usage in Navigation:**
```cobol
MOVE 'COMEN01C' TO CDEMO-TO-PROGRAM
MOVE 'CM00' TO CDEMO-TO-TRANID
EXEC CICS XCTL
    PROGRAM(CDEMO-TO-PROGRAM)
    COMMAREA(CARDDEMO-COMMAREA)
    LENGTH(LENGTH OF CARDDEMO-COMMAREA)
END-EXEC
```

---

## 6. Business Logic and Program Execution Flow

### Comprehensive Narrative

The COACTVWC program implements a READ-ONLY account viewing function in the CardDemo credit card management system. It follows a strict pseudo-conversational design pattern to optimize CICS resource utilization.

#### Phase 1: Program Initialization and Entry Point (Lines 267-317)

When the transaction CAVW is invoked, the program begins at MAIN-PARA:

```cobol
EXEC CICS HANDLE ABEND
    LABEL(ABEND-HANDLING)
END-EXEC.
```

This establishes an abnormal termination handler to catch unexpected errors. The program then evaluates whether this is the first interaction or a subsequent one by examining EIBCALEN (the length of the COMMAREA passed by CICS):

```cobol
IF EIBCALEN = 0
   MOVE LOW-VALUES TO CACTVWAO
   MOVE 'COACTVWC' TO PGMNAMEO
   MOVE 'CAVW' TO TRNNAMEO
   SET SEND-ERASE TO TRUE
   PERFORM 1000-SEND-MAP
ELSE
   PERFORM 9000-READ-DATA
END-IF.
```

**First Time Logic:** If EIBCALEN = 0, this is a fresh start. The program initializes the output screen structure (CACTVWAO) with LOW-VALUES to clear all fields, sets the program name and transaction ID in the header, and calls 1000-SEND-MAP to display an empty screen with just the input field ready.

**Subsequent Interaction:** If EIBCALEN > 0, the user has pressed ENTER or a function key after entering data. The program calls 9000-READ-DATA to process the input.

#### Phase 2: Initial Screen Display (Lines 323-360)

Paragraph 1000-SEND-MAP handles all screen output operations:

```cobol
PERFORM 1100-SCREEN-INIT
PERFORM 1200-SETUP-SCREEN-VARS
PERFORM 1300-SETUP-SCREEN-ATTRS
```

**Screen Initialization (1100-SCREEN-INIT):** Populates header fields with current date and time:
```cobol
EXEC CICS ASKTIME ABSTIME(WS-ABS-TIME) END-EXEC
EXEC CICS FORMATTIME ABSTIME(WS-ABS-TIME)
    DDMMYYYY(WS-CURDATE-DATA)
    TIME(WS-CURTIME-DATA)
    DATESEP('/')
END-EXEC
MOVE WS-CURDATE TO CURDATEO
MOVE WS-CURTIME TO CURTIMEO
```

**Screen Variables Setup (1200-SETUP-SCREEN-VARS):** Sets up literal text labels:
```cobol
MOVE 'CardDemo Account View' TO TITLE01O
MOVE 'Secure Credit Card System' TO TITLE02O
MOVE 'View Account' TO TITLEA1O
```

**Attribute Setup (1300-SETUP-SCREEN-ATTRS):** Configures field display attributes:
```cobol
MOVE DFHBMASK TO ACCTSIDC  (Make input field unprotected)
MOVE DFHBMPRO TO ACSTTUSA  (Make display fields protected)
```

Finally, the SEND MAP command transmits the screen to the terminal:

```cobol
EXEC CICS SEND MAP('CACTVWA')
    MAPSET('COACTVW')
    FROM(CACTVWAO)
    ERASE
    CURSOR
END-EXEC

EXEC CICS RETURN TRANSID('CAVW') END-EXEC
```

The RETURN with TRANSID implements pseudo-conversational design - the program ends but schedules itself to restart (with transaction ID CAVW) when the user presses a key. This frees up CICS resources while waiting for user input.

#### Phase 3: Input Reception and Validation (Lines 363-428)

When the user enters an account number and presses ENTER, CICS restarts the transaction at MAIN-PARA. This time EIBCALEN > 0, so control flows to 9000-READ-DATA:

```cobol
PERFORM 2000-RECEIVE-MAP
IF EIBRESP = DFHRESP(NORMAL)
   PERFORM 2100-EDIT-MAP-INPUTS
   IF WS-ERR-FLG = 'N'
      PERFORM 3000-PROCESS-INPUTS
   END-IF
   PERFORM 1000-SEND-MAP
END-IF
```

**Receive Map (2000-RECEIVE-MAP):** Retrieves user input from terminal:
```cobol
EXEC CICS RECEIVE MAP('CACTVWA')
    MAPSET('COACTVW')
    INTO(CACTVWAI)
END-EXEC
```

If the receive fails (user pressed CLEAR or PA key), an error is set and processing continues to display the error.

**PF Key Handling (Lines 277-302):** Before validation, the program checks if a function key was pressed:

```cobol
EVALUATE EIBAID
   WHEN DFHENTER
      CONTINUE (proceed with validation)
   WHEN DFHPF3
      PERFORM 9100-RETURN-TO-PREV-SCREEN (XCTL to menu)
   WHEN DFHCLEAR
      MOVE 'Please press F3 to exit...' TO ERRMSGO
      SET SEND-DATAONLY-ALARM TO TRUE
   WHEN OTHER
      MOVE 'Invalid key pressed...' TO ERRMSGO
      SET SEND-DATAONLY-ALARM TO TRUE
END-EVALUATE
```

**Input Validation (2100-EDIT-MAP-INPUTS):** Validates the account number:

```cobol
PERFORM 2200-EDIT-ACCOUNT-NUM
IF WS-ERR-FLG = 'N'
   PERFORM 2500-VALIDATE-KEYS
END-IF
```

The validation paragraph 2210-EDIT-ACCOUNT (Lines 705-728) performs three checks:

1. **Non-Empty Check:**
```cobol
IF ACCTIDINI = SPACES OR LOW-VALUES
   MOVE 'Y' TO WS-ERR-FLG
   MOVE 'Please enter an Account Number...' TO ERRMSGO
END-IF
```

2. **Numeric Check:**
```cobol
IF ACCTSIDI IS NOT NUMERIC
   MOVE 'Y' TO WS-ERR-FLG
   MOVE 'Invalid Account Number...' TO ERRMSGO
END-IF
```

3. **Non-Zero Check:**
```cobol
IF ACCTSIDI = ZEROS
   MOVE 'Y' TO WS-ERR-FLG
   MOVE 'Account Number cannot be zero...' TO ERRMSGO
END-IF
```

If any validation fails, WS-ERR-FLG is set to 'Y' and an appropriate error message is placed in ERRMSGO for display.

#### Phase 4: Data Retrieval (Lines 430-618)

If validation succeeds (WS-ERR-FLG = 'N'), the program proceeds to 3000-PROCESS-INPUTS:

```cobol
PERFORM 3100-GET-ACCT-DATA
IF CDEMO-ACCT-FOUND-FLAG = 'Y' AND
   WS-ACCOUNT-MASTER-READ-FLAG = 'Y'
   PERFORM 3200-GET-CUST-DATA
END-IF
```

**Account Data Retrieval (3100-GET-ACCT-DATA):** This involves two sequential VSAM reads:

**Step 1 - Card Cross-Reference Read (Lines 444-490):**
```cobol
MOVE ACCTSIDI TO WS-CARD-RID-ACCT-ID
EXEC CICS READ FILE('CARDDAT')
    INTO(CARD-XREF-RECORD)
    RIDFLD(WS-CARD-RID-ACCT-ID)
    KEYLENGTH(11)
END-EXEC
```

The CARDDAT file (also known as CXACAIX) is a card cross-reference VSAM KSDS indexed by account number. This read validates that the account exists and retrieves:
- Customer ID (FD-CUST-ID)
- Card number (FD-CARD-NUM)
- Account ID (FD-ACCT-ID)

Error handling distinguishes between "not found" (valid response, sets CDEMO-ACCT-FOUND-FLAG = 'N') and other I/O errors (calls 9910-DISPLAY-IO-STATUS).

**Step 2 - Account Master Read (Lines 509-575):**
```cobol
MOVE FD-ACCT-ID TO ACCOUNT-ID
EXEC CICS READ FILE('ACCTDAT')
    INTO(ACCOUNT-RECORD)
    RIDFLD(FD-ACCT-ID)
END-EXEC
```

The ACCTDAT file is the account master VSAM KSDS indexed by account ID. This read retrieves:
- Account status (ACCT-ACTIVE-STATUS) → ACSTTUSO
- Open date (ACCT-OPEN-DATE) → ADTOPENO
- Expiry date (ACCT-EXPIRY-DATE) → AEXPDTO
- Reissue date (ACCT-REISSUE-DATE) → AREISDTO
- Credit limit (ACCT-CREDIT-LIMIT) → ACRDLIMO
- Cash limit (ACCT-CASH-CREDIT-LIMIT) → ACSHLIMO
- Current balance (ACCT-CURR-BAL) → ACURBALO
- Cycle credit (ACCT-CURR-CYC-CREDIT) → ACRCYCRO
- Cycle debit (ACCT-CURR-CYC-DEBIT) → ACRCYDBO
- Account group (ACCT-GROUP-ID) → AADDGRPO

The program formats monetary amounts using the PICOUT attribute (+ZZZ,ZZZ,ZZZ.99) and dates using YYYY-MM-DD format.

**Customer Data Retrieval (3200-GET-CUST-DATA - Lines 579-618):**

Only if both the cross-reference and account master reads succeed, the program retrieves customer data:

```cobol
MOVE FD-CUST-ID TO CUSTOMER-ID
EXEC CICS READ FILE('CUSTDAT')
    INTO(CUSTOMER-RECORD)
    RIDFLD(FD-CUST-ID)
END-EXEC
```

The CUSTDAT file is the customer master VSAM KSDS indexed by customer ID. This read retrieves:
- Customer ID (CUST-ID) → ACSTNUMO
- SSN (CUST-SSN) → ACSTSSNO (formatted with dashes: XXX-XX-XXXX)
- Date of birth (CUST-DOB-YYYYMMDD) → ACSTDOBO
- FICO score (CUST-FICO-CREDIT-SCORE) → ACSTFCOO
- First name (CUST-FIRST-NAME) → ACSFNAMO
- Middle name (CUST-MIDDLE-NAME) → ACSMNAMO
- Last name (CUST-LAST-NAME) → ACSLNAMO
- Address line 1 (CUST-ADDR-LINE-1) → ACSADL1O
- Address line 2 (CUST-ADDR-LINE-2) → ACSADL2O
- City (CUST-ADDR-CITY-NAME) → ACSCITYO
- State (CUST-ADDR-STATE-CD) → ACSSTTEO
- Zip code (CUST-ADDR-ZIP) → ACSZIPCO
- Country (CUST-ADDR-COUNTRY-CD) → ACSCTRYО
- Phone 1 (CUST-PHONE-NUM-1) → ACSPHN1O
- Phone 2 (CUST-PHONE-NUM-2) → ACSPHN2O
- Government ID (CUST-GOVT-ISSUED-ID) → ACSGOVTO
- EFT account (CUST-EFT-ACCOUNT-ID) → ACSEFTCO
- Primary holder flag (CUST-PRI-CARD-HOLDER-IND) → ACSPFLGO

#### Phase 5: Results Display (Lines 621-634)

After successful data retrieval, the program returns to 1000-SEND-MAP which was called from 9000-READ-DATA. This time, instead of LOW-VALUES, the output symbolic map CACTVWAO contains all the retrieved account and customer data.

The SEND MAP command with DATAONLY option updates only the data fields without redrawing the entire screen structure:

```cobol
EXEC CICS SEND MAP('CACTVWA')
    MAPSET('COACTVW')
    FROM(CACTVWAO)
    DATAONLY
    CURSOR
END-EXEC
```

The user now sees the complete account and customer information displayed on the screen. They can:
1. Enter a new account number to view different account
2. Press F3 to return to the main menu
3. Press ENTER with the same account number to refresh the display

#### Phase 6: Pseudo-Conversational Return (Line 293)

After displaying the results, the program executes:

```cobol
EXEC CICS RETURN TRANSID('CAVW') END-EXEC
```

This terminates the program but schedules CAVW to restart when the user presses a key. The cycle repeats, maintaining conversational behavior while freeing CICS resources between interactions.

### Key Business Rules Implemented

1. **READ-ONLY Access:** The program performs only READ operations, no WRITE or UPDATE commands. This ensures data integrity for inquiry-only users.

2. **Sequential File Access:** The program must access three VSAM files in sequence:
   - CARDDAT (to validate account and get customer ID)
   - ACCTDAT (to get account details)
   - CUSTDAT (to get customer details)

3. **Data Consistency:** If any read fails, the program does not proceed to subsequent reads, ensuring partial data is not displayed.

4. **User-Friendly Error Messages:** All error conditions provide clear, actionable messages:
   - "Please enter an Account Number..."
   - "Invalid Account Number..."
   - "Account Number cannot be zero..."
   - "Account not found..."

5. **Audit Trail:** The program maintains COMMAREA context including user ID, transaction history, and navigation path for audit purposes.

6. **Security Context:** User ID and user type from COMMAREA ensure only authorized users can view account data.

---

## 7. Data Structures and Sources

### VSAM Files

#### File 1: CARDDAT (Card Cross-Reference Index - CXACAIX)

**File Type:** KSDS (Key Sequenced Data Set)  
**Primary Key:** Account Number (11 bytes, numeric)  
**Purpose:** Cross-reference index to map account numbers to account IDs and customer IDs  
**Access Method:** Direct read by account number

**Record Layout (from CVCRD01Y copybook):**
```cobol
01  CARD-XREF-RECORD.
    05  FD-CARD-NUM                PIC X(16).
    05  FD-CUST-ID                 PIC 9(09).
    05  FD-ACCT-ID                 PIC 9(11).
    05  FD-CARD-ACCT-ID            PIC X(11).
    05  FILLER                     PIC X(23).
```

**Field Details:**
- **FD-CARD-NUM** (16 bytes): Credit card number
- **FD-CUST-ID** (9 bytes): Customer ID (links to CUSTDAT)
- **FD-ACCT-ID** (11 bytes): Account ID (links to ACCTDAT)
- **FD-CARD-ACCT-ID** (11 bytes): Alternate account ID format
- **FILLER** (23 bytes): Reserved space

**Usage in Program:**
```cobol
MOVE ACCTSIDI TO WS-CARD-RID-ACCT-ID
EXEC CICS READ FILE('CARDDAT')
    INTO(CARD-XREF-RECORD)
    RIDFLD(WS-CARD-RID-ACCT-ID)
    KEYLENGTH(11)
END-EXEC
```

#### File 2: ACCTDAT (Account Master File)

**File Type:** KSDS (Key Sequenced Data Set)  
**Primary Key:** Account ID (11 bytes, numeric)  
**Purpose:** Master file containing all account information  
**Access Method:** Direct read by account ID

**Record Layout (from CVACT01Y copybook):**
```cobol
01  ACCOUNT-RECORD.
    05  ACCT-ID                    PIC 9(11).
    05  ACCT-ACTIVE-STATUS         PIC X(01).
    05  ACCT-CURR-BAL              PIC S9(10)V99 COMP-3.
    05  ACCT-CREDIT-LIMIT          PIC S9(10)V99 COMP-3.
    05  ACCT-CASH-CREDIT-LIMIT     PIC S9(10)V99 COMP-3.
    05  ACCT-OPEN-DATE             PIC X(10).
    05  ACCT-EXPIRY-DATE           PIC X(10).
    05  ACCT-REISSUE-DATE          PIC X(10).
    05  ACCT-CURR-CYC-CREDIT       PIC S9(10)V99 COMP-3.
    05  ACCT-CURR-CYC-DEBIT        PIC S9(10)V99 COMP-3.
    05  ACCT-GROUP-ID              PIC X(10).
    05  CUST-ID                    PIC 9(09).
    05  FILLER                     PIC X(178).
```

**Field Details:**
- **ACCT-ID** (11 bytes): Unique account identifier
- **ACCT-ACTIVE-STATUS** (1 byte): 'Y' = Active, 'N' = Inactive
- **ACCT-CURR-BAL** (7 bytes): Current account balance (packed decimal)
- **ACCT-CREDIT-LIMIT** (7 bytes): Maximum credit limit (packed decimal)
- **ACCT-CASH-CREDIT-LIMIT** (7 bytes): Cash advance limit (packed decimal)
- **ACCT-OPEN-DATE** (10 bytes): Account opening date (YYYY-MM-DD)
- **ACCT-EXPIRY-DATE** (10 bytes): Account expiration date (YYYY-MM-DD)
- **ACCT-REISSUE-DATE** (10 bytes): Card reissue date (YYYY-MM-DD)
- **ACCT-CURR-CYC-CREDIT** (7 bytes): Current billing cycle credits (packed decimal)
- **ACCT-CURR-CYC-DEBIT** (7 bytes): Current billing cycle debits (packed decimal)
- **ACCT-GROUP-ID** (10 bytes): Account group classification
- **CUST-ID** (9 bytes): Customer ID (links to CUSTDAT)

**Usage in Program:**
```cobol
MOVE FD-ACCT-ID TO ACCOUNT-ID
EXEC CICS READ FILE('ACCTDAT')
    INTO(ACCOUNT-RECORD)
    RIDFLD(FD-ACCT-ID)
END-EXEC
MOVE ACCT-ACTIVE-STATUS TO ACSTTUSO
MOVE ACCT-OPEN-DATE TO ADTOPENO
MOVE ACCT-CREDIT-LIMIT TO ACRDLIMO
```

#### File 3: CUSTDAT (Customer Master File)

**File Type:** KSDS (Key Sequenced Data Set)  
**Primary Key:** Customer ID (9 bytes, numeric)  
**Purpose:** Master file containing all customer demographic and contact information  
**Access Method:** Direct read by customer ID

**Record Layout (from CVCUS01Y copybook):**
```cobol
01  CUSTOMER-RECORD.
    05  CUST-ID                    PIC 9(09).
    05  CUST-FIRST-NAME            PIC X(25).
    05  CUST-MIDDLE-NAME           PIC X(25).
    05  CUST-LAST-NAME             PIC X(25).
    05  CUST-ADDR-LINE-1           PIC X(50).
    05  CUST-ADDR-LINE-2           PIC X(50).
    05  CUST-ADDR-CITY-NAME        PIC X(50).
    05  CUST-ADDR-STATE-CD         PIC X(02).
    05  CUST-ADDR-COUNTRY-CD       PIC X(03).
    05  CUST-ADDR-ZIP              PIC X(10).
    05  CUST-PHONE-NUM-1           PIC X(15).
    05  CUST-PHONE-NUM-2           PIC X(15).
    05  CUST-SSN                   PIC 9(09).
    05  CUST-GOVT-ISSUED-ID        PIC X(20).
    05  CUST-DOB-YYYYMMDD          PIC X(10).
    05  CUST-EFT-ACCOUNT-ID        PIC X(10).
    05  CUST-PRI-CARD-HOLDER-IND   PIC X(01).
    05  CUST-FICO-CREDIT-SCORE     PIC 9(03).
    05  FILLER                     PIC X(168).
```

**Field Details:**
- **CUST-ID** (9 bytes): Unique customer identifier
- **CUST-FIRST-NAME** (25 bytes): Customer first name
- **CUST-MIDDLE-NAME** (25 bytes): Customer middle name
- **CUST-LAST-NAME** (25 bytes): Customer last name
- **CUST-ADDR-LINE-1** (50 bytes): Primary address line
- **CUST-ADDR-LINE-2** (50 bytes): Secondary address line
- **CUST-ADDR-CITY-NAME** (50 bytes): City name
- **CUST-ADDR-STATE-CD** (2 bytes): State code
- **CUST-ADDR-COUNTRY-CD** (3 bytes): Country code
- **CUST-ADDR-ZIP** (10 bytes): ZIP/Postal code
- **CUST-PHONE-NUM-1** (15 bytes): Primary phone number
- **CUST-PHONE-NUM-2** (15 bytes): Secondary phone number
- **CUST-SSN** (9 bytes): Social Security Number (numeric)
- **CUST-GOVT-ISSUED-ID** (20 bytes): Government ID reference
- **CUST-DOB-YYYYMMDD** (10 bytes): Date of birth (YYYY-MM-DD)
- **CUST-EFT-ACCOUNT-ID** (10 bytes): Electronic funds transfer account ID
- **CUST-PRI-CARD-HOLDER-IND** (1 byte): Primary cardholder indicator ('Y'/'N')
- **CUST-FICO-CREDIT-SCORE** (3 bytes): FICO credit score (300-850)

**Usage in Program:**
```cobol
MOVE FD-CUST-ID TO CUSTOMER-ID
EXEC CICS READ FILE('CUSTDAT')
    INTO(CUSTOMER-RECORD)
    RIDFLD(FD-CUST-ID)
END-EXEC
MOVE CUST-FIRST-NAME TO ACSFNAMO
MOVE CUST-SSN TO WS-SSN-NUM
(format SSN with dashes)
MOVE WS-SSN-FORMAT TO ACSTSSNO
```

### COMMAREA Structure

**Copybook:** COCOM01Y  
**Purpose:** Inter-program communication area for maintaining transaction context

**Structure:**
```cobol
01  CARDDEMO-COMMAREA.
    05  CDEMO-FROM-TRANID          PIC X(04).
    05  CDEMO-FROM-PROGRAM         PIC X(08).
    05  CDEMO-TO-TRANID            PIC X(04).
    05  CDEMO-TO-PROGRAM           PIC X(08).
    05  CDEMO-USER-ID              PIC X(08).
    05  CDEMO-USER-TYPE            PIC X(01).
    05  CDEMO-PGM-CONTEXT          PIC X(01).
    05  CDEMO-CUST-ID              PIC 9(09).
    05  CDEMO-ACCT-ID              PIC 9(11).
    05  CDEMO-CARD-NUM             PIC X(16).
    05  CDEMO-LAST-MAP             PIC X(08).
    05  CDEMO-LAST-MAPSET          PIC X(08).
    05  CDEMO-ACCT-FOUND-FLAG      PIC X(01).
    05  CDEMO-CUST-FOUND-FLAG      PIC X(01).
    05  FILLER                     PIC X(50).
```

**Field Usage:**
- **CDEMO-FROM-TRANID**: Transaction ID of calling program
- **CDEMO-FROM-PROGRAM**: Program name of caller
- **CDEMO-TO-TRANID**: Transaction ID to transfer to
- **CDEMO-TO-PROGRAM**: Program name to transfer to
- **CDEMO-USER-ID**: Current logged-in user ID
- **CDEMO-USER-TYPE**: 'A' = Admin, 'U' = Regular user
- **CDEMO-PGM-CONTEXT**: Program execution context
- **CDEMO-CUST-ID**: Current customer ID in context
- **CDEMO-ACCT-ID**: Current account ID in context
- **CDEMO-CARD-NUM**: Current card number in context
- **CDEMO-LAST-MAP**: Last BMS map displayed
- **CDEMO-LAST-MAPSET**: Last BMS mapset used
- **CDEMO-ACCT-FOUND-FLAG**: 'Y' if account found, 'N' if not
- **CDEMO-CUST-FOUND-FLAG**: 'Y' if customer found, 'N' if not

### BMS Map Structures

**Input Symbolic Map (CACTVWAI):**
```cobol
01  CACTVWAI.
    02  ACCTSIDI     PIC X(11).    (User input - Account Number)
    02  ACCTIDIL     PIC S9(4) COMP.
    02  ACCTIDIF     PIC X.
    02  FILLER REDEFINES ACCTSIDIF.
        03  ACCTIDIAI  PIC X.
```

**Output Symbolic Map (CACTVWAO):**
```cobol
01  CACTVWAO.
    02  TRNNAMEO     PIC X(04).    (Transaction ID)
    02  PGMNAMEO     PIC X(08).    (Program Name)
    02  TITLE01O     PIC X(40).    (Screen Title)
    02  TITLE02O     PIC X(40).    (Screen Subtitle)
    02  CURDATEO     PIC X(08).    (Current Date)
    02  CURTIMEO     PIC X(08).    (Current Time)
    02  ACCTSIDO     PIC X(11).    (Account Number - echo)
    02  ACSTTUSO     PIC X(01).    (Account Status)
    02  ADTOPENO     PIC X(10).    (Open Date)
    02  AEXPDTO      PIC X(10).    (Expiry Date)
    02  AREISDTO     PIC X(10).    (Reissue Date)
    02  ACRDLIMO     PIC X(15).    (Credit Limit)
    02  ACSHLIMO     PIC X(15).    (Cash Limit)
    02  ACURBALO     PIC X(15).    (Current Balance)
    02  ACRCYCRO     PIC X(15).    (Cycle Credit)
    02  ACRCYDBO     PIC X(15).    (Cycle Debit)
    02  AADDGRPO     PIC X(10).    (Account Group)
    02  ACSTNUMO     PIC X(09).    (Customer ID)
    02  ACSTSSNO     PIC X(12).    (SSN)
    02  ACSTDOBO     PIC X(10).    (Date of Birth)
    02  ACSTFCOO     PIC X(03).    (FICO Score)
    02  ACSFNAMO     PIC X(25).    (First Name)
    02  ACSMNAMO     PIC X(25).    (Middle Name)
    02  ACSLNAMO     PIC X(18).    (Last Name)
    02  ACSADL1O     PIC X(50).    (Address Line 1)
    02  ACSADL2O     PIC X(50).    (Address Line 2)
    02  ACSCITYO     PIC X(50).    (City)
    02  ACSSTTEO     PIC X(02).    (State)
    02  ACSZIPCO     PIC X(05).    (ZIP Code)
    02  ACSCTRYО     PIC X(03).    (Country)
    02  ACSPHN1O     PIC X(13).    (Phone 1)
    02  ACSPHN2O     PIC X(13).    (Phone 2)
    02  ACSGOVTO     PIC X(20).    (Government ID)
    02  ACSEFTCO     PIC X(10).    (EFT Account)
    02  ACSPFLGO     PIC X(01).    (Primary Holder Flag)
    02  INFOMSGO     PIC X(45).    (Info Message)
    02  ERRMSGO      PIC X(78).    (Error Message)
```

### Data Flow Diagram

```
┌────────────┐
│   USER     │
│  Terminal  │
└─────┬──────┘
      │ Account Number
      ↓
┌────────────┐     RIDFLD=AcctNum     ┌────────────┐
│ COACTVWC   │────────────────────────→│  CARDDAT   │
│  Program   │←────────────────────────│  (CXACAIX) │
└─────┬──────┘  FD-ACCT-ID, FD-CUST-ID└────────────┘
      │
      │ FD-ACCT-ID
      ↓
┌────────────┐     RIDFLD=FD-ACCT-ID  ┌────────────┐
│ COACTVWC   │────────────────────────→│  ACCTDAT   │
│  Program   │←────────────────────────│ (Account)  │
└─────┬──────┘   ACCOUNT-RECORD       └────────────┘
      │
      │ FD-CUST-ID
      ↓
┌────────────┐     RIDFLD=FD-CUST-ID  ┌────────────┐
│ COACTVWC   │────────────────────────→│  CUSTDAT   │
│  Program   │←────────────────────────│ (Customer) │
└─────┬──────┘   CUSTOMER-RECORD      └────────────┘
      │
      │ Formatted Display Data
      ↓
┌────────────┐
│   USER     │
│  Terminal  │
└────────────┘
```

---

## 8. Dependencies

### External Programs

**COMEN01C (Main Menu Program)**
- **Relationship:** Parent program / Navigation target
- **Communication Method:** XCTL with COMMAREA
- **Direction:** COACTVWC → COMEN01C (on F3 exit)
- **COMMAREA Fields Used:**
  - CDEMO-TO-PROGRAM = 'COMEN01C'
  - CDEMO-TO-TRANID = 'CM00'
  - CDEMO-USER-ID (passed through)
  - CDEMO-USER-TYPE (passed through)

**Call Sequence:**
```cobol
MOVE 'COMEN01C' TO CDEMO-TO-PROGRAM
MOVE 'CM00' TO CDEMO-TO-TRANID
EXEC CICS XCTL
    PROGRAM(CDEMO-TO-PROGRAM)
    COMMAREA(CARDDEMO-COMMAREA)
    LENGTH(LENGTH OF CARDDEMO-COMMAREA)
END-EXEC
```

### VSAM Files

**CARDDAT (Card Cross-Reference File - CXACAIX)**
- **Access Type:** READ
- **Key Field:** Account Number (11 bytes)
- **Record Layout:** CARD-XREF-RECORD (from CVCRD01Y)
- **Purpose:** Map account number to account ID and customer ID
- **Error Handling:** NOTFND treated as valid response

**ACCTDAT (Account Master File)**
- **Access Type:** READ
- **Key Field:** Account ID (11 bytes)
- **Record Layout:** ACCOUNT-RECORD (from CVACT01Y)
- **Purpose:** Retrieve account details (status, dates, limits, balances)
- **Error Handling:** NOTFND displays "Account details not found"

**CUSTDAT (Customer Master File)**
- **Access Type:** READ
- **Key Field:** Customer ID (9 bytes)
- **Record Layout:** CUSTOMER-RECORD (from CVCUS01Y)
- **Purpose:** Retrieve customer demographics and contact information
- **Error Handling:** NOTFND displays "Customer details not found"

### Copybooks

**Data Structure Copybooks:**
1. **CVCRD01Y** - Card data structures
2. **CVACT01Y** - Account record layout (ACCTDAT)
3. **CVACT02Y** - Account cross-reference structures
4. **CVACT03Y** - Additional account structures
5. **CVCUS01Y** - Customer record layout (CUSTDAT)
6. **COCOM01Y** - COMMAREA structure

**BMS and Screen Copybooks:**
7. **COACTVW** - BMS symbolic map (CACTVWA screen)
8. **DFHBMSCA** - CICS BMS attribute byte definitions
9. **DFHAID** - CICS attention identifier definitions
10. **COTTL01Y** - Common title lines

**Utility Copybooks:**
11. **CSDAT01Y** - Date formatting utilities
12. **CSMSG01Y** - Common message definitions
13. **CSMSG02Y** - Extended message definitions
14. **CSUSR01Y** - User security structures
15. **CSSTRPFY** - String manipulation utilities

### CICS Resources

**Transaction Definition:**
- **Transaction ID:** CAVW
- **Program:** COACTVWC
- **Profile:** (Defined in CICS CSD)

**BMS Resources:**
- **Mapset:** COACTVW
- **Map:** CACTVWA
- **Physical Map:** Pre-compiled and loaded in CICS

**COMMAREA:**
- **Structure:** CARDDEMO-COMMAREA (from COCOM01Y)
- **Length:** 150 bytes (approximate)
- **Usage:** Maintains context across pseudo-conversational interactions

### Environmental Dependencies

**CICS System Services:**
- **HANDLE ABEND** - Error recovery
- **ASKTIME** - Current timestamp
- **FORMATTIME** - Date/time formatting
- **SEND MAP** - Screen output
- **RECEIVE MAP** - Screen input
- **READ** - File I/O
- **XCTL** - Program transfer
- **RETURN** - Transaction termination

**Date/Time Services:**
- EIBDATE - Current date from CICS
- EIBTIME - Current time from CICS
- FORMATTIME - Convert to display format

**Terminal Services:**
- 3270 terminal emulation
- BMS (Basic Mapping Support)
- EIBAID - Attention identifier
- EIBCALEN - COMMAREA length

---

## 9. Error Handling

### CICS Error Management

**HANDLE ABEND Setup (Line 272-274):**
```cobol
EXEC CICS HANDLE ABEND
    LABEL(ABEND-HANDLING)
END-EXEC.
```

This command directs CICS to transfer control to the ABEND-HANDLING paragraph (Lines 929-940) if any abend occurs during program execution.

**ABEND-HANDLING Paragraph:**
```cobol
ABEND-HANDLING.
    MOVE 'Abnormal termination occurred...' TO WS-MESSAGE
    PERFORM 9999-ABEND-ROUTINE
```

The routine displays an error message and performs an orderly termination.

### File I/O Error Handling

#### Error Scenario 1: Card Cross-Reference Not Found

**Location:** Lines 444-490  
**Error Code:** DFHRESP(NOTFND)  
**Cause:** Account number does not exist in CARDDAT file

**Handling:**
```cobol
EVALUATE WS-RESP-CD
   WHEN DFHRESP(NORMAL)
      MOVE 'Y' TO CDEMO-ACCT-FOUND-FLAG
      MOVE FD-ACCT-ID TO CDEMO-ACCT-ID
      MOVE FD-CUST-ID TO CDEMO-CUST-ID
   WHEN DFHRESP(NOTFND)
      MOVE 'N' TO CDEMO-ACCT-FOUND-FLAG
      MOVE 'Account not found...' TO ERRMSGO
      SET SEND-DATAONLY-ALARM TO TRUE
   WHEN OTHER
      MOVE 'CARDDAT' TO ERROR-FILE
      MOVE 'READ' TO ERROR-OPNAME
      PERFORM 9910-DISPLAY-IO-STATUS
END-EVALUATE
```

**Recovery:** Display error message and allow user to enter different account number

#### Error Scenario 2: Account Master Read Failure

**Location:** Lines 509-575  
**Error Code:** DFHRESP(NOTFND) or other I/O errors  
**Cause:** Account ID from cross-reference does not exist in ACCTDAT, or file I/O error

**Handling:**
```cobol
EVALUATE WS-RESP-CD
   WHEN DFHRESP(NORMAL)
      MOVE 'Y' TO WS-ACCOUNT-MASTER-READ-FLAG
      (Move account fields to screen)
   WHEN DFHRESP(NOTFND)
      MOVE 'Account details not found...' TO ERRMSGO
      SET SEND-DATAONLY-ALARM TO TRUE
   WHEN OTHER
      MOVE 'ACCTDAT' TO ERROR-FILE
      MOVE 'READ' TO ERROR-OPNAME
      PERFORM 9910-DISPLAY-IO-STATUS
END-EVALUATE
```

**Recovery:** Display error message, do not attempt customer read

#### Error Scenario 3: Customer Master Read Failure

**Location:** Lines 579-618  
**Error Code:** DFHRESP(NOTFND) or other I/O errors  
**Cause:** Customer ID from account record does not exist in CUSTDAT, or file I/O error

**Handling:**
```cobol
EVALUATE WS-RESP-CD
   WHEN DFHRESP(NORMAL)
      MOVE 'Y' TO WS-CUST-MASTER-READ-FLAG
      (Move customer fields to screen)
   WHEN DFHRESP(NOTFND)
      MOVE 'Customer details not found...' TO ERRMSGO
      SET SEND-DATAONLY-ALARM TO TRUE
   WHEN OTHER
      MOVE 'CUSTDAT' TO ERROR-FILE
      MOVE 'READ' TO ERROR-OPNAME
      PERFORM 9910-DISPLAY-IO-STATUS
END-EVALUATE
```

**Recovery:** Display error message, show account data without customer data

### Input Validation Errors

#### Error Scenario 4: Empty Account Number

**Location:** Lines 705-728 (2210-EDIT-ACCOUNT)  
**Validation:** ACCTIDINI = SPACES OR LOW-VALUES

**Handling:**
```cobol
IF ACCTIDINI = SPACES OR LOW-VALUES
   MOVE 'Y' TO WS-ERR-FLG
   MOVE 'Please enter an Account Number...' TO ERRMSGO
   SET SEND-DATAONLY-ALARM TO TRUE
END-IF
```

**Recovery:** Display error message and prompt for input

#### Error Scenario 5: Non-Numeric Account Number

**Location:** Lines 705-728 (2210-EDIT-ACCOUNT)  
**Validation:** ACCTSIDI IS NOT NUMERIC

**Handling:**
```cobol
IF WS-ERR-FLG = 'N'
   IF ACCTSIDI IS NOT NUMERIC
      MOVE 'Y' TO WS-ERR-FLG
      MOVE 'Invalid Account Number...' TO ERRMSGO
      SET SEND-DATAONLY-ALARM TO TRUE
   END-IF
END-IF
```

**Recovery:** Display error message and prompt for corrected input

#### Error Scenario 6: Zero Account Number

**Location:** Lines 705-728 (2210-EDIT-ACCOUNT)  
**Validation:** ACCTSIDI = ZEROS

**Handling:**
```cobol
IF WS-ERR-FLG = 'N'
   IF ACCTSIDI = ZEROS
      MOVE 'Y' TO WS-ERR-FLG
      MOVE 'Account Number cannot be zero...' TO ERRMSGO
      SET SEND-DATAONLY-ALARM TO TRUE
   END-IF
END-IF
```

**Recovery:** Display error message and prompt for valid input

### User Action Errors

#### Error Scenario 7: Invalid Function Key

**Location:** Lines 395-399  
**Condition:** EIBAID not in (DFHENTER, DFHPF3, DFHCLEAR)

**Handling:**
```cobol
WHEN OTHER
   MOVE 'Invalid key pressed. Please see below...' TO INFOMSGO
   MOVE 'Valid keys: Enter, F3=Exit' TO ERRMSGO
   SET SEND-DATAONLY-ALARM TO TRUE
```

**Recovery:** Display guidance message and continue

### Catastrophic Error Handling

#### 9910-DISPLAY-IO-STATUS (Lines 871-927)

**Purpose:** Format and display detailed I/O error information for unexpected file errors

```cobol
MOVE WS-RESP-CD TO WS-RESP-EDIT
MOVE WS-RESP-EDIT TO ERROR-RESP
STRING 'File Error: ' DELIMITED BY SIZE
       ERROR-OPNAME DELIMITED BY SIZE
       ' on ' DELIMITED BY SIZE
       ERROR-FILE DELIMITED BY SIZE
       ' returned RESP ' DELIMITED BY SIZE
       ERROR-RESP DELIMITED BY SIZE
       INTO WS-MESSAGE
END-STRING
PERFORM 9999-ABEND-ROUTINE
```

**Information Captured:**
- Operation name (READ, WRITE, etc.)
- File name (CARDDAT, ACCTDAT, CUSTDAT)
- RESP code
- RESP2 code

#### 9999-ABEND-ROUTINE (Lines 899-927)

**Purpose:** Display error message and terminate transaction abnormally

```cobol
EXEC CICS SEND TEXT
    FROM(WS-MESSAGE)
    LENGTH(LENGTH OF WS-MESSAGE)
    ERASE
    FREEKB
END-EXEC

EXEC CICS ABEND
    ABCODE('HAPI')
END-EXEC
```

**ABEND Code:** 'HAPI' (Application-specific identifier)

### Error Message Display Mechanism

**Error Message Field:** ERRMSGO (78 characters)  
**Info Message Field:** INFOMSGO (45 characters)

**Display Attributes:**
- Error messages: Red, bright, alarm
- Info messages: Neutral, normal

**Message Positioning:**
- INFOMSG at line 27, column 23
- ERRMSG at line 28, column 1

### Error Prevention Strategies

1. **Sequential Validation:** Validate input before attempting file access
2. **Defensive Programming:** Check all RESP codes explicitly
3. **Graceful Degradation:** Continue with partial data when possible
4. **User-Friendly Messages:** Clear, actionable error text
5. **Context Preservation:** Maintain COMMAREA across errors for recovery
6. **Audit Trail:** Log error conditions via COMMAREA flags

---

## 10. Additional Technical Details

### Pseudo-Conversational Design

**Implementation Pattern:**

The program implements a strict pseudo-conversational design to optimize CICS resource utilization:

```cobol
IF EIBCALEN = 0
   (First time - display empty screen)
   PERFORM 1000-SEND-MAP
ELSE
   (Subsequent - process input)
   PERFORM 9000-READ-DATA
END-IF

(At end of processing)
EXEC CICS RETURN TRANSID('CAVW') END-EXEC
```

**Benefits:**
1. **Resource Efficiency:** Program terminates after each interaction, freeing memory and CPU
2. **Scalability:** Supports many concurrent users without resource contention
3. **Reliability:** Each interaction is independent, reducing state management complexity

**State Management:**
- EIBCALEN determines if this is first interaction (= 0) or subsequent (> 0)
- COMMAREA maintains context across interactions
- Screen data preserved via BMS terminal storage

### READ-ONLY Nature

**Characteristics:**

This program performs **NO UPDATE operations**:
- No WRITE commands
- No REWRITE commands
- No DELETE commands
- No SYNCPOINT commands

**Implications:**
1. **Data Integrity:** Cannot corrupt data through programming errors
2. **Concurrency:** No locking issues, multiple users can view same account simultaneously
3. **Performance:** Faster execution, no commit overhead
4. **Security:** Suitable for inquiry-only users without update privileges
5. **Audit:** No need to log data modifications

**File Access Pattern:**
```
READ CARDDAT (Cross-reference lookup)
  ↓
READ ACCTDAT (Account details)
  ↓
READ CUSTDAT (Customer details)
  ↓
DISPLAY results (No updates)
```

### Date and Time Handling

**Current Date/Time Acquisition:**
```cobol
EXEC CICS ASKTIME ABSTIME(WS-ABS-TIME) END-EXEC
```

Retrieves CICS internal timestamp (number of milliseconds since 1900-01-01)

**Formatting for Display:**
```cobol
EXEC CICS FORMATTIME
    ABSTIME(WS-ABS-TIME)
    DDMMYYYY(WS-CURDATE-DATA)
    TIME(WS-CURTIME-DATA)
    DATESEP('/')
END-EXEC
```

Converts to:
- Date: MM/DD/YYYY format
- Time: HH:MM:SS format

**Date Storage Formats:**
- ACCT-OPEN-DATE: YYYY-MM-DD (10 characters)
- ACCT-EXPIRY-DATE: YYYY-MM-DD (10 characters)
- CUST-DOB-YYYYMMDD: YYYY-MM-DD (10 characters)

### BMS Attribute Management

**Attribute Bytes Control:**

**DFHBMASK (Unprotected):**
```cobol
MOVE DFHBMASK TO ACCTSIDC  (Input field)
```
Allows user input, cursor can enter field

**DFHBMPRO (Protected):**
```cobol
MOVE DFHBMPRO TO ACSTTUSA  (Display fields)
```
Prevents user modification, skip on cursor movement

**DFHBMASB (Skip, Bright):**
```cobol
MOVE DFHBMASB TO ERRMSGA  (Error messages)
```
Protected, highlighted for visibility

**Color Attributes:**
- DFHGREEN: Input fields
- DFHBLUE: Header information
- DFHYELLOW: Titles
- DFHTURQ: Customer data grouping
- DFHRED: Error messages

### COMMAREA Management

**Structure Length:** Approximately 150 bytes

**Key Fields Updated:**
```cobol
MOVE 'COACTVWC' TO CDEMO-TO-PROGRAM
MOVE 'CAVW' TO CDEMO-TO-TRANID
MOVE FD-ACCT-ID TO CDEMO-ACCT-ID
MOVE FD-CUST-ID TO CDEMO-CUST-ID
MOVE 'Y' TO CDEMO-ACCT-FOUND-FLAG
```

**Usage Patterns:**
1. **Navigation Context:** Track previous and next programs
2. **User Context:** Maintain user ID and type
3. **Data Context:** Current account/customer/card in focus
4. **Status Flags:** Success/failure of operations
5. **Audit Trail:** Transaction history across programs

### Working Storage Organization

**Sections:**
1. **CICS Variables (Lines 35-45):** Response codes, transaction ID
2. **Flags (Lines 50-65):** Input validation, PFK validation, edit flags
3. **File Keys (Lines 73-80):** Cross-reference key structures
4. **File Flags (Lines 81-85):** Read success indicators
5. **Error Messages (Lines 86-132):** Formatted error text
6. **Processing Variables (Lines 134-172):** Error flag, message buffer

### Performance Characteristics

**Response Time Factors:**
1. **VSAM Read Performance:** 3 sequential VSAM reads required
2. **BMS Overhead:** Screen send/receive operations
3. **Network Latency:** Terminal communication time
4. **CICS Dispatch:** Transaction initiation overhead

**Optimization Techniques:**
1. **Single Input Field:** Minimal data transmission from terminal
2. **DATAONLY Option:** Efficient screen updates without full redraw
3. **Early Validation:** Filter invalid input before file access
4. **Sequential Reads:** Minimize file I/O by stopping on first error

**Scalability:**
- Pseudo-conversational design supports 1000+ concurrent users
- READ-ONLY nature eliminates locking contention
- No long-running transactions
- Minimal working storage requirements

### Security Considerations

**Authentication:**
- User ID passed via COMMAREA from login program
- User type determines available menu options

**Authorization:**
- READ-ONLY operations limit security exposure
- No update capabilities prevent unauthorized data modification

**Data Privacy:**
- SSN displayed but not editable
- Account balances visible only to authorized users
- FICO scores protected information

**Audit Requirements:**
- User ID logged in COMMAREA
- Transaction ID tracked
- Account access history maintainable through CICS logging

### Migration Considerations for Cloud Modernization

**High Priority for Migration:**
1. **Simple Business Logic:** Single function (view account), easy to replicate
2. **READ-ONLY Nature:** No transaction management complexity
3. **Clear Data Flow:** Sequential file reads, straightforward to implement with APIs
4. **Standard Validation:** Common patterns transferable to modern frameworks

**Migration Approach:**

**Phase 1 - Database:**
- Convert VSAM KSDS to relational database tables
- CARDDAT → CARD_XREF table (indexed by account_number)
- ACCTDAT → ACCOUNTS table (indexed by account_id)
- CUSTDAT → CUSTOMERS table (indexed by customer_id)
- Maintain referential integrity through foreign keys

**Phase 2 - Business Logic:**
- Implement as RESTful API endpoint: GET /accounts/{accountNumber}
- Validation logic → Input validation layer
- File reads → Database queries with JOIN operations
- Error handling → HTTP status codes and JSON error responses

**Phase 3 - Presentation:**
- BMS map → HTML/React component
- 24×80 screen → Responsive web UI
- Function keys → Button clicks
- COMMAREA → Session management / JWT tokens

**Sample Modern Implementation:**
```
API Endpoint: GET /api/v1/accounts/{accountNumber}
Response:
{
  "account": {
    "id": "12345678901",
    "status": "Y",
    "openDate": "2020-01-15",
    "creditLimit": 15000.00,
    ...
  },
  "customer": {
    "id": "123456789",
    "firstName": "John",
    "lastName": "Doe",
    ...
  }
}
```

**Cloud Deployment Strategy:**
- Containerize as microservice (Docker)
- Deploy to Kubernetes or serverless (AWS Lambda)
- Use cloud-native database (Amazon RDS, Aurora)
- Implement API Gateway for routing
- Add authentication/authorization (OAuth 2.0, AWS Cognito)

**Benefits of Migration:**
1. **Reduced Infrastructure Costs:** No mainframe MIPS charges
2. **Improved User Experience:** Modern responsive UI
3. **Better Scalability:** Auto-scaling cloud resources
4. **Enhanced Security:** Modern authentication protocols
5. **API Enablement:** Mobile app integration, third-party access

---

## Summary

The COACTVWC program is a well-structured, efficient COBOL CICS application that exemplifies best practices in mainframe transaction processing. Its READ-ONLY nature, pseudo-conversational design, and comprehensive error handling make it an excellent candidate for cloud migration. The program serves a critical business function - enabling users to quickly view complete account and customer information - while maintaining strict data integrity and security controls.

**Key Strengths:**
- Clean separation of concerns (validation, retrieval, display)
- Robust error handling with user-friendly messages
- Efficient resource utilization through pseudo-conversational design
- Comprehensive validation rules preventing invalid data access
- Clear audit trail through COMMAREA management

**Migration Readiness:**
- Simple, well-defined business logic
- READ-ONLY operations reduce complexity
- Clear data dependencies (3 VSAM files)
- Standard input validation patterns
- Single-screen interaction model

This analysis provides a complete foundation for understanding, maintaining, and eventually migrating the COACTVWC program to a modern cloud-based architecture while preserving its core business functionality and user experience.
