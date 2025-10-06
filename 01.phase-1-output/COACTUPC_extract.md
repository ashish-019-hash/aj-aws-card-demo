# COACTUPC - Account Update Program - COBOL CICS Extraction Documentation

**Program Name:** COACTUPC  
**Transaction ID:** CAUP  
**Mapset:** COACTUP  
**Map:** CACTUPA  
**Purpose:** Accept and process credit card account updates (Account Master and Customer Master data)  
**Date:** October 03, 2025  
**Analyst:** Devin AI  

---

## 1. Screen Visualization

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│                                                                                 │
│                          AWS Mainframe Modernization                            │
│                       Credit Card Demo Application                              │
│                                                                                 │
│ Tran-Id: CAUP                    Date: MM/DD/YY                                │
│ Program: COACTUPC                Time: HH:MM:SS                                │
│                                                                                 │
│                           ACCOUNT UPDATE                                        │
│                                                                                 │
│    Account Number: ___________                                                 │
│                                                                                 │
│    Account Status: _                                                           │
│    Open Date: ____ / __ / __       Credit Limit: ______________                │
│    Expiry Date: ____ / __ / __     Current Balance: ______________             │
│    Reissue Date: ____ / __ / __    Cash Credit Limit: ______________           │
│                                    Current Cycle Credit: ______________        │
│                                    Current Cycle Debit: ______________         │
│    Account Group: __________                                                   │
│                                                                                 │
│    Customer Id: __________                                                     │
│    SSN: ___ - __ - ____           FICO Score: ___                             │
│    Date of Birth: ____ / __ / __                                              │
│    First Name: _________________________   Middle: _________________________   │
│    Last Name: _________________________                                        │
│    Address Line 1: __________________________________________________          │
│    Address Line 2: __________________________________________________          │
│    City: __________________________________________________                     │
│    State: __                          Zip: _____                              │
│    Country: ___                                                                │
│    Phone 1: ___ - ___ - ____          Phone 2: ___ - ___ - ____              │
│    EFT Account Id: __________         Primary Card Holder Y/N: _              │
│                                                                                 │
│                                                                                 │
│    [Information message area]                                                  │
│                                                                                 │
│ [Error message area]                                                           │
│ ENTER=Process F3=Exit F5=Save F12=Cancel                                      │
└─────────────────────────────────────────────────────────────────────────────────┘
```

---

## 2. Field Details Table

| Line | Column | Field Name | Type | Length | Data Source | Attribute |
|------|--------|------------|------|--------|-------------|-----------|
| 1 | 1 | TITLE01 | Display | 40 | Literal | ASKIP, BRT |
| 2 | 1 | TITLE02 | Display | 40 | Literal | ASKIP, BRT |
| 4 | 10 | TRNNAME | Display | 4 | LIT-THISTRANID | ASKIP, NORM |
| 4 | 50 | CURDATE | Display | 8 | Current Date MM/DD/YY | ASKIP, NORM |
| 5 | 10 | PGMNAME | Display | 8 | LIT-THISPGM | ASKIP, NORM |
| 5 | 50 | CURTIME | Display | 8 | Current Time HH:MM:SS | ASKIP, NORM |
| 7 | 25 | - | Display | 14 | "ACCOUNT UPDATE" | ASKIP, BRT |
| 9 | 5 | - | Display | 16 | "Account Number:" | ASKIP, TURQUOISE |
| 9 | 22 | ACCTSID | Input | 11 | ACCTDAT-ACCT-ID | UNPROT, FSET, IC |
| 11 | 5 | - | Display | 16 | "Account Status:" | ASKIP, TURQUOISE |
| 11 | 22 | ACSTTUS | Input | 1 | ACCTDAT-ACCT-ACTIVE-STATUS | UNPROT, FSET |
| 12 | 5 | - | Display | 11 | "Open Date:" | ASKIP, TURQUOISE |
| 12 | 17 | OPNYEAR | Input | 4 | ACCTDAT-ACCT-OPEN-DATE(1:4) | UNPROT, FSET |
| 12 | 23 | OPNMON | Input | 2 | ACCTDAT-ACCT-OPEN-DATE(6:2) | UNPROT, FSET |
| 12 | 27 | OPNDAY | Input | 2 | ACCTDAT-ACCT-OPEN-DATE(9:2) | UNPROT, FSET |
| 12 | 35 | - | Display | 14 | "Credit Limit:" | ASKIP, TURQUOISE |
| 12 | 50 | ACRDLIM | Input | 14 | ACCTDAT-ACCT-CREDIT-LIMIT | UNPROT, FSET, NUM |
| 13 | 5 | - | Display | 12 | "Expiry Date:" | ASKIP, TURQUOISE |
| 13 | 17 | EXPYEAR | Input | 4 | ACCTDAT-ACCT-EXPIRAION-DATE(1:4) | UNPROT, FSET |
| 13 | 23 | EXPMON | Input | 2 | ACCTDAT-ACCT-EXPIRAION-DATE(6:2) | UNPROT, FSET |
| 13 | 27 | EXPDAY | Input | 2 | ACCTDAT-ACCT-EXPIRAION-DATE(9:2) | UNPROT, FSET |
| 13 | 35 | - | Display | 17 | "Current Balance:" | ASKIP, TURQUOISE |
| 13 | 53 | ACURBAL | Input | 14 | ACCTDAT-ACCT-CURR-BAL | UNPROT, FSET, NUM |
| 14 | 5 | - | Display | 13 | "Reissue Date:" | ASKIP, TURQUOISE |
| 14 | 17 | RISYEAR | Input | 4 | ACCTDAT-ACCT-REISSUE-DATE(1:4) | UNPROT, FSET |
| 14 | 23 | RISMON | Input | 2 | ACCTDAT-ACCT-REISSUE-DATE(6:2) | UNPROT, FSET |
| 14 | 27 | RISDAY | Input | 2 | ACCTDAT-ACCT-REISSUE-DATE(9:2) | UNPROT, FSET |
| 14 | 35 | - | Display | 20 | "Cash Credit Limit:" | ASKIP, TURQUOISE |
| 14 | 56 | ACSHLIM | Input | 14 | ACCTDAT-ACCT-CASH-CREDIT-LIMIT | UNPROT, FSET, NUM |
| 15 | 35 | - | Display | 22 | "Current Cycle Credit:" | ASKIP, TURQUOISE |
| 15 | 58 | ACRCYCR | Input | 14 | ACCTDAT-ACCT-CURR-CYC-CREDIT | UNPROT, FSET, NUM |
| 16 | 35 | - | Display | 21 | "Current Cycle Debit:" | ASKIP, TURQUOISE |
| 16 | 57 | ACRCYDB | Input | 14 | ACCTDAT-ACCT-CURR-CYC-DEBIT | UNPROT, FSET, NUM |
| 17 | 5 | - | Display | 15 | "Account Group:" | ASKIP, TURQUOISE |
| 17 | 21 | AADDGRP | Input | 10 | ACCTDAT-ACCT-GROUP-ID | UNPROT, FSET |
| 19 | 5 | - | Display | 13 | "Customer Id:" | ASKIP, TURQUOISE |
| 19 | 19 | ACSTNUM | Display | 10 | CUSTDAT-CUST-ID | ASKIP, NORM |
| 20 | 5 | - | Display | 5 | "SSN:" | ASKIP, TURQUOISE |
| 20 | 11 | ACTSSN1 | Input | 3 | CUSTDAT-CUST-SSN(1:3) | UNPROT, FSET |
| 20 | 16 | ACTSSN2 | Input | 2 | CUSTDAT-CUST-SSN(4:2) | UNPROT, FSET |
| 20 | 20 | ACTSSN3 | Input | 4 | CUSTDAT-CUST-SSN(6:4) | UNPROT, FSET |
| 20 | 35 | - | Display | 12 | "FICO Score:" | ASKIP, TURQUOISE |
| 20 | 48 | ACSTFCO | Input | 3 | CUSTDAT-CUST-FICO-CREDIT-SCORE | UNPROT, FSET, NUM |
| 21 | 5 | - | Display | 15 | "Date of Birth:" | ASKIP, TURQUOISE |
| 21 | 21 | DOBYEAR | Input | 4 | CUSTDAT-CUST-DOB-YYYY-MM-DD(1:4) | UNPROT, FSET |
| 21 | 27 | DOBMON | Input | 2 | CUSTDAT-CUST-DOB-YYYY-MM-DD(6:2) | UNPROT, FSET |
| 21 | 31 | DOBDAY | Input | 2 | CUSTDAT-CUST-DOB-YYYY-MM-DD(9:2) | UNPROT, FSET |
| 22 | 5 | - | Display | 12 | "First Name:" | ASKIP, TURQUOISE |
| 22 | 18 | ACSFNAM | Input | 25 | CUSTDAT-CUST-FIRST-NAME | UNPROT, FSET |
| 22 | 50 | - | Display | 8 | "Middle:" | ASKIP, TURQUOISE |
| 22 | 59 | ACSMNAM | Input | 25 | CUSTDAT-CUST-MIDDLE-NAME | UNPROT, FSET |
| 23 | 5 | - | Display | 11 | "Last Name:" | ASKIP, TURQUOISE |
| 23 | 17 | ACSLNAM | Input | 25 | CUSTDAT-CUST-LAST-NAME | UNPROT, FSET |
| 24 | 5 | - | Display | 16 | "Address Line 1:" | ASKIP, TURQUOISE |
| 24 | 22 | ACSADL1 | Input | 50 | CUSTDAT-CUST-ADDR-LINE-1 | UNPROT, FSET |
| 25 | 5 | - | Display | 16 | "Address Line 2:" | ASKIP, TURQUOISE |
| 25 | 22 | ACSADL2 | Input | 50 | CUSTDAT-CUST-ADDR-LINE-2 | UNPROT, FSET |
| 26 | 5 | - | Display | 6 | "City:" | ASKIP, TURQUOISE |
| 26 | 12 | ACSCITY | Input | 50 | CUSTDAT-CUST-ADDR-LINE-3 | UNPROT, FSET |
| 27 | 5 | - | Display | 7 | "State:" | ASKIP, TURQUOISE |
| 27 | 13 | ACSSTTE | Input | 2 | CUSTDAT-CUST-ADDR-STATE-CD | UNPROT, FSET |
| 27 | 35 | - | Display | 5 | "Zip:" | ASKIP, TURQUOISE |
| 27 | 41 | ACZZIPC | Input | 5 | CUSTDAT-CUST-ADDR-ZIP | UNPROT, FSET, NUM |
| 28 | 5 | - | Display | 9 | "Country:" | ASKIP, TURQUOISE |
| 28 | 15 | ACSCTRY | Input | 3 | CUSTDAT-CUST-ADDR-COUNTRY-CD | UNPROT, FSET |
| 29 | 5 | - | Display | 9 | "Phone 1:" | ASKIP, TURQUOISE |
| 29 | 15 | ACSPH1A | Input | 3 | CUSTDAT-CUST-PHONE-NUM-1(2:3) | UNPROT, FSET, NUM |
| 29 | 20 | ACSPH1B | Input | 3 | CUSTDAT-CUST-PHONE-NUM-1(6:3) | UNPROT, FSET, NUM |
| 29 | 25 | ACSPH1C | Input | 4 | CUSTDAT-CUST-PHONE-NUM-1(10:4) | UNPROT, FSET, NUM |
| 29 | 40 | - | Display | 9 | "Phone 2:" | ASKIP, TURQUOISE |
| 29 | 50 | ACSPH2A | Input | 3 | CUSTDAT-CUST-PHONE-NUM-2(2:3) | UNPROT, FSET, NUM |
| 29 | 55 | ACSPH2B | Input | 3 | CUSTDAT-CUST-PHONE-NUM-2(6:3) | UNPROT, FSET, NUM |
| 29 | 60 | ACSPH2C | Input | 4 | CUSTDAT-CUST-PHONE-NUM-2(10:4) | UNPROT, FSET, NUM |
| 30 | 5 | - | Display | 16 | "EFT Account Id:" | ASKIP, TURQUOISE |
| 30 | 22 | ACSEFTC | Input | 10 | CUSTDAT-CUST-EFT-ACCOUNT-ID | UNPROT, FSET |
| 30 | 40 | - | Display | 24 | "Primary Card Holder Y/N:" | ASKIP, TURQUOISE |
| 30 | 65 | ACSPFLG | Input | 1 | CUSTDAT-CUST-PRI-CARD-HOLDER-IND | UNPROT, FSET |
| 33 | 5 | INFOMSG | Display | 45 | Status/info messages | ASKIP, NEUTRAL |
| 35 | 1 | ERRMSG | Display | 78 | Error messages | ASKIP, BRT, RED |
| 36 | 1 | FKEYS | Display | 21 | "ENTER=Process F3=Exit" | ASKIP, YELLOW |
| 36 | 23 | FKEY05 | Display | 7 | "F5=Save" | ASKIP, YELLOW (conditional) |
| 36 | 31 | FKEY12 | Display | 10 | "F12=Cancel" | ASKIP, YELLOW (conditional) |

---

## 3. Program Structure

### 3.1 Program Identification
- **Program-ID:** COACTUPC
- **Program Name:** Account Update Program
- **Transaction ID:** CAUP
- **Mapset Name:** COACTUP
- **Map Name:** CACTUPA
- **Purpose:** Accept and process credit card account updates affecting both Account Master (ACCTDAT) and Customer Master (CUSTDAT) files

### 3.2 Copybooks Used

**Screen/Map Copybooks:**
- `COACTUP` - BMS-generated map copybook containing screen field definitions

**Common Copybooks:**
- `CSUTLDWY` - Date utility working storage with date validation fields
- `CVCRD01Y` - Card record working storage
- `CSLKPCDY` - Lookup code working storage
- `DFHBMSCA` - CICS BMS attribute definitions
- `DFHAID` - CICS attention identifier definitions
- `COTTL01Y` - Title storage

**Communication Area Copybooks:**
- `COCOM01Y` - Common communication area structure (CARDDEMO-COMMAREA)

**Date and Message Copybooks:**
- `CSDAT01Y` - Date working storage
- `CSMSG01Y` - Message working storage (message IDs and texts)
- `CSMSG02Y` - Additional message working storage

**User and Account Copybooks:**
- `CSUSR01Y` - User information working storage
- `CVACT01Y` - Account record structure (ACCTDAT file layout)
- `CVACT03Y` - Card cross-reference record structure (CARDDAT file layout)
- `CVCUS01Y` - Customer record structure (CUSTDAT file layout)

**Screen Attribute Copybook:**
- `CSSETATY` - Screen attribute setting utility (used with COPY REPLACING for field validation)

**Common Procedures:**
- `CSSTRPFY` - Store PF key common procedure
- `CSUTLDPY` - Date utility procedures (date validation routines)

### 3.3 Files and Datasets

The program accesses the following VSAM files:

1. **ACCTDAT** - Account Master File
   - Access: Direct (READ, READ UPDATE, REWRITE)
   - Key: Account ID (11 bytes)
   - Contains: Account status, balances, limits, dates

2. **CUSTDAT** - Customer Master File
   - Access: Direct (READ, READ UPDATE, REWRITE)
   - Key: Customer ID (10 bytes)
   - Contains: Customer demographics, address, phone, SSN

3. **CARDDAT (via CXACAIX)** - Card Cross-Reference File
   - Access: Direct (READ only via alternate index)
   - Alternate Index: Account ID → Card Number, Customer ID
   - Used to link Account ID to Customer ID

---

## 4. CICS Commands

### 4.1 Transaction Control Commands

**EXEC CICS HANDLE ABEND**
```cobol
EXEC CICS HANDLE ABEND
          LABEL(ABEND-ROUTINE)
END-EXEC
```
- **Purpose:** Establish abend handling routine
- **Location:** Paragraph 0000-MAIN (line 862)
- **When:** At program initialization

**EXEC CICS RETURN**
```cobol
EXEC CICS RETURN
     TRANSID (LIT-THISTRANID)
     COMMAREA (WS-COMMAREA)
     LENGTH(LENGTH OF WS-COMMAREA)
END-EXEC
```
- **Purpose:** Return control to CICS with pseudo-conversational restart
- **Location:** Paragraph COMMON-RETURN (line 1015)
- **Parameters:**
  - TRANSID: 'CAUP' - causes this transaction to restart on next terminal input
  - COMMAREA: 2000-byte communication area preserving program state
  - LENGTH: Dynamic length of COMMAREA

**EXEC CICS XCTL**
```cobol
EXEC CICS XCTL
     PROGRAM (CDEMO-TO-PROGRAM)
     COMMAREA(CARDDEMO-COMMAREA)
END-EXEC
```
- **Purpose:** Transfer control to another program (exit scenario)
- **Location:** Paragraph 0000-MAIN when PFK03 pressed (line 956)
- **Target Programs:** COMEN01C (main menu) or calling program stored in CDEMO-FROM-PROGRAM

**EXEC CICS SYNCPOINT**
```cobol
EXEC CICS
     SYNCPOINT
END-EXEC
```
- **Purpose:** Commit database changes before program transfer
- **Location:** Before XCTL when exiting (line 952)

**EXEC CICS SYNCPOINT ROLLBACK**
```cobol
EXEC CICS
     SYNCPOINT ROLLBACK
END-EXEC
```
- **Purpose:** Rollback changes when customer update fails after account update succeeded
- **Location:** Paragraph 9600-WRITE-PROCESSING (line 4099)

### 4.2 Screen I/O Commands

**EXEC CICS RECEIVE MAP**
```cobol
EXEC CICS RECEIVE MAP(LIT-THISMAP)
          MAPSET(LIT-THISMAPSET)
          INTO(CACTUPAI)
          RESP(WS-RESP-CD)
          RESP2(WS-REAS-CD)
END-EXEC
```
- **Purpose:** Receive user input from terminal screen
- **Location:** Paragraph 1100-RECEIVE-MAP (line 1040)
- **Parameters:**
  - MAP: 'CACTUPA'
  - MAPSET: 'COACTUP'
  - INTO: Input area of BMS map
  - RESP/RESP2: Response codes for error handling

**EXEC CICS SEND MAP**
```cobol
EXEC CICS SEND MAP(CCARD-NEXT-MAP)
               MAPSET(CCARD-NEXT-MAPSET)
               FROM(CACTUPAO)
               CURSOR
               DATAONLY
               RESP(WS-RESP-CD)
END-EXEC
```
- **Purpose:** Send screen output to terminal
- **Location:** Paragraph 3400-SEND-SCREEN (line 3594)
- **Parameters:**
  - MAP: 'CACTUPA'
  - MAPSET: 'COACTUP'
  - FROM: Output area of BMS map
  - CURSOR: Position cursor at field with error or -1 value
  - DATAONLY: Send data only, not mapset definition

### 4.3 File I/O Commands

**EXEC CICS READ (Card Cross-Reference)**
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
- **Purpose:** Read card cross-reference via alternate index to get Customer ID
- **Location:** Paragraph 9200-GETCARDXREF-BYACCT (line 3654)
- **Access Method:** Keyed direct via alternate index CXACAIX
- **Key:** Account ID (11 bytes)

**EXEC CICS READ (Account Master)**
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
- **Purpose:** Read account master record for display
- **Location:** Paragraph 9300-GETACCTDATA-BYACCT (line 3703)
- **Access Method:** Keyed direct
- **Key:** Account ID (11 bytes)

**EXEC CICS READ (Customer Master)**
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
- **Purpose:** Read customer master record for display
- **Location:** Paragraph 9400-GETCUSTDATA-BYCUST (line 3753)
- **Access Method:** Keyed direct
- **Key:** Customer ID (10 bytes)

**EXEC CICS READ UPDATE (Account Master)**
```cobol
EXEC CICS READ
     FILE      (LIT-ACCTFILENAME)
     UPDATE
     RIDFLD    (WS-CARD-RID-ACCT-ID-X)
     KEYLENGTH (LENGTH OF WS-CARD-RID-ACCT-ID-X)
     INTO      (ACCOUNT-RECORD)
     LENGTH    (LENGTH OF ACCOUNT-RECORD)
     RESP      (WS-RESP-CD)
     RESP2     (WS-REAS-CD)
END-EXEC
```
- **Purpose:** Read account record with exclusive lock for update
- **Location:** Paragraph 9600-WRITE-PROCESSING (line 3894)
- **Access Method:** Keyed direct with UPDATE option
- **Locking:** Exclusive lock held until REWRITE or SYNCPOINT

**EXEC CICS READ UPDATE (Customer Master)**
```cobol
EXEC CICS READ
     FILE      (LIT-CUSTFILENAME)
     UPDATE
     RIDFLD    (WS-CARD-RID-CUST-ID-X)
     KEYLENGTH (LENGTH OF WS-CARD-RID-CUST-ID-X)
     INTO      (CUSTOMER-RECORD)
     LENGTH    (LENGTH OF CUSTOMER-RECORD)
     RESP      (WS-RESP-CD)
     RESP2     (WS-REAS-CD)
END-EXEC
```
- **Purpose:** Read customer record with exclusive lock for update
- **Location:** Paragraph 9600-WRITE-PROCESSING (line 3921)
- **Access Method:** Keyed direct with UPDATE option

**EXEC CICS REWRITE (Account Master)**
```cobol
EXEC CICS
     REWRITE FILE(LIT-ACCTFILENAME)
             FROM(ACCT-UPDATE-RECORD)
             LENGTH(LENGTH OF ACCT-UPDATE-RECORD)
             RESP      (WS-RESP-CD)
             RESP2     (WS-REAS-CD)
END-EXEC
```
- **Purpose:** Update account master record
- **Location:** Paragraph 9600-WRITE-PROCESSING (line 4065)
- **Prerequisite:** Must have previously locked record with READ UPDATE

**EXEC CICS REWRITE (Customer Master)**
```cobol
EXEC CICS
     REWRITE FILE(LIT-CUSTFILENAME)
             FROM(CUST-UPDATE-RECORD)
             LENGTH(LENGTH OF CUST-UPDATE-RECORD)
             RESP      (WS-RESP-CD)
             RESP2     (WS-REAS-CD)
END-EXEC
```
- **Purpose:** Update customer master record
- **Location:** Paragraph 9600-WRITE-PROCESSING (line 4085)
- **Prerequisite:** Must have previously locked record with READ UPDATE

### 4.4 Error and Exception Handling Commands

**EXEC CICS SEND (Abend Data)**
```cobol
EXEC CICS SEND
              FROM (ABEND-DATA)
              LENGTH(LENGTH OF ABEND-DATA)
              NOHANDLE
              ERASE
END-EXEC
```
- **Purpose:** Send abend information to terminal before abnormal termination
- **Location:** Paragraph ABEND-ROUTINE (line 4211)

**EXEC CICS HANDLE ABEND CANCEL**
```cobol
EXEC CICS HANDLE ABEND
     CANCEL
END-EXEC
```
- **Purpose:** Cancel abend handler before issuing ABEND command
- **Location:** Paragraph ABEND-ROUTINE (line 4218)

**EXEC CICS ABEND**
```cobol
EXEC CICS ABEND
     ABCODE('9999')
END-EXEC
```
- **Purpose:** Abnormally terminate transaction with code 9999
- **Location:** Paragraph ABEND-ROUTINE (line 4222)

---

## 5. Navigational Details

### 5.1 Function Key Behaviors

**ENTER Key**
- **First Entry (no account number):** Display blank screen prompting for account number
- **Account Number Entered:** Fetch and display account and customer data
- **Changes Made:** Validate all input fields, highlight errors in red, position cursor at first error
- **Validation Passed:** Enable F5 (Save) and F12 (Cancel) keys, display confirmation message

**F3 (PFK03) - Exit**
- **Purpose:** Exit the Account Update transaction
- **Behavior:**
  - Issues `EXEC CICS SYNCPOINT` to commit any pending changes
  - Transfers control via `EXEC CICS XCTL` to:
    - Calling program (stored in CDEMO-FROM-PROGRAM) if available
    - Main menu (COMEN01C) if no calling program
  - Passes COMMAREA back to target program
- **Location:** Lines 927-959
- **Availability:** Always active

**F5 (PFK05) - Save**
- **Purpose:** Confirm and save changes to account and customer records
- **Behavior:**
  - Only active when validation passes (ACUP-CHANGES-OK-NOT-CONFIRMED state)
  - Locks both account and customer records with READ UPDATE
  - Performs optimistic locking check (9700-CHECK-CHANGE-IN-REC)
  - Updates both ACCTDAT and CUSTDAT files via REWRITE
  - If customer update fails, issues SYNCPOINT ROLLBACK
  - On success, displays "Account Updated Successfully" message
  - Resets to initial state for next account update
- **Location:** Lines 2602-2615
- **Availability:** Displayed/enabled only after successful validation
- **Error Conditions:**
  - "Could not lock account for update" - another user has record locked
  - "Data was changed by another user" - concurrent update detected
  - "Update failed" - REWRITE operation failed

**F12 (PFK12) - Cancel**
- **Purpose:** Cancel changes and return to account entry
- **Behavior:**
  - Only active when account details have been fetched
  - Abandons any changes made by user
  - Resets to initial state with blank screen
  - Clears account number and prompts for new entry
- **Location:** Line 910
- **Availability:** Displayed/enabled only after account data is fetched

**Invalid Keys**
- Any other PF key press is treated as ENTER
- Location: Lines 914-916
- This prevents user errors from causing unexpected behavior

### 5.2 Screen Flow Sequence

```
┌─────────────────────────────────────────────────────────────┐
│                    INITIAL ENTRY                             │
│  - User invokes transaction CAUP                            │
│  - Program displays blank screen                            │
│  - Prompt: "Account Number: ___________"                    │
│  - Available keys: ENTER, F3                                │
│  - State: ACUP-DETAILS-NOT-FETCHED                          │
└──────────────────┬──────────────────────────────────────────┘
                   │
          ┌────────▼────────┐
          │  User enters    │
          │  Account Number │
          │  Presses ENTER  │
          └────────┬────────┘
                   │
     ┌─────────────▼──────────────┐
     │  FETCH ACCOUNT DATA         │
     │  - 9200-GETCARDXREF-BYACCT │
     │  - 9300-GETACCTDATA-BYACCT │
     │  - 9400-GETCUSTDATA-BYCUST │
     │  - 9500-STORE-FETCHED-DATA │
     └─────────────┬──────────────┘
                   │
        ┌──────────▼───────────┐
        │  Account Found?     │
        └──────────┬───────────┘
                   │
         ┌─────────┴──────────┐
         │                    │
      YES│                    │NO
         │                    │
         ▼                    ▼
   ┌─────────────┐    ┌──────────────┐
   │ DISPLAY     │    │ Display Error│
   │ ACCOUNT     │    │ "Account not │
   │ DATA        │    │  found"      │
   │ - All fields│    │ Return to    │
   │   populated │    │ entry screen │
   │ - F3, F12   │    └──────────────┘
   │   enabled   │
   │ - State:    │
   │   ACUP-SHOW │
   │   -DETAILS  │
   └──────┬──────┘
          │
   ┌──────▼──────┐
   │ User modifies│
   │ field values │
   │ Presses ENTER│
   └──────┬──────┘
          │
   ┌──────▼──────────┐
   │ VALIDATE INPUT  │
   │ - 1200-EDIT-MAP │
   │   -INPUTS       │
   │ - All field     │
   │   validations   │
   └──────┬──────────┘
          │
   ┌──────▼──────────┐
   │ Check for       │
   │ changes         │
   │ - 1205-COMPARE  │
   │   -OLD-NEW      │
   └──────┬──────────┘
          │
   ┌──────┴──────────┐
   │                 │
   │ Changes?        │
   └──────┬──────────┘
          │
    ┌─────┴──────┐
    │            │
  YES│            │NO
    │            │
    ▼            ▼
┌────────┐   ┌──────────────┐
│Errors? │   │ Display      │
└────┬───┘   │ "No changes  │
     │       │  detected"   │
  ┌──┴───┐   └──────────────┘
  │      │
YES│      │NO
  │      │
  ▼      ▼
┌────┐ ┌─────────────────┐
│RED │ │ Display fields  │
│high│ │ Enable F5, F12  │
│ligt│ │ Message: "Press │
│errr│ │ F5 to save or   │
│    │ │ F12 to cancel"  │
│Ret │ │ State: ACUP-    │
│urn │ │ CHANGES-OK-NOT- │
│    │ │ CONFIRMED       │
└────┘ └────────┬────────┘
               │
        ┌──────▼──────┐
        │ User Action │
        └──────┬──────┘
               │
    ┌──────────┼──────────┐
    │          │          │
    ▼          ▼          ▼
┌───────┐  ┌─────┐   ┌──────┐
│  F5   │  │ F12 │   │  F3  │
│ SAVE  │  │CNCL │   │ EXIT │
└───┬───┘  └──┬──┘   └───┬──┘
    │         │          │
    │         │          └──────┐
    │         │                 │
    ▼         ▼                 ▼
┌────────┐ ┌─────┐        ┌─────────┐
│UPDATE  │ │Reset│        │SYNCPOINT│
│PROCESS │ │Clear│        │XCTL to  │
│9600-   │ │Start│        │calling  │
│WRITE-  │ │Over │        │program  │
│PROC    │ └─────┘        └─────────┘
└───┬────┘
    │
    ▼
┌─────────────────┐
│ Lock Records    │
│ - READ UPDATE   │
│   ACCTDAT       │
│ - READ UPDATE   │
│   CUSTDAT       │
└────────┬────────┘
         │
    ┌────▼─────┐
    │Lock OK?  │
    └────┬─────┘
         │
    ┌────┴────┐
    │         │
  YES│         │NO
    │         │
    ▼         ▼
┌────────┐ ┌──────────┐
│Concur  │ │Display   │
│rent    │ │"Could not│
│update  │ │lock"     │
│check   │ │error     │
│9700-   │ └──────────┘
│CHECK   │
└───┬────┘
    │
    ▼
┌──────────┐
│Data      │
│changed?  │
└────┬─────┘
     │
  ┌──┴───┐
  │      │
YES│      │NO
  │      │
  ▼      ▼
┌────┐ ┌─────────┐
│Show│ │REWRITE  │
│old │ │ACCTDAT  │
│data│ └────┬────┘
│Ret │      │
│urn │      ▼
└────┘ ┌─────────┐
       │Success? │
       └────┬────┘
            │
         ┌──┴───┐
         │      │
       YES│      │NO
         │      │
         ▼      ▼
    ┌────────┐ ┌──────┐
    │REWRITE │ │Error │
    │CUSTDAT │ │Return│
    └───┬────┘ └──────┘
        │
        ▼
    ┌────────┐
    │Success?│
    └───┬────┘
        │
     ┌──┴───┐
     │      │
   YES│      │NO
     │      │
     ▼      ▼
┌─────────┐ ┌──────────┐
│Display  │ │SYNCPOINT │
│"Account │ │ROLLBACK  │
│Updated  │ │Display   │
│Success" │ │error     │
│Clear    │ └──────────┘
│State    │
│Start    │
│Over     │
└─────────┘
```

### 5.3 Transaction Entry Points

**Direct Transaction Invocation:**
- User enters: `CAUP` at CICS terminal
- Program starts in initial state (CDEMO-PGM-ENTER)
- EIBCALEN = 0 (no COMMAREA passed)
- Displays blank screen for account entry

**Program Transfer (XCTL):**
- Called from another program (e.g., account list, main menu)
- COMMAREA contains context:
  - CDEMO-FROM-PROGRAM: Calling program name
  - CDEMO-FROM-TRANID: Calling transaction ID
  - CDEMO-ACCT-ID: Pre-filled account number (optional)
- If account number provided, auto-fetches data
- If no account number, displays entry screen

**Pseudo-Conversational Return:**
- Program returns after each user interaction
- COMMAREA preserves state between interactions
- WS-THIS-PROGCOMMAREA preserves:
  - Fetched account data (ACUP-OLD-DETAILS)
  - Current state flags (ACUP-DETAILS-NOT-FETCHED, etc.)
  - Validation flags
- Transaction automatically restarts on next terminal input

### 5.4 Exit Points

**Normal Exit via F3:**
- Target: CDEMO-TO-PROGRAM (from COMMAREA)
- Default: COMEN01C (main menu)
- Method: EXEC CICS XCTL
- Changes: Committed via SYNCPOINT before XCTL

**Completion Exit:**
- After successful update
- Returns to initial state for next update
- Same program continues running
- No XCTL, just RETURN with same TRANSID

**Abnormal Exit:**
- ABEND-ROUTINE paragraph
- Sends error message to terminal
- Abends with code '9999'
- Transaction terminates

---

## 6. Business Logic and Program Execution Flow

### 6.1 Program Initialization

When the COACTUPC program begins execution (paragraph **0000-MAIN**, line 859), it first establishes an abend handling routine using `EXEC CICS HANDLE ABEND LABEL(ABEND-ROUTINE)` to trap unexpected errors. The program then initializes its working storage areas (`CC-WORK-AREA`, `WS-MISC-STORAGE`, `WS-COMMAREA`), stores its transaction context (transaction ID 'CAUP' in `WS-TRANID`, program name in `WS-PROGRAM`), and clears any error message flags.

### 6.2 COMMAREA Processing and State Management

The program employs a pseudo-conversational design pattern. On each invocation, it examines the COMMAREA to determine its current state (lines 880-893).

**Case 1: Initial Entry** - When `EIBCALEN = 0` or fresh entry from menu program (`CDEMO-FROM-PROGRAM = LIT-MENUPGM` with `NOT CDEMO-PGM-REENTER`), the program initializes both `CARDDEMO-COMMAREA` and `WS-THIS-PROGCOMMAREA`, sets state to `CDEMO-PGM-ENTER` and `ACUP-DETAILS-NOT-FETCHED`, indicating no data has been fetched yet.

**Case 2: Re-entry** - When the user pressed ENTER, F5, F12, or any other key, the program restores the previous state by moving the first portion of `DFHCOMMAREA` to `CARDDEMO-COMMAREA` and the second portion to `WS-THIS-PROGCOMMAREA`. This preserves all previously fetched data, validation flags, and state indicators across pseudo-conversational interactions.

### 6.3 PF Key Mapping and Validation

After restoring state, the program performs PF key validation through the `YYYY-STORE-PFKEY` procedure (line 898), a common routine from copybook `CSSTRPFY` that maps the attention identifier (AID) to standardized PF key flags.

The program then validates which PF keys are acceptable in the current state (lines 905-916). It initializes the key as invalid, then checks if the pressed key is one of the allowed keys: ENTER (always valid), F3 (always valid), F5 (valid only when `ACUP-CHANGES-OK-NOT-CONFIRMED`), or F12 (valid only when `NOT ACUP-DETAILS-NOT-FETCHED`). If the key is valid, the valid flag is set; otherwise any invalid key press is treated as ENTER to prevent user confusion.

### 6.4 Main Processing Logic

The program uses an `EVALUATE TRUE` structure (lines 921-1004) to route processing based on the current state and PF key pressed.

#### 6.4.1 F3 Exit Processing

When the user presses F3 to exit (lines 927-959), the program performs a clean exit. It first checks if a calling program exists by examining `CDEMO-FROM-TRANID` and `CDEMO-FROM-PROGRAM`. If empty or spaces, it defaults to the main menu program (`LIT-MENUPGM` = 'COMEN01C') and menu transaction (`LIT-MENUTRANID` = 'CMEN'). Otherwise, it returns to the calling program using the stored program and transaction IDs.

The program then updates the navigation context by moving the current program and transaction IDs to the from-fields, setting user type and entry flags, and recording the last mapset and map. It commits any pending changes via `EXEC CICS SYNCPOINT`, then transfers control using:

```cobol
EXEC CICS XCTL
     PROGRAM (CDEMO-TO-PROGRAM)
     COMMAREA(CARDDEMO-COMMAREA)
END-EXEC
```

This preserves navigation context in `CARDDEMO-COMMAREA`, allowing the target program to know where the user came from.

#### 6.4.2 Initial Display and Reset Scenarios

When displaying the initial entry screen or resetting after completion (lines 964-989), the program branches based on state. For initial entry (`ACUP-DETAILS-NOT-FETCHED` with `CDEMO-PGM-ENTER`) or fresh entry from menu (`CDEMO-FROM-PROGRAM = LIT-MENUPGM` with `NOT CDEMO-PGM-REENTER`), it initializes the program-specific area, sends a blank map via `3000-SEND-MAP`, sets the re-entry flag, and returns to CICS.

For terminal states (`ACUP-CHANGES-OKAYED-AND-DONE` or `ACUP-CHANGES-FAILED`), the program initializes all working storage areas and the account ID, sets entry state, sends a blank map, sets re-entry flag, and returns. These scenarios display a blank screen prompting for a new account number.

#### 6.4.3 Normal Processing Flow

For all other cases (lines 996-1003), the program follows the standard processing path by performing `1000-PROCESS-INPUTS` to receive and validate input, then `2000-DECIDE-ACTION` to determine what action to take, then `3000-SEND-MAP` to display the screen, and finally proceeding to `COMMON-RETURN`. This is the main processing path involving input reception, validation, decision logic, and screen display.

### 6.5 Input Processing

The `1000-PROCESS-INPUTS` section (lines 1025-1037) coordinates receiving and validating input. It performs `1100-RECEIVE-MAP` to retrieve screen data, then `1200-EDIT-MAP-INPUTS` to validate all inputs. After validation, it prepares navigation context by moving the return message to `CCARD-ERROR-MSG` and setting the next program, mapset, and map identifiers all to reference this program, ensuring the user stays on this screen.

#### 6.5.1 Receive Map

The `1100-RECEIVE-MAP` paragraph (lines 1039-1426) receives screen data using:

```cobol
EXEC CICS RECEIVE MAP(LIT-THISMAP)
          MAPSET(LIT-THISMAPSET)
          INTO(CACTUPAI)
          RESP(WS-RESP-CD)
          RESP2(WS-REAS-CD)
END-EXEC
```

The routine then systematically moves each field from the input map (`CACTUPAI`) to working storage. For each field, it checks if the input is an asterisk or spaces, indicating no change. If so, it moves LOW-VALUES to the corresponding working storage field. Otherwise, it moves the actual input value.

For the account number field (lines 1051-1062), the program checks for asterisk or spaces and moves the value to both `CC-ACCT-ID` and `ACUP-NEW-ACCT-ID-X`. If details haven't been fetched yet (`ACUP-DETAILS-NOT-FETCHED`), it exits immediately since only the account number is needed for initial search.

For numeric fields like credit limit (lines 1073-1084), the program performs additional validation. It moves the input to the display format field (`ACUP-NEW-CREDIT-LIMIT-X`), then uses `FUNCTION TEST-NUMVAL-C` to verify the input is numeric. If valid, it converts the value using `FUNCTION NUMVAL-C` and stores it in the numeric format field (`ACUP-NEW-CREDIT-LIMIT-N`).

For date fields (lines 1144-1163), the program splits the input into separate components: year (4 digits), month (2 digits), and day (2 digits), storing each component in its own working storage field (`ACUP-NEW-OPEN-YEAR`, `ACUP-NEW-OPEN-MON`, `ACUP-NEW-OPEN-DAY`) for independent validation later.

The process repeats for all screen fields including account status, expiration date, reissue date, current balance, cash credit limit, current cycle credit/debit, group ID, customer name components, three address lines, state code, country code, zip code, two phone numbers, SSN components, government ID, date of birth components, FICO score, and EFT account ID. The routine systematically captures all user inputs while handling blank/unchanged fields appropriately.

#### 6.5.2 Edit Map Inputs

The `1200-EDIT-MAP-INPUTS` paragraph (lines 1429-1678) performs comprehensive validation. It first initializes validation status by setting `INPUT-OK` to TRUE.

**If Account Not Yet Fetched** (lines 1433-1446) - When `ACUP-DETAILS-NOT-FETCHED` is true, the program only validates the account number by performing `1210-EDIT-ACCOUNT`, which checks that the account number is 11 digits, numeric, and non-zero (lines 1783-1822). If the account number is blank (`FLG-ACCTFILTER-BLANK`), it sets the `NO-SEARCH-CRITERIA-RECEIVED` flag. The routine then initializes `ACUP-OLD-ACCT-DATA` to LOW-VALUES and exits early since no other fields are relevant yet.

**If Account Already Fetched** (lines 1451-1460) - The program sets various found flags (`FOUND-ACCOUNT-DATA`, `FOUND-ACCT-IN-MASTER`, `FLG-ACCTFILTER-ISVALID`, `FOUND-CUST-IN-MASTER`, `FLG-CUSTFILTER-ISVALID`), then performs `1205-COMPARE-OLD-NEW` to compare every field between old and new values.

The `1205-COMPARE-OLD-NEW` paragraph (lines 1681-1778) performs a field-by-field comparison between `ACUP-OLD-*` and `ACUP-NEW-*` values. For each field, it checks if the values differ. If any field has changed, it sets the `CHANGES-FOUND` flag and updates a corresponding field-level change flag. This comparison covers all account fields (status, balance, credit limits, dates, cycle amounts, group ID) and all customer fields (name components, address lines, state, country, zip, phone numbers, SSN, government ID, date of birth, FICO score, EFT account).

If no changes are found, or if changes were already validated or completed (lines 1462-1465), the program clears non-key flags and exits validation early.

If changes are detected, the program validates each modified field by calling specialized validation routines. For example, it moves the variable name and value to working storage fields, then performs the appropriate validation routine like `1220-EDIT-YESNO` for account status, `EDIT-DATE-CCYYMMDD` for dates, `1250-EDIT-SIGNED-9V2` for currency fields, `1225-EDIT-ALPHA-REQD` for customer names, `1230-EDIT-ALPHA-OR-SPACE` for address fields, `1260-EDIT-US-PHONE-NUM` for phone numbers, `1265-EDIT-US-SSN` for SSN, `1270-EDIT-US-STATE-CD` for state code, `1280-EDIT-US-STATE-ZIP-CD` for zip/state combo, and `1245-EDIT-NUM-REQD` for FICO score.

Each validation routine stores its result in corresponding working storage flags. If any validation fails, the `INPUT-ERROR` flag is set.

At the end of validation (lines 1671-1675), if all validations passed (`INPUT-OK`), the program sets `ACUP-CHANGES-OK-NOT-CONFIRMED` to enable the F5 save button and prompt the user to confirm the save operation.

### 6.6 Decision Logic

The `2000-DECIDE-ACTION` paragraph (lines 2549-2645) determines actions based on the current state.

**First Search - Fetch Account Data** (lines 2552-2570) - When `ACUP-DETAILS-NOT-FETCHED` is true and the account filter is valid (`FLG-ACCTFILTER-ISVALID`), the program performs `9000-READ-ACCT` to read data from the three files (card cross-reference, account master, customer master). If the account is found (`FOUND-ACCOUNT-DATA`), it sets the state to `ACUP-SHOW-DETAILS` to display the fetched data on screen.

**Changes Made and Valid** (lines 2579-2583) - When `ACUP-CHANGES-MADE` is true and all inputs are valid (`INPUT-OK`), the program sets state to `ACUP-CHANGES-OK-NOT-CONFIRMED` and sets the `PROMPT-FOR-CONFIRM-SAVE` flag to display a message asking the user to press F5 to save or F12 to cancel.

**User Confirms Save (F5)** (lines 2602-2615) - When in the confirm state (`ACUP-CHANGES-OK-NOT-CONFIRMED`) and F5 is pressed (`CCARD-AID-PFK05`), the program performs `9600-WRITE-PROCESSING` to update the database. Based on the results, it evaluates four possible outcomes: if could not lock account, sets state to `ACUP-CHANGES-OKAYED-LOCK-ERROR`; if locked but update failed, sets state to `ACUP-CHANGES-OKAYED-BUT-FAILED`; if data was changed before update, another user modified the data, so sets state back to `ACUP-SHOW-DETAILS` to display current values; otherwise update succeeded, sets state to `ACUP-CHANGES-OKAYED-AND-DONE`.

**Unexpected State** (lines 2633-2640) - Any unhandled scenario triggers an abend by setting `ABEND-CULPRIT` to the program name, `ABEND-CODE` to '0001', and `ABEND-MSG` to 'UNEXPECTED DATA SCENARIO', then performing the `ABEND-ROUTINE`.

### 6.7 Account Data Fetch Process

The `9000-READ-ACCT` paragraph (lines 3608-3648) orchestrates reading from three files. It begins by initializing the old details area (`ACUP-OLD-DETAILS`), setting the no-info-message flag, and moving the account ID to working storage fields (`ACUP-OLD-ACCT-ID`, `WS-CARD-RID-ACCT-ID`). It then performs three sequential read operations.

**Step 1: Read Card Cross-Reference** (lines 3654-3662) - The program performs `9200-GETCARDXREF-BYACCT` which executes:

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

This accesses the alternate index CXACAIX to get Customer ID from Account ID. The response code is checked; if not found, the program sets error flags and exits. If successful, it proceeds to read the account master.

**Step 2: Read Account Master** (lines 3703-3711) - The program performs `9300-GETACCTDATA-BYACCT` which executes:

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

This reads the account master file (ACCTDAT) to retrieve account financial data including balances, credit limits, and dates. If not found, it sets error flags and exits. If successful, it continues to read the customer master.

**Step 3: Read Customer Master** (lines 3753-3761) - The program first moves the customer ID from the previous read (`CDEMO-CUST-ID`) to the read key field (`WS-CARD-RID-CUST-ID`), then performs `9400-GETCUSTDATA-BYCUST` which executes:

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

This reads the customer master file (CUSTDAT) to retrieve customer demographics including name, address, phone numbers, SSN, date of birth, government ID, FICO score, and EFT account ID. If not found, it sets error flags and exits.

**Step 4: Store Fetched Data** (lines 3801-3886) - The `9500-STORE-FETCHED-DATA` paragraph moves all fields from the three file records into the `ACUP-OLD-*` working storage fields. This preserves the original values for later comparison during updates. Account fields are moved to `ACUP-OLD-ACCT-DATA` and customer fields to `ACUP-OLD-CUST-DATA`. For date fields, the program splits the YYYY-MM-DD format into separate year, month, and day components. For phone numbers, it parses the (999)999-9999 format into separate area code, prefix, and line number components.

### 6.8 Update Processing

The `9600-WRITE-PROCESSING` paragraph (lines 3888-4107) implements the database update with proper locking and transaction integrity.

**Locking Phase** - The program first attempts to lock both records for exclusive update. For the account record (lines 3894-3903):

```cobol
EXEC CICS READ
     FILE      (LIT-ACCTFILENAME)
     UPDATE
     RIDFLD    (WS-CARD-RID-ACCT-ID-X)
     KEYLENGTH (LENGTH OF WS-CARD-RID-ACCT-ID-X)
     INTO      (ACCOUNT-RECORD)
     LENGTH    (LENGTH OF ACCOUNT-RECORD)
     RESP      (WS-RESP-CD)
     RESP2     (WS-REAS-CD)
END-EXEC
```

If the response is not normal, it sets the could-not-lock flag (`COULD-NOT-LOCK-ACCT-FOR-UPDATE`), displays an error message, and exits without updating.

For the customer record (lines 3921-3930):

```cobol
EXEC CICS READ
     FILE      (LIT-CUSTFILENAME)
     UPDATE
     RIDFLD    (WS-CARD-RID-CUST-ID-X)
     KEYLENGTH (LENGTH OF WS-CARD-RID-CUST-ID-X)
     INTO      (CUSTOMER-RECORD)
     LENGTH    (LENGTH OF CUSTOMER-RECORD)
     RESP      (WS-RESP-CD)
     RESP2     (WS-REAS-CD)
END-EXEC
```

If locking fails, it sets the lock error flag and exits.

**Optimistic Concurrency Check** (lines 3947-3952) - The program performs `9700-CHECK-CHANGE-IN-REC` which compares every field in the locked records against the `ACUP-OLD-*` values that were originally fetched. This implements optimistic locking to detect concurrent updates.

The `9700-CHECK-CHANGE-IN-REC` paragraph (lines 4109-4195) performs field-by-field comparison. For each field, it checks if the value in the locked record differs from the original value. If any field has changed, another user modified the data concurrently, so it sets the `DATA-WAS-CHANGED-BEFORE-UPDATE` flag, displays an error message, and exits. If no concurrent changes are detected, the update can proceed safely.

**Prepare Update Records** (lines 3954-4061) - The program builds the update records by moving values from `ACUP-NEW-*` working storage to the update record structures (`ACCT-UPDATE-RECORD` and `CUST-UPDATE-RECORD`). For date fields, it reassembles the components using STRING statements. For example, the open date is assembled by stringing together the year, a dash, the month, another dash, and the day into the target date field. Phone numbers are similarly reassembled by stringing together the area code in parentheses, prefix, dash, and line number into the (999)999-9999 format.

**Update Account Record** (lines 4065-4071) - The program executes:

```cobol
EXEC CICS
     REWRITE FILE(LIT-ACCTFILENAME)
             FROM(ACCT-UPDATE-RECORD)
             LENGTH(LENGTH OF ACCT-UPDATE-RECORD)
             RESP      (WS-RESP-CD)
             RESP2     (WS-REAS-CD)
END-EXEC
```

If the response is not normal, it sets the update failed flag (`LOCKED-BUT-UPDATE-FAILED`) and exits.

**Update Customer Record with Rollback** (lines 4085-4103) - The program executes:

```cobol
EXEC CICS
     REWRITE FILE(LIT-CUSTFILENAME)
             FROM(CUST-UPDATE-RECORD)
             LENGTH(LENGTH OF CUST-UPDATE-RECORD)
             RESP      (WS-RESP-CD)
             RESP2     (WS-REAS-CD)
END-EXEC
```

After the REWRITE, the program checks the response code. If successful, both updates succeeded and the transaction commits automatically when the program returns. However, if the customer update fails after the account update already succeeded, this creates a data inconsistency. To handle this, the program issues:

```cobol
EXEC CICS
     SYNCPOINT ROLLBACK
END-EXEC
```

This undoes the account update, ensuring transaction integrity - either both files are updated atomically or neither is updated. The program sets the update failed flag and exits.

### 6.9 Program Return

Every processing path ends at `COMMON-RETURN` (lines 1007-1020). The program first moves the return message to the error message field in the communication area (`CCARD-ERROR-MSG`). It then packs both communication areas into a single structure: the shared area (`CARDDEMO-COMMAREA`) goes into the first portion of `WS-COMMAREA`, and the program-specific area (`WS-THIS-PROGCOMMAREA`) goes into the second portion.

Finally, it executes:

```cobol
EXEC CICS RETURN
     TRANSID (LIT-THISTRANID)
     COMMAREA (WS-COMMAREA)
     LENGTH(LENGTH OF WS-COMMAREA)
END-EXEC
```

This returns control to CICS with transaction ID 'CAUP', passing the packed COMMAREA (2000 bytes total) to preserve state across pseudo-conversational interactions. The program releases all resources and waits for the next terminal input. When the user presses a key, CICS automatically restarts the program with the same transaction ID, restores the COMMAREA, and processing continues from the beginning with the preserved state intact.

---

## 7. Data Structures and Sources

### 7.1 Screen I/O Structure

The BMS map generates two data structures:

**CACTUPAI - Input Map Structure:**
Contains length indicators and input data for each field. Each field has a corresponding `xxxL` (length) and `xxxI` (input) component.

**CACTUPAO - Output Map Structure:**
Contains output data and attribute bytes for each field. Each field has a corresponding `xxxO` (output) and `xxxC` (attribute) component.

### 7.2 File Record Structures

**ACCOUNT-RECORD (from CVACT01Y copybook):**
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

**CUSTOMER-RECORD (from CVCUS01Y copybook):**
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

**Purpose:** Stores customer demographic and contact information. Phone numbers stored in (999)999-9999 format. Dates in YYYY-MM-DD format.

**CARD-XREF-RECORD (from CVACT03Y copybook):**
```cobol
01  CARD-XREF-RECORD.
    05  XREF-CARD-NUM               PIC X(16).
    05  XREF-CUST-ID                PIC 9(09) COMP.
    05  XREF-ACCT-ID                PIC X(11).
    05  FILLER                      PIC X(219).
```

**Purpose:** Cross-reference file linking card numbers to accounts and customers. Alternate index CXACAIX allows lookup by XREF-ACCT-ID to get XREF-CUST-ID.

### 7.3 COMMAREA Structures

**CARDDEMO-COMMAREA (from COCOM01Y copybook):**
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

**WS-THIS-PROGCOMMAREA (Program-Specific State):**
```cobol
01  WS-THIS-PROGCOMMAREA.
    05  ACUP-OLD-DETAILS.
        10  ACUP-OLD-ACCT-DATA.
            15  ACUP-OLD-ACCT-ID            PIC 9(11).
            15  ACUP-OLD-ACTIVE-STATUS      PIC X(01).
            15  ACUP-OLD-CURR-BAL-N         PIC S9(10)V99.
            15  ACUP-OLD-CREDIT-LIMIT-N     PIC S9(10)V99.
            [... all account fields ...]
        10  ACUP-OLD-CUST-DATA.
            15  ACUP-OLD-CUST-ID            PIC 9(09).
            15  ACUP-OLD-CUST-SSN           PIC 9(09).
            15  ACUP-OLD-CUST-FIRST-NAME    PIC X(25).
            [... all customer fields ...]
    05  ACUP-NEW-DETAILS.
        10  ACUP-NEW-ACCT-DATA.
            [... mirrors old structure ...]
        10  ACUP-NEW-CUST-DATA.
            [... mirrors old structure ...]
    05  WS-NON-KEY-FLAGS.
        10  WS-EDIT-ACCT-STATUS             PIC X(01).
        10  WS-EDIT-OPEN-DATE-FLGS.
            15  FLG-OPEN-YEAR-ISVALID       PIC X(01).
            15  FLG-OPEN-MONTH-ISVALID      PIC X(01).
            15  FLG-OPEN-DAY-ISVALID        PIC X(01).
        [... validation flags for all fields ...]
```

**Purpose:** Preserves original fetched data (ACUP-OLD-*), user modifications (ACUP-NEW-*), and validation results across pseudo-conversational interactions.

### 7.4 State Management Flags

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
```

**Purpose:** 88-level condition names control program flow through pseudo-conversational interactions, tracking whether data has been fetched and the validation/save state.

---

## 8. Dependencies

### 8.1 External Programs

**COMEN01C - Main Menu Program**
- **Literal:** `LIT-MENUPGM` = 'COMEN01C'
- **Transaction:** `LIT-MENUTRANID` = 'CMEN'
- **Relationship:** Default exit target when F3 pressed and no calling program specified
- **Data Passed:** CARDDEMO-COMMAREA with navigation context

**Calling Program (Dynamic)**
- **Source:** `CDEMO-FROM-PROGRAM` in COMMAREA
- **Relationship:** Program that invoked COACTUPC via XCTL
- **Exit Behavior:** When F3 pressed, returns to calling program if specified
- **Data Passed:** Updated CARDDEMO-COMMAREA with account/customer context

### 8.2 Files and Datasets

**ACCTDAT - Account Master File**
- **File Type:** VSAM KSDS (Key Sequenced Data Set)
- **Literal:** `LIT-ACCTFILENAME` = 'ACCTDAT'
- **Key:** ACCT-ID (PIC X(11))
- **Operations:** READ, READ UPDATE, REWRITE
- **Usage:** Stores account balances, limits, dates, and status

**CUSTDAT - Customer Master File**
- **File Type:** VSAM KSDS
- **Literal:** `LIT-CUSTFILENAME` = 'CUSTDAT'
- **Key:** CUST-ID (PIC 9(09) COMP)
- **Operations:** READ, READ UPDATE, REWRITE
- **Usage:** Stores customer demographics, address, phone, SSN

**CARDDAT - Card Cross-Reference File**
- **File Type:** VSAM KSDS with Alternate Index
- **Literal (AIX):** `LIT-CARDXREFNAME-ACCT-PATH` = 'CXACAIX'
- **Primary Key:** XREF-CARD-NUM
- **Alternate Index:** XREF-ACCT-ID (used by this program)
- **Operations:** READ (via AIX)
- **Usage:** Maps Account ID → Customer ID and Card Number

### 8.3 Inter-Program Communication

**COMMAREA Structure**
- **Total Length:** 2000 bytes
- **Component 1:** CARDDEMO-COMMAREA (~500 bytes) - shared across all CardDemo programs
- **Component 2:** WS-THIS-PROGCOMMAREA (~1500 bytes) - program-specific state

**Data Passed TO Calling Program:**
- Account/Customer context (IDs, names, status)
- Navigation context (from/to program/transaction)
- Last screen displayed

**Data Received FROM Calling Program:**
- Calling program/transaction identification
- Optional pre-filled account number
- User session information

### 8.4 Copybook Dependencies

**Critical Copybooks:**
- `COCOM01Y` - CARDDEMO-COMMAREA structure
- `CVACT01Y` - ACCOUNT-RECORD structure
- `CVCUS01Y` - CUSTOMER-RECORD structure
- `CVACT03Y` - CARD-XREF-RECORD structure
- `COACTUP` - BMS-generated map
- `CSUTLDPY` - Date validation routines
- `CSSTRPFY` - PF key processing

**Supporting Copybooks:**
- `DFHBMSCA` - CICS BMS attributes
- `DFHAID` - CICS AID constants
- `CSSETATY` - Screen attribute utility

---

## 9. Error Handling

### 9.1 CICS Response Code Handling

The program checks RESP and RESP2 codes for all CICS commands using pattern:

```cobol
EVALUATE WS-RESP-CD
    WHEN DFHRESP(NORMAL)
       [Success processing]
    WHEN DFHRESP(NOTFND)
       [Record not found handling]
    WHEN OTHER
       [Unexpected error handling]
END-EVALUATE
```

**Response Codes:**
- **DFHRESP(NORMAL)** - Successful operation
- **DFHRESP(NOTFND)** - Record not found in file
- **OTHER** - Unexpected errors (deadlock, I/O error, etc.)

### 9.2 Field Validation Rules

**Yes/No Fields** (1220-EDIT-YESNO):
- Must be 'Y' or 'N'
- Errors: "[Field name] must be supplied", "[Field name] must be Y or N"

**Dates** (EDIT-DATE-CCYYMMDD):
- Year: 4 digits, 1900-2099
- Month: 01-12
- Day: Valid for month (handles leap years)
- DOB: Not in future
- Errors: Year/Month/Day validation messages

**Currency/Numeric** (1250-EDIT-SIGNED-9V2):
- Must pass FUNCTION TEST-NUMVAL-C
- Can be negative, two decimal places
- Errors: "[Field name] must be supplied", "[Field name] is not valid"

**Simple Numeric** (1245-EDIT-NUM-REQD):
- All numeric, not zero
- Errors: Must be supplied, must be numeric, must not be zero
- Special: FICO Score 300-850 range check

**Required Alpha** (1225-EDIT-ALPHA-REQD):
- Only alphabetic + spaces
- Non-alpha characters removed, result must not be blank
- Errors: Must be supplied, can have alphabets only

**SSN** (1265-EDIT-US-SSN):
- Part 1: 3 digits, not 000
- Part 2: 2 digits, not 00
- Part 3: 4 digits, not 0000
- Errors: Part-specific validation messages

**State Code** (1270-EDIT-US-STATE-CD):
- 2 alphabetic characters
- Must be valid US state code
- Errors: Must be supplied, invalid state code

**Phone Numbers** (1260-EDIT-US-PHONE-NUM):
- Optional: Can be completely blank
- If any part provided, all parts required
- Area code: 3 digits, valid North American code
- Prefix: 3 digits, not 000
- Line: 4 digits
- Errors: Part-specific validation messages

**Cross-Field** (1280-EDIT-US-STATE-ZIP-CD):
- Zip code must be valid for state
- Error: "Zip code is not valid for state [XX]"

### 9.3 Database Error Conditions

**Record Not Found:**
- Account in CARDDAT: Return to entry screen
- Account in ACCTDAT: Return to entry screen
- Customer in CUSTDAT: Return to entry screen

**Record Locking Failures:**
- Cannot lock account: "Could not lock account for update"
- Cannot lock customer: "Could not lock customer for update"
- Recovery: Do not proceed, return to display

**Update Failures:**
- Account REWRITE fails: "Update failed"
- Customer REWRITE fails after account success: SYNCPOINT ROLLBACK

**Concurrent Update Detection** (9700-CHECK-CHANGE-IN-REC):
- Compares ACUP-OLD-* vs locked record
- Message: "Data was changed by another user. Please review and retry."
- Recovery: Display current data

### 9.4 Error Message Storage

```cobol
01  WS-RETURN-MSG               PIC X(78).
```

Contains primary error message displayed at bottom of screen with 88-level condition names for standard messages.

```cobol
01  WS-INFO-MSG                 PIC X(45).
```

Contains status/info messages like "Press F5 to save or F12 to cancel" or "Account Updated Successfully".

### 9.5 Screen Error Display

When validation fails:
1. Field attribute set to DFHRED
2. Cursor positioned at first error field (-1 value)
3. Error message displayed in ERRMSG area
4. Field value may show error indicator

### 9.6 ABEND Handling

**ABEND Data Structure:**
```cobol
01  ABEND-DATA.
    05  ABEND-MSG           PIC X(72).
    05  ABEND-CULPRIT       PIC X(8).
    05  ABEND-CODE          PIC X(4).
    05  ABEND-REASON        PIC X(50).
```

**ABEND Scenarios:**
1. **Unexpected Program State** - Code '0001', Message 'UNEXPECTED DATA SCENARIO'
2. **General ABEND** - Code '9999', Default message

**ABEND-ROUTINE** (lines 4203-4228):
```cobol
EXEC CICS SEND
         FROM (ABEND-DATA)
         LENGTH(LENGTH OF ABEND-DATA)
         NOHANDLE
         ERASE
END-EXEC

EXEC CICS HANDLE ABEND
     CANCEL
END-EXEC

EXEC CICS ABEND
     ABCODE('9999')
END-EXEC
```

---

## 10. Additional Technical Details

### 10.1 Transaction Control

**Pseudo-Conversational Design:**
The program uses CICS pseudo-conversational pattern to optimize resource usage. After each screen interaction, the program:
1. Saves state in COMMAREA (2000 bytes)
2. Returns to CICS with TRANSID 'CAUP'
3. Releases all resources
4. Waits for next terminal input
5. Restarts automatically when user presses a key
6. Restores state from COMMAREA

This design prevents tying up system resources while waiting for user input.

**SYNCPOINT Usage:**
- Before XCTL (line 952): Commits any pending changes before transferring control
- After successful updates: Implicit commit when RETURN issued
- SYNCPOINT ROLLBACK (line 4099): Undoes account update if customer update fails

### 10.2 State Management

**State Flags Control Flow:**
- `ACUP-DETAILS-NOT-FETCHED` - Initial state, prompting for account number
- `ACUP-SHOW-DETAILS` - Account data fetched and displayed
- `ACUP-CHANGES-MADE` - User modified fields
- `ACUP-CHANGES-OK-NOT-CONFIRMED` - Validation passed, awaiting F5 confirmation
- `ACUP-CHANGES-OKAYED-AND-DONE` - Update completed successfully
- `ACUP-CHANGES-FAILED` - Update failed

**COMMAREA Preservation:**
The program maintains two areas in COMMAREA:
1. **CARDDEMO-COMMAREA** - Shared across all programs
2. **WS-THIS-PROGCOMMAREA** - Program-specific including:
   - Original fetched data (ACUP-OLD-*)
   - User modifications (ACUP-NEW-*)
   - Validation flags
   - State indicators

### 10.3 Date/Time Handling

**Current Date/Time:**
Uses `FUNCTION CURRENT-DATE` to populate screen header with current date (MM/DD/YY format) and time (HH:MM:SS format).

**Date Storage Format:**
- **File Storage:** YYYY-MM-DD (10 bytes)
- **Screen Display:** YYYY / MM / DD (separated components)
- **Validation:** Year (4), Month (2), Day (2) validated separately

**Date Conversion:**
For updates, date components are reassembled (lines 4013-4045):
```cobol
STRING ACUP-NEW-OPEN-YEAR
       '-'
       ACUP-NEW-OPEN-MON
       '-'
       ACUP-NEW-OPEN-DAY
DELIMITED BY SIZE
                            INTO ACCT-UPDATE-OPEN-DATE
```

### 10.4 Concurrent Update Detection

**Optimistic Locking Pattern:**
1. User views data (stored in ACUP-OLD-*)
2. User modifies data (stored in ACUP-NEW-*)
3. On save (F5), lock records with READ UPDATE
4. Compare locked record values against ACUP-OLD-* values
5. If any field changed, reject update and show current data
6. If unchanged, proceed with REWRITE

This prevents lost updates when multiple users edit the same account concurrently.

**Implementation** (9700-CHECK-CHANGE-IN-REC, lines 4109-4195):
Every field is compared. If any differs:
```cobol
SET DATA-WAS-CHANGED-BEFORE-UPDATE TO TRUE
```

### 10.5 Phone Number Formatting

**Input Format:** Three separate fields (area code, prefix, line number)
**Storage Format:** (999)999-9999
**Assembly** (lines 4047-4058):
```cobol
STRING '(',
       ACUP-NEW-CUST-PHONE-NUM-1A,
       ')',
       ACUP-NEW-CUST-PHONE-NUM-1B,
       '-',
       ACUP-NEW-CUST-PHONE-NUM-1C
DELIMITED BY SIZE    INTO CUST-UPDATE-PHONE-NUM-1
```

### 10.6 Currency Formatting

**Display Format:** -,---,---,--9.99 (with commas and sign)
**Storage Format:** S9(10)V99 COMP-3 (packed decimal)
**Conversion:** FUNCTION NUMVAL-C for input, edited move for display

### 10.7 Screen Attribute Management

The program uses copybook `CSSETATY` with COPY REPLACING to efficiently set screen attributes for multiple fields:
- Colors: DFHRED (errors), DFHTURQ (labels), DFHYELLOW (function keys)
- Protection: DFHPROT (display only), DFHUNPRO (input allowed)
- Intensity: DFHBRT (bright), DFHNORM (normal)
- Position: -1 (cursor positioning)

### 10.8 Transaction Integrity

**Two-Phase Update Pattern:**
1. Lock account record
2. Lock customer record
3. Validate optimistic locking
4. Update account (REWRITE)
5. Update customer (REWRITE)
6. If customer fails: SYNCPOINT ROLLBACK

This ensures both files are updated atomically - either both succeed or neither is updated.

### 10.9 Navigation Context

The program maintains navigation breadcrumbs in COMMAREA:
- **CDEMO-FROM-PROGRAM/TRANID:** Where this program was called from
- **CDEMO-TO-PROGRAM/TRANID:** Where to go on exit
- **CDEMO-LAST-MAPSET/MAP:** Last screen displayed

This enables flexible navigation, allowing the program to return to its caller or default to the main menu.

### 10.10 User Session Management

**Session Data in COMMAREA:**
- `CDEMO-USER-ID` - Logged-in user identifier
- `CDEMO-USER-TYPE` - Admin vs. regular user
- `CDEMO-PGM-CONTEXT` - Entry vs. re-entry state

This preserves user context across pseudo-conversational interactions and program transfers.

---

## End of COACTUPC Extraction Documentation

This document provides comprehensive coverage of the COACTUPC Account Update program suitable for migrating from COBOL CICS to a modern framework. All information is traceable to specific line numbers in the source code for verification purposes.
