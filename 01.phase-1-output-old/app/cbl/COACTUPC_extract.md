# COBOL CICS Program Analysis: COACTUPC

**Program ID:** COACTUPC  
**Transaction ID:** CAUP  
**Mapset:** COACTUP  
**Map:** CACTUPA  
**Purpose:** Accept and process credit card account updates for both Account Master and Customer Master data

---

## 1. Screen Visualization

```
┌────────────────────────────────────────────────────────────────────────────┐
│Tran:CAUP        CardDemo Account Update                      Date:mm/dd/yy │
│Prog:COACTUPC    Secure Credit Card System                    Time:hh:mm:ss │
│                                                                              │
│                             Update Account                                   │
│                  Account Number : ___________   Active Y/N: _               │
│       Opened :____-__-__                        Credit Limit        :______│
│       Expiry :____-__-__                        Cash credit Limit   :______│
│       Reissue:____-__-__                        Current Balance     :______│
│                                                 Current Cycle Credit:______│
│       Account Group:__________                  Current Cycle Debit :______│
│                            Customer Details                                  │
│       Customer id  :_________            SSN:___-__-____                    │
│       Date of birth:____-__-__           FICO Score:___                     │
│       First Name             Middle Name:            Last Name :            │
│       _________________________  _________________________  ________________│
│       Address:__________________________________________________  State __   │
│                 __________________________________________________  Zip _____│
│       City __________________________________________________  Country ___   │
│       Phone 1:___-___-____       Government Issued Id Ref    : ____________│
│       Phone 2:___-___-____                                                  │
│                                                                              │
│                                                                              │
│                                                                              │
└────────────────────────────────────────────────────────────────────────────┘
```

---

## 2. Field Details Table

### Screen Fields Mapping

| Line | Column | Field Name | Type | Length | Data Source | Attribute |
|------|--------|------------|------|--------|-------------|-----------|
| 1 | 1 | TRNNAME | Display | 4 | Transaction ID | ASKIP, FSET, NORM |
| 1 | 21 | TITLE01 | Display | 40 | Screen Title | ASKIP, NORM |
| 1 | 71 | CURDATE | Display | 8 | Current Date | ASKIP, NORM |
| 2 | 7 | PGMNAME | Display | 8 | Program Name | ASKIP, NORM |
| 2 | 21 | TITLE02 | Display | 40 | Screen Subtitle | ASKIP, NORM |
| 2 | 71 | CURTIME | Display | 8 | Current Time | ASKIP, NORM |
| 5 | 38 | ACCTSID | Input | 11 | Account Number | IC, UNPROT, UNDERLINE |
| 5 | 70 | ACSTTUS | Input | 1 | Account Active Status (Y/N) | UNPROT, UNDERLINE |
| 6 | 17 | OPNYEAR | Input | 4 | Account Open Year | FSET, UNPROT, UNDERLINE, RIGHT |
| 6 | 24 | OPNMON | Input | 2 | Account Open Month | UNPROT, UNDERLINE, RIGHT |
| 6 | 29 | OPNDAY | Input | 2 | Account Open Day | UNPROT, UNDERLINE, RIGHT |
| 6 | 61 | ACRDLIM | Input | 15 | Credit Limit | FSET, UNPROT, UNDERLINE |
| 7 | 17 | EXPYEAR | Input | 4 | Expiry Year | UNPROT, UNDERLINE, RIGHT |
| 7 | 24 | EXPMON | Input | 2 | Expiry Month | UNPROT, UNDERLINE, RIGHT |
| 7 | 29 | EXPDAY | Input | 2 | Expiry Day | UNPROT, UNDERLINE, RIGHT |
| 7 | 61 | ACSHLIM | Input | 15 | Cash Credit Limit | FSET, UNPROT, UNDERLINE |
| 8 | 17 | RISYEAR | Input | 4 | Reissue Year | UNPROT, UNDERLINE, RIGHT |
| 8 | 24 | RISMON | Input | 2 | Reissue Month | UNPROT, UNDERLINE, RIGHT |
| 8 | 29 | RISDAY | Input | 2 | Reissue Day | UNPROT, UNDERLINE, RIGHT |
| 8 | 61 | ACURBAL | Input | 15 | Current Balance | FSET, UNPROT, UNDERLINE |
| 9 | 61 | ACRCYCR | Input | 15 | Current Cycle Credit | FSET, UNPROT, UNDERLINE |
| 10 | 23 | AADDGRP | Input | 10 | Account Group | UNPROT, UNDERLINE |
| 10 | 61 | ACRCYDB | Input | 15 | Current Cycle Debit | FSET, UNPROT, UNDERLINE |
| 12 | 23 | ACSTNUM | Input | 9 | Customer ID | UNPROT, UNDERLINE |
| 12 | 55 | ACTSSN1 | Input | 3 | SSN Part 1 | UNPROT, UNDERLINE |
| 12 | 61 | ACTSSN2 | Input | 2 | SSN Part 2 | UNPROT, UNDERLINE |
| 12 | 66 | ACTSSN3 | Input | 4 | SSN Part 3 | UNPROT, UNDERLINE |
| 13 | 23 | DOBYEAR | Input | 4 | Date of Birth Year | UNPROT, UNDERLINE, RIGHT |
| 13 | 30 | DOBMON | Input | 2 | Date of Birth Month | UNPROT, UNDERLINE, RIGHT |
| 13 | 35 | DOBDAY | Input | 2 | Date of Birth Day | UNPROT, UNDERLINE, RIGHT |
| 13 | 62 | ACSTFCO | Input | 3 | FICO Credit Score | UNPROT, UNDERLINE |
| 15 | 1 | ACSFNAM | Input | 25 | First Name | UNPROT, UNDERLINE |
| 15 | 28 | ACSMNAM | Input | 25 | Middle Name | UNPROT, UNDERLINE |
| 15 | 55 | ACSLNAM | Input | 25 | Last Name | UNPROT, UNDERLINE |
| 16 | 10 | ACSADL1 | Input | 50 | Address Line 1 | UNPROT, UNDERLINE |
| 16 | 73 | ACSSTTE | Input | 2 | State Code | UNPROT, UNDERLINE |
| 17 | 10 | ACSADL2 | Input | 50 | Address Line 2 | UNPROT, UNDERLINE |
| 17 | 73 | ACSZIPC | Input | 5 | ZIP Code | UNPROT, UNDERLINE |
| 18 | 10 | ACSCITY | Input | 50 | City | UNPROT, UNDERLINE |
| 18 | 73 | ACSCTRY | Input | 3 | Country Code | UNPROT, UNDERLINE |
| 19 | 10 | ACSPH1A | Input | 3 | Phone 1 Area Code | UNPROT, UNDERLINE, RIGHT |
| 19 | 14 | ACSPH1B | Input | 3 | Phone 1 Exchange | UNPROT, UNDERLINE, RIGHT |
| 19 | 18 | ACSPH1C | Input | 4 | Phone 1 Number | UNPROT, UNDERLINE, RIGHT |
| 19 | 58 | ACSGOVT | Input | 20 | Government Issued ID | UNPROT, UNDERLINE |
| 20 | 10 | ACSPH2A | Input | 3 | Phone 2 Area Code | UNPROT, UNDERLINE, RIGHT |
| 20 | 14 | ACSPH2B | Input | 3 | Phone 2 Exchange | UNPROT, UNDERLINE, RIGHT |
| 20 | 18 | ACSPH2C | Input | 4 | Phone 2 Number | UNPROT, UNDERLINE, RIGHT |

---

## 3. Program Structure

### 3.1 Program Identification

**Program Name:** COACTUPC  
**Program Type:** CICS Online Transaction Processing (OLTP)  
**Transaction Code:** CAUP  
**Programming Language:** COBOL  
**Architecture:** Pseudo-conversational CICS application

### 3.2 Copybooks Used

**IBM Supplied Copybooks:**
- `DFHBMSCA` - BMS attribute characters
- `DFHAID` - Attention identifier definitions

**BMS Map Copybook:**
- `COACTUP` - Account Update screen map (Mapset: COACTUP, Map: CACTUPA)

**Common Application Copybooks:**
- `COTTL01Y` - Screen titles
- `CSDAT01Y` - Current date structure
- `CSMSG01Y` - Common messages
- `CSMSG02Y` - Abend variables
- `CSUSR01Y` - Signed-on user data
- `COCOM01Y` - Application COMMAREA structure

**Utility Copybooks:**
- `CSUTLDWY` - Generic date edit variables (CCYYMMDD format)
- `CVCRD01Y` - Other common working storage variables
- `CSLKPCDY` - North America phone area codes lookup

**File Layout Copybooks:**
- `CVACT01Y` - Account record layout (ACCTDAT)
- `CVACT03Y` - Card cross-reference layout (CARDDAT)
- `CVCUS01Y` - Customer record layout (CUSTDAT)

**Code Generation Copybook:**
- `CSSETATY` - Used with COPY REPLACING for setting BMS attributes dynamically

### 3.3 File Resources

1. **ACCTDAT** - Account Master File
   - Access: Direct keyed (READ, READ UPDATE, REWRITE)
   - Key: Account ID (11 bytes)
   - Purpose: Store account-level information (limits, balances, dates)

2. **CUSTDAT** - Customer Master File
   - Access: Direct keyed (READ, READ UPDATE, REWRITE)
   - Key: Customer ID (10 bytes)
   - Purpose: Store customer demographics and contact information

3. **CARDDAT (via CXACAIX)** - Card Cross-Reference File
   - Access: Direct via alternate index (READ only)
   - Alternate Index Key: Account ID → Card Number, Customer ID
   - Purpose: Link Account ID to Customer ID for data retrieval

---

## 4. CICS Commands

### 4.1 Transaction Control Commands

#### EXEC CICS HANDLE ABEND
```cobol
EXEC CICS HANDLE ABEND
          LABEL(ABEND-ROUTINE)
END-EXEC
```
- **Purpose:** Establish abend handling routine for abnormal termination
- **Location:** Paragraph 0000-MAIN (line 862)
- **When:** At program initialization, before any processing
- **Effect:** Routes control to ABEND-ROUTINE if program abends

#### EXEC CICS RETURN
```cobol
EXEC CICS RETURN
     TRANSID (LIT-THISTRANID)
     COMMAREA (WS-COMMAREA)
     LENGTH(LENGTH OF WS-COMMAREA)
END-EXEC
```
- **Purpose:** Return control to CICS with pseudo-conversational restart capability
- **Location:** Paragraph COMMON-RETURN (line 1015)
- **Parameters:**
  - TRANSID: 'CAUP' - Transaction code that will restart on next terminal input
  - COMMAREA: 2000-byte communication area preserving program state
  - LENGTH: Dynamic length of COMMAREA
- **Execution:** Called at end of every program iteration to enable pseudo-conversational operation

#### EXEC CICS XCTL
```cobol
EXEC CICS XCTL
     PROGRAM (CDEMO-TO-PROGRAM)
     COMMAREA(CARDDEMO-COMMAREA)
END-EXEC
```
- **Purpose:** Transfer control to another program (exit scenario)
- **Location:** Paragraph 0000-MAIN when PFK03 pressed (line 956)
- **Target Programs:** 
  - COMEN01C (main menu) - default
  - Or calling program stored in CDEMO-FROM-PROGRAM
- **Parameters:**
  - PROGRAM: Dynamic program name stored in CDEMO-TO-PROGRAM
  - COMMAREA: Standard CardDemo communication area

#### EXEC CICS SYNCPOINT
```cobol
EXEC CICS
     SYNCPOINT
END-EXEC
```
- **Purpose:** Commit all database changes before program transfer
- **Location:** Before XCTL when exiting via PFK03 (line 952)
- **Effect:** Makes all file updates permanent, releases all record locks

#### EXEC CICS SYNCPOINT ROLLBACK
```cobol
EXEC CICS
     SYNCPOINT ROLLBACK
END-EXEC
```
- **Purpose:** Rollback all changes when customer update fails after account update succeeded
- **Location:** Paragraph 9600-WRITE-PROCESSING (line 4099)
- **Scenario:** When account update succeeds but customer update fails, rollback maintains data consistency
- **Effect:** Reverses all file updates since last SYNCPOINT, restores original data

### 4.2 Screen I/O Commands

#### EXEC CICS RECEIVE MAP
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
  - MAP: 'CACTUPA' - Map name from BMS mapset
  - MAPSET: 'COACTUP' - BMS mapset name
  - INTO: CACTUPAI - Input area of BMS map structure
  - RESP/RESP2: Response codes for error handling
- **Processing:** Validates that map data was received; handles MAPFAIL condition

#### EXEC CICS SEND MAP
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
  - MAP: 'CACTUPA' - Map name
  - MAPSET: 'COACTUP' - Mapset name
  - FROM: CACTUPAO - Output area of BMS map structure
  - CURSOR: Position cursor at field with error indicator (-1 value)
  - DATAONLY: Send data only, not entire mapset definition
  - RESP: Response code for error handling
- **Variations:** Can also send with ERASE, ERASEAUP options depending on scenario

### 4.3 File I/O Commands

#### EXEC CICS READ (Card Cross-Reference via Alternate Index)
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
- **Purpose:** Read card cross-reference via alternate index to retrieve Customer ID
- **Location:** Paragraph 9200-GETCARDXREF-BYACCT (line 3654)
- **Access Method:** Keyed direct via alternate index CXACAIX
- **Key:** Account ID (11 bytes)
- **File:** CARDDAT accessed via alternate index path
- **Why Needed:** Links Account ID to Customer ID for subsequent customer data retrieval

#### EXEC CICS READ (Account Master)
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
- **Purpose:** Read account master record for initial display
- **Location:** Paragraph 9300-GETACCTDATA-BYACCT (line 3703)
- **Access Method:** Keyed direct (primary key)
- **Key:** Account ID (11 bytes)
- **File:** ACCTDAT
- **Record:** ACCOUNT-RECORD structure from CVACT01Y copybook

#### EXEC CICS READ (Customer Master)
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
- **Purpose:** Read customer master record for initial display
- **Location:** Paragraph 9400-GETCUSTDATA-BYCUST (line 3753)
- **Access Method:** Keyed direct (primary key)
- **Key:** Customer ID (10 bytes)
- **File:** CUSTDAT
- **Record:** CUSTOMER-RECORD structure from CVCUS01Y copybook

#### EXEC CICS READ UPDATE (Account Master)
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
- **Purpose:** Read account record with exclusive lock for update processing
- **Location:** Paragraph 9600-WRITE-PROCESSING (line 3894)
- **Access Method:** Keyed direct with UPDATE option
- **Locking:** Exclusive lock held until REWRITE or SYNCPOINT
- **Lock Scope:** Single account record locked

#### EXEC CICS READ UPDATE (Customer Master)
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
- **Purpose:** Read customer record with exclusive lock for update processing
- **Location:** Paragraph 9600-WRITE-PROCESSING (line 3921)
- **Access Method:** Keyed direct with UPDATE option
- **Locking:** Exclusive lock held until REWRITE or SYNCPOINT

#### EXEC CICS REWRITE (Account Master)
```cobol
EXEC CICS
     REWRITE FILE(LIT-ACCTFILENAME)
             FROM(ACCT-UPDATE-RECORD)
             LENGTH(LENGTH OF ACCT-UPDATE-RECORD)
             RESP(WS-RESP-CD)
             RESP2(WS-REAS-CD)
END-EXEC
```
- **Purpose:** Update account master record with modified data
- **Location:** Paragraph 9600-WRITE-PROCESSING (line 4065)
- **Prerequisite:** Must have previously executed READ UPDATE for this record
- **Record:** ACCT-UPDATE-RECORD contains modified account data
- **Effect:** Persists account changes to ACCTDAT file

#### EXEC CICS REWRITE (Customer Master)
```cobol
EXEC CICS
             REWRITE FILE(LIT-CUSTFILENAME)
             FROM(CUST-UPDATE-RECORD)
             LENGTH(LENGTH OF CUST-UPDATE-RECORD)
             RESP(WS-RESP-CD)
             RESP2(WS-REAS-CD)
END-EXEC
```
- **Purpose:** Update customer master record with modified data
- **Location:** Paragraph 9600-WRITE-PROCESSING (line 4086)
- **Prerequisite:** Must have previously executed READ UPDATE for this record
- **Record:** CUST-UPDATE-RECORD contains modified customer data
- **Effect:** Persists customer changes to CUSTDAT file

#### EXEC CICS SEND (Abend Information)
```cobol
EXEC CICS SEND
                 FROM (ABEND-DATA)
                 LENGTH(LENGTH OF ABEND-DATA)
                 ERASE
                 RESP(WS-RESP-CD)
                 RESP2(WS-REAS-CD)
END-EXEC
```
- **Purpose:** Send abend information to terminal when program abends
- **Location:** ABEND-ROUTINE paragraph (line 4211)
- **Effect:** Displays diagnostic information including abend code and culprit program

---

## 5. Navigational Details

### 5.1 Function Key Support

**ENTER Key**
- **Primary Function:** Process user input
- **Behaviors:**
  - **Initial Entry:** When account number entered, fetch account and customer data
  - **Data Display:** When data modified, validate changes and prepare for confirmation
  - **Confirmation:** Process validated changes (when in confirmation state)
- **Location:** Lines 906, 915, 921
- **Always Available:** Yes

**F3 (PFK03) - Exit**
- **Function:** Exit program and return to calling application
- **Behavior:**
  - Determines return destination (calling program or main menu)
  - Executes SYNCPOINT to commit any pending changes
  - Transfers control via XCTL to target program
  - Passes COMMAREA to maintain application state
- **Location:** Lines 907, 927-959
- **Return Destinations:**
  - If CDEMO-FROM-PROGRAM is populated: Return to calling program
  - If CDEMO-FROM-PROGRAM is LOW-VALUES or SPACES: Return to COMEN01C (main menu)
- **Always Available:** Yes

**F5 (PFK05) - Save Changes**
- **Function:** Confirm and save account/customer updates
- **Behavior:**
  - Only available when changes detected and validated
  - Triggers update processing (paragraph 9600-WRITE-PROCESSING)
  - Executes READ UPDATE, REWRITE for both account and customer records
  - Performs concurrent update detection
  - Returns success or failure message
- **Location:** Line 908
- **Availability:** Displayed/enabled only when ACUP-CHANGES-OK-NOT-CONFIRMED state is active
- **Prerequisite:** User must have made valid changes that passed all edits

**F12 (PFK12) - Cancel Changes**
- **Function:** Abandon changes and return to initial entry state
- **Behavior:**
  - Only active when account details have been fetched
  - Abandons any modifications made by user
  - Resets to initial state with blank screen
  - Clears account number field
  - Prompts for new account entry
- **Location:** Line 910
- **Availability:** Displayed/enabled only after account data is fetched (not in ACUP-DETAILS-NOT-FETCHED state)

**Invalid Keys**
- **Handling:** Any other PF key press is treated as ENTER
- **Location:** Lines 914-916
- **Reason:** Prevents user errors from causing unexpected behavior or program failures

### 5.2 Screen Flow and State Machine

```
┌─────────────────────────────────────────────────────────────────┐
│                        INITIAL ENTRY                             │
│  - User invokes transaction CAUP                                │
│  - Program displays blank screen                                │
│  - Prompt: "Account Number: ___________"                        │
│  - Available keys: ENTER, F3                                    │
│  - State: ACUP-DETAILS-NOT-FETCHED                              │
└──────────────────┬──────────────────────────────────────────────┘
                   │
          ┌────────▼────────┐
          │  User enters    │
          │  Account Number │
          │  Presses ENTER  │
          └────────┬────────┘
                   │
     ┌─────────────▼──────────────┐
     │  FETCH ACCOUNT DATA         │
     │  - 9200-GETCARDXREF-BYACCT │ (Get Customer ID)
     │  - 9300-GETACCTDATA-BYACCT │ (Get Account data)
     │  - 9400-GETCUSTDATA-BYCUST │ (Get Customer data)
     │  - 9500-STORE-FETCHED-DATA │ (Store for display)
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
   │ ACCOUNT &   │    │ "Account not │
   │ CUSTOMER    │    │  found"      │
   │ DATA        │    │ Return to    │
   │ - All fields│    │ entry screen │
   │   populated │    └──────────────┘
   │ - ENTER,F3, │
   │   F12 shown │
   │ - State:    │
   │   ACUP-SHOW-│
   │   DETAILS   │
   └──────┬──────┘
          │
   ┌──────▼──────┐
   │ User modifies│
   │ one or more  │
   │ field values │
   │ Presses ENTER│
   └──────┬──────┘
          │
   ┌──────▼──────────────┐
   │ VALIDATE ALL INPUTS │
   │ - 1200-EDIT-MAP-    │
   │   INPUTS            │
   │ - Field-level edits:│
   │   * 1210-1295 series│
   │ - Date validations  │
   │ - Numeric checks    │
   │ - Required fields   │
   └──────┬──────────────┘
          │
   ┌──────▼────────────┐
   │ COMPARE OLD vs NEW│
   │ - 1205-COMPARE-   │
   │   OLD-NEW         │
   │ - Detect changes  │
   └──────┬────────────┘
          │
   ┌──────▼─────────┐
   │ Any changes?   │
   └──────┬─────────┘
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
  ┌──┴───┐   │ Return data  │
  │      │   └──────────────┘
YES│      │NO
  │      │
  ▼      ▼
┌────┐ ┌──────────────────┐
│RED │ │ Highlight changed│
│high│ │ fields in GREEN  │
│ligt│ │ Display message: │
│erro│ │ "Press F5 to save│
│r   │ │ or F12 to cancel"│
│flds│ │ Enable: F5, F12  │
│Ret │ │ State: ACUP-     │
│urn │ │ CHANGES-OK-NOT-  │
└────┘ │ CONFIRMED        │
       └────────┬─────────┘
                │
         ┌──────▼──────┐
         │ User chooses│
         │   action    │
         └──────┬──────┘
                │
     ┌──────────┼──────────┐
     │          │          │
     ▼          ▼          ▼
┌─────────┐ ┌──────┐  ┌──────┐
│   F5    │ │ F12  │  │  F3  │
│  SAVE   │ │CANCEL│  │ EXIT │
└────┬────┘ └──┬───┘  └───┬──┘
     │         │          │
     │         │          └────────┐
     │         │                   │
     ▼         ▼                   ▼
┌──────────┐ ┌────────┐      ┌─────────┐
│ UPDATE   │ │ Reset  │      │SYNCPOINT│
│ PROCESS  │ │ to new │      │XCTL to  │
│ 9600-    │ │ entry  │      │calling  │
│ WRITE-   │ │ screen │      │program/ │
│ PROC     │ └────────┘      │main menu│
└────┬─────┘                 └─────────┘
     │
     ▼
┌──────────────────┐
│ Lock Records     │
│ - READ UPDATE    │
│   ACCTDAT        │
│ - READ UPDATE    │
│   CUSTDAT        │
└────────┬─────────┘
         │
    ┌────▼──────┐
    │ Locks OK? │
    └────┬──────┘
         │
    ┌────┴────┐
    │         │
  YES│         │NO
    │         │
    ▼         ▼
┌────────┐ ┌──────────┐
│Concur  │ │Display   │
│rent    │ │"Could not│
│update  │ │lock      │
│check   │ │records"  │
│9700-   │ │error     │
│CHECK-  │ │Return    │
│FOR-    │ └──────────┘
│CONCUR  │
│RENT    │
└───┬────┘
    │
    ▼
┌───────────┐
│ Data      │
│ changed   │
│ by another│
│ user?     │
└─────┬─────┘
      │
   ┌──┴────┐
   │       │
 YES│       │NO
   │       │
   ▼       ▼
┌──────┐ ┌──────────┐
│Display│ │ REWRITE  │
│"Data │ │ ACCTDAT  │
│change│ └─────┬────┘
│d"    │       │
│Redis │       ▼
│play  │ ┌──────────┐
│fresh │ │ Success? │
│data  │ └─────┬────┘
└──────┘       │
            ┌──┴────┐
            │       │
          YES│       │NO
            │       │
            ▼       ▼
       ┌─────────┐ ┌───────┐
       │ REWRITE │ │ Error │
       │ CUSTDAT │ │ msg   │
       └────┬────┘ │Return │
            │      └───────┘
            ▼
       ┌─────────┐
       │Success? │
       └────┬────┘
            │
         ┌──┴────┐
         │       │
       YES│       │NO
         │       │
         ▼       ▼
    ┌────────┐ ┌──────────┐
    │Success │ │SYNCPOINT │
    │message │ │ROLLBACK  │
    │"Update │ │(undo acct│
    │success"│ │ update)  │
    │State:  │ │Error msg │
    │ACUP-   │ │Return    │
    │CHANGES-│ └──────────┘
    │OKAYED  │
    │-AND-   │
    │DONE    │
    └────┬───┘
         │
         ▼
    ┌────────┐
    │ Reset  │
    │ to new │
    │ entry  │
    │ screen │
    └────────┘
```

### 5.3 Program States

**State: ACUP-DETAILS-NOT-FETCHED**
- Initial entry state
- No account data loaded
- Only account number field active
- Available keys: ENTER (to fetch), F3 (to exit)

**State: ACUP-SHOW-DETAILS**
- Account and customer data displayed
- All fields editable
- Available keys: ENTER (to validate), F3 (to exit), F12 (to cancel)

**State: ACUP-CHANGES-OK-NOT-CONFIRMED**
- Valid changes detected
- Awaiting user confirmation
- Fields highlighted to show changes
- Available keys: F5 (to save), F12 (to cancel), F3 (to exit)

**State: ACUP-CHANGES-OKAYED-AND-DONE**
- Update successfully completed
- Success message displayed
- Screen reset for new entry
- Available keys: ENTER (new search), F3 (to exit)

**State: ACUP-CHANGES-FAILED**
- Update failed (concurrent update or other error)
- Error message displayed
- Screen reset for new entry
- Available keys: ENTER (new search), F3 (to exit)

---

## 6. Business Logic and Program Execution Flow

### 6.1 Program Initialization (Paragraph 0000-MAIN, Lines 859-1023)

The program begins execution at paragraph **0000-MAIN** when transaction CAUP is invoked. The first critical step is establishing an abend handler:

```cobol
EXEC CICS HANDLE ABEND
          LABEL(ABEND-ROUTINE)
END-EXEC
```

This ensures that if the program encounters any abnormal termination, control transfers to the ABEND-ROUTINE for graceful error reporting.

Next, the program initializes its working storage areas:
```cobol
INITIALIZE CC-WORK-AREA
           WS-MISC-STORAGE
           WS-COMMAREA
```

The program stores its transaction context (`MOVE LIT-THISTRANID TO WS-TRANID`) and clears any existing error messages (`SET WS-RETURN-MSG-OFF TO TRUE`).

**COMMAREA Processing:** The program examines `EIBCALEN` to determine if this is a fresh invocation or a pseudo-conversational restart:

- **If EIBCALEN = 0** (no COMMAREA passed): This is a fresh entry, so the program initializes CARDDEMO-COMMAREA and WS-THIS-PROGCOMMAREA, sets CDEMO-PGM-ENTER to TRUE, and sets ACUP-DETAILS-NOT-FETCHED to TRUE
- **If EIBCALEN > 0** (COMMAREA exists): The program is restarting from a previous iteration, so it moves DFHCOMMAREA to CARDDEMO-COMMAREA and WS-THIS-PROGCOMMAREA to restore the previous state

**PF Key Remapping:** The program calls paragraph YYYY-STORE-PFKEY to remap and store any pressed function keys into the COMMAREA structure.

**AID Key Validation:** The program validates which attention identifier (AID) key was pressed:
```cobol
SET PFK-INVALID TO TRUE
IF CCARD-AID-ENTER OR
   CCARD-AID-PFK03 OR
   (CCARD-AID-PFK05 AND ACUP-CHANGES-OK-NOT-CONFIRMED) OR
   (CCARD-AID-PFK12 AND NOT ACUP-DETAILS-NOT-FETCHED)
   SET PFK-VALID TO TRUE
END-IF
```

If an invalid key was pressed, the program treats it as ENTER to prevent unexpected behavior.

### 6.2 Main Decision Logic (EVALUATE Statement, Lines 921-1004)

The program uses an EVALUATE TRUE statement to route processing based on the current state and user action:

#### Case 1: F3 Exit (Lines 927-959)

When the user presses F3, the program prepares to exit:

1. **Determine Return Destination:**
   - If CDEMO-FROM-TRANID is LOW-VALUES or SPACES, set CDEMO-TO-TRANID to LIT-MENUTRANID (main menu)
   - Otherwise, move CDEMO-FROM-TRANID to CDEMO-TO-TRANID (return to caller)
   - Similarly for program names (CDEMO-TO-PROGRAM)

2. **Update Navigation Context:**
   ```cobol
   MOVE LIT-THISTRANID     TO CDEMO-FROM-TRANID
   MOVE LIT-THISPGM        TO CDEMO-FROM-PROGRAM
   ```

3. **Commit Changes:**
   ```cobol
   EXEC CICS SYNCPOINT END-EXEC
   ```
   This commits any pending database changes before transferring control.

4. **Transfer Control:**
   ```cobol
   EXEC CICS XCTL
        PROGRAM (CDEMO-TO-PROGRAM)
        COMMAREA(CARDDEMO-COMMAREA)
   END-EXEC
   ```

#### Case 2: Fresh Entry (Lines 964-973)

When the program is entered fresh (ACUP-DETAILS-NOT-FETCHED and CDEMO-PGM-ENTER) or returning from main menu:

1. Initialize program-specific COMMAREA: `INITIALIZE WS-THIS-PROGCOMMAREA`
2. Display blank entry screen: `PERFORM 3000-SEND-MAP THRU 3000-SEND-MAP-EXIT`
3. Set state flags: `SET CDEMO-PGM-REENTER TO TRUE` and `SET ACUP-DETAILS-NOT-FETCHED TO TRUE`
4. Return to CICS: `GO TO COMMON-RETURN`

#### Case 3: Update Complete or Failed (Lines 979-989)

After a successful update (ACUP-CHANGES-OKAYED-AND-DONE) or failed update (ACUP-CHANGES-FAILED):

1. Reset all working storage:
   ```cobol
   INITIALIZE WS-THIS-PROGCOMMAREA
              WS-MISC-STORAGE
              CDEMO-ACCT-ID
   ```
2. Display fresh entry screen for new account search
3. Set state flags for fresh entry
4. Return to CICS

#### Case 4: Normal Processing (Lines 996-1003)

For all other cases (user entering data, making changes, or pressing ENTER):

1. **Process Inputs:** `PERFORM 1000-PROCESS-INPUTS THRU 1000-PROCESS-INPUTS-EXIT`
   - Receive map data from terminal
   - Edit and validate all inputs
   
2. **Decide Action:** `PERFORM 2000-DECIDE-ACTION THRU 2000-DECIDE-ACTION-EXIT`
   - Determine what to do based on validation results and current state
   
3. **Send Screen:** `PERFORM 3000-SEND-MAP THRU 3000-SEND-MAP-EXIT`
   - Display results to user
   
4. **Return:** `GO TO COMMON-RETURN`

### 6.3 Input Processing (Paragraph 1000-PROCESS-INPUTS, Lines 1025-1037)

This paragraph orchestrates input processing:

1. **Receive Map Data:** `PERFORM 1100-RECEIVE-MAP THRU 1100-RECEIVE-MAP-EXIT`
2. **Edit Map Inputs:** `PERFORM 1200-EDIT-MAP-INPUTS THRU 1200-EDIT-MAP-INPUTS-EXIT`
3. **Store Error Message:** `MOVE WS-RETURN-MSG TO CCARD-ERROR-MSG`
4. **Set Navigation Context:** Store current program, mapset, and map names

### 6.4 Receiving Map Data (Paragraph 1100-RECEIVE-MAP, Lines 1039-1426)

The program executes:
```cobol
EXEC CICS RECEIVE MAP(LIT-THISMAP)
          MAPSET(LIT-THISMAPSET)
          INTO(CACTUPAI)
          RESP(WS-RESP-CD)
          RESP2(WS-REAS-CD)
END-EXEC
```

Then initializes the new details area: `INITIALIZE ACUP-NEW-DETAILS`

**Field Extraction Logic:** The program examines each input field from the BMS map. For each field:
- If the field contains '*' or SPACES, move LOW-VALUES to the corresponding work field (indicating no change or blank)
- Otherwise, move the input value to the work field

**Example for Account ID:**
```cobol
IF  ACCTSIDI OF CACTUPAI = '*'
OR  ACCTSIDI OF CACTUPAI = SPACES
    MOVE LOW-VALUES           TO CC-ACCT-ID
                                 ACUP-NEW-ACCT-ID-X
ELSE
    MOVE ACCTSIDI OF CACTUPAI TO CC-ACCT-ID
                                 ACUP-NEW-ACCT-ID-X
END-IF
```

**Special Handling for Numeric Fields:** For monetary amounts (Credit Limit, Cash Limit, Current Balance, etc.), the program:
1. Tests if the input is a valid numeric string using `FUNCTION TEST-NUMVAL-C`
2. If valid, converts the string to numeric using `FUNCTION NUMVAL-C`
3. Stores both the character and numeric representations

**Early Exit:** If the state is ACUP-DETAILS-NOT-FETCHED, the program exits after extracting only the account ID (no need to process other fields yet):
```cobol
IF ACUP-DETAILS-NOT-FETCHED
   GO TO 1100-RECEIVE-MAP-EXIT
END-IF
```

Otherwise, it continues extracting all remaining fields (account status, dates, customer information, address, phone numbers, etc.).

### 6.5 Input Validation (Paragraph 1200-EDIT-MAP-INPUTS, Lines 1429-1678)

This is the comprehensive validation engine of the program. It begins by setting `INPUT-OK TO TRUE`, then systematically validates each field.

**Key Validation Chain:**

1. **Account Number Validation** (Paragraph 1210-EDIT-ACCOUNT):
   - Check if account number provided
   - Check if numeric
   - Check if within valid range
   - If initial fetch, proceed to data retrieval
   - If ACUP-DETAILS-NOT-FETCHED, perform:
     ```cobol
     PERFORM 9200-GETCARDXREF-BYACCT THRU 9200-GETCARDXREF-BYACCT-EXIT
     PERFORM 9300-GETACCTDATA-BYACCT THRU 9300-GETACCTDATA-BYACCT-EXIT
     PERFORM 9400-GETCUSTDATA-BYCUST THRU 9400-GETCUSTDATA-BYCUST-EXIT
     PERFORM 9500-STORE-FETCHED-DATA THRU 9500-STORE-FETCHED-DATA-EXIT
     ```

2. **Non-Key Field Validations** (when data already fetched):
   - **Account Status** (1220-EDIT-YESNO): Must be 'Y' or 'N'
   - **Credit Limit** (1225-EDIT-ALPHA-REQD or similar): Must be numeric, positive
   - **Cash Credit Limit**: Must be numeric, positive
   - **Current Balance**: Must be numeric
   - **Current Cycle Credit/Debit**: Must be numeric
   - **Open Date** (1240-EDIT-DATE-CCYYMMDD): Validates year, month, day
   - **Expiry Date**: Future date validation, valid format
   - **Reissue Date**: Valid date format
   - **Date of Birth** (1265-EDIT-DOB): Must be in past, age restrictions
   - **FICO Score** (1250-EDIT-FICO): Range 300-850
   - **Customer ID** (1215-EDIT-MANDATORY): Required field
   - **SSN** (1260-EDIT-SSN): 9-digit format, validity checks
   - **Name Fields** (1225-EDIT-ALPHA-REQD): First and last name required, alphabetic
   - **Address Fields** (1230-EDIT-ALPHANUM-REQD): Address line 1, city required
   - **State Code** (1270-EDIT-STATE): Valid 2-character US state code
   - **ZIP Code** (1275-EDIT-ZIP): 5-digit format
   - **Country Code** (1280-EDIT-COUNTRY): Valid country code
   - **Phone Numbers** (1285-EDIT-PHONE): 10-digit US format, area code validation

Each validation paragraph follows a pattern:
```cobol
1xxx-EDIT-FIELDNAME.
    SET FLG-FIELD-NOT-OK TO TRUE
    
    [Validation logic]
    
    IF [validation fails]
        SET INPUT-ERROR TO TRUE
        SET FLG-FIELD-NOT-OK TO TRUE
        MOVE error-message TO WS-RETURN-MSG
        GO TO 1xxx-EDIT-FIELDNAME-EXIT
    END-IF
    
    SET FLG-FIELD-ISVALID TO TRUE
    .
1xxx-EDIT-FIELDNAME-EXIT.
    EXIT
    .
```

**Error Message Accumulation:** When validation fails, the program:
1. Sets `INPUT-ERROR TO TRUE`
2. Stores error message in `WS-RETURN-MSG`
3. Sets field-specific error flag
4. Exits the validation paragraph

**Attribute Setting:** After all validations, the program uses COPY REPLACING to set BMS attributes for each field:
```cobol
COPY CSSETATY REPLACING
  ==(TESTVAR1)== BY ==ACCT-STATUS==
  ==(SCRNVAR2)== BY ==ACSTTUS==
  ==(MAPNAME3)== BY ==CACTUPA== .
```

This dynamically sets field attributes (PROT/UNPROT, COLOR, HILIGHT) based on validation results.

### 6.6 Change Detection (Paragraph 1205-COMPARE-OLD-NEW, Lines 1681-1777)

After validation, if the state is NOT ACUP-DETAILS-NOT-FETCHED (meaning data was already displayed), the program compares old vs. new values:

```cobol
SET NO-CHANGES-FOUND TO TRUE

IF  ACUP-NEW-ACCT-ID-X         = ACUP-OLD-ACCT-ID-X
AND ACUP-NEW-ACTIVE-STATUS     = ACUP-OLD-ACTIVE-STATUS
AND ACUP-NEW-CREDIT-LIMIT-N    = ACUP-OLD-CREDIT-LIMIT-N
AND ACUP-NEW-CASH-CREDIT-LIMIT-N = ACUP-OLD-CASH-CREDIT-LIMIT-N
[... comparison continues for all fields ...]
    CONTINUE
ELSE
    SET CHANGE-HAS-OCCURRED TO TRUE
END-IF
```

If any field differs, `CHANGE-HAS-OCCURRED` is set, indicating user made modifications.

### 6.7 Action Decision Logic (Paragraph 2000-DECIDE-ACTION, Lines 1800+)

Based on validation results and change detection, the program decides the next action:

1. **If INPUT-ERROR**: Display error messages, re-show screen with error highlights
2. **If NO-CHANGES-FOUND**: Display "No changes detected" message
3. **If CHANGE-HAS-OCCURRED and INPUT-OK**: 
   - Set state to ACUP-CHANGES-OK-NOT-CONFIRMED
   - Display message "Press F5 to save or F12 to cancel"
   - Highlight changed fields
4. **If CCARD-AID-PFK05 (F5 pressed) and ACUP-CHANGES-OK-NOT-CONFIRMED**:
   - Proceed to update processing
   - Perform 9600-WRITE-PROCESSING

### 6.8 Data Fetching (Paragraphs 9200-9500)

#### 9200-GETCARDXREF-BYACCT (Lines 3652-3699)

Purpose: Retrieve Customer ID using Account ID via alternate index.

```cobol
MOVE CC-ACCT-ID TO WS-CARD-RID-ACCT-ID

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

Error handling:
- If RESP = DFHRESP(NOTFND): "Account not found" message
- If RESP ≠ DFHRESP(NORMAL): "Error reading card xref file" message

Success: Extracts Customer ID from CARD-XREF-RECORD

#### 9300-GETACCTDATA-BYACCT (Lines 3701-3749)

Purpose: Read complete account master record.

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

Error handling:
- If RESP = DFHRESP(NOTFND): "Account record not found" message
- If RESP ≠ DFHRESP(NORMAL): "Error reading account file" message

#### 9400-GETCUSTDATA-BYCUST (Lines 3751-3799)

Purpose: Read complete customer master record.

```cobol
MOVE CARD-XREF-CUST-ID TO WS-CARD-RID-CUST-ID

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

Error handling:
- If RESP = DFHRESP(NOTFND): "Customer record not found" message
- If RESP ≠ DFHRESP(NORMAL): "Error reading customer file" message

#### 9500-STORE-FETCHED-DATA (Lines 3801-3889)

Purpose: Move fetched data from file records to COMMAREA storage for display.

This paragraph systematically moves data:
- From ACCOUNT-RECORD to ACUP-OLD-xxx fields
- From CUSTOMER-RECORD to ACUP-OLD-xxx fields

Example:
```cobol
MOVE ACCT-ID              TO ACUP-OLD-ACCT-ID-X
MOVE ACCT-ACTIVE-STATUS   TO ACUP-OLD-ACTIVE-STATUS
MOVE ACCT-CURR-BAL        TO ACUP-OLD-CURR-BAL-N
MOVE CUST-FIRST-NAME      TO ACUP-OLD-FIRST-NAME
[... continues for all fields ...]
```

After storing, sets `ACUP-SHOW-DETAILS` state to indicate data is ready for display.

### 6.9 Update Processing (Paragraph 9600-WRITE-PROCESSING, Lines 3891-4105)

This is the critical update transaction logic that ensures data integrity.

#### Phase 1: Lock Records (Lines 3894-3945)

**Lock Account Record:**
```cobol
MOVE CC-ACCT-ID TO WS-CARD-RID-ACCT-ID

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

Error handling:
- If NOTFND: "Account record not found for update"
- If other error: "Could not lock account record"

**Lock Customer Record:**
```cobol
MOVE CDEMO-CUST-ID TO WS-CARD-RID-CUST-ID

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

Both records are now exclusively locked for this transaction.

#### Phase 2: Concurrent Update Detection (Paragraph 9700-CHECK-FOR-CONCURRENT-UPDATE, Lines 4107-4182)

Purpose: Detect if another user modified the data between when this user fetched it and now.

```cobol
IF  ACCOUNT-RECORD = ACUP-OLD-ACCOUNT-RECORD
AND CUSTOMER-RECORD = ACUP-OLD-CUSTOMER-RECORD
    SET WS-LOCKED-NOT-CHANGED TO TRUE
ELSE
    SET WS-LOCKED-BUT-CHANGED TO TRUE
END-IF
```

If data changed:
- Display message "Data changed by another user"
- Re-fetch and display current data
- Release locks (implicit SYNCPOINT or program termination)
- User must re-apply changes

#### Phase 3: Update Account Record (Lines 4047-4070)

If concurrent update check passes, build update record:
```cobol
MOVE ACCOUNT-RECORD TO ACCT-UPDATE-RECORD

[Update individual fields from ACUP-NEW-xxx values]
MOVE ACUP-NEW-ACTIVE-STATUS     TO ACCT-UPDATE-ACTIVE-STATUS
MOVE ACUP-NEW-CREDIT-LIMIT-N    TO ACCT-UPDATE-CREDIT-LIMIT
MOVE ACUP-NEW-CURR-BAL-N        TO ACCT-UPDATE-CURR-BAL
[... continues for all modified fields ...]

EXEC CICS
     REWRITE FILE(LIT-ACCTFILENAME)
             FROM(ACCT-UPDATE-RECORD)
             LENGTH(LENGTH OF ACCT-UPDATE-RECORD)
             RESP(WS-RESP-CD)
             RESP2(WS-REAS-CD)
END-EXEC
```

Error handling:
- If REWRITE fails: Set LOCKED-BUT-UPDATE-FAILED, display error

#### Phase 4: Update Customer Record (Lines 4082-4095)

```cobol
MOVE CUSTOMER-RECORD TO CUST-UPDATE-RECORD

[Update individual fields from ACUP-NEW-xxx values]
MOVE ACUP-NEW-CUST-ID          TO CUST-UPDATE-ID
MOVE ACUP-NEW-FIRST-NAME       TO CUST-UPDATE-FIRST-NAME
MOVE ACUP-NEW-LAST-NAME        TO CUST-UPDATE-LAST-NAME
[... continues for all modified fields ...]

EXEC CICS
             REWRITE FILE(LIT-CUSTFILENAME)
             FROM(CUST-UPDATE-RECORD)
             LENGTH(LENGTH OF CUST-UPDATE-RECORD)
             RESP(WS-RESP-CD)
             RESP2(WS-REAS-CD)
END-EXEC
```

**Critical Error Handling (Lines 4096-4102):**
If customer REWRITE fails after account REWRITE succeeded:
```cobol
SET LOCKED-BUT-UPDATE-FAILED TO TRUE
EXEC CICS
     SYNCPOINT ROLLBACK
END-EXEC
GO TO 9600-WRITE-PROCESSING-EXIT
```

This SYNCPOINT ROLLBACK undoes the account update, maintaining transactional integrity.

#### Phase 5: Success Path

If both REWRITEs succeed:
```cobol
SET ACUP-CHANGES-OKAYED-AND-DONE TO TRUE
MOVE 'Update successful' TO WS-RETURN-MSG
```

Locks are released when program returns (implicit SYNCPOINT).

### 6.10 Screen Display (Paragraph 3000-SEND-MAP, Lines 3001-3589)

The program builds the output screen by:

1. **Populate Static Fields:**
   - Transaction name, program name
   - Current date and time
   - Screen titles

2. **Populate Data Fields:**
   - If ACUP-SHOW-DETAILS: Display fetched/updated data
   - If ACUP-DETAILS-NOT-FETCHED: Display blank entry form
   - If error state: Display error messages and highlight error fields

3. **Set Field Attributes:**
   - Normal fields: UNPROT, UNDERLINE
   - Error fields: PROT or UNPROT with RED HILIGHT
   - Changed fields: GREEN HILIGHT
   - Protected fields: PROT, ASKIP

4. **Position Cursor:**
   - If errors: Position at first error field (field attribute = -1)
   - If no errors: Position at account number field (IC attribute)

5. **Send Map:**
   ```cobol
   EXEC CICS SEND MAP(CCARD-NEXT-MAP)
                MAPSET(CCARD-NEXT-MAPSET)
                FROM(CACTUPAO)
                CURSOR
                DATAONLY
                RESP(WS-RESP-CD)
   END-EXEC
   ```

### 6.11 Pseudo-Conversational Return (Paragraph COMMON-RETURN, Lines 1007-1020)

Every program iteration ends here:

1. **Move Error Message to COMMAREA:**
   ```cobol
   MOVE WS-RETURN-MSG TO CCARD-ERROR-MSG
   ```

2. **Build Complete COMMAREA:**
   ```cobol
   MOVE CARDDEMO-COMMAREA TO WS-COMMAREA
   MOVE WS-THIS-PROGCOMMAREA TO
        WS-COMMAREA(LENGTH OF CARDDEMO-COMMAREA + 1:
                     LENGTH OF WS-THIS-PROGCOMMAREA)
   ```

3. **Return to CICS:**
   ```cobol
   EXEC CICS RETURN
        TRANSID (LIT-THISTRANID)
        COMMAREA (WS-COMMAREA)
        LENGTH(LENGTH OF WS-COMMAREA)
   END-EXEC
   ```

This pseudo-conversational design:
- Releases all system resources between user interactions
- Maintains application state in COMMAREA
- Enables efficient multi-user operation
- Automatically restarts transaction when user presses any key

### 6.12 Abend Handling (ABEND-ROUTINE, Lines 4184-4237)

If the program abends, control transfers here:

1. **Populate Abend Data:**
   ```cobol
   MOVE EIBRESP    TO ABEND-RESP-CD
   MOVE EIBRESP2   TO ABEND-RESP2-CD
   MOVE EIBFN      TO ABEND-FUNCTION-CD
   MOVE LIT-THISPGM TO ABEND-CULPRIT
   ```

2. **Send Abend Information:**
   ```cobol
   EXEC CICS SEND
                FROM (ABEND-DATA)
                LENGTH(LENGTH OF ABEND-DATA)
                ERASE
                RESP(WS-RESP-CD)
                RESP2(WS-REAS-CD)
   END-EXEC
   ```

3. **Terminate:**
   ```cobol
   EXEC CICS RETURN END-EXEC
   ```

---

## 7. Data Structures and Sources

### 7.1 Screen I/O Structure (BMS Copybook COACTUP)

**Input Area (CACTUPAI):**
```cobol
01  CACTUPAI.
    03  ACCTSIDI        PIC X(11).  
    03  ACSTTUSI        PIC X(1).   
    03  OPNYEARI        PIC X(4).   
    03  OPNMONI         PIC X(2).   
    03  OPNDAYI         PIC X(2).   
    03  ACRDLIMI        PIC X(15).  
    03  EXPYEARI        PIC X(4).   
    03  EXPMONI         PIC X(2).   
    03  EXPDAYI         PIC X(2).   
    03  ACSHLIMI        PIC X(15).  
    03  RISYEARI        PIC X(4).   
    03  RISMONI         PIC X(2).   
    03  RISDAYI         PIC X(2).   
    03  ACURBALI        PIC X(15).  
    03  ACRCYCRI        PIC X(15).  
    03  AADDGRPI        PIC X(10).  
    03  ACRCYDBI        PIC X(15).  
    03  ACSTNUMI        PIC X(9).   
    03  ACTSSN1I        PIC X(3).   
    03  ACTSSN2I        PIC X(2).   
    03  ACTSSN3I        PIC X(4).   
    03  DOBYEARI        PIC X(4).   
    03  DOBMONI         PIC X(2).   
    03  DOBDAYI         PIC X(2).   
    03  ACSTFCOI        PIC X(3).   
    03  ACSFNAMI        PIC X(25).  
    03  ACSMNAMI        PIC X(25).  
    03  ACSLNAMI        PIC X(25).  
    03  ACSADL1I        PIC X(50).  
    03  ACSSTTEI        PIC X(2).   
    03  ACSADL2I        PIC X(50).  
    03  ACSZIPCI        PIC X(5).   
    03  ACSCITYI        PIC X(50).  
    03  ACSCTRY I       PIC X(3).   
    03  ACSPH1AI        PIC X(3).   
    03  ACSPH1BI        PIC X(3).   
    03  ACSPH1CI        PIC X(4).   
    03  ACSGOVTI        PIC X(20).  
    03  ACSPH2AI        PIC X(3).   
    03  ACSPH2BI        PIC X(3).   
    03  ACSPH2CI        PIC X(4).   
```

**Output Area (CACTUPAO):**
Similar structure with 'O' suffix fields for output, plus attribute bytes for each field.

### 7.2 Account Master Record (Copybook CVACT01Y)

```cobol
01  ACCOUNT-RECORD.
    03  ACCT-ID                     PIC X(11).
    03  ACCT-ACTIVE-STATUS          PIC X(1).
    03  ACCT-CURR-BAL               PIC S9(10)V99 COMP-3.
    03  ACCT-CREDIT-LIMIT           PIC S9(10)V99 COMP-3.
    03  ACCT-CASH-CREDIT-LIMIT      PIC S9(10)V99 COMP-3.
    03  ACCT-OPEN-DATE              PIC X(10).
    03  ACCT-EXPIRATION-DATE        PIC X(10).
    03  ACCT-REISSUE-DATE           PIC X(10).
    03  ACCT-CURR-CYC-CREDIT        PIC S9(10)V99 COMP-3.
    03  ACCT-CURR-CYC-DEBIT         PIC S9(10)V99 COMP-3.
    03  ACCT-GROUP-ID               PIC X(10).
    03  [Additional fields...]
```

**Migration Considerations:**
- COMP-3 (packed decimal) fields need conversion to appropriate cloud database numeric types
- Date fields in format CCYY-MM-DD need date type mapping
- Fixed-length character fields may benefit from VARCHAR in cloud databases

### 7.3 Customer Master Record (Copybook CVCUS01Y)

```cobol
01  CUSTOMER-RECORD.
    03  CUST-ID                     PIC X(10).
    03  CUST-FIRST-NAME             PIC X(25).
    03  CUST-MIDDLE-NAME            PIC X(25).
    03  CUST-LAST-NAME              PIC X(25).
    03  CUST-ADDR-LINE-1            PIC X(50).
    03  CUST-ADDR-LINE-2            PIC X(50).
    03  CUST-ADDR-CITY              PIC X(50).
    03  CUST-ADDR-STATE             PIC X(2).
    03  CUST-ADDR-ZIP               PIC X(5).
    03  CUST-ADDR-COUNTRY           PIC X(3).
    03  CUST-PHONE-NUM-1            PIC X(10).
    03  CUST-PHONE-NUM-2            PIC X(10).
    03  CUST-SSN                    PIC X(9).
    03  CUST-DOB-YYYY-MM-DD         PIC X(10).
    03  CUST-FICO-CREDIT-SCORE      PIC 9(3).
    03  CUST-GOVT-ISSUED-ID         PIC X(20).
    03  [Additional fields...]
```

**Migration Considerations:**
- SSN should be encrypted/hashed in cloud environments
- Phone numbers stored as 10-digit strings, may need formatting
- FICO score is numeric 300-850 range

### 7.4 Card Cross-Reference Record (Copybook CVACT03Y)

```cobol
01  CARD-XREF-RECORD.
    03  CARD-XREF-CARD-NUM          PIC X(16).
    03  CARD-XREF-CUST-ID           PIC X(10).
    03  CARD-XREF-ACCT-ID           PIC X(11).
    03  [Additional fields...]
```

**Purpose:** Links card numbers, customer IDs, and account IDs. This program uses the alternate index CXACAIX to find customer ID from account ID.

### 7.5 Application COMMAREA (Copybook COCOM01Y)

```cobol
01  CARDDEMO-COMMAREA.
    03  CDEMO-FROM-TRANID           PIC X(4).
    03  CDEMO-FROM-PROGRAM          PIC X(8).
    03  CDEMO-TO-TRANID             PIC X(4).
    03  CDEMO-TO-PROGRAM            PIC X(8).
    03  CDEMO-CUST-ID               PIC X(10).
    03  CDEMO-ACCT-ID               PIC X(11).
    03  CDEMO-CARD-NUM              PIC X(16).
    03  CDEMO-LAST-MAP              PIC X(7).
    03  CDEMO-LAST-MAPSET           PIC X(7).
    03  CCARD-AID-xxx               [PF key flags]
    03  CCARD-ERROR-MSG             PIC X(80).
    03  CDEMO-PGM-CONTEXT           [State flags]
    03  [Additional navigation fields...]
```

**Purpose:** Maintains application state across pseudo-conversational iterations, stores navigation context, error messages, and PF key states.

### 7.6 Program-Specific COMMAREA (WS-THIS-PROGCOMMAREA)

```cobol
01  WS-THIS-PROGCOMMAREA.
    05  ACCT-UPDATE-SCREEN-DATA.
        10  ACUP-PROCESSING-STATE   [State indicators]
        10  ACUP-OLD-DETAILS        [Original fetched data]
            15  ACUP-OLD-ACCOUNT-RECORD.
            15  ACUP-OLD-CUSTOMER-RECORD.
        10  ACUP-NEW-DETAILS        [User-modified data]
            15  ACUP-NEW-ACCT-ID-X  PIC X(11).
            15  ACUP-NEW-ACTIVE-STATUS PIC X(1).
            15  [All modifiable fields...]
```

**Purpose:** Stores both original (old) and modified (new) versions of all data for comparison and concurrent update detection.

---

## 8. Dependencies

### 8.1 External Program Dependencies

**COMEN01C - Main Menu Program**
- **Invocation:** Via EXEC CICS XCTL when user presses F3 and no calling program specified
- **Purpose:** CardDemo application main menu
- **Communication:** Passes CARDDEMO-COMMAREA with navigation context
- **Return Path:** COMEN01C can invoke COACTUPC again with account ID pre-populated

### 8.2 File Dependencies

#### ACCTDAT - Account Master File
- **File Type:** VSAM KSDS (Key Sequenced Data Set)
- **Access Method:** Direct keyed access
- **Primary Key:** ACCT-ID (11 bytes)
- **Operations:** READ, READ UPDATE, REWRITE
- **Record Layout:** CVACT01Y copybook
- **Usage:** 
  - Initial read to display account data
  - Read update and rewrite for modifications
- **Locking:** Exclusive lock during update (READ UPDATE)

#### CUSTDAT - Customer Master File
- **File Type:** VSAM KSDS
- **Access Method:** Direct keyed access
- **Primary Key:** CUST-ID (10 bytes)
- **Operations:** READ, READ UPDATE, REWRITE
- **Record Layout:** CVCUS01Y copybook
- **Usage:**
  - Initial read to display customer data
  - Read update and rewrite for modifications
- **Locking:** Exclusive lock during update (READ UPDATE)

#### CARDDAT - Card Cross-Reference File
- **File Type:** VSAM KSDS with Alternate Index
- **Access Method:** 
  - Primary: CARD-NUM key
  - Alternate Index (CXACAIX): ACCT-ID key
- **Operations:** READ (via alternate index only)
- **Record Layout:** CVACT03Y copybook
- **Usage:** Lookup customer ID from account ID
- **Alternate Index:** CXACAIX provides ACCT-ID → CUST-ID mapping

### 8.3 Copybook Dependencies

**Critical Copybooks for Migration:**

1. **CVACT01Y** - Account record structure
   - Contains all account field definitions
   - Defines packed decimal formats
   - Critical for data migration mapping

2. **CVCUS01Y** - Customer record structure
   - Contains all customer field definitions
   - Defines PII fields (SSN, DOB)
   - Critical for data migration and security

3. **COCOM01Y** - Application COMMAREA
   - Inter-program communication structure
   - Navigation state management
   - Would need session management equivalent in cloud

4. **CSUTLDWY** - Date utility structures
   - Date validation routines
   - Format: CCYYMMDD
   - Cloud equivalent: Date/datetime types

5. **CSLKPCDY** - Phone area code lookup
   - North American area code validation
   - Reference data table in cloud

### 8.4 Transaction Dependencies

**CAUP - Account Update Transaction**
- **Program:** COACTUPC
- **Transaction Type:** Pseudo-conversational
- **Security:** Requires user authentication (CSUSR01Y copybook)
- **Prerequisites:** User must be signed on to CardDemo system

### 8.5 No Database Dependencies

**Important Note:** This program does NOT use EXEC SQL. All data access is through VSAM files via CICS file control commands. For cloud migration:
- VSAM files → Relational database tables
- EXEC CICS READ/REWRITE → SQL SELECT/UPDATE
- CICS file locking → Database transaction management
- COMMAREA persistence → Session state management

---

## 9. Error Handling and Recovery

### 9.1 CICS Error Handling

**Response Code Checking Pattern:**

Every CICS command includes RESP and RESP2 parameters:
```cobol
EXEC CICS [COMMAND]
     [parameters...]
     RESP(WS-RESP-CD)
     RESP2(WS-REAS-CD)
END-EXEC

EVALUATE WS-RESP-CD
    WHEN DFHRESP(NORMAL)
        [Success path]
    WHEN DFHRESP(NOTFND)
        [Record not found handling]
    WHEN DFHRESP(MAPFAIL)
        [Map receive failed]
    WHEN OTHER
        [General error handling]
END-EVALUATE
```

### 9.2 File I/O Error Conditions

#### NOTFND (Record Not Found)
- **Scenarios:**
  - Account ID not in CARDDAT (card xref file)
  - Account ID not in ACCTDAT
  - Customer ID not in CUSTDAT
- **Handling:**
  - Display specific error message
  - Return to entry screen
  - Preserve user's input for correction

#### LOCKED (Resource Busy)
- **Scenarios:**
  - Another user has account record locked
  - Another user has customer record locked
- **Handling:**
  - Display "Could not lock record" message
  - Advise user to try again later
  - Do not proceed with update

#### IOERR (I/O Error)
- **Scenarios:**
  - File not available
  - Disk error
  - File corruption
- **Handling:**
  - Display generic error message
  - Log technical details (RESP, RESP2)
  - Recommend contacting support

### 9.3 Data Validation Errors

**Validation Error Pattern:**
```cobol
IF [validation fails]
    SET INPUT-ERROR TO TRUE
    SET FLG-FIELD-NOT-OK TO TRUE
    MOVE 'Specific error message' TO WS-RETURN-MSG
    GO TO [PARAGRAPH]-EXIT
END-IF
```

**Common Validation Errors:**

1. **Required Field Missing:**
   - Error: "Field xxx is required"
   - Action: Highlight field in RED, position cursor

2. **Invalid Data Type:**
   - Error: "Field xxx must be numeric"
   - Action: Highlight field in RED, position cursor

3. **Invalid Date:**
   - Error: "Invalid date format or value"
   - Action: Highlight date components, explain requirements

4. **Out of Range:**
   - Error: "FICO score must be 300-850"
   - Action: Highlight field, show valid range

5. **Invalid Format:**
   - Error: "SSN must be 9 digits"
   - Action: Highlight field, show format example

### 9.4 Business Logic Errors

**No Changes Detected:**
```cobol
IF NO-CHANGES-FOUND
    MOVE 'No changes detected' TO WS-RETURN-MSG
    [Display message, keep data on screen]
END-IF
```

**Concurrent Update Detected:**
```cobol
IF WS-LOCKED-BUT-CHANGED
    MOVE 'Data changed by another user' TO WS-RETURN-MSG
    [Re-fetch current data]
    [Display fresh data with message]
    [User must re-enter changes]
END-IF
```

### 9.5 Update Transaction Errors

**Account Update Failed:**
- **Scenario:** REWRITE of ACCTDAT fails
- **Handling:**
  - Set LOCKED-BUT-UPDATE-FAILED flag
  - Display "Account update failed" message
  - Lock is released automatically
  - User can retry

**Customer Update Failed After Account Update Succeeded:**
- **Scenario:** REWRITE of CUSTDAT fails after ACCTDAT succeeded
- **Critical Handling:**
  ```cobol
  EXEC CICS SYNCPOINT ROLLBACK END-EXEC
  ```
- **Effect:** Undoes account update to maintain data integrity
- **Message:** "Update failed - all changes rolled back"
- **Recovery:** User must retry entire update

### 9.6 Abend Handling

**EXEC CICS HANDLE ABEND:**
```cobol
EXEC CICS HANDLE ABEND
          LABEL(ABEND-ROUTINE)
END-EXEC
```

**Abend Routine Actions:**
1. Capture abend code (EIBRESP)
2. Capture function code (EIBFN)
3. Identify culprit program (LIT-THISPGM)
4. Format abend data structure
5. Send formatted abend message to terminal:
   ```
   ABEND OCCURRED
   Program: COACTUPC
   RESP Code: [code]
   RESP2 Code: [code]
   Function: [function]
   ```
6. Execute EXEC CICS RETURN to terminate

**Abend Recovery:**
- Terminal displays diagnostic information
- User informed of system error
- All locks automatically released by CICS
- Transaction can be reinitiated
- Data integrity maintained (no partial updates)

### 9.7 Map Handling Errors

**MAPFAIL Condition:**
- **Scenario:** RECEIVE MAP when no data modified
- **Typical Cause:** User pressed ENTER without changing anything after initial display
- **Handling:**
  - Treat as valid input
  - Re-display current screen
  - No error message needed

**Screen Transmission Errors:**
- **Scenario:** SEND MAP fails
- **Handling:**
  - Capture RESP code
  - Log error details
  - Attempt recovery or terminate gracefully

### 9.8 Error Message Display

**Message Area:**
- Location: Bottom of screen (line 22-23 typically)
- Storage: CCARD-ERROR-MSG in COMMAREA
- Length: 80 bytes
- Attribute: RED HILIGHT for errors, YELLOW for information

**Message Persistence:**
- Messages stored in COMMAREA
- Survive pseudo-conversational iterations
- Cleared when appropriate (new search, successful update)

### 9.9 Field-Level Error Indication

**Error Field Attributes:**
```cobol
MOVE DFHBMPRO TO [FIELD]A    [Protected]
MOVE DFHRED   TO [FIELD]C    [Red color]
MOVE DFHBLINK TO [FIELD]H    [Blinking or highlight]
MOVE -1       TO [FIELD]L    [Cursor position]
```

**Changed Field Attributes:**
```cobol
MOVE DFHUNPRO TO [FIELD]A    [Unprotected]
MOVE DFHGREEN TO [FIELD]C    [Green color]
MOVE DFHULINE TO [FIELD]H    [Underline]
```

### 9.10 Recovery Procedures

**User Recovery Steps:**
1. For validation errors: Correct field and press ENTER
2. For not found errors: Verify account number and retry
3. For concurrent update: Review fresh data, re-enter changes
4. For system errors: Note error details, contact support, retry transaction

**System Recovery:**
- CICS automatically releases locks on transaction end
- Pseudo-conversational design prevents resource exhaustion
- SYNCPOINT ROLLBACK ensures atomic transactions
- Abend handler prevents data corruption

---

## 10. Additional Technical Details

### 10.1 Pseudo-Conversational Design

**Concept:** Between user interactions, the program terminates and releases all resources, preserving state in COMMAREA.

**Implementation:**
```cobol
EXEC CICS RETURN
     TRANSID (LIT-THISTRANID)  
     COMMAREA (WS-COMMAREA)
     LENGTH(LENGTH OF WS-COMMAREA)
END-EXEC
```

**Benefits:**
- Efficient resource utilization (no resources held while user thinks)
- Scalability (supports many concurrent users)
- Automatic timeout handling by CICS
- Reduced memory footprint

**State Preservation:**
- COMMAREA contains: Application context (2000 bytes)
- Program-specific data (fetched account/customer records)
- User modifications (new values)
- Processing state flags
- Navigation history

**Restart Mechanism:**
When user presses any key:
1. CICS reinvokes transaction CAUP
2. Passes saved COMMAREA to program
3. Program examines EIBCALEN to detect restart
4. Restores state from COMMAREA
5. Processes user input
6. Saves updated state to COMMAREA
7. Returns to CICS (cycle repeats)

### 10.2 Transaction Control and Atomicity

**SYNCPOINT Command:**
```cobol
EXEC CICS SYNCPOINT END-EXEC
```
- **Purpose:** Commits all file updates
- **Effect:** Makes changes permanent, releases all locks
- **Usage:** Before XCTL when exiting program

**SYNCPOINT ROLLBACK Command:**
```cobol
EXEC CICS SYNCPOINT ROLLBACK END-EXEC
```
- **Purpose:** Undo all file updates since last SYNCPOINT
- **Effect:** Restores original data, releases locks
- **Usage:** When customer update fails after account update succeeded

**Implicit SYNCPOINT:**
- Occurs automatically when program terminates normally (EXEC CICS RETURN without TRANSID)
- Not used in this pseudo-conversational program

**Transaction Boundaries:**
- Each user interaction is a separate transaction
- Data fetching (READ): Separate transaction, no locks held
- Data updating (READ UPDATE + REWRITE): Single atomic transaction
- Locks acquired during READ UPDATE, released after REWRITE or ROLLBACK

### 10.3 Concurrent Update Detection

**Pattern:**
```cobol
[READ data and store in ACUP-OLD-xxx fields]
[User modifies data, stored in ACUP-NEW-xxx fields]
[READ UPDATE to lock records]
[Compare current file data with ACUP-OLD-xxx]
IF data-unchanged
    [Proceed with REWRITE using ACUP-NEW-xxx values]
ELSE
    [Display "concurrent update" message]
    [Store fresh data in ACUP-OLD-xxx]
    [Redisplay screen with fresh data]
END-IF
```

**Why Needed:**
- Time gap between initial read and update attempt
- Another user might update same data in the gap
- Prevents lost updates (User A's changes overwriting User B's changes)

**Implementation:**
```cobol
IF  ACCOUNT-RECORD = ACUP-OLD-ACCOUNT-RECORD
AND CUSTOMER-RECORD = ACUP-OLD-CUSTOMER-RECORD
    SET WS-LOCKED-NOT-CHANGED TO TRUE
ELSE
    SET WS-LOCKED-BUT-CHANGED TO TRUE
END-IF
```

**Cloud Migration Consideration:**
- COBOL record comparison → Database timestamp/version columns
- Optimistic locking pattern
- WHERE clause with original values

### 10.4 Date and Time Handling

**Date Utilities (CSUTLDWY copybook):**
- Format: CCYYMMDD (8 bytes character)
- Validates year, month, day ranges
- Leap year handling
- Future/past date checks

**Date Validation Pattern:**
```cobol
MOVE date-year  TO WS-EDIT-YEAR
MOVE date-month TO WS-EDIT-MONTH
MOVE date-day   TO WS-EDIT-DAY
PERFORM 1240-EDIT-DATE-CCYYMMDD
IF WS-EDIT-DATE-IS-INVALID
    [Error handling]
END-IF
```

**Special Date Validations:**
- Open Date: Must be in past or today
- Expiry Date: Must be in future
- Reissue Date: Valid format
- Date of Birth: Must be in past, age restrictions (18+ typically)

**Current Date/Time:**
- Retrieved from CSDAT01Y copybook
- Formatted for display on screen header
- Used for date arithmetic and validations

### 10.5 BMS Attribute Management

**Dynamic Attribute Setting:**
Program uses COPY REPLACING to generate repetitive attribute-setting code:
```cobol
COPY CSSETATY REPLACING
  ==(TESTVAR1)== BY ==ACCT-STATUS==
  ==(SCRNVAR2)== BY ==ACSTTUS==
  ==(MAPNAME3)== BY ==CACTUPA== .
```

This expands to generated code that sets:
- ACSTTUSA (attribute byte): PROT/UNPROT, IC, FSET
- ACSTTUSH (highlight byte): NORM/BLINK
- ACSTTUSC (color byte): DEFAULT/RED/GREEN/YELLOW

**Attribute Types:**
- **DFHBMPRO:** Protected (user cannot modify)
- **DFHUNPRO:** Unprotected (user can modify)
- **DFHBMASK:** Askip (skip field, no cursor stop)
- **DFHBMFSE:** FSET (modified data tag)
- **DFHIC:** Initial cursor position

**Color Codes:**
- **DFHGREEN:** Normal field or changed field
- **DFHRED:** Error field
- **DFHYELLOW:** Informational
- **DFHTURQ:** Labels and prompts

**Extended Attributes:**
- Underline for input fields
- Highlight for emphasis
- Reverse video for emphasis

### 10.6 COMMAREA Structure and Size

**Total COMMAREA Size:** ~2000 bytes
- CARDDEMO-COMMAREA: ~300 bytes (navigation, state, keys)
- WS-THIS-PROGCOMMAREA: ~1700 bytes (account/customer data, old/new copies)

**Why So Large:**
- Stores complete account record (~400 bytes)
- Stores complete customer record (~600 bytes)
- Stores both OLD (original) and NEW (modified) copies for comparison
- Stores processing state flags
- Stores error messages and navigation context

**COMMAREA Limit:** CICS typically allows up to 32KB COMMAREA
- This program uses ~6% of max size
- Well within limits for pseudo-conversational design

### 10.7 Security Considerations

**User Authentication:**
- Program assumes user is authenticated (CSUSR01Y copybook)
- No explicit authentication in this program
- Security handled by CICS transaction security

**Data Access:**
- No field-level security in this program
- All users with CAUP access can modify all fields
- PII (SSN, DOB) not masked on screen

**Audit Trail:**
- No explicit audit logging in this program
- File updates create implicit audit via file management
- Cloud migration should add comprehensive audit logging

**Cloud Migration Security Enhancements:**
- Add field-level access control
- Implement data masking for PII
- Add comprehensive audit logging
- Encrypt sensitive fields (SSN) at rest and in transit
- Implement row-level security in database

### 10.8 Performance Considerations

**File Access Pattern:**
- Initial display: 3 file reads (card xref, account, customer)
- Update: 2 file reads with UPDATE, 2 file rewrites
- No sequential processing (all direct keyed access)

**Locking Strategy:**
- Locks acquired only during update transaction
- Lock duration: milliseconds (READ UPDATE to REWRITE)
- Minimal contention potential

**Screen I/O:**
- DATAONLY option reduces data transmission
- Only changed fields transmitted
- Cursor positioning reduces user navigation

**Optimization for Cloud:**
- Combine 3 reads into single join query
- Use prepared statements
- Implement connection pooling
- Cache reference data (area codes, state codes)
- Use optimistic locking with timestamps

### 10.9 Migration Considerations

**CICS → Cloud Web Application:**
1. **Terminal I/O → Web UI:**
   - BMS map → HTML form
   - SEND MAP → HTTP response
   - RECEIVE MAP → HTTP POST

2. **Pseudo-Conversational → Stateless:**
   - COMMAREA → Session state (Redis, database)
   - Transaction restart → HTTP request handling
   - State preservation automatic in HTTP session

3. **File I/O → Database:**
   - EXEC CICS READ → SQL SELECT
   - EXEC CICS REWRITE → SQL UPDATE
   - VSAM keys → Database primary keys
   - Alternate index → Database foreign key with index

4. **Transaction Control:**
   - SYNCPOINT → COMMIT
   - SYNCPOINT ROLLBACK → ROLLBACK
   - READ UPDATE locking → SELECT FOR UPDATE

5. **Error Handling:**
   - RESP codes → Exception handling
   - HANDLE ABEND → try-catch blocks
   - Screen error display → Web form validation

6. **Program Flow:**
   - EVALUATE → switch or if-else
   - PERFORM → function calls
   - GO TO → structured control flow

### 10.10 Testing Recommendations

**Unit Testing:**
- Test each validation paragraph independently
- Test date validation with edge cases (leap years, etc.)
- Test numeric field handling (negative, decimal, overflow)
- Test concurrent update detection

**Integration Testing:**
- Test full create-read-update cycle
- Test with locked records
- Test with missing records (not found scenarios)
- Test rollback scenarios

**User Acceptance Testing:**
- Verify all function keys work correctly
- Verify error messages are clear and accurate
- Verify screen layout matches requirements
- Verify data integrity after updates

**Performance Testing:**
- Measure response time under load
- Test with multiple concurrent users
- Verify locking behavior
- Monitor resource utilization

**Security Testing:**
- Verify authentication requirements
- Test for SQL injection (in cloud version)
- Verify audit logging
- Test data encryption

---

## Summary

COACTUPC is a well-structured CICS pseudo-conversational online transaction program that enables authorized users to update credit card account and customer master data. The program demonstrates best practices in CICS programming including proper transaction control, concurrent update detection, comprehensive input validation, and graceful error handling. Its design is suitable for cloud modernization with appropriate adaptations for web UI, relational databases, and modern transaction management patterns.

**Key Strengths:**
- Robust error handling at all levels
- Atomic transaction processing with rollback capability
- Comprehensive input validation
- Concurrent update detection
- Clean separation of concerns (fetch, validate, update)
- Pseudo-conversational efficiency

**Migration Priorities:**
1. Preserve business logic and validation rules
2. Implement equivalent transaction atomicity
3. Maintain data integrity checks
4. Enhance security (PII protection, audit logging)
5. Optimize database access patterns
6. Modernize user interface while preserving workflow
