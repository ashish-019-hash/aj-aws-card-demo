# CardDemo Application - Program Call Hierarchy Analysis

## Document Information

**Application Name:** CardDemo (AWS Mainframe Modernization CardDemo)  
**Analysis Date:** October 16, 2025  
**Analyzed By:** Devin AI  
**Source Documents:** 3 COBOL program extraction documents in 01.phase-1-output folder  
**Repository:** ashish-019-hash/aj-aws-card-demo  
**Devin Session:** https://app.devin.ai/sessions/9500ef6db7e2471ab49e97a14ae0f6ee  
**Requested By:** @ajpulikken

---

## 1. Program Inventory Table

| Program ID | Program Type | File Path | Called By Count | Calls Count | Classification |
|------------|--------------|-----------|-----------------|-------------|----------------|
| CBACT01C | Batch | /home/ubuntu/repos/aj-aws-card-demo/00.phase-1-input/app/cbl/CBACT01C.cbl | 0 | 2 | Batch Entry Point |
| COACTUPC | Online Transaction | /home/ubuntu/repos/aj-aws-card-demo/00.phase-1-input/app/cbl/COACTUPC.cbl | 0 | 1 | CICS Entry Point (CAUP) |
| COACTVWC | Online Transaction | /home/ubuntu/repos/aj-aws-card-demo/00.phase-1-input/app/cbl/COACTVWC.cbl | 0 | 1 | CICS Entry Point (CAVW) |
| COBDATFT | External Assembler | N/A - External Program | 1 | 0 | External Utility (Unresolved) |
| CEE3ABD | System Service | N/A - System Service | 1 | 0 | System Service (Unresolved) |
| COMEN01C | Main Menu | N/A - Not in extraction documents | 2 | 0 | Menu Program (Unresolved) |

**Notes:**
- **Unresolved** indicates programs that are called but do not have extraction documents
- All three programs in extraction documents (CBACT01C, COACTUPC, COACTVWC) are entry points with depth 0
- File paths shown are for COBOL source files in the repository

---

## 2. Detailed Call Relationships Table

| Calling Program | Called Program | Call Type | Call Condition | Parameters Passed | Purpose | Source Reference |
|-----------------|----------------|-----------|----------------|-------------------|---------|------------------|
| CBACT01C | COBDATFT | CALL | Always (per record) | CODATECN-REC structure: CODATECN-TYPE='2', CODATECN-OUTTYPE='2', CODATECN-INP-DATE=ACCT-REISSUE-DATE (input), CODATECN-OUT-DATE (output) | Convert date from YYYY-MM-DD to YYYYMMDD format | CBACT01_extract.md, Line 231 |
| CBACT01C | CEE3ABD | CALL | On any file error | ABCODE (S9(9) BINARY value=999), TIMING (S9(9) BINARY value=0) | Terminate program abnormally with abend code 999 | CBACT01_extract.md, Line 410 |
| COACTUPC | COMEN01C | XCTL | When F3 pressed (exit) | CARDDEMO-COMMAREA (2000 bytes): CDEMO-USER-ID, CDEMO-USER-TYPE, CDEMO-FROM-PROGRAM, CDEMO-FROM-TRANID, CDEMO-TO-PROGRAM, CDEMO-TO-TRANID, CDEMO-ACCT-ID, CDEMO-CUST-ID | Return to main menu or calling program after committing changes via SYNCPOINT | COACTUPC_extract.md, Lines 927-959 |
| COACTVWC | COMEN01C | XCTL | When F3 pressed (exit) | CARDDEMO-COMMAREA (2000 bytes): CDEMO-USER-ID, CDEMO-USER-TYPE, CDEMO-FROM-PROGRAM, CDEMO-FROM-TRANID, CDEMO-TO-PROGRAM, CDEMO-TO-TRANID, CDEMO-ACCT-ID, CDEMO-CUST-ID | Return to main menu or calling program (read-only, no SYNCPOINT) | COACTVWC_extract.md, Lines 324-352 |

**Call Relationship Details:**

### CBACT01C → COBDATFT (Date Conversion)
- **Call Type:** Static CALL to external assembler program
- **Frequency:** Once per input record processed (high frequency)
- **Parameter Structure:**
  - Input: CODATECN-TYPE='2' (YYYY-MM-DD format), CODATECN-OUTTYPE='2' (YYYYMMDD format), CODATECN-INP-DATE (date to convert)
  - Output: CODATECN-OUT-DATE (converted date)
- **Business Purpose:** Reformat account reissue dates for output files
- **Performance Impact:** Called in loop, adds overhead to batch processing

### CBACT01C → CEE3ABD (Error Handling)
- **Call Type:** Static CALL to IBM Language Environment service
- **Frequency:** Only on error conditions (file open/read/write/close failures)
- **Parameters:**
  - ABCODE=999 (abend completion code S999)
  - TIMING=0 (immediate termination, no dump suppression)
- **Business Purpose:** Ensure batch job fails immediately on any file error
- **Effect:** Program terminates with S999 abend code

### COACTUPC → COMEN01C (Navigation)
- **Call Type:** EXEC CICS XCTL (transfer control to another program)
- **Frequency:** Once per user session when F3 is pressed
- **COMMAREA Contents:**
  - User session: CDEMO-USER-ID, CDEMO-USER-TYPE
  - Navigation context: CDEMO-FROM-PROGRAM, CDEMO-FROM-TRANID, CDEMO-TO-PROGRAM, CDEMO-TO-TRANID
  - Business context: CDEMO-ACCT-ID, CDEMO-CUST-ID
- **Pre-Transfer Actions:** Issues EXEC CICS SYNCPOINT to commit all file updates
- **Navigation Logic:** Returns to calling program if CDEMO-FROM-PROGRAM is set, otherwise defaults to COMEN01C main menu

### COACTVWC → COMEN01C (Navigation)
- **Call Type:** EXEC CICS XCTL (transfer control to another program)
- **Frequency:** Once per user session when F3 is pressed
- **COMMAREA Contents:** Same structure as COACTUPC (user session, navigation context, business context)
- **Pre-Transfer Actions:** Updates navigation context only (no SYNCPOINT needed as this is read-only)
- **Navigation Logic:** Same as COACTUPC - returns to calling program or main menu

---

## 3. Complete Call Hierarchy Tree

```
APPLICATION: CardDemo (AWS Mainframe Modernization CardDemo)

═══════════════════════════════════════════════════════════════════════════

ENTRY POINT 1: CBACT01C [Batch Processing - Account Data Transformation]
File: /home/ubuntu/repos/aj-aws-card-demo/00.phase-1-input/app/cbl/CBACT01C.cbl
Level 0: CBACT01C [Batch Entry Point]
  │
  ├─ Level 1: COBDATFT [Date Format Conversion - Assembler Utility]
  │   ├─ ** EXTERNAL UTILITY - Not in extraction documents **
  │   ├─ Call Type: CALL (static)
  │   ├─ Parameters: CODATECN-REC structure (input/output)
  │   ├─ Purpose: Convert dates from YYYY-MM-DD to YYYYMMDD format
  │   └─ Frequency: Per record (high frequency)
  │
  └─ Level 1: CEE3ABD [Program Abend Handler - System Service]
      ├─ ** SYSTEM SERVICE - Not in extraction documents **
      ├─ Call Type: CALL (static)
      ├─ Parameters: ABCODE=999, TIMING=0
      ├─ Purpose: Terminate program with S999 abend code on errors
      └─ Frequency: On error only (low frequency)

═══════════════════════════════════════════════════════════════════════════

ENTRY POINT 2: COACTUPC [Online Transaction - CAUP - Account Update]
File: /home/ubuntu/repos/aj-aws-card-demo/00.phase-1-input/app/cbl/COACTUPC.cbl
Level 0: COACTUPC [CICS Transaction Entry Point]
  │
  └─ Level 1: COMEN01C [Main Menu Program]
      ├─ ** UNRESOLVED - Not in extraction documents **
      ├─ Transfer Type: EXEC CICS XCTL
      ├─ Parameters: CARDDEMO-COMMAREA (2000 bytes)
      ├─ Trigger: User presses F3 (Exit key)
      ├─ Pre-Transfer: EXEC CICS SYNCPOINT (commit changes)
      ├─ Purpose: Return to application main menu or calling program
      └─ Frequency: Per user session exit (medium frequency)

═══════════════════════════════════════════════════════════════════════════

ENTRY POINT 3: COACTVWC [Online Transaction - CAVW - Account View]
File: /home/ubuntu/repos/aj-aws-card-demo/00.phase-1-input/app/cbl/COACTVWC.cbl
Level 0: COACTVWC [CICS Transaction Entry Point]
  │
  └─ Level 1: COMEN01C [Main Menu Program]
      ├─ ** UNRESOLVED - Not in extraction documents **
      ├─ Transfer Type: EXEC CICS XCTL
      ├─ Parameters: CARDDEMO-COMMAREA (2000 bytes)
      ├─ Trigger: User presses F3 (Exit key)
      ├─ Pre-Transfer: Update navigation context (no SYNCPOINT - read-only)
      ├─ Purpose: Return to application main menu or calling program
      └─ Frequency: Per user session exit (medium frequency)

═══════════════════════════════════════════════════════════════════════════
```

**Hierarchy Summary:**
- **Total Programs Analyzed:** 3 (CBACT01C, COACTUPC, COACTVWC)
- **Total Call Relationships:** 4 (2 CALL statements, 2 XCTL statements)
- **Maximum Call Depth:** 1 level (flat hierarchy)
- **Entry Points:** All 3 programs are independent entry points
- **External Dependencies:** 3 unresolved programs (COBDATFT, CEE3ABD, COMEN01C)
- **Circular Dependencies:** None
- **Call Pattern:** Simple one-level calls from entry points to external programs/services

**Architecture Characteristics:**
- **Independent Entry Points**: All three programs operate independently and are not called by other COBOL programs
- **Flat Call Structure**: Maximum depth of 1 indicates simple, maintainable architecture
- **Minimal Inter-Dependencies**: No direct calls between the three main programs
- **Shared Navigation**: Both online programs use COMEN01C for menu navigation
- **External Utilities**: Batch program relies on external assembler utility for date conversion
