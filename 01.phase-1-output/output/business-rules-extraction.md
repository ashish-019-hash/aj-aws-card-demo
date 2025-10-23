# CardDemo COBOL Business Rules Extraction

## Overview
This document contains business-level calculation rules extracted from the CardDemo COBOL codebase. Only business logic related to financial calculations, credit processing, payments, and transactions are included. Technical infrastructure, file handling, screen processing, and validation logic are excluded.

## Summary Statistics
- **Total COBOL Files Analyzed**: 18/18
- **Total Business Rules Identified**: 9
- **Analysis Status**: COMPLETED

## Business Rules by Category

### Credit Limit Processing
*Rules related to credit limit calculations and validations*
- RULE-CALC-001: Credit Limit Conversion and Validation
- RULE-CALC-002: Cash Credit Limit Conversion and Validation

### Payment Processing  
*Rules related to bill payments and payment calculations*
- RULE-CALC-006: Transaction ID Generation for Bill Payments
- RULE-CALC-007: Bill Payment Balance Update Calculation

### Transaction Processing
*Rules related to transaction amount processing and calculations*
- RULE-CALC-008: Account ID Numeric Conversion for Transaction Processing
- RULE-CALC-009: Card Number Numeric Conversion for Transaction Processing

### Balance Calculations
*Rules related to account balance updates and computations*
- RULE-CALC-003: Current Balance Conversion and Validation

### Cycle Processing
*Rules related to current cycle credit and debit calculations*
- RULE-CALC-004: Current Cycle Credit Conversion and Validation
- RULE-CALC-005: Current Cycle Debit Conversion and Validation

---

## Detailed Business Rules

### RULE-CALC-001: Credit Limit Conversion and Validation
**Rule Description**: Converts user input credit limit from character format to numeric format for account processing
**COBOL Source Location**: COACTUPC.cbl, lines 1072-1084
**Involved Variables**: 
- Input: `ACRDLIMI OF CACTUPAI` (character input)
- Output: `ACUP-NEW-CREDIT-LIMIT-N` (numeric value)
- Working: `ACUP-NEW-CREDIT-LIMIT-X` (character working storage)
**Input Conditions**: 
- IF `ACRDLIMI OF CACTUPAI = '*'` OR `ACRDLIMI OF CACTUPAI = SPACES` THEN set to LOW-VALUES
- IF `FUNCTION TEST-NUMVAL-C(ACUP-NEW-CREDIT-LIMIT-X) = 0` THEN convert to numeric
**Calculation Logic**: 
```
IF credit_limit_input is not blank or asterisk THEN
    COMPUTE numeric_credit_limit = FUNCTION NUMVAL-C(credit_limit_input)
ELSE
    SET credit_limit to LOW-VALUES
END-IF
```

### RULE-CALC-002: Cash Credit Limit Conversion and Validation
**Rule Description**: Converts user input cash credit limit from character format to numeric format for cash advance processing
**COBOL Source Location**: COACTUPC.cbl, lines 1086-1098
**Involved Variables**: 
- Input: `ACSHLIMI OF CACTUPAI` (character input)
- Output: `ACUP-NEW-CASH-CREDIT-LIMIT-N` (numeric value)
- Working: `ACUP-NEW-CASH-CREDIT-LIMIT-X` (character working storage)
**Input Conditions**: 
- IF `ACSHLIMI OF CACTUPAI = '*'` OR `ACSHLIMI OF CACTUPAI = SPACES` THEN set to LOW-VALUES
- IF `FUNCTION TEST-NUMVAL-C(ACUP-NEW-CASH-CREDIT-LIMIT-X) = 0` THEN convert to numeric
**Calculation Logic**: 
```
IF cash_credit_limit_input is not blank or asterisk THEN
    COMPUTE numeric_cash_credit_limit = FUNCTION NUMVAL-C(cash_credit_limit_input)
ELSE
    SET cash_credit_limit to LOW-VALUES
END-IF
```

### RULE-CALC-003: Current Balance Conversion and Validation
**Rule Description**: Converts user input current balance from character format to numeric format for account balance processing
**COBOL Source Location**: COACTUPC.cbl, lines 1100-1112
**Involved Variables**: 
- Input: `ACURBALI OF CACTUPAI` (character input)
- Output: `ACUP-NEW-CURR-BAL-N` (numeric value)
- Working: `ACUP-NEW-CURR-BAL-X` (character working storage)
**Input Conditions**: 
- IF `ACURBALI OF CACTUPAI = '*'` OR `ACURBALI OF CACTUPAI = SPACES` THEN set to LOW-VALUES
- IF `FUNCTION TEST-NUMVAL-C(ACUP-NEW-CURR-BAL-X) = 0` THEN convert to numeric
**Calculation Logic**: 
```
IF current_balance_input is not blank or asterisk THEN
    COMPUTE numeric_current_balance = FUNCTION NUMVAL-C(current_balance_input)
ELSE
    SET current_balance to LOW-VALUES
END-IF
```

### RULE-CALC-004: Current Cycle Credit Conversion and Validation
**Rule Description**: Converts user input current cycle credit from character format to numeric format for billing cycle processing
**COBOL Source Location**: COACTUPC.cbl, lines 1114-1126
**Involved Variables**: 
- Input: `ACRCYCRI OF CACTUPAI` (character input)
- Output: `ACUP-NEW-CURR-CYC-CREDIT-N` (numeric value)
- Working: `ACUP-NEW-CURR-CYC-CREDIT-X` (character working storage)
**Input Conditions**: 
- IF `ACRCYCRI OF CACTUPAI = '*'` OR `ACRCYCRI OF CACTUPAI = SPACES` THEN set to LOW-VALUES
- IF `FUNCTION TEST-NUMVAL-C(ACUP-NEW-CURR-CYC-CREDIT-X) = 0` THEN convert to numeric
**Calculation Logic**: 
```
IF current_cycle_credit_input is not blank or asterisk THEN
    COMPUTE numeric_current_cycle_credit = FUNCTION NUMVAL-C(current_cycle_credit_input)
ELSE
    SET current_cycle_credit to LOW-VALUES
END-IF
```

### RULE-CALC-005: Current Cycle Debit Conversion and Validation
**Rule Description**: Converts user input current cycle debit from character format to numeric format for billing cycle processing
**COBOL Source Location**: COACTUPC.cbl, lines 1128-1140
**Involved Variables**: 
- Input: `ACRCYDBI OF CACTUPAI` (character input)
- Output: `ACUP-NEW-CURR-CYC-DEBIT-N` (numeric value)
- Working: `ACUP-NEW-CURR-CYC-DEBIT-X` (character working storage)
**Input Conditions**: 
- IF `ACRCYDBI OF CACTUPAI = '*'` OR `ACRCYDBI OF CACTUPAI = SPACES` THEN set to LOW-VALUES
- IF `FUNCTION TEST-NUMVAL-C(ACUP-NEW-CURR-CYC-DEBIT-X) = 0` THEN convert to numeric
**Calculation Logic**: 
```
IF current_cycle_debit_input is not blank or asterisk THEN
    COMPUTE numeric_current_cycle_debit = FUNCTION NUMVAL-C(current_cycle_debit_input)
ELSE
    SET current_cycle_debit to LOW-VALUES
END-IF
```

### RULE-CALC-006: Transaction ID Generation for Bill Payments
**Rule Description**: Generates sequential transaction IDs for bill payment transactions by incrementing the last transaction ID
**COBOL Source Location**: COBIL00C.cbl, lines 214-219
**Involved Variables**: 
- Input: `TRAN-ID` (last transaction ID from file)
- Working: `WS-TRAN-ID-NUM` (numeric working storage)
- Output: `WS-TRAN-ID-NUM` (incremented transaction ID)
**Input Conditions**: 
- After reading the previous transaction record from the transaction file
- Before creating a new bill payment transaction record
**Calculation Logic**: 
```
READ last_transaction_record
MOVE transaction_id TO working_transaction_number
ADD 1 TO working_transaction_number
MOVE working_transaction_number TO new_transaction_id
```

### RULE-CALC-007: Bill Payment Balance Update Calculation
**Rule Description**: Updates account current balance by subtracting the full payment amount (current balance) for bill payment processing
**COBOL Source Location**: COBIL00C.cbl, line 234
**Involved Variables**: 
- Input: `ACCT-CURR-BAL` (current account balance)
- Input: `TRAN-AMT` (transaction amount - equals current balance for full payment)
- Output: `ACCT-CURR-BAL` (updated account balance after payment)
**Input Conditions**: 
- After user confirms bill payment
- Transaction amount equals current account balance (full payment)
- Before updating the account data file
**Calculation Logic**: 
```
COMPUTE updated_account_balance = current_account_balance - transaction_amount
WHERE transaction_amount = current_account_balance (full payment)
RESULT: updated_account_balance = 0 (account paid in full)
```

### RULE-CALC-008: Account ID Numeric Conversion for Transaction Processing
**Rule Description**: Converts user input account ID from character format to numeric format for transaction processing and validation
**COBOL Source Location**: COTRN02C.cbl, lines 204-206
**Involved Variables**: 
- Input: `ACTIDINI OF COTRN2AI` (character input from screen)
- Output: `WS-ACCT-ID-N` (numeric working storage)
- Output: `XREF-ACCT-ID` (numeric account ID for cross-reference lookup)
**Input Conditions**: 
- After validating that account ID input is numeric
- Before performing account cross-reference lookup
- Used when account ID is provided for transaction processing
**Calculation Logic**: 
```
IF account_id_input is numeric THEN
    COMPUTE numeric_account_id = FUNCTION NUMVAL(account_id_input)
    MOVE numeric_account_id TO cross_reference_account_id
    MOVE numeric_account_id TO screen_display_field
END-IF
```

### RULE-CALC-009: Card Number Numeric Conversion for Transaction Processing
**Rule Description**: Converts user input card number from character format to numeric format for transaction processing and validation
**COBOL Source Location**: COTRN02C.cbl, lines 218-221
**Involved Variables**: 
- Input: `CARDNINI OF COTRN2AI` (character input from screen)
- Output: `WS-CARD-NUM-N` (numeric working storage)
- Output: `XREF-CARD-NUM` (numeric card number for cross-reference lookup)
**Input Conditions**: 
- After validating that card number input is numeric
- Before performing card cross-reference lookup
- Used when card number is provided for transaction processing
**Calculation Logic**: 
```
IF card_number_input is numeric THEN
    COMPUTE numeric_card_number = FUNCTION NUMVAL(card_number_input)
    MOVE numeric_card_number TO cross_reference_card_number
    MOVE numeric_card_number TO screen_display_field
END-IF
```

---

## Analysis Notes
- Analysis started: $(date)
- Exclusions: File I/O, screen handling, error processing, validation logic, navigation
- Focus: Arithmetic operations (COMPUTE, ADD, SUBTRACT, MULTIPLY, DIVIDE) with business impact
- Files with no business rules: COACTVWC.cbl (display logic only), COCRDLIC.cbl (screen pagination logic only), COCRDUPC.cbl (screen processing only), CORPT00C.cbl (date processing and report generation only), COTRN00C.cbl (screen pagination and display logic only), COTRN01C.cbl (transaction view display logic only), COCRDSLC.cbl (credit card selection and screen processing only), COADM01C.cbl (admin menu navigation and screen processing only), COMEN01C.cbl (main menu navigation and screen processing only), COSGN00C.cbl (authentication and screen processing only), COUSR00C.cbl (user management and pagination logic only), COUSR01C.cbl (user addition and validation logic only), COUSR02C.cbl (user update and validation logic only), COUSR03C.cbl (user deletion and validation logic only), CSUTLDTC.cbl (date utility and validation logic only)
