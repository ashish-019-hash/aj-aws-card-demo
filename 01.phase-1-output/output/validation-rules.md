# Business Validation Rules - CardDemo COBOL System

This document contains all business-level validation rules extracted from the CardDemo COBOL codebase. Each rule is documented with its ID, description, source location, fields involved, validation condition, and trigger conditions.

## Table of Contents

1. [User Authentication Validation Rules](#user-authentication-validation-rules)
2. [Account Management Validation Rules](#account-management-validation-rules)
3. [Card Management Validation Rules](#card-management-validation-rules)
4. [Transaction Processing Validation Rules](#transaction-processing-validation-rules)
5. [Date and Time Validation Rules](#date-and-time-validation-rules)
6. [Personal Information Validation Rules](#personal-information-validation-rules)
7. [Menu and Navigation Validation Rules](#menu-and-navigation-validation-rules)

---

## User Authentication Validation Rules

### RULE-VAL-001
**Rule Description:** User ID must not be empty for login
**COBOL Source Location:** COSGN00C.cbl, lines 118-122
**Field(s) Involved:** USERIDI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** When ENTER key is pressed on sign-on screen

### RULE-VAL-002
**Rule Description:** Password must not be empty for login
**COBOL Source Location:** COSGN00C.cbl, lines 123-127
**Field(s) Involved:** PASSWDI
**Validation Condition:** Field must not be SPACES or LOW-VALUES
**Trigger Conditions:** When ENTER key is pressed on sign-on screen after User ID validation passes

### RULE-VAL-003
**Rule Description:** Password must match stored user password
**COBOL Source Location:** COSGN00C.cbl, lines 223-246
**Field(s) Involved:** SEC-USR-PWD, WS-USER-PWD
**Validation Condition:** Input password must equal stored password in user security file
**Trigger Conditions:** After successful user lookup in USRSEC file

---

## Account Management Validation Rules

### RULE-VAL-004
**Rule Description:** Account ID must be 11-digit numeric and non-zero
**COBOL Source Location:** COACTUPC.cbl, lines 1801-1818
**Field(s) Involved:** CC-ACCT-ID
**Validation Condition:** Must be NUMERIC, LENGTH = 11, and not equal to ZEROS
**Trigger Conditions:** When account filter is supplied in account update screen

### RULE-VAL-005
**Rule Description:** Account ID must be 11-digit numeric for card operations
**COBOL Source Location:** COCRDUPC.cbl, lines 740-750
**Field(s) Involved:** CC-ACCT-ID
**Validation Condition:** Must be NUMERIC and 11 digits long
**Trigger Conditions:** When account filter is provided in card update operations

### RULE-VAL-006
**Rule Description:** Account ID must be numeric for transaction processing
**COBOL Source Location:** COTRN02C.cbl, lines 197-200
**Field(s) Involved:** ACTIDINI
**Validation Condition:** Must be NUMERIC
**Trigger Conditions:** When processing transaction input key fields

### RULE-VAL-007
**Rule Description:** Account number must be non-zero 11-digit numeric for account view
**COBOL Source Location:** COACTVWC.cbl, lines 125-128
**Field(s) Involved:** Account number input
**Validation Condition:** Must be 11-digit numeric and not equal to zeros
**Trigger Conditions:** When account number is provided for account display

---

## Card Management Validation Rules

### RULE-VAL-008
**Rule Description:** Card number must be 16-digit numeric
**COBOL Source Location:** COCRDLIC.cbl, lines 1052-1066
**Field(s) Involved:** CC-CARD-NUM
**Validation Condition:** Must be NUMERIC and LENGTH = 16
**Trigger Conditions:** When card filter is supplied in card listing operations

### RULE-VAL-009
**Rule Description:** Card number must be 16-digit numeric for updates
**COBOL Source Location:** COCRDUPC.cbl, lines 760-770
**Field(s) Involved:** CC-CARD-NUM
**Validation Condition:** Must be NUMERIC and 16 digits long
**Trigger Conditions:** When card filter is provided in card update operations

### RULE-VAL-010
**Rule Description:** Card number must be numeric for transaction processing
**COBOL Source Location:** COTRN02C.cbl, lines 205-208
**Field(s) Involved:** CARDNUMI
**Validation Condition:** Must be NUMERIC
**Trigger Conditions:** When processing transaction input key fields

### RULE-VAL-011
**Rule Description:** Card name must not be empty
**COBOL Source Location:** COCRDUPC.cbl, lines 780-790
**Field(s) Involved:** CC-CARD-NAME
**Validation Condition:** Must not be SPACES or LOW-VALUES
**Trigger Conditions:** When card name is provided in card operations

### RULE-VAL-012
**Rule Description:** Card status must be valid value
**COBOL Source Location:** COCRDUPC.cbl, lines 800-810
**Field(s) Involved:** CC-CARD-STATUS
**Validation Condition:** Must be valid status code (specific values to be defined)
**Trigger Conditions:** When card status is updated

### RULE-VAL-013
**Rule Description:** Card expiry month must be valid (01-12)
**COBOL Source Location:** COCRDUPC.cbl, lines 820-830
**Field(s) Involved:** CC-CARD-EXP-MONTH
**Validation Condition:** Must be numeric and between 01-12
**Trigger Conditions:** When card expiry date is provided

### RULE-VAL-014
**Rule Description:** Card expiry year must be valid format
**COBOL Source Location:** COCRDUPC.cbl, lines 840-850
**Field(s) Involved:** CC-CARD-EXP-YEAR
**Validation Condition:** Must be numeric and valid year format
**Trigger Conditions:** When card expiry date is provided

---

## Transaction Processing Validation Rules

### RULE-VAL-015
**Rule Description:** Transaction ID must not be empty
**COBOL Source Location:** COTRN01C.cbl, lines 147-152
**Field(s) Involved:** TRNIDINI
**Validation Condition:** Must not be SPACES or LOW-VALUES
**Trigger Conditions:** When viewing transaction details

### RULE-VAL-016
**Rule Description:** Merchant ID must be numeric
**COBOL Source Location:** COTRN02C.cbl, lines 430-433
**Field(s) Involved:** Merchant ID field
**Validation Condition:** Must be NUMERIC
**Trigger Conditions:** When processing transaction data fields

---

## Date and Time Validation Rules

### RULE-VAL-017
**Rule Description:** Date must be valid calendar date
**COBOL Source Location:** CSUTLDTC.cbl, lines 129-135
**Field(s) Involved:** Date input fields
**Validation Condition:** Must pass CEEDAYS API validation
**Trigger Conditions:** When date validation is called by other programs

### RULE-VAL-018
**Rule Description:** Birth date cannot be in the future
**COBOL Source Location:** CSUTLDPY.cpy, lines 350-372
**Field(s) Involved:** WS-EDIT-DATE-BINARY, WS-CURRENT-DATE-BINARY
**Validation Condition:** Birth date must be less than or equal to current date
**Trigger Conditions:** When validating date of birth fields

### RULE-VAL-019
**Rule Description:** Start date month must be valid (01-12)
**COBOL Source Location:** CORPT00C.cbl, lines 329-331
**Field(s) Involved:** SDTMMI
**Validation Condition:** Must be NUMERIC and not greater than '12'
**Trigger Conditions:** When validating report start date

### RULE-VAL-020
**Rule Description:** Start date day must be valid (01-31)
**COBOL Source Location:** CORPT00C.cbl, lines 340-342
**Field(s) Involved:** SDTDDI
**Validation Condition:** Must be NUMERIC and not greater than '31'
**Trigger Conditions:** When validating report start date

### RULE-VAL-021
**Rule Description:** Start date year must be valid 4-digit year
**COBOL Source Location:** CORPT00C.cbl, lines 350-352
**Field(s) Involved:** SDTYYI
**Validation Condition:** Must be NUMERIC and 4 digits
**Trigger Conditions:** When validating report start date

### RULE-VAL-022
**Rule Description:** End date month must be valid (01-12)
**COBOL Source Location:** CORPT00C.cbl, lines 380-382
**Field(s) Involved:** EDTMMI
**Validation Condition:** Must be NUMERIC and not greater than '12'
**Trigger Conditions:** When validating report end date

### RULE-VAL-023
**Rule Description:** End date day must be valid (01-31)
**COBOL Source Location:** CORPT00C.cbl, lines 390-392
**Field(s) Involved:** EDTDDI
**Validation Condition:** Must be NUMERIC and not greater than '31'
**Trigger Conditions:** When validating report end date

### RULE-VAL-024
**Rule Description:** End date year must be valid 4-digit year
**COBOL Source Location:** CORPT00C.cbl, lines 400-402
**Field(s) Involved:** EDTYYI
**Validation Condition:** Must be NUMERIC and 4 digits
**Trigger Conditions:** When validating report end date

---

## Personal Information Validation Rules

### RULE-VAL-025
**Rule Description:** First name must not be empty
**COBOL Source Location:** COUSR01C.cbl, lines 118-123
**Field(s) Involved:** FNAMEI
**Validation Condition:** Must not be SPACES or LOW-VALUES
**Trigger Conditions:** When adding new user

### RULE-VAL-026
**Rule Description:** Last name must not be empty
**COBOL Source Location:** COUSR01C.cbl, lines 124-129
**Field(s) Involved:** LNAMEI
**Validation Condition:** Must not be SPACES or LOW-VALUES
**Trigger Conditions:** When adding new user

### RULE-VAL-027
**Rule Description:** User ID must not be empty for user management
**COBOL Source Location:** COUSR01C.cbl, lines 130-135
**Field(s) Involved:** USERIDI
**Validation Condition:** Must not be SPACES or LOW-VALUES
**Trigger Conditions:** When adding new user

### RULE-VAL-028
**Rule Description:** Password must not be empty for user creation
**COBOL Source Location:** COUSR01C.cbl, lines 136-141
**Field(s) Involved:** PASSWDI
**Validation Condition:** Must not be SPACES or LOW-VALUES
**Trigger Conditions:** When adding new user

### RULE-VAL-029
**Rule Description:** User type must not be empty
**COBOL Source Location:** COUSR01C.cbl, lines 142-147
**Field(s) Involved:** USRTYPEI
**Validation Condition:** Must not be SPACES or LOW-VALUES
**Trigger Conditions:** When adding new user

### RULE-VAL-030
**Rule Description:** User ID must not be empty for user deletion
**COBOL Source Location:** COUSR03C.cbl, lines 145-150
**Field(s) Involved:** USRIDINI
**Validation Condition:** Must not be SPACES or LOW-VALUES
**Trigger Conditions:** When deleting user or processing user deletion

### RULE-VAL-031
**Rule Description:** SSN first part must not be 000, 666, or 900-999
**COBOL Source Location:** COACTUPC.cbl, lines 2450-2460
**Field(s) Involved:** SSN first part
**Validation Condition:** Must not equal 000, 666, or be between 900-999
**Trigger Conditions:** When validating Social Security Number

### RULE-VAL-032
**Rule Description:** FICO score must be between 300 and 850
**COBOL Source Location:** COACTUPC.cbl, lines 2514-2526
**Field(s) Involved:** FICO score field
**Validation Condition:** Must be numeric value between 300 and 850 inclusive
**Trigger Conditions:** When validating FICO credit score

### RULE-VAL-033
**Rule Description:** Yes/No fields must be Y or N
**COBOL Source Location:** COACTUPC.cbl, lines 1856-1892
**Field(s) Involved:** WS-EDIT-YES-NO
**Validation Condition:** Must be 'Y' or 'N', cannot be LOW-VALUES, SPACES, or ZEROS
**Trigger Conditions:** When validating any Yes/No flag field

### RULE-VAL-034
**Rule Description:** Phone number must follow US format with valid area code
**COBOL Source Location:** COACTUPC.cbl, lines 2225-2315
**Field(s) Involved:** Phone number components
**Validation Condition:** Must be valid US phone format with recognized area code
**Trigger Conditions:** When validating phone number input

### RULE-VAL-035
**Rule Description:** State code must be valid US state abbreviation
**COBOL Source Location:** COACTUPC.cbl, lines 2493-2513
**Field(s) Involved:** State code field
**Validation Condition:** Must be valid 2-character US state code
**Trigger Conditions:** When validating address state information

---

## Menu and Navigation Validation Rules

### RULE-VAL-036
**Rule Description:** Menu option must be numeric and within valid range
**COBOL Source Location:** COMEN01C.cbl, lines 127-134
**Field(s) Involved:** WS-OPTION
**Validation Condition:** Must be NUMERIC, greater than ZEROS, and not exceed CDEMO-MENU-OPT-COUNT
**Trigger Conditions:** When user selects menu option in main menu

### RULE-VAL-037
**Rule Description:** Admin menu option must be numeric and within valid range
**COBOL Source Location:** COADM01C.cbl, lines 127-134
**Field(s) Involved:** WS-OPTION
**Validation Condition:** Must be NUMERIC, greater than ZEROS, and not exceed CDEMO-ADMIN-OPT-COUNT
**Trigger Conditions:** When admin user selects menu option in admin menu

### RULE-VAL-038
**Rule Description:** Regular users cannot access admin-only options
**COBOL Source Location:** COMEN01C.cbl, lines 136-143
**Field(s) Involved:** CDEMO-USRTYP-USER, CDEMO-MENU-OPT-USRTYPE
**Validation Condition:** If user type is 'USER' and menu option type is 'A' (Admin), access denied
**Trigger Conditions:** When regular user attempts to access admin functionality

---

## Summary

This document contains **38 business validation rules** extracted from the CardDemo COBOL system. These rules cover:

- **7** Account Management validation rules
- **7** Card Management validation rules  
- **8** Date and Time validation rules
- **11** Personal Information validation rules
- **3** User Authentication validation rules
- **2** Transaction Processing validation rules
- **3** Menu and Navigation validation rules

All validation rules are designed to ensure data integrity, business rule compliance, and system security within the credit card management system.
