# Phase 2 Migration uirements: Account Update Program

## Overview
This document providcomprehensive migration requirements for the Account Update program, extracted f the COBOL program extraction document. The requirements are organized to supportgration to modern technology stacks including Angular/React for frontend and Node.jsthon/Java for backend.

**Program Purpose:**low users to view and update account and customer information through a web interf with comprehensive validation, optimistic locking, and transactional integr.

---

## 1️⃣ FRONTEND REQUIRNTS

### **UI Components ayout**

**Page Structure: Acnt Update Form**
- Three-section layowith clear visual separation
- Responsive design  desktop and tablet
- Fixed header with igation breadcrumbs and function buttons

**Section 1: Accountformation**
```
Fields (11 total):
- Account Number (te 11 chars, alphanumeric, primary key)
- Account Status (drown: Active='Y', Inactive='N')
- Current Balance (cency, read-only, 12 digits + 2 decimals)
- Credit Limit (curry, editable, 12 digits + 2 decimals)
- Cash Credit Limit rrency, editable, 12 digits + 2 decimals)
- Open Date (date pir: Year/Month/Day as 3 separate fields)
- Expiration Date (d picker: Year/Month/Day as 3 separate fields)
- Reissue Date (datecker: Year/Month/Day as 3 separate fields)
- Current Cycle Cred(currency, read-only, 12 digits + 2 decimals)
- Current Cycle Debicurrency, read-only, 12 digits + 2 decimals)
- Account Group ID (t, 10 chars, alphanumeric)
```

**Section 2: Customeemographics**
```
Fields (8 total):
- Customer ID (text,digits, read-only, auto-populated)
- First Name (text, chars, required, alphabetic only)
- Middle Name (text, chars, optional, alphabetic only)
- Last Name (text, 2hars, required, alphabetic only)
- Social Security Nur (3 separate fields: 3-2-4 digits, masked input)
- Government Issued (text, 20 chars, optional)
- Date of Birth (daticker: Year/Month/Day as 3 separate fields)
- FICO Credit Score mber, 3 digits, range 300-850)
```

**Section 3: AddressContact Information**
```
Fields (11 total):
- Address Line 1 (te 50 chars, optional)
- Address Line 2 (te 50 chars, optional)
- Address Line 3/Cittext, 50 chars, optional)
- State Code (dropdo 2 chars, US states only)
- Country Code (text chars, optional)
- Zip Code (text, 10ars, optional, must validate with state)
- Phone Number 1 (3 arate fields: area code/prefix/line, optional but all-or-nothing)
- Phone Number 2 (3 arate fields: area code/prefix/line, optional but all-or-nothing)
- EFT Account ID (te 10 chars, optional)
- Primary Card Holdendicator (dropdown: Yes='Y', No='N')
```

**Header Elements**
- Application title:ccount Update"
- Current date (MM/DY format)
- Current time (HH:MS format)
- User session info er ID, user type)

**Function Buttons**
- Enter: Fetch accoudata (initial) or validate changes (after data displayed)
- F3/Exit: Return toevious screen or main menu
- F5/Save: Confirm aexecute update (enabled only after validation passes)
- F12/Cancel: Discarhanges and reset form (enabled only after data fetched)

**Message Areas**
- Error message area bottom (78 chars, red text for errors)
- Info message area  chars, blue text for status/prompts)

---

### **Client-Side Vaation Rules**

**Account Status (Yeo Validation)**
- Must be 'Y' or 'N'
- Required field
- Errors: "Account Sus must be supplied", "Account Status must be Y or N"

**Date Validation (3elds: Year, Month, Day)**
- Year: 4 digits, nuic, range 1900-2099
- Month: 2 digits, nric, range 01-12
- Day: 2 digits, numc, valid for month (handle leap years)
- Special rule for D of Birth: Cannot be in future
- Errors per compone
  - "Open Date year t be supplied"
  - "Open Date year t be 4 digits"
  - "Open Date year t be between 1900 and 2099"
  - "Open Date monthst be between 01 and 12"
  - "Open Date day m be valid for the month"
  - "Date of Birth cot be in the future"

**Currency/Decimal Vdation**
- Must be numeric wioptional sign
- Maximum 10 digits ore decimal, 2 digits after
- Can be negative
- Format: -9,999,9999.99
- Errors: "[Field na must be supplied", "[Field name] is not valid"

**Customer Name Fiel(Required Alphabetic)**
- Only alphabetic chcters and spaces allowed
- Non-alphabetic chaters should be stripped/rejected
- Result must not beank after stripping
- Errors: "[Field na must be supplied", "[Field name] can have alphabets only"

**Address Fields (Opnal Alphabetic)**
- Only alphabetic characters and spaces allowed
- Can be completely blank
- If provided, must contain only valid characters

**Social Security Number (3 separate fields)**
- Part 1: 3 digits, cannot be 000
- Part 2: 2 digits, cannot be 00
- Part 3: 4 digits, cannot be 0000
- Errors: "SSN Part 1 must be 3 digits", "SSN Part 1 cannot be 000", etc.

**Phone Number (3 separate fields per phone)**
- Area Code: 3 digits, must be valid North American area code
- Prefix: 3 digits, cannot be 000
- Line: 4 digits
- Optional: Can be completely blank
- All-or-nothing: If any part provided, all parts required
- Errors: "Phone area code must be 3 digits", "Phone prefix cannot be 000", etc.

**State Code**
- Exactly 2 alphabetic characters
- Must be valid US state code (AL, AK, AZ, ..., WY)
- Errors: "State Code must be supplied", "State Code is not a valid US state"

**Zip Code Cross-Field Validation**
- Zip code format must be valid (5 digits or 5+4 format)
- Zip code must be valid for the selected state
- Error: "Zip code is not valid for state [XX]"

**FICO Credit Score**
- Must be numeric
- Required field
- Range: 300-850 inclusive
- Errors: "FICO Score must be supplied", "FICO Score must be numeric", "FICO Score must be between 300 and 850"

**Account Number (Initial Search)**
- Exactly 11 characters
- Must be numeric
- Cannot be all zeros
- Errors: "Account Number must be supplied", "Account Number must be 11 digits", "Account Number must not be zero"

---

### **UI State Management**

**State 1: Initial Entry (INITIAL_ENTRY)**
- Display: Blank form with only Account Number field enabled
- Available actions: Enter (to search), F3 (to exit)
- Disabled: All other fields, F5, F12
- Prompt: "Enter Account Number and press Enter"

**State 2: Data Fetched & Displayed (DATA_DISPLAYED)**
- Display: All fields populated with fetched data
- Available actions: Edit any field, Enter (to validate), F3 (to exit), F12 (to cancel)
- Disabled: F5 (until validation passes)
- All fields except Account Number and Customer ID are editable
- Mark read-only fields visually (grayed out)

**State 3: Changes Made (CHANGES_MADE)**
- Display: User has modified one or more fields
- Track changed fields for visual indication (e.g., highlight modified fields)
- Available actions: Continue editing, Enter (to validate), F3 (to exit), F12 (to cancel)
- Disabled: F5 (until validation passes)

**State 4: Validation Passed (VALIDATION_PASSED)**
- Display: All validations successful
- Available actions: F5 (to save), F12 (to cancel), F3 (to exit)
- Show prompt: "Press F5 to save or F12 to cancel"
- F5 button should be highlighted/enabled
- User can still edit fields (returns to state 3)

**State 5: Update In Progress (UPDATING)**
- Display: Loading indicator/spinner
- Disable all user input
- Show message: "Updating account..."

**State 6: Update Complete (UPDATE_SUCCESS)**
- Display: Success message "Account Updated Successfully"
- Reset form to initial entry state
- Clear all fields except account number
- Return to state 1 for next account

**State 7: Update Failed (UPDATE_FAILED)**
- Display: Error message explaining failure
- Keep current data displayed
- Available actions: F12 (to cancel), F3 (to exit)
- User can review error and decide next action

**State 8: Validation Failed (VALIDATION_FAILED)**
- Display: Field-level error indicators
- Highlight first error field in red
- Position cursor at first error field
- Show error message at bottom
- Available actions: Edit fields, Enter (to re-validate), F12 (to cancel), F3 (to exit)
- Track all error fields for multi-field error display

**State Transitions**
```
INITIAL_ENTRY
  → [Enter + valid account] → DATA_DISPLAYED
  → [Enter + invalid account] → INITIAL_ENTRY (with error)
  → [F3] → Exit

DATA_DISPLAYED
  → [Edit any field] → CHANGES_MADE
  → [F3] → Exit
  → [F12] → INITIAL_ENTRY

CHANGES_MADE
  → [Enter + validation fails] → VALIDATION_FAILED
  → [Enter + validation passes + no changes detected] → DATA_DISPLAYED (with info message)
  → [Enter + validation passes + changes detected] → VALIDATION_PASSED
  → [F3] → Exit (confirm unsaved changes)
  → [F12] → DATA_DISPLAYED (discard changes)

VALIDATION_FAILED
  → [Edit fields] → CHANGES_MADE
  → [F12] → DATA_DISPLAYED
  → [F3] → Exit

VALIDATION_PASSED
  → [F5] → UPDATING
  → [F12] → DATA_DISPLAYED
  → [F3] → Exit (confirm unsaved changes)
  → [Edit any field] → CHANGES_MADE

UPDATING
  → [Success] → UPDATE_SUCCESS
  → [Lock failure] → UPDATE_FAILED
  → [Concurrent update detected] → DATA_DISPLAYED (with current data + warning)
  → [Database error] → UPDATE_FAILED

UPDATE_SUCCESS
  → Auto-transition → INITIAL_ENTRY

UPDATE_FAILED
  → [F12] → DATA_DISPLAYED
  → [F3] → Exit
```

---

### **Data Formatting & Display**

**Date Display Format**
- Storage: YYYY-MM-DD (single string)
- Display: Three separate input fields
  - Year: 4-digit input (YYYY)
  - Month: 2-digit input (MM)
  - Day: 2-digit input (DD)
- Visual: "YYYY / MM / DD" with separators
- On save: Concatenate with hyphens → "YYYY-MM-DD"

**Phone Number Display Format**
- Storage: (999)999-9999 (single string)
- Display: Three separate input fields
  - Area Code: 3-digit input
  - Prefix: 3-digit input
  - Line: 4-digit input
- Visual: "(999) 999-9999" with formatting
- On save: Concatenate with formatting → "(999)999-9999"

**Social Security Number Display Format**
- Storage: 9-digit integer
- Display: Three separate masked input fields
  - Part 1: 3-digit masked input (***) or (999)
  - Part 2: 2-digit masked input (**) or (99)
  - Part 3: 4-digit masked input (****) or (9999)
- Visual: "999-99-9999" or "***-**-****" (masked)
- On save: Concatenate as 9-digit number

**Currency Display Format**
- Storage: Decimal number (e.g., 12345.67)
- Display: Formatted with commas and 2 decimal places
- Input: Allow negative sign, commas optional
- Format: "-9,999,999,999.99"
- On save: Parse to decimal number
- Read-only fields: Display formatted with $ sign

**Account Status Display**
- Storage: 'Y' or 'N'
- Display: Dropdown with options
  - "Active" (value='Y')
  - "Inactive" (value='N')

**Primary Card Holder Indicator Display**
- Storage: 'Y' or 'N'
- Display: Dropdown with options
  - "Yes" (value='Y')
  - "No" (value='N')

---

### **Error Handling & Display**

**Field-Level Error Display**
- Highlight error fields with red border
- Display error icon next to field
- Show inline error message below field
- Position cursor at first error field automatically

**Form-Level Error Display**
- Display error message in dedicated error area at bottom
- Use red background or red text
- Show error icon
- Allow multiple errors to be displayed
- Format: "[Field Name]: [Error Message]"

**Error Message Examples**
```
- "Account Number must be 11 digits"
- "Credit Limit is not valid"
- "Open Date day must be valid for the month"
- "First Name can have alphabets only"
- "SSN Part 1 cannot be 000"
- "Phone area code must be a valid North American code"
- "State Code is not a valid US state"
- "Zip code is not valid for state CA"
- "FICO Score must be between 300 and 850"
```

**Success Message Display**
- Show in dedicated info area
- Use green background or green text
- Show success icon
- Auto-dismiss after 3-5 seconds
- Example: "Account Updated Successfully"

**Info/Prompt Message Display**
- Show in dedicated info area
- Use blue background or blue text
- Show info icon
- Persist until action taken
- Examples:
  - "Enter Account Number and press Enter"
  - "Press F5 to save or F12 to cancel"
  - "No changes detected"

**Loading State Display**
- Show spinner/loading indicator during:
  - Account data fetch
  - Update processing
- Disable all user input during loading
- Show status message: "Loading account data..." or "Updating account..."

**Concurrent Update Warning**
- Special error type when another user modified data
- Display message: "Data was changed by another user. Please review and retry."
- Automatically reload current data from server
- Highlight fields that changed
- User must review and decide whether to proceed

**Record Not Found**
- Display error: "Account not found"
- Keep focus on Account Number field
- Allow user to try different account number

**Record Locked**
- Display error: "Could not lock account for update - another user is editing"
- Keep form in current state
- User can try again or cancel

---

### **Navigation & User Actions**

**Enter Key / Submit Button**
- **When in INITIAL_ENTRY state**: Fetch account data
- **When in DATA_DISPLAYED state**: Validate changes (if any)
- **When in CHANGES_MADE state**: Validate all changes
- **When in VALIDATION_FAILED state**: Re-validate after corrections

**F3 Key / Exit Button**
- Return to calling screen (previous screen in navigation)
- If no calling screen specified, return to main menu
- If unsaved changes exist, show confirmation dialog:
  - "You have unsaved changes. Are you sure you want to exit?"
  - Options: "Stay", "Exit without saving"

**F5 Key / Save Button**
- Only enabled when in VALIDATION_PASSED state
- Execute update transaction
- Show loading state during update
- On success: Show success message and reset to INITIAL_ENTRY
- On failure: Show error and remain in current state

**F12 Key / Cancel Button**
- Only enabled after data is fetched
- Discard all changes
- Reload original data from server
- Return to DATA_DISPLAYED state with original values
- If in INITIAL_ENTRY state, clear form

**Keyboard Shortcuts**
- Tab: Move to next field
- Shift+Tab: Move to previous field
- Enter: Submit/Validate
- Esc: Same as F3/Exit
- Ctrl+S: Same as F5/Save (when enabled)

**Mouse/Touch Interactions**
- All fields clickable/tappable
- Buttons show hover state
- Disabled buttons are visually grayed out
- Form supports scrolling on mobile devices

---

## 2️⃣ BACKEND REQUIREMENTS

### **API Endpoints**

**1. GET /api/accounts/{accountId}**
```
Purpose: Fetch account and customer data by account ID
Method: GET
Path Parameters:
  - accountId (string, 11 chars): Account identifier

Response 200 (Success):
{
  "account": {
    "accountId": "00000000001",
    "activeStatus": "Y",
    "currentBalance": 12345.67,
    "creditLimit": 50000.00,
    "cashCreditLimit": 5000.00,
    "openDate": "2020-01-15",
    "expirationDate": "2025-01-15",
    "reissueDate": "2023-01-15",
    "currentCycleCredit": 1000.00,
    "currentCycleDebit": 500.00,
    "accountGroupId": "GROUP001"
  },
  "customer": {
    "customerId": 123456789,
    "firstName": "John",
    "middleName": "Robert",
    "lastName": "Smith",
    "addressLine1": "123 Main Street",
    "addressLine2": "Apt 4B",
    "addressLine3": "Springfield",
    "stateCode": "CA",
    "countryCode": "USA",
    "zipCode": "90210",
    "phoneNumber1": "(555)123-4567",
    "phoneNumber2": "(555)987-6543",
    "ssn": 123456789,
    "govtIssuedId": "DL12345678",
    "dateOfBirth": "1980-05-20",
    "eftAccountId": "EFT001",
    "primaryCardHolderInd": "Y",
    "ficoCreditScore": 750
  }
}

Response 404 (Not Found):
{
  "error": "Account not found",
  "accountId": "00000000001"
}

Response 500 (Server Error):
{
  "error": "Database error occurred",
  "message": "Unable to retrieve account data"
}
```

**2. PUT /api/accounts/{accountId}**
```
Purpose: Update account and customer data with optimistic locking
Method: PUT
Path Parameters:
  - accountId (string, 11 chars): Account identifier

Request Body:
{
  "account": {
    "activeStatus": "Y",
    "creditLimit": 55000.00,
    "cashCreditLimit": 5500.00,
    "openDate": "2020-01-15",
    "expirationDate": "2025-06-30",
    "reissueDate": "2023-01-15",
    "accountGroupId": "GROUP002"
  },
  "customer": {
    "firstName": "John",
    "middleName": "Robert",
    "lastName": "Smith",
    "addressLine1": "456 Oak Avenue",
    "addressLine2": "Suite 100",
    "addressLine3": "Springfield",
    "stateCode": "CA",
    "countryCode": "USA",
    "zipCode": "90211",
    "phoneNumber1": "(555)123-9999",
    "phoneNumber2": "(555)987-6543",
    "govtIssuedId": "DL12345678",
    "dateOfBirth": "1980-05-20",
    "eftAccountId": "EFT001",
    "primaryCardHolderInd": "Y",
    "ficoCreditScore": 780
  },
  "oldValues": {
    "account": { /* original account values for optimistic lock check */ },
    "customer": { /* original customer values for optimistic lock check */ }
  }
}

Response 200 (Success):
{
  "success": true,
  "message": "Account updated successfully"
}

Response 400 (Validation Error):
{
  "error": "Validation failed",
  "errors": [
    {"field": "creditLimit", "message": "Credit Limit is not valid"},
    {"field": "zipCode", "message": "Zip code is not valid for state CA"}
  ]
}

Response 409 (Concurrent Update Detected):
{
  "error": "Concurrent update detected",
  "message": "Data was changed by another user. Please review and retry.",
  "currentData": {
    "account": { /* current account values from database */ },
    "customer": { /* current customer values from database */ }
  }
}

Response 423 (Locked):
{
  "error": "Record locked",
  "message": "Could not lock account for update - another user is editing"
}

Response 500 (Server Error):
{
  "error": "Update failed",
  "message": "Database error occurred during update"
}
```

**3. GET /api/navigation/context**
```
Purpose: Retrieve navigation breadcrumbs and session information
Method: GET

Response 200:
{
  "userId": "USER001",
  "userType": "A",
  "callingProgram": "MAINMENU",
  "callingTransaction": "MENU",
  "sessionData": {
    "lastMapDisplayed": "ACCTUPD",
    "navigationHistory": ["LOGIN", "MAINMENU", "ACCTUPD"]
  }
}
```

---

### **Business Logic - Data Fetch**

**Repository Method: AccountRepository.getAccountWithCustomer(accountId)**

Implementation using JOIN query (single database round-trip):

```sql
SELECT
  a.account_id,
  a.active_status,
  a.current_balance,
  a.credit_limit,
  a.cash_credit_limit,
  a.open_date,
  a.expiration_date,
  a.reissue_date,
  a.current_cycle_credit,
  a.current_cycle_debit,
  a.account_group_id,
  c.customer_id,
  c.first_name,
  c.middle_name,
  c.last_name,
  c.address_line1,
  c.address_line2,
  c.address_line3,
  c.state_code,
  c.country_code,
  c.zip_code,
  c.phone_number1,
  c.phone_number2,
  c.ssn,
  c.govt_issued_id,
  c.date_of_birth,
  c.eft_account_id,
  c.primary_card_holder_ind,
  c.fico_credit_score,
  cx.card_number
FROM card_xref cx
INNER JOIN accounts a ON cx.account_id = a.account_id
INNER JOIN customers c ON cx.customer_id = c.customer_id
WHERE cx.account_id = ?
LIMIT 1;
```

**Logic Flow:**
1. Execute JOIN query with account_id parameter
2. If no rows returned → Account not found (404)
3. If query succeeds:
   - Map result set to Account and Customer objects
   - Parse date strings to appropriate date format
   - Parse phone number string to separate components
   - Return combined result
4. If database error → Return 500 error

**Error Handling:**
- Account not in card_xref → Return 404 "Account not found"
- Account not in accounts table → Return 404 "Account not found"
- Customer not in customers table → Return 404 "Customer not found"
- Database connection error → Return 500 "Database error"

---

### **Business Logic - Update Processing**

**Service Method: AccountService.updateAccountAndCustomer(accountId, updates, oldValues)**

**Step-by-Step Execution (Chronological Order):**

**1. Change Detection**
```
Compare oldValues with updates field-by-field
If no changes detected:
  Return response: "No changes detected"
  Exit without database operations
Track which specific fields changed for audit logging
```

**2. Server-Side Validation**
```
Re-validate ALL fields (never trust client validation):
  - Account status: Must be 'Y' or 'N'
  - Credit limits: Must be valid currency, non-negative
  - Dates: Valid format, valid ranges, DOB not in future
  - Customer names: Required, alphabetic only
  - SSN: Valid 3-2-4 format, no invalid parts
  - Phone numbers: Valid format if provided, all-or-nothing
  - State code: Valid US state
  - Zip/State: Cross-validate
  - FICO score: Range 300-850

If validation fails:
  Collect all validation errors
  Return 400 with field-level error details
  Exit without database operations
```

**3. Begin Database Transaction**
```
START TRANSACTION;
Set transaction isolation level to SERIALIZABLE or REPEATABLE READ
```

**4. Lock Account Record**
```
Execute:
  SELECT * FROM accounts
  WHERE account_id = ?
  FOR UPDATE;

If lock cannot be obtained (timeout):
  ROLLBACK TRANSACTION;
  Return 423 "Could not lock account for update"
  Exit

Store locked account record for comparison
```

**5. Lock Customer Record**
```
First, get customer_id from card_xref:
  SELECT customer_id FROM card_xref WHERE account_id = ?

Then lock customer record:
  SELECT * FROM customers
  WHERE customer_id = ?
  FOR UPDATE;

If lock cannot be obtained (timeout):
  ROLLBACK TRANSACTION;
  Return 423 "Could not lock customer for update"
  Exit

Store locked customer record for comparison
```

**6. Optimistic Concurrency Check**
```
Compare locked account record with oldValues.account field-by-field:
  - active_status
  - credit_limit
  - cash_credit_limit
  - open_date
  - expiration_date
  - reissue_date
  - account_group_id

Compare locked customer record with oldValues.customer field-by-field:
  - first_name, middle_name, last_name
  - address_line1, address_line2, address_line3
  - state_code, country_code, zip_code
  - phone_number1, phone_number2
  - ssn, govt_issued_id, date_of_birth
  - eft_account_id, primary_card_holder_ind, fico_credit_score

If ANY field differs from oldValues:
  ROLLBACK TRANSACTION;
  Return 409 with current data from locked records
  Message: "Data was changed by another user. Please review and retry."
  Exit
```

**7. Prepare Update Data**
```
Build account update object:
  - Map all changed account fields from updates.account
  - Reassemble date fields: year + "-" + month + "-" + day → "YYYY-MM-DD"
  - Validate all fields one final time

Build customer update object:
  - Map all changed customer fields from updates.customer
  - Reassemble date fields: year + "-" + month + "-" + day → "YYYY-MM-DD"
  - Reassemble phone: "(" + area + ")" + prefix + "-" + line → "(999)999-9999"
  - Reassemble SSN: part1 + part2 + part3 → 9-digit integer
  - Validate all fields one final time
```

**8. Update Account Record**
```
Execute:
  UPDATE accounts
  SET active_status = ?,
      credit_limit = ?,
      cash_credit_limit = ?,
      open_date = ?,
      expiration_date = ?,
      reissue_date = ?,
      account_group_id = ?
  WHERE account_id = ?;

If update fails (database error):
  ROLLBACK TRANSACTION;
  Return 500 "Update failed"
  Exit
```

**9. Update Customer Record**
```
Execute:
  UPDATE customers
  SET first_name = ?,
      middle_name = ?,
      last_name = ?,
      address_line1 = ?,
      address_line2 = ?,
      address_line3 = ?,
      state_code = ?,
      country_code = ?,
      zip_code = ?,
      phone_number1 = ?,
      phone_number2 = ?,
      govt_issued_id = ?,
      date_of_birth = ?,
      eft_account_id = ?,
      primary_card_holder_ind = ?,
      fico_credit_score = ?
  WHERE customer_id = ?;

If update fails (database error):
  ROLLBACK TRANSACTION; (undoes account update too)
  Return 500 "Update failed"
  Exit
```

**10. Commit Transaction**
```
COMMIT TRANSACTION;

Both account and customer records updated atomically
Locks released automatically
Return 200 "Account updated successfully"
```

**11. Audit Logging (Optional)**
```
After successful commit, log:
  - User ID who made the change
  - Timestamp
  - Account ID and Customer ID
  - Which fields were changed (old → new values)
  - Source IP address
  - Session ID
```

---

### **Server-Side Validation Logic**

**All validation rules must be implemented server-side (duplicate of frontend validation)**

**Date Validation Function**
```javascript
function validateDate(year, month, day, fieldName, allowFuture = true) {
  const errors = [];

  // Year validation
  if (!year) {
    errors.push(`${fieldName} year must be supplied`);
  } else if (!/^\d{4}$/.test(year)) {
    errors.push(`${fieldName} year must be 4 digits`);
  } else if (year < 1900 || year > 2099) {
    errors.push(`${fieldName} year must be between 1900 and 2099`);
  }

  // Month validation
  if (!month) {
    errors.push(`${fieldName} month must be supplied`);
  } else if (!/^\d{2}$/.test(month)) {
    errors.push(`${fieldName} month must be 2 digits`);
  } else if (month < 1 || month > 12) {
    errors.push(`${fieldName} month must be between 01 and 12`);
  }

  // Day validation
  if (!day) {
    errors.push(`${fieldName} day must be supplied`);
  } else if (!/^\d{2}$/.test(day)) {
    errors.push(`${fieldName} day must be 2 digits`);
  } else {
    // Check day is valid for month (handle leap years)
    const daysInMonth = new Date(year, month, 0).getDate();
    if (day < 1 || day > daysInMonth) {
      errors.push(`${fieldName} day must be valid for the month`);
    }
  }

  // Future date check
  if (!allowFuture && errors.length === 0) {
    const date = new Date(year, month - 1, day);
    const today = new Date();
    if (date > today) {
      errors.push(`${fieldName} cannot be in the future`);
    }
  }

  return errors;
}
```

**SSN Validation Function**
```javascript
function validateSSN(part1, part2, part3) {
  const errors = [];

  // Part 1: 3 digits, cannot be 000
  if (!part1 || !/^\d{3}$/.test(part1)) {
    errors.push("SSN Part 1 must be 3 digits");
  } else if (part1 === "000") {
    errors.push("SSN Part 1 cannot be 000");
  }

  // Part 2: 2 digits, cannot be 00
  if (!part2 || !/^\d{2}$/.test(part2)) {
    errors.push("SSN Part 2 must be 2 digits");
  } else if (part2 === "00") {
    errors.push("SSN Part 2 cannot be 00");
  }

  // Part 3: 4 digits, cannot be 0000
  if (!part3 || !/^\d{4}$/.test(part3)) {
    errors.push("SSN Part 3 must be 4 digits");
  } else if (part3 === "0000") {
    errors.push("SSN Part 3 cannot be 0000");
  }

  return errors;
}
```

**Phone Number Validation Function**
```javascript
function validatePhoneNumber(areaCode, prefix, lineNumber, fieldName) {
  const errors = [];

  // Check if all parts provided or all parts empty
  const partsProvided = [areaCode, prefix, lineNumber].filter(p => p).length;
  if (partsProvided > 0 && partsProvided < 3) {
    errors.push(`${fieldName}: All parts required if any part provided`);
    return errors;
  }

  // If all empty, it's valid (optional field)
  if (partsProvided === 0) {
    return errors;
  }

  // Area code: 3 digits, valid North American code
  if (!/^\d{3}$/.test(areaCode)) {
    errors.push(`${fieldName} area code must be 3 digits`);
  } else if (!isValidAreaCode(areaCode)) {
    errors.push(`${fieldName} area code must be a valid North American code`);
  }

  // Prefix: 3 digits, cannot be 000
  if (!/^\d{3}$/.test(prefix)) {
    errors.push(`${fieldName} prefix must be 3 digits`);
  } else if (prefix === "000") {
    errors.push(`${fieldName} prefix cannot be 000`);
  }

  // Line number: 4 digits
  if (!/^\d{4}$/.test(lineNumber)) {
    errors.push(`${fieldName} line number must be 4 digits`);
  }

  return errors;
}
```

**State/Zip Cross-Validation Function**
```javascript
function validateStateZip(stateCode, zipCode) {
  const errors = [];

  // State code validation
  if (!stateCode || stateCode.length !== 2) {
    errors.push("State Code must be 2 characters");
    return errors;
  }

  if (!isValidUSState(stateCode)) {
    errors.push("State Code is not a valid US state");
    return errors;
  }

  // Zip code validation for state
  if (zipCode && !isValidZipForState(zipCode, stateCode)) {
    errors.push(`Zip code is not valid for state ${stateCode}`);
  }

  return errors;
}
```

**FICO Score Validation Function**
```javascript
function validateFICOScore(score) {
  const errors = [];

  if (!score) {
    errors.push("FICO Score must be supplied");
  } else if (!/^\d+$/.test(score)) {
    errors.push("FICO Score must be numeric");
  } else {
    const scoreNum = parseInt(score);
    if (scoreNum < 300 || scoreNum > 850) {
      errors.push("FICO Score must be between 300 and 850");
    }
  }

  return errors;
}
```

**Currency Validation Function**
```javascript
function validateCurrency(value, fieldName) {
  const errors = [];

  if (!value) {
    errors.push(`${fieldName} must be supplied`);
    return errors;
  }

  // Remove commas and spaces
  const cleaned = value.replace(/[,\s]/g, '');

  // Check format: optional sign, digits, optional decimal with 2 digits
  if (!/^-?\d+(\.\d{2})?$/.test(cleaned)) {
    errors.push(`${fieldName} is not valid`);
  }

  return errors;
}
```

---

### **Data Transformation Logic**

**Date Assembly (Frontend to Backend)**
```javascript
function assembleDate(year, month, day) {
  // Input: year="2023", month="06", day="15"
  // Output: "2023-06-15"
  return `${year}-${month.padStart(2, '0')}-${day.padStart(2, '0')}`;
}
```

**Date Parsing (Backend to Frontend)**
```javascript
function parseDate(dateString) {
  // Input: "2023-06-15"
  // Output: {year: "2023", month: "06", day: "15"}
  const [year, month, day] = dateString.split('-');
  return { year, month, day };
}
```

**Phone Number Assembly (Frontend to Backend)**
```javascript
function assemblePhoneNumber(areaCode, prefix, lineNumber) {
  // Input: areaCode="555", prefix="123", lineNumber="4567"
  // Output: "(555)123-4567"
  if (!areaCode && !prefix && !lineNumber) return null;
  return `(${areaCode})${prefix}-${lineNumber}`;
}
```

**Phone Number Parsing (Backend to Frontend)**
```javascript
function parsePhoneNumber(phoneString) {
  // Input: "(555)123-4567"
  // Output: {areaCode: "555", prefix: "123", lineNumber: "4567"}
  if (!phoneString) return { areaCode: '', prefix: '', lineNumber: '' };

  const match = phoneString.match(/\((\d{3})\)(\d{3})-(\d{4})/);
  if (!match) return { areaCode: '', prefix: '', lineNumber: '' };

  return {
    areaCode: match[1],
    prefix: match[2],
    lineNumber: match[3]
  };
}
```

**SSN Assembly (Frontend to Backend)**
```javascript
function assembleSSN(part1, part2, part3) {
  // Input: part1="123", part2="45", part3="6789"
  // Output: 123456789 (integer)
  return parseInt(`${part1}${part2}${part3}`);
}
```

**SSN Parsing (Backend to Frontend)**
```javascript
function parseSSN(ssnNumber) {
  // Input: 123456789 (integer)
  // Output: {part1: "123", part2: "45", part3: "6789"}
  const ssnString = ssnNumber.toString().padStart(9, '0');
  return {
    part1: ssnString.substring(0, 3),
    part2: ssnString.substring(3, 5),
    part3: ssnString.substring(5, 9)
  };
}
```

**Currency Formatting (Backend to Frontend)**
```javascript
function formatCurrency(amount) {
  // Input: 12345.67
  // Output: "12,345.67"
  return new Intl.NumberFormat('en-US', {
    minimumFractionDigits: 2,
    maximumFractionDigits: 2
  }).format(amount);
}
```

**Currency Parsing (Frontend to Backend)**
```javascript
function parseCurrency(currencyString) {
  // Input: "12,345.67" or "-12,345.67"
  // Output: 12345.67 or -12345.67
  const cleaned = currencyString.replace(/[,\s]/g, '');
  return parseFloat(cleaned);
}
```

---

### **Transaction Integrity**

**Two-Phase Commit Pattern**

The update process implements a two-phase commit pattern to ensure atomic updates across multiple tables:

**Phase 1: Lock Both Records**
```
1. BEGIN TRANSACTION
2. SELECT FROM accounts WHERE account_id = ? FOR UPDATE
3. SELECT FROM customers WHERE customer_id = ? FOR UPDATE
4. Validate optimistic locks (compare with old values)
```

**Phase 2: Update Both Records**
```
5. UPDATE accounts SET ... WHERE account_id = ?
6. UPDATE customers SET ... WHERE customer_id = ?
7. If both succeed: COMMIT TRANSACTION
8. If either fails: ROLLBACK TRANSACTION (undoes all changes)
```

**Critical Rules:**
- Both updates must succeed or both must fail (atomicity)
- No partial updates allowed
- Transaction isolation level must prevent dirty reads
- Locks must be held until commit/rollback completes

**Rollback Scenarios:**
1. Customer update fails after account succeeds → ROLLBACK (undoes account update)
2. Optimistic lock check fails → ROLLBACK (no updates made)
3. Lock acquisition fails → No transaction started, return error
4. Validation fails → No transaction started, return error

**Database Transaction Configuration:**
```sql
-- Set transaction isolation level
SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;

-- Or for stricter consistency
SET TRANSACTION ISOLATION LEVEL SERIALIZABLE;

-- Lock timeout (prevent indefinite waits)
SET lock_timeout = '5s';
```

---

### **Concurrency Control**

**Optimistic Locking Pattern Implementation**

**Step 1: Store Original Values on Fetch**
```
When GET /api/accounts/{accountId} is called:
  - Fetch data from database
  - Return data to frontend
  - Frontend stores as "oldValues" for later comparison
```

**Step 2: User Modifies Data**
```
User edits fields in UI
Frontend tracks which fields changed
Frontend prepares update request with both:
  - New values (updates)
  - Old values (for optimistic lock check)
```

**Step 3: Optimistic Lock Check During Update**
```
When PUT /api/accounts/{accountId} is called:
  1. Lock the records (FOR UPDATE)
  2. Compare EVERY field in locked records with oldValues
  3. Field-by-field comparison:

     Account fields to compare:
       - active_status
       - credit_limit
       - cash_credit_limit
       - open_date
       - expiration_date
       - reissue_date
       - account_group_id

     Customer fields to compare:
       - first_name, middle_name, last_name
       - address_line1, address_line2, address_line3
       - state_code, country_code, zip_code
       - phone_number1, phone_number2
       - ssn, govt_issued_id, date_of_birth
       - eft_account_id, primary_card_holder_ind, fico_credit_score

  4. If ANY field differs:
     - Set concurrentUpdateDetected = true
     - Store current values from database
     - ROLLBACK transaction
     - Return HTTP 409 Conflict with current data

  5. If no differences:
     - Proceed with update
     - COMMIT transaction
     - Return HTTP 200 Success
```

**Frontend Handling of Concurrent Update**
```
When receiving HTTP 409:
  1. Display message: "Data was changed by another user. Please review and retry."
  2. Replace form data with current values from response
  3. Highlight fields that changed
  4. User reviews current data
  5. User can make new edits and retry save
```

**Benefits of This Pattern:**
- Prevents lost updates (User A's changes don't overwrite User B's changes)
- No need for pessimistic locking during edit (better scalability)
- User is informed when conflicts occur
- User can review conflicting changes before deciding

**Lock Timeout Handling:**
```
If database lock cannot be acquired (another user is saving):
  - Wait up to lock_timeout (e.g., 5 seconds)
  - If timeout expires:
    - ROLLBACK transaction
    - Return HTTP 423 Locked
    - Message: "Could not lock account - another user is editing"
  - User can retry after a moment
```

---

### **Error Response Handling**

**HTTP Status Codes & Responses**

**200 - Success**
```json
{
  "success": true,
  "message": "Account updated successfully"
}
```

**400 - Validation Error**
```json
{
  "error": "Validation failed",
  "errors": [
    {
      "field": "creditLimit",
      "message": "Credit Limit is not valid",
      "value": "invalid-value"
    },
    {
      "field": "zipCode",
      "message": "Zip code is not valid for state CA",
      "value": "12345"
    }
  ]
}
```

**404 - Not Found**
```json
{
  "error": "Account not found",
  "accountId": "00000000001",
  "message": "No account exists with this ID"
}
```

**409 - Concurrent Update Detected**
```json
{
  "error": "Concurrent update detected",
  "message": "Data was changed by another user. Please review and retry.",
  "conflictingFields": ["creditLimit", "address_line1", "phoneNumber1"],
  "currentData": {
    "account": {
      "accountId": "00000000001",
      "creditLimit": 60000.00,
      // ... other current account values
    },
    "customer": {
      "customerId": 123456789,
      "addressLine1": "789 New Street",
      "phoneNumber1": "(555)999-8888",
      // ... other current customer values
    }
  }
}
```

**423 - Locked**
```json
{
  "error": "Record locked",
  "message": "Could not lock account for update - another user is editing",
  "retryAfter": 5
}
```

**500 - Server Error**
```json
{
  "error": "Internal server error",
  "message": "Database error occurred during update",
  "errorCode": "DB_UPDATE_FAILED",
  "timestamp": "2023-06-15T10:30:00Z"
}
```

**Error Logging:**
- All 500 errors must be logged with full stack trace
- Include: timestamp, user ID, session ID, request details, error details
- Alert operations team for repeated errors

---

## 3️⃣ DATABASE REQUIREMENTS

### **Table 1: accounts**

```sql
CREATE TABLE accounts (
  -- Primary Key
  account_id VARCHAR(11) PRIMARY KEY,

  -- Account Status
  active_status CHAR(1) NOT NULL
    CHECK (active_status IN ('Y', 'N')),

  -- Financial Information (all amounts in dollars with 2 decimal places)
  current_balance DECIMAL(12,2) NOT NULL DEFAULT 0.00,
  credit_limit DECIMAL(12,2) NOT NULL,
  cash_credit_limit DECIMAL(12,2) NOT NULL,

  -- Important Dates
  open_date DATE NOT NULL,
  expiration_date DATE NOT NULL,
  reissue_date DATE,

  -- Cycle Information
  current_cycle_credit DECIMAL(12,2) NOT NULL DEFAULT 0.00,
  current_cycle_debit DECIMAL(12,2) NOT NULL DEFAULT 0.00,

  -- Account Grouping
  account_group_id VARCHAR(10),

  -- Audit Columns (optional but recommended)
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  updated_by VARCHAR(20)
);

-- Indexes for Performance
CREATE INDEX idx_accounts_status ON accounts(active_status);
CREATE INDEX idx_accounts_group ON accounts(account_group_id);
CREATE INDEX idx_accounts_open_date ON accounts(open_date);

-- Comments
COMMENT ON TABLE accounts IS 'Stores account financial information including balances, limits, and important dates';
COMMENT ON COLUMN accounts.account_id IS '11-character account identifier (primary key)';
COMMENT ON COLUMN accounts.active_status IS 'Y=Active, N=Inactive';
COMMENT ON COLUMN accounts.current_balance IS 'Current account balance in dollars';
COMMENT ON COLUMN accounts.credit_limit IS 'Maximum credit limit in dollars';
COMMENT ON COLUMN accounts.cash_credit_limit IS 'Maximum cash advance limit in dollars';
```

---

### **Table 2: customers**

```sql
CREATE TABLE customers (
  -- Primary Key
  customer_id INTEGER PRIMARY KEY,

  -- Name Information
  first_name VARCHAR(25) NOT NULL,
  middle_name VARCHAR(25),
  last_name VARCHAR(25) NOT NULL,

  -- Address Information
  address_line1 VARCHAR(50),
  address_line2 VARCHAR(50),
  address_line3 VARCHAR(50),  -- City
  state_code CHAR(2)
    CHECK (LENGTH(state_code) = 2 AND state_code ~ '^[A-Z]{2}$'),
  country_code CHAR(3),
  zip_code VARCHAR(10),

  -- Contact Information
  phone_number1 VARCHAR(15),  -- Format: (999)999-9999
  phone_number2 VARCHAR(15),  -- Format: (999)999-9999

  -- Personal Identification
  ssn INTEGER,  -- 9 digits, stored as integer
  govt_issued_id VARCHAR(20),
  date_of_birth DATE NOT NULL
    CHECK (date_of_birth <= CURRENT_DATE),

  -- Financial Information
  eft_account_id VARCHAR(10),
  primary_card_holder_ind CHAR(1)
    CHECK (primary_card_holder_ind IN ('Y', 'N')),
  fico_credit_score SMALLINT
    CHECK (fico_credit_score BETWEEN 300 AND 850),

  -- Audit Columns (optional but recommended)
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  updated_by VARCHAR(20)
);

-- Indexes for Performance
CREATE INDEX idx_customers_ssn ON customers(ssn);
CREATE INDEX idx_customers_last_name ON customers(last_name);
CREATE INDEX idx_customers_dob ON customers(date_of_birth);
CREATE INDEX idx_customers_state ON customers(state_code);
CREATE INDEX idx_customers_fico ON customers(fico_credit_score);

-- Comments
COMMENT ON TABLE customers IS 'Stores customer demographic, contact, and personal information';
COMMENT ON COLUMN customers.customer_id IS '9-digit customer identifier (primary key)';
COMMENT ON COLUMN customers.ssn IS 'Social Security Number stored as 9-digit integer';
COMMENT ON COLUMN customers.phone_number1 IS 'Primary phone in format (999)999-9999';
COMMENT ON COLUMN customers.phone_number2 IS 'Secondary phone in format (999)999-9999';
COMMENT ON COLUMN customers.date_of_birth IS 'Date of birth, must not be in future';
COMMENT ON COLUMN customers.fico_credit_score IS 'FICO credit score, range 300-850';
```

---

### **Table 3: card_xref**

```sql
CREATE TABLE card_xref (
  -- Primary Key
  card_number VARCHAR(16) PRIMARY KEY,

  -- Foreign Keys
  customer_id INTEGER NOT NULL,
  account_id VARCHAR(11) NOT NULL,

  -- Audit Columns (optional but recommended)
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

  -- Foreign Key Constraints
  CONSTRAINT fk_card_xref_customer
    FOREIGN KEY (customer_id)
    REFERENCES customers(customer_id)
    ON DELETE RESTRICT
    ON UPDATE CASCADE,

  CONSTRAINT fk_card_xref_account
    FOREIGN KEY (account_id)
    REFERENCES accounts(account_id)
    ON DELETE RESTRICT
    ON UPDATE CASCADE
);

-- Indexes for Performance
CREATE INDEX idx_card_xref_account ON card_xref(account_id);
CREATE INDEX idx_card_xref_customer ON card_xref(customer_id);

-- Unique constraint to prevent duplicate relationships
CREATE UNIQUE INDEX idx_card_xref_unique ON card_xref(account_id, customer_id);

-- Comments
COMMENT ON TABLE card_xref IS 'Cross-reference table linking cards to accounts and customers';
COMMENT ON COLUMN card_xref.card_number IS '16-digit card number (primary key)';
COMMENT ON COLUMN card_xref.customer_id IS 'References customers table';
COMMENT ON COLUMN card_xref.account_id IS 'References accounts table';
```

---

### **Entity Relationships**

```
Relationships:
┌──────────────┐       ┌──────────────┐       ┌──────────────┐
│   accounts   │       │  card_xref   │       │  customers   │
│              │       │              │       │              │
│ account_id ◄─┼───────┼─ account_id  │       │              │
│ (PK)         │       │  card_number │       │              │
│              │       │  (PK)        │       │              │
│              │       │  customer_id ├───────┼─► customer_id│
│              │       │              │       │   (PK)       │
└──────────────┘       └──────────────┘       └──────────────┘

Cardinality:
- One account → Many cards (via card_xref)
- One customer → Many cards (via card_xref)
- Account ↔ Customer: Many-to-many relationship through card_xref

Business Rules:
- An account can have multiple cards
- A customer can have multiple cards
- A card belongs to exactly one account and one customer
- Cannot delete account or customer if cards reference them (RESTRICT)
```

---

### **Database Constraints**

**Primary Key Constraints:**
- `accounts.account_id` - 11-character string
- `customers.customer_id` - 9-digit integer
- `card_xref.card_number` - 16-character string

**Foreign Key Constraints:**
- `card_xref.account_id` → `accounts.account_id` (ON DELETE RESTRICT, ON UPDATE CASCADE)
- `card_xref.customer_id` → `customers.customer_id` (ON DELETE RESTRICT, ON UPDATE CASCADE)

**Check Constraints:**
- `accounts.active_status` IN ('Y', 'N')
- `customers.primary_card_holder_ind` IN ('Y', 'N')
- `customers.fico_credit_score` BETWEEN 300 AND 850
- `customers.date_of_birth` <= CURRENT_DATE
- `customers.state_code` LENGTH = 2 AND format [A-Z]{2}

**Unique Constraints:**
- `card_xref(account_id, customer_id)` - Prevent duplicate relationships

**NOT NULL Constraints:**
- `accounts`: account_id, active_status, current_balance, credit_limit, cash_credit_limit, open_date, expiration_date, current_cycle_credit, current_cycle_debit
- `customers`: customer_id, first_name, last_name, date_of_birth
- `card_xref`: card_number, customer_id, account_id

---

### **Database Indexes**

**Performance Indexes:**
```sql
-- For account lookups
CREATE INDEX idx_accounts_status ON accounts(active_status);
CREATE INDEX idx_accounts_group ON accounts(account_group_id);
CREATE INDEX idx_accounts_open_date ON accounts(open_date);

-- For customer lookups
CREATE INDEX idx_customers_ssn ON customers(ssn);
CREATE INDEX idx_customers_last_name ON customers(last_name);
CREATE INDEX idx_customers_dob ON customers(date_of_birth);
CREATE INDEX idx_customers_state ON customers(state_code);
CREATE INDEX idx_customers_fico ON customers(fico_credit_score);

-- For card cross-reference lookups
CREATE INDEX idx_card_xref_account ON card_xref(account_id);
CREATE INDEX idx_card_xref_customer ON card_xref(customer_id);

-- Composite index for common JOIN query
CREATE INDEX idx_card_xref_composite ON card_xref(account_id, customer_id);
```

**Index Usage:**
- `idx_card_xref_account`: Used in JOIN query to find customer by account
- `idx_card_xref_customer`: Used to find all cards for a customer
- `idx_customers_ssn`: Used for SSN lookups
- `idx_customers_last_name`: Used for name searches
- `idx_accounts_status`: Used to filter active/inactive accounts

---

### **Sample Data Requirements**

**Valid Account Statuses:**
- 'Y' = Active
- 'N' = Inactive

**Valid Primary Card Holder Indicators:**
- 'Y' = Yes (primary card holder)
- 'N' = No (secondary card holder)

**FICO Score Ranges:**
- 300-579: Poor
- 580-669: Fair
- 670-739: Good
- 740-799: Very Good
- 800-850: Exceptional

**Date Formats:**
- All dates stored as DATE type in YYYY-MM-DD format
- Open date, expiration date, reissue date for accounts
- Date of birth for customers

**Phone Number Format:**
- Stored as VARCHAR(15) in format: (999)999-9999
- Example: (555)123-4567

**Currency Precision:**
- All monetary amounts stored as DECIMAL(12,2)
- Supports values up to 9,999,999,999.99
- Always 2 decimal places

---

## 4️⃣ BUSINESS RULES (Chronological Execution Order)

This section describes the business logic flow in the order it executes from program start to completion.

---

### **Step 1: Program Initialization**

**Trigger:** Application starts or user navigates to Account Update page

**Actions:**
1. Establish error handling mechanisms
2. Initialize all working variables to default values
3. Clear error flags and message areas
4. Set initial state to `INITIAL_ENTRY`
5. Store transaction context (calling program, user session)
6. Check user permissions (ensure user can access this function)

**Validation:** None at this stage

**Output:** Blank form with Account Number field ready for input

---

### **Step 2: Session State Restoration**

**Trigger:** User returns to page (not first entry)

**Actions:**
1. Check if session data exists
2. If initial entry (no session data):
   - Initialize new session
   - Set state to `INITIAL_ENTRY`
   - Display blank form
3. If re-entry (session data exists):
   - Restore previous state from session storage
   - Restore fetched account/customer data (old values)
   - Restore user modifications (new values)
   - Restore validation flags
   - Restore current UI state indicator
   - Resume from previous state

**Session Data Includes:**
- Fetched account data (original values)
- Fetched customer data (original values)
- User modifications (new values)
- Validation results (field-level flags)
- Current state (INITIAL_ENTRY, DATA_DISPLAYED, etc.)
- Navigation context (calling program, breadcrumbs)
- Error messages

**Validation:** None at this stage

**Output:** Restored form in previous state or blank form if initial entry

---

### **Step 3: User Action Processing**

**Trigger:** User presses a button or key

**Actions:**
1. Capture user action:
   - Enter key → Action code = 'ENTER'
   - F3/Exit key → Action code = 'EXIT'
   - F5/Save key → Action code = 'SAVE'
   - F12/Cancel key → Action code = 'CANCEL'
2. Validate action is allowed in current state:
   - F5 only allowed in `VALIDATION_PASSED` state
   - F12 only allowed after data is fetched
   - Enter allowed in most states
   - F3 allowed in all states
3. If invalid action for current state:
   - Treat as Enter key
   - Continue to next step

**Validation:** Check if action is valid for current state

**Output:** Action code for routing to appropriate handler

---

### **Step 4: Route to Appropriate Handler**

**Trigger:** Action code determined in Step 3

**Routing Logic:**
1. If action = 'EXIT':
   - Go to Exit Processing (Step 5)
2. If state = `INITIAL_ENTRY` AND action = 'ENTER':
   - Go to Initial Display Processing (Step 6)
3. If state = `INITIAL_ENTRY` AND action = anything else:
   - Stay in Initial Display (Step 6)
4. For all other combinations:
   - Go to Normal Processing Flow (Step 7)

**Validation:** None at this stage

**Output:** Route to next processing step

---

### **Step 5: Exit Processing**

**Trigger:** User pressed F3/Exit key

**Actions:**
1. Check if unsaved changes exist:
   - If changes exist AND not yet saved:
     - Display confirmation dialog
     - "You have unsaved changes. Are you sure you want to exit?"
     - Wait for user response
     - If user confirms exit → Continue
     - If user cancels → Return to current screen
2. Determine target destination:
   - If calling program specified in session:
     - Target = calling program
   - Else:
     - Target = main menu (default)
3. Update navigation context:
   - Set from_program = current program
   - Set to_program = target program
   - Update navigation breadcrumbs
4. Save session state (in case user returns)
5. Commit any pending changes (if auto-save enabled)
6. Transfer control to target program/screen

**Validation:** None, but confirm unsaved changes if present

**Output:** Navigate to target screen

---

### **Step 6: Initial Display Processing**

**Trigger:** First time on screen or reset after completion

**Conditions:**
- State = `INITIAL_ENTRY`
- No account data fetched yet

**Actions:**
1. Initialize all form fields to blank/default values
2. Clear error messages
3. Clear info messages
4. Display blank form
5. Enable only Account Number field for input
6. Disable all other fields
7. Enable Enter button and F3 Exit button
8. Disable F5 Save and F12 Cancel buttons
9. Display prompt: "Enter Account Number and press Enter"
10. Position cursor in Account Number field
11. Set state to `INITIAL_ENTRY`
12. Wait for user input

**Validation:** None at this stage

**Output:** Blank form ready for account number entry

---

### **Step 7: Input Reception (Enter Pressed)**

**Trigger:** User presses Enter key

**Actions:**
1. Capture all screen field values from form
2. For each field on the form:
   a. Read current value from UI component
   b. Check if value is blank, asterisk, or unchanged indicator
   c. If blank/asterisk → Store as "no change"
   d. If has value → Store as "new value"
3. Special handling for Account Number field:
   - Always capture account number value
   - Store in both display and working storage
   - If in `INITIAL_ENTRY` state (no data fetched yet):
     - Exit reception immediately
     - Only account number is needed
     - Skip capturing other fields
4. Special handling for numeric fields (credit limits, FICO score):
   - Capture display value
   - Validate numeric format using regex or parse function
   - If valid → Convert to numeric and store
   - If invalid → Store error indicator
5. Special handling for date fields (open date, expiration date, DOB):
   - Capture three separate components: year, month, day
   - Store each component separately
   - Do not assemble into date string yet (validation will do that)
6. Special handling for phone numbers:
   - Capture three separate components: area code, prefix, line
   - Store each component separately
   - Do not assemble into phone string yet (validation will do that)
7. Special handling for SSN:
   - Capture three separate components: part1, part2, part3
   - Store each component separately (masked if needed)
   - Do not assemble into SSN number yet (validation will do that)
8. Repeat for all 40+ fields on the form

**Data Captured:**
- Account Number (11 chars)
- Account Status (1 char)
- Credit Limit (display format)
- Cash Credit Limit (display format)
- Open Date (year, month, day separately)
- Expiration Date (year, month, day separately)
- Reissue Date (year, month, day separately)
- Account Group ID (10 chars)
- Customer First/Middle/Last Name (25 chars each)
- Address Lines 1, 2, 3 (50 chars each)
- State Code (2 chars)
- Country Code (3 chars)
- Zip Code (10 chars)
- Phone 1 (area, prefix, line separately)
- Phone 2 (area, prefix, line separately)
- SSN (3 parts separately)
- Government ID (20 chars)
- Date of Birth (year, month, day separately)
- EFT Account ID (10 chars)
- Primary Card Holder Indicator (1 char)
- FICO Score (3 digits)

**Validation:** Basic format validation for numeric fields only

**Output:** All user input stored in working variables

---

### **Step 8: Input Validation**

**Trigger:** Immediately after Step 7 (Input Reception)

**Actions:**

**8.1 Initialize Validation**
1. Set validation status flag = OK
2. Clear all field-level error flags
3. Clear error message collection

**8.2 Determine Validation Scope**
1. If state = `INITIAL_ENTRY` (account not fetched yet):
   - Validate ONLY the Account Number field
   - Skip all other field validation
   - Go to Step 8.3
2. If state = `DATA_DISPLAYED` or later (account already fetched):
   - Set all "found" flags (account found, customer found, etc.)
   - Go to Step 8.4

**8.3 Account Number Validation (Initial Search)**
1. Check if Account Number is provided:
   - If blank → Set error: "Account Number must be supplied"
   - Set validation status = ERROR
2. Check if Account Number is 11 characters:
   - If not 11 chars → Set error: "Account Number must be 11 digits"
   - Set validation status = ERROR
3. Check if Account Number is numeric:
   - Use regex: /^\d{11}$/
   - If not numeric → Set error: "Account Number must be numeric"
   - Set validation status = ERROR
4. Check if Account Number is not all zeros:
   - If equals "00000000000" → Set error: "Account Number must not be zero"
   - Set validation status = ERROR
5. Initialize old account data area to empty
6. Exit validation (skip to Step 9)

**8.4 Compare Old vs New Values (Change Detection)**
1. Perform field-by-field comparison:
   - For each account field: Compare old value vs new value
   - For each customer field: Compare old value vs new value
2. Track changes:
   - If any field differs: Set `CHANGES_FOUND` flag = TRUE
   - Set corresponding field change flag for each changed field
   - Example: If credit_limit changed → Set `credit_limit_changed` = TRUE
3. If NO changes found:
   - Display info message: "No changes detected"
   - Clear validation status (no need to validate)
   - Set state back to `DATA_DISPLAYED`
   - Exit validation (skip to Step 9)
4. If changes found OR already in confirmation/error state:
   - Continue to field validation

**8.5 Validate Changed Fields**

For each field that was changed (new value differs from old value):

**Account Status Validation:**
1. Check if provided
2. Check if 'Y' or 'N'
3. Errors: "Account Status must be supplied", "Account Status must be Y or N"

**Date Field Validation (Open Date, Expiration Date, Reissue Date, DOB):**
1. Validate year component:
   - Must be 4 digits
   - Must be numeric
   - Must be in range 1900-2099
2. Validate month component:
   - Must be 2 digits
   - Must be numeric
   - Must be in range 01-12
3. Validate day component:
   - Must be 2 digits
   - Must be numeric
   - Must be valid for the month (handle leap years)
   - Example: If month=02 and day=30 → Invalid
4. Special rule for Date of Birth:
   - Assembled date must not be in future
   - Compare against current date
5. Errors per component:
   - "[Field] year must be supplied"
   - "[Field] year must be 4 digits"
   - "[Field] year must be between 1900 and 2099"
   - "[Field] month must be between 01 and 12"
   - "[Field] day must be valid for the month"
   - "Date of Birth cannot be in the future"

**Currency Field Validation (Credit Limit, Cash Credit Limit):**
1. Check if provided
2. Remove commas and spaces from display value
3. Check format using regex: /^-?\d+(\.\d{2})?$/
4. If valid → Convert to decimal number
5. Errors: "[Field] must be supplied", "[Field] is not valid"

**Customer Name Validation (First Name, Last Name):**
1. Check if provided (required)
2. Strip non-alphabetic characters except spaces
3. Check if result is not blank
4. Errors: "[Field] must be supplied", "[Field] can have alphabets only"

**Middle Name Validation:**
1. Optional field
2. If provided, strip non-alphabetic characters except spaces
3. Error: "[Field] can have alphabets only"

**Address Field Validation (Address Lines 1, 2, 3):**
1. Optional fields
2. If provided, can contain alphabetic characters and spaces
3. Error: "[Field] can have alphabets only" (if invalid chars)

**State Code Validation:**
1. Check if provided
2. Check if exactly 2 characters
3. Check if alphabetic
4. Check if valid US state code (lookup in state table)
5. Errors: "State Code must be supplied", "State Code is not a valid US state"

**Zip Code Validation:**
1. Optional field
2. If provided, validate format (5 digits or 5+4 format)

**State/Zip Cross-Validation:**
1. If both state and zip provided:
   - Lookup valid zip prefixes for state
   - Check if zip is valid for state
   - Error: "Zip code is not valid for state [XX]"

**SSN Validation:**
1. Validate part 1:
   - Must be 3 digits
   - Cannot be "000"
   - Errors: "SSN Part 1 must be 3 digits", "SSN Part 1 cannot be 000"
2. Validate part 2:
   - Must be 2 digits
   - Cannot be "00"
   - Errors: "SSN Part 2 must be 2 digits", "SSN Part 2 cannot be 00"
3. Validate part 3:
   - Must be 4 digits
   - Cannot be "0000"
   - Errors: "SSN Part 3 must be 4 digits", "SSN Part 3 cannot be 0000"

**Phone Number Validation (Phone 1 and Phone 2):**
1. Check if all parts provided or all parts empty (all-or-nothing rule)
2. If any part provided but not all:
   - Error: "[Field]: All parts required if any part provided"
3. If all parts empty:
   - Valid (optional field)
4. If all parts provided:
   - Validate area code:
     - Must be 3 digits
     - Must be valid North American area code
     - Errors: "[Field] area code must be 3 digits", "[Field] area code must be valid"
   - Validate prefix:
     - Must be 3 digits
     - Cannot be "000"
     - Errors: "[Field] prefix must be 3 digits", "[Field] prefix cannot be 000"
   - Validate line number:
     - Must be 4 digits
     - Error: "[Field] line number must be 4 digits"

**FICO Score Validation:**
1. Check if provided (required)
2. Check if numeric
3. Check if in range 300-850
4. Errors: "FICO Score must be supplied", "FICO Score must be numeric", "FICO Score must be between 300 and 850"

**8.6 Accumulate Validation Results**
1. For each validation rule that failed:
   - Add error to error collection
   - Set corresponding field error flag
   - Set overall validation status = ERROR
2. Track first error field for cursor positioning

**8.7 Determine Next State Based on Validation**
1. If validation status = ERROR:
   - Set state to `VALIDATION_FAILED`
   - Prepare to display errors (Step 9)
2. If validation status = OK AND no changes detected:
   - Set state to `DATA_DISPLAYED`
   - Display info message: "No changes detected"
   - Exit (Step 9)
3. If validation status = OK AND changes detected:
   - Set state to `VALIDATION_PASSED`
   - Enable F5 Save button
   - Display prompt: "Press F5 to save or F12 to cancel"
   - Exit (Step 9)

**Validation:** This entire step IS the validation

**Output:**
- Validation status (OK or ERROR)
- Collection of field-level errors (if any)
- Updated state flag
- Field error indicators

---

### **Step 9: Decision Logic**

**Trigger:** After validation completes

**Decision Tree:**

**9.1 If Account Not Yet Fetched (INITIAL_ENTRY state)**
1. Check if account number is valid (from Step 8.3):
   - If invalid → Stay in `INITIAL_ENTRY`, display error, exit
   - If valid → Continue to Step 9.2
2. Execute account data fetch (go to Step 10)
3. If fetch succeeds:
   - Set state to `DATA_DISPLAYED`
   - Display all fetched data
   - Enable F12 Cancel button
   - Exit decision logic
4. If fetch fails (account not found):
   - Stay in `INITIAL_ENTRY`
   - Display error: "Account not found"
   - Exit decision logic

**9.2 If Changes Made and Valid (VALIDATION_PASSED state)**
1. Display prompt: "Press F5 to save or F12 to cancel"
2. Enable F5 Save button
3. Enable F12 Cancel button
4. Wait for user action
5. Exit decision logic

**9.3 If User Confirms Save (F5 pressed in VALIDATION_PASSED state)**
1. Execute update processing (go to Step 11)
2. Evaluate update results:

   **Scenario A: Could Not Lock Account**
   - Set state to `UPDATE_FAILED`
   - Display error: "Could not lock account for update"
   - Keep current data displayed
   - Exit decision logic

   **Scenario B: Locked But Update Failed**
   - Set state to `UPDATE_FAILED`
   - Display error: "Update failed - database error"
   - Keep current data displayed
   - Exit decision logic

   **Scenario C: Concurrent Update Detected**
   - Another user modified the data
   - Set state to `DATA_DISPLAYED`
   - Display warning: "Data was changed by another user. Please review and retry."
   - Reload current data from database
   - Highlight changed fields
   - User must review and decide
   - Exit decision logic

   **Scenario D: Update Succeeded**
   - Set state to `UPDATE_SUCCESS`
   - Display success: "Account Updated Successfully"
   - Reset to `INITIAL_ENTRY` state
   - Clear all fields
   - Exit decision logic

**9.4 If Validation Failed (VALIDATION_FAILED state)**
1. Display all field-level errors
2. Highlight error fields in red
3. Position cursor at first error field
4. Display error message at bottom
5. Stay in `VALIDATION_FAILED` state
6. User can correct errors and press Enter to re-validate
7. Exit decision logic

**9.5 If Cancel Pressed (F12 in any state after data fetched)**
1. Discard all changes
2. Reload original data (old values)
3. Set state to `DATA_DISPLAYED`
4. Display info: "Changes discarded"
5. Exit decision logic

**9.6 Unexpected State**
1. Log error: "Unexpected program state"
2. Display error to user
3. Offer options: Exit or Contact Support
4. Exit decision logic

**Validation:** None, this step routes based on validation results

**Output:** Route to appropriate next step (fetch, update, or display)

---

### **Step 10: Account Data Fetch Process**

**Trigger:** Account number validated in Step 8.3 and Step 9.1 routed here

**Actions:**

**10.1 Initialize Fetch Process**
1. Clear old data storage areas
2. Set "no info" message flag
3. Store account number in working variables
4. Prepare for three-table fetch

**10.2 Execute Single JOIN Query (Repository Method)**

Repository method: `AccountRepository.getAccountWithCustomer(accountId)`

Execute SQL query:
```sql
SELECT
  a.account_id, a.active_status, a.current_balance, a.credit_limit,
  a.cash_credit_limit, a.open_date, a.expiration_date, a.reissue_date,
  a.current_cycle_credit, a.current_cycle_debit, a.account_group_id,
  c.customer_id, c.first_name, c.middle_name, c.last_name,
  c.address_line1, c.address_line2, c.address_line3,
  c.state_code, c.country_code, c.zip_code,
  c.phone_number1, c.phone_number2, c.ssn, c.govt_issued_id,
  c.date_of_birth, c.eft_account_id, c.primary_card_holder_ind,
  c.fico_credit_score,
  cx.card_number
FROM card_xref cx
INNER JOIN accounts a ON cx.account_id = a.account_id
INNER JOIN customers c ON cx.customer_id = c.customer_id
WHERE cx.account_id = ?
LIMIT 1;
```

Parameters: [accountId]

**10.3 Handle Query Results**

**If no rows returned (account not found):**
1. Set error flag: `ACCOUNT_NOT_FOUND`
2. Display error: "Account not found"
3. Stay in `INITIAL_ENTRY` state
4. Exit fetch process

**If database error occurs:**
1. Log error details
2. Set error flag: `DATABASE_ERROR`
3. Display error: "Database error occurred"
4. Stay in `INITIAL_ENTRY` state
5. Exit fetch process

**If query succeeds (row returned):**
1. Continue to Step 10.4

**10.4 Store Fetched Account Data**

Map query results to working storage (old values):
1. Store account fields:
   - account_id → old_account_id
   - active_status → old_active_status
   - current_balance → old_current_balance
   - credit_limit → old_credit_limit
   - cash_credit_limit → old_cash_credit_limit
   - open_date → old_open_date (store as-is)
   - expiration_date → old_expiration_date (store as-is)
   - reissue_date → old_reissue_date (store as-is)
   - current_cycle_credit → old_current_cycle_credit
   - current_cycle_debit → old_current_cycle_debit
   - account_group_id → old_account_group_id

**10.5 Store Fetched Customer Data**

2. Store customer fields:
   - customer_id → old_customer_id
   - first_name → old_first_name
   - middle_name → old_middle_name
   - last_name → old_last_name
   - address_line1 → old_address_line1
   - address_line2 → old_address_line2
   - address_line3 → old_address_line3
   - state_code → old_state_code
   - country_code → old_country_code
   - zip_code → old_zip_code
   - phone_number1 → old_phone_number1 (store as-is)
   - phone_number2 → old_phone_number2 (store as-is)
   - ssn → old_ssn (store as integer)
   - govt_issued_id → old_govt_issued_id
   - date_of_birth → old_date_of_birth (store as-is)
   - eft_account_id → old_eft_account_id
   - primary_card_holder_ind → old_primary_card_holder_ind
   - fico_credit_score → old_fico_credit_score

**10.6 Parse Complex Fields for Display**

3. Parse date fields for display (split YYYY-MM-DD):
   - open_date: "2020-01-15" → year="2020", month="01", day="15"
   - expiration_date: "2025-01-15" → year="2025", month="01", day="15"
   - reissue_date: "2023-01-15" → year="2023", month="01", day="15"
   - date_of_birth: "1980-05-20" → year="1980", month="05", day="20"

4. Parse phone numbers for display (split (999)999-9999):
   - phone_number1: "(555)123-4567" → area="555", prefix="123", line="4567"
   - phone_number2: "(555)987-6543" → area="555", prefix="987", line="6543"

5. Parse SSN for display (split 9-digit integer):
   - ssn: 123456789 → part1="123", part2="45", part3="6789"

**10.7 Copy to Display Areas**
6. Copy all old values to display areas (for showing on screen)
7. Copy all old values to new values areas (starting point for editing)

**10.8 Update State and UI**
8. Set state to `DATA_DISPLAYED`
9. Set found flags:
   - `ACCOUNT_FOUND` = TRUE
   - `CUSTOMER_FOUND` = TRUE
   - `DATA_FETCHED` = TRUE
10. Enable F12 Cancel button
11. Enable all editable fields for user input
12. Display success message: "Account data loaded"
13. Exit fetch process

**Validation:** None, but checks for database errors

**Output:**
- All account and customer data stored in old values
- Data displayed on screen
- State changed to `DATA_DISPLAYED`
- Ready for user to edit

---

### **Step 11: Update Processing**

**Trigger:** User pressed F5 in `VALIDATION_PASSED` state (Step 9.3)

**Actions:**

**11.1 Begin Transaction**
1. Start database transaction:
   ```sql
   BEGIN TRANSACTION;
   SET TRANSACTION ISOLATION LEVEL REPEATABLE READ;
   ```
2. Set lock timeout (e.g., 5 seconds)

**11.2 Lock Account Record**
1. Execute lock query:
   ```sql
   SELECT * FROM accounts
   WHERE account_id = ?
   FOR UPDATE;
   ```
2. Check lock result:
   - If timeout or lock error:
     - Set flag: `COULD_NOT_LOCK_ACCOUNT`
     - Display error: "Could not lock account for update"
     - ROLLBACK transaction
     - Exit update process
   - If success:
     - Store locked account record
     - Continue to Step 11.3

**11.3 Lock Customer Record**
1. Get customer_id from card_xref:
   ```sql
   SELECT customer_id FROM card_xref WHERE account_id = ?;
   ```
2. Execute lock query:
   ```sql
   SELECT * FROM customers
   WHERE customer_id = ?
   FOR UPDATE;
   ```
3. Check lock result:
   - If timeout or lock error:
     - Set flag: `COULD_NOT_LOCK_CUSTOMER`
     - Display error: "Could not lock customer for update"
     - ROLLBACK transaction
     - Exit update process
   - If success:
     - Store locked customer record
     - Continue to Step 11.4

**11.4 Optimistic Concurrency Check**

Compare locked records with old values (field-by-field):

**Account Fields to Compare:**
- active_status
- credit_limit
- cash_credit_limit
- open_date
- expiration_date
- reissue_date
- account_group_id

**Customer Fields to Compare:**
- first_name
- middle_name
- last_name
- address_line1
- address_line2
- address_line3
- state_code
- country_code
- zip_code
- phone_number1
- phone_number2
- ssn
- govt_issued_id
- date_of_birth
- eft_account_id
- primary_card_holder_ind
- fico_credit_score

**For Each Field:**
1. Compare locked value with old value
2. If ANY field differs:
   - Set flag: `DATA_CHANGED_BY_ANOTHER_USER`
   - Store current values from locked records
   - ROLLBACK transaction
   - Display warning: "Data was changed by another user. Please review and retry."
   - Set state to `DATA_DISPLAYED`
   - Display current data from database
   - Highlight fields that changed
   - Exit update process

3. If all fields match:
   - Continue to Step 11.5

**11.5 Prepare Update Records**

**Build Account Update Object:**
1. Map changed account fields from new values
2. Reassemble date fields:
   - open_date: year + "-" + month + "-" + day → "YYYY-MM-DD"
   - expiration_date: year + "-" + month + "-" + day → "YYYY-MM-DD"
   - reissue_date: year + "-" + month + "-" + day → "YYYY-MM-DD"
3. Validate all fields one final time (server-side validation)
4. If validation fails:
   - ROLLBACK transaction
   - Return to validation error state
   - Exit update process

**Build Customer Update Object:**
1. Map changed customer fields from new values
2. Reassemble date fields:
   - date_of_birth: year + "-" + month + "-" + day → "YYYY-MM-DD"
3. Reassemble phone numbers:
   - phone_number1: "(" + area + ")" + prefix + "-" + line → "(555)123-4567"
   - phone_number2: "(" + area + ")" + prefix + "-" + line → "(555)987-6543"
4. Reassemble SSN:
   - ssn: part1 + part2 + part3 → 123456789 (integer)
5. Validate all fields one final time (server-side validation)
6. If validation fails:
   - ROLLBACK transaction
   - Return to validation error state
   - Exit update process

**11.6 Update Account Record**
1. Execute update query:
   ```sql
   UPDATE accounts
   SET active_status = ?,
       credit_limit = ?,
       cash_credit_limit = ?,
       open_date = ?,
       expiration_date = ?,
       reissue_date = ?,
       account_group_id = ?,
       updated_at = CURRENT_TIMESTAMP,
       updated_by = ?
   WHERE account_id = ?;
   ```
2. Check update result:
   - If error occurs:
     - Set flag: `ACCOUNT_UPDATE_FAILED`
     - Display error: "Update failed - account record"
     - ROLLBACK transaction
     - Exit update process
   - If success:
     - Continue to Step 11.7

**11.7 Update Customer Record**
1. Execute update query:
   ```sql
   UPDATE customers
   SET first_name = ?,
       middle_name = ?,
       last_name = ?,
       address_line1 = ?,
       address_line2 = ?,
       address_line3 = ?,
       state_code = ?,
       country_code = ?,
       zip_code = ?,
       phone_number1 = ?,
       phone_number2 = ?,
       govt_issued_id = ?,
       date_of_birth = ?,
       eft_account_id = ?,
       primary_card_holder_ind = ?,
       fico_credit_score = ?,
       updated_at = CURRENT_TIMESTAMP,
       updated_by = ?
   WHERE customer_id = ?;
   ```
2. Check update result:
   - If error occurs:
     - Set flag: `CUSTOMER_UPDATE_FAILED`
     - Display error: "Update failed - customer record"
     - ROLLBACK transaction (undoes account update too!)
     - Exit update process
   - If success:
     - Continue to Step 11.8

**11.8 Commit Transaction**
1. Execute commit:
   ```sql
   COMMIT TRANSACTION;
   ```
2. Locks are released automatically
3. Both records updated atomically
4. Continue to Step 11.9

**11.9 Post-Update Actions**
1. Display success message: "Account Updated Successfully"
2. Log audit trail:
   - User ID who made the change
   - Timestamp
   - Account ID and Customer ID
   - Which fields were changed (old → new values)
   - Source IP address
3. Set state to `UPDATE_SUCCESS`
4. Auto-transition to `INITIAL_ENTRY` state
5. Clear all fields
6. Reset form for next account
7. Exit update process

**Validation:** Final server-side validation in Step 11.5

**Output:**
- Success: Account and customer updated atomically
- OR Error: Transaction rolled back, error message displayed

---

### **Step 12: Change Detection (Detailed)**

**Trigger:** Part of Step 8.4 validation process

**Purpose:** Determine which fields have changed to avoid unnecessary database updates

**Actions:**

1. Initialize change tracking:
   - Set `CHANGES_FOUND` = FALSE
   - Clear all individual field change flags

2. Compare Account Fields (old vs new):
   ```
   IF old_active_status ≠ new_active_status THEN
     SET active_status_changed = TRUE
     SET CHANGES_FOUND = TRUE

   IF old_credit_limit ≠ new_credit_limit THEN
     SET credit_limit_changed = TRUE
     SET CHANGES_FOUND = TRUE

   IF old_cash_credit_limit ≠ new_cash_credit_limit THEN
     SET cash_credit_limit_changed = TRUE
     SET CHANGES_FOUND = TRUE

   [... repeat for all account fields ...]
   ```

3. Compare Customer Fields (old vs new):
   ```
   IF old_first_name ≠ new_first_name THEN
     SET first_name_changed = TRUE
     SET CHANGES_FOUND = TRUE

   IF old_last_name ≠ new_last_name THEN
     SET last_name_changed = TRUE
     SET CHANGES_FOUND = TRUE

   [... repeat for all customer fields ...]
   ```

4. Handle Date Field Comparison:
   - For dates, compare reassembled values:
   ```
   old_date = old_year + "-" + old_month + "-" + old_day
   new_date = new_year + "-" + new_month + "-" + new_day
   IF old_date ≠ new_date THEN
     SET date_field_changed = TRUE
     SET CHANGES_FOUND = TRUE
   ```

5. Handle Phone Number Comparison:
   - For phones, compare reassembled values:
   ```
   old_phone = "(" + old_area + ")" + old_prefix + "-" + old_line
   new_phone = "(" + new_area + ")" + new_prefix + "-" + new_line
   IF old_phone ≠ new_phone THEN
     SET phone_field_changed = TRUE
     SET CHANGES_FOUND = TRUE
   ```

6. Handle SSN Comparison:
   ```
   old_ssn_assembled = old_part1 + old_part2 + old_part3
   new_ssn_assembled = new_part1 + new_part2 + new_part3
   IF old_ssn_assembled ≠ new_ssn_assembled THEN
     SET ssn_changed = TRUE
     SET CHANGES_FOUND = TRUE
   ```

7. Evaluate Results:
   - If `CHANGES_FOUND` = FALSE:
     - Display info message: "No changes detected"
     - Skip update process
     - Return to `DATA_DISPLAYED` state
   - If `CHANGES_FOUND` = TRUE:
     - Log which fields changed (for audit trail)
     - Continue with validation and update process

**Validation:** None, this is detection only

**Output:**
- Boolean flag: changes found or not
- List of specific fields that changed
- Used to skip unnecessary updates
- Used for audit logging

---

### **Step 13: Concurrent Update Detection (Detailed)**

**Trigger:** Part of Step 11.4 update process

**Purpose:** Detect if another user modified the data between fetch and update (optimistic locking)

**Actions:**

1. For each account field, compare:
   ```
   locked_value = value from SELECT FOR UPDATE query
   old_value = value stored when data was fetched

   IF locked_value ≠ old_value THEN
     SET concurrent_update_detected = TRUE
     TRACK field_name in conflicting_fields list
   ```

2. For each customer field, compare:
   ```
   locked_value = value from SELECT FOR UPDATE query
   old_value = value stored when data was fetched

   IF locked_value ≠ old_value THEN
     SET concurrent_update_detected = TRUE
     TRACK field_name in conflicting_fields list
   ```

3. If ANY field differs:
   - **Concurrent Update Detected!**
   - Actions:
     a. Store current values from locked records
     b. ROLLBACK transaction (release locks)
     c. Prepare response with current data
     d. Display warning message:
        "Data was changed by another user. Please review and retry."
     e. Return HTTP 409 Conflict with:
        - Current values from database
        - List of conflicting fields
        - Timestamp of last update
     f. Frontend will:
        - Display current data
        - Highlight conflicting fields
        - Clear user's changes
        - User must review and decide:
          - Accept current data and make new edits
          - Or abandon changes

4. If no differences found:
   - Data has not changed since fetch
   - Safe to proceed with update
   - Continue to Step 11.5

**Example Scenario:**
```
Time 0: User A fetches account (credit_limit = 50000)
Time 0: User B fetches account (credit_limit = 50000)
Time 1: User A changes credit_limit to 60000 and saves
Time 1: User A's update succeeds (credit_limit now 60000 in DB)
Time 2: User B changes credit_limit to 55000 and saves
Time 2: User B's update is attempted
Time 2: Lock acquired, credit_limit in DB = 60000
Time 2: Compare: DB has 60000 but User B's old value was 50000
Time 2: Mismatch detected! → Concurrent update detected
Time 2: ROLLBACK User B's transaction
Time 2: Return current data (credit_limit = 60000) to User B
Time 2: User B sees: "Data changed by another user"
Time 2: User B reviews: credit_limit is now 60000 (User A's value)
Time 3: User B decides whether to keep 60000 or change to 55000
```

**Validation:** Comparing database state vs stored old values

**Output:**
- Boolean: concurrent update detected or not
- List of conflicting fields
- Current data from database
- HTTP 409 response if conflict detected

---

### **Step 14: Error Recovery**

**Trigger:** Any error occurs in Steps 10, 11, or 13

**Error Scenarios and Recovery Actions:**

**14.1 Record Not Found (Step 10)**
- Error: Account number not in database
- Recovery Actions:
  1. Display error: "Account not found"
  2. Keep focus on Account Number field
  3. Stay in `INITIAL_ENTRY` state
  4. User can try different account number
  5. Do NOT clear entered account number (user might have typo)

**14.2 Lock Failure (Step 11.2 or 11.3)**
- Error: Cannot acquire database lock (another user is updating)
- Recovery Actions:
  1. ROLLBACK transaction (if started)
  2. Display error: "Could not lock account for update - another user is editing"
  3. Keep user on form with current data
  4. Stay in `VALIDATION_PASSED` state
  5. User can wait and retry F5
  6. Or user can press F12 to cancel
  7. Or user can press F3 to exit

**14.3 Concurrent Update (Step 11.4)**
- Error: Data changed by another user between fetch and update
- Recovery Actions:
  1. ROLLBACK transaction
  2. Fetch current data from database
  3. Replace displayed data with current values
  4. Highlight fields that changed
  5. Display warning: "Data was changed by another user. Please review and retry."
  6. Set state to `DATA_DISPLAYED`
  7. User must review current data
  8. User can make new edits and retry save
  9. Or user can cancel with F12
  10. Or user can exit with F3

**14.4 Update Failure After Lock (Step 11.6 or 11.7)**
- Error: Database update command failed
- Recovery Actions:
  1. ROLLBACK transaction (undoes all changes)
  2. Display error: "Update failed - database error"
  3. Keep user on form with their edits
  4. Stay in `VALIDATION_PASSED` state
  5. Log error details for support team
  6. User can retry F5
  7. Or user can cancel with F12
  8. Or user can exit with F3

**14.5 Validation Errors (Step 8)**
- Error: One or more fields failed validation
- Recovery Actions:
  1. Collect all validation errors
  2. Highlight error fields in red
  3. Display inline error messages below each error field
  4. Display first error message at bottom
  5. Position cursor at first error field
  6. Set state to `VALIDATION_FAILED`
  7. User corrects errors
  8. User presses Enter to re-validate
  9. Loop until all errors corrected

**14.6 Database Connection Error**
- Error: Cannot connect to database
- Recovery Actions:
  1. Display error: "Database connection error - please try again"
  2. Log error for support team
  3. Keep user on form
  4. User can retry operation
  5. Or user can exit with F3
  6. Alert operations team if repeated

**14.7 Timeout Errors**
- Error: Database operation timed out
- Recovery Actions:
  1. ROLLBACK transaction (if applicable)
  2. Display error: "Operation timed out - please try again"
  3. Log timeout details
  4. Keep user on form
  5. User can retry operation
  6. Recommend retry during off-peak hours if persistent

**Validation:** None, this step handles errors

**Output:**
- Error message displayed to user
- System in safe state (transaction rolled back if needed)
- User can take corrective action

---

### **Step 15: Program Return (Session Management)**

**Trigger:** After any operation completes (success or error)

**Purpose:** Preserve state for pseudo-conversational interaction

**Actions:**

**15.1 Prepare Session Data**
1. Copy error message to session storage
2. Pack shared session data:
   - User ID
   - User type
   - Calling program/transaction
   - Navigation context
   - Current account/customer IDs
   - Current account/customer names
   - Last map displayed
   - Last action taken

3. Pack program-specific session data:
   - All old values (fetched account/customer data)
   - All new values (user modifications)
   - All validation flags (field-level)
   - Current state indicator
   - Change detection flags
   - Error flags

4. Total session data size: approximately 2000 bytes
   - Shared area: ~500 bytes
   - Program-specific area: ~1500 bytes

**15.2 Store Session**
1. If using server-side session:
   - Generate session ID
   - Store session data in session store
   - Return session ID to client (cookie or token)

2. If using client-side session:
   - Encrypt session data (if sensitive)
   - Return session data in response
   - Client stores in sessionStorage or localStorage

3. Session expiration:
   - Set timeout (e.g., 30 minutes of inactivity)
   - Warn user before timeout
   - Allow session extension on user activity

**15.3 Release Resources**
1. Close database connections
2. Release locks (if any held)
3. Clear sensitive data from memory
4. Free allocated resources

**15.4 Return Control**
1. Send response to client:
   - HTTP status code
   - Response body (data or error)
   - Session identifier
   - Any updated session data

2. If navigating to another screen:
   - Include navigation target
   - Include context data for target screen
   - Update breadcrumb trail

3. Wait for next user action

**15.5 Session Restoration (Next Interaction)**
1. When user returns (next request):
   - Retrieve session ID from cookie/token
   - Load session data from session store
   - Restore program state:
     - Unpack shared session data
     - Unpack program-specific data
     - Restore old values
     - Restore new values
     - Restore validation flags
     - Restore state indicator
   - Resume from previous state
   - Process new user action

**Session Data Contents:**
```
{
  shared: {
    userId: "USER001",
    userType: "A",
    callingProgram: "MAINMENU",
    callingTransaction: "MENU",
    accountId: "00000000001",
    customerId: 123456789,
    customerName: "John Smith",
    navigationHistory: ["LOGIN", "MAINMENU", "ACCTUPD"],
    lastMapDisplayed: "ACCTUPD",
    lastAction: "FETCH"
  },
  programSpecific: {
    state: "DATA_DISPLAYED",
    dataFetched: true,
    oldValues: {
      account: { /* all account fields */ },
      customer: { /* all customer fields */ }
    },
    newValues: {
      account: { /* modified account fields */ },
      customer: { /* modified customer fields */ }
    },
    validationFlags: {
      creditLimit: true,
      zipCode: false,
      /* ... all field validation flags ... */
    },
    changeFlags: {
      creditLimit: true,
      addressLine1: true,
      /* ... all field change flags ... */
    },
    errorMessage: "",
    infoMessage: "Press F5 to save or F12 to cancel"
  }
}
```

**Validation:** None, this is state management

**Output:**
- Session data stored
- Resources released
- Ready for next interaction

---

## 5️⃣ INTEGRATION & SESSION MANAGEMENT

### **Session Context Data**

**User Session Information:**
```javascript
{
  userId: string,              // "USER001" - Logged-in user identifier
  userType: string,            // "A" = Admin, "R" = Regular user
  userFullName: string,        // "John Smith" - For display
  userPermissions: string[],   // ["VIEW_ACCOUNT", "UPDATE_ACCOUNT", ...]
  loginTimestamp: datetime,    // When user logged in
  lastActivityTimestamp: datetime  // Last user action
}
```

**Program Context:**
```javascript
{
  currentProgram: string,      // "ACCOUNT_UPDATE" - Current screen/module
  currentTransaction: string,  // "ACCT_UPD" - Current transaction ID
  programState: string,        // "DATA_DISPLAYED", "VALIDATION_PASSED", etc.
  sessionId: string,           // Unique session identifier
  ipAddress: string,           // User's IP address (for audit)
  browserInfo: string          // User agent string
}
```

**Account/Customer Context:**
```javascript
{
  accountId: string,           // "00000000001" - Current account being edited
  customerId: number,          // 123456789 - Associated customer
  customerName: string,        // "John Robert Smith" - Full name for display
  accountStatus: string,       // "Y" or "N" - Active/Inactive
  cardNumber: string,          // "1234567890123456" - Associated card
  dataFetchTimestamp: datetime // When data was fetched (for staleness check)
}
```

---

### **Navigation Context**

**Navigation Breadcrumbs:**
```javascript
{
  navigationHistory: [
    {
      programName: "LOGIN",
      programId: "LOGIN01",
      transactionId: "LOGN",
      timestamp: "2023-06-15T10:00:00Z"
    },
    {
      programName: "MAIN_MENU",
      programId: "MENU01",
      transactionId: "MENU",
      timestamp: "2023-06-15T10:01:00Z"
    },
    {
      programName: "ACCOUNT_UPDATE",
      programId: "ACCT_UPD",
      transactionId: "ACUP",
      timestamp: "2023-06-15T10:02:00Z"
    }
  ],
  currentIndex: 2,              // Current position in history
  canGoBack: true,              // Whether back navigation is allowed
  canGoForward: false           // Whether forward navigation is allowed
}
```

**Calling Context:**
```javascript
{
  fromProgram: string,         // "MAIN_MENU" - Program that launched this one
  fromTransaction: string,     // "MENU" - Transaction that launched this one
  fromUrl: string,             // "/menu" - URL of calling page
  returnUrl: string,           // "/menu" - Where to return on exit
  navigationParams: {          // Parameters passed from calling program
    prefilledAccountId: "00000000001",  // Optional pre-filled data
    viewMode: "edit",          // "view" or "edit" mode
    highlightFields: ["creditLimit"]  // Fields to highlight
  }
}
```

**Target Context (On Exit):**
```javascript
{
  toProgram: string,           // "MAIN_MENU" - Next program to navigate to
  toTransaction: string,       // "MENU" - Next transaction
  toUrl: string,               // "/menu" - URL to navigate to
  navigationParams: {          // Parameters to pass to next program
    updatedAccountId: "00000000001",
    updateSuccessful: true
  }
}
```

---

### **Entry Points**

**Entry Point 1: Direct Navigation**
```
User navigates directly to Account Update URL
  ↓
Check authentication:
  - If not logged in → Redirect to login
  - If logged in → Continue
  ↓
Check permissions:
  - If no update permission → Show error or read-only mode
  - If has permission → Continue
  ↓
Initialize program:
  - Set fromProgram = null (direct access)
  - Set state = INITIAL_ENTRY
  - Display blank form
  - Prompt for account number
```

**Entry Point 2: From Main Menu**
```
User selects "Account Update" from main menu
  ↓
Main menu calls Account Update with:
  - fromProgram = "MAIN_MENU"
  - fromTransaction = "MENU"
  - returnUrl = "/menu"
  - Optional: prefilledAccountId
  ↓
Account Update receives context:
  - Store calling program info
  - If accountId provided → Auto-fetch account data
  - If no accountId → Display blank form for entry
  ↓
Enable F3 Exit button (returns to menu)
```

**Entry Point 3: From Other Screens**
```
User is on another screen (e.g., Account List, Transaction History)
User clicks "Edit Account" button/link
  ↓
Calling screen prepares context:
  - fromProgram = "ACCOUNT_LIST"
  - fromTransaction = "ACLS"
  - prefilledAccountId = selected account
  - viewMode = "edit"
  - returnUrl = "/accounts/list"
  ↓
Account Update receives context:
  - Store calling program info
  - Auto-fetch account data (accountId provided)
  - Display data immediately
  - Enable edit mode
  - Enable F3 Exit button (returns to account list)
```

**Entry Point 4: Deep Link with Account ID**
```
User receives URL with account ID: /accounts/update/00000000001
  ↓
Check authentication and permissions
  ↓
Parse account ID from URL
  ↓
Account Update initializes:
  - fromProgram = null (deep link)
  - Auto-fetch account data
  - Display data immediately
  - Enable edit mode
  - Enable F3 Exit (returns to default: main menu)
```

---

### **Exit Points**

**Exit Point 1: F3/Exit Button - Return to Caller**
```
User presses F3 or clicks Exit button
  ↓
Check for unsaved changes:
  - If changes exist AND not saved:
    → Display confirmation: "You have unsaved changes. Exit?"
    → Wait for user response
    → If cancel → Return to current screen
    → If confirm → Continue
  ↓
Prepare exit context:
  - Retrieve fromProgram and fromTransaction from session
  - Set toProgram = fromProgram (or default to MAIN_MENU)
  - Set toTransaction = fromTransaction (or default to MENU)
  - Set returnParams = { accountId, updateResult, ... }
  ↓
Save session state (in case user navigates back)
  ↓
Navigate to target:
  - If fromProgram specified → Navigate to that URL
  - If fromProgram = null → Navigate to main menu (default)
  - Pass context to target program
  ↓
Clean up resources
```

**Exit Point 2: After Successful Update - Stay on Screen**
```
Update completes successfully
  ↓
Display success message: "Account Updated Successfully"
  ↓
Reset program state:
  - Clear all old values
  - Clear all new values
  - Clear all change flags
  - Clear all validation flags
  - Set state = INITIAL_ENTRY
  ↓
Display blank form
  ↓
Prompt for next account: "Enter Account Number"
  ↓
User can:
  - Enter another account number and continue
  - Or press F3 to exit
```

**Exit Point 3: Error Scenarios - Stay on Screen**
```
Error occurs (validation, lock, update failure)
  ↓
Display error message
  ↓
Keep user on form with current data
  ↓
User can:
  - Correct errors and retry
  - Cancel with F12 (discard changes)
  - Exit with F3 (abandon changes)
```

**Exit Point 4: Session Timeout**
```
User inactive for timeout period (e.g., 30 minutes)
  ↓
Display timeout warning: "Session about to expire"
  ↓
Give user option to extend session
  ↓
If user doesn't respond:
  - Save current state (if possible)
  - Log user out
  - Redirect to login page
  - After re-login → Can restore saved state
```

**Exit Point 5: Logout**
```
User clicks logout (from menu or header)
  ↓
Check for unsaved changes:
  - If changes exist → Confirm before logout
  ↓
If confirmed:
  - Clear all session data
  - Clear authentication tokens
  - Navigate to login page
  - Cannot restore state after logout
```

---

### **State Preservation Between Interactions**

**State Storage Strategy:**

**Server-Side Session Storage (Recommended):**
```javascript
// Store in Redis, Memcached, or database
sessionStore.set(sessionId, {
  // Shared session data
  userId: "USER001",
  userType: "A",
  loginTimestamp: "2023-06-15T10:00:00Z",

  // Program-specific state
  accountUpdate: {
    state: "DATA_DISPLAYED",
    oldValues: {
      account: { /* full account object */ },
      customer: { /* full customer object */ }
    },
    newValues: {
      account: { /* modified account fields */ },
      customer: { /* modified customer fields */ }
    },
    validationFlags: { /* field-level flags */ },
    changeFlags: { /* change detection flags */ },
    errorMessage: "",
    infoMessage: "Press F5 to save"
  },

  // Navigation context
  fromProgram: "MAIN_MENU",
  navigationHistory: [ /* breadcrumb trail */ ]
}, {
  ttl: 1800  // 30 minutes expiration
});

// Retrieve on next request
const sessionData = sessionStore.get(sessionId);
```

**Client-Side Session Storage (Alternative):**
```javascript
// Store in sessionStorage or localStorage
// Encrypt if sensitive data
const sessionData = {
  // Same structure as server-side
  // But store on client
};

// Encrypt
const encryptedData = encrypt(JSON.stringify(sessionData), secretKey);

// Store
sessionStorage.setItem('accountUpdateState', encryptedData);

// Retrieve on next action
const encryptedData = sessionStorage.getItem('accountUpdateState');
const sessionData = JSON.parse(decrypt(encryptedData, secretKey));
```

**State Data to Preserve:**

1. **Fetched Account Data (Old Values):**
   - Complete account record as fetched from database
   - Complete customer record as fetched from database
   - Card cross-reference data
   - Fetch timestamp (to detect stale data)

2. **User Modifications (New Values):**
   - All fields modified by user
   - Only store changed fields (optimization)
   - Modification timestamp per field

3. **Validation State:**
   - Field-level validation results (pass/fail)
   - Specific error messages per field
   - Overall validation status

4. **UI State:**
   - Current state indicator (INITIAL_ENTRY, DATA_DISPLAYED, etc.)
   - Which buttons are enabled/disabled
   - Cursor position
   - Scroll position

5. **Change Detection:**
   - Which fields have been changed
   - Change flags per field
   - Overall "changes exist" flag

6. **Navigation State:**
   - Breadcrumb trail
   - Calling program context
   - Return URL

7. **Messages:**
   - Current error message (if any)
   - Current info message (if any)

**State Restoration Process:**

```javascript
// When user returns to page
function restoreState(sessionId) {
  // 1. Retrieve session data
  const sessionData = sessionStore.get(sessionId);

  if (!sessionData) {
    // No session found → Start fresh
    initializeNewSession();
    return;
  }

  // 2. Restore UI state
  setCurrentState(sessionData.accountUpdate.state);

  // 3. Restore data
  if (sessionData.accountUpdate.oldValues) {
    populateFormFields(sessionData.accountUpdate.oldValues);
  }

  if (sessionData.accountUpdate.newValues) {
    applyUserModifications(sessionData.accountUpdate.newValues);
  }

  // 4. Restore validation state
  applyValidationResults(sessionData.accountUpdate.validationFlags);

  // 5. Restore change indicators
  highlightChangedFields(sessionData.accountUpdate.changeFlags);

  // 6. Restore messages
  displayMessage(sessionData.accountUpdate.errorMessage, 'error');
  displayMessage(sessionData.accountUpdate.infoMessage, 'info');

  // 7. Restore button states
  updateButtonStates(sessionData.accountUpdate.state);

  // 8. Restore navigation context
  setupNavigationContext(sessionData.fromProgram, sessionData.navigationHistory);
}
```

---

### **Browser Refresh Handling**

**Challenge:** Browser refresh loses all client-side state

**Solution: Persist State Before Refresh**

**Approach 1: Server-Side Session (Preferred)**
```
1. On every user action, save state to server
2. On browser refresh:
   - Browser requests page with session cookie
   - Server retrieves session data
   - Server sends page with restored state
   - Frontend rebuilds UI from server-provided state
3. User sees same state as before refresh
```

**Approach 2: sessionStorage**
```
1. On every user action, save state to sessionStorage
2. On browser refresh:
   - Page reloads
   - On page load, check sessionStorage
   - If state found → Restore from sessionStorage
   - Rebuild UI from stored state
3. User sees same state as before refresh
```

**Implementation:**
```javascript
// Before unload, save state
window.addEventListener('beforeunload', (event) => {
  // Save current state
  saveStateToSessionStorage();

  // If unsaved changes, warn user
  if (hasUnsavedChanges()) {
    event.preventDefault();
    event.returnValue = 'You have unsaved changes. Are you sure?';
  }
});

// On page load, restore state
window.addEventListener('load', () => {
  const savedState = getStateFromSessionStorage();

  if (savedState) {
    restoreUIState(savedState);
  }
});
```

**Unsaved Changes Warning:**
```javascript
// Detect browser navigation/close
window.addEventListener('beforeunload', (event) => {
  if (hasUnsavedChanges() && !updateJustCompleted) {
    // Show browser warning
    event.preventDefault();
    event.returnValue = 'You have unsaved changes. Are you sure?';
    return event.returnValue;
  }
});

// Also handle internal navigation
router.beforeEach((to, from, next) => {
  if (hasUnsavedChanges()) {
    // Show custom modal
    showConfirmation(
      'You have unsaved changes. Are you sure you want to leave?',
      () => next(),      // User confirms
      () => next(false)  // User cancels
    );
  } else {
    next();
  }
});
```

---

### **Session Expiration Handling**

**Session Timeout Strategy:**

**1. Idle Timeout (No Activity)**
```javascript
// Track user activity
let lastActivityTime = Date.now();

// Update on any user interaction
document.addEventListener('click', () => {
  lastActivityTime = Date.now();
  resetIdleTimer();
});

document.addEventListener('keypress', () => {
  lastActivityTime = Date.now();
  resetIdleTimer();
});

// Check for idle timeout
setInterval(() => {
  const idleTime = Date.now() - lastActivityTime;
  const timeoutMs = 30 * 60 * 1000; // 30 minutes

  if (idleTime > timeoutMs) {
    handleSessionExpiration();
  } else if (idleTime > timeoutMs - 60000) {
    // Warn user 1 minute before expiration
    showTimeoutWarning();
  }
}, 10000); // Check every 10 seconds
```

**2. Absolute Timeout (Max Session Duration)**
```javascript
// Set max session duration (e.g., 8 hours)
const sessionStartTime = Date.now();
const maxSessionMs = 8 * 60 * 60 * 1000;

setInterval(() => {
  const sessionDuration = Date.now() - sessionStartTime;

  if (sessionDuration > maxSessionMs) {
    handleSessionExpiration('Maximum session duration exceeded');
  }
}, 60000); // Check every minute
```

**3. Timeout Warning Dialog**
```javascript
function showTimeoutWarning() {
  showModal({
    title: 'Session About to Expire',
    message: 'Your session will expire in 1 minute due to inactivity. ' +
             'Click "Stay Logged In" to continue working.',
    buttons: [
      {
        label: 'Stay Logged In',
        action: () => {
          extendSession();
          closeModal();
        }
      },
      {
        label: 'Logout Now',
        action: () => {
          handleSessionExpiration();
        }
      }
    ],
    countdown: 60 // Show countdown timer
  });
}
```

**4. Handle Session Expiration**
```javascript
async function handleSessionExpiration(reason = 'Session expired') {
  // Try to save current state (if possible)
  try {
    await saveStateForRecovery();
  } catch (error) {
    console.error('Failed to save state:', error);
  }

  // Clear session
  sessionStore.clear(sessionId);

  // Clear authentication
  clearAuthTokens();

  // Show message
  showMessage(reason + '. Please log in again.', 'warning');

  // Redirect to login with return URL
  const returnUrl = window.location.pathname;
  window.location.href = `/login?returnUrl=${encodeURIComponent(returnUrl)}`;
}
```

**5. Extend Session**
```javascript
async function extendSession() {
  try {
    // Call backend to extend session
    const response = await fetch('/api/session/extend', {
      method: 'POST',
      credentials: 'include'
    });

    if (response.ok) {
      lastActivityTime = Date.now();
      showMessage('Session extended', 'success');
    } else {
      throw new Error('Failed to extend session');
    }
  } catch (error) {
    console.error('Session extension failed:', error);
    handleSessionExpiration();
  }
}
```

---

### **Multi-Tab/Window Coordination**

**Challenge:** User opens account in multiple tabs

**Strategy:**

**1. Detect Multiple Tabs**
```javascript
// Use BroadcastChannel API
const channel = new BroadcastChannel('account_update_channel');

// Notify other tabs when opening account
channel.postMessage({
  type: 'ACCOUNT_OPENED',
  accountId: currentAccountId,
  tabId: generateTabId()
});

// Listen for messages from other tabs
channel.addEventListener('message', (event) => {
  if (event.data.type === 'ACCOUNT_OPENED') {
    if (event.data.accountId === currentAccountId &&
        event.data.tabId !== currentTabId) {
      // Same account open in another tab
      showWarning(
        'This account is already open in another tab. ' +
        'Changes in one tab may conflict with the other.'
      );
    }
  }
});
```

**2. Lock Coordination**
```javascript
// When user starts editing
channel.postMessage({
  type: 'ACCOUNT_EDITING',
  accountId: currentAccountId,
  tabId: currentTabId
});

// Other tabs show read-only mode
channel.addEventListener('message', (event) => {
  if (event.data.type === 'ACCOUNT_EDITING' &&
      event.data.accountId === currentAccountId &&
      event.data.tabId !== currentTabId) {
    switchToReadOnlyMode('Another tab is editing this account');
  }
});
```

**3. Update Notification**
```javascript
// When update succeeds
channel.postMessage({
  type: 'ACCOUNT_UPDATED',
  accountId: currentAccountId,
  tabId: currentTabId
});

// Other tabs refresh data
channel.addEventListener('message', (event) => {
  if (event.data.type === 'ACCOUNT_UPDATED' &&
      event.data.accountId === currentAccountId &&
      event.data.tabId !== currentTabId) {
    showNotification('Account was updated in another tab');
    offerToRefresh();
  }
});
```

---

## Summary

This document provides comprehensive phase 2 migration requirements for the Account Update program, extracted from the COBOL program extraction document. All requirements are organized into five main sections suitable for modern framework implementation:

1. **Frontend Requirements**: Complete UI specifications, validation rules, state management, and error handling
2. **Backend Requirements**: API endpoints, business logic, validation, transaction integrity, and concurrency control using repository pattern with JOINs
3. **Database Requirements**: Table schemas, relationships, constraints, and indexes
4. **Business Rules**: Chronological execution flow from program initialization through completion (15 detailed steps)
5. **Integration & Session Management**: Session handling, navigation context, entry/exit points, and state preservation

All mainframe-specific terminology has been removed and replaced with modern equivalents suitable for Angular/React frontend and Node.js/Python/Java backend implementation.
