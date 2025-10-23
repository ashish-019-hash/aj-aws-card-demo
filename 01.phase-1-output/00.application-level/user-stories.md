# CardDemo COBOL Application - Business User Stories

## Overview
Business-level user stories extracted from the CardDemo COBOL codebase for modernization into a user-centric web or mobile application. These stories focus on end-user functionality and business value, excluding technical implementation details.

## User Types
- **Customer**: Regular users who manage their own accounts, cards, and transactions
- **Administrator**: System administrators who manage users and have elevated privileges
- **Account Holder**: Customers with active credit card accounts who perform financial operations

## Summary
- **Total User Stories**: 44
- **Functional Areas Covered**: 8
- **User Types Addressed**: 3 (Customer, Administrator, Account Holder)
- **Screens Covered**: 17
- **Validation Rules Referenced**: 38
- **Business Rules Referenced**: 9

---

## 1. Authentication & Access Management

### STORY-001: User System Login

**User Story**: "As a customer, I want to log into the system with my user ID and password so that I can securely access my account information and perform banking operations"

**Story Type**: Customer-Facing

**Source Location**: COSGN00C.cbl, SCREEN-001 (User Authentication)

**Acceptance Criteria**:
- User can enter User ID (8 characters maximum) (RULE-VAL-001)
- User can enter Password (8 characters, hidden from view) (RULE-VAL-002)
- System validates credentials against USRSEC file (RULE-VAL-003)
- System routes to appropriate menu based on user type (Regular User or Administrator)
- Clear error messages displayed for invalid User ID or incorrect password
- Session is properly established upon successful login with user context

**User Journey Context**:
- Entry Point: Application launch displays login screen with CardDemo branding
- User Actions: Enter User ID in USERIDI field, enter Password in PASSWDI field, press Enter key
- Expected Outcomes: Authenticated regular user routed to Main Menu (COMEN01), authenticated admin user routed to Admin Menu (COADM01)

**Business Value**: Provides secure access control to protect customer data and ensures users can only access authorized functions based on their assigned role

---

### STORY-002: Role-Based Menu Access

**User Story**: "As an authenticated user, I want to be automatically directed to the appropriate menu based on my role so that I can access only the functions relevant to my responsibilities"

**Story Type**: Operational

**Source Location**: COSGN00C.cbl, SCREEN-001; COMEN01C.cbl, SCREEN-002; COADM01C.cbl, SCREEN-003

**Acceptance Criteria**:
- Regular users (type 'U') are directed to Main Menu with 10 standard options
- Administrator users (type 'A') are directed to Admin Menu with all options plus user management
- User type validation enforces access restrictions (RULE-VAL-038)
- Menu displays current user information, date, and time
- All menu options are clearly numbered and described

**User Journey Context**:
- Entry Point: Successful authentication at login screen
- User Actions: System automatically routes based on user type from USRSEC file
- Expected Outcomes: Appropriate menu displayed with relevant options; regular users cannot access admin functions

**Business Value**: Ensures proper segregation of duties and prevents unauthorized access to administrative functions, maintaining system security and compliance

---

### STORY-003: Secure Application Exit

**User Story**: "As a user, I want to securely log out of the application so that my session is properly terminated and my data remains protected"

**Story Type**: Customer-Facing

**Source Location**: COMEN01C.cbl, COADM01C.cbl (F3 key handling)

**Acceptance Criteria**:
- User can press F3 key from any menu to return to login screen
- Session data is cleared upon logout
- User is returned to COSGN00 login screen
- No residual data from previous session is accessible
- Logout is confirmed with appropriate screen transition

**User Journey Context**:
- Entry Point: User at Main Menu or Admin Menu
- User Actions: Press F3 function key
- Expected Outcomes: Session terminated, login screen displayed, user must re-authenticate to access system

**Business Value**: Protects customer data privacy by ensuring sessions are properly terminated when users finish their work or step away from their workstation

---

## 2. Account Management

### STORY-004: View Account Details

**User Story**: "As a customer, I want to view my complete account information including personal details and current balances so that I can monitor my account status"

**Story Type**: Customer-Facing

**Source Location**: COACTVWC.cbl, SCREEN-004 (Account View)

**Acceptance Criteria**:
- User can enter 11-digit account number to search (RULE-VAL-007)
- System displays account information from ACCTDAT file including status, dates, credit limits, and balances
- System displays associated customer information from CUSTDAT file including name, address, contact details
- Account number must be numeric and non-zero for successful lookup
- Clear error message displayed if account not found or account number is invalid
- All displayed fields are read-only in view mode

**User Journey Context**:
- Entry Point: User selects Option 1 from Main Menu or Admin Menu
- User Actions: Enter 11-digit account number in ACCTSID field, press Enter
- Expected Outcomes: Complete account and customer details displayed on screen with all financial and personal information

**Business Value**: Enables customers to quickly access comprehensive account information for financial planning and account monitoring without contacting support

---

### STORY-005: Search and Update Account Information

**User Story**: "As a customer service representative, I want to search for an account and update its information so that I can maintain accurate customer records"

**Story Type**: Administrative

**Source Location**: COACTUPC.cbl, SCREEN-005 (Account Update)

**Acceptance Criteria**:
- User can enter 11-digit account number to retrieve account (RULE-VAL-004)
- System retrieves and displays current account data in editable format
- User can modify account status, dates, credit limits, balances, and customer information
- Credit limit validation applied (RULE-CALC-001)
- Cash credit limit validation applied (RULE-CALC-002)
- Current balance validation applied (RULE-CALC-003)
- Cycle credit/debit validation applied (RULE-CALC-004, RULE-CALC-005)
- All changes are validated before updating ACCTDAT and CUSTDAT files
- Confirmation message displayed upon successful update

**User Journey Context**:
- Entry Point: User selects Option 2 from Main Menu or Admin Menu
- User Actions: Enter account number, modify editable fields (status, dates, limits, personal info), press Enter
- Expected Outcomes: Account and customer records updated in system with validation applied, confirmation displayed

**Business Value**: Maintains data accuracy and allows authorized users to respond to customer requests for account modifications, improving customer service quality

---

### STORY-006: Modify Customer Personal Information

**User Story**: "As a customer service representative, I want to update customer personal details including name, address, and contact information so that customer records remain current and communications reach the right person"

**Story Type**: Administrative

**Source Location**: COACTUPC.cbl, SCREEN-005 (Account Update - Customer Fields)

**Acceptance Criteria**:
- User can modify customer first name, middle name, and last name fields
- User can update address fields including address lines 1 and 2, city, state, ZIP code, and country
- State code must be valid US state abbreviation (RULE-VAL-035)
- Phone numbers can be updated with US format validation (RULE-VAL-034)
- SSN validation ensures proper format and excludes invalid patterns (RULE-VAL-031)
- Date of birth cannot be in the future (RULE-VAL-018)
- FICO score must be between 300 and 850 (RULE-VAL-032)
- Government ID and EFT account ID can be updated
- Primary card holder flag can be set (Y/N) (RULE-VAL-033)

**User Journey Context**:
- Entry Point: From account update screen after entering valid account number
- User Actions: Modify customer information fields, press Enter to save changes
- Expected Outcomes: Customer information updated in CUSTDAT file with validation, confirmation message displayed

**Business Value**: Ensures customer contact information is accurate for communications and compliance, improving customer experience and reducing returned mail or failed contact attempts

---

### STORY-007: Manage Account Credit Limits

**User Story**: "As a credit analyst, I want to adjust account credit limits and cash advance limits so that I can respond to credit review decisions and customer requests"

**Story Type**: Administrative

**Source Location**: COACTUPC.cbl, SCREEN-005 (Account Update - Credit Limits)

**Acceptance Criteria**:
- User can modify credit limit amount in ACRDLIM field (RULE-CALC-001)
- User can modify cash credit limit amount in ACSHLIM field (RULE-CALC-002)
- Numeric validation ensures only valid currency amounts are accepted
- System converts character input to numeric format using NUMVAL-C function
- Fields can be cleared by entering asterisk (*) or spaces
- Changes are validated before updating ACCTDAT file
- Confirmation displayed upon successful limit adjustment

**User Journey Context**:
- Entry Point: From account update screen with focus on credit limit fields
- User Actions: Enter new credit limit values, press Enter to save
- Expected Outcomes: Credit limits updated in account record, available credit recalculated

**Business Value**: Enables credit risk management by allowing authorized users to adjust credit exposure based on customer creditworthiness and business rules

---

### STORY-008: Update Account Status and Dates

**User Story**: "As an account administrator, I want to update account status flags and important dates so that account lifecycle is properly managed"

**Story Type**: Administrative

**Source Location**: COACTUPC.cbl, SCREEN-005 (Account Update - Status and Dates)

**Acceptance Criteria**:
- User can update account active status (Y/N) in ACSTTUS field (RULE-VAL-033)
- User can modify account open date components (year, month, day)
- User can modify account expiration date components
- User can modify account reissue date components
- Date validation ensures valid calendar dates (RULE-VAL-017)
- Month validation ensures values between 01-12
- Day validation ensures values between 01-31
- Year must be 4-digit numeric format
- Changes update ACCTDAT file upon validation success

**User Journey Context**:
- Entry Point: From account update screen with focus on status and date fields
- User Actions: Modify status flag or date components, press Enter to save
- Expected Outcomes: Account status and date fields updated, account lifecycle properly tracked

**Business Value**: Maintains accurate account lifecycle information for regulatory compliance and operational management of account renewals and closures

---

## 3. Credit Card Management

### STORY-009: Browse Credit Card Portfolio

**User Story**: "As a customer, I want to view a list of all my credit cards so that I can see my card portfolio at a glance"

**Story Type**: Customer-Facing

**Source Location**: COCRDLIC.cbl, SCREEN-006 (Card Listing)

**Acceptance Criteria**:
- System displays up to 7 cards per page in paginated list
- User can optionally filter by 11-digit account number (RULE-VAL-005)
- User can optionally filter by 16-digit card number (RULE-VAL-008)
- Each card shows account number, card number, and active status
- User can navigate forward with F8 key to next page
- User can navigate backward with F7 key to previous page
- Current page number displayed
- User can select a card by entering selection number (1-7)
- Clear message displayed when no cards found

**User Journey Context**:
- Entry Point: User selects Option 3 from Main Menu or Admin Menu
- User Actions: Optionally enter account or card number filter, press Enter to view list, use F7/F8 to paginate
- Expected Outcomes: Paginated list of cards displayed, user can navigate through results and select cards for detailed view

**Business Value**: Provides customers with easy access to their complete card portfolio, enabling quick card selection and management

---

### STORY-010: View Credit Card Details

**User Story**: "As a customer, I want to view detailed information about a specific credit card so that I can verify card details and expiration date"

**Story Type**: Customer-Facing

**Source Location**: COCRDSLC.cbl, SCREEN-007 (Card Detail View)

**Acceptance Criteria**:
- User can enter 11-digit account number (RULE-VAL-005)
- User can enter 16-digit card number (RULE-VAL-008)
- Both account and card number must be provided for lookup
- System retrieves card data from CARDDAT file
- Display shows card name, active status (Y/N), expiration month, and expiration year
- Clear error message displayed if card not found
- Error message displayed if required fields are missing
- F3 key returns to previous screen (card listing or menu)

**User Journey Context**:
- Entry Point: User selects card from listing (COCRDLI) or selects Option 4 from menu
- User Actions: Enter account number and card number, press Enter
- Expected Outcomes: Complete card details displayed including name on card, status, and expiration date

**Business Value**: Enables customers to quickly verify card information for online purchases, over-the-phone transactions, or to check expiration dates for renewal planning

---

### STORY-011: Update Credit Card Information

**User Story**: "As a card services representative, I want to update credit card details so that card information remains accurate and reflects customer requests"

**Story Type**: Administrative

**Source Location**: COCRDUPC.cbl, SCREEN-008 (Card Update)

**Acceptance Criteria**:
- User can enter 11-digit account number (RULE-VAL-005)
- User can enter 16-digit card number (RULE-VAL-009)
- System retrieves current card data in editable format
- User can modify card name on card (RULE-VAL-011)
- User can modify card active status (RULE-VAL-012)
- User can modify card expiration month (01-12) (RULE-VAL-013)
- User can modify card expiration year in valid format (RULE-VAL-014)
- All changes validated before updating CARDDAT file
- Confirmation message displayed upon successful update
- Error message displayed for validation failures

**User Journey Context**:
- Entry Point: User selects Option 5 from Main Menu or Admin Menu
- User Actions: Enter account and card numbers, modify editable fields, press Enter to save
- Expected Outcomes: Card information updated in CARDDAT file with validation applied, confirmation displayed

**Business Value**: Maintains accurate card records and enables timely updates for card replacement, name changes, or status modifications requested by customers

---

### STORY-012: Manage Card Status

**User Story**: "As a fraud prevention specialist, I want to activate or deactivate credit cards so that I can respond to fraud reports or card replacement requests"

**Story Type**: Administrative

**Source Location**: COCRDUPC.cbl, SCREEN-008 (Card Update - Status Field)

**Acceptance Criteria**:
- User can change card active status from Y to N (deactivate) or N to Y (activate)
- Status must be valid value (Y/N) (RULE-VAL-012)
- Status change immediately reflected in CARDDAT file upon save
- Deactivated cards cannot be used for new transactions
- Status change logged with timestamp
- Confirmation message indicates successful status change

**User Journey Context**:
- Entry Point: From card update screen with focus on status field
- User Actions: Change CRDSTCD field value, press Enter to save
- Expected Outcomes: Card status updated, fraudulent or lost cards immediately deactivated

**Business Value**: Enables rapid response to fraud reports and lost card incidents, protecting customers from unauthorized transactions and reducing financial losses

---

### STORY-013: Update Card Expiration Date

**User Story**: "As a card services representative, I want to update credit card expiration dates so that card renewal information is accurate"

**Story Type**: Administrative

**Source Location**: COCRDUPC.cbl, SCREEN-008 (Card Update - Expiration Fields)

**Acceptance Criteria**:
- User can modify expiration month (01-12) (RULE-VAL-013)
- User can modify expiration year (4-digit format) (RULE-VAL-014)
- Month must be numeric and between 01-12
- Year must be numeric and valid 4-digit year format
- Combined month/year must represent a future date
- Changes update CARDDAT file upon validation
- Confirmation displayed after successful update

**User Journey Context**:
- Entry Point: From card update screen when processing card renewal
- User Actions: Update EXPMON and EXPYEAR fields, press Enter to save
- Expected Outcomes: Card expiration date updated for renewed cards

**Business Value**: Ensures customers can continue using their cards without interruption by maintaining accurate expiration dates for renewal processing

---

## 4. Transaction Management

### STORY-014: Browse Transaction History

**User Story**: "As a customer, I want to view a paginated list of my transactions so that I can review my spending and transaction activity"

**Story Type**: Customer-Facing

**Source Location**: COTRN00C.cbl, SCREEN-009 (Transaction List)

**Acceptance Criteria**:
- System displays up to 10 transactions per page in paginated list
- User can optionally filter by transaction ID
- Each transaction shows transaction ID, date, description, and amount
- User can navigate forward with F8 key to next page
- User can navigate backward with F7 key to previous page
- Current page number displayed (PAGENUM field)
- User can select a transaction by entering selection number (1-10)
- Clear message displayed when no transactions found
- F3 key returns to previous menu

**User Journey Context**:
- Entry Point: User selects Option 6 from Main Menu or Admin Menu
- User Actions: Optionally enter transaction ID filter in TRNIDIN field, press Enter, use F7/F8 to navigate pages
- Expected Outcomes: Paginated transaction list displayed from TRANSACT file, user can browse history and select transactions

**Business Value**: Enables customers to monitor their spending, review transaction history, and identify any unauthorized or questionable transactions

---

### STORY-015: View Transaction Details

**User Story**: "As a customer, I want to view complete details of a specific transaction so that I can verify the transaction information and merchant details"

**Story Type**: Customer-Facing

**Source Location**: COTRN01C.cbl, SCREEN-010 (Transaction View)

**Acceptance Criteria**:
- User can enter transaction ID to search (RULE-VAL-015)
- Transaction ID must not be empty or spaces
- System retrieves transaction data from TRANSACT file
- Display shows transaction ID, card number, type code, category code, source, description, amount
- Display shows transaction dates (original date and processing date)
- Display shows merchant information including merchant ID, name, city, and ZIP code
- Clear error message if transaction not found
- F3 returns to previous screen, F4 clears screen, F5 navigates to transaction listing

**User Journey Context**:
- Entry Point: User selects transaction from list (COTRN00) or selects Option 7 from menu
- User Actions: Enter transaction ID in TRNIDIN field, press Enter
- Expected Outcomes: Complete transaction details displayed including all merchant and transaction metadata

**Business Value**: Provides transparency in transaction details for customer verification, dispute resolution, and record-keeping

---

### STORY-016: Add New Transaction

**User Story**: "As a merchant services representative, I want to manually add a transaction to an account so that I can record offline or phone transactions"

**Story Type**: Administrative

**Source Location**: COTRN02C.cbl, SCREEN-011 (Transaction Add)

**Acceptance Criteria**:
- User can enter account number OR card number as identifier (RULE-VAL-006, RULE-VAL-010)
- Account ID numeric conversion applied (RULE-CALC-008)
- Card number numeric conversion applied (RULE-CALC-009)
- User must enter transaction type code in TTYPCD field
- User must enter transaction category code in TCATCD field
- User must enter transaction source in TRNSRC field
- User must enter transaction description in TDESC field
- Transaction amount must be numeric, range -99999999.99 to 99999999.99
- Original transaction date must be valid date format (YYYY-MM-DD)
- Processing date must be valid date format (YYYY-MM-DD)
- Merchant ID must be numeric (RULE-VAL-016)
- User must confirm transaction with Y/N input before creation
- F5 key copies last transaction data for easy entry of similar transactions
- Transaction created in TRANSACT file only with Y confirmation

**User Journey Context**:
- Entry Point: User selects Option 8 from Main Menu or Admin Menu
- User Actions: Enter account/card number, fill all transaction fields, enter Y in CONFIRM field, press Enter
- Expected Outcomes: New transaction record created in TRANSACT file, confirmation message displayed

**Business Value**: Enables manual transaction entry for offline purchases, phone orders, or correction entries, ensuring complete transaction records

---

### STORY-017: Confirm Transaction Before Creation

**User Story**: "As a data entry operator, I want to review and confirm transaction details before finalizing so that I can prevent errors in transaction records"

**Story Type**: Operational

**Source Location**: COTRN02C.cbl, SCREEN-011 (Transaction Add - Confirmation)

**Acceptance Criteria**:
- System validates all required fields before allowing confirmation
- User must explicitly enter Y or N in CONFIRM field
- Entering Y creates the transaction and displays success message
- Entering N cancels transaction creation without saving
- Clear validation errors displayed if required fields missing
- F4 key clears all fields for fresh entry
- All validation rules enforced before transaction creation

**User Journey Context**:
- Entry Point: After filling all transaction fields on transaction add screen
- User Actions: Review entered data, enter Y or N in CONFIRM field, press Enter
- Expected Outcomes: Transaction created if confirmed, cancelled if not confirmed, user stays on screen for next entry

**Business Value**: Reduces transaction entry errors by requiring explicit confirmation, improving data quality and reducing correction workload

---

### STORY-018: Copy Previous Transaction Data

**User Story**: "As a data entry operator, I want to copy the last transaction's data so that I can quickly enter multiple similar transactions"

**Story Type**: Operational

**Source Location**: COTRN02C.cbl, SCREEN-011 (Transaction Add - F5 Function)

**Acceptance Criteria**:
- F5 key copies all fields from the last successfully created transaction
- Copied fields include account/card number, type, category, source, description, merchant information
- User can modify any copied fields before confirmation
- Amount and dates can be adjusted for the new transaction
- User must still confirm new transaction with Y before creation
- Feature reduces data entry time for batch processing

**User Journey Context**:
- Entry Point: On transaction add screen after creating at least one transaction
- User Actions: Press F5 key to copy last transaction data, modify necessary fields, confirm
- Expected Outcomes: Previous transaction data populated in all fields, ready for modification and confirmation

**Business Value**: Improves data entry efficiency for processing multiple similar transactions, such as recurring merchant charges or batch processing

---

## 5. Bill Payment & Financial Operations

### STORY-019: Pay Full Account Balance

**User Story**: "As an account holder, I want to pay my full account balance so that I can clear my outstanding debt and avoid interest charges"

**Story Type**: Customer-Facing

**Source Location**: COBIL00C.cbl, SCREEN-012 (Bill Payment)

**Acceptance Criteria**:
- User can enter 11-digit account ID to process payment
- Account ID must not be empty (error: "Acct ID can NOT be empty")
- System retrieves current balance from ACCTDAT file
- System displays current balance amount to user for review
- User must confirm payment with Y or N in CONFIRM field
- System generates new transaction ID for payment (RULE-CALC-006)
- System updates account balance to zero using balance update calculation (RULE-CALC-007)
- System creates payment transaction record in TRANSACT file
- Error displayed if account not found or balance is already zero ("You have nothing to pay")
- F3 returns to menu, F4 clears screen

**User Journey Context**:
- Entry Point: User selects Option 10 from Main Menu or Admin Menu
- User Actions: Enter account ID in ACTIDIN field, review displayed balance in CURBAL field, enter Y in CONFIRM field, press Enter
- Expected Outcomes: Payment processed, balance zeroed, payment transaction created, confirmation displayed

**Business Value**: Enables customers to make full balance payments conveniently, reducing outstanding debt and interest charges while simplifying the payment process

---

### STORY-020: Review Balance Before Payment

**User Story**: "As an account holder, I want to see my current balance before confirming payment so that I can verify the payment amount is correct"

**Story Type**: Customer-Facing

**Source Location**: COBIL00C.cbl, SCREEN-012 (Bill Payment - Balance Display)

**Acceptance Criteria**:
- After entering valid account ID, system immediately displays current balance
- Balance shown in CURBAL field is current value from ACCTDAT file
- Balance display is read-only, user cannot modify amount
- User can review balance before deciding to confirm or cancel payment
- If balance is zero, error message displayed: "You have nothing to pay"
- Positive balance enables confirmation option
- Clear display of amount to be paid before commitment

**User Journey Context**:
- Entry Point: After entering account ID on bill payment screen
- User Actions: Review displayed current balance amount
- Expected Outcomes: User sees exact amount that will be paid if confirmed, can make informed decision

**Business Value**: Provides transparency in payment processing, allowing customers to verify amounts before commitment and preventing erroneous payments

---

### STORY-021: Cancel Bill Payment

**User Story**: "As an account holder, I want to cancel a bill payment before it's processed so that I can defer payment if needed"

**Story Type**: Customer-Facing

**Source Location**: COBIL00C.cbl, SCREEN-012 (Bill Payment - Cancel Confirmation)

**Acceptance Criteria**:
- After reviewing balance, user can enter N in CONFIRM field to cancel
- No transaction is created when user enters N
- No changes made to account balance when cancelled
- User remains on bill payment screen after cancellation
- User can enter different account ID or F3 to exit
- Cancel action is immediate with no further confirmation needed

**User Journey Context**:
- Entry Point: After balance is displayed and before confirming payment
- User Actions: Enter N in CONFIRM field, press Enter
- Expected Outcomes: Payment cancelled, no transaction created, user can exit or try different account

**Business Value**: Gives customers control over payment timing, allowing them to review and defer payments based on their financial situation

---

## 6. Reporting & Analytics

### STORY-022: Generate Monthly Transaction Report

**User Story**: "As an account holder, I want to generate a monthly transaction report so that I can review my monthly spending and reconcile my records"

**Story Type**: Customer-Facing

**Source Location**: CORPT00C.cbl, SCREEN-013 (Transaction Reports - Monthly Option)

**Acceptance Criteria**:
- User can select MONTHLY report option by marking the field
- No date range entry required for monthly reports
- User must confirm report generation with Y in CONFIRM field
- System submits batch job for report generation upon confirmation
- Report covers current month's transactions
- Confirmation message displayed upon successful submission
- User can cancel with N confirmation
- F3 returns to menu

**User Journey Context**:
- Entry Point: User selects Option 9 from Main Menu or Admin Menu
- User Actions: Select MONTHLY option, enter Y in CONFIRM field, press Enter
- Expected Outcomes: Monthly report batch job submitted, confirmation message displayed

**Business Value**: Provides customers with regular monthly statements for budgeting, tax preparation, and financial record-keeping

---

### STORY-023: Generate Yearly Transaction Report

**User Story**: "As an account holder, I want to generate a yearly transaction report so that I can review my annual spending and prepare tax documentation"

**Story Type**: Customer-Facing

**Source Location**: CORPT00C.cbl, SCREEN-013 (Transaction Reports - Yearly Option)

**Acceptance Criteria**:
- User can select YEARLY report option by marking the field
- No date range entry required for yearly reports
- User must confirm report generation with Y in CONFIRM field
- System submits batch job for report generation upon confirmation
- Report covers current year's transactions
- Confirmation message displayed upon successful submission
- User can cancel with N confirmation

**User Journey Context**:
- Entry Point: User selects report option from menu, chooses yearly
- User Actions: Select YEARLY option, enter Y in CONFIRM field, press Enter
- Expected Outcomes: Yearly report batch job submitted, confirmation message displayed

**Business Value**: Enables customers to obtain annual summaries for tax preparation, financial planning, and year-end reconciliation

---

### STORY-024: Generate Custom Date Range Report

**User Story**: "As an account holder, I want to generate a transaction report for a custom date range so that I can analyze spending for specific time periods"

**Story Type**: Customer-Facing

**Source Location**: CORPT00C.cbl, SCREEN-013 (Transaction Reports - Custom Option)

**Acceptance Criteria**:
- User can select CUSTOM report option by marking the field
- User must enter start date components: month (RULE-VAL-019), day (RULE-VAL-020), year (RULE-VAL-021)
- User must enter end date components: month (RULE-VAL-022), day (RULE-VAL-023), year (RULE-VAL-024)
- Start date month must be numeric and not greater than 12
- Start date day must be numeric and not greater than 31
- Start date year must be numeric 4-digit format
- End date month must be numeric and not greater than 12
- End date day must be numeric and not greater than 31
- End date year must be numeric 4-digit format
- Date validation ensures valid calendar dates (RULE-VAL-017)
- User must confirm report generation with Y in CONFIRM field
- System submits batch job with specified date range
- Clear error messages for invalid date formats or ranges

**User Journey Context**:
- Entry Point: User selects report option from menu, chooses custom
- User Actions: Select CUSTOM option, enter SDTMM/SDTDD/SDTYYYY (start date), enter EDTMM/EDTDD/EDTYYYY (end date), enter Y in CONFIRM, press Enter
- Expected Outcomes: Custom date range report batch job submitted with specified dates, confirmation displayed

**Business Value**: Provides flexibility for customers to analyze spending for any time period relevant to their needs, supporting budgeting and financial analysis

---

### STORY-025: Validate Report Date Ranges

**User Story**: "As a system user, I want the system to validate my report date inputs so that I receive accurate reports without invalid date errors"

**Story Type**: Operational

**Source Location**: CORPT00C.cbl, SCREEN-013 (Transaction Reports - Date Validation)

**Acceptance Criteria**:
- System validates all date components before submitting report
- Month values must be 01-12 for both start and end dates
- Day values must be 01-31 for both start and end dates
- Year values must be valid 4-digit years
- System uses CEEDAYS API for complete date validation (RULE-VAL-017)
- Clear, specific error messages indicate which date component is invalid
- User cannot submit report with invalid dates
- User remains on screen to correct date errors

**User Journey Context**:
- Entry Point: After entering custom date range on report screen
- User Actions: Press Enter to submit report with dates
- Expected Outcomes: Invalid dates rejected with clear error messages, valid dates accepted and report submitted

**Business Value**: Ensures data quality in reporting by preventing invalid date ranges, saving time by catching errors before report generation

---

## 7. User Administration

### STORY-026: View System Users List

**User Story**: "As an administrator, I want to view a paginated list of all system users so that I can manage user accounts and monitor access"

**Story Type**: Administrative

**Source Location**: COUSR00C.cbl, SCREEN-014 (User List)

**Acceptance Criteria**:
- Only administrator users can access this function (admin-only)
- System displays up to 10 users per page from USRSEC file
- User can optionally filter by User ID in USRIDIN field
- Each user shows User ID, first name, last name, and user type (A=Admin, U=User)
- Current page number displayed in PAGENUM field
- User can navigate with F7 (previous page) and F8 (next page)
- User can select a user with U (Update) or D (Delete) action
- F3 returns to admin menu (COADM01)
- Error message for invalid selections

**User Journey Context**:
- Entry Point: Administrator selects user management option from Admin Menu
- User Actions: Optionally enter user ID filter, press Enter to view list, use F7/F8 to paginate, select user with U or D
- Expected Outcomes: Paginated user list displayed, administrator can navigate and select users for management

**Business Value**: Enables centralized user account management, allowing administrators to efficiently oversee system access and maintain security

---

### STORY-027: Add New System User

**User Story**: "As an administrator, I want to add a new user to the system so that I can grant access to new employees or customers"

**Story Type**: Administrative

**Source Location**: COUSR01C.cbl, SCREEN-015 (Add User)

**Acceptance Criteria**:
- Only administrator users can access this function (admin-only)
- User must enter first name (RULE-VAL-025)
- User must enter last name (RULE-VAL-026)
- User must enter User ID (8 characters) (RULE-VAL-027)
- User must enter Password (8 characters, hidden) (RULE-VAL-028)
- User must enter User Type (A=Admin, U=User) (RULE-VAL-029)
- System checks for duplicate User ID before creation
- All fields validated before creating user in USRSEC file
- Success message displayed upon user creation
- Error message if User ID already exists
- F3 returns to admin menu, F4 clears all fields
- F12 exits application

**User Journey Context**:
- Entry Point: Administrator selects add user option from Admin Menu or user list
- User Actions: Enter FNAME, LNAME, USERID, PASSWD, USRTYPE fields, press Enter
- Expected Outcomes: New user created in USRSEC file, confirmation displayed, new user can log in immediately

**Business Value**: Streamlines onboarding process by allowing administrators to quickly provision system access for new users with appropriate role assignment

---

### STORY-028: Update Existing User Information

**User Story**: "As an administrator, I want to update an existing user's information so that I can maintain accurate user records and modify access permissions"

**Story Type**: Administrative

**Source Location**: COUSR02C.cbl, SCREEN-016 (Update User)

**Acceptance Criteria**:
- Only administrator users can access this function (admin-only)
- User enters User ID to search (RULE-VAL-027)
- System retrieves user data from USRSEC file for editing
- Administrator can modify first name in editable FNAME field
- Administrator can modify last name in editable LNAME field
- Administrator can modify password in editable PASSWD field (hidden)
- Administrator can modify user type in editable USRTYPE field
- F5 saves changes to USRSEC file
- F3 saves and returns to admin menu
- F4 clears fields for new search
- F12 cancels without saving
- Error message if user not found
- Validation errors displayed for invalid entries

**User Journey Context**:
- Entry Point: Administrator selects user for update from user list or selects update option from menu
- User Actions: Enter user ID in USRIDIN, press Enter to fetch data, modify fields, press F5 to save or F3 to save and exit
- Expected Outcomes: User information updated in USRSEC file, changes effective immediately

**Business Value**: Enables efficient user account maintenance, allowing administrators to update contact information, reset passwords, or modify permissions as organizational needs change

---

### STORY-029: Delete System User

**User Story**: "As an administrator, I want to delete a user from the system so that I can remove access for terminated employees or inactive accounts"

**Story Type**: Administrative

**Source Location**: COUSR03C.cbl, SCREEN-017 (Delete User)

**Acceptance Criteria**:
- Only administrator users can access this function (admin-only)
- User enters User ID to search (RULE-VAL-030)
- System retrieves and displays user data for confirmation
- Display shows first name, last name, and user type (read-only)
- Administrator must press F5 to confirm deletion
- System deletes user from USRSEC file with confirmation
- Success message displayed upon deletion
- F3 returns to admin menu without deleting
- F4 clears fields for new search
- Error message if user not found
- Deleted user cannot log in immediately

**User Journey Context**:
- Entry Point: Administrator selects user for deletion from user list or selects delete option from menu
- User Actions: Enter user ID in USRIDIN, press Enter to view user details, press F5 to confirm deletion
- Expected Outcomes: User record deleted from USRSEC file, confirmation displayed, access revoked immediately

**Business Value**: Maintains system security by enabling timely removal of access for terminated employees or inactive users, reducing security risks

---

### STORY-030: Assign User Roles

**User Story**: "As an administrator, I want to assign or change a user's role between User and Administrator so that I can control access levels based on job responsibilities"

**Story Type**: Administrative

**Source Location**: COUSR01C.cbl, COUSR02C.cbl (User Type Field)

**Acceptance Criteria**:
- During user creation, administrator selects user type (A=Admin, U=User)
- During user update, administrator can change user type
- User type A grants admin menu access with all functions including user management
- User type U restricts to main menu access without user management functions
- Type validation ensures only A or U values accepted (RULE-VAL-029)
- Role changes take effect on user's next login
- Current admin users can modify their own role (with caution message if changing self)

**User Journey Context**:
- Entry Point: During add user or update user operations
- User Actions: Set USRTYPE field to A or U
- Expected Outcomes: User role assigned or changed, access permissions adjusted accordingly

**Business Value**: Implements principle of least privilege by enabling granular access control based on job responsibilities, improving security and compliance

---

## 8. Application Navigation

### STORY-031: Navigate Main Menu Options

**User Story**: "As a customer, I want to select options from a numbered menu so that I can easily access different functions of the application"

**Story Type**: Customer-Facing

**Source Location**: COMEN01C.cbl, SCREEN-002 (Main Menu)

**Acceptance Criteria**:
- Menu displays 10 numbered options clearly
- User enters option number in WS-OPTION field (RULE-VAL-036)
- Option must be numeric, greater than zero, and not exceed total option count
- System validates option and transfers to selected function
- Option 1: Account View (COACTVW)
- Option 2: Account Update (COACTUP)
- Option 3: Card List (COCRDLI)
- Option 4: Card View (COCRDSL)
- Option 5: Card Update (COCRDUP)
- Option 6: Transaction List (COTRN00)
- Option 7: Transaction View (COTRN01)
- Option 8: Transaction Add (COTRN02)
- Option 9: Reports (CORPT00)
- Option 10: Bill Payment (COBIL00)
- Invalid option displays error: "Please enter a valid option number..."
- F3 key returns to login screen

**User Journey Context**:
- Entry Point: After successful login as regular user
- User Actions: View menu options, enter option number, press Enter
- Expected Outcomes: Selected function screen displayed, user can perform chosen operation

**Business Value**: Provides intuitive navigation interface that minimizes training needs and enables customers to quickly access desired functions

---

### STORY-032: Access Admin Menu Functions

**User Story**: "As an administrator, I want to access an extended menu with administrative functions so that I can perform both regular and administrative tasks"

**Story Type**: Administrative

**Source Location**: COADM01C.cbl, SCREEN-003 (Admin Menu)

**Acceptance Criteria**:
- Admin menu displays all 10 regular options plus additional admin options
- User enters option number in WS-OPTION field (RULE-VAL-037)
- Option must be numeric, greater than zero, and not exceed admin option count
- Regular users cannot access admin menu (RULE-VAL-038)
- Admin menu includes all main menu options (1-10)
- Admin menu includes additional user management options (11+)
- Additional options navigate to user management functions (COUSR00, COUSR01, COUSR02, COUSR03)
- Invalid option displays error: "Please enter a valid option number..."
- F3 key returns to login screen

**User Journey Context**:
- Entry Point: After successful login as administrator user
- User Actions: View extended menu options, enter option number, press Enter
- Expected Outcomes: Selected function screen displayed, administrator can perform both regular and administrative operations

**Business Value**: Provides administrators with comprehensive access to all system functions through a single unified interface, improving operational efficiency

---

### STORY-033: Enforce Role-Based Menu Access

**User Story**: "As a system user, I want the system to only show me menu options appropriate for my role so that I'm not confused by functions I cannot access"

**Story Type**: Operational

**Source Location**: COMEN01C.cbl (RULE-VAL-038)

**Acceptance Criteria**:
- Regular users with type 'U' only see main menu with 10 options
- Regular users attempting to access admin-only options receive access denied error
- Administrator users with type 'A' see admin menu with all options
- User type validated during menu option selection (RULE-VAL-038)
- If user type is 'USER' and menu option type is 'A' (Admin), access denied
- Clear error message indicates insufficient permissions
- User context maintained throughout session

**User Journey Context**:
- Entry Point: During menu option selection after login
- User Actions: Attempt to select menu option
- Expected Outcomes: Authorized options execute normally, unauthorized options display access denied message

**Business Value**: Implements proper access control and improves user experience by preventing confusion from inaccessible functions, maintaining security boundaries

---

### STORY-034: Return to Previous Screen

**User Story**: "As a user, I want to press F3 from any screen to return to the previous screen so that I can easily navigate backward through the application"

**Story Type**: Operational

**Source Location**: All screen programs (universal F3 key handling)

**Acceptance Criteria**:
- F3 key available on all screens throughout application
- From detail screens, F3 returns to list screen or menu
- From list screens, F3 returns to menu
- From menu screens, F3 returns to login screen
- Navigation history maintained properly
- No data loss when using F3 (unsaved changes may be lost with warning)
- Consistent behavior across all screens

**User Journey Context**:
- Entry Point: User at any screen in application
- User Actions: Press F3 function key
- Expected Outcomes: User returned to logically previous screen in navigation hierarchy

**Business Value**: Provides intuitive backward navigation that matches user expectations, reducing training time and improving user satisfaction

---

### STORY-035: Clear Screen Fields

**User Story**: "As a data entry user, I want to clear all fields on the current screen so that I can start fresh entry without manually deleting each field"

**Story Type**: Operational

**Source Location**: Multiple screens with F4 key support (COTRN02, COBIL00, COUSR01, COUSR02, COUSR03)

**Acceptance Criteria**:
- F4 key available on applicable data entry screens
- Pressing F4 clears all input fields on current screen
- User remains on same screen after clear operation
- Output/display fields (read-only) not affected by clear
- User can immediately begin entering new data
- No confirmation required for clear operation

**User Journey Context**:
- Entry Point: User on data entry screen with populated fields
- User Actions: Press F4 function key
- Expected Outcomes: All input fields cleared, cursor positioned at first field, ready for new entry

**Business Value**: Improves data entry efficiency by allowing quick reset of screen for processing multiple records in sequence

---

### STORY-036: Navigate Between List Pages

**User Story**: "As a user viewing paginated lists, I want to use F7 and F8 keys to navigate between pages so that I can browse through all available records"

**Story Type**: Operational

**Source Location**: COCRDLIC.cbl, COTRN00C.cbl, COUSR00C.cbl (list screens with pagination)

**Acceptance Criteria**:
- F8 key navigates to next page of results
- F7 key navigates to previous page of results
- Current page number clearly displayed (PAGENUM or PAGENO field)
- F8 disabled or shows message when on last page
- F7 disabled or shows message when on first page
- Page transitions are smooth without data loss
- User selection and filter criteria maintained across pages
- Applicable to card lists, transaction lists, and user lists

**User Journey Context**:
- Entry Point: User viewing any paginated list screen
- User Actions: Press F8 to go forward, F7 to go backward through pages
- Expected Outcomes: Next or previous page of results displayed, page number updated, navigation constraints respected

**Business Value**: Enables efficient browsing of large datasets, allowing users to find specific records without overwhelming screen display

---

### STORY-037: Select Items from Lists

**User Story**: "As a user viewing lists, I want to select an item by entering a selection number so that I can view details or perform actions on specific records"

**Story Type**: Operational

**Source Location**: COCRDLIC.cbl (card selection), COTRN00C.cbl (transaction selection), COUSR00C.cbl (user selection)

**Acceptance Criteria**:
- List screens display selection field for each row (CRDSEL1-7, SEL0001-10)
- User enters selection indicator in appropriate field
- For card lists: entering selection number navigates to card detail view
- For transaction lists: entering selection number navigates to transaction view
- For user lists: entering U navigates to update user, entering D navigates to delete user
- Invalid selections display appropriate error message
- Selection action executes upon pressing Enter
- User can select only one item at a time

**User Journey Context**:
- Entry Point: User viewing paginated list of cards, transactions, or users
- User Actions: Enter selection indicator (number, U, D) in selection field, press Enter
- Expected Outcomes: User navigated to detail screen or action screen for selected item

**Business Value**: Provides efficient drill-down navigation from summary to detail views, enabling quick access to specific record information

---

## Cross-Functional Stories

### STORY-038: Input Field Validation

**User Story**: "As a user entering data, I want the system to validate my input and provide clear error messages so that I can correct mistakes before saving"

**Story Type**: Operational

**Source Location**: All data entry screens (validation rules RULE-VAL-001 through RULE-VAL-038)

**Acceptance Criteria**:
- All required fields validated for empty/spaces before processing
- Numeric fields validated for numeric content
- Date fields validated for valid calendar dates
- Phone numbers validated for US format with valid area codes
- SSN validated for proper format excluding invalid patterns
- State codes validated against valid US state abbreviations
- FICO scores validated for range 300-850
- Yes/No fields validated for Y or N values only
- Card numbers validated for 16-digit numeric format
- Account numbers validated for 11-digit numeric format
- Error messages displayed at field level with cursor positioned at error
- Clear, descriptive error messages explain validation failure
- User can correct errors and resubmit

**User Journey Context**:
- Entry Point: User entering data on any data entry screen
- User Actions: Enter data in fields, press Enter to process
- Expected Outcomes: Valid data accepted and processed, invalid data rejected with clear error messages

**Business Value**: Ensures data quality and integrity by preventing invalid data entry, reducing downstream errors and correction workload

---

### STORY-039: Error Message Display

**User Story**: "As a user, I want to see clear error messages when something goes wrong so that I understand the issue and know how to resolve it"

**Story Type**: Operational

**Source Location**: All screens (ERRMSG field in screen definitions)

**Acceptance Criteria**:
- Error messages displayed in dedicated ERRMSG field on each screen
- Messages are clear, concise, and actionable
- Examples: "Account ID NOT found", "User ID already exists", "Please enter a valid option number"
- Error messages remain visible until user takes corrective action
- Cursor automatically positioned at error field when applicable
- Different error types distinguished (validation errors, not found errors, business rule violations)
- No technical error codes or jargon in user-facing messages

**User Journey Context**:
- Entry Point: User encounters an error condition on any screen
- User Actions: Read error message, take corrective action
- Expected Outcomes: User understands error and can resolve issue without external help

**Business Value**: Reduces support calls and user frustration by providing self-service error resolution through clear messaging

---

### STORY-040: Session Management

**User Story**: "As a user, I want the system to maintain my session context so that I don't have to re-enter information as I navigate between screens"

**Story Type**: Operational

**Source Location**: All screens (session context maintained via COMMAREA)

**Acceptance Criteria**:
- User ID and user type maintained throughout session
- Transaction data maintained when navigating between related screens
- Filter criteria preserved when returning from detail to list screens
- Last entered data available for copy operations (F5 in transaction add)
- Session terminates on logout (F3 from menu to login)
- No session data leakage between different user sessions
- Current screen context available for navigation decisions

**User Journey Context**:
- Entry Point: Throughout entire user session after login
- User Actions: Navigate between screens, perform operations
- Expected Outcomes: Context maintained, user doesn't need to re-authenticate or re-enter data unnecessarily

**Business Value**: Improves user experience and productivity by eliminating redundant data entry and maintaining workflow context

---

### STORY-041: Display Current System Information

**User Story**: "As a user, I want to see current date, time, and screen identification on each screen so that I can orient myself within the application"

**Story Type**: Operational

**Source Location**: All screens (TRNNAME, PGMNAME, CURDATE, CURTIME fields)

**Acceptance Criteria**:
- Current date displayed in CURDATE field on all screens
- Current time displayed in CURTIME field on all screens
- Current transaction ID displayed in TRNNAME field
- Current program name displayed in PGMNAME field
- Information automatically updated as screens are accessed
- Consistent placement across all screens
- Date and time reflect system time accurately

**User Journey Context**:
- Entry Point: User viewing any screen in application
- User Actions: View screen header/footer information
- Expected Outcomes: User sees current date/time and screen identification information

**Business Value**: Provides context awareness for users, supports audit trails, and helps users understand their location within the application

---

### STORY-042: Phone Number Validation

**User Story**: "As a data entry user, I want phone numbers validated for proper US format so that contact information is accurate and callable"

**Story Type**: Operational

**Source Location**: COACTUPC.cbl, phone number fields (RULE-VAL-034)

**Acceptance Criteria**:
- Phone numbers entered in three components: area code, prefix, suffix
- Area code validated for valid US area codes
- Complete phone number must follow US format (XXX-XXX-XXXX)
- Invalid formats rejected with clear error message
- Both primary and secondary phone numbers validated
- Validation occurs on account update operations
- System ensures phone numbers are contactable

**User Journey Context**:
- Entry Point: User updating customer phone numbers on account update screen
- User Actions: Enter phone number components in ACSPH1A/ACSPH1B/ACSPH1C or ACSPH2A/ACSPH2B/ACSPH2C fields
- Expected Outcomes: Valid US phone numbers accepted, invalid formats rejected with error message

**Business Value**: Ensures accurate contact information for customer communications, reducing failed contact attempts and improving customer service

---

### STORY-043: Date of Birth Validation

**User Story**: "As a data entry user, I want date of birth validated to ensure it's not in the future so that customer records contain valid biographical data"

**Story Type**: Operational

**Source Location**: COACTUPC.cbl, date of birth fields (RULE-VAL-018)

**Acceptance Criteria**:
- Date of birth entered in components: year, month, day (DOBYEAR/DOBMON/DOBDAY)
- System converts date to binary for comparison
- Birth date must be less than or equal to current system date
- Future dates rejected with error message
- Date must be valid calendar date
- Validation uses CEEDAYS API for accuracy
- Applied during customer information updates

**User Journey Context**:
- Entry Point: User updating customer date of birth on account update screen
- User Actions: Enter birth date components
- Expected Outcomes: Valid past dates accepted, future dates rejected with clear error

**Business Value**: Maintains data integrity for age-dependent business rules, regulatory compliance, and accurate customer demographics

---

### STORY-044: Credit Score Validation

**User Story**: "As a credit analyst, I want FICO scores validated for the standard range of 300-850 so that credit assessments are based on accurate data"

**Story Type**: Administrative

**Source Location**: COACTUPC.cbl, FICO score field (RULE-VAL-032)

**Acceptance Criteria**:
- FICO score field (ACSTFCO) accepts numeric values only
- Score must be between 300 and 850 inclusive
- Scores below 300 rejected with error message
- Scores above 850 rejected with error message
- Validation applied during customer information updates
- Clear error message indicates valid range
- Score used for credit decisions and account management

**User Journey Context**:
- Entry Point: User updating customer FICO score on account update screen
- User Actions: Enter numeric value in ACSTFCO field
- Expected Outcomes: Valid scores (300-850) accepted, out-of-range scores rejected

**Business Value**: Ensures credit scoring data integrity for accurate credit risk assessment and compliance with lending standards

---

## Summary

This document contains 44 business-level user stories extracted from the CardDemo COBOL application, organized into 8 functional modules:

1. **Authentication & Access Management**: 3 stories covering login, role-based access, and logout
2. **Account Management**: 5 stories covering account viewing, updating, and credit limit management
3. **Credit Card Management**: 5 stories covering card listing, viewing, updating, and status management
4. **Transaction Management**: 5 stories covering transaction browsing, viewing, adding, and confirmation
5. **Bill Payment & Financial Operations**: 3 stories covering balance payments and payment review
6. **Reporting & Analytics**: 4 stories covering monthly, yearly, custom, and validated reports
7. **User Administration**: 5 stories covering user listing, adding, updating, deleting, and role assignment
8. **Application Navigation**: 7 stories covering menu navigation, screen transitions, and list operations

**Cross-Functional Stories**: 7 stories covering validation, error handling, session management, and data quality

All stories reference specific source locations from the COBOL codebase, include acceptance criteria based on the 38 validation rules and 9 business rules, and provide clear business value for modernization efforts.

These user stories provide a comprehensive foundation for modernizing the legacy CardDemo COBOL system into a modern, user-centric application while preserving all essential business functionality.
