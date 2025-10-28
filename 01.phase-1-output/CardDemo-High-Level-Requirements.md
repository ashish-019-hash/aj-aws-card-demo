# CardDemo - High-Level Requirements Document

**Document Version:** 1.0  
**Generated From:** Codebase analysis (app/ folder only)  
**Date:** October 27, 2025  
**Source Repository:** aj-aws-card-demo

---

## 1. System Overview & Purpose

### System Identification
- **System Name**: CardDemo
- **Application Code/ID**: CardDemo Application
- **Business Domain**: Financial Services - Credit Card Management

### Business Purpose
The CardDemo system is a comprehensive credit card management application that handles all aspects of credit card account administration, customer management, transaction processing, and billing operations. The system supports both real-time online operations for user interactions and scheduled batch processing for high-volume transaction posting, statement generation, and interest calculations.

### System Criticality
- **Criticality Level**: High
- **Business Impact if Unavailable**: Loss of ability to process credit card transactions, generate customer statements, manage accounts, and provide customer service functions.

### System Type
- **Architecture**: Mixed (Batch + Online)
- **Processing Model**: Real-time online CICS transactions combined with scheduled batch processing
- **Technology Stack**: COBOL, CICS (Customer Information Control System), VSAM (Virtual Storage Access Method)

### Key Stakeholders
- Credit Card Operations Department
- Customer Service Representatives
- Account Management Teams
- Financial Processing Teams
- System Administrators
- Compliance and Audit Teams

---

## 2. Core Capabilities Inventory

### Category: Authentication & Security
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 1 | User Sign-on | Authenticate users and route to appropriate menu based on user type (admin or regular) | Real-time | High |
| 2 | User Management | List, view, add, update, and delete system users with role-based access control | On-demand | Medium |

### Category: Account Management
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 3 | Account View | Display account details including balance, credit limit, status, and transaction history | Real-time | High |
| 4 | Account Update | Modify account information such as credit limits, status, and account attributes | On-demand | Medium |
| 5 | Account Data Loading | Load and initialize account data from external sources | Batch | Low |
| 6 | Account Data Reading | Read and print account data for reporting purposes | Batch | Low |

### Category: Card Management
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 7 | Card Listing | Display all credit cards or cards associated with specific accounts | Real-time | High |
| 8 | Card Selection | Allow users to select and view details of specific credit cards | Real-time | High |
| 9 | Card Update | Update card information including status, expiration date, and associated account | On-demand | Medium |
| 10 | Card Data Loading | Load and initialize card data from external sources | Batch | Low |
| 11 | Card Data Reading | Read and print card data for reporting purposes | Batch | Low |

### Category: Customer Management
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 12 | Customer Data Loading | Load and initialize customer information including personal details and contact information | Batch | Low |
| 13 | Customer Data Reading | Read and print customer data for reporting and verification | Batch | Low |

### Category: Transaction Processing
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 14 | Transaction Inquiry | View transaction history at multiple detail levels (summary, category, individual) | Real-time | High |
| 15 | Daily Transaction Posting | Post daily transactions to accounts and update balances | Daily Batch | High |
| 16 | Transaction Data Loading | Load transaction data from external sources | Batch | Medium |
| 17 | Transaction Indexing | Create and maintain transaction indexes for efficient retrieval | Batch | Medium |
| 18 | Transaction Combination | Combine and consolidate transaction data from multiple sources | Batch | Medium |
| 19 | Transaction Reporting | Generate detailed transaction reports for analysis and audit | Batch | Medium |
| 20 | Daily Rejection Processing | Process and report rejected transactions that failed validation | Daily Batch | Medium |
| 21 | Transaction Backup | Create backup copies of transaction files | Batch | Low |

### Category: Billing & Statements
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 22 | Statement Generation | Generate monthly statements for all active cards with transaction details | Monthly Batch | High |
| 23 | Billing Inquiry | View billing information and statement history online | Real-time | Medium |
| 24 | Interest Calculation | Calculate and post interest charges on outstanding balances | Monthly Batch | High |

### Category: Financial Processing
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 25 | Transaction Category Balance | Maintain and update transaction category balances by account | Daily Batch | High |
| 26 | Disclosure Group Management | Manage interest rates and terms by account group and transaction type | On-demand | Low |
| 27 | Cross-reference Management | Maintain card-to-account-to-customer relationships | Batch | Medium |

### Category: Reporting & Administration
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 28 | Report Generation | Generate various administrative and operational reports | On-demand | Medium |
| 29 | Admin Menu | Provide administrative functions for system management | Real-time | Low |
| 30 | Main Menu | Provide navigation interface for regular users | Real-time | High |
| 31 | File Management | Open and close VSAM files for batch processing | Batch | Low |
| 32 | GDG Management | Manage Generation Data Groups for backup and archival | Batch | Low |

### Category: Optional - Transaction Type Management (DB2 Module)
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 33 | Transaction Type Listing | Display all transaction types and categories from DB2 database | On-demand | Medium |
| 34 | Transaction Type Update | Add, modify, or delete transaction type definitions | On-demand | Low |
| 35 | Transaction Type Data Loading | Load transaction type data into DB2 tables | Batch | Low |

### Category: Optional - Authorization Management (IMS/DB2/MQ Module)
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 36 | Authorization Summary View | Display summary of pending authorization requests | Real-time | Medium |
| 37 | Authorization Detail View | View detailed information about specific authorization requests | Real-time | Medium |
| 38 | Authorization Processing | Process authorization requests and update approval status | Real-time | Medium |
| 39 | Authorization Batch Processing | Batch process authorization messages and update IMS database | Batch | Medium |

### Category: Optional - Asynchronous Processing (MQ Module)
| # | Capability Name | Description | Frequency | Volume |
|---|----------------|-------------|-----------|--------|
| 40 | Account Data Extraction | Extract account information for external processing via message queues | Asynchronous | Medium |

### Capability Summary
- **Total Capabilities Identified**: 40
- **Batch Jobs**: 37 core + 8 optional = 45 total
- **Online Transactions**: 31 core + 11 optional = 42 programs
- **Primary Business Functions**: Authentication, Account Management, Card Management, Customer Management, Transaction Processing, Billing, Reporting, Administration
- **Optional Modules**: Transaction Type Management (DB2), Authorization Management (IMS/DB2/MQ), Asynchronous Processing (MQ)

---

## 3. Data Landscape

### Core Business Entities
The system manages the following primary data entities:

| # | Entity Name | Definition | Record Length |
|---|------------|------------|---------------|
| 1 | Account | Credit card account containing balance, credit limit, status, open date, expiration date, and current cycle credits/debits | 300 bytes |
| 2 | Card | Physical credit card associated with an account, including card number, CVV, embossed name, expiration date, and active status | 150 bytes |
| 3 | Customer | Individual or business entity holding accounts, including personal information, address, phone numbers, SSN, date of birth, and FICO credit score | 500 bytes |
| 4 | Card Cross-reference | Relationship mapping between card number, customer ID, and account ID | 50 bytes |
| 5 | Transaction | Individual credit card transaction containing transaction ID, type, category, amount, merchant information, card number, and timestamps | 350 bytes |
| 6 | Transaction Category Balance | Balance tracking by account, transaction type, and category code | 50 bytes |
| 7 | Disclosure Group | Interest rate and terms associated with account groups and transaction categories | 50 bytes |
| 8 | Transaction Type | Definition of transaction type codes and descriptions | 60 bytes |
| 9 | Transaction Category Type | Definition of transaction categories within transaction types | 60 bytes |
| 10 | User Security | System user credentials, names, passwords, and user type (admin or regular) | 80 bytes |

### Data Sources (Inputs)

**VSAM Files (Indexed and Sequential):**
- Account Data (ACCTFILE) - Indexed VSAM KSDS
- Card Data (CARDFILE) - Indexed VSAM KSDS
- Customer Data (CUSTFILE) - Indexed VSAM KSDS
- Card Cross-reference (XREFFILE) - Indexed VSAM KSDS
- Transaction Master (TRANSACT) - Indexed VSAM KSDS
- Daily Transaction File (DALYTRAN) - Sequential file
- Transaction Category Balance (TCATBALF) - Indexed VSAM KSDS
- User Security File (USRSEC) - Indexed VSAM KSDS

**External Input Files:**
- Daily transaction feed from payment processors
- Customer data updates from external systems
- Account opening data from origination systems
- Card issuance data from card production systems

**Optional DB2 Tables:**
- Transaction Type table
- Transaction Category table
- Authorization log table

**Optional IMS Databases:**
- Customer hierarchical database
- Authorization request database

### Data Outputs

**VSAM File Updates:**
- Updated account balances and statuses
- Updated card information
- Updated transaction master file
- Updated transaction category balances

**Sequential Output Files:**
- Monthly statement files (text and HTML format)
- Transaction detail reports
- Daily rejection reports
- Transaction backup files
- Various operational reports

**Generation Data Groups (GDG):**
- Daily rejection files (retained for history)
- Transaction backup generations

**Optional External Outputs:**
- Messages to external systems via MQ queues
- Account extracts for downstream processing
- Authorization responses to payment processors

### Data Volume Characteristics
- **Record Counts**: System designed for demonstration and testing purposes; production volumes would vary by deployment
- **Primary Data Stores**: VSAM KSDS (Key-Sequenced Data Sets) for indexed access to accounts, cards, customers, and transactions
- **Transaction Volumes**: Supports daily batch processing of transaction files and real-time online access
- **Growth Rate**: Designed to accommodate typical credit card portfolio growth patterns
- **Data Retention**: Uses GDG for historical retention of key batch outputs

---

## 4. Processing Patterns

### Processing Type
**Mixed Architecture**: The system employs both batch and online processing models to handle different operational requirements:
- **Online Processing**: Real-time CICS transactions for user interactions, inquiries, and updates
- **Batch Processing**: Scheduled jobs for high-volume transaction posting, statement generation, and reporting

### Execution Schedule

**Daily Batch Jobs:**
- **Transaction Posting** (POSTTRAN) - Posts daily transactions to account balances and updates transaction master file
- **Daily Rejection Processing** (DALYREJS) - Processes and reports transactions that failed validation
- **Transaction Indexing** (TRANIDX) - Maintains transaction indexes for efficient retrieval
- **Transaction Backup** (TRANBKP) - Creates backup copies of transaction files

**Monthly Batch Jobs:**
- **Statement Generation** (CREASTMT) - Generates monthly statements for all active cards with detailed transaction listings in both text and HTML formats
- **Interest Calculation** (INTCALC) - Calculates interest charges on outstanding balances and posts to accounts

**On-Demand Batch Jobs:**
- **Account Data Loading** (ACCTFILE) - Initial load or bulk update of account data
- **Card Data Loading** (CARDFILE) - Initial load or bulk update of card data
- **Customer Data Loading** (CUSTFILE) - Initial load or bulk update of customer information
- **Cross-reference Loading** (XREFFILE) - Load card-to-account-to-customer relationships
- **Transaction Data Loading** (TRANFILE) - Load historical transaction data
- **Transaction Reporting** (TRANREPT) - Generate transaction detail reports for specified periods
- **File Management** (OPENFIL, CLOSEFIL) - Open and close VSAM files for processing
- **GDG Definition** (DEFGDGB, DEFGDGD) - Define and manage Generation Data Groups

**Real-time Online Processing:**
- User authentication and session management
- Account inquiries and updates
- Card management operations
- Transaction inquiries at multiple detail levels
- Billing inquiries
- User administration functions
- Report generation on demand

### Key Dependencies
Batch job dependencies follow a logical sequence:
- Transaction posting must complete before daily rejection processing
- Account and card files must be loaded before transaction processing
- Cross-reference file must be current before statement generation
- Statement generation requires sorted transaction file by card number and transaction ID
- Interest calculation depends on current account balances from transaction posting

### Error Handling Approach

**Batch Processing Error Handling:**
- File status checking for all VSAM operations
- Rejection file creation for transactions that fail validation (DALYREJS)
- SYSPRINT and SYSOUT for job execution logs and error messages
- Return code checking with conditional job execution (COND parameter)
- ABEND handling routines in COBOL programs

**Online Processing Error Handling:**
- CICS response code checking (RESP and RESP2)
- User-friendly error messages displayed on screens
- Invalid key handling (PF key validation)
- Field-level validation for user inputs
- Transaction rollback capabilities via CICS SYNCPOINT
- PGMIDERR condition handling for missing programs
- Error message display with color coding (green for info, red for errors)

**Data Validation:**
- Mandatory field checking (user ID, password, account numbers)
- Numeric field validation
- Date format validation
- Credit limit enforcement
- Account status validation
- User type and authorization checking

### Performance Characteristics
- **Batch Processing Windows**: Designed to complete daily processing within overnight batch window
- **Response Time Requirements**: Real-time CICS transactions expected to respond within seconds
- **Throughput Requirements**: Batch jobs designed to process typical daily transaction volumes
- **File Access Methods**: Indexed VSAM (KSDS) for direct access by key, sequential access for batch processing
- **Transaction Volume Handling**: Supports paging through large result sets in online inquiries

---

## 5. Integration & External Interactions

### External Systems

| System Name | Data Exchanged | Direction | Method | Frequency |
|------------|---------------|-----------|---------|-----------|
| Payment Processors | Daily transaction feed | Inbound | Sequential File | Daily |
| Customer Origination Systems | New customer and account data | Inbound | Sequential File | As needed |
| Card Production Systems | Card issuance data | Inbound | Sequential File | As needed |
| Statement Distribution Systems | Customer statements | Outbound | Sequential File | Monthly |
| Downstream Analytics Systems | Account extracts (optional MQ module) | Outbound | Message Queue | Asynchronous |
| External Databases | Transaction type data (optional DB2 module) | Both | DB2 SQL | Real-time |
| Authorization Systems | Authorization requests and responses (optional IMS module) | Both | IMS DB / MQ | Real-time |

### File Interfaces

**Inbound Files:**
- Daily Transaction File (DALYTRAN) - Contains: Card transactions from payment network - Format: Fixed-length sequential file
- Account Data File - Contains: Account master data for initial load - Format: Sequential file
- Card Data File - Contains: Card master data for initial load - Format: Sequential file
- Customer Data File - Contains: Customer information for initial load - Format: Sequential file
- Cross-reference File - Contains: Card-to-account-to-customer mappings - Format: Sequential file

**Outbound Files:**
- Statement Files (Text and HTML) - To: Statement distribution system - Contains: Monthly customer statements - Format: Fixed-length text and variable HTML
- Transaction Reports - To: Management reporting - Contains: Transaction detail and summary reports - Format: Fixed-length text
- Daily Rejection File - To: Operations team - Contains: Rejected transactions requiring manual review - Format: Fixed-length sequential (GDG)
- Account Extract (optional) - To: Analytics systems - Contains: Account data snapshots - Format: Sequential file

### Database Integration

**Optional DB2 Integration (app-transaction-type-db2 module):**
- Transaction Type tables shared across systems
- Transaction Category tables for classification
- SQL operations for CRUD (Create, Read, Update, Delete) on transaction types
- Real-time database access from CICS transactions
- Batch DB2 LOAD and UNLOAD utilities for data movement

**Optional IMS DB Integration (app-authorization-ims-db2-mq module):**
- Hierarchical customer database access
- Authorization request database for pending approvals
- DL/I calls for IMS database operations
- Batch IMS database load and unload utilities

### Middleware & Infrastructure

**CICS (Customer Information Control System):**
- Transaction server for all online processing
- Manages user sessions and terminal I/O
- Provides transaction integrity and rollback capabilities
- File control for VSAM access
- Terminal control for BMS map I/O

**VSAM (Virtual Storage Access Method):**
- Primary data storage mechanism
- KSDS (Key-Sequenced Data Sets) for indexed files
- ESDS (Entry-Sequenced Data Sets) for sequential files
- RRDS (Relative Record Data Sets) for direct access files

**Optional Message Queuing (MQ) (app-authorization-ims-db2-mq and app-vsam-mq modules):**
- Asynchronous message processing for authorization requests
- Account data extraction via message queues
- Integration with external systems via point-to-point messaging
- Queue-based communication for decoupled processing

**JCL (Job Control Language):**
- Batch job scheduling and execution
- File allocation and management
- Program execution control
- Conditional processing based on return codes

### Integration Patterns

**File-Based Integration:**
- Primary pattern for bulk data exchange
- Sequential file processing for transaction feeds
- Fixed-length record formats for predictable parsing
- GDG for historical retention and backup

**Database Integration:**
- Real-time SQL access for reference data (optional DB2 module)
- Hierarchical database access for complex customer relationships (optional IMS module)
- Shared tables across multiple applications

**Message-Based Integration:**
- Asynchronous processing via MQ queues (optional)
- Event-driven architecture for authorization processing
- Decoupled communication for external system integration

**Real-Time Online Access:**
- CICS transaction processing for user-initiated operations
- Direct VSAM file access for immediate data retrieval
- BMS (Basic Mapping Support) for screen I/O

---

## 6. Critical Constraints & Requirements

### Compliance Requirements

**Regulatory Considerations:**
Given the nature of credit card processing, the system likely needs to comply with:
- PCI-DSS (Payment Card Industry Data Security Standard) for handling credit card data
- Financial industry regulations for transaction processing and reporting
- Data privacy regulations for customer personal information
- Audit logging requirements for financial transactions

**Audit Capabilities:**
- Transaction logging at multiple levels
- User action tracking through CICS transaction IDs
- File status and error logging in batch jobs
- Generation Data Groups for historical retention
- Optional DB2 transaction logging (in authorization module)

**Data Retention:**
- GDG-based retention for daily rejection files
- Statement file retention for customer service
- Transaction history retention in master files
- Backup and recovery capabilities through file copies

### Security Requirements

**Authentication & Authorization:**
- User authentication via USRSEC file with user ID and password
- Role-based access control with two user types: Admin and Regular
- User type validation before allowing access to administrative functions
- Session management through CICS COMMAREA
- Program-level security checking before function execution

**Access Control:**
- Admin-only functions restricted to users with admin user type
- Account view limited by user type (admins see all, regular users see assigned accounts only)
- Card listing filtered based on user permissions
- Optional program inquiry to check if features are installed before access

**Sensitive Data Handling:**
- User passwords stored in USRSEC file (system should employ encryption in production)
- Credit card numbers (16-digit) stored and transmitted
- Customer SSN (Social Security Number) stored in customer records
- CVV codes stored with card records
- Personal Identifiable Information (PII) in customer records including addresses and phone numbers

**Security Features:**
- Invalid key checking to prevent unauthorized function access
- Field-level input validation to prevent injection attacks
- Transaction integrity through CICS SYNCPOINT for rollback capabilities
- Error message sanitization (no sensitive data in error messages)

### Top Critical Business Rules

1. **Account Balance Management**: Account balances must accurately reflect all posted transactions, including credits and debits, with current cycle tracking for statement generation.

2. **Credit Limit Enforcement**: System maintains account credit limits and cash credit limits; transactions exceeding these limits should be flagged for authorization processing.

3. **Account Status Control**: Account active status determines whether transactions can be processed; inactive accounts should not accept new transactions.

4. **Card-Account-Customer Relationship Integrity**: The cross-reference file maintains the critical linkage between cards, accounts, and customers; this relationship must remain consistent across all operations.

5. **Transaction Categorization**: All transactions must be categorized by transaction type and category code for proper interest calculation, statement presentation, and balance tracking.

6. **Interest Rate Calculation**: Interest rates are determined by disclosure group, which is based on account group and transaction category; rates must be applied consistently during monthly interest calculation.

7. **Statement Generation Timing**: Monthly statements must be generated for all active cards, including all transactions within the statement period, sorted by card and transaction ID.

8. **Daily Transaction Posting**: Daily transactions must be posted to accounts before end-of-day processing to maintain accurate balances; rejected transactions must be logged for manual review.

9. **User Type-Based Access**: Administrative functions are restricted to admin user type; regular users can only access assigned accounts and standard inquiry functions.

10. **Transaction Category Balance Maintenance**: Category-level balances must be maintained by account, transaction type, and category for accurate interest calculation and reporting.

### Performance Requirements

**Batch Processing Windows:**
- Daily transaction posting should complete within overnight batch window to ensure balances are current for next business day
- Monthly statement generation must complete before statement distribution deadline
- Interest calculation timing must align with billing cycle requirements

**Response Time:**
- Online CICS transactions expected to respond within seconds for user interactions
- Account and card inquiries should provide near-instant results through indexed VSAM access
- Transaction inquiries may take longer for large transaction histories due to sequential browsing

**Throughput:**
- Batch jobs designed to process typical daily transaction volumes within allocated time windows
- Real-time CICS system should support concurrent users accessing accounts, cards, and transactions
- Indexed file access (VSAM KSDS) provides efficient retrieval for high-volume operations

**Data Volume Considerations:**
- System designed as a demonstration application; production deployment volumes would depend on portfolio size
- GDG usage provides efficient historical file management
- Paging capabilities in online inquiries manage large result sets

### Availability Requirements

**Business Hours:**
- Online CICS system should be available during business hours for customer service and account management operations
- Extended hours may be required for different time zones and global operations

**Batch Processing Windows:**
- Overnight batch window for daily processing (transaction posting, rejection processing)
- Month-end window for statement generation and interest calculation
- Flexible scheduling for on-demand jobs (data loading, reporting)

**System Dependencies:**
- CICS region availability for online processing
- VSAM file availability for both online and batch operations
- Optional DB2 database availability for transaction type management module
- Optional IMS database availability for authorization module
- Optional MQ queue manager availability for asynchronous processing

**Recovery Capabilities:**
- File backup through transaction backup jobs
- GDG-based historical retention for recovery
- CICS transaction rollback for online processing failures
- Checkpoint/restart capabilities in batch programs (indicated by ABEND handling)

---

## Summary

The CardDemo credit card management system is a comprehensive mixed-mode application that handles the complete lifecycle of credit card operations. With 40 distinct capabilities spanning authentication, account management, card management, customer management, transaction processing, billing, and reporting, the system serves multiple stakeholder groups including customer service representatives, account managers, and administrators.

The system processes data across 10 core business entities stored primarily in VSAM files, with optional DB2 and IMS database integration for enhanced functionality. The mixed processing architecture efficiently handles both real-time user interactions through CICS online transactions and high-volume batch processing for transaction posting, statement generation, and interest calculations.

Optional modules extend the system's capabilities to include DB2-based transaction type management, IMS-based authorization processing, and MQ-based asynchronous integration with external systems. These modular enhancements demonstrate the system's flexibility to adapt to different technical environments and integration requirements.

Critical business rules govern account balance management, credit limit enforcement, transaction categorization, and interest calculation. Security requirements include user authentication, role-based access control, and sensitive data protection. The system's design emphasizes data integrity, audit capability, and operational reliability suitable for financial services processing.

This high-level requirements document provides the breadth of coverage needed for subject matter expert review, user story creation, and migration planning activities.

---

**End of Document**
