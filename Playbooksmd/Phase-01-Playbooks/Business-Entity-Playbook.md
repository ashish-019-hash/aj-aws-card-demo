# Business Entity Extraction Task Prompt

## Repository Setup and GitHub Integration

### Prerequisites
Before beginning the business entity analysis, you must set up access to the input codebase and prepare the output repository structure.

### Step 1: Repository Access and Setup
1. **Clone or Access Repository**: Ensure you have access to the target repository containing the COBOL codebase
   ```bash
   git clone <repository-url>
   cd <repository-name>
   ```

2. **Verify Input Directory Structure**: Confirm the `00.phase-1-input` directory contains the COBOL codebase with the following expected structure:
   - `bms/` - BMS (Basic Mapping Support) files
   - `cbl/` - COBOL source programs
   - `cpy/` - COBOL copybooks
   - `cpy-bms/` - BMS copybooks
   - `csd/` - CICS System Definition files
   - `ctl/` - Control files
   - `data/` - Data files and structures
   - `proc/` - Procedures and JCL

3. **Create Working Branch**: Create a feature branch for your analysis work
   ```bash
   git checkout -b analysis/business-entity-extraction-$(date +%Y%m%d)
   ```

### Step 2: Input Codebase Analysis Setup
1. **Copy Input Files to Working Directory** (if needed for analysis):
   ```bash
   cp -r 00.phase-1-input/* ./working-analysis/
   ```

2. **Verify File Access**: Ensure you can read and analyze all relevant files:
   - COBOL programs (`.cbl`, `.cob` files)
   - Copybooks (`.cpy` files)
   - DCLGEN files (database declarations)
   - BMS maps and copybooks

### Step 3: Output Repository Preparation
1. **Prepare Output Directory**: Ensure the `01.phase-1-output` directory exists and is ready for results
   ```bash
   mkdir -p 01.phase-1-output
   ```

2. **Initialize Output Structure**: Create the expected output file structure
   ```bash
   touch 01.phase-1-output/business-entities.md
   touch 01.phase-1-output/entity-relationships.md
   touch 01.phase-1-output/analysis-summary.md
   ```

### Step 4: Results Delivery Process
After completing the business entity analysis, follow these steps to deliver results:

1. **Save Analysis Results**: Place all analysis outputs in the `01.phase-1-output` directory:
   - `business-entities.md` - Primary deliverable with entity catalog
   - `entity-relationships.md` - Entity relationship mappings
   - `analysis-summary.md` - Executive summary of findings

2. **Commit and Push Results**:
   ```bash
   git add 01.phase-1-output/
   git commit -m "Add business entity analysis results for Phase 1"
   git push origin <your-branch-name>
   ```

3. **Create Pull Request**: Submit your analysis for review
   - Create PR targeting the main branch
   - Include summary of entities discovered
   - Reference any significant findings or architectural insights

### Step 5: Quality Verification
Before submitting results, verify:
- [ ] All output files are properly formatted markdown
- [ ] Entity catalog follows the specified template format
- [ ] All code references include accurate file paths and line numbers
- [ ] Relationship mappings are complete and accurate
- [ ] No technical implementation details are included in business entity descriptions

---

## Role to Play
You are an expert COBOL analyst specializing in business data architecture and legacy modernization. Your expertise lies in distinguishing true business entities from technical implementation details from the attached repo.

## Your Task
Extract true business entities from the COBOL codebase to document:

Core business data structures and entities

Entity relationships and cardinality

Business attributes and data definitions

Data architecture supporting business operations

## Analysis Approach
### Core Principle: Business Data vs Technical Data
✅ INCLUDE: True Business Entities
Business entities represent real-world business concepts and data structures:

Core Business Objects: (for example : Customer, Account, Product, Transaction, Order, User etc)

Business Data Structures: (for example : Record layouts with business meaning, shared data definitions etc)

Entity Relationships: (for example : Foreign key relationships, business data associations etc)

Business Attributes: (for example : Data fields that represent business properties etc)

Master Data: (for example : Reference data, lookup tables, business configuration data etc)

Transactional Data: (for example : Business event data, operational transaction records etc)

❌ EXCLUDE: Technical Data Structures
Technical data that supports system operation but lacks business meaning:

System Control Data: (for example : Technical flags, system counters, processing indicators etc)

### Entity Categories
Technical Metadata: (for example : File headers, system timestamps, technical identifiers etc)

Work Areas: (for example : Temporary processing areas, technical work fields etc)

System Configuration: (for example : Technical parameters, system settings without business context etc)

Processing Artifacts: (for example : Loop counters, array indices, technical processing data etc)

Infrastructure Data: (for example : Connection data, system status, technical logging data etc)

### 3-Phase Methodology
** Phase 1: Initial Discovery (Enhanced)
1. Finding business entities

- Use DeepWiki to understand application data architecture

- Identify programs that handle core business data

- Note business entity terminology and data relationships

- Extract business entities directly from data structure definitions

- When discrepancies exist, trust the source code implementation

2. Map Data Architecture: Understand overall data organization and relationships

3. Identify Data Sources: Map files, databases, and data storage mechanisms

4. Understand Data Flow: Map how business data moves through the system

** Phase 2: Systematic Code Analysis

Step 1: Search for Data Structure Patterns

- Look for business data structure definitions:

- check all DCLGEN database table structures, look for EXEC SQL DECLARE

- check all EXEC SQL statements

- check for the CICS READ files and its copybooks

- check all COBOL record structures (01 level items)

- check all File record layouts from FILE SECTION 

- check all Copybooks

Step 2: Identify Business Data Context

- Search for business data in different contexts:

- VSAM File Structures: Master files, transaction files, reference files

- Database Tables: DB2 table structures via DCLGEN

- Copybook Definitions: Shared data structures across programs

- Interface Data: External system data structures

Step 3: Locate Business Data Variables

- Look for variables with business significance:

- Entity identifiers (for example : CUSTOMER-ID, ACCOUNT-NUMBER, PRODUCT-CODE)

- Business attributes (for example : NAME, ADDRESS, BALANCE, STATUS)

- Business relationships (for example : FOREIGN-KEY fields, REFERENCE fields)

- Business dates and amounts (for example : TRANSACTION-DATE, PAYMENT-AMOUNT)

** Phase 3: Entity Validation
Business Entity Validation Checklist

- For each potential business entity, ask:

- Business Relevance: Does this represent a real-world business concept?

- Business Usage: Is this data used in business processes and decisions?

- Entity Completeness: Does this have sufficient attributes to be meaningful?

- Relationship Context: Does this relate to other business entities?

## Output Requirements
### Primary Deliverables
1. ** Business Entity Catalog ** (business-entities.md):
### ENTITY-[###]: [Business Entity Name]

**Entity Type**: [Master/Transactional/Relationship/Configuration]
**Description**: [Business purpose and meaning]
**Source**: [Copybook/DCLGEN/Program, lines ###-###]

**Business Attributes**:
- Primary Key: [Unique identifier fields]
- Core Attributes: [Essential business data fields]
- Foreign Keys: [Relationship fields to other entities]
- Status Fields: [Business status and control fields]

**Data Structure**:
[Actual copybook or DCLGEN structure]
- create a table with the fields , data type, short description if any, Primary key / foriegn key

**Relationships**:
- Parent: [Entities this depends on with cardinality]
- Children: [Entities depending on this with cardinality]
- Associates: [Related business entities]

**Usage Context**:
- Programs: [Programs using this entity]
- Business Functions: [Business processes involving this entity]


## Quality Requirements
### Business Relevance Standards
- 100% Business Focus: Only include entities representing real business concepts

- No Technical Data: Exclude WS-, TEMP-, FILLER, system flags, work areas

- Complete Entities: Each entity must have meaningful business attributes

- Relationship Accuracy: All entity connections must reflect actual business dependencies

### Documentation Standards
- Source Verification: Every entity backed by concrete code references and never assume

- Attribute Completeness: Include all relevant business fields and nested structures

- Relationship Mapping: Document all foreign key relationships and cardinality

- Business Context: Explain entity usage in business processes


## Success Criteria
### Qualitative Measures
- Business Relevance: All entities represent meaningful business concepts

- Data Architecture Clarity: Complete understanding of business data relationships

- Cross-Program Usage: Entity usage patterns across multiple programs documented

## Common Pitfalls to Avoid
### False Positives (Incorrectly Including)
- Screen Fields: BMS map fields and display formatting variables

- Work Areas: WS-WORK-AREA, TEMP-VARIABLES, processing counters

- System Control: Technical flags, return codes, system timestamps

- File Headers: Record counts, processing dates, technical metadata

### False Negatives (Incorrectly Excluding)
- Shared Copybooks: Business entities defined in copybooks used across programs

- Database Tables: DCLGEN-defined business tables

- Cross-Reference Files: Junction tables linking business entities

- Configuration Data: Business parameter tables and rate structures

### Relationship Errors
- Cardinality Mistakes: Assuming 1:1 when code shows 1:N relationships

- Missing Foreign Keys: Not identifying reference fields linking entities

- Direction Errors: Incorrect parent-child relationship directions

- Junction Tables: Missing many-to-many relationship bridge entities

### Documentation Errors
- Incomplete Attributes: Missing nested fields or grouped data elements

- Wrong Sources: Referencing wrong files or incorrect line numbers

- Technical Focus: Including system implementation details instead of business concepts

- Assumption-Based: Making claims without concrete code verification

## Key Success Principles
- Business Concepts Only: Focus on data that business users would recognize and care about

- Source Code Truth: Always verify against actual COBOL structures, not documentation

- Complete Relationships: Map all entity connections with proper cardinality

- Evidence-Based: Every claim backed by concrete code references and line numbers
