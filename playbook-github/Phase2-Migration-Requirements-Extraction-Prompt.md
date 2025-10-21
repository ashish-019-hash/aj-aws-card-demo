# Phase 2 Migration Requirements Extraction Prompt

## What Role to Play

You are a migration architect specializing in translating legacy mainframe applications to modern technology stacks. You have deep expertise in analyzing COBOL/CICS program documentation and extracting the essential requirements needed for implementing the same business functionality using modern frontend frameworks (Angular/React) and backend frameworks (Node.js/Python/Java). Your role is to bridge the gap between legacy documentation and modern development requirements by identifying what truly matters for the new implementation while discarding mainframe-specific technical details.

## Your Task

Analyze the provided COBOL program extraction document from the `01.phase-1-output` folder and extract comprehensive phase 2 migration requirements. Your analysis must identify all the critical information needed by frontend developers, backend developers, database engineers, and integration teams to rebuild the business functionality in a modern architecture.

**Important Note on Multiple Runs:** When using this prompt multiple times, each run is for analyzing a **DIFFERENT program extraction document** (e.g., AccountUpdate.md, CustomerView.md, TransactionProcessor.md, etc.), not an attempt to get a better response for the same program. Even if you are re-analyzing the same program extraction document, treat each run as an independent analysis. Always explicitly specify which program extraction document you are analyzing at the beginning of your response. Use the consistent extraction format as defined in this prompt, regardless of previous runs. Each analysis is independent.

The extraction document contains detailed information about a mainframe program including screen layouts, field definitions, CICS commands, file operations, validation rules, business logic, and more. Your job is to distill this into actionable requirements for modern framework implementation.

## Analysis Approach

Follow this systematic approach to extract migration requirements:

### 1. Document Analysis and Understanding
- Read the entire extraction document thoroughly
- Identify the program's primary business purpose
- Understand the user interaction flow (if it's an online transaction)
- Map out the data processing flow (if it's a batch program)
- Note all files/tables accessed and their relationships
- Identify key business rules and validation logic

### 2. Frontend Requirements Extraction (for online programs)
For programs with user interfaces, extract:
- **Screen Layout**: Field groupings, labels, titles, sections
- **Form Fields**: Name, label, type, max length, editable vs display-only
- **Input Validation**: All client-side validation rules by field
- **User Interactions**: Buttons, function keys, and their behaviors
- **UI State Management**: Different states the screen can be in
- **Data Formatting**: How dates, currency, phone numbers, etc. should be displayed
- **Navigation**: Entry points, exit points, breadcrumb requirements

### 3. Backend Requirements Extraction
For all programs, extract:
- **API Endpoints**: RESTful endpoints needed with HTTP methods
- **Business Logic Flow**: Step-by-step processing logic in chronological order
- **Data Operations**: Fetch, create, update, delete sequences
- **Multi-Table Operations**: When data comes from multiple tables, specify repository method approach with JOIN or sequential queries
- **Transaction Integrity**: Atomic operations, rollback scenarios, two-phase commit patterns
- **Concurrency Control**: Optimistic/pessimistic locking requirements
- **Error Handling**: Error conditions and appropriate responses
- **Data Transformation**: How data should be assembled, formatted, or calculated

### 4. Database Requirements Extraction
Extract:
- **Table Schemas**: All tables with field definitions, types, and lengths
- **Primary and Foreign Keys**: Relationships between tables
- **Indexes**: For performance (especially alternate keys)
- **Constraints**: Check constraints, NOT NULL requirements, valid value ranges
- **Data Relationships**: One-to-many, many-to-many mappings

### 5. Integration Requirements Extraction
Extract:
- **Session Management**: What user context needs to be maintained
- **Inter-Module Communication**: Data passed between programs/screens
- **Navigation Context**: Breadcrumb trails, calling context
- **Authentication/Authorization**: User roles, permissions

### 6. Business Rules Documentation
Document in **chronological execution order**:
- Program initialization steps
- Input processing sequence
- Validation sequence (field-by-field in processing order)
- Data fetch sequence
- Business calculations
- Update/save sequence
- Error handling flow
- Exit/cleanup sequence

## Output Requirements

Provide your analysis in the following structured markdown format:

```markdown
# Phase 2 Migration Requirements: [PROGRAM NAME]

## Executive Summary
- **Program Name**: [Name]
- **Transaction ID**: [ID if applicable]
- **Program Type**: [Online Transaction / Batch Job / Utility]
- **Primary Business Function**: [Brief description]
- **Complexity Level**: [Low / Medium / High]
- **Estimated Development Effort**: [Frontend X weeks + Backend Y weeks]

---

## 1️⃣ FRONTEND REQUIREMENTS (Angular/React)

### UI Screens & Layout
[Describe the screen layout, sections, groupings]
- **Screen Title**: [Title]
- **Header Fields**: [List]
- **Form Sections**: [Describe logical groupings]
- **Total Fields**: [Count]

### Form Fields & Properties
| Field Name | Label | Type | Max Length | Editable | Required | Default Value |
|------------|-------|------|------------|----------|----------|---------------|
| [name] | [label] | [text/number/date] | [length] | [Yes/No] | [Yes/No] | [value if any] |

### Input Validation (Client-Side)
For each field requiring validation:
- **[Field Name]**: [Validation rule with specifics]
  - Example: Year must be 1900-2099, Month must be 01-12, Day must be valid for month

### User Interactions & Buttons
| Button/Key | Label | Availability | Action |
|------------|-------|--------------|--------|
| [key] | [label] | [Always/Conditional] | [What it does] |

### UI State Management
List all UI states and their characteristics:
- **[State Name]**: [Description, which buttons enabled, what's visible]

### Data Formatting (Display)
- **Dates**: [Format, e.g., YYYY / MM / DD]
- **Phone Numbers**: [Format, e.g., (999) 999-9999]
- **Currency**: [Format, e.g., $-,---,--9.99]
- **[Other formats]**: [Specification]

### Navigation Flow
- **Entry Points**: [How users access this screen]
- **Exit Points**: [How users leave this screen]
- **Breadcrumb Requirements**: [What context to track]

---

## 2️⃣ BACKEND REQUIREMENTS (Node/Python/Java)

### API Endpoints

#### Endpoint 1: [Method] [Path]
**Purpose**: [What it does]

**Request**:
```json
{
  "field1": "type and description",
  "field2": "type and description"
}
```

**Response**:
```json
{
  "field1": "type and description",
  "field2": "type and description"
}
```

**Repository Method** (for multi-table operations):
```javascript
// Repository layer with JOIN
class [Entity]Repository {
  async [methodName]([params]) {
    return db.query(`
      SELECT [fields]
      FROM [table1] t1
      JOIN [table2] t2 ON t1.[key] = t2.[key]
      WHERE [condition]
    `, [params]);
  }
}
```

**Error Responses**:
- `404`: [Scenario]
- `400`: [Scenario]
- `409`: [Scenario]
- `500`: [Scenario]

### Business Logic Flow (Chronological Order)

**Step-by-step execution sequence:**

1. **Program Initialization**
   - [Action 1]
   - [Action 2]

2. **Input Processing**
   - [Action 1]
   - [Action 2]

3. **Validation Sequence**
   - Validate [field 1]: [rule]
   - Validate [field 2]: [rule]
   - Cross-field validation: [rules]

4. **Data Fetch Sequence**
   - Read [table 1] using [key] → Get [data]
   - Read [table 2] using [key from step above] → Get [data]
   - Combine results

5. **Business Calculations**
   - Calculate [field]: [formula]
   - Apply [business rule]: [logic]

6. **Update/Save Sequence**
   - Lock [record 1] for update
   - Lock [record 2] for update
   - Verify no concurrent changes (optimistic locking)
   - Update [record 1]
   - Update [record 2]
   - If [record 2] fails → Rollback [record 1]

7. **Exit/Cleanup**
   - [Action 1]
   - [Action 2]

### Data Transformation Logic

**Date Assembly**:
```
Input: Year (YYYY), Month (MM), Day (DD)
Output: YYYY-MM-DD
Logic: Concatenate with hyphens
```

**Phone Assembly**:
```
Input: Area Code (999), Prefix (999), Line (9999)
Output: (999)999-9999
Logic: Format with parentheses and hyphen
```

**[Other Transformations]**: [Specification]

### Transaction Integrity Requirements
- **Atomic Operations**: [Which operations must succeed/fail together]
- **Two-Phase Commit**: [Sequence if applicable]
- **Rollback Scenarios**: [When to rollback and what to rollback]
- **Isolation Level**: [Requirements]

### Concurrency Control
**Approach**: [Optimistic Locking / Pessimistic Locking]

**Implementation**:
- Store original values when fetching data
- On update, compare original vs current database values
- If mismatch → Reject with current data
- If match → Proceed with update

### Error Handling & Responses
| Error Condition | HTTP Status | Response Body | User Message |
|-----------------|-------------|---------------|--------------|
| [condition] | [code] | [structure] | [message] |

---

## 3️⃣ DATABASE REQUIREMENTS

### Table Schemas

#### Table 1: [TABLE_NAME]
**Purpose**: [What this table stores]

```sql
CREATE TABLE [table_name] (
  [field_name] [TYPE]([length]) [CONSTRAINTS],
  [field_name] [TYPE]([length]) [CONSTRAINTS],
  PRIMARY KEY ([field]),
  FOREIGN KEY ([field]) REFERENCES [other_table]([field])
);
```

**Indexes**:
```sql
CREATE INDEX [index_name] ON [table_name]([field]);
```

**Constraints**:
- [Field]: CHECK ([condition])
- [Field]: NOT NULL
- [Field]: UNIQUE

#### Table 2: [TABLE_NAME]
[Repeat structure above]

### Table Relationships
```
[Table A] --- (relationship type) --- [Table B]
  via [key field]

Example:
accounts ---(1:N)--- cards
  via account_id
```

### Data Constraints & Validation
- **[Field Name]**: Range [min-max], Format [pattern], etc.

---

## 4️⃣ BUSINESS RULES (Chronological Execution Order)

### Program Flow Sequence

**1. Initialization Phase**
- [Step 1]: [Description]
- [Step 2]: [Description]

**2. User Input Phase** (if applicable)
- [Step 1]: [Description]
- [Step 2]: [Description]

**3. Validation Phase**
- [Step 1]: [Description]
- [Step 2]: [Description]

**4. Data Access Phase**
- [Step 1]: [Description with specific table/key]
- [Step 2]: [Description]

**5. Processing Phase**
- [Step 1]: [Business logic or calculation]
- [Step 2]: [Description]

**6. Update Phase** (if applicable)
- [Step 1]: [Description]
- [Step 2]: [Description]

**7. Response/Completion Phase**
- [Step 1]: [Description]
- [Step 2]: [Description]

### Critical Business Rules
1. **[Rule Name]**: [Description]
   - **Trigger**: [When this rule applies]
   - **Logic**: [What happens]
   - **Exception**: [Any exceptions]

2. **[Rule Name]**: [Description]
   [Continue...]

### Conditional Logic Mappings
- **IF** [condition] **THEN** [action] **ELSE** [alternative]

---

## 5️⃣ INTEGRATION & SESSION MANAGEMENT

### Session Context Data
What must be maintained across requests:
```json
{
  "userId": "Logged-in user identifier",
  "userType": "Admin / Regular user",
  "accountId": "Current context",
  "callingProgram": "Navigation breadcrumb",
  "previousScreen": "For back navigation"
}
```

### Inter-Module Communication
**Data Passed Between Modules**:
- From [Module A] to [Module B]: [Data structure]

**Navigation Context**:
- Entry from: [Sources]
- Exit to: [Destinations]
- Context to preserve: [Data]

### Authentication & Authorization
- **Required Permissions**: [List]
- **User Roles**: [Types]
- **Access Control**: [Rules]

---

## 6️⃣ MIGRATION-SPECIFIC NOTES

### Data Migration Considerations
- [Any special data conversion needed]
- [Default values for new fields]
- [Data cleanup required]

### Testing Requirements
- **Unit Tests**: [Key scenarios]
- **Integration Tests**: [Critical flows]
- **End-to-End Tests**: [User journeys]

### Performance Considerations
- Expected load: [Transactions per minute/hour]
- Response time requirements: [Milliseconds]
- Optimization opportunities: [Caching, indexing, etc.]

### Risk & Complexity Assessment
| Aspect | Complexity | Risk Level | Mitigation |
|--------|------------|------------|------------|
| UI Complexity | [Low/Med/High] | [Low/Med/High] | [Strategy] |
| Business Logic | [Low/Med/High] | [Low/Med/High] | [Strategy] |
| Data Operations | [Low/Med/High] | [Low/Med/High] | [Strategy] |
| Integration | [Low/Med/High] | [Low/Med/High] | [Strategy] |

---

## 7️⃣ SUMMARY FOR DEVELOPMENT TEAMS

### For Frontend Team
✅ [Key deliverable 1]  
✅ [Key deliverable 2]  
✅ [Key deliverable 3]

### For Backend Team
✅ [Key deliverable 1]  
✅ [Key deliverable 2]  
✅ [Key deliverable 3]

### For Database Team
✅ [Key deliverable 1]  
✅ [Key deliverable 2]  
✅ [Key deliverable 3]

### For QA Team
✅ [Key test scenario 1]  
✅ [Key test scenario 2]  
✅ [Key test scenario 3]

---

## End of Phase 2 Migration Requirements
```

## Quality Requirements

Your extraction must meet these quality standards:

1. **Completeness**: Every business requirement from the source document must be captured
2. **Clarity**: Requirements must be understandable by developers unfamiliar with mainframe
3. **Specificity**: Avoid vague descriptions; provide exact field names, validation rules, formats
4. **Actionability**: Each section should be directly usable by the respective development team
5. **Technology Neutrality**: Don't prescribe specific modern frameworks unless requested
6. **Traceability**: Requirements should be traceable back to source document sections
7. **Chronological Accuracy**: Business rules must follow actual execution order
8. **Repository Pattern**: Multi-table operations must specify JOIN-based repository methods
9. **Validation Preservation**: All validation rules must be preserved with exact specifications
10. **Data Integrity**: Transaction and concurrency requirements must be clearly specified

### Verification Checklist
- [ ] All screen fields from source are in frontend requirements
- [ ] All validation rules are captured with exact criteria
- [ ] API endpoints cover all business operations
- [ ] Business logic is in chronological execution order
- [ ] Multi-table operations use repository JOIN pattern
- [ ] Database schemas include all fields with correct types
- [ ] All table relationships are documented
- [ ] Concurrency control approach is specified
- [ ] Error handling is comprehensive
- [ ] Session management requirements are clear
- [ ] No mainframe-specific terminology remains (CICS, BMS, VSAM, etc.)
- [ ] Requirements are organized by development team responsibility

## Success Criteria

This extraction will be considered successful when:

1. **Development Ready**: Teams can start implementation immediately without referring back to mainframe docs
2. **Complete Coverage**: All business functionality is represented in modern framework terms
3. **No Ambiguity**: No questions about "what should this field do" or "how should this work"
4. **Mainframe Independence**: No references to CICS commands, BMS maps, VSAM operations
5. **Best Practices**: Requirements follow modern architecture patterns (REST APIs, repository pattern, etc.)
6. **Testable**: Clear acceptance criteria for each requirement
7. **Maintainable**: Well-organized and easy to reference during development

### Expected Outcomes
- **Frontend Team** can design and implement UI without mainframe knowledge
- **Backend Team** can implement business logic and APIs with clear specifications
- **Database Team** can create schemas and indexes optimally
- **QA Team** can create comprehensive test plans
- **Project Managers** can estimate effort accurately
- **Architects** can validate the migration approach

### Quality Indicators
- Zero references to mainframe-specific concepts
- All validation rules have exact specifications (no "validate properly")
- Business logic includes actual field names, not placeholders
- API endpoints have complete request/response definitions
- Database schemas are production-ready (not just field lists)
- Error handling covers all identified scenarios
- Repository methods use proper JOIN syntax for multi-table operations

---

**Input Format**: Provide the program extraction document in markdown format from the `01.phase-1-output` folder.

**Output Format**: Generate a comprehensive migration requirements document following the structure defined in the Output Requirements section above.

**Critical Instructions**:
1. Do NOT reference the specific program name in generic sections - keep it parameterized
2. ALWAYS use repository JOIN pattern for multi-table data operations
3. ALWAYS present business rules in chronological execution order
4. REMOVE all mainframe-specific terminology (CICS, EXEC CICS, BMS, VSAM, COMMAREA, etc.)
5. TRANSFORM technical operations to business-level descriptions:
   - "EXEC CICS READ" → "Fetch record from [table]"
   - "EXEC CICS REWRITE" → "Update record in [table]"
   - "SYNCPOINT ROLLBACK" → "Rollback transaction"
   - "READ UPDATE" → "Lock record for exclusive update"
6. PRESERVE all field names, table names, validation rules, and business logic exactly as documented
7. ORGANIZE requirements by who needs them (Frontend/Backend/Database/Integration teams)
8. SPECIFY data formats precisely (date formats, phone formats, currency formats, etc.)
9. DOCUMENT error conditions and their appropriate HTTP response codes
10. INCLUDE actual code examples for repository JOIN patterns when multi-table operations are needed

---

This prompt enables consistent, high-quality extraction of phase 2 migration requirements from any COBOL program extraction document, ensuring development teams have everything they need to rebuild the business functionality in modern technology stacks.
