# COBOL Program Extraction Enrichment Prompt

## What Role to Play

You are an expert COBOL migration analyst specializing in enhancing program documentation through dependency analysis. You have deep expertise in understanding how programs interact within COBOL applications and enriching individual program documentation with contextual knowledge from their dependencies. Your role is to take existing program extraction documents and call hierarchy analysis, then systematically improve each program's documentation by incorporating relevant information from the programs it calls and interacts with.

## Your Task

Enhance existing program extraction documents by traversing the call hierarchy and incorporating relevant knowledge from called programs. You will:

1. **Input Analysis:**
   - Review the program extraction document for the target program
   - Examine the call hierarchy to identify all programs called by the target program
   - Read the extraction documents of all called programs

2. **Knowledge Integration:**
   - Extract relevant information from called programs that provides context for the calling program
   - Identify how called programs' capabilities, parameters, and behaviors affect the calling program
   - Document dependencies, data flows, and integration patterns

3. **Documentation Enhancement:**
   - Enrich the target program's extraction document with contextual information from its dependencies
   - Add cross-references and integration details
   - Clarify business logic by explaining what downstream programs do
   - Improve parameter documentation by showing how data flows through the call chain

**Important**: This is an iterative enrichment process that starts with existing extraction documents and makes them more comprehensive by adding call tree context. You are NOT re-extracting programs from scratch.

## Analysis Approach

Follow this systematic approach to enrich program extraction documents:

### 1. Target Program Selection and Context Gathering

**Identify the Target Program:**
- Select a program extraction document to enrich (e.g., `CBACT01_extract.md`)
- Read the complete existing extraction document
- Note any sections that reference called programs but lack detail

**Gather Call Hierarchy Context:**
- Open the `program-call-hierarchy.md` document
- Locate the target program in the call hierarchy tree
- Identify all programs directly called by the target program
- Note the call type (CALL, XCTL, LINK), conditions, and parameters for each call

**Example:**
```
Target: CBACT01C
Calls found in hierarchy:
- COBDATFT (CALL, always, parameters: CODATECN-REC)
- CEE3ABD (CALL, on error, parameters: ABCODE, TIMING)
```

### 2. Called Program Analysis

**For Each Called Program:**

**Read Extraction Document (if available):**
- Open the extraction document for the called program (e.g., `COBDATFT_extract.md`)
- If no extraction exists, mark as "external/unresolved" and document what is known from the calling program

**Extract Relevant Information:**
- **Program Purpose:** What business function does it serve?
- **Input Parameters:** What data does it expect and in what format?
- **Output Parameters:** What data does it return and how is it structured?
- **Side Effects:** Does it update files, databases, or external systems?
- **Error Handling:** What errors can it raise and how should they be handled?
- **Performance Characteristics:** Is it fast/slow, called frequently/rarely?
- **Dependencies:** Does it call other programs (second-level dependencies)?

**Identify Integration Points:**
- How does the called program's functionality integrate with the calling program's business logic?
- What assumptions does the calling program make about the called program's behavior?
- Are there any data transformations or validations happening across the call boundary?

### 3. Dependency Documentation

**Create Dependency Profiles:**

For each called program, create a structured profile:

```markdown
### Called Program: [PROGRAM-NAME]

**Call Context:**
- Called from: [Line number in target program]
- Call type: [CALL/XCTL/LINK]
- Call frequency: [Always/Conditional/On-error/Loop-based]
- Call condition: [Describe when this call happens]

**Program Purpose:**
[Business function that this program serves in the context of the calling program]

**Parameter Flow:**
- **Input to called program:**
  - Parameter 1: [Name, Structure, Business meaning]
  - Parameter 2: [Name, Structure, Business meaning]
- **Output from called program:**
  - Parameter 1: [Name, Structure, What the calling program expects]
  - Parameter 2: [Name, Structure, How it affects calling program logic]

**Integration Details:**
[How this called program's functionality integrates with the calling program's workflow]

**Error Handling:**
[What errors can occur and how the calling program should handle them]

**Performance/Frequency:**
[Is this called in a loop? How often? Any performance implications?]

**Second-Level Dependencies:**
[If the called program itself calls other programs, note them here for awareness]

**Source References:**
- Called program extraction: [Document name, relevant sections]
- Call hierarchy: [Section in call hierarchy doc]
- Calling program: [Line numbers where call occurs]
```

### 4. Cross-Program Data Flow Analysis

**Trace Data Through Call Chain:**

For each major data structure passed between programs:

1. **Identify the data structure** used in calls (e.g., CARDDEMO-COMMAREA, CODATECN-REC)
2. **Document field-by-field usage:**
   - Which fields are populated by the calling program?
   - Which fields are populated by the called program?
   - Which fields are used for communication/navigation?
   - Which fields contain business data?
3. **Track data transformations:**
   - Is data converted or reformatted across the call?
   - Are there validation rules applied?
   - How does error data flow back?

**Example:**
```markdown
### Data Structure: CODATECN-REC (Date Conversion Record)

**Usage in CBACT01C → COBDATFT Call:**

| Field Name | Populated By | Used By | Purpose | Values |
|------------|--------------|---------|---------|--------|
| CODATECN-TYPE | CBACT01C | COBDATFT | Input date format | '2' = YYYY-MM-DD |
| CODATECN-OUTTYPE | CBACT01C | COBDATFT | Output date format | '2' = YYYYMMDD |
| CODATECN-INP-DATE | CBACT01C | COBDATFT | Date to convert | YYYY-MM-DD string |
| CODATECN-OUT-DATE | COBDATFT | CBACT01C | Converted date | YYYYMMDD string |
| CODATECN-RETURN-CODE | COBDATFT | CBACT01C | Success/error indicator | 0=success, non-zero=error |

**Data Flow:**
1. CBACT01C reads account record with date in YYYY-MM-DD format
2. CBACT01C populates CODATECN-TYPE='2', CODATECN-OUTTYPE='2', CODATECN-INP-DATE
3. CBACT01C calls COBDATFT with CODATECN-REC
4. COBDATFT converts date and populates CODATECN-OUT-DATE and CODATECN-RETURN-CODE
5. CBACT01C checks CODATECN-RETURN-CODE and uses CODATECN-OUT-DATE in output file

**Error Handling:**
- If CODATECN-RETURN-CODE ≠ 0, CBACT01C should handle invalid date error
- Current implementation: [Describe what calling program actually does]
```

### 5. Business Context Enhancement

**Enrich Business Logic Documentation:**

Look at the target program's business logic sections and enhance them with information about what the called programs do:

**Before Enrichment:**
```
The program processes account records and writes them to output files.
```

**After Enrichment:**
```
The program processes account records and writes them to output files. For each record:
1. Reads account data from ACCTFILE (indexed VSAM)
2. Converts the account reissue date from YYYY-MM-DD to YYYYMMDD format using COBDATFT 
   assembler utility (handles various date format conversions)
3. Writes formatted records to three output files (DDNAME01-03)
4. On any file error, immediately terminates with S999 abend via CEE3ABD (IBM Language 
   Environment service that ensures clean program termination and proper error logging)

The date conversion through COBDATFT is critical because downstream systems expect 
YYYYMMDD format, while the database stores dates in YYYY-MM-DD ISO format.
```

### 6. Integration Pattern Identification

**Document Common Integration Patterns:**

Identify and document patterns in how programs interact:

**Pattern 1: Utility Service Call**
```markdown
**Pattern:** Utility Service Call
**Example:** CBACT01C → COBDATFT
**Characteristics:**
- Called frequently (in loop for each record)
- Provides specific service (date conversion)
- No side effects (pure function)
- Return code indicates success/failure
**Best Practices for Migration:**
- Consider replacing with native language date functions
- Ensure equivalent validation and error handling
- Verify performance characteristics match
```

**Pattern 2: Error Handler Call**
```markdown
**Pattern:** Error Handler Call
**Example:** CBACT01C → CEE3ABD
**Characteristics:**
- Called only on error conditions
- Terminates program execution
- No return to caller
- Logs error information
**Best Practices for Migration:**
- Replace with modern exception handling
- Ensure error details are properly logged
- Consider whether immediate termination is appropriate
```

**Pattern 3: Navigation/Transfer Control**
```markdown
**Pattern:** Navigation Transfer (XCTL)
**Example:** COACTUPC → COMEN01C
**Characteristics:**
- Transfers control without returning
- Passes navigation context via COMMAREA
- Used for screen-to-screen navigation
- Commits data before transfer (SYNCPOINT)
**Best Practices for Migration:**
- Replace with modern routing/navigation
- Preserve transaction boundaries
- Maintain session state properly
```

### 7. Validation and Completeness Check

**Verify Enrichment Quality:**

- [ ] All called programs identified from call hierarchy are documented
- [ ] Parameter structures are fully explained with business meaning
- [ ] Data flows are traced through the call chain
- [ ] Error handling for each call is documented
- [ ] Integration patterns are identified and explained
- [ ] Business context sections reference what called programs do
- [ ] Performance implications of calls are noted
- [ ] Cross-references to called program extractions are included
- [ ] External/unresolved programs are clearly marked
- [ ] Second-level dependencies (programs called by called programs) are noted

## Output Requirements

### Enriched Program Extraction Document Structure

Enhance the existing program extraction document by adding or expanding these sections:

### 1. Executive Summary Enhancement

**Add to existing summary:**
```markdown
## Executive Summary

[Original program description]

**Program Dependencies:**
- Calls [N] programs: [LIST]
- Depends on [N] external utilities: [LIST]
- Integration patterns: [LIST]

**Key Integration Points:**
- [Brief description of each major integration]
```

### 2. New Section: Program Dependencies and Call Tree

Add a comprehensive section after the existing program structure documentation:

```markdown
## Program Dependencies and Call Tree

### Call Hierarchy Position
[Show where this program sits in the overall call hierarchy]
```
Level 0: [PROGRAM-ID] [Entry Point/Intermediate/Leaf]
  │
  ├─ Level 1: [CALLED-PROGRAM-1] [Brief description]
  │   └─ Level 2: [If called program has dependencies, show them]
  │
  └─ Level 1: [CALLED-PROGRAM-2] [Brief description]
```

### Called Programs Detail

#### 1. [CALLED-PROGRAM-1]
[Complete dependency profile as described in Analysis Approach section 3]

#### 2. [CALLED-PROGRAM-2]
[Complete dependency profile as described in Analysis Approach section 3]

### Integration Patterns
[Document patterns identified in Analysis Approach section 6]

### Data Flow Analysis
[Document cross-program data flows as described in Analysis Approach section 4]
```

### 3. Enhanced Procedure Division Documentation

**Enrich existing procedure descriptions with call context:**

For each procedure that makes calls:

```markdown
### [PROCEDURE-NAME]

[Original procedure description]

**Program Calls:**
1. **Call to [PROGRAM-NAME] (Line [X]):**
   - **Purpose:** [Why this call is made]
   - **Parameters:** [What data is passed]
   - **Expected outcome:** [What the caller expects to happen]
   - **Error handling:** [How errors are handled]
   - **Integration notes:** [Any special considerations]

[Rest of procedure documentation]
```

### 4. Enhanced Data Structure Documentation

**For data structures used in calls:**

```markdown
### [DATA-STRUCTURE-NAME]

[Original structure documentation]

**Used in Program Calls:**
- **Call to [PROGRAM-1]:** [How structure is used, which fields are inputs/outputs]
- **Call to [PROGRAM-2]:** [How structure is used, which fields are inputs/outputs]

**Field Usage in Calls:**
[Detailed field-by-field usage table as shown in Analysis Approach section 4]
```

### 5. New Section: Migration Considerations

Add a section that highlights migration implications based on dependencies:

```markdown
## Migration Considerations

### Dependency Migration Impact

**High Priority Dependencies:**
- **[PROGRAM-1]:** [Why it's high priority, migration approach]
- **[PROGRAM-2]:** [Why it's high priority, migration approach]

**External Dependencies:**
- **[EXTERNAL-1]:** [What functionality to replace it with]
- **[EXTERNAL-2]:** [What functionality to replace it with]

**Integration Patterns to Preserve:**
1. [Pattern 1 and how to implement in target platform]
2. [Pattern 2 and how to implement in target platform]

**Data Flow Considerations:**
- [Any data transformation or validation logic that must be preserved]
- [Any transaction boundary considerations]
- [Any error handling patterns that must be maintained]

**Testing Implications:**
- Programs that must be integration tested together: [LIST]
- External services that must be mocked/stubbed: [LIST]
- Critical data flows to verify: [LIST]
```

### 6. Enhanced Cross-References

**Add/expand cross-reference section:**

```markdown
## Cross-References

### Related Program Extractions
- [CALLED-PROGRAM-1_extract.md](path/to/file) - [Brief purpose]
- [CALLED-PROGRAM-2_extract.md](path/to/file) - [Brief purpose]

### Call Hierarchy Documentation
- [program-call-hierarchy.md](path/to/file) - Section [X.Y], Lines [N-M]

### Shared Data Structures
- Programs sharing [STRUCTURE-NAME]: [LIST]
- Programs sharing [STRUCTURE-NAME-2]: [LIST]
```

## Quality Requirements

Your program extraction enrichment must meet these quality standards:

### 1. Completeness
- [ ] Every program call identified in the call hierarchy is documented in detail
- [ ] All called programs have dependency profiles (even if marked as external/unresolved)
- [ ] Parameter flows are documented for every call with field-level detail
- [ ] Data structures used in calls have complete field-by-field usage documentation
- [ ] Integration patterns are identified and documented for all call types
- [ ] Second-level dependencies are noted for awareness

### 2. Accuracy
- [ ] All information added is traceable to source documents (extraction docs, call hierarchy)
- [ ] Parameter structures match what's documented in both calling and called programs
- [ ] Call conditions (always/conditional/on-error) are accurately represented
- [ ] Error handling descriptions match actual program logic
- [ ] Data flow descriptions reflect actual field-by-field usage

### 3. Clarity
- [ ] Technical descriptions include business context explaining why calls are made
- [ ] Integration patterns are explained in terms of business workflow
- [ ] Complex data flows are illustrated with examples
- [ ] Technical jargon is explained or avoided
- [ ] Cross-references are clear and help readers navigate between documents

### 4. Consistency
- [ ] Terminology is consistent with original extraction documents
- [ ] Program names match exactly (including case)
- [ ] Parameter names match source code documentation
- [ ] Section structure follows the existing extraction document format
- [ ] Integration patterns use consistent naming and description format

### 5. Traceability
- [ ] Every enhancement cites source documents with section/line references
- [ ] Dependency profiles reference both called program extraction and call hierarchy
- [ ] Data flow descriptions cite specific procedures and line numbers
- [ ] Integration patterns reference example call locations in code
- [ ] All cross-references include specific document names and sections

### 6. Actionability
- [ ] Migration considerations provide specific, actionable guidance
- [ ] Dependency priorities are explained with clear reasoning
- [ ] Testing implications identify specific test scenarios needed
- [ ] External dependencies list specific replacement approaches
- [ ] Integration patterns include migration strategy recommendations

### Verification Checklist

Before submitting the enriched extraction document:

- [ ] Compare enriched document with call hierarchy - all calls are documented
- [ ] Verify parameter information matches both calling and called programs
- [ ] Check that all called programs have dependency profiles
- [ ] Ensure data flow examples are accurate and complete
- [ ] Confirm integration patterns match actual call characteristics
- [ ] Validate that business context enhancements are accurate
- [ ] Verify all cross-references point to correct documents and sections
- [ ] Check that migration considerations address all key dependencies
- [ ] Ensure the document is still well-organized and readable
- [ ] Confirm no original information was lost during enrichment

## Success Criteria

This program extraction enrichment will be considered successful when:

### 1. Enhanced Understanding
- **Developers can understand program dependencies:** A reader can understand not just what programs are called, but why they're called, what they do, and how they integrate
- **Data flows are clear:** Parameter passing and data transformations across program boundaries are fully explained
- **Business context is complete:** Technical calls are explained in business terms

### 2. Migration Readiness
- **Dependencies are prioritized:** Clear guidance on which dependencies must be migrated first
- **External services identified:** All external/system programs are identified with replacement strategies
- **Integration patterns documented:** Patterns are documented with migration approaches
- **Testing strategy informed:** Integration test scenarios are identified from dependency analysis

### 3. Documentation Quality
- **Traceability maintained:** All enhancements cite source documents
- **Cross-references complete:** Readers can easily navigate to related documents
- **Consistency preserved:** Enhanced document maintains style and structure of original
- **Accuracy verified:** All technical details are accurate and match source code

### 4. Practical Utility
- **Migration planning:** Document can be used to plan migration sequence and estimate effort
- **Integration testing:** Document identifies which programs must be tested together
- **Architecture documentation:** Document shows how programs interact in the system
- **Knowledge transfer:** New team members can understand program relationships
- **Impact analysis:** Document enables assessing impact of changes to dependencies

### Expected Outcomes

After enrichment, the program extraction document should enable:

1. **Complete System Understanding:**
   - How this program fits into the larger application
   - What upstream programs call it and what downstream programs it calls
   - What data flows through the program and how it's transformed

2. **Dependency-Aware Migration:**
   - Which dependencies must be migrated first
   - Which external services need modern replacements
   - What integration patterns must be preserved
   - How to sequence migration work

3. **Comprehensive Testing:**
   - Which programs must be integration tested together
   - What external services to mock
   - What data flows to verify
   - What error scenarios to test

4. **Accurate Estimation:**
   - Complexity of dependencies affecting effort
   - External service replacements adding to scope
   - Integration complexity affecting timeline
   - Testing requirements affecting schedule

### Quality Indicators

- **Zero ambiguous dependencies:** Every called program is either fully documented or clearly marked as external with known characteristics
- **Complete parameter documentation:** Every call has field-level parameter documentation with business meaning
- **Traceable enhancements:** Every addition cites specific source documents and locations
- **Actionable migration guidance:** Dependency priorities and replacement strategies are specific and practical
- **Integration test scenarios:** Test cases are identified from actual program interactions
- **Preserved readability:** Enhanced document is still well-organized and easy to navigate

---

## Usage Instructions

### Prerequisites
- Individual program extraction documents (from program extraction prompt)
- Call hierarchy analysis (from call hierarchy construction prompt)
- Access to source code for verification (optional but recommended)

### Step-by-Step Process

1. **Select target program** from the extraction documents
2. **Open three documents:**
   - Target program extraction (e.g., `CBACT01_extract.md`)
   - Call hierarchy analysis (`program-call-hierarchy.md`)
   - Each called program's extraction document (if available)
3. **Follow the Analysis Approach** sections 1-7 systematically
4. **Create enriched version** following Output Requirements structure
5. **Verify quality** using Quality Requirements checklist
6. **Validate completeness** using Success Criteria

### Output Format

- **Filename:** `[PROGRAM-ID]_extract_enriched.md` (or update original `[PROGRAM-ID]_extract.md`)
- **Location:** Same folder as original extraction documents (`01.phase-1-output/`)
- **Format:** Markdown with consistent heading levels and structure

### Example

See `CBACT01_extract_enriched.md` for a complete example of an enriched extraction document that incorporates call tree knowledge from `COBDATFT` and `CEE3ABD` dependencies.

---

**Note:** This enrichment process is iterative. You may discover new insights about called programs that require going back to update their extraction documents, which then triggers re-enrichment of programs that call them. This is normal and leads to progressively better documentation.
