# COBOL Call Hierarchy Construction Prompt

## What Role to Play

You are an expert COBOL migration analyst specializing in synthesizing information from individual program analyses to construct comprehensive system-level views. You have deep expertise in understanding program dependencies, call hierarchies, and data flow patterns across COBOL applications. Your role is to analyze individual program extraction documents and construct a complete, accurate call hierarchy that shows how all programs in the application interact.

## Your Task

Analyze the provided collection of individual COBOL program extraction documents from 01.phase-1-output folder to construct a comprehensive call hierarchy map for the entire application. These extraction documents contain details about each program including the programs they call (CALL statements). Your task is to synthesize this information into a unified hierarchical structure that shows:

- All programs in the application and their relationships
- The complete call tree from entry points to leaf nodes
- Program dependencies and interaction patterns

**Important**: You are constructing the call hierarchy for the **[APPLICATION_NAME]** application. This is an independent analysis, not a refinement of previous outputs. Use consistent format and approach for all application analyses.

## Analysis Approach

Follow this systematic approach to construct the call hierarchy:

### 1. Program Inventory Collection
- Review all provided program extraction documents
- Extract the program identifier from each document
- Note the program type (main/entry, subprogram, utility)
- Create a master list of all programs in the application
- Identify any referenced programs that don't have extraction documents (external/missing)

### 2. Call Relationship Extraction
For each program extraction document:
- Locate the section that lists programs called (typically "Programs Called" or similar)
- Extract all CALL statements and called program names
- Note call conditions (always, conditional, loop-based)
- Record parameters passed in each call
- Identify dynamic vs static calls
- Note any error handling for each call

### 3. Entry Point Identification
- Identify programs that are not called by any other program (entry points)
- Classify entry points by type:
  - Screen/transaction entry points (CICS transactions)
  - Batch job entry points
  - External API entry points
- Verify entry points against transaction definitions if available

### 4. Hierarchy Construction
- Start from each entry point program
- Build the call tree recursively following CALL statements
- Assign depth levels (Level 0 = entry points, Level 1 = directly called, etc.)
- Track all paths through the application
- Identify leaf programs (programs that don't call others)
- Detect circular dependencies or recursive calls

### 5. Validation and Completeness Check
- Verify all CALL references resolve to known programs
- Check for dangling references (called but not defined)
- Validate bidirectional relationships
- Ensure no programs are missing from the hierarchy
- Cross-check with any provided transaction or JCL definitions

## Output Requirements

Provide your call hierarchy analysis in the following structured format with **3 sections only**:

### 1. Program Inventory Table

List all programs with their file paths and call statistics:

```
| Program ID | Program Type | File Path | Called By Count | Calls Count | Classification |
|------------|--------------|-----------|-----------------|-------------|----------------|
| PROG001    | Entry        | /path/to/PROG001.cbl | 0          | 2           | Entry Point Handler |
| MENU01     | Menu         | /path/to/MENU01.cbl  | 1          | 5           | Main Menu |
| TRAN01     | Transaction  | /path/to/TRAN01.cbl  | 1          | 3           | Transaction Screen A |
| UTIL01     | Utility      | /path/to/UTIL01.cbl  | 8          | 2           | Validation Utility |
| COPY01     | Copybook     | N/A - External       | 12         | 0           | Common Data Structure (Unresolved) |
```

**Notes:**
- Include file paths for all programs in the repository
- Mark external/unresolved programs with "N/A - External" or similar
- Include classification to help understand program purpose

### 2. Detailed Call Relationships Table

Document all call relationships with complete parameter information:

```
| Calling Program | Called Program | Call Type | Call Condition | Parameters Passed | Purpose | Source Reference |
|-----------------|----------------|-----------|----------------|-------------------|---------|------------------|
| PROG001         | MENU01         | CALL      | Always         | USER-DATA, AUTH-LEVEL | Route to main menu after entry processing | PROG001_extract.md, Line 145 |
| PROG001         | MENU02         | CALL      | Conditional    | USER-DATA         | Route to alternate menu path | PROG001_extract.md, Line 178 |
| MENU01          | TRAN01         | XCTL      | Menu Option 1  | RECORD-KEY        | Display transaction screen A | MENU01_extract.md, Line 234 |
| TRAN01          | UTIL01         | CALL      | Always         | INPUT-DATA        | Validate data before processing | TRAN01_extract.md, Line 567 |
```

**Include for each call:**
- Calling and called program names
- Call type (CALL, XCTL, LINK, etc.)
- Call condition (always, conditional, on error, etc.)
- All parameters passed with their structure/fields
- Business purpose of the call
- Source reference with document name and line number

### 3. Complete Call Hierarchy Tree

Present the hierarchy starting from each entry point, showing all levels:

```
APPLICATION: [APPLICATION_NAME]

═══════════════════════════════════════════════════════════════════

ENTRY POINT 1: PROG001 (Online Transaction - TXN1)
File: /path/to/PROG001.cbl
Level 0: PROG001 [Entry Point Handler]
  │
  ├─ Level 1: MENU01 [Main Menu - Path A]
  │   ├─ Level 2: TRAN01 [Transaction Screen A]
  │   │   ├─ Level 3: UTIL01 [Validation Utility]
  │   │   │   └─ Level 4: COPY01 [Common Data Copybook]
  │   │   └─ Level 3: PROC01 [Detail Processor]
  │   │       └─ Level 4: UTIL01 [Validation Utility]
  │   │
  │   ├─ Level 2: VIEW01 [View Screen]
  │   │   └─ Level 3: UTIL01 [Validation Utility]
  │   │
  │   └─ Level 2: LIST01 [List Screen]
  │       ├─ Level 3: COPY01 [Common Data Copybook]
  │       └─ Level 3: DTAIL01 [Detail Handler]
  │           └─ Level 4: UTIL01 [Validation Utility]
  │
  └─ Level 1: MENU02 [Main Menu - Path B]
      ├─ Level 2: VIEW01 [View Screen]
      │   └─ Level 3: UTIL01 [Validation Utility]
      │
      └─ Level 2: TRAN02 [Transaction Screen B]
          ├─ Level 3: UTIL01 [Validation Utility]
          └─ Level 3: COPY01 [Common Data Copybook]

═══════════════════════════════════════════════════════════════════

ENTRY POINT 2: BATCH01 [Batch Processing Job]
File: /path/to/BATCH01.cbl
Level 0: BATCH01 [Batch Job - Daily Processing]
  │
  ├─ Level 1: UTIL01 [Validation Utility]
  │   └─ Level 2: COPY01 [Common Data Copybook]
  │
  └─ Level 1: BPROC01 [Batch Processor]
      └─ Level 2: COPY01 [Common Data Copybook]

═══════════════════════════════════════════════════════════════════
```

**Include for each entry point:**
- Entry point program name with business description
- File path for the entry point program
- Complete call tree showing all levels
- Program descriptions at each node
- Visual hierarchy using tree structure characters
- Annotations for external/unresolved programs

**Hierarchy Summary:**
After the tree, include a summary with:
- Total programs analyzed
- Total call relationships
- Maximum call depth
- Number of entry points
- Number of external dependencies
- Circular dependencies (if any)

## Quality Requirements

Your call hierarchy construction must meet these quality standards:

1. **Completeness**: Every program from the extraction documents must be in the hierarchy
2. **Accuracy**: All call relationships must correctly reflect the extraction documents
3. **Consistency**: Program names and relationships must be consistent throughout
4. **Traceability**: Each call relationship should be traceable to source extraction documents
5. **Clarity**: The hierarchy must be easy to understand and navigate
6. **Validation**: All references must be validated against the program inventory

### Verification Checklist
- [ ] All programs from extraction documents are in the inventory
- [ ] All CALL statements from extractions are mapped in the hierarchy
- [ ] Entry points are correctly identified (not called by others)
- [ ] Leaf programs are correctly identified (don't call others)
- [ ] File paths are included for all repository programs
- [ ] Call parameters are completely documented
- [ ] Source references include document name and line numbers
- [ ] Unresolved references are clearly noted
- [ ] The hierarchy tree is visually clear and readable

## Success Criteria

This call hierarchy construction will be considered successful when:

1. **Complete Coverage**: All programs and all call relationships from the extraction documents are represented
2. **Accurate Structure**: The hierarchy correctly shows parent-child relationships and depth levels
3. **Clear Visualization**: A developer can understand the application structure from the hierarchy alone
4. **Validated**: All program references are resolved or marked as missing
5. **Actionable**: The output can be used for:
   - Migration planning and sequencing
   - Impact analysis for changes
   - Test planning and coverage
   - Architecture documentation
   - Team workload distribution

### Expected Outcomes
- **System Understanding**: Clear picture of how the application is structured
- **Entry Point Clarity**: Know where users and batch jobs enter the system
- **Dependency Mapping**: Understand which programs depend on which
- **Parameter Documentation**: Complete parameter information for all calls
- **File Path Reference**: Know where to find each program's source code

---

**Input Format**: Provide all individual program extraction documents in markdown format. Each document should contain at minimum:
- Program identifier
- List of programs called (CALL statements)
- Program purpose/description
- Entry point indicator (if applicable)

**Output File**: Create a file called `program-call-hierarchy.md` in the `01.phase-1-output` folder.

**Example Repository**: See `ashish-019-hash/aj-aws-card-demo` repository for a complete example of this analysis format.
