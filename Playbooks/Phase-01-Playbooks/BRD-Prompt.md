Overview
This template guides systematic extraction of Business Requirements from any legacy system codebase through specialized roles, ensuring comprehensive BRD coverage regardless of technology stack or business domain.

Role 1: Business Analyst - System Purpose & Objectives
Mission
Understand the high-level business purpose, objectives, and value the system delivers.

Extract
System Purpose: What business problem does this solve? Who are the stakeholders?

Business Objectives: What business goals does it achieve? What processes does it automate or support?

Scope: What's included and excluded from the system's responsibilities?

Success Metrics: How does the business measure success (throughput, accuracy, satisfaction, cost savings)?

Business Context: Which department owns it? How critical is it? What's the impact if unavailable?

Business Value: What ROI or competitive advantage does it provide?

Where to Look
Main entry points, top-level modules, user interfaces, reporting components, data models, configuration files

Deliver
Executive summary, business objectives list, scope statement, stakeholder identification, business context

Role 2: Functional Requirements Analyst - Business Capabilities
Mission
Document all business functions and capabilities the system must perform.

Extract
Authentication & Access: How do users gain access? How are user types differentiated?

Core Business Functions: For each capability, document purpose, inputs, processing, outputs, triggers, and role access

Data Management: How are entities created, read, updated, deleted? What validations occur?

Business Processes: What end-to-end processes are supported? What workflows and state transitions exist?

Reporting & Analytics: What reports exist? What data do they present? Who consumes them?

Integration: What external systems interact? What data is exchanged? How are failures handled?

Administration: How is configuration, reference data, and user access managed?

Where to Look
Module structure, user interface screens, menu hierarchies, API endpoints, database operations, workflow definitions

Deliver
Functional requirements organized by category, clear descriptions with inputs/outputs, role mappings, priority rankings

Role 3: Business Rules Analyst - Logic & Constraints
Mission
Document all business rules, validations, calculations, and constraints.

Extract
Data Validation: Field-level and cross-field validations, constraints, error messages

Calculations: Mathematical formulas, business algorithms, rates/factors, rounding rules

Business Logic: Conditional logic, decision trees, threshold triggers, prioritization

Authorization: Access controls, role permissions, approval hierarchies, segregation of duties

State Transitions: Business states, transition conditions, state-based restrictions

Data Integrity: Referential integrity, cascade behaviors, mandatory relationships, duplicate prevention

Temporal Rules: Date/time-based rules, expiration periods, scheduling constraints

Business Constraints: Capacity limits, rate limiting, geographic rules, eligibility criteria

Where to Look
Conditional logic, validation routines, calculation functions, state management, configuration parameters, database constraints

Deliver
Business rules catalog by category, clear rule statements with conditions and actions, identification of configurable vs hard-coded rules

Role 4: Data Analyst - Business Entities & Information Model
Mission
Document all business entities, attributes, relationships, and the information model.

Extract
Business Entities: Identify core, supporting, transactional, relationship, and temporal entities

Entity Attributes: For each entity, document name, business definition, all attributes with business meaning, data types, mandatory/optional, unique identifiers, valid values, calculated vs stored

Relationships: One-to-one, one-to-many, many-to-many relationships with business meaning, hierarchies, lookups, temporal relationships

Business Keys: Natural keys, surrogate keys, alternate identifiers, uniqueness schemes

Data Lifecycle: How entities are created, updated, deleted/inactivated, historical retention, versioning, archival

Data Quality: Completeness, accuracy, consistency requirements, quality issue detection

Data Volumes: Record counts, growth patterns, distribution characteristics, retention patterns

Where to Look
Database schemas, table structures, object-relational mappings, data access layers, foreign keys, indexes, sample data

Deliver
Entity-relationship model with business definitions, data dictionary, relationship diagram, business keys, lifecycle descriptions, volume estimates

Role 5: Process Analyst - Workflows & Business Processes
Mission
Document end-to-end business processes and workflows the system supports.

Extract
Process Map: Major business processes, sequences, dependencies, triggers, inputs/outputs, success criteria

Detailed Workflows: Step-by-step activities, actors, decisions, alternative paths, handoffs, wait states, notifications

User Journeys: Tasks users accomplish, interaction sequences, information needs, decision points

Automated Workflows: Background processes, scheduled jobs, event-driven processes, timing, monitoring, failure handling

Exception Handling: Error conditions, detection, recovery mechanisms, manual intervention, rollback logic

Approval Workflows: What requires approval, approval hierarchies, escalation, rejection handling, audit trails

Integration Points: Cross-system hand-offs, synchronization, external system unavailability handling

Where to Look
Execution flow traces, state machines, workflow engines, user interface navigation, event handlers, scheduled jobs, error handling

Deliver
Process flow diagrams, step-by-step descriptions, user journey maps, exception procedures, bottleneck identification

Role 6: User Experience Analyst - Users & Interactions
Mission
Understand user roles, permissions, and interaction patterns.

Extract
User Roles: Distinct roles/personas, business definitions, job functions, goals, proportions

Role Capabilities: What each role can access, view, modify, delete, approve

Interface Inventory: For each screen/page, document purpose, role access, displayed information, available actions, required inputs, navigation options, help/guidance, error messages

Navigation: Movement between parts, main structure, shortcuts, search/find, return paths, breadcrumbs

Interaction Patterns: Common task flows, data entry patterns, selection mechanisms, bulk operations, keyboard shortcuts

User Assistance: Inline help, error messages, confirmations, field prompts, documentation, onboarding

Notifications: Event notifications, alerts, process completion, email/external notifications

Where to Look
User interface code, navigation components, access control logic, form definitions, error message catalogs, notification components

Deliver
Role definitions with permissions matrix, screen inventory, navigation map, user task flows, assistance features

Role 7: Integration Analyst - Data Flows & System Interactions
Mission
Understand data flows and external interactions.

Extract
System Architecture: Major components, communication methods, protocols, message formats, middleware

Data Inputs: External sources, data received, formats, frequency, validation, error handling, transformations

Data Outputs: External consumers, data sent, formats, frequency/triggers, transformations, delivery confirmation, failure handling

Internal Flows: Data movement between modules, intermediate processing, transformations, caching, queuing

Database Operations: Databases used, access patterns, partitioning, indexes, locking, backup/recovery, replication

APIs & Services: Exposed APIs, consumed APIs, authentication, rate limiting, versioning, documentation

Batch Processing: Batch jobs, business purpose, data processed, schedules, dependencies, monitoring, failure handling

Event-Driven: Business events, event systems, schemas, ordering/delivery guarantees, failure handling

Where to Look
Database schemas, API definitions, configuration files, import/export utilities, message queue configs, scheduled job definitions

Deliver
System context diagram, data flow diagrams, interface catalog, batch job schedule, API documentation, data mappings

Role 8: Compliance & Security Analyst - Controls & Audit
Mission
Understand security controls, audit trails, and compliance requirements.

Extract
Authentication: Identity verification methods, credential storage/protection, password policies, multi-factor authentication

Authorization: Access decision logic, role/attribute-based controls, least privilege, segregation of duties

Data Protection: Sensitive data identification (PII), encryption at rest/in transit, masking/redaction, access controls, retention/disposal, consent mechanisms

Audit Trail: Activity logging, event recording, log information (who/what/when/where/why), retention, access, tamper-protection

Transaction Integrity: Accuracy/completeness assurance, duplicate prevention, reversal controls, approval workflows, reconciliation, checksums/signatures

Error Handling: Error detection/logging, information leakage prevention, recovery, rollback, consistency maintenance, disaster recovery

Compliance: Applicable regulations (financial, healthcare, data protection), security standards, reporting requirements, audit requirements, certifications

Security Features: Input validation, vulnerability protection, session management, rate limiting, security headers, testing

Risk Management: Fraud detection/prevention, anomaly detection, incident response, business continuity, single points of failure

Where to Look
Authentication/authorization code, encryption implementations, logging, input validation, security configs, access control matrices, error handling

Deliver
Security architecture, compliance requirements mapping, audit specifications, data protection inventory, risk assessment, security recommendations

Role 9: Requirements Synthesizer - Integration & Documentation
Mission
Integrate all extracted information into a coherent Business Requirements Document.

Create
Executive Summary: Business purpose, scope, objectives, stakeholders, critical capabilities, business value

Business Context: Organizational context, drivers, strategic alignment, stakeholder landscape, environment

Functional Requirements: Organized by category, clear testable statements, prioritized, with business rationale

Business Rules: Structured catalog by category, clear rule statements, configurable vs hard-coded indication

Data Requirements: Complete information model, entity definitions, attributes, relationships, data quality/integrity

Process Requirements: End-to-end flows with diagrams, step-by-step processes, decision points, exceptions, performance requirements

User Requirements: Role definitions, responsibilities, capabilities, permissions, interaction patterns, usability/accessibility

Integration Requirements: Integration points, data exchange formats/protocols, interface contracts, service levels, batch processing, error handling

Non-Functional Requirements: Security, performance, scalability, availability, reliability, compliance, audit, backup/recovery

Assumptions & Constraints: Key assumptions, technical constraints, resource constraints, dependencies

Business Glossary: Business terms, acronyms, consistent terminology, domain-specific language

Traceability Matrix: Requirements to code components mapping for verification and impact analysis

Open Questions & Risks: Ambiguities needing clarification, discovered risks, areas for investigation

Documentation Standards
Clear, unambiguous language for business audiences

Number requirements for traceability

Use consistent terminology throughout

Include visual diagrams where helpful

Maintain appropriate detail level

Cross-reference related content

General Guidelines for All Roles
Analysis Mindset
Think Business First: Ask "What business need does this serve?" not "What does the code do?"

Be Systematic: Work methodically through the codebase

Question Assumptions: Verify interpretations, don't assume

Seek Patterns: Look for recurring patterns revealing business rules

Document Ambiguities: Note unclear or inconsistent code for stakeholder validation

Validate Cross-References: Ensure different parts tell a consistent story

What to Avoid
Don't just describe code; translate to business language

Don't assume technical knowledge in readers

Don't include code snippets; keep technology-agnostic

Don't use undefined jargon

Don't overlook implicit business rules in code

Don't ignore edge cases; they reveal requirements

Analysis Techniques
Static code analysis, dynamic analysis, data examination

Reverse engineering from outputs

Pattern recognition for business rules

Dependency and impact analysis

Quality Checks
Can non-technical stakeholders understand this?

Is each requirement specific, measurable, testable?

Are all business scenarios covered?

Are business rules consistently applied?

Is the rationale clear?

Can this serve as a specification for testing or replacement?

Approach by Codebase Type
Object-Oriented (Java, C#, Python)
Focus on class hierarchies (entities), service layers (logic), controllers (interactions), data access objects (data model)

Procedural (COBOL, C)
Focus on program modules and call hierarchies, copybooks/includes (data), screen maps (UI), file operations (data flows)

Web Applications
Study page/component structure (UI), routing patterns (organization), API endpoints (integration), state management (logic)

Database-Centric
Focus on stored procedures (logic), triggers (rules), views (reporting), schema (entities)

Event-Driven/Microservices
Map event types to business events, trace flows through boundaries, understand service responsibilities, document eventual consistency

Deliverables Checklist
Each role produces:

✓ Structured section covering their area

✓ Business-oriented descriptions (not technical)

✓ Business rationale for key findings

✓ Questions needing stakeholder clarification

✓ Traceability to source code locations

✓ Visual diagrams where valuable

✓ Contributions to business glossary

Final BRD should:

✓ Completely describe what the business needs

✓ Be understandable without technical knowledge

✓ Cover all functions, rules, entities, processes

✓ Provide detail to validate current behavior

✓ Support business decision-making

✓ Serve as specification for testing/enhancement/replacement

✓ Include clear source code traceability

✓ Document compliance requirements

Success Criteria
A successful BRD extraction enables stakeholders to:

Understand: See what business problems the system solves

Validate: Verify system operates per business needs

Decide: Determine whether to maintain, enhance, replace, or retire

Plan: Understand impact of proposed changes

Test: Create test cases verifying business requirements

Train: Educate new users on capabilities and processes

Comply: Demonstrate regulatory requirements are met

Transfer: Preserve institutional knowledge about the system

this is the codebase

https://github.com/ashish-019-hash/aws-card-demo-new-approach/tree/main

Create the BRD document in the .md file
