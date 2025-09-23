Phase-02 Frontend Playbook



You are a Senior Frontend Developer tasked with delivering a complete, production-ready React application. You must interpret and implement business workflows described in markdown documents and integrate the UI with a Spring Boot backend.



-----------------------------------------------------------------------------------------------------------------------------------------------------------

**Your Task**

**-----------------------------------------------------------------------------------------------------------------------------------------------------------**



Build a fully functional React frontend application for a mainframe modernization project. The UI must strictly adhere to business requirements, validation logic, and screen transitions described in the documentation. The application must be integrated with backend REST APIs and support error-handling mechanisms using backend feedback.



Source of truth for requirements is the following repository and files:



Repository: devin/1753704997-cobol-to-spring-entities



Screen Flow Document: screen-flow.md



User Stories: user\_stories.md



Validation Rules: validation-rules+1.md





**Analyze Input Files**



From user\_stories.md:



Identify and list all functional modules.



Extract goals and expected outcomes for each feature.



Understand which business entities and workflows are in scope.



From screen-flow-final-new-corrected+1.md:



Extract and document all screens and their transitions.



Identify dependencies between screens and user actions.



Document flow logic, including conditional navigation or multi-step processes.



From validation-rules+1.md:



Capture backend-driven validation rules for all input fields.



Document which fields are mandatory, conditional, or dependent on backend validation.



**Define the Screen Architecture**



Create a list of all unique screens required in the frontend.



For each screen, define the following:



Page layout structure



Required input fields and form elements



Navigation components (e.g., buttons, links, tabs)



Display-only sections for retrieved data



Identify reusable UI components (e.g., input field, dropdown, table row, error message container).



Create a screen-to-API interaction matrix that maps frontend screens to backend endpoints.



**Implement Screens and Forms**

Build each screen as a functional component using React Hooks.



Use reusable form components where applicable.



Apply layout and styling using a consistent CSS methodology (CSS Modules, Tailwind, or styled-components).



Implement input fields with appropriate attributes (type, format, default values).



Add placeholders or initial values as needed based on screen flow or user stories.



Support dynamic field rendering based on screen flow rules.



Ensure required fields from validation-rules+1.md are captured in the UI, but actual validation is deferred to backend.



**Integrate with Backend APIs**

Identify the specific backend endpoints needed for each user action or screen transition.



Implement a centralized API service module to:



Handle HTTP requests and responses (e.g., using Axios or Fetch)



Manage authentication headers, error handling, and timeouts



Map each screen's inputs to the expected backend API payload.



Display backend data in the UI with appropriate loading, error, and empty states.



**Implement Backend-Driven Validation and Error Handling**



Use API responses to render all validation and error messages.



Avoid hardcoding or duplicating backend validation logic in the frontend.



Map backend error keys or codes to user-friendly messages in the UI.



Block screen transitions or submissions based on backend validation failures.



Ensure all input-related errors are highlighted inline for clarity.



**Output Requirements**



Deliver the following:



A complete, tested React application with:



Screens defined in screen-flow-final-new-corrected+1.md



Logic and workflows derived from user\_stories.md



Field requirements and validations aligned with validation-rules+1.md



A centralized API integration module



Modular, readable, and maintainable code structure



Reusable component architecture



Deployment-ready codebase with:



Build scripts (e.g., npm run build)



Environment variable configuration (.env.example)



Setup or usage instructions in README.md (if required)



**Quality Requirements**



Clean component structure using React functional components and hooks



Consistent styling and layout across screens



Modularized folder structure for components, services, and pages



Mobile responsive design



Proper use of loading states, error boundaries, and fallback UI



Fully working error-handling mechanisms without exposing stack traces or backend internals



**Success Criteria**



All required screens are implemented based on screen-flow-final-new-corrected+1.md



Functional behavior matches user goals in user\_stories.md



All backend APIs are correctly integrated and tested



Validations are handled correctly based on validation-rules+1.md



The application is demo-ready, deployable, and ready for handoff to QA





