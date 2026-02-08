---
name: story-format
description: "Structured story template for feature specifications. Loaded by the story-partner agent and /story command to ensure consistent output format."
---

# Story Format Specification

When authoring a feature story, produce output in this exact structure. This format is used by the architect and verifier agents downstream.

## Markdown File Format (`stories/<slug>.md`)

```markdown
# <Feature Title>

## Summary
1-2 sentences describing the feature and its purpose.

## User Stories
- As a <role>, I want <capability> so that <benefit>.
- ...

## Acceptance Criteria
1. <Testable criterion with clear pass/fail condition>
2. <Another criterion>
3. ...

## Edge Cases
- <What happens when X?>
- <What if Y is empty/null/invalid?>
- ...

## Out of Scope
- <Explicitly excluded functionality>
- <Things that look related but are NOT part of this story>

## Technical Notes
- <Constraints, dependencies, performance requirements>
- <Existing patterns or code to be aware of>
```

## Beads Field Mapping

When creating the beads issue:

- `--title`: Feature title (concise, imperative: "Add dark mode support")
- `--type`: `feature`
- `--priority`: As appropriate (0-4)
- `--description`: Summary + User Stories sections combined
- `--design`: Technical Notes section (architect will append to this later)
- `--notes`: Edge Cases + Out of Scope sections combined

## Quality Checklist

Before finalizing a story, verify:
- [ ] Every acceptance criterion is testable (has a clear pass/fail)
- [ ] Edge cases cover error states, empty inputs, and boundary conditions
- [ ] Out of scope is explicit (prevents scope creep during implementation)
- [ ] Technical notes reference existing code patterns where relevant
- [ ] No acceptance criterion is ambiguous or subjective ("should be fast" -> "responds within 200ms")
