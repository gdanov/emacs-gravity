---
name: story
description: "Start collaborative story authoring for a new feature"
skills:
  - story-format
---

# /story — Author a Feature Story

You are starting a collaborative story authoring session. Your goal is to work WITH the user to produce a detailed, well-structured feature specification.

## Process

1. **Understand intent**: Ask the user what they want to build. If they provided a description with the command, use that as the starting point.

2. **Explore the codebase**: Before asking detailed questions, read relevant code to understand the current state. Use Explore agents to search broadly if the scope is unclear.

3. **Ask probing questions**: Don't accept the first description at face value. Ask about:
   - Who is this for? What problem does it solve?
   - What does "done" look like? How will we know it works?
   - What should explicitly NOT be included?
   - Are there existing patterns in the codebase we should follow?

4. **Draft the story**: Propose acceptance criteria, edge cases, and scope boundaries. Present them to the user for refinement.

5. **Iterate**: Refine based on user feedback until both parties agree the story is complete.

6. **Persist**: Create a beads issue and write the markdown spec file following the story-format skill template.

## Output

When the story is finalized:
- Create a beads issue: `bd create --title="<title>" --type=feature --priority=<N> --description="<summary + user stories>" --design="<technical notes>" --notes="<edge cases + out of scope>"`
- Write the full spec to `stories/<slug>.md` (create `stories/` directory if needed)
- Report the beads issue ID and file path to the user

## Important

- Be a genuine thought partner, not a yes-machine. Challenge weak acceptance criteria.
- Ground everything in the actual codebase — don't propose features that ignore existing architecture.
- Keep stories focused. If the scope is growing, suggest splitting into multiple stories.
