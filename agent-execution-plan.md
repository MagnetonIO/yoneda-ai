# Multi-Agent Execution Plan

Generic schema for parallel research + code generation with peer review.
Scales to N subjects — add agents by duplicating the Worker block.

---

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│  AGENT 1 — Knowledge Base / Memory (always runs first)  │
│  Reads source files → builds shared knowledge base      │
└──────────────────────┬──────────────────────────────────┘
                       │ .knowledge-base.md
        ┌──────────────┼──────────────────┐
        ▼              ▼                  ▼
┌──────────────┐┌──────────────┐  ┌──────────────┐
│  AGENT 2     ││  AGENT 3     │  │  AGENT N     │
│  Subject A   ││  Subject B   │  │  Subject X   │
│              ││              │  │              │
│  Draft ──────││──────────────│──│──────────────│
│  Peer Review ││  Peer Review │  │  Peer Review │
│  Revise ─────││──────────────│──│──────────────│
│  Final + PDF ││  Final + PDF │  │  Final + PDF │
└──────────────┘└──────────────┘  └──────────────┘
```

---

## Agent 1 — Knowledge Base / Memory

**Role**: Foundation agent. Runs first, all other agents depend on it.

| Field | Value |
|-------|-------|
| Input | `sources/**/*.{tex,md,pdf}` |
| Output | `.knowledge-base.md` |
| Task | Read all source files, extract structured summaries, build cross-reference index |

**Steps**:
1. Glob all source files in `sources/`
2. For each file: extract title, authors, abstract, key concepts, frameworks, theorems
3. Build cross-reference section (how sources relate)
4. Write `.knowledge-base.md`
5. Signal completion → unblocks all worker agents

---

## Agent N+ — Worker (one per subject)

**Role**: Draft → Peer Review → Revise → Final PDF. Repeatable for any subject.

### Config per worker

```yaml
agent_id: agent-$N            # agent-2, agent-3, ...
subject: "$SUBJECT"           # any research topic
perspective: "$PERSPECTIVE"   # framing for all papers
language: "$LANG"             # e.g. "Haskell", "Python", "Lean4"
blocked_by: agent-1           # always depends on knowledge base
peer_reviewer: "gemini"       # "gemini" | "claude" | "grok"
```

### Prompt framing

Every worker agent receives its subject prefixed with the perspective:

> "$PERSPECTIVE, analyze and formalize **$SUBJECT**."

### Pipeline (5 stages)

#### Stage 1 — Draft
| Field | Value |
|-------|-------|
| Input | `.knowledge-base.md` + subject prompt |
| Output | `papers/$SUBJECT.tex` + `src/$SUBJECT/` (code) |
| Prompt | "$PERSPECTIVE, analyze and formalize **$SUBJECT**" |
| Task | Write arxiv-style paper (>=20 pages) + accompanying code |

#### Stage 2 — Peer Review
| Field | Value |
|-------|-------|
| Input | `papers/$SUBJECT.tex` + `src/$SUBJECT/` |
| Output | `reviews/$SUBJECT-review.md` |
| Task | External reviewer evaluates paper + code for correctness, clarity, gaps |
| Reviewer | Configurable: Gemini CLI, Claude agent, or Grok |

Gemini CLI invocation:
```bash
gemini -p "Peer review this paper and code. Evaluate: mathematical correctness, \
clarity, completeness, code quality. Output structured feedback with specific \
line-level suggestions." < papers/$SUBJECT.tex
```

#### Stage 3 — Revise
| Field | Value |
|-------|-------|
| Input | `papers/$SUBJECT.tex` + `reviews/$SUBJECT-review.md` |
| Output | `papers/$SUBJECT.tex` (revised in-place) + `src/$SUBJECT/` (revised) |
| Task | Address all review feedback, revise paper and code |

#### Stage 4 — Final
| Field | Value |
|-------|-------|
| Input | Revised `papers/$SUBJECT.tex` |
| Output | Final `papers/$SUBJECT.tex` with GrokRxiv sidebar |
| Task | Add GrokRxiv DOI sidebar (see template), final formatting pass |

#### Stage 5 — Compile PDF
| Field | Value |
|-------|-------|
| Input | Final `papers/$SUBJECT.tex` |
| Output | `papers/$SUBJECT.pdf` |
| Task | Compile to PDF, fix errors until it compiles, clean artifacts |

**Compile loop**: Keep compiling and fixing LaTeX errors until PDF builds successfully.

```bash
# Compile (run twice for references/TOC)
cd papers && pdflatex -interaction=nonstopmode $SUBJECT.tex && pdflatex -interaction=nonstopmode $SUBJECT.tex

# If errors: read the .log, fix the .tex, recompile. Repeat until clean.

# Clean artifacts — keep ONLY .tex and .pdf
rm -f *.aux *.log *.toc *.out *.bbl *.blg *.nav *.snm *.vrb *.fls *.fdb_latexmk *.synctex.gz
```

**Rules**:
- Do NOT leave a broken PDF. Fix all errors before marking complete.
- Do NOT leave build artifacts. Only `.tex` and `.pdf` remain in `papers/`.
- If a package is missing, remove or substitute it and recompile.

#### Stage 6 — Generate Cover Image
| Field | Value |
|-------|-------|
| Input | `papers/pdf/$SUBJECT.pdf` |
| Output | `images/$SUBJECT.png` |
| Task | Extract first page of PDF as PNG image |

```bash
mkdir -p images
# Use pdftoppm (poppler) — ImageMagick produces black pages on macOS
pdftoppm -png -f 1 -l 1 -r 300 "papers/pdf/$SUBJECT.pdf" "images/$SUBJECT"
mv "images/$SUBJECT-1.png" "images/$SUBJECT.png"
```

**Rules**:
- 300 DPI minimum for readable text
- PNG format only
- One image per paper (first page only)
- Images directory must exist before writing

#### Stage 7 — Facebook Post
| Field | Value |
|-------|-------|
| Input | `papers/latex/$SUBJECT.tex` |
| Output | `posts/$SUBJECT.md` |
| Task | Write an engaging Facebook post about the paper |

**Format rules** (Facebook does NOT render markdown):
- PLAIN TEXT ONLY — no `**bold**`, no `*italic*`, no `# headers`, no `- ` bullets
- Use emojis for visual structure and emphasis
- Use line breaks (blank lines) to separate sections
- Use ALL CAPS sparingly for emphasis
- Use em dashes (—) and smart quotes for polish
- Use Unicode section dividers (e.g. _____)
- Structure: hook → problem → insight → why it matters → link → hashtags
- Hashtags on their own line at the end, no `**` wrapping
- Link to https://magnetonio.github.io/yoneda-ai/
- Tone: accessible to science-curious audience, not academic

---

## Adding More Agents

Duplicate the worker block with your perspective and subjects:

```yaml
perspective: "$PERSPECTIVE"

workers:
  - agent_id: agent-2
    subject: "$SUBJECT_A"
    language: "Haskell"
    peer_reviewer: "gemini"

  - agent_id: agent-3
    subject: "$SUBJECT_B"
    language: "Haskell"
    peer_reviewer: "gemini"

  - agent_id: agent-N
    subject: "$SUBJECT_X"
    language: "Haskell"
    peer_reviewer: "gemini"
```

All workers run in parallel after Agent 1 completes.

---

## Templates

### Author Block
```
$AUTHOR
$COLLABORATION
$INSTITUTION
$LOCATION
$EMAIL · $URL
```

### Paper Format
- arxiv-style LaTeX, >=20 pages
- Include: abstract, introduction, mathematical framework, results, discussion, references

### GrokRxiv Sidebar
- Applied in Stage 4 (Final) after all revisions
- See `grokrxiv-sidebar-template.md` for full LaTeX implementation

---

## Execution

```bash
# 1. Create team
# 2. Create Task #1 (knowledge base)
# 3. Create Tasks #2..N+1 (one per subject, all blocked by #1)
# 4. Spawn Agent 1 → completes → unblocks all workers
# 5. Spawn Agents 2..N+1 in parallel
# 6. Each worker: draft → peer review → revise → final → compile PDF
# 7. Collect outputs in papers/ and src/
```

### Output Structure
```
papers/
  $SUBJECT.tex          # final paper with GrokRxiv sidebar
  $SUBJECT.pdf          # compiled PDF (final deliverable)
reviews/
  $SUBJECT-review.md    # peer review feedback
src/
  $SUBJECT/             # accompanying code
    Main.hs (or .py, .lean, etc.)
```
