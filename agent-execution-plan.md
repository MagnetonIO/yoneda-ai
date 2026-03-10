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
- Link to $PAGES_URL
- Tone: accessible to science-curious audience, not academic

#### Stage 8 — HTML Conversion
| Field | Value |
|-------|-------|
| Input | `papers/latex/$SUBJECT.tex` + HTML template |
| Output | `docs/papers/$SUBJECT.html` |
| Task | Convert LaTeX paper to readable, mobile-friendly HTML5 page |
| Tool | `scripts/latex2html.py` (pandoc + post-processing) |

**Conversion tool** (`scripts/latex2html.py`):

Uses pandoc for LaTeX→HTML5 conversion, then cleans remaining LaTeX artifacts
(`\texttt`, `\textbf`, `\emph`, `\vspace`, lstlisting remnants, etc.) via regex
post-processing. Injects content into a styled HTML template with dark theme,
sidebar TOC, and KaTeX math rendering.

```bash
# Convert all papers using config file
python3 scripts/latex2html.py --config papers.yaml

# Or specify inline
python3 scripts/latex2html.py \
  --latex-dir papers/latex \
  --html-dir docs/papers \
  --template scripts/paper-template.html \
  --project-title "$PROJECT_TITLE" \
  --papers "$SUBJECT_A:$TITLE_A:Part I" "$SUBJECT_B:$TITLE_B:Part II"
```

**Config file format** (`papers.yaml`):
```yaml
project_title: "The AI Operating System"
latex_dir: "papers/latex"
html_dir: "docs/papers"
template: "scripts/paper-template.html"
papers:
  - name: "agent-scheduler"
    title: "Agent Scheduler"
    part: "Part I"
  - name: "tool-interface"
    title: "Tool Interface Layer"
    part: "Part II"
```

**Dependencies**: `pandoc` (brew install pandoc)

**Template** (`scripts/paper-template.html`):
- Dark theme with CSS variables (--bg, --surface, --accent, etc.)
- KaTeX CDN for math rendering ($...$ inline, $$...$$ display)
- Sticky sidebar TOC auto-generated from h2/h3 headings via JS
- Responsive: hamburger menu on mobile, sidebar collapse
- PDF download link, series navigation
- Placeholders: `{{TITLE}}`, `{{PART}}`, `{{PDF_LINK}}`, `{{BODY}}`

**Post-conversion QA**:
After running the conversion, launch a frontend analysis agent per file to check for:
- Remaining LaTeX artifacts
- Broken code blocks or tables
- Missing TikZ figures (pandoc can't convert these — add notes or use PDF reference)
- KaTeX math rendering issues
- Dark theme consistency

#### Stage 9 — Slack Notification (optional)
| Field | Value |
|-------|-------|
| Input | Completed paper metadata |
| Output | Slack message in configured channel |
| Task | Send completion notification with paper title, links, and summary |

**Config**:
```yaml
slack_channel: "$SLACK_CHANNEL"   # channel name or ID
notify_on: "completion"            # "completion" | "each_stage"
```

---

## Infrastructure Steps (run once per project, after all workers complete)

### Step I — Create Directory Structure
```bash
mkdir -p $PROJECT/{papers/{latex,pdf},src,reviews,posts,images,docs/papers,.github/workflows}
cd $PROJECT && git init
```

### Step II — GitHub Pages Site
| Field | Value |
|-------|-------|
| Input | Paper metadata + CSS template |
| Output | `docs/index.html` |
| Task | Create landing page with paper cards, architecture diagram, links |

**Requirements**:
- Dark theme (match YonedaAI style)
- Responsive (mobile + desktop)
- Paper cards with "Read" (HTML), "PDF", and "Code" links
- Architecture diagram
- Author info and project description

### Step III — GitHub Actions Workflow
| Field | Value |
|-------|-------|
| Output | `.github/workflows/pages.yml` |
| Task | GitHub Pages deployment from `docs/` directory |

### Step IV — Remote Repo
```bash
# Create repo
gh repo create $GITHUB_ORG/$PROJECT --public \
  --description "$DESCRIPTION" \
  --homepage "$PAGES_URL"
git remote add origin https://github.com/$GITHUB_ORG/$PROJECT.git

# Enable Actions for the repo
gh api repos/$GITHUB_ORG/$PROJECT/actions/permissions -X PUT -f enabled=true

# Enable GitHub Pages with Actions as the build source
gh api repos/$GITHUB_ORG/$PROJECT/pages -X POST -f build_type=workflow
# If Pages already exists, switch to Actions build:
# gh api repos/$GITHUB_ORG/$PROJECT/pages -X PUT -f build_type=workflow
```

### Step V — README
| Field | Value |
|-------|-------|
| Output | `README.md` |
| Task | Project overview with architecture, paper table, tech stack |

### Step VI — Commit (don't push unless instructed)
```bash
git add . && git commit -m "$COMMIT_MESSAGE"
# Only push if explicitly instructed:
# git push -u origin main
```

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
$PROJECT/
  papers/
    latex/$SUBJECT.tex      # final paper with GrokRxiv sidebar
    latex/$SUBJECT.pdf      # compiled PDF (final deliverable)
  reviews/
    $SUBJECT-review.md      # peer review feedback
  src/
    $SUBJECT/               # accompanying code
      Main.hs (or .ex, .py, .lean, etc.)
  scripts/
    latex2html.py           # reusable LaTeX→HTML converter (pandoc + post-processing)
    paper-template.html     # dark-theme HTML template with sidebar TOC + KaTeX
  docs/
    index.html              # GitHub Pages landing page
    og-image.png            # Open Graph social image (1200x630)
    papers/$SUBJECT.html    # readable HTML version of each paper
  images/
    $SUBJECT.png            # cover image (first PDF page, 300 DPI)
  posts/
    $SUBJECT.md             # social media post
  .github/workflows/
    pages.yml               # GitHub Pages deployment
  README.md
```

---

## Runnable Config (agent-planner format)

To execute this plan, provide a YAML config block. The planner reads it and executes all phases automatically.

```yaml
# ── Project Config ──
project: "$PROJECT"                    # directory name
project_path: "$PROJECT_PATH"         # absolute path to create project
github_org: "$GITHUB_ORG"             # GitHub organization
pages_url: "https://$GITHUB_ORG.github.io/$PROJECT/"

# ── Author Block ──
author: "$AUTHOR"
collaboration: "$COLLABORATION"
institution: "$INSTITUTION"
location: "$LOCATION"
email: "$EMAIL"
url: "$URL"

# ── Research Config ──
perspective: "$PERSPECTIVE"            # framing for all papers
language: "$LANG"                      # code language (Haskell, Elixir, Python, Lean4)
peer_reviewer: "gemini"                # gemini | claude | grok

# ── Subjects (one worker agent per subject) ──
workers:
  - subject: "$SUBJECT_A"
    description: "$DESCRIPTION_A"
  - subject: "$SUBJECT_B"
    description: "$DESCRIPTION_B"
  - subject: "$SUBJECT_N"
    description: "$DESCRIPTION_N"

# ── Optional: Synthesis Paper ──
synthesis:
  enabled: true                        # generate a final unifying paper
  subject: "$SYNTHESIS_SUBJECT"
  depends_on: all                      # waits for all workers to complete

# ── Optional: Infrastructure ──
infrastructure:
  github_pages: true
  github_actions: true
  remote_repo: true
  readme: true
  cover_images: true
  html_conversion: true
  social_posts: true
  slack_notification:
    enabled: false
    channel: "$SLACK_CHANNEL"

# ── Knowledge Base Sources ──
sources:
  - "sources/**/*.{tex,md,pdf}"        # glob patterns for input files
  - "$ADDITIONAL_SOURCE_PATHS"
```

### Example: AI Operating System

```yaml
project: "agent-os"
project_path: "/Users/mlong/Documents/Development/agentherowork/agent-os"
github_org: "AgentHeroWork"
pages_url: "https://agentherowork.github.io/agent-os/"

author: "Matthew Long"
collaboration: "The YonedaAI Collaboration"
institution: "YonedaAI Research Collective"
location: "Chicago, IL"
email: "matthew@yonedaai.com"
url: "https://yonedaai.com"

perspective: "AI Operating System through category theory"
language: "Elixir"
peer_reviewer: "gemini"

workers:
  - subject: "agent-scheduler"
    description: "Complex orchestration as process management — agents as objects"
  - subject: "tool-interface"
    description: "Morphisms, security, and capability abstraction — tools as morphisms"
  - subject: "memory-layer"
    description: "Typed filesystem for persistent agent cognition — memory as functor"
  - subject: "planner-engine"
    description: "Order book dynamics and natural transformation — planner as nat. trans."

synthesis:
  enabled: true
  subject: "synthesis"
  depends_on: all

infrastructure:
  github_pages: true
  github_actions: true
  remote_repo: true
  readme: true
  cover_images: true
  html_conversion: true
  social_posts: true
  slack_notification:
    enabled: true
    channel: "agenthero"

sources:
  - "../agent-hero/**/*.ts"
  - "../agent-testing-framework/**/*.ts"
  - "../../contextfs-ai/contextfs/src/**/*.py"
```

---

## Execution Phases (what `run agent-planner` does)

```
Phase 1: Setup
  ├─ Create directory structure
  └─ Initialize git repo

Phase 2: Knowledge Base (Agent 1)
  ├─ Glob all source files
  ├─ Extract architecture, patterns, key abstractions
  └─ Write .knowledge-base.md

Phase 3: Workers (Agents 2..N, parallel)
  For each worker:
    ├─ Stage 1: Draft paper + code
    ├─ Stage 2: Peer review (Gemini/Claude/Grok)
    ├─ Stage 3: Revise based on feedback
    ├─ Stage 4: Final formatting + GrokRxiv sidebar
    ├─ Stage 5: Compile PDF (pdflatex, fix errors)
    ├─ Stage 6: Generate cover image (pdftoppm)
    ├─ Stage 7: Social post
    └─ Stage 8: HTML conversion

Phase 4: Synthesis (if enabled, depends on Phase 3)
  ├─ Draft synthesis paper referencing all worker papers
  ├─ Peer review + revise
  ├─ Compile PDF + image + HTML
  └─ Social post

Phase 5: Infrastructure (parallel)
  ├─ GitHub Pages site (docs/index.html)
  ├─ GitHub Actions workflow
  ├─ Remote repo creation
  ├─ README.md
  └─ Slack notification (if enabled)

Phase 6: Finalize
  ├─ git add + commit
  └─ (push only if explicitly instructed)
```
