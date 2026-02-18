# YonedaAI Research Collective

**Category-theoretic foundations for open problems in quantum mechanics, gravity, and observer theory.**

This repository contains a systematic research program applying the **Yoneda Constraint** — a category-theoretic framework based on the Yoneda lemma — to six foundational problems in physics. Each paper includes formal proofs, accompanying Haskell implementations, and Gemini-reviewed peer feedback.

---

## Papers

| Paper | Pages | Category |
|-------|-------|----------|
| [Black Hole Information Paradox](papers/pdf/black-hole-information-paradox.pdf) | 26 | `hep-th` |
| [Hidden Variable Debates](papers/pdf/hidden-variable-debates.pdf) | 27 | `quant-ph` |
| [Measurement Problem](papers/pdf/measurement-problem.pdf) | 28 | `quant-ph` |
| [Wigner's Friend](papers/pdf/wigners-friend.pdf) | 32 | `quant-ph` |
| [Horizon Problems](papers/pdf/horizon-problems.pdf) | 33 | `hep-th` |
| [Limits of Quantum Gravity Observation](papers/pdf/limits-quantum-gravity-observation.pdf) | 33 | `gr-qc` |

**Total: 179 pages across 6 papers**

---

## Core Framework

The Yoneda Constraint reformulates fundamental physics problems as **category-theoretic obstructions**:

- **Measurement categories** encode observer-system interactions as morphisms
- **Representable presheaves** characterize accessible knowledge via the Yoneda embedding
- **Kan extension deficits** quantify information loss at epistemic horizons
- **Cohomological obstructions** unify no-go theorems (Bell, Kochen-Specker, PBR) as presheaf failures

Each paper builds on a shared mathematical vocabulary from category theory (AQFT, topos theory, enriched categories) and connects to the broader YonedaAI research program.

---

## Repository Structure

```
yoneda-ai/
  sources/              # Original .tex research papers (knowledge base inputs)
  papers/
    latex/              # New papers (.tex source)
    pdf/                # Compiled papers (.pdf)
  reviews/              # Gemini CLI peer review reports
  src/
    black-hole-information-paradox/    # Haskell: representable functors, Page curve
    hidden-variable-debates/           # Haskell: Bell/KS verification, Kan extensions
    measurement-problem/               # Haskell: Born rule, decoherence, obstruction
    wigners-friend/                    # Haskell: presheaf irreconcilability, CHSH
    horizon-problems/                  # Haskell: causal categories, horizon deficits
    limits-quantum-gravity-observation/# Haskell: QG obstructions, entropy bounds
```

---

## Key Results

- **Presheaf Irreconcilability Theorem** — Friend's and Wigner's presheaves are provably non-isomorphic (Wigner's Friend)
- **Measurement Obstruction Theorem** — The measurement problem is a Yoneda obstruction with cohomological invariant (Measurement Problem)
- **Three Independent Yoneda Obstructions** to Planck-scale observation: gravitational horizon, diffeomorphism gauge, holographic saturation (QG Limits)
- **Unified No-Go Framework** — Bell, Kochen-Specker, PBR, von Neumann, and Free Will theorems as presheaf obstructions (Hidden Variables)
- **Horizon Classification** — All horizon types (cosmological, event, Rindler, de Sitter, holographic) as accessible subcategory inclusions with Kan extension deficits (Horizon Problems)
- **Page Curve as Kan Extension** — Island formula emerges as the left Kan extension along the inclusion of the radiation subsystem (Black Hole Information)

---

## Building

### Papers
```bash
cd papers/latex
pdflatex -interaction=nonstopmode <paper>.tex
pdflatex -interaction=nonstopmode <paper>.tex  # second pass for references
```

### Haskell Code
```bash
cd src/<subject>
ghc -o main Main.hs
./main
```

---

## Authors

**Matthew Long**
The YonedaAI Collaboration
YonedaAI Research Collective
Chicago, IL
matthew@yonedaai.com | [yonedaai.com](https://yonedaai.com)

---

## Methodology

Papers were generated using a multi-agent pipeline:
1. **Knowledge Base Agent** — ingested 7 source .tex files into a structured knowledge base
2. **6 Worker Agents** — each wrote a 20+ page paper with Haskell code from the Yoneda Constraint perspective
3. **Gemini CLI Peer Review** — each paper reviewed by Google Gemini for mathematical correctness, clarity, and completeness
4. **Revision Pass** — all reviewer feedback addressed and incorporated
5. **PDF Compilation** — LaTeX compiled with GrokRxiv DOI sidebars

See [`agent-execution-plan.md`](agent-execution-plan.md) for the reusable execution schema.

---

## License

All rights reserved. For academic use and citation, please contact the authors.
