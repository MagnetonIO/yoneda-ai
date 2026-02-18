# GrokRxiv DOI Sidebar — Agent Instruction Template

## Overview
After peer review is complete, add the GrokRxiv DOI sidebar to the LaTeX document. This is a rotated 90° label on the left margin of page 1 only, spanning the full page height, styled to mimic arXiv's gray DOI sidebar.

## Required Information (extract from the paper)
- **DOI slug**: `GrokRxiv:YYYY.MM.<short-slug>` — use publication year, month, and a kebab-case slug derived from the paper's title or topic
- **Category tag**: e.g. `[pol.TH]`, `[phys.GR]`, `[cs.AI]`, `[math.CT]` — choose the most appropriate arXiv-style category
- **Date**: Publication date in `DD Mon YYYY` format (e.g. `14 Feb 2026`)

## LaTeX Implementation

### 1. Required packages (add to preamble if not already present)
```latex
\usepackage{tikz}
\usepackage{everypage}
\usepackage{xcolor}
```

### 2. Color definition
```latex
\definecolor{grokgray}{RGB}{110,110,110}
```

### 3. Sidebar hook (place in preamble after package imports)
```latex
\AddEverypageHook{%
  \ifnum\value{page}=1
    \begin{tikzpicture}[remember picture, overlay]
      \node[
        rotate=90,
        anchor=south,
        font=\Large\sffamily\bfseries\color{grokgray},
        inner sep=0pt
      ] at ([xshift=38pt, yshift=0.52\paperheight]current page.south west)
      {GrokRxiv:<DOI_SLUG>\quad
       [\,<CATEGORY>\,]\quad
       <DATE>};
    \end{tikzpicture}
  \fi
}
```

### 4. Substitution rules

| Placeholder    | Rule                                                                 | Example                                      |
|----------------|----------------------------------------------------------------------|----------------------------------------------|
| `<DOI_SLUG>`   | `YYYY.MM.<kebab-case-topic>` from title/subject                      | `2026.02.largo-eu-democratic-capture`         |
| `<CATEGORY>`   | arXiv-style category code matching the paper's primary domain        | `pol.TH`, `hep-th`, `cs.AI`, `math.CT`       |
| `<DATE>`       | Publication date, `DD Mon YYYY`                                      | `14 Feb 2026`                                 |

## Positioning Parameters
- `xshift=38pt` — horizontal offset from left edge (adjusts distance from spine)
- `yshift=0.52\paperheight` — vertical centering (0.5 = exact center; 0.52 nudges slightly upward)
- `rotate=90` — text reads bottom-to-top along left margin
- `anchor=south` — anchors the text baseline at the computed position
- `\Large\sffamily\bfseries` — large, sans-serif, bold (matches arXiv aesthetic)

## Formatting Constraints
- Page 1 only (controlled by `\ifnum\value{page}=1`)
- Uses `fancyhdr` + `everypage` pattern for reliable placement
- Must appear BEFORE `\begin{document}`
- The `\quad` spacers between DOI, category, and date are mandatory for readability
- Category tag is wrapped in `\,` thin spaces inside brackets: `[\,cat.XX\,]`

## Author Block Convention
- **Author**: Matthew Long
- **Collaboration**: The YonedaAI Collaboration, YonedaAI Research Collective
- **Location**: Chicago, IL
- **Contact**: matthew@yonedaai.com · https://yonedaai.com

## Quick Reference — Full Block (copy-paste ready)
```latex
\definecolor{grokgray}{RGB}{110,110,110}
\AddEverypageHook{%
  \ifnum\value{page}=1
    \begin{tikzpicture}[remember picture, overlay]
      \node[
        rotate=90,
        anchor=south,
        font=\Large\sffamily\bfseries\color{grokgray},
        inner sep=0pt
      ] at ([xshift=38pt, yshift=0.52\paperheight]current page.south west)
      {GrokRxiv:YYYY.MM.slug-here\quad
       [\,cat.XX\,]\quad
       DD Mon YYYY};
    \end{tikzpicture}
  \fi
}
```
