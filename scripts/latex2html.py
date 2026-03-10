#!/usr/bin/env python3
"""
Convert LaTeX papers to styled HTML for GitHub Pages using pandoc + post-processing.

Usage:
    python3 latex2html.py --config papers.yaml
    python3 latex2html.py --latex-dir papers/latex --html-dir docs/papers --template scripts/paper-template.html

Config YAML format:
    project_title: "The AI Operating System"
    papers:
      - name: "agent-scheduler"
        title: "Agent Scheduler"
        part: "Part I"
      - name: "tool-interface"
        title: "Tool Interface Layer"
        part: "Part II"

Requires: pandoc (brew install pandoc)
"""

import argparse
import re
import subprocess
import sys
import json
from pathlib import Path


def clean_latex_artifacts(html: str) -> str:
    """Remove remaining LaTeX commands from pandoc HTML output."""
    # Inline commands
    html = re.sub(r'\\texttt\{([^}]*)\}', r'<code>\1</code>', html)
    html = re.sub(r'\\textbf\{([^}]*)\}', r'<strong>\1</strong>', html)
    html = re.sub(r'\\emph\{([^}]*)\}', r'<em>\1</em>', html)
    html = re.sub(r'\\textit\{([^}]*)\}', r'<em>\1</em>', html)
    html = re.sub(r'\\textsc\{([^}]*)\}', r'\1', html)
    html = re.sub(r'\\textsf\{([^}]*)\}', r'\1', html)

    # Sizing and spacing
    html = re.sub(r'\\(Large|LARGE|large|huge|Huge|small|footnotesize|tiny|normalsize)\b', '', html)
    html = re.sub(r'\\vspace\*?\{[^}]*\}', '', html)
    html = re.sub(r'\\hspace\*?\{[^}]*\}', '', html)
    html = re.sub(r'\\noindent\b', '', html)
    html = re.sub(r'\\centering\b', '', html)
    html = re.sub(r'\\raggedright\b', '', html)

    # References and labels
    html = re.sub(r'\\label\{[^}]*\}', '', html)

    # lstlisting remnants
    html = re.sub(r'\\begin\{lstlisting\}\[[^\]]*\]', '', html)
    html = re.sub(r'\\begin\{lstlisting\}', '', html)
    html = re.sub(r'\\end\{lstlisting\}', '', html)

    # Math symbols that might not render outside KaTeX
    html = html.replace('\\cdot', '&middot;')
    html = html.replace('\\times', '&times;')
    html = html.replace('\\leq', '&le;')
    html = html.replace('\\geq', '&ge;')
    html = html.replace('\\rightarrow', '&rarr;')
    html = html.replace('\\Rightarrow', '&rArr;')
    html = html.replace('\\mapsto', '&#8614;')
    html = html.replace('\\infty', '&infin;')
    html = html.replace('\\ldots', '&hellip;')

    # Clean leftover backslash commands
    html = re.sub(r'\\(par|medskip|bigskip|smallskip|newline|linebreak)\b', '', html)
    html = re.sub(r'\\(clearpage|newpage|pagebreak)\b', '', html)

    # Second pass for nested patterns
    html = re.sub(r'\\texttt\{([^}]*)\}', r'<code>\1</code>', html)
    html = re.sub(r'\\textbf\{([^}]*)\}', r'<strong>\1</strong>', html)
    html = re.sub(r'\\emph\{([^}]*)\}', r'<em>\1</em>', html)

    return html


def convert_paper(
    name: str,
    title: str,
    part: str,
    project_title: str,
    latex_dir: Path,
    html_dir: Path,
    template: str,
) -> int:
    """Convert a single paper from LaTeX to styled HTML. Returns artifact count."""
    tex_file = latex_dir / f"{name}.tex"
    html_file = html_dir / f"{name}.html"
    full_title = f"{title} — {project_title}, {part}"

    print(f"Converting {name}...")

    if not tex_file.exists():
        print(f"  ERROR: {tex_file} not found")
        return -1

    # Run pandoc to get HTML body
    result = subprocess.run(
        [
            "pandoc", str(tex_file),
            "--to", "html5",
            "--katex",
            "--toc", "--toc-depth=3",
            "--number-sections",
            "--no-highlight",
            "--wrap=none",
        ],
        capture_output=True, text=True,
    )

    if result.returncode != 0:
        print(f"  pandoc warning: {result.stderr[:200]}")

    body = result.stdout
    if not body.strip():
        print(f"  ERROR: pandoc produced empty output for {name}")
        return -1

    # Clean LaTeX artifacts
    body = clean_latex_artifacts(body)

    # Substitute into template
    html = template.replace("{{TITLE}}", full_title)
    html = html.replace("{{PDF_LINK}}", f"../latex/{name}.pdf")
    html = html.replace("{{PART}}", part)
    html = html.replace("{{BODY}}", body)

    # Write output
    html_dir.mkdir(parents=True, exist_ok=True)
    html_file.write_text(html)

    # Count remaining artifacts
    remaining = len(re.findall(r'\\(texttt|textbf|vspace|Large|emph|textsc)\b', html))
    lines = html.count('\n') + 1
    print(f"  Done: {lines} lines, {remaining} remaining artifacts")
    return remaining


def load_config(config_path: Path) -> dict:
    """Load YAML or JSON config file."""
    text = config_path.read_text()
    # Try JSON first (no dependency), then YAML
    try:
        return json.loads(text)
    except json.JSONDecodeError:
        pass
    try:
        import yaml
        return yaml.safe_load(text)
    except ImportError:
        print("ERROR: PyYAML not installed. Use JSON config or: pip install pyyaml")
        sys.exit(1)


def main():
    parser = argparse.ArgumentParser(description="Convert LaTeX papers to styled HTML")
    parser.add_argument("--config", type=Path, help="YAML/JSON config file")
    parser.add_argument("--latex-dir", type=Path, help="Directory containing .tex files")
    parser.add_argument("--html-dir", type=Path, help="Output directory for .html files")
    parser.add_argument("--template", type=Path, help="HTML template file")
    parser.add_argument("--project-title", default="Research Series", help="Project title for page headers")
    parser.add_argument("--papers", nargs="*", help="Papers as name:title:part (e.g. 'my-paper:My Paper:Part I')")
    args = parser.parse_args()

    if args.config:
        config = load_config(args.config)
        project_title = config.get("project_title", "Research Series")
        latex_dir = Path(config.get("latex_dir", "papers/latex"))
        html_dir = Path(config.get("html_dir", "docs/papers"))
        template_path = Path(config.get("template", "scripts/paper-template.html"))
        papers = [(p["name"], p["title"], p["part"]) for p in config["papers"]]
    elif args.latex_dir and args.html_dir and args.template and args.papers:
        project_title = args.project_title
        latex_dir = args.latex_dir
        html_dir = args.html_dir
        template_path = args.template
        papers = []
        for p in args.papers:
            parts = p.split(":")
            if len(parts) != 3:
                print(f"ERROR: Paper spec '{p}' must be 'name:title:part'")
                sys.exit(1)
            papers.append(tuple(parts))
    else:
        parser.print_help()
        print("\nExamples:")
        print("  python3 latex2html.py --config papers.yaml")
        print("  python3 latex2html.py --latex-dir papers/latex --html-dir docs/papers \\")
        print("    --template scripts/paper-template.html --project-title 'My Research' \\")
        print("    --papers 'paper-a:Paper A:Part I' 'paper-b:Paper B:Part II'")
        sys.exit(1)

    if not template_path.exists():
        print(f"ERROR: Template not found at {template_path}")
        sys.exit(1)

    template = template_path.read_text()

    total_artifacts = 0
    for name, title, part in papers:
        count = convert_paper(name, title, part, project_title, latex_dir, html_dir, template)
        if count > 0:
            total_artifacts += count

    print(f"\nAll {len(papers)} papers converted.")
    if total_artifacts == 0:
        print("  All clean — no LaTeX artifacts detected.")
    else:
        print(f"  WARNING: {total_artifacts} total remaining LaTeX artifacts.")


if __name__ == "__main__":
    main()
