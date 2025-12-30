# Sigma-Distribution Hypothesis

## Overview
A probabilistic framework for understanding lifestyle disease onset, aging, and prevention using sigma-based positioning within population distributions.

## Quick Start
```bash
cd code
pip install -r requirements.txt
python generate_figures.py
```
## Repository Structure

- `manuscript/` - LaTeX source and compiled manuscript
- `code/` - Python scripts for figure generation
- `docs/` - Submission materials and documentation
- `supplementary/` - Additional materials

```text
sigma-distribution-hypothesis/
├─ README.md
├─ LICENSE
├─ CITATION.cff                     # optional but recommended
├─ environment.yml                  # or requirements.txt (choose one)
├─ .gitignore
│
├─ manuscript/
│  ├─ main.tex                      # LaTeX submitted manuscript
│  ├─ references.bib                # move bib entries here
│  ├─ figures/
│  │  ├─ figure1\_sigma\_hba1c\_sample.png
│  │  ├─ figure1\_sigma\_hba1c.pdf  # to be added later
│  │  └─ README.md                  # brief note: figures are illustrative
│  └─ build/                        # local output only
│
├─ code/
│  ├─ figure\_generator.py           # your main plotting script
│  ├─ public\_database\_illustration.py
│  ├─ utils/
│  │  ├─ __init__.py
│  │  └─ plotting.py                # helper functions later
│  └─ run\_all\_figures.py          # regenerates all figures
│
├─ data/
│  ├─ README.md                     # explain: no raw NHANES included
│  └─ synthetic/                    # keep synthetic sample data if any
│     └─ (optional csv files)
│
└─ docs/
   ├─ figure\_policy.md              # what is illustrative vs empirical
   └─ reproducibility.md            # exact steps to regenerate
```

## Reproducibility
### Compile manuscript
- `cd manuscript`
- `pdflatex main.tex` (or `latexmk -pdf main.tex`)

### Regenerate figures
- `python code/figure_generator.py`

## Note on data
This repository does not contain NHANES microdata. Figures labeled “illustrative” use synthetic or NHANES-like parameters for conceptual demonstration only.
