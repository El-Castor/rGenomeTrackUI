#!/bin/bash
# =============================================================================
# run_app.sh — Launch rGenomeTrackUI using the conda R
# =============================================================================
# Usage: bash scripts/run_app.sh
#
# This script finds the Rscript from the conda env "rgenometrackui" and uses
# it directly, bypassing any system R that may shadow it in PATH.
# =============================================================================

set -e

ENV_NAME="rgenometrackui"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
APP_DIR="$(dirname "$SCRIPT_DIR")"

# ---- locate conda ----
if command -v conda >/dev/null 2>&1; then
  CONDA_BASE=$(conda info --base 2>/dev/null)
else
  echo "ERROR: conda not found in PATH." >&2
  exit 1
fi

RSCRIPT="${CONDA_BASE}/envs/${ENV_NAME}/bin/Rscript"

if [ ! -x "$RSCRIPT" ]; then
  echo "ERROR: Rscript not found at: $RSCRIPT" >&2
  echo "  Is the conda env '${ENV_NAME}' created?"  >&2
  echo "  Run: conda env create -f environment.yml && Rscript install.R" >&2
  exit 1
fi

echo "=== rGenomeTrackUI ===" 
echo "  R        : $("$RSCRIPT" --version 2>&1 | head -1)"
echo "  Rscript  : $RSCRIPT"
echo "  App dir  : $APP_DIR"
echo ""

# Make the conda env visible to all subprocess calls (pyGenomeTracks, bedtools, etc.)
CONDA_ENV_PREFIX="$(dirname "$(dirname "$RSCRIPT")")"
export PATH="${CONDA_ENV_PREFIX}/bin:${PATH}"
export CONDA_PREFIX="${CONDA_ENV_PREFIX}"
export CONDA_DEFAULT_ENV="${ENV_NAME}"

# Make conda libs visible to dynamically loaded R packages (e.g. imager → libX11)
export DYLD_LIBRARY_PATH="${CONDA_ENV_PREFIX}/lib${DYLD_LIBRARY_PATH:+:$DYLD_LIBRARY_PATH}"

exec "$RSCRIPT" "${APP_DIR}/app.R" "$@"
