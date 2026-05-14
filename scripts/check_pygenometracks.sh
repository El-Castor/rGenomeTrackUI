#!/usr/bin/env bash
# =============================================================================
# rGenomeTrackUI — pyGenomeTracks verification
# =============================================================================
# Version      : 1.0.0
# Last updated : 2026-05-12
#
# Description:
#   Verifies that pyGenomeTracks and its Python dependencies are correctly
#   installed in the rgenometrackui conda environment.
#   Also runs a minimal functional test (generate a simple tracks.ini and
#   attempt a placeholder plot if a test region and input are available).
#
# Usage:
#   conda activate rgenometrackui
#   bash scripts/check_pygenometracks.sh
#
# Exit codes:
#   0  — all checks passed (or only warnings)
#   1  — critical failures detected
# =============================================================================

set -uo pipefail

# ---------------------------------------------------------------------------
# Colors
# ---------------------------------------------------------------------------
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'

ok()   { echo -e "  ${GREEN}[OK]${NC}    $*"; }
warn() { echo -e "  ${YELLOW}[WARN]${NC}   $*"; WARN_COUNT=$((WARN_COUNT+1)); }
err()  { echo -e "  ${RED}[FAIL]${NC}   $*"; FAIL_COUNT=$((FAIL_COUNT+1)); }
info() { echo -e "  ${BLUE}[INFO]${NC}   $*"; }

FAIL_COUNT=0
WARN_COUNT=0
ENV_NAME="rgenometrackui"

echo ""
echo -e "${BOLD}=============================================================${NC}"
echo -e "${BOLD}  rGenomeTrackUI — pyGenomeTracks Check${NC}"
echo -e "${BOLD}=============================================================${NC}"

# ---------------------------------------------------------------------------
# Detect environment context
# ---------------------------------------------------------------------------

ACTIVE_ENV="${CONDA_DEFAULT_ENV:-<none>}"
ACTIVE_PREFIX="${CONDA_PREFIX:-<none>}"

echo ""
echo -e "${BOLD}--- 0. Environment context ---${NC}"
info "Active env    : ${ACTIVE_ENV}"
info "Active prefix : ${ACTIVE_PREFIX}"

if [[ "${ACTIVE_ENV}" == "${ENV_NAME}" ]]; then
  ok "Running inside '${ENV_NAME}' conda environment."
else
  warn "Not running inside '${ENV_NAME}'. Active env: '${ACTIVE_ENV}'."
  warn "Consider: conda activate ${ENV_NAME} && bash scripts/check_pygenometracks.sh"
fi

# Try to find the env prefix even if not activated
if [[ "${ACTIVE_ENV}" != "${ENV_NAME}" ]]; then
  ENV_PREFIX="$(conda env list 2>/dev/null | grep "^${ENV_NAME}[[:space:]]" | awk '{print $NF}' || echo '')"
  if [[ -n "${ENV_PREFIX}" ]]; then
    info "Found env prefix: ${ENV_PREFIX}"
    PYTHON_CMD="${ENV_PREFIX}/bin/python"
    PGT_CMD="${ENV_PREFIX}/bin/pyGenomeTracks"
  else
    PYTHON_CMD="$(command -v python 2>/dev/null || echo 'python')"
    PGT_CMD="$(command -v pyGenomeTracks 2>/dev/null || echo 'pyGenomeTracks')"
  fi
else
  PYTHON_CMD="$(command -v python 2>/dev/null || echo 'python')"
  PGT_CMD="$(command -v pyGenomeTracks 2>/dev/null || echo 'pyGenomeTracks')"
fi

# ---------------------------------------------------------------------------
# 1. Python availability
# ---------------------------------------------------------------------------

echo ""
echo -e "${BOLD}--- 1. Python availability ---${NC}"

if command -v "${PYTHON_CMD}" &> /dev/null || [[ -x "${PYTHON_CMD}" ]]; then
  PY_VERSION="$("${PYTHON_CMD}" --version 2>&1)"
  PY_PATH="$(command -v "${PYTHON_CMD}" 2>/dev/null || echo "${PYTHON_CMD}")"
  ok "Python found: ${PY_PATH}"
  info "Version: ${PY_VERSION}"

  # Check Python >= 3.8
  PY_VER_NUM="$("${PYTHON_CMD}" -c "import sys; print(f'{sys.version_info.major}.{sys.version_info.minor}')" 2>/dev/null || echo '0.0')"
  if awk "BEGIN {exit !(${PY_VER_NUM} >= 3.8)}"; then
    ok "Python version >= 3.8 (${PY_VER_NUM})"
  else
    err "Python version ${PY_VER_NUM} is below required 3.8"
  fi
else
  err "Python not found."
fi

# ---------------------------------------------------------------------------
# 2. pyGenomeTracks version
# ---------------------------------------------------------------------------

echo ""
echo -e "${BOLD}--- 2. pyGenomeTracks version ---${NC}"

if [[ -x "${PGT_CMD}" ]] || command -v pyGenomeTracks &> /dev/null; then
  PGT_CMD_RESOLVED="$(command -v pyGenomeTracks 2>/dev/null || echo "${PGT_CMD}")"
  PGT_VERSION="$("${PGT_CMD_RESOLVED}" --version 2>&1 || echo 'unknown')"
  ok "pyGenomeTracks found: ${PGT_CMD_RESOLVED}"
  ok "Version: ${PGT_VERSION}"
else
  err "pyGenomeTracks not found in PATH or environment."
  err "Install: conda activate ${ENV_NAME} && conda install -c bioconda pygenometracks"
fi

# ---------------------------------------------------------------------------
# 3. Python package imports
# ---------------------------------------------------------------------------

echo ""
echo -e "${BOLD}--- 3. Python package imports ---${NC}"

check_python_pkg () {
  local pkg="$1"
  local import_name="${2:-$1}"
  if "${PYTHON_CMD}" -c "import ${import_name}" &> /dev/null 2>&1; then
    local ver
    ver="$("${PYTHON_CMD}" -c "import ${import_name}; print(getattr(${import_name}, '__version__', 'unknown'))" 2>/dev/null || echo 'unknown')"
    ok "$(printf '%-20s' "${pkg}") version: ${ver}"
  else
    err "$(printf '%-20s' "${pkg}") NOT available"
  fi
}

check_python_pkg "numpy"
check_python_pkg "matplotlib"
check_python_pkg "intervaltree"
check_python_pkg "pyBigWig"
check_python_pkg "pybedtools"
check_python_pkg "gffutils"
check_python_pkg "pysam"
check_python_pkg "tqdm"
check_python_pkg "pyfaidx"
check_python_pkg "pygenometracks" "pygenometracks"

# ---------------------------------------------------------------------------
# 4. BEDTools availability (required since pyGenomeTracks >= 3.5)
# ---------------------------------------------------------------------------

echo ""
echo -e "${BOLD}--- 4. BEDTools (required by pyGenomeTracks >= 3.5) ---${NC}"

if command -v bedtools &> /dev/null; then
  BT_VERSION="$(bedtools --version 2>&1 | head -1)"
  BT_PATH="$(command -v bedtools)"
  ok "bedtools found: ${BT_PATH}"
  info "Version: ${BT_VERSION}"
else
  err "bedtools not found in PATH."
  err "Install: conda activate ${ENV_NAME} && conda install -c bioconda bedtools"
fi

# ---------------------------------------------------------------------------
# 5. Minimal functional test: generate a tracks.ini
# ---------------------------------------------------------------------------

echo ""
echo -e "${BOLD}--- 5. Minimal functional test (tracks.ini generation) ---${NC}"

TMPDIR_TEST="$(mktemp -d)"
TRACKS_INI="${TMPDIR_TEST}/test_tracks.ini"

# Write a minimal test tracks.ini (no data file needed for x-axis / spacer)
cat > "${TRACKS_INI}" << 'EOF'
[x-axis]
where = bottom
fontsize = 10

[spacer]
height = 0.5
EOF

if [[ -f "${TRACKS_INI}" ]]; then
  ok "Minimal tracks.ini created at: ${TRACKS_INI}"
  info "Content:"
  sed 's/^/    /' "${TRACKS_INI}"
else
  err "Failed to create minimal tracks.ini"
fi

# Attempt a minimal pyGenomeTracks plot (requires no data files)
# We use a small synthetic BED to test the rendering pipeline
if command -v pyGenomeTracks &> /dev/null; then
  MINI_BED="${TMPDIR_TEST}/mini.bed"
  MINI_TRACKS="${TMPDIR_TEST}/mini_tracks.ini"
  OUT_PNG="${TMPDIR_TEST}/test_output.png"

  # Create minimal BED file
  printf "chr1\t1000\t2000\tgene1\t0\t+\n" > "${MINI_BED}"
  printf "chr1\t3000\t4000\tgene2\t0\t-\n" >> "${MINI_BED}"

  # Create minimal tracks.ini referencing that BED
  cat > "${MINI_TRACKS}" << EOF
[test_bed]
file = ${MINI_BED}
file_type = bed
title = Test BED
height = 2
color = blue

[x-axis]
where = bottom
EOF

  info "Attempting minimal pyGenomeTracks plot ..."
  if pyGenomeTracks \
      --tracks "${MINI_TRACKS}" \
      --region "chr1:500-4500" \
      --outFileName "${OUT_PNG}" \
      --width 10 \
      --dpi 72 \
      > "${TMPDIR_TEST}/pgt_stdout.log" 2> "${TMPDIR_TEST}/pgt_stderr.log"; then

    if [[ -f "${OUT_PNG}" ]]; then
      ok "pyGenomeTracks generated output: ${OUT_PNG}"
      ok "Minimal rendering test PASSED."
    else
      warn "pyGenomeTracks command succeeded but output file not found."
      WARN_COUNT=$((WARN_COUNT+1))
    fi
  else
    warn "pyGenomeTracks rendering failed (may be OK if display/X11 unavailable)."
    info "stdout: $(cat "${TMPDIR_TEST}/pgt_stdout.log" 2>/dev/null || echo '<empty>')"
    info "stderr: $(head -5 "${TMPDIR_TEST}/pgt_stderr.log" 2>/dev/null || echo '<empty>')"
    warn "Hint: headless rendering may require setting MPLBACKEND=Agg"
    WARN_COUNT=$((WARN_COUNT+1))
  fi
else
  warn "Skipping rendering test — pyGenomeTracks not in PATH."
fi

# Cleanup
rm -rf "${TMPDIR_TEST}"

# ---------------------------------------------------------------------------
# 6. MPLBACKEND check (headless rendering)
# ---------------------------------------------------------------------------

echo ""
echo -e "${BOLD}--- 6. Matplotlib backend (headless rendering) ---${NC}"

MPL_BACKEND="${MPLBACKEND:-<not set>}"
info "MPLBACKEND env var: ${MPL_BACKEND}"

if [[ "${MPL_BACKEND}" == "Agg" || "${MPL_BACKEND}" == "agg" ]]; then
  ok "MPLBACKEND=Agg set — headless rendering enabled."
elif [[ "${MPL_BACKEND}" == "<not set>" ]]; then
  warn "MPLBACKEND is not set. pyGenomeTracks may fail in headless environments."
  warn "Recommended: export MPLBACKEND=Agg"
else
  info "MPLBACKEND=${MPL_BACKEND}"
fi

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

echo ""
echo -e "${BOLD}=============================================================${NC}"
echo -e "${BOLD}  pyGenomeTracks Check — Summary${NC}"
echo -e "${BOLD}=============================================================${NC}"
echo ""
echo -e "  Errors   : ${FAIL_COUNT}"
echo -e "  Warnings : ${WARN_COUNT}"
echo ""

if [[ "${FAIL_COUNT}" -eq 0 && "${WARN_COUNT}" -eq 0 ]]; then
  echo -e "  ${GREEN}${BOLD}STATUS: OK${NC}"
  exit 0
elif [[ "${FAIL_COUNT}" -eq 0 ]]; then
  echo -e "  ${YELLOW}${BOLD}STATUS: WARNING — ${WARN_COUNT} warning(s). Check details above.${NC}"
  exit 0
else
  echo -e "  ${RED}${BOLD}STATUS: FAIL — ${FAIL_COUNT} critical error(s).${NC}"
  exit 1
fi
