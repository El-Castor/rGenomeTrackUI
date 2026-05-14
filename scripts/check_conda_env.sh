#!/usr/bin/env bash
# =============================================================================
# rGenomeTrackUI — Conda environment verification
# =============================================================================
# Version      : 1.0.0
# Last updated : 2026-05-12
#
# Description:
#   Verifies that the rgenometrackui Conda environment is correctly configured.
#   Checks that all key binaries point to the expected environment,
#   and that no global/system installation is being used unintentionally.
#
# Usage:
#   # Basic check (does not require activation):
#   bash scripts/check_conda_env.sh
#
#   # Check within activated environment:
#   conda activate rgenometrackui
#   bash scripts/check_conda_env.sh
#
# Exit codes:
#   0  — all checks passed
#   1  — one or more checks failed
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
warn() { echo -e "  ${YELLOW}[WARN]${NC}   $*"; }
err()  { echo -e "  ${RED}[FAIL]${NC}   $*"; FAIL_COUNT=$((FAIL_COUNT+1)); }
info() { echo -e "  ${BLUE}[INFO]${NC}   $*"; }

FAIL_COUNT=0
WARN_COUNT=0

ENV_NAME="rgenometrackui"

echo ""
echo -e "${BOLD}=============================================================${NC}"
echo -e "${BOLD}  rGenomeTrackUI — Conda Environment Check${NC}"
echo -e "${BOLD}=============================================================${NC}"

# ---------------------------------------------------------------------------
# 1. Check that conda is available
# ---------------------------------------------------------------------------

echo ""
echo -e "${BOLD}--- 1. Conda availability ---${NC}"

if ! command -v conda &> /dev/null; then
  err "conda is not available in PATH."
  echo ""
  echo -e "  ${RED}${BOLD}ABORT: conda not found.${NC}"
  exit 1
fi
ok "conda found: $(command -v conda)"
info "conda version: $(conda --version 2>&1)"

# ---------------------------------------------------------------------------
# 2. Check that the environment exists
# ---------------------------------------------------------------------------

echo ""
echo -e "${BOLD}--- 2. Environment existence ---${NC}"

if conda env list | grep -q "^${ENV_NAME}[[:space:]]"; then
  ok "Environment '${ENV_NAME}' exists."
  ENV_PREFIX="$(conda env list | grep "^${ENV_NAME}[[:space:]]" | awk '{print $NF}')"
  info "Prefix: ${ENV_PREFIX}"
else
  err "Environment '${ENV_NAME}' does NOT exist."
  err "Run: conda env create -f environment.yml"
  echo ""
  echo -e "  ${RED}${BOLD}ABORT: environment not found.${NC}"
  exit 1
fi

# ---------------------------------------------------------------------------
# 3. Check active environment
# ---------------------------------------------------------------------------

echo ""
echo -e "${BOLD}--- 3. Active environment ---${NC}"

ACTIVE_ENV="${CONDA_DEFAULT_ENV:-<none>}"
ACTIVE_PREFIX="${CONDA_PREFIX:-<none>}"

info "Active env    : ${ACTIVE_ENV}"
info "Active prefix : ${ACTIVE_PREFIX}"

if [[ "${ACTIVE_ENV}" == "${ENV_NAME}" ]]; then
  ok "Running inside '${ENV_NAME}' environment."
else
  warn "Not activated — active env is '${ACTIVE_ENV}'."
  warn "For runtime checks, activate first: conda activate ${ENV_NAME}"
  WARN_COUNT=$((WARN_COUNT+1))
fi

# ---------------------------------------------------------------------------
# 4. Check R binary
# ---------------------------------------------------------------------------

echo ""
echo -e "${BOLD}--- 4. R binary ---${NC}"

R_ENV_BIN="${ENV_PREFIX}/bin/R"
R_ACTIVE="$(command -v R 2>/dev/null || echo '')"

if [[ -x "${R_ENV_BIN}" ]]; then
  R_VERSION="$("${R_ENV_BIN}" --version 2>&1 | head -1)"
  ok "R found in env: ${R_ENV_BIN}"
  info "Version: ${R_VERSION}"
else
  err "R not found in environment at: ${R_ENV_BIN}"
fi

if [[ -n "${R_ACTIVE}" ]]; then
  if [[ "${R_ACTIVE}" == "${R_ENV_BIN}" || "${R_ACTIVE}" == *"${ENV_NAME}"* ]]; then
    ok "Active R points to conda env: ${R_ACTIVE}"
  else
    warn "Active R does NOT point to '${ENV_NAME}': ${R_ACTIVE}"
    warn "Activate the env before running Rscript: conda activate ${ENV_NAME}"
    WARN_COUNT=$((WARN_COUNT+1))
  fi
fi

# ---------------------------------------------------------------------------
# 5. Check Python binary
# ---------------------------------------------------------------------------

echo ""
echo -e "${BOLD}--- 5. Python binary ---${NC}"

PYTHON_ENV_BIN="${ENV_PREFIX}/bin/python"
PYTHON_ACTIVE="$(command -v python 2>/dev/null || echo '')"

if [[ -x "${PYTHON_ENV_BIN}" ]]; then
  PYTHON_VERSION="$("${PYTHON_ENV_BIN}" --version 2>&1)"
  ok "Python found in env: ${PYTHON_ENV_BIN}"
  info "Version: ${PYTHON_VERSION}"
else
  err "Python not found in environment at: ${PYTHON_ENV_BIN}"
fi

if [[ -n "${PYTHON_ACTIVE}" ]]; then
  if [[ "${PYTHON_ACTIVE}" == "${PYTHON_ENV_BIN}" || "${PYTHON_ACTIVE}" == *"${ENV_NAME}"* ]]; then
    ok "Active Python points to conda env: ${PYTHON_ACTIVE}"
  else
    warn "Active Python does NOT point to '${ENV_NAME}': ${PYTHON_ACTIVE}"
    warn "Activate the env before running Python: conda activate ${ENV_NAME}"
    WARN_COUNT=$((WARN_COUNT+1))
  fi
fi

# ---------------------------------------------------------------------------
# 6. Check pyGenomeTracks
# ---------------------------------------------------------------------------

echo ""
echo -e "${BOLD}--- 6. pyGenomeTracks ---${NC}"

PGT_BIN="${ENV_PREFIX}/bin/pyGenomeTracks"
PGT_ACTIVE="$(command -v pyGenomeTracks 2>/dev/null || echo '')"

if [[ -x "${PGT_BIN}" ]]; then
  PGT_VERSION="$("${PGT_BIN}" --version 2>&1 | tr -d '\n')"
  ok "pyGenomeTracks found in env: ${PGT_BIN}"
  info "Version: ${PGT_VERSION}"
else
  err "pyGenomeTracks NOT found in environment at: ${PGT_BIN}"
  err "Run: conda activate ${ENV_NAME} && conda install -c bioconda pygenometracks"
fi

if [[ -n "${PGT_ACTIVE}" ]]; then
  if [[ "${PGT_ACTIVE}" == *"${ENV_NAME}"* || "${PGT_ACTIVE}" == "${PGT_BIN}" ]]; then
    ok "Active pyGenomeTracks points to conda env: ${PGT_ACTIVE}"
  else
    warn "Active pyGenomeTracks does NOT point to '${ENV_NAME}': ${PGT_ACTIVE}"
    WARN_COUNT=$((WARN_COUNT+1))
  fi
fi

# ---------------------------------------------------------------------------
# 7. Check BEDTools
# ---------------------------------------------------------------------------

echo ""
echo -e "${BOLD}--- 7. BEDTools ---${NC}"

BT_BIN="${ENV_PREFIX}/bin/bedtools"
BT_ACTIVE="$(command -v bedtools 2>/dev/null || echo '')"

if [[ -x "${BT_BIN}" ]]; then
  BT_VERSION="$("${BT_BIN}" --version 2>&1 | head -1)"
  ok "bedtools found in env: ${BT_BIN}"
  info "Version: ${BT_VERSION}"
else
  err "bedtools NOT found in environment at: ${BT_BIN}"
  err "Run: conda activate ${ENV_NAME} && conda install -c bioconda bedtools"
fi

if [[ -n "${BT_ACTIVE}" ]]; then
  if [[ "${BT_ACTIVE}" == *"${ENV_NAME}"* || "${BT_ACTIVE}" == "${BT_BIN}" ]]; then
    ok "Active bedtools points to conda env: ${BT_ACTIVE}"
  else
    warn "Active bedtools does NOT point to '${ENV_NAME}': ${BT_ACTIVE}"
    WARN_COUNT=$((WARN_COUNT+1))
  fi
fi

# ---------------------------------------------------------------------------
# 8. Check SAMtools
# ---------------------------------------------------------------------------

echo ""
echo -e "${BOLD}--- 8. SAMtools ---${NC}"

ST_BIN="${ENV_PREFIX}/bin/samtools"

if [[ -x "${ST_BIN}" ]]; then
  ST_VERSION="$("${ST_BIN}" --version 2>&1 | head -1)"
  ok "samtools found in env: ${ST_BIN}"
  info "Version: ${ST_VERSION}"
else
  warn "samtools NOT found in environment at: ${ST_BIN}"
  WARN_COUNT=$((WARN_COUNT+1))
fi

# ---------------------------------------------------------------------------
# 9. Safety: detect potential global contamination
# ---------------------------------------------------------------------------

echo ""
echo -e "${BOLD}--- 9. Global environment contamination check ---${NC}"

GLOBAL_R="$(which -a R 2>/dev/null | grep -v "${ENV_NAME}" | grep -v "${ENV_PREFIX}" | head -1 || echo '')"
if [[ -n "${GLOBAL_R}" && "${GLOBAL_R}" != "${R_ENV_BIN}" ]]; then
  warn "A system/global R was also found: ${GLOBAL_R}"
  warn "Ensure the conda env is activated before launching the application."
  WARN_COUNT=$((WARN_COUNT+1))
else
  ok "No conflicting system R detected."
fi

GLOBAL_PGT="$(which -a pyGenomeTracks 2>/dev/null | grep -v "${ENV_NAME}" | grep -v "${ENV_PREFIX}" | head -1 || echo '')"
if [[ -n "${GLOBAL_PGT}" && "${GLOBAL_PGT}" != "${PGT_BIN}" ]]; then
  warn "A system/global pyGenomeTracks was also found: ${GLOBAL_PGT}"
  warn "Ensure the conda env is activated before running pyGenomeTracks."
  WARN_COUNT=$((WARN_COUNT+1))
else
  ok "No conflicting system pyGenomeTracks detected."
fi

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

echo ""
echo -e "${BOLD}=============================================================${NC}"
echo -e "${BOLD}  Conda Environment Check — Summary${NC}"
echo -e "${BOLD}=============================================================${NC}"
echo ""
echo -e "  Environment  : ${ENV_NAME}"
echo -e "  Prefix       : ${ENV_PREFIX}"
echo -e "  Errors       : ${FAIL_COUNT}"
echo -e "  Warnings     : ${WARN_COUNT}"
echo ""

if [[ "${FAIL_COUNT}" -eq 0 && "${WARN_COUNT}" -eq 0 ]]; then
  echo -e "  ${GREEN}${BOLD}STATUS: OK${NC}"
  exit 0
elif [[ "${FAIL_COUNT}" -eq 0 ]]; then
  echo -e "  ${YELLOW}${BOLD}STATUS: WARNING — ${WARN_COUNT} warning(s). Activate the environment before use.${NC}"
  exit 0
else
  echo -e "  ${RED}${BOLD}STATUS: FAIL — ${FAIL_COUNT} error(s), ${WARN_COUNT} warning(s).${NC}"
  exit 1
fi
