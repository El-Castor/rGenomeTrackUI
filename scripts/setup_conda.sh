#!/usr/bin/env bash
# =============================================================================
# rGenomeTrackUI — Automated Conda environment setup
# =============================================================================
# Version      : 1.0.0
# Last updated : 2026-05-12
#
# Description:
#   Full automated setup of the rgenometrackui Conda environment.
#   Creates the environment, installs R packages via install.R,
#   then runs all verification scripts.
#
# Usage:
#   bash scripts/setup_conda.sh
#
# Requirements:
#   - Conda (Miniconda or Anaconda) must be installed and in PATH
#   - Run from the root of the rGenomeTrackUI project directory
#
# Exit codes:
#   0  — success
#   1  — fatal error (conda not found, env creation failed, etc.)
# =============================================================================

set -euo pipefail

# ---------------------------------------------------------------------------
# Colors and formatting
# ---------------------------------------------------------------------------
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m'  # No color

ok()   { echo -e "  ${GREEN}[OK]${NC}    $*"; }
warn() { echo -e "  ${YELLOW}[WARN]${NC}   $*"; }
err()  { echo -e "  ${RED}[ERROR]${NC}  $*"; }
info() { echo -e "  ${BLUE}[INFO]${NC}   $*"; }
step() { echo -e "\n${BOLD}--- $* ---${NC}"; }

# ---------------------------------------------------------------------------
# Variables
# ---------------------------------------------------------------------------
ENV_NAME="rgenometrackui"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
ENV_FILE="${PROJECT_ROOT}/environment.yml"
INSTALL_R="${PROJECT_ROOT}/install.R"
LOG_DIR="${PROJECT_ROOT}/logs"
LOG_FILE="${LOG_DIR}/setup_conda_$(date +%Y%m%d_%H%M%S).log"

# ---------------------------------------------------------------------------
# Sanity checks
# ---------------------------------------------------------------------------

echo ""
echo -e "${BOLD}=============================================================${NC}"
echo -e "${BOLD}  rGenomeTrackUI — Conda Environment Setup${NC}"
echo -e "${BOLD}=============================================================${NC}"

step "Verifying project structure"

if [[ ! -f "${ENV_FILE}" ]]; then
  err "environment.yml not found at: ${ENV_FILE}"
  err "Run this script from the rGenomeTrackUI project root."
  exit 1
fi
ok "Found environment.yml"

if [[ ! -f "${INSTALL_R}" ]]; then
  err "install.R not found at: ${INSTALL_R}"
  exit 1
fi
ok "Found install.R"

# Create logs directory
mkdir -p "${LOG_DIR}"
info "Logs will be written to: ${LOG_FILE}"

# ---------------------------------------------------------------------------
# Step 1: Find conda or mamba
# ---------------------------------------------------------------------------

step "Detecting package manager (mamba preferred over conda)"

CONDA_CMD=""
SOLVER_NOTE=""

if command -v mamba &> /dev/null; then
  CONDA_CMD="mamba"
  SOLVER_NOTE="Using mamba (faster solver)"
  ok "mamba found: $(command -v mamba)"
elif command -v conda &> /dev/null; then
  CONDA_CMD="conda"
  SOLVER_NOTE="Using conda (consider installing mamba for faster resolution)"
  ok "conda found: $(command -v conda)"
  warn "${SOLVER_NOTE}"
else
  err "Neither mamba nor conda found in PATH."
  err "Please install Miniconda or Anaconda first:"
  err "  https://docs.conda.io/en/latest/miniconda.html"
  exit 1
fi

# ---------------------------------------------------------------------------
# Step 2: Check if environment already exists
# ---------------------------------------------------------------------------

step "Checking if environment '${ENV_NAME}' already exists"

ENV_EXISTS=false
if conda env list | grep -q "^${ENV_NAME}[[:space:]]"; then
  ENV_EXISTS=true
  warn "Environment '${ENV_NAME}' already exists."
  echo ""
  read -r -p "  Options: [u]pdate existing / [r]ecreate from scratch / [s]kip creation (default: skip): " CHOICE
  CHOICE="${CHOICE:-s}"

  case "${CHOICE}" in
    u|U)
      info "Updating existing environment '${ENV_NAME}' ..."
      ${CONDA_CMD} env update -n "${ENV_NAME}" -f "${ENV_FILE}" --prune \
        2>&1 | tee -a "${LOG_FILE}"
      ok "Environment updated."
      ;;
    r|R)
      warn "Removing existing environment '${ENV_NAME}' ..."
      conda env remove -n "${ENV_NAME}" -y 2>&1 | tee -a "${LOG_FILE}"
      ENV_EXISTS=false
      ;;
    *)
      info "Skipping environment creation — using existing environment."
      ;;
  esac
fi

# ---------------------------------------------------------------------------
# Step 3: Create environment
# ---------------------------------------------------------------------------

if [[ "${ENV_EXISTS}" == "false" ]]; then
  step "Creating environment '${ENV_NAME}' from ${ENV_FILE}"
  info "This may take several minutes..."

  ${CONDA_CMD} env create -f "${ENV_FILE}" 2>&1 | tee -a "${LOG_FILE}"

  if conda env list | grep -q "^${ENV_NAME}[[:space:]]"; then
    ok "Environment '${ENV_NAME}' created successfully."
  else
    err "Environment creation failed. Check log: ${LOG_FILE}"
    exit 1
  fi
fi

# ---------------------------------------------------------------------------
# Step 4: Detect conda prefix for the environment
# ---------------------------------------------------------------------------

step "Detecting environment paths"

CONDA_ENV_PREFIX="$(conda env list | grep "^${ENV_NAME}[[:space:]]" | awk '{print $NF}')"

if [[ -z "${CONDA_ENV_PREFIX}" ]]; then
  err "Could not detect prefix for environment '${ENV_NAME}'."
  exit 1
fi

R_BIN="${CONDA_ENV_PREFIX}/bin/R"
RSCRIPT_BIN="${CONDA_ENV_PREFIX}/bin/Rscript"
PYTHON_BIN="${CONDA_ENV_PREFIX}/bin/python"
PGT_BIN="${CONDA_ENV_PREFIX}/bin/pyGenomeTracks"

info "Conda prefix : ${CONDA_ENV_PREFIX}"
info "R binary     : ${R_BIN}"
info "Python       : ${PYTHON_BIN}"

for BIN in "${R_BIN}" "${RSCRIPT_BIN}" "${PYTHON_BIN}"; do
  if [[ -x "${BIN}" ]]; then
    ok "$(basename ${BIN}) found at ${BIN}"
  else
    err "Binary not found: ${BIN}"
    exit 1
  fi
done

# ---------------------------------------------------------------------------
# Step 5: Ensure critical conda packages are installed (bedtools, samtools...)
# ---------------------------------------------------------------------------
# This step handles the case where the environment already existed before
# environment.yml was updated (e.g. first run, partial setup, etc.)

step "Ensuring critical conda packages are present"

install_conda_pkg () {
  local pkg="$1"
  local channel="${2:-bioconda}"
  local bin_name="${3:-$1}"
  local bin_path="${CONDA_ENV_PREFIX}/bin/${bin_name}"

  if [[ -x "${bin_path}" ]]; then
    ok "${pkg} already present: ${bin_path}"
  else
    info "Installing ${pkg} from ${channel} ..."
    ${CONDA_CMD} install -n "${ENV_NAME}" -c "${channel}" -c conda-forge "${pkg}" -y \
      2>&1 | tee -a "${LOG_FILE}"
    if [[ -x "${bin_path}" ]]; then
      ok "${pkg} installed successfully."
    else
      warn "${pkg} installation may have failed — binary not found at ${bin_path}"
      WARNINGS=$((WARNINGS+1))
    fi
  fi
}

install_conda_pkg "bedtools"   "bioconda"   "bedtools"
install_conda_pkg "samtools"   "bioconda"   "samtools"

# ---------------------------------------------------------------------------
# Step 6: Install R packages via install.R
# ---------------------------------------------------------------------------

step "Installing R packages (install.R)"
info "Running: ${RSCRIPT_BIN} ${INSTALL_R}"

"${RSCRIPT_BIN}" "${INSTALL_R}" 2>&1 | tee -a "${LOG_FILE}"
INSTALL_EXIT="${PIPESTATUS[0]}"

if [[ "${INSTALL_EXIT}" -eq 0 ]]; then
  ok "install.R completed."
else
  warn "install.R exited with code ${INSTALL_EXIT}. Check log for details."
fi

# ---------------------------------------------------------------------------
# Step 6: Run verification scripts
# ---------------------------------------------------------------------------

step "Running verification scripts"

ERRORS=0
WARNINGS=0

# --- Conda environment check ---
if [[ -f "${SCRIPT_DIR}/check_conda_env.sh" ]]; then
  info "Running check_conda_env.sh ..."
  bash "${SCRIPT_DIR}/check_conda_env.sh" 2>&1 | tee -a "${LOG_FILE}" || WARNINGS=$((WARNINGS+1))
else
  warn "check_conda_env.sh not found — skipping."
fi

# --- R dependencies check ---
if [[ -f "${SCRIPT_DIR}/check_r_dependencies.R" ]]; then
  info "Running check_r_dependencies.R ..."
  "${RSCRIPT_BIN}" "${SCRIPT_DIR}/check_r_dependencies.R" 2>&1 | tee -a "${LOG_FILE}" || WARNINGS=$((WARNINGS+1))
else
  warn "check_r_dependencies.R not found — skipping."
fi

# --- pyGenomeTracks check ---
if [[ -f "${SCRIPT_DIR}/check_pygenometracks.sh" ]]; then
  info "Running check_pygenometracks.sh ..."
  bash "${SCRIPT_DIR}/check_pygenometracks.sh" 2>&1 | tee -a "${LOG_FILE}" || WARNINGS=$((WARNINGS+1))
else
  warn "check_pygenometracks.sh not found — skipping."
fi

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------

echo ""
echo -e "${BOLD}=============================================================${NC}"
echo -e "${BOLD}  Setup Summary${NC}"
echo -e "${BOLD}=============================================================${NC}"
echo ""
echo -e "  Environment  : ${BOLD}${ENV_NAME}${NC}"
echo -e "  Prefix       : ${CONDA_ENV_PREFIX}"
echo -e "  Log file     : ${LOG_FILE}"
echo ""

if [[ "${ERRORS}" -eq 0 && "${WARNINGS}" -eq 0 ]]; then
  echo -e "  ${GREEN}${BOLD}STATUS: OK — Environment ready.${NC}"
elif [[ "${ERRORS}" -eq 0 ]]; then
  echo -e "  ${YELLOW}${BOLD}STATUS: WARNING — Setup completed with ${WARNINGS} warning(s). Check log.${NC}"
else
  echo -e "  ${RED}${BOLD}STATUS: ERROR — Setup failed with ${ERRORS} error(s). Check log.${NC}"
fi

echo ""
echo -e "  To activate the environment:"
echo -e "    ${BOLD}conda activate ${ENV_NAME}${NC}"
echo ""
echo -e "  To launch the application:"
echo -e "    ${BOLD}conda activate ${ENV_NAME} && Rscript app.R${NC}"
echo ""
echo -e "  Or without activation:"
echo -e "    ${BOLD}conda run -n ${ENV_NAME} Rscript app.R${NC}"
echo ""
echo -e "${BOLD}=============================================================${NC}"
