#!/usr/bin/env bash
set -eo pipefail

ENV_NAME="${RGENOMETRACKUI_CONDA_ENV:-rgenometrackui}"
PORT="${PORT:-3838}"
HOST="${HOST:-127.0.0.1}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
APP_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"

cd "$APP_DIR"

if command -v conda >/dev/null 2>&1; then
  CONDA_BASE="$(conda info --base 2>/dev/null)"
else
  echo "[ERROR] conda not found in PATH." >&2
  exit 1
fi

# Conda activation is not compatible with bash nounset (-u),
# so do not enable set -u around this block.
source "${CONDA_BASE}/etc/profile.d/conda.sh"

if [ "${CONDA_DEFAULT_ENV:-}" != "$ENV_NAME" ]; then
  conda activate "$ENV_NAME"
fi

export PATH="${CONDA_PREFIX}/bin:${PATH}"
export CONDA_DEFAULT_ENV="$ENV_NAME"
export RGENOMETRACKUI_ROOT="$APP_DIR"
export MPLBACKEND=Agg
export PORT="$PORT"
export HOST="$HOST"

# macOS dynamic libraries
export DYLD_LIBRARY_PATH="${CONDA_PREFIX}/lib${DYLD_LIBRARY_PATH:+:$DYLD_LIBRARY_PATH}"

echo "=== rGenomeTrackUI ==="
echo "  APP_DIR           : $APP_DIR"
echo "  CONDA_DEFAULT_ENV : ${CONDA_DEFAULT_ENV:-NA}"
echo "  CONDA_PREFIX      : ${CONDA_PREFIX:-NA}"
echo "  Rscript           : $(which Rscript || true)"
echo "  R                 : $(which R || true)"
echo "  python            : $(which python || true)"
echo "  pyGenomeTracks    : $(which pyGenomeTracks || true)"
echo "  bedtools          : $(which bedtools || true)"
echo "  URL               : http://${HOST}:${PORT}"
echo ""

exec Rscript app.R
