#!/bin/sh

here="$(cd "$(dirname "$0")" && pwd)"
set -x

cd "${here}"

poetry install --no-root


set -e

./.venv/bin/python3 main.py
