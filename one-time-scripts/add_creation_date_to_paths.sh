#!/usr/bin/env bash

set -euo pipefail

add_creation_date() {
    file_path="$1"
    creation_date=$(stat -c %y "$file_path" | cut -d' ' -f1)
    echo "$file_path - Creation Date: $creation_date"
}

add_creation_dates() {
    for file in "$@"; do
        add_creation_date "$file"
    done
}

add_creation_dates "$@"