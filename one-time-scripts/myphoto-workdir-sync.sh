#!/usr/bin/env bash

set -euo pipefail

usage() {
    cat <<'EOF'
Usage:
  myphoto-workdir-sync.sh [--backup] [--recursive] [--dry-run] <src-dir> <dst-dir>

Syncs files from src-dir to dst-dir without deleting anything in dst-dir.

Default behaviour (no --recursive):
  - Copies every top-level file from src-dir to dst-dir.
  - For each top-level directory in src-dir, copies its direct children
    (files only) into the matching directory under dst-dir.
  - Does NOT recurse deeper than one level of directories.

Flags:
  --backup      Before overwriting a file that differs, save the existing
                copy as <file>.bak (timestamped if .bak already exists).
  --recursive   Recurse into all directory levels instead of stopping
                after one level of subdirectories.
  --dry-run     Print what would be copied without actually copying.
  -h, --help    Show this help.

Examples:
  myphoto-workdir-sync.sh /mnt/photos/2024 /data/photos/2024
  myphoto-workdir-sync.sh --backup --recursive /mnt/A /mnt/B
EOF
}

# ---- Defaults ----
BACKUP=0
RECURSIVE=0
DRY_RUN=0

# ---- Parse args ----
POSITIONAL=()
while (($#)); do
    case "$1" in
        --backup)
            BACKUP=1
            shift
            ;;
        --recursive)
            RECURSIVE=1
            shift
            ;;
        --dry-run)
            DRY_RUN=1
            shift
            ;;
        -h | --help)
            usage
            exit 0
            ;;
        --)
            shift
            POSITIONAL+=("$@")
            break
            ;;
        -*)
            echo "Unknown option: $1" >&2
            usage >&2
            exit 1
            ;;
        *)
            POSITIONAL+=("$1")
            shift
            ;;
    esac
done

if ((${#POSITIONAL[@]} != 2)); then
    echo "Error: expected exactly 2 positional arguments (src-dir dst-dir), got ${#POSITIONAL[@]}" >&2
    usage >&2
    exit 1
fi

SRC="${POSITIONAL[0]}"
DST="${POSITIONAL[1]}"

if [[ ! -d $SRC ]]; then
    echo "Error: source directory does not exist: $SRC" >&2
    exit 1
fi

# ---- Helpers ----

# backup_if_needed <dst-file>
#   If --backup is active and dst-file exists and differs from the source,
#   rename it to <dst-file>.bak (or <dst-file>.bak.<timestamp> if .bak exists).
backup_if_needed() {
    local dst_file="$1"
    local src_file="$2"

    if ((BACKUP == 0)); then
        return
    fi
    if [[ ! -e $dst_file ]]; then
        return
    fi
    # Only back up when content actually differs
    if cmp -s "$src_file" "$dst_file"; then
        return
    fi

    local bak="${dst_file}.bak"
    if [[ -e $bak ]]; then
        bak="${dst_file}.bak.$(date +%Y%m%d%H%M%S)"
    fi
    if ((DRY_RUN)); then
        echo "[dry-run] backup: $dst_file -> $bak"
    else
        echo "backup: $dst_file -> $bak"
        mv -- "$dst_file" "$bak"
    fi
}

# copy_file <src-file> <dst-file>
copy_file() {
    local src_file="$1"
    local dst_file="$2"

    # Skip if identical
    if [[ -e $dst_file ]] && cmp -s "$src_file" "$dst_file"; then
        return
    fi

    backup_if_needed "$dst_file" "$src_file"

    if ((DRY_RUN)); then
        echo "[dry-run] copy: $src_file -> $dst_file"
    else
        echo "copy: $src_file -> $dst_file"
        cp -- "$src_file" "$dst_file"
    fi
}

# sync_dir <src-dir> <dst-dir> <depth>
#   depth=0 means we are at the top level.
#   Without --recursive, we stop recursing when depth > 0.
sync_dir() {
    local src_dir="$1"
    local dst_dir="$2"
    local depth="$3"

    # Ensure destination directory exists
    if [[ ! -d $dst_dir ]]; then
        if ((DRY_RUN)); then
            echo "[dry-run] mkdir: $dst_dir"
        else
            echo "mkdir: $dst_dir"
            mkdir -p -- "$dst_dir"
        fi
    fi

    # Copy direct child files
    for entry in "$src_dir"/*; do
        [[ -e $entry ]] || continue
        local name
        name="$(basename -- "$entry")"

        if [[ -f $entry ]]; then
            copy_file "$entry" "$dst_dir/$name"
        elif [[ -d $entry ]]; then
            if ((RECURSIVE)) || ((depth == 0)); then
                sync_dir "$entry" "$dst_dir/$name" $((depth + 1))
            fi
        fi
    done
}

# ---- Main ----
sync_dir "$SRC" "$DST" 0
