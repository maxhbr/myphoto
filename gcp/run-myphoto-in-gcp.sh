#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: $0 \
  --project <gcp-project> \
  --region <region> \
  --zone <zone> \
  --bucket <bucket-name> \
  --input-dir <local-input-dir> \
  --output-dir <local-output-dir> \
  [--vm <vm-name>] \
  [--machine-type <type>] \
  [--disk-size <size>] \
  [--image-family <family>] \
  [--image-project <project>] \
  [--input-prefix <prefix>] \
  [--output-prefix <prefix>] \
  [--keep-vm] \
  [--keep-bucket]
EOF
}

PROJECT=""
REGION=""
ZONE=""
BUCKET=""
INPUT_DIR=""
OUTPUT_DIR=""

VM_NAME="myphoto-gcp-$(date +%Y%m%d-%H%M%S)"
MACHINE_TYPE="n2-standard-32"
DISK_SIZE="500GB"
IMAGE_FAMILY="debian-12"
IMAGE_PROJECT="debian-cloud"
INPUT_PREFIX="${VM_NAME}-input"
OUTPUT_PREFIX="${VM_NAME}-output"
KEEP_VM="no"
KEEP_BUCKET="no"

while [ $# -gt 0 ]; do
  case "$1" in
    --project) PROJECT="$2"; shift 2 ;;
    --region) REGION="$2"; shift 2 ;;
    --zone) ZONE="$2"; shift 2 ;;
    --bucket) BUCKET="$2"; shift 2 ;;
    --input-dir) INPUT_DIR="$2"; shift 2 ;;
    --output-dir) OUTPUT_DIR="$2"; shift 2 ;;
    --vm) VM_NAME="$2"; shift 2 ;;
    --machine-type) MACHINE_TYPE="$2"; shift 2 ;;
    --disk-size) DISK_SIZE="$2"; shift 2 ;;
    --image-family) IMAGE_FAMILY="$2"; shift 2 ;;
    --image-project) IMAGE_PROJECT="$2"; shift 2 ;;
    --input-prefix) INPUT_PREFIX="$2"; shift 2 ;;
    --output-prefix) OUTPUT_PREFIX="$2"; shift 2 ;;
    --keep-vm) KEEP_VM="yes"; shift 1 ;;
    --keep-bucket) KEEP_BUCKET="yes"; shift 1 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown argument: $1" >&2; usage; exit 1 ;;
  esac
done

if [ -z "$PROJECT" ] || [ -z "$REGION" ] || [ -z "$ZONE" ] || [ -z "$BUCKET" ]; then
  echo "Missing required GCP parameters." >&2
  usage
  exit 1
fi

if [ -z "$INPUT_DIR" ] || [ -z "$OUTPUT_DIR" ]; then
  echo "Missing input/output directories." >&2
  usage
  exit 1
fi

if [ ! -d "$INPUT_DIR" ]; then
  echo "Input directory does not exist: $INPUT_DIR" >&2
  exit 1
fi

cleanup_vm() {
  if [ "$KEEP_VM" = "no" ]; then
    echo "cleaning up VM: $VM_NAME"
    gcloud compute instances delete "$VM_NAME" \
      --project "$PROJECT" \
      --zone "$ZONE" \
      --quiet || true
  fi
}

trap cleanup_vm EXIT

set -x

if ! gsutil ls -b "gs://$BUCKET" >/dev/null 2>&1; then
  gsutil mb -p "$PROJECT" -l "$REGION" "gs://$BUCKET"
else
  echo "Bucket already exists: $BUCKET"
fi

INPUT_BUCKET_PATH="gs://$BUCKET/$INPUT_PREFIX/"
OUTPUT_BUCKET_PATH="gs://$BUCKET/$OUTPUT_PREFIX/"

gsutil -m rsync -r "$INPUT_DIR" "$INPUT_BUCKET_PATH"

gcloud compute instances create "$VM_NAME" \
  --project "$PROJECT" \
  --zone "$ZONE" \
  --machine-type "$MACHINE_TYPE" \
  --boot-disk-size "$DISK_SIZE" \
  --image-family "$IMAGE_FAMILY" \
  --image-project "$IMAGE_PROJECT" \
  --scopes "https://www.googleapis.com/auth/cloud-platform"

gcloud compute scp "@MYPHOTO_DOCKER@" "$VM_NAME:~/myphoto-docker.tar" \
  --project "$PROJECT" \
  --zone "$ZONE"

gcloud compute scp "@REMOTE_SCRIPT@" "$VM_NAME:~/myphoto-remote.sh" \
  --project "$PROJECT" \
  --zone "$ZONE"

gcloud compute ssh "$VM_NAME" \
  --project "$PROJECT" \
  --zone "$ZONE" \
  --command "bash ~/myphoto-remote.sh '$INPUT_BUCKET_PATH' '$OUTPUT_BUCKET_PATH' ~/myphoto-docker.tar"

mkdir -p "$OUTPUT_DIR"
gsutil -m rsync -r "$OUTPUT_BUCKET_PATH" "$OUTPUT_DIR"

if [ "$KEEP_BUCKET" = "no" ]; then
  gsutil -m rm -r "$INPUT_BUCKET_PATH" || true
  gsutil -m rm -r "$OUTPUT_BUCKET_PATH" || true
fi
