#!/usr/bin/env nix-shell
#! nix-shell -i bash -p google-cloud-sdk nix coreutils
set -euo pipefail

usage() {
  cat <<'EOF'
Usage: gcp/run-gcp-harness.sh \
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
  [--image-tar <path>] \
  [--keep-vm] \
  [--keep-bucket]

Notes:
- Builds the docker image locally with `nix build .#myphoto-docker` unless --image-tar is provided.
- Requires gcloud + gsutil configured locally.
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
INPUT_PREFIX="input"
OUTPUT_PREFIX="output"
IMAGE_TAR=""
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
    --image-tar) IMAGE_TAR="$2"; shift 2 ;;
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

if ! command -v gcloud >/dev/null 2>&1; then
  echo "gcloud is required in PATH." >&2
  exit 1
fi

if ! command -v gsutil >/dev/null 2>&1; then
  echo "gsutil is required in PATH." >&2
  exit 1
fi

cleanup_vm() {
  if [ "$KEEP_VM" = "no" ]; then
    gcloud compute instances delete "$VM_NAME" \
      --project "$PROJECT" \
      --zone "$ZONE" \
      --quiet || true
  fi
}

trap cleanup_vm EXIT

if ! gsutil ls -b "gs://$BUCKET" >/dev/null 2>&1; then
  gsutil mb -p "$PROJECT" -l "$REGION" "gs://$BUCKET"
fi

gsutil -m rsync -r "$INPUT_DIR" "gs://$BUCKET/$INPUT_PREFIX"

gcloud compute instances create "$VM_NAME" \
  --project "$PROJECT" \
  --zone "$ZONE" \
  --machine-type "$MACHINE_TYPE" \
  --boot-disk-size "$DISK_SIZE" \
  --image-family "$IMAGE_FAMILY" \
  --image-project "$IMAGE_PROJECT" \
  --scopes "https://www.googleapis.com/auth/cloud-platform"

if [ -z "$IMAGE_TAR" ]; then
  IMAGE_TAR="result-myphoto-docker"
  nix build .#myphoto-docker -o "$IMAGE_TAR"
fi

if [ ! -f "$IMAGE_TAR" ]; then
  echo "Docker image tar not found: $IMAGE_TAR" >&2
  exit 1
fi

gcloud compute scp "$IMAGE_TAR" "$VM_NAME:~/myphoto-docker.tar" \
  --project "$PROJECT" \
  --zone "$ZONE"

REMOTE_SCRIPT="$(mktemp)"
cat > "$REMOTE_SCRIPT" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail

BUCKET="$1"
INPUT_PREFIX="$2"
OUTPUT_PREFIX="$3"
IMAGE_TAR="$4"

sudo apt-get update
sudo apt-get install -y ca-certificates curl gnupg

sudo install -m 0755 -d /etc/apt/keyrings
curl -fsSL https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo gpg --dearmor -o /etc/apt/keyrings/cloud.google.gpg
echo "deb [signed-by=/etc/apt/keyrings/cloud.google.gpg] http://packages.cloud.google.com/apt cloud-sdk main" \
  | sudo tee /etc/apt/sources.list.d/google-cloud-sdk.list >/dev/null

sudo apt-get update
sudo apt-get install -y google-cloud-cli docker.io
sudo systemctl enable --now docker

sudo mkdir -p /data/input /data/output
sudo chown -R "$USER":"$USER" /data

gsutil -m rsync -r "gs://$BUCKET/$INPUT_PREFIX" /data/input

sudo docker load -i "$IMAGE_TAR"
sudo docker run --rm \
  -v /data/input:/input:ro \
  -v /data/output:/output \
  myphoto:latest

gsutil -m rsync -r /data/output "gs://$BUCKET/$OUTPUT_PREFIX"
EOF

gcloud compute scp "$REMOTE_SCRIPT" "$VM_NAME:~/myphoto-remote.sh" \
  --project "$PROJECT" \
  --zone "$ZONE"

gcloud compute ssh "$VM_NAME" \
  --project "$PROJECT" \
  --zone "$ZONE" \
  --command "bash ~/myphoto-remote.sh '$BUCKET' '$INPUT_PREFIX' '$OUTPUT_PREFIX' ~/myphoto-docker.tar"

rm -f "$REMOTE_SCRIPT"

mkdir -p "$OUTPUT_DIR"
gsutil -m rsync -r "gs://$BUCKET/$OUTPUT_PREFIX" "$OUTPUT_DIR"

if [ "$KEEP_BUCKET" = "no" ]; then
  gsutil -m rm -r "gs://$BUCKET/$INPUT_PREFIX" || true
  gsutil -m rm -r "gs://$BUCKET/$OUTPUT_PREFIX" || true
fi
