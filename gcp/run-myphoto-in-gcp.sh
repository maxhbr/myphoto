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
  [--machine-type <type>] \
  [--disk-size <size>] \
  [--image-family <family>] \
  [--image-project <project>] \
  [--input-prefix <prefix>] \
  [--output-prefix <prefix>] \
  [--keep-vm] \
  [--keep-bucket] \
  [--detach]
EOF
}

PROJECT=""
REGION=""
ZONE=""
INPUT_DIR=""
OUTPUT_DIR=""
INPUT_BUCKET=""

MACHINE_TYPE="n2-standard-32"
DISK_SIZE="500GB"
IMAGE_FAMILY="debian-12"
IMAGE_PROJECT="debian-cloud"
KEEP_VM="no"
KEEP_BUCKET="no"
DETACH="no"

while [ $# -gt 0 ]; do
  case "$1" in
    --project) PROJECT="$2"; shift 2 ;;
    --region) REGION="$2"; shift 2 ;;
    --zone) ZONE="$2"; shift 2 ;;
    --input-dir) INPUT_DIR="$2"; shift 2 ;;
    --output-dir) OUTPUT_DIR="$2"; shift 2 ;;
    --input-bucket) INPUT_BUCKET="$2"; shift 2 ;;
    --machine-type) MACHINE_TYPE="$2"; shift 2 ;;
    --disk-size) DISK_SIZE="$2"; shift 2 ;;
    --image-family) IMAGE_FAMILY="$2"; shift 2 ;;
    --image-project) IMAGE_PROJECT="$2"; shift 2 ;;
    --keep-vm) KEEP_VM="yes"; shift 1 ;;
    --keep-bucket) KEEP_BUCKET="yes"; shift 1 ;;
    --detach) DETACH="yes"; shift 1 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown argument: $1" >&2; usage; exit 1 ;;
  esac
done

if [ -z "$PROJECT" ] || [ -z "$REGION" ] || [ -z "$ZONE" ]; then
  echo "Missing required GCP parameters." >&2
  usage
  exit 1
fi

if [ -z "$INPUT_BUCKET" ] && [ -z "$INPUT_DIR" ]; then
  echo "Either --input-bucket or --input-dir must be specified." >&2
  usage
  exit 1
fi
if [ -z "$OUTPUT_DIR" ]; then
  echo "Missing input/output directories." >&2
  usage
  exit 1
fi

if [ -n "$INPUT_DIR" ] && [ ! -d "$INPUT_DIR" ]; then
  echo "Input directory does not exist: $INPUT_DIR" >&2
  exit 1
fi

if [ -n "$INPUT_DIR" ]; then
  INPUT_DIR="$(cd "$INPUT_DIR" && pwd)"
fi

mkdir -p "$OUTPUT_DIR"
if [ -n "$OUTPUT_DIR" ]; then
  OUTPUT_DIR="$(cd "$OUTPUT_DIR" && pwd)"
fi

wait_for_ssh() (
  set +x
  local max_attempts=30
  local attempt=1
  while [ "$attempt" -le "$max_attempts" ]; do
    if gcloud compute ssh "$VM_NAME" \
      --project "$PROJECT" \
      --zone "$ZONE" \
      --command "true" \
      --ssh-flag "-o ConnectionAttempts=1" \
      --ssh-flag "-o ConnectTimeout=5" >/dev/null 2>&1; then
      return 0
    fi
    echo "Waiting for SSH on $VM_NAME (attempt $attempt/$max_attempts)..."
    sleep 10
    attempt=$((attempt + 1))
  done
  echo "SSH did not become available on $VM_NAME" >&2
  return 1
)

set_up_bucket() {
  local bucket_name="$1"
  local initial_content="${2:-}"
  if ! gsutil ls -b "$bucket_name" >/dev/null 2>&1; then
    gsutil mb -p "$PROJECT" -l "$REGION" "$bucket_name"
    gcloud storage buckets update --clear-soft-delete "$bucket_name" || true
  else
    echo "Bucket already exists: $bucket_name"
  fi
  if [ -n "$initial_content" ]; then
    # only input gets label
    gcloud storage buckets update "$bucket_name" \
      --project "$PROJECT" \
      --update-labels "run=$LABEL_VALUE,date=$DATE"

    gsutil -m rsync -r "$initial_content" "$bucket_name"
  fi
}

if [ -n "$INPUT_DIR" ]; then
  SIZE=$(find "$INPUT_DIR" -type f | wc -l)
else
  SIZE=gs
fi
DATE=$(date +%Y%m%d-%H%M%S)
VM_NAME="myphoto-$DATE-$SIZE"
LABEL_VALUE="$(printf '%s' "$VM_NAME" | tr '[:upper:]' '[:lower:]' | sed -E 's/[^a-z0-9_-]+/-/g; s/^-+//; s/-+$//')"
if [ -z "$LABEL_VALUE" ]; then
  LABEL_VALUE="run"
fi

if [ -z "$INPUT_BUCKET" ]; then
  INPUT_BUCKET="gs://$VM_NAME-input/"
  set_up_bucket "$INPUT_BUCKET" "$INPUT_DIR"
fi
OUTPUT_BUCKET="gs://myphoto-output/"
set_up_bucket "$OUTPUT_BUCKET"
OUTPUT_BUCKET_PATH="${OUTPUT_BUCKET}${VM_NAME}/"

DOCKER_BUCKET="gs://myphoto-docker-images/"
if ! gsutil ls -b "$DOCKER_BUCKET" >/dev/null 2>&1; then
  gsutil mb -p "$PROJECT" -l "$REGION" "$DOCKER_BUCKET"
fi
DOCKER_TAR_PATH="${DOCKER_BUCKET}${VM_NAME}-myphoto-docker.tar"
gsutil cp "@MYPHOTO_DOCKER@" "$DOCKER_TAR_PATH"

set -x


gcloud compute instances create "$VM_NAME" \
  --project "$PROJECT" \
  --zone "$ZONE" \
  --machine-type "$MACHINE_TYPE" \
  --boot-disk-size "$DISK_SIZE" \
  --image-family "$IMAGE_FAMILY" \
  --image-project "$IMAGE_PROJECT" \
  --labels "run=$LABEL_VALUE,date=$DATE" \
  --scopes "https://www.googleapis.com/auth/cloud-platform"

wait_for_ssh

gcloud compute scp "@REMOTE_PROVISION@" "$VM_NAME:~/myphoto-remote-provision.sh" \
  --project "$PROJECT" \
  --zone "$ZONE"

gcloud compute scp "@REMOTE_EXECUTE@" "$VM_NAME:~/myphoto-remote-execute.sh" \
  --project "$PROJECT" \
  --zone "$ZONE"

if ! gcloud compute ssh "$VM_NAME" \
    --project "$PROJECT" \
    --zone "$ZONE" \
    --command "bash ~/myphoto-remote-provision.sh"; then
  echo "Provisioning failed, cleaning up..."
  gcloud compute instances delete "$VM_NAME" \
    --project "$PROJECT" \
    --zone "$ZONE" \
    --quiet || true
  if [ -n "$INPUT_DIR" ]; then
    gsutil -m rm -r "$INPUT_BUCKET" || true
  fi
  gsutil rm "$DOCKER_TAR_PATH" || true
  exit 1
fi

mkdir -p "\$OUTPUT_DIR"
$(which gsutil) -m rsync -r "\$OUTPUT_BUCKET_PATH" "\$OUTPUT_DIR"
EOF
chmod +x "$DOWNLOAD_SCRIPT"
echo "Download script created: $DOWNLOAD_SCRIPT"

TEARDOWN_SCRIPT="${OUTPUT_DIR}/teardown.${VM_NAME}.sh"
cat > "$TEARDOWN_SCRIPT" <<EOF
#!/usr/bin/env bash
set -euo pipefail

VM_NAME="$VM_NAME"
PROJECT="$PROJECT"
ZONE="$ZONE"
INPUT_BUCKET="$INPUT_BUCKET"

echo "cleaning up VM: \$VM_NAME"
$(which gcloud) compute instances delete "\$VM_NAME" \
  --project "\$PROJECT" \
  --zone "\$ZONE" \
  --quiet || true

$(which gsutil) -m rm -r "$INPUT_BUCKET" || true
EOF
chmod +x "$TEARDOWN_SCRIPT"

set -x

gcloud compute instances create "$VM_NAME" \
  --project "$PROJECT" \
  --zone "$ZONE" \
  --machine-type "$MACHINE_TYPE" \
  --boot-disk-size "$DISK_SIZE" \
  --image-family "$IMAGE_FAMILY" \
  --image-project "$IMAGE_PROJECT" \
  --labels "run=$LABEL_VALUE,date=$DATE" \
  --scopes "https://www.googleapis.com/auth/cloud-platform"

wait_for_ssh

gcloud compute scp "@MYPHOTO_DOCKER@" "$VM_NAME:~/myphoto-docker.tar" \
  --project "$PROJECT" \
  --zone "$ZONE"

gcloud compute scp "@REMOTE_PROVISION@" "$VM_NAME:~/myphoto-remote-provision.sh" \
  --project "$PROJECT" \
  --zone "$ZONE"

gcloud compute scp "@REMOTE_EXECUTE@" "$VM_NAME:~/myphoto-remote-execute.sh" \
  --project "$PROJECT" \
  --zone "$ZONE"

if ! gcloud compute ssh "$VM_NAME" \
    --project "$PROJECT" \
    --zone "$ZONE" \
    --command "bash ~/myphoto-remote-provision.sh"; then
  echo "Provisioning failed, cleaning up..."
  gcloud compute instances delete "$VM_NAME" \
    --project "$PROJECT" \
    --zone "$ZONE" \
    --quiet || true
  if [ -n "$INPUT_DIR" ]; then
    gsutil -m rm -r "$INPUT_BUCKET" || true
  fi
  exit 1
fi

if [ "$DETACH" = "yes" ]; then
  gcloud compute ssh "$VM_NAME" \
    --project "$PROJECT" \
    --zone "$ZONE" \
    --command "nohup bash -c \"bash ~/myphoto-remote-execute.sh '$INPUT_BUCKET' '$OUTPUT_BUCKET_PATH' '$DOCKER_TAR_PATH' no\" > /dev/null 2>&1 &"
  echo "Detached execution started."
  echo "Output will be available at: $OUTPUT_BUCKET_PATH"
  
else
  gcloud compute ssh "$VM_NAME" \
    --project "$PROJECT" \
    --zone "$ZONE" \
    --command "bash ~/myphoto-remote-execute.sh '$INPUT_BUCKET' '$OUTPUT_BUCKET_PATH' '$DOCKER_TAR_PATH' no"

  if [ "$KEEP_VM" = "no" ]; then
    echo "cleaning up VM: $VM_NAME"
    gcloud compute instances delete "$VM_NAME" \
      --project "$PROJECT" \
      --zone "$ZONE" \
      --quiet || true
  fi

  if [ -n "$INPUT_DIR" ] && [ "$KEEP_BUCKET" = "no" ]; then
    echo "cleaning up input bucket: $INPUT_BUCKET"
    gsutil -m rm -r "$INPUT_BUCKET" || true
  fi

  mkdir -p "$OUTPUT_DIR"
  gsutil -m rsync -r "$OUTPUT_BUCKET_PATH" "$OUTPUT_DIR"

  gsutil rm "$DOCKER_TAR_PATH" || true

  times
fi
