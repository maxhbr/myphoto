#!/usr/bin/env bash
set -euo pipefail

#####################################
# Argument Parsing

usage() {
    cat <<'EOF'
Usage: 
  $0 \
    --input-dir <local-input-dir> \
    --output-dir <local-output-dir> \
    [--input-bucket <bucket-name>] \
    [--machine-type <type>] \
    [--disk-size <size>] \
    [--image-family <family>] \
    [--image-project <project>] \
    [--keep-vm] \
    [--keep-bucket] \
    [--detach]
  $0 --teardown <vm-name> [<input-bucket>]
  $0 --direct-download <vm-name> [<output-dir>]
EOF
}

# Read the following variables from `~/.myphoto/gcp.env`
if [ -f "$HOME/.myphoto/gcp.env" ]; then
    source "$HOME/.myphoto/gcp.env"
    if [ -z "$PROJECT" ] || [ -z "$REGION" ] || [ -z "$ZONE" ]; then
        echo "Missing required GCP parameters." >&2
        usage
        exit 1
    fi
else
    echo "GCP configuration file ~/.myphoto/gcp.env not found." >&2
    exit 1
fi

#####################################
# Early Mode Detection

MODE=""
VM_NAME=""
INPUT_BUCKET=""
OUTPUT_DIR=""

if [ $# -gt 0 ]; then
    case "$1" in
        --teardown)
            MODE="teardown"
            VM_NAME="${2:-}"
            INPUT_BUCKET="${3:-}"
            if [ -z "$VM_NAME" ]; then
                echo "Error: --teardown requires VM_NAME as argument" >&2
                usage
                exit 1
            fi
            set -x
            gcloud compute instances delete "$VM_NAME" \
                --project "$PROJECT" \
                --zone "$ZONE" \
                --quiet || true

            if [ -n "$INPUT_BUCKET" ]; then
                gsutil -m rm -r "$INPUT_BUCKET" || true
            fi
            exit 0
            ;;
        --direct-download)
            MODE="direct-download"
            VM_NAME="${2:-}"
            OUTPUT_DIR="${3:-.}"
            if [ -z "$VM_NAME" ]; then
                echo "Error: --direct-download requires VM_NAME as argument" >&2
                usage
                exit 1
            fi

            if ! gcloud compute instances describe "$VM_NAME" \
                --project="$PROJECT" \
                --zone="$ZONE" \
                --format="get(status)" 2>/dev/null | grep -q "RUNNING"; then
                echo "Error: VM $VM_NAME is no longer running" >&2
                exit 1
            fi

            mkdir -p "$OUTPUT_DIR"
            set -x
            gcloud compute scp --recurse "$VM_NAME:/data/workdir/*" "$OUTPUT_DIR/" \
                --project="$PROJECT" \
                --zone="$ZONE"
            exit 0
            ;;
    esac
fi

# End of Early Mode Detection
#####################################

MACHINE_TYPE="n2-standard-32"
DISK_SIZE="500GB"
IMAGE_FAMILY="debian-12"
IMAGE_PROJECT="debian-cloud"
KEEP_VM="no"
KEEP_BUCKET="no"
DETACH="no"

DOCKER_BUCKET="gs://myphoto-docker-images/"
DOCKER_TAR_PATH="${DOCKER_BUCKET}myphoto-docker.tar"

while [ $# -gt 0 ]; do
    case "$1" in
        --input-dir)
            INPUT_DIR="$2"
            shift 2
            ;;
        --output-dir)
            OUTPUT_DIR="$2"
            shift 2
            ;;
        --input-bucket)
            INPUT_BUCKET="$2"
            shift 2
            ;;
        --machine-type)
            MACHINE_TYPE="$2"
            shift 2
            ;;
        --disk-size)
            DISK_SIZE="$2"
            shift 2
            ;;
        --image-family)
            IMAGE_FAMILY="$2"
            shift 2
            ;;
        --image-project)
            IMAGE_PROJECT="$2"
            shift 2
            ;;
        --keep-vm)
            KEEP_VM="yes"
            shift 1
            ;;
        --keep-bucket)
            KEEP_BUCKET="yes"
            shift 1
            ;;
        --detach)
            DETACH="yes"
            shift 1
            ;;
        -h | --help)
            usage
            exit 0
            ;;
        *)
            echo "Unknown argument: $1" >&2
            usage
            exit 1
            ;;
    esac
done

if [ -z "$INPUT_BUCKET" ] && [ -z "$INPUT_DIR" ]; then
    echo "Either --input-bucket or --input-dir must be specified." >&2
    usage
    exit 1
fi
if [ -z "$OUTPUT_DIR" ]; then
    echo "Missing --output-dir parameter." >&2
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

# End of Argument Parsing
#####################################

#####################################
# Functions

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

upload_docker_tar() {
    if ! gsutil ls -b "$DOCKER_BUCKET" >/dev/null 2>&1; then
        gsutil mb -p "$PROJECT" -l "$REGION" "$DOCKER_BUCKET"
    fi

    if gsutil ls "$DOCKER_TAR_PATH" >/dev/null 2>&1; then
        LOCAL_HASH=$(md5sum "@MYPHOTO_DOCKER@" | awk '{print $1}')
        REMOTE_HASH=$(gsutil hash -h "$DOCKER_TAR_PATH" | grep -i md5 | awk '{print $2}')
        if [ "$LOCAL_HASH" = "$REMOTE_HASH" ]; then
            echo "Docker image already exists in bucket with matching hash, skipping upload"
            return
        else
            echo "Docker image hash mismatch, uploading new image"
        fi
    else
        echo "Docker image not found in bucket, uploading"
    fi
    gsutil cp "@MYPHOTO_DOCKER@" "$DOCKER_TAR_PATH"
}

create_vm() {
    gcloud compute instances create "$VM_NAME" \
        --project "$PROJECT" \
        --zone "$ZONE" \
        --machine-type "$MACHINE_TYPE" \
        --boot-disk-size "$DISK_SIZE" \
        --image-family "$IMAGE_FAMILY" \
        --image-project "$IMAGE_PROJECT" \
        --labels "run=$LABEL_VALUE,date=$DATE" \
        --scopes "https://www.googleapis.com/auth/cloud-platform"
}

provision() {
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
        cleanup_on_provision_failure
        exit 1
    fi
}

cleanup_on_provision_failure() {
    gcloud compute instances delete "$VM_NAME" \
        --project "$PROJECT" \
        --zone "$ZONE" \
        --quiet || true
    if [ -n "$INPUT_DIR" ]; then
        gsutil -m rm -r "$INPUT_BUCKET" || true
    fi
    gsutil rm "$DOCKER_TAR_PATH" || true
}

create_download_script() {
    local script_path="${OUTPUT_DIR}/${VM_NAME}.download.sh"
    cat >"$script_path" <<SCRIPT
#!/usr/bin/env bash
set -euo pipefail

VM_NAME="$VM_NAME"
PROJECT="$PROJECT"
ZONE="$ZONE"
OUTPUT_DIR="$OUTPUT_DIR"
OUTPUT_BUCKET_PATH="$OUTPUT_BUCKET_PATH"

if $(which gcloud) compute instances describe "\$VM_NAME" \
    --project="\$PROJECT" \
    --zone="\$ZONE" \
    --format="get(status)" 2>/dev/null | grep -q "RUNNING"; then
  echo "Error: VM \$VM_NAME is still running. Wait for the job to complete before downloading."
  exit 1
fi

mkdir -p "\$OUTPUT_DIR"
set -x
$(which gsutil) -m rsync -r "\$OUTPUT_BUCKET_PATH" "\$OUTPUT_DIR"
SCRIPT
    chmod +x "$script_path"
    echo "Download script created: $script_path"
}

create_direct_download_script() {
    local script_path="${OUTPUT_DIR}/${VM_NAME}.direct-download.sh"
    cat >"$script_path" <<SCRIPT
#!/usr/bin/env bash
set -euo pipefail

set -x
exec "$0" --direct-download "$VM_NAME" "${OUTPUT_DIR:-.}"
SCRIPT
    chmod +x "$script_path"
}

create_teardown_script() {
    local script_path="${OUTPUT_DIR}/${VM_NAME}.teardown.sh"
    cat >"$script_path" <<SCRIPT
#!/usr/bin/env bash
set -euo pipefail

set -x
exec "$0" --teardown "$VM_NAME" "${INPUT_BUCKET:-}"
SCRIPT
    chmod +x "$script_path"
}

run_execute() {
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

        times
    fi
}

# End of Functions
#####################################

######################################
# Run

main() {
    if [ -z "$INPUT_BUCKET" ]; then
        INPUT_BUCKET="gs://$VM_NAME-input/"
        set_up_bucket "$INPUT_BUCKET" "$INPUT_DIR"
    fi
    OUTPUT_BUCKET="gs://myphoto-output/"
    set_up_bucket "$OUTPUT_BUCKET"
    OUTPUT_BUCKET_PATH="${OUTPUT_BUCKET}${VM_NAME}/"

    upload_docker_tar

    set -x

    create_vm
    wait_for_ssh
    provision

    set +x

    mkdir -p "$OUTPUT_DIR"
    create_download_script
    create_direct_download_script
    create_teardown_script

    set -x

    run_execute
}

main "$@"
