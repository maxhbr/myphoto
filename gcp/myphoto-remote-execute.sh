#!/usr/bin/env bash
set -euo pipefail

INPUT_BUCKET="$1"
OUTPUT_BUCKET="$2"
IMAGE_TAR="$3"
LEVELS="${4:-2}"

exec &> >(tee -a /data/output/myphoto-gcp-execute.log)

cat <<EOF
 INPUT_BUCKET: $INPUT_BUCKET
 OUTPUT_BUCKET: $OUTPUT_BUCKET
       IMAGE_TAR: $IMAGE_TAR
          LEVELS: $LEVELS
EOF

set -x

gsutil -m rsync -r "$INPUT_BUCKET" /data/input

gsutil -m rm -r "$INPUT_BUCKET" || true

if [[ $IMAGE_TAR == gs://* ]]; then
    gsutil cp "$IMAGE_TAR" .
    IMAGE_TAR="$(basename "$IMAGE_TAR")"
fi
sudo docker load -i "$IMAGE_TAR"

if [ "$LEVELS" -gt 0 ]; then
    sudo docker run --rm \
        -e LANG=C.UTF-8 \
        -e LC_ALL=C.UTF-8 \
        -v /data/input:/input:ro \
        -v /data/workdir:/output \
        myphoto:latest

    find /data/workdir -maxdepth "$LEVELS" -type f -print0 |
        while IFS= read -r -d '' file; do
            dest="/data/output/$(basename "$file")"
            mkdir -p "$(dirname "$dest")"
            cp "$file" "$dest"
        done
else
    sudo docker run --rm \
        -e LANG=C.UTF-8 \
        -e LC_ALL=C.UTF-8 \
        -v /data/input:/input:ro \
        -v /data/output:/output \
        myphoto:latest
fi

gsutil -m rsync -r /data/output "$OUTPUT_BUCKET"

(
    set +e
    instance_name="$(curl -fsS -H "Metadata-Flavor: Google" \
        "http://metadata/computeMetadata/v1/instance/name")"
    zone="$(curl -fsS -H "Metadata-Flavor: Google" \
        "http://metadata/computeMetadata/v1/instance/zone" | awk -F/ '{print $NF}')"
    project="$(curl -fsS -H "Metadata-Flavor: Google" \
        "http://metadata/computeMetadata/v1/project/project-id")"
    nohup gcloud compute instances delete "$instance_name" \
        --project "$project" \
        --zone "$zone" \
        --quiet >/tmp/myphoto-self-delete.log 2>&1 &
)
