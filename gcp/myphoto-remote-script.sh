#!/usr/bin/env bash
set -euo pipefail

INPUT_BUCKET_PATH="$1"
OUTPUT_BUCKET_PATH="$2"
IMAGE_TAR="$3"
LEVELS="${4:-2}"

sudo mkdir -p /data/input /data/workdir /data/output
sudo chown -R "$USER":"$USER" /data

exec &> >(tee -a /data/output/myphoto-gcp-remote-script.log)

sudo systemd-run --on-active=24h /sbin/shutdown -h now
sudo systemd-run --on-active=23h --unit myphoto-self-delete /bin/bash -c \
  'set +e
  instance_name="$(curl -fsS -H "Metadata-Flavor: Google" \
    "http://metadata/computeMetadata/v1/instance/name")"
  zone="$(curl -fsS -H "Metadata-Flavor: Google" \
    "http://metadata/computeMetadata/v1/instance/zone" | awk -F/ "{print \$NF}")"
  project="$(curl -fsS -H "Metadata-Flavor: Google" \
    "http://metadata/computeMetadata/v1/project/project-id")"
  gcloud compute instances delete "$instance_name" \
    --project "$project" \
    --zone "$zone" \
    --quiet >/tmp/myphoto-self-delete-timer.log 2>&1'

sudo apt-get update
sudo apt-get install -y ca-certificates curl gnupg

sudo install -m 0755 -d /etc/apt/keyrings
curl -fsSL https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo gpg --dearmor -o /etc/apt/keyrings/cloud.google.gpg
echo "deb [signed-by=/etc/apt/keyrings/cloud.google.gpg] http://packages.cloud.google.com/apt cloud-sdk main" \
  | sudo tee /etc/apt/sources.list.d/google-cloud-sdk.list >/dev/null

sudo apt-get update
sudo apt-get install -y google-cloud-cli docker.io
sudo systemctl enable --now docker

set -x

gsutil -m rsync -r "$INPUT_BUCKET_PATH" /data/input

if [[ "$IMAGE_TAR" == gs://* ]]; then
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

gsutil -m rsync -r /data/output "$OUTPUT_BUCKET_PATH"

if [ "${SELF_DELETE:-no}" = "yes" ]; then
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
fi
