#!/usr/bin/env bash
set -euo pipefail

TIMEOUT="${TIMEOUT:-24}"

sudo mkdir -p /data/input /data/workdir /data/output
sudo chown -R "$USER":"$USER" /data

exec &> >(tee -a /data/output/myphoto-gcp-provision.log)

cat <<EOF
  TIMEOUT: $TIMEOUT (h)
EOF

sudo systemd-run --on-active=$((TIMEOUT + 1))h /sbin/shutdown -h now
sudo systemd-run --on-active=${TIMEOUT}h --unit myphoto-self-delete /bin/bash -c \
    'set +e
  instance_name="$(curl -fsS -H "Metadata-Flavor: Google" \
    "http://metadata/computeMetadata/v1/instance/name")"
  zone="$(curl -fsS -H "Metadata-Flavor: Google" \
    "http://metadata/computeMetadata/v1/instance/zone" | awk -F/ "{print $NF}")"
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
echo "deb [signed-by=/etc/apt/keyrings/cloud.google.gpg] http://packages.cloud.google.com/apt cloud-sdk main" |
    sudo tee /etc/apt/sources.list.d/google-cloud-sdk.list >/dev/null

sudo apt-get update
sudo apt-get install -y google-cloud-cli docker.io
sudo systemctl enable --now docker

echo "Provisioning complete"
