# Run in GCP

This folder contains the harness for computation of images in GCP cloud.

## Requirements:

- up to 100GB of input images (up to 2000 images, either JPG or RAW)
  - stores everything multiple times, might use up to 300GB during computation
- requires up to 128GB of RAM
- requires a lot of CPU threads
- creates several GB of output
- runs for several hours

## Plan 
It does it in the following way:

### Step 1: create Bucket and upload provided images to folder
### Step 2: provision Single Compute Engine VM with Docker
*  Create a VM with 128 GB RAM and enough vCPUs.
*  Attach a sufficiently large SSD disk.
*  Install Docker, pull image.
### Step 3: pull images from Bucket to local directory
via `gsutil -m rsync`

after success, clean up bucket.
### Step 4: run myphoto-stack dockerized and write to output
### Step 5: upload result to bucket
### Step 6: tear down VM

## Harness
`gcp/run-myphoto-in-gcp.sh` provisions the VM, uploads inputs, runs the docker image, and downloads results.

### This is packaged via nix

```
$ nix run .#myphoto-docker-in-gcp -- ...
```

#### Example Call

```bash
nix run .#myphoto-docker-in-gcp -- \
  --project my-gcp-project \
  --region europe-west3 \
  --zone europe-west3-a \
  --bucket myphoto-processing-bucket \
  --input-dir ~/photos/stack-input \
  --output-dir ~/photos/stack-output
```




