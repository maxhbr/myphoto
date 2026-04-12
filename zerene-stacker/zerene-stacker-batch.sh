#!/usr/bin/env bash
set -euo pipefail

help_msg() {
    cat <<EOF >&2
Usage: $0 [OPTIONS] IMG1 IMG2 ...
       $0 [OPTIONS] DIR

Options:
  --project-folder DIR                          Save Zerene project to designated folder
  --already-aligned                             Skip alignment step
  --no-pmax                                     Disable PMax stacking method
  --no-dmap                                     Disable DMap stacking method
  --pmax-output FILE                            Override PMax output filename
  --dmap-output FILE                            Override DMap output filename
  --dmap-fixed-contrast-threshold-percentile N  Set DMap contrast threshold percentile
  --dmap-fixed-contrast-threshold-level N       Set DMap contrast threshold level (default: 0.0016078169)
  --wait                                        Wait for completion instead of exiting

By default, both PMax and DMap stacking methods are enabled.
EOF
}

if [ "$#" -lt 1 ]; then
    help_msg
    exit 1
fi
if [ "$1" == "--help" ]; then
    help_msg
    exit 0
fi

PROJECT_FOLDER=""
DO_NOT_ALIGN="false"
DO_PMAX="true"
DO_DMAP="true"
PMAX_OUTPUT=""
DMAP_OUTPUT=""
DMAP_FIXED_CONTRAST_THRESHOLD_PERCENTILE=""
DMAP_FIXED_CONTRAST_THRESHOLD_LEVEL="0.0016078169"
EXTRA_ARGS=("-exitOnBatchScriptCompletion")
POSITIONAL=()
while [[ $# -gt 0 ]]; do
    key="$1"
    case $key in
        --project-file)
            echo "WARNING: --project-file is deprecated and ignored (Zerene Stacker does not support ProjectFilePath in batch scripts)" >&2
            shift # past argument
            shift # past value
            ;;
        --project-folder)
            PROJECT_FOLDER="$(readlink -f "$2")"
            shift # past argument
            shift # past value
            ;;
        --already-aligned)
            DO_NOT_ALIGN="true"
            shift # past argument
            ;;
        --no-pmax)
            DO_PMAX="false"
            shift # past argument
            ;;
        --no-dmap)
            DO_DMAP="false"
            shift # past argument
            ;;
        --pmax-output)
            PMAX_OUTPUT="$(readlink -f "$2")"
            DO_PMAX="true"
            shift # past argument
            shift # past value
            ;;
        --dmap-output)
            DMAP_OUTPUT="$(readlink -f "$2")"
            DO_DMAP="true"
            shift # past argument
            shift # past value
            ;;
        --dmap-fixed-contrast-threshold-percentile)
            DMAP_FIXED_CONTRAST_THRESHOLD_PERCENTILE="$2"
            DMAP_FIXED_CONTRAST_THRESHOLD_LEVEL=""
            shift # past argument
            shift # past value
            ;;
        --dmap-fixed-contrast-threshold-level)
            DMAP_FIXED_CONTRAST_THRESHOLD_LEVEL="$2"
            DMAP_FIXED_CONTRAST_THRESHOLD_PERCENTILE=""
            shift # past argument
            shift # past value
            ;;
        --wait)
            EXTRA_ARGS=()
            shift # past argument
            ;;
        *)
            POSITIONAL+=("$1") # save it in an array for later
            shift              # past argument
            ;;
    esac
done

# --- Handle directory argument: expand to contained image files ---
if [[ ${#POSITIONAL[@]} -eq 1 && -d ${POSITIONAL[0]} ]]; then
    img_dir="$(readlink -f "${POSITIONAL[0]}")"
    POSITIONAL=()
    shopt -s nullglob
    for ext in jpg jpeg tif tiff png bmp; do
        for f in "$img_dir"/*."$ext" "$img_dir"/*."${ext^^}"; do
            POSITIONAL+=("$f")
        done
    done
    shopt -u nullglob
    if [[ ${#POSITIONAL[@]} -eq 0 ]]; then
        echo "ERROR: No image files found in directory: $img_dir" >&2
        exit 1
    fi
    # Sort for deterministic order
    mapfile -t POSITIONAL < <(printf '%s\n' "${POSITIONAL[@]}" | sort)
fi

# --- Input validation ---
if [[ ${#POSITIONAL[@]} -eq 0 ]]; then
    echo "ERROR: No input images specified" >&2
    help_msg
    exit 1
fi

for img in "${POSITIONAL[@]}"; do
    if [[ ! -f $img ]]; then
        echo "ERROR: File does not exist: $img" >&2
        exit 1
    fi
    case "${img,,}" in
        *.jpg | *.jpeg | *.tif | *.tiff | *.png | *.bmp) ;;
        *)
            echo "ERROR: Unrecognized image extension: $img" >&2
            exit 1
            ;;
    esac
done

# --- Compute output prefix ---
length="${#POSITIONAL[@]}"
basenameFirstImage="$(basename "${POSITIONAL[0]}")"
basenameLastImage="$(basename "${POSITIONAL[-1]}")"
PREFIX="$(pwd)/${basenameFirstImage%%.*}_to_${basenameLastImage%%.*}_stack_of_${length}"

mkTask() {
    local taskIndicatorCode="$1"
    local output="$2"
    local outputDir
    outputDir="$(dirname "$output")"
    local outputBasename
    outputBasename="$(basename "$output")"
    local outputImageNamingTemplate="${outputBasename%%.*}"
    local outputFileType="${output##*.}"
    local doNotAlign="${3:-false}"
    cat <<EOF
          <Task>
            <TaskIndicatorCode value="$taskIndicatorCode" />
            <OutputImageDispositionCode value="5" />
            <OutputImagesDesignatedFolder value="$outputDir" />
            <Preferences>
              <OutputImageNaming.Template value="$outputImageNamingTemplate" />
              <SaveImage.FileType value="$outputFileType" />
              <Slabbing.SaveImage.FileType value="tif" />
EOF
    if [ "$doNotAlign" = "true" ]; then
        cat <<EOF
              <AlignmentControl.AllowRotation value="false" />
              <AlignmentControl.AllowScale value="false" />
              <AlignmentControl.AllowShiftX value="false" />
              <AlignmentControl.AllowShiftY value="false" />
              <AlignmentControl.CorrectBrightness value="false" />
              <AlignmentControl.Order.Automatic value="false" />
EOF
    fi
    if [ "$taskIndicatorCode" = "2" ]; then
        if [ -n "$DMAP_FIXED_CONTRAST_THRESHOLD_PERCENTILE" ]; then
            cat <<EOF
              <DepthMapControl.UseFixedContrastThresholdPercentile value="true" />
              <DepthMapControl.ContrastThresholdPercentile value="$DMAP_FIXED_CONTRAST_THRESHOLD_PERCENTILE" />
              <DepthMapControl.UseFixedContrastThresholdLevel value="false" />
EOF
        fi
        if [ -n "$DMAP_FIXED_CONTRAST_THRESHOLD_LEVEL" ]; then
            cat <<EOF
              <DepthMapControl.UseFixedContrastThresholdPercentile value="false" />
              <DepthMapControl.UseFixedContrastThresholdLevel value="true" />
              <DepthMapControl.ContrastThresholdLevel value="$DMAP_FIXED_CONTRAST_THRESHOLD_LEVEL" />
EOF
        fi
    fi
    cat <<EOF
            </Preferences>
          </Task>
EOF
}

mkTasks() {
    local taskPmax=""
    local taskDmap=""
    local task_length=0
    if [ "$DO_PMAX" = "true" ]; then
        local pmax_output="${PMAX_OUTPUT:-${PREFIX}_zerene_PMax.tif}"
        taskPmax=$(mkTask "1" "$pmax_output" "$DO_NOT_ALIGN")
        task_length=$((task_length + 1))
    fi
    if [ "$DO_DMAP" = "true" ]; then
        local dmap_output="${DMAP_OUTPUT:-${PREFIX}_zerene_DMap.tif}"
        taskDmap=$(mkTask "2" "$dmap_output" "$DO_NOT_ALIGN")
        task_length=$((task_length + 1))
    fi

    if [ "$task_length" -eq 0 ]; then
        echo "ERROR: No tasks specified (neither PMax nor DMap)" >&2
        exit 1
    fi

    cat <<EOF
        <Tasks length="$task_length">
$taskPmax
$taskDmap
        </Tasks>
EOF
}

mkProject() {
    if [[ -z $PROJECT_FOLDER ]]; then
        cat <<EOF
        <ProjectDispositionCode value="101" />
EOF
    else
        cat <<EOF
        <ProjectDispositionCode value="103" />
        <ProjectsDesignatedFolder value="$PROJECT_FOLDER" />
EOF
    fi
}

mkXml() {
    local source_dir="$1"
    cat <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<ZereneStackerBatchScript>
  <BatchQueue>
    <Batches length="1">
      <Batch>
        <Sources length="1">
          <Source value="$source_dir" />
        </Sources>
EOF
    mkProject
    mkTasks
    cat <<EOF
      </Batch>
    </Batches>
  </BatchQueue>
</ZereneStackerBatchScript>
EOF
}

# Create a temporary directory containing symlinks to all input images.
# Zerene Stacker batch mode requires a directory source (not %CurrentProject%)
# for project saving (ProjectDispositionCode) to work correctly.
mkSourceDir() {
    local source_dir
    source_dir="$(mktemp -d "${PREFIX}_zerene_input.XXXXXX")"
    local counter=0
    for img in "${POSITIONAL[@]}"; do
        ((++counter))
        local numbered_name
        numbered_name=$(printf "%05d.%s" "$counter" "$(basename "$img")")
        ln -s "$img" "$source_dir/$numbered_name"
    done
    echo "$source_dir"
}

main() {
    local xml="${PREFIX}_zerene_batch"
    if [[ $DO_PMAX == "true" ]]; then
        xml="${xml}_PMax"
    fi
    if [[ $DO_DMAP == "true" ]]; then
        xml="${xml}_DMap"
    fi
    xml="${xml}.xml"

    local source_dir
    source_dir="$(mkSourceDir)"
    mkXml "$source_dir" | tee "$xml"

    local cmd=(zerene-stacker -batchScript "$xml" "${EXTRA_ARGS[@]}")
    echo "DEBUG: \$ ${cmd[*]}" >&2
    local rc=0
    "${cmd[@]}" || rc=$?

    # # Clean up the temporary symlink directory
    # rm -rf "$source_dir"

    # Verify expected outputs exist
    local missing=0
    if [[ $DO_PMAX == "true" ]]; then
        local pmax_output="${PMAX_OUTPUT:-${PREFIX}_zerene_PMax.tif}"
        if [[ ! -f $pmax_output ]]; then
            echo "WARNING: Expected PMax output not found: $pmax_output" >&2
            missing=1
        fi
    fi
    if [[ $DO_DMAP == "true" ]]; then
        local dmap_output="${DMAP_OUTPUT:-${PREFIX}_zerene_DMap.tif}"
        if [[ ! -f $dmap_output ]]; then
            echo "WARNING: Expected DMap output not found: $dmap_output" >&2
            missing=1
        fi
    fi

    if [[ $rc -ne 0 ]]; then
        echo "ERROR: zerene-stacker exited with code $rc" >&2
        exit "$rc"
    fi
    if [[ $missing -eq 1 ]]; then
        echo "ERROR: One or more expected output files were not produced" >&2
        exit 1
    fi
}

main
