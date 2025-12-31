#!/usr/bin/env bash
set -euo pipefail

help_msg() {
  cat <<EOF >&2
Usage: $0 [OPTIONS] IMG1 IMG2 ...
       $0 [OPTIONS] DIR

Options:
  --headless                                    Run in headless mode (with xvfb-run)
  --project-file FILE                           Use specific project file
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

if [ "$#" -lt 1 ] || [ "$1" == "--help" ]; then
  help_msg
  exit 1
fi

HEADLESS="false"
PREFIX=""
PROJECT_FILE=""
DO_NOT_ALIGN="false"
DO_PMAX="true"
DO_DMAP="true"
PMAX_OUTPUT=""
DMAP_OUTPUT=""
DMAP_FIXED_CONTRAST_THRESHOLD_PERCENTILE=""
DMAP_FIXED_CONTRAST_THRESHOLD_LEVEL="0.0016078169"
EXIT_ARG="-exitOnBatchScriptCompletion"
POSITIONAL=()
while [[ $# -gt 0 ]]; do
  key="$1"
  case $key in
    --headless)
      HEADLESS="true"
      shift # past argument
      ;;
    --project-file)
      PROJECT_FILE="$(readlink -f "$2")"
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
      EXIT_ARG=""
      shift # past argument
      ;;
    *)
      POSITIONAL+=("$1") # save it in an array for later
      shift # past argument
      ;;
  esac
done

if [ -z "$PREFIX" ]; then
  length="${#POSITIONAL[@]}"
  basenameFirstImage="$(basename "${POSITIONAL[0]}")"
  basenameLastImage="$(basename "${POSITIONAL[-1]}")"
  PREFIX="$(pwd)/${basenameFirstImage%%.*}_to_${basenameLastImage%%.*}_stack_of_${length}"
fi

# if [ -z "$PROJECT_FILE" ]; then
#   PROJECT_FILE="${PREFIX}_zerene-stack.zsp"
#   mkdir -p "$PROJECT_FILE"
# fi

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
  cat << EOF
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
  cat << EOF
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
      cat << EOF
              <DepthMapControl.UseFixedContrastThresholdPercentile value="true" />
              <DepthMapControl.ContrastThresholdPercentile value="$DMAP_FIXED_CONTRAST_THRESHOLD_PERCENTILE" />
              <DepthMapControl.UseFixedContrastThresholdLevel value="false" />
EOF
    fi
    if [ -n "$DMAP_FIXED_CONTRAST_THRESHOLD_LEVEL" ]; then
      cat << EOF
              <DepthMapControl.UseFixedContrastThresholdPercentile value="false" />
              <DepthMapControl.UseFixedContrastThresholdLevel value="true" />
              <DepthMapControl.ContrastThresholdLevel value="$DMAP_FIXED_CONTRAST_THRESHOLD_LEVEL" />
EOF
    fi
  fi
  cat << EOF
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

  cat << EOF
        <Tasks length="$task_length">
$taskPmax
$taskDmap
        </Tasks>
EOF
}

mkProject() {
  if [ -z "$PROJECT_FILE" ]; then
    cat << EOF
        <ProjectDispositionCode value="101" />
EOF
  else
    cat << EOF
        <ProjectDispositionCode value="103" />
        <ProjectFilePath value="$PROJECT_FILE" />
EOF
  fi
}

mkXml() {
  cat << EOF
<?xml version="1.0" encoding="UTF-8"?>
<ZereneStackerBatchScript>
  <BatchQueue>
    <Batches length="1">
      <Batch>
        <Sources length="1">
          <Source value="%CurrentProject%" />
        </Sources>
EOF
  mkProject
  mkTasks
  cat << EOF
      </Batch>
    </Batches>
  </BatchQueue>
</ZereneStackerBatchScript>
EOF
}

main() {
  # xml=$(mktemp "$TMP/zerene-batch-XXXXXX.xml")
  local xml="${PREFIX}_zerene_batch.xml"
  mkXml |tee "$xml"

  echo "DEBUG: $ zerene-stacker -batchScript $xml $EXIT_ARG ..." >&2
  if [ "$HEADLESS" == "true" ]; then
    echo "DEBUG: running in headless mode" >&2
    exec xvfb-run -a zerene-stacker -batchScript "$xml" $EXIT_ARG "${POSITIONAL[@]}"
  else 
    exec zerene-stacker -batchScript "$xml" $EXIT_ARG "${POSITIONAL[@]}"
  fi
}

main