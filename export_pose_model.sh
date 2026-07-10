#!/usr/bin/env bash
# Export a YOLO-pose model to ONNX for the SDL test UI's pose→touch bridge.
#
# Produces a COCO-17-keypoint pose model (wrists/elbows/shoulders — everything the
# pose→touch score needs). The app loads it via the "Video Pose Model" config key or
# the $PROJECTM_POSE_MODEL env var; the default search path is
#   ~/.projectM/models/yolo11n-pose.onnx
#
# Requirements: python3 with the ultralytics package (pip install ultralytics).
# The first run downloads the .pt weights from Ultralytics, then exports ONNX.
#
# Usage:
#   ./export_pose_model.sh              # yolo11n-pose (nano, fastest) -> ~/.projectM/models
#   ./export_pose_model.sh yolo11s-pose # small (more accurate, slower)
#   MODELS_DIR=/somewhere ./export_pose_model.sh
set -euo pipefail

MODEL="${1:-yolo11n-pose}"
MODELS_DIR="${MODELS_DIR:-$HOME/.projectM/models}"
IMGSZ="${IMGSZ:-640}"      # square input; 640 is the YOLO-pose default
OPSET="${OPSET:-12}"       # opset 12 is broadly compatible with onnxruntime + CoreML

mkdir -p "$MODELS_DIR"

if ! python3 -c "import ultralytics" 2>/dev/null; then
    echo "ultralytics not found. Install it with:  pip install ultralytics" >&2
    exit 1
fi

echo "Exporting ${MODEL} (imgsz=${IMGSZ}, opset=${OPSET}) to ONNX…"
# Export in a temp cwd so the downloaded .pt and .onnx land together, then move the onnx.
workdir="$(mktemp -d)"
trap 'rm -rf "$workdir"' EXIT
(
    cd "$workdir"
    yolo export model="${MODEL}.pt" format=onnx opset="${OPSET}" imgsz="${IMGSZ}" simplify=True
)

src="$workdir/${MODEL}.onnx"
dst="$MODELS_DIR/${MODEL}.onnx"
if [[ ! -f "$src" ]]; then
    echo "Export failed: $src not produced." >&2
    exit 1
fi
mv -f "$src" "$dst"
echo "Wrote $dst"
echo
echo "Enable it in config.inp (or the environment):"
echo "  Video Pose Model = $dst"
echo "  # or:  export PROJECTM_POSE_MODEL=\"$dst\""
