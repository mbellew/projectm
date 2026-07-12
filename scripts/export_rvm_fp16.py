#!/usr/bin/env python3
"""Export an FP16 build of the RVM seg model for the CUDA box.

    pip install onnx onnxconverter_common
    ./scripts/export_rvm_fp16.py /opt/projectm/models/rvm_mobilenetv3.onnx

Writes <model>_fp16.onnx next to the input. Point "Video Seg Model" (or $PROJECTM_SEG_MODEL) at it.

Measured on the RTX 5060 @ 512: seg inference 10.3 ms -> 8.7 ms, model 14.6 MB -> 7.3 MB, matte
unchanged. See SEG_MASK_PERF.md.

Keep the FP32 model for machines without CUDA: ONNX Runtime's CPU EP has poor fp16 kernels (it
largely casts back to fp32 internally), and CoreML already computes in fp16 on the ANE regardless of
the file's dtype. FP16 is a CUDA-path optimization, not a universal one.

Two things here are NOT obvious, and both cost real debugging time:

1. `keep_io_types=True` produces a model ORT REFUSES TO LOAD.
   RVM's recurrent state tensors (r1o..r4o) are graph outputs AND internal tensors -- each ConvGRU
   feeds its new hidden state forward within the same frame. keep_io_types casts them to fp32 for the
   boundary, which then poisons their internal consumers:
       Type Error: Type parameter (T) of Optype (Concat) bound to different types
       (tensor(float16) and tensor(float)) in node (Concat_188)
   Converting fully to fp16 and re-floating only the boundaries (below) sidesteps this entirely.

2. src and pha must stay FP32; the recurrent state must NOT.
   - src / pha are the only tensors the CPU touches. Converting src on the CPU would mean 786k
     float->half conversions per frame (3*512*512), which costs more than the fp16 win. One GPU Cast
     at each boundary is far cheaper.
   - r1i..r4o and downsample_ratio live on the device and never touch the CPU, so they go native
     fp16: no per-frame casts, half the state bandwidth. (Leaving casts on the recurrent inputs also
     trips an ORT buffer-reuse bug, because the state changes shape between frame 1's 1x1x1x1 zeros
     and frame 2's real shapes: "Shape mismatch attempting to re-use buffer".)

SegMasker detects the fp16 export from r1i's declared type and binds the zero-init state and
downsample_ratio as half accordingly; no config change is needed beyond the model path.
"""

import os
import sys

import onnx
from onnx import TensorProto, helper
from onnxconverter_common import float16


def relax_input_to_fp32(graph, name: str) -> None:
    """Make graph input `name` fp32 again, feeding an inserted Cast->fp16 for the rest of the graph."""
    value_info = next(i for i in graph.input if i.name == name)
    value_info.type.tensor_type.elem_type = TensorProto.FLOAT
    inner = f"{name}_fp16"
    for node in graph.node:
        for k, inp in enumerate(node.input):
            if inp == name:
                node.input[k] = inner
    graph.node.insert(0, helper.make_node("Cast", [name], [inner],
                                          name=f"cast_{name}_to_fp16", to=TensorProto.FLOAT16))


def relax_output_to_fp32(graph, name: str) -> None:
    """Make graph output `name` fp32 again, via a Cast appended after its fp16 producer."""
    value_info = next(o for o in graph.output if o.name == name)
    value_info.type.tensor_type.elem_type = TensorProto.FLOAT
    inner = f"{name}_fp16"
    for node in graph.node:
        for k, out in enumerate(node.output):
            if out == name:
                node.output[k] = inner
    graph.node.append(helper.make_node("Cast", [inner], [name],
                                       name=f"cast_{name}_to_fp32", to=TensorProto.FLOAT))


def main() -> int:
    if len(sys.argv) != 2:
        print(__doc__)
        return 2
    src = sys.argv[1]
    dst = os.path.splitext(src)[0] + "_fp16.onnx"

    model = float16.convert_float_to_float16(onnx.load(src), keep_io_types=False)
    relax_input_to_fp32(model.graph, "src")   # the CPU writes this (RgbToChw)
    relax_output_to_fp32(model.graph, "pha")  # the CPU reads this (compositing)
    onnx.checker.check_model(model)
    onnx.save(model, dst)

    names = {i.name: TensorProto.DataType.Name(i.type.tensor_type.elem_type) for i in model.graph.input}
    names.update({o.name: TensorProto.DataType.Name(o.type.tensor_type.elem_type) for o in model.graph.output})
    print(f"wrote {dst} ({os.path.getsize(dst) // 1024} KB, from {os.path.getsize(src) // 1024} KB)")
    print("  " + ", ".join(f"{k}={v}" for k, v in names.items()))
    return 0


if __name__ == "__main__":
    sys.exit(main())
