#include "PerFrameContext.hpp"

#include "MilkdropPresetExceptions.hpp"
#include "PaletteEvalFunctions.hpp"
#include "PoseEvalFunctions.hpp"

#include <Logging.hpp>

#define REG_VAR(var) \
    var = projectm_eval_context_register_variable(perFrameCodeContext, #var);

namespace libprojectM {
namespace MilkdropPreset {

PerFrameContext::PerFrameContext(projectm_eval_mem_buffer gmegabuf, PRJM_EVAL_F (*globalRegisters)[100], const Palette* palette, const Renderer::PoseState* pose)
    : perFrameCodeContext(projectm_eval_context_create(gmegabuf, globalRegisters))
{
    RegisterPaletteFunctions(perFrameCodeContext, palette);
    RegisterPoseFunctions(perFrameCodeContext, pose);
}

PerFrameContext::~PerFrameContext()
{
    if (perFrameCodeHandle != nullptr)
    {
        projectm_eval_code_destroy(perFrameCodeHandle);
    }

    if (perFrameCodeContext != nullptr)
    {
        projectm_eval_context_destroy(perFrameCodeContext);
    }
}

void PerFrameContext::RegisterBuiltinVariables()
{
    projectm_eval_context_reset_variables(perFrameCodeContext);
    // Constants must be re-set AFTER the reset: it zeroes every registered variable.
    RegisterPoseConstants(perFrameCodeContext);

    REG_VAR(zoom);
    REG_VAR(zoomexp);
    REG_VAR(rot);
    REG_VAR(warp);
    REG_VAR(cx);
    REG_VAR(cy);
    REG_VAR(dx);
    REG_VAR(dy);
    REG_VAR(sx);
    REG_VAR(sy);
    REG_VAR(time);
    REG_VAR(fps);
    REG_VAR(bass);
    REG_VAR(mid);
    REG_VAR(treb);
    REG_VAR(bass_att);
    REG_VAR(mid_att);
    REG_VAR(treb_att);
    REG_VAR(beat_phase);
    REG_VAR(beat_onset);
    REG_VAR(beat_bpm);
    REG_VAR(beat_conf);
    REG_VAR(seg_cx);
    REG_VAR(seg_cy);
    REG_VAR(seg_vx);
    REG_VAR(seg_vy);
    REG_VAR(seg_coverage);
    REG_VAR(seg_valid);
    REG_VAR(touch_on);
    REG_VAR(touch_x);
    REG_VAR(touch_y);
    REG_VAR(touch_pressure);
    REG_VAR(touch_vx);
    REG_VAR(touch_vy);
    REG_VAR(pose_valid);
    REG_VAR(pose_hands_apart);
    REG_VAR(pose_hands_together);
    REG_VAR(pose_hands_height);
    REG_VAR(pose_arm_span);
    REG_VAR(pose_lunge);
    REG_VAR(preset_complete);
    REG_VAR(frame);
    REG_VAR(decay);
    REG_VAR(wave_a);
    REG_VAR(wave_av); /*FLOATBUF*/
    REG_VAR(wave_r);
    REG_VAR(wave_g);
    REG_VAR(wave_b);
    REG_VAR(wave_x);
    REG_VAR(wave_y);
    REG_VAR(wave_mystery);
    REG_VAR(wave_mode);
    for (int q = 0; q < QVarCount; q++)
    {
        std::string qvar = "q" + std::to_string(q + 1);
        q_vars[q] = projectm_eval_context_register_variable(perFrameCodeContext, qvar.c_str());
    }
    REG_VAR(progress);
    REG_VAR(ob_size);
    REG_VAR(ob_r);
    REG_VAR(ob_g);
    REG_VAR(ob_b);
    REG_VAR(ob_a);
    REG_VAR(ob_av); /*FLOATBUF*/
    REG_VAR(ib_size);
    REG_VAR(ib_r);
    REG_VAR(ib_g);
    REG_VAR(ib_b);
    REG_VAR(ib_a);
    REG_VAR(ib_av); /*FLOATBUF*/
    REG_VAR(mv_x);
    REG_VAR(mv_y);
    REG_VAR(mv_dx);
    REG_VAR(mv_dy);
    REG_VAR(mv_l);
    REG_VAR(mv_r);
    REG_VAR(mv_g);
    REG_VAR(mv_b);
    REG_VAR(mv_a);
    REG_VAR(echo_zoom);
    REG_VAR(echo_alpha);
    REG_VAR(echo_orient);
    REG_VAR(wave_usedots);
    REG_VAR(wave_thick);
    REG_VAR(wave_additive);
    REG_VAR(wave_brighten);
    REG_VAR(darken_center);
    REG_VAR(gamma);
    REG_VAR(wrap);
    REG_VAR(invert);
    REG_VAR(brighten);
    REG_VAR(darken);
    REG_VAR(solarize);
    REG_VAR(meshx);
    REG_VAR(meshy);
    REG_VAR(pixelsx);
    REG_VAR(pixelsy);
    REG_VAR(aspectx);
    REG_VAR(aspecty);
    REG_VAR(blur1_min);
    REG_VAR(blur2_min);
    REG_VAR(blur3_min);
    REG_VAR(blur1_max);
    REG_VAR(blur2_max);
    REG_VAR(blur3_max);
    REG_VAR(blur1_edge_darken);
    REG_VAR(video_alpha_mode);
    REG_VAR(video_alpha_value);
    REG_VAR(video_alpha_init);
    REG_VAR(video_alpha_decay);
    REG_VAR(video_cleanup);
    REG_VAR(video_refine);
}

void PerFrameContext::EvaluateInitCode(PresetState& state)
{
    if (state.perFrameInitCode.empty())
    {
        return;
    }

    auto* initCode = projectm_eval_code_compile(perFrameCodeContext, state.perFrameInitCode.c_str());
    if (initCode == nullptr)
    {
        std::string error;
        int line;
        int col;
        auto* errmsg = projectm_eval_get_error(perFrameCodeContext, &line, &col);
        if (errmsg)
        {
            error = "[PerFrameContext] Could not compile per-frame INIT code: ";
            error += errmsg;
            error += "(L" + std::to_string(line) + " C" + std::to_string(col) + ")";
        }
        else
        {
            error = "[PerFrameContext] Could not compile per-frame init code.";
        }
        LOG_DEBUG("[PerFrameContext] Failed per-frame INIT code:\n" + state.perFrameInitCode);
        throw MilkdropCompileException(error);
    }

    projectm_eval_code_execute(initCode);
    projectm_eval_code_destroy(initCode);

    for (int q = 0; q < QVarCount; q++)
    {
        q_values_after_init_code[q] = *q_vars[q];
        state.frameQVariables[q] = *q_vars[q];
    }

    // Persist video parameters set in the per-frame init code back to the state, so they become
    // the per-frame defaults (LoadStateVariables reseeds them from state every frame). Without
    // this, values set in the init section would be overwritten by the defaults on the first frame.
    state.videoAlphaMode = static_cast<float>(*video_alpha_mode);
    state.videoAlphaValue = static_cast<float>(*video_alpha_value);
    state.videoAlphaInit = static_cast<float>(*video_alpha_init);
    state.videoAlphaDecay = static_cast<float>(*video_alpha_decay);
    state.videoCleanup = static_cast<float>(*video_cleanup);
    state.videoRefine = static_cast<float>(*video_refine);
}

void PerFrameContext::LoadStateVariables(PresetState& state)
{
    *zoom = static_cast<PRJM_EVAL_F>(state.zoom);
    *zoomexp = static_cast<PRJM_EVAL_F>(state.zoomExponent);
    *rot = static_cast<PRJM_EVAL_F>(state.rot);
    *warp = static_cast<PRJM_EVAL_F>(state.warpAmount);
    *cx = static_cast<PRJM_EVAL_F>(state.rotCX);
    *cy = static_cast<PRJM_EVAL_F>(state.rotCY);
    *dx = static_cast<PRJM_EVAL_F>(state.xPush);
    *dy = static_cast<PRJM_EVAL_F>(state.yPush);
    *sx = static_cast<PRJM_EVAL_F>(state.stretchX);
    *sy = static_cast<PRJM_EVAL_F>(state.stretchY);
    *time = static_cast<PRJM_EVAL_F>(state.renderContext.time);
    *fps = static_cast<PRJM_EVAL_F>(state.renderContext.fps);
    *bass = static_cast<PRJM_EVAL_F>(state.audioData.bass);
    *mid = static_cast<PRJM_EVAL_F>(state.audioData.mid);
    *treb = static_cast<PRJM_EVAL_F>(state.audioData.treb);
    *bass_att = static_cast<PRJM_EVAL_F>(state.audioData.bassAtt);
    *mid_att = static_cast<PRJM_EVAL_F>(state.audioData.midAtt);
    *treb_att = static_cast<PRJM_EVAL_F>(state.audioData.trebAtt);
    *beat_phase = static_cast<PRJM_EVAL_F>(state.audioData.beatPhase);
    *beat_onset = static_cast<PRJM_EVAL_F>(state.audioData.beatOnset);
    *beat_bpm = static_cast<PRJM_EVAL_F>(state.audioData.bpm);
    *beat_conf = static_cast<PRJM_EVAL_F>(state.audioData.beatConf);
    *seg_cx = static_cast<PRJM_EVAL_F>(state.renderContext.segCx);
    *seg_cy = static_cast<PRJM_EVAL_F>(state.renderContext.segCy);
    *seg_vx = static_cast<PRJM_EVAL_F>(state.renderContext.segVx);
    *seg_vy = static_cast<PRJM_EVAL_F>(state.renderContext.segVy);
    *seg_coverage = static_cast<PRJM_EVAL_F>(state.renderContext.segCoverage);
    *seg_valid = static_cast<PRJM_EVAL_F>(state.renderContext.segValid);
    *touch_on = static_cast<PRJM_EVAL_F>(state.renderContext.touchOn);
    *touch_x = static_cast<PRJM_EVAL_F>(state.renderContext.touchX);
    *touch_y = static_cast<PRJM_EVAL_F>(state.renderContext.touchY);
    *touch_pressure = static_cast<PRJM_EVAL_F>(state.renderContext.touchPressure);
    *touch_vx = static_cast<PRJM_EVAL_F>(state.renderContext.touchVx);
    *touch_vy = static_cast<PRJM_EVAL_F>(state.renderContext.touchVy);
    *pose_valid = static_cast<PRJM_EVAL_F>(state.renderContext.pose.valid);
    *pose_hands_apart = static_cast<PRJM_EVAL_F>(state.renderContext.pose.handsApart);
    *pose_hands_together = static_cast<PRJM_EVAL_F>(state.renderContext.pose.handsTogether);
    *pose_hands_height = static_cast<PRJM_EVAL_F>(state.renderContext.pose.handsHeight);
    *pose_arm_span = static_cast<PRJM_EVAL_F>(state.renderContext.pose.armSpan);
    *pose_lunge = static_cast<PRJM_EVAL_F>(state.renderContext.pose.lunge);
    *preset_complete = 0.0; // Output flag: cleared each frame; the preset re-asserts it to stay "done".
    *frame = static_cast<PRJM_EVAL_F>(state.renderContext.frame);
    for (int q = 0; q < QVarCount; q++)
    {
        *q_vars[q] = q_values_after_init_code[q];
    }
    *progress = static_cast<PRJM_EVAL_F>(state.renderContext.progress);
    *decay = static_cast<PRJM_EVAL_F>(state.decay);
    *wave_a = static_cast<PRJM_EVAL_F>(state.waveAlpha);
    *wave_av = static_cast<PRJM_EVAL_F>(state.waveAlphaState); /*FLOATBUF*/
    *wave_r = static_cast<PRJM_EVAL_F>(state.waveR);
    *wave_g = static_cast<PRJM_EVAL_F>(state.waveG);
    *wave_b = static_cast<PRJM_EVAL_F>(state.waveB);
    *wave_x = static_cast<PRJM_EVAL_F>(state.waveX);
    *wave_y = static_cast<PRJM_EVAL_F>(state.waveY);
    *wave_mystery = static_cast<PRJM_EVAL_F>(state.waveParam);
    *wave_mode = static_cast<PRJM_EVAL_F>(state.waveMode);
    *ob_size = static_cast<PRJM_EVAL_F>(state.outerBorderSize);
    *ob_r = static_cast<PRJM_EVAL_F>(state.outerBorderR);
    *ob_g = static_cast<PRJM_EVAL_F>(state.outerBorderG);
    *ob_b = static_cast<PRJM_EVAL_F>(state.outerBorderB);
    *ob_a = static_cast<PRJM_EVAL_F>(state.outerBorderA);
    *ob_av = static_cast<PRJM_EVAL_F>(state.outerBorderAlphaState); /*FLOATBUF*/
    *ib_size = static_cast<PRJM_EVAL_F>(state.innerBorderSize);
    *ib_r = static_cast<PRJM_EVAL_F>(state.innerBorderR);
    *ib_g = static_cast<PRJM_EVAL_F>(state.innerBorderG);
    *ib_b = static_cast<PRJM_EVAL_F>(state.innerBorderB);
    *ib_a = static_cast<PRJM_EVAL_F>(state.innerBorderA);
    *ib_av = static_cast<PRJM_EVAL_F>(state.innerBorderAlphaState); /*FLOATBUF*/
    *mv_x = static_cast<PRJM_EVAL_F>(state.mvX);
    *mv_y = static_cast<PRJM_EVAL_F>(state.mvY);
    *mv_dx = static_cast<PRJM_EVAL_F>(state.mvDX);
    *mv_dy = static_cast<PRJM_EVAL_F>(state.mvDY);
    *mv_l = static_cast<PRJM_EVAL_F>(state.mvL);
    *mv_r = static_cast<PRJM_EVAL_F>(state.mvR);
    *mv_g = static_cast<PRJM_EVAL_F>(state.mvG);
    *mv_b = static_cast<PRJM_EVAL_F>(state.mvB);
    *mv_a = static_cast<PRJM_EVAL_F>(state.mvA);
    *echo_zoom = static_cast<PRJM_EVAL_F>(state.videoEchoZoom);
    *echo_alpha = static_cast<PRJM_EVAL_F>(state.videoEchoAlpha);
    *echo_orient = static_cast<PRJM_EVAL_F>(state.videoEchoOrientation);
    *wave_usedots = static_cast<PRJM_EVAL_F>(state.waveDots);
    *wave_thick = static_cast<PRJM_EVAL_F>(state.waveThick);
    *wave_additive = static_cast<PRJM_EVAL_F>(state.additiveWaves);
    *wave_brighten = static_cast<PRJM_EVAL_F>(state.maximizeWaveColor);
    *darken_center = static_cast<PRJM_EVAL_F>(state.darkenCenter);
    *gamma = static_cast<PRJM_EVAL_F>(state.gammaAdj);
    *wrap = static_cast<PRJM_EVAL_F>(state.texWrap);
    *invert = static_cast<PRJM_EVAL_F>(state.invert);
    *brighten = static_cast<PRJM_EVAL_F>(state.brighten);
    *darken = static_cast<PRJM_EVAL_F>(state.darken);
    *solarize = static_cast<PRJM_EVAL_F>(state.solarize);
    *meshx = static_cast<PRJM_EVAL_F>(state.renderContext.perPixelMeshX);
    *meshy = static_cast<PRJM_EVAL_F>(state.renderContext.perPixelMeshY);
    *pixelsx = static_cast<PRJM_EVAL_F>(state.renderContext.viewportSizeX);
    *pixelsy = static_cast<PRJM_EVAL_F>(state.renderContext.viewportSizeY);
    *aspectx = static_cast<PRJM_EVAL_F>(state.renderContext.invAspectX);
    *aspecty = static_cast<PRJM_EVAL_F>(state.renderContext.invAspectY);
    *blur1_min = static_cast<PRJM_EVAL_F>(state.blur1Min);
    *blur2_min = static_cast<PRJM_EVAL_F>(state.blur2Min);
    *blur3_min = static_cast<PRJM_EVAL_F>(state.blur3Min);
    *blur1_max = static_cast<PRJM_EVAL_F>(state.blur1Max);
    *blur2_max = static_cast<PRJM_EVAL_F>(state.blur2Max);
    *blur3_max = static_cast<PRJM_EVAL_F>(state.blur3Max);
    *blur1_edge_darken = static_cast<PRJM_EVAL_F>(state.blur1EdgeDarken);
    *video_alpha_mode = static_cast<PRJM_EVAL_F>(state.videoAlphaMode);
    *video_alpha_value = static_cast<PRJM_EVAL_F>(state.videoAlphaValue);
    *video_alpha_init = static_cast<PRJM_EVAL_F>(state.videoAlphaInit);
    *video_alpha_decay = static_cast<PRJM_EVAL_F>(state.videoAlphaDecay);
    *video_cleanup = static_cast<PRJM_EVAL_F>(state.videoCleanup);
    *video_refine = static_cast<PRJM_EVAL_F>(state.videoRefine);
}

void PerFrameContext::CompilePerFrameCode(const std::string& perFrameCode)
{
    if (perFrameCode.empty())
    {
        return;
    }

    perFrameCodeHandle = projectm_eval_code_compile(perFrameCodeContext, perFrameCode.c_str());
    if (perFrameCodeHandle == nullptr)
    {
        std::string error;
        int line;
        int col;
        auto* errmsg = projectm_eval_get_error(perFrameCodeContext, &line, &col);
        if (errmsg)
        {
            error = "[PerFrameContext] Could not compile per-frame code: ";
            error += errmsg;
            error += "(L" + std::to_string(line) + " C" + std::to_string(col) + ")";
        }
        else
        {
            error = "[PerFrameContext] Could not compile per-frame code.";
        }
        LOG_DEBUG("[PerFrameContext] Failed per-frame code:\n" + perFrameCode);
        throw MilkdropCompileException(error);
    }
}

void PerFrameContext::ExecutePerFrameCode()
{
    if (perFrameCodeHandle != nullptr)
    {
        projectm_eval_code_execute(perFrameCodeHandle);
    }
}

} // namespace MilkdropPreset
} // namespace libprojectM
