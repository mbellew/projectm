# Music Gate + Canned-Audio Fallback — Spec

Status: design (2026-06-22). Builds directly on the existing beat tracker
([`BeatDetect`](src/libprojectM/Audio/BeatDetect.cpp)).

## Problem

In a noisy room with a **microphone** input (not a clean line feed), there is never
silence — HVAC, chatter, clinks keep the energy meter pinned. The MeltonJohn trick
(RMS-below-threshold ⇒ play canned music) therefore never fires here: an energy gate
would conclude "audio present" ~100 % of the time.

The question is not *"is there sound?"* but *"is there **music** in the room?"*. When
the answer is no, fall back to an internal canned track so beat-reactive presets keep
moving instead of flatlining into something ugly. We always want variation.

### No feedback problem

The app **never emits audio** — it only analyzes (system loopback or mic). The canned
track is played *nowhere*; it is fed silently into the analyzer purely to drive visuals.
So there is no acoustic path back into the mic and no lock-in: the gate can keep watching
the live mic the entire time the visuals are driven by canned audio.

## Architecture

Two analysis streams, one selector:

```
                   ┌────────────────────────────────────────────┐
  live mic  ──────►│ MusicGate (always-on, analyzes mic only)    │
   (always)        │   owns a BeatDetect + extra features + FSM   │
                   │   → MusicPresent() : bool (debounced)        │
                   └───────────────┬────────────────────────────┘
                                   │ selects
  live mic  ─────────┐             ▼
                     ├─► [PCM crossfade mixer] ─► projectm_pcm_add_float ─► visuals
  canned .wav ───────┘        (app side)
  (file, looped)
```

Detection must watch the **live mic** even while the visuals are driven by the **canned**
source. A single analyzer can only analyze what you feed it, so two analyzers are
unavoidable — but the detection one (`MusicGate`) is tiny (it is the `BeatDetect`
machinery). The *selected* stream is what gets pushed into projectM's `PCM` for the
actual spectrum/waveform/beat the presets see.

### Canned source: a real (looped) audio file

projectM presets need the **480-sample waveform** and **512-bin spectrum** in
[FrameAudioData](src/libprojectM/Audio/FrameAudioData.hpp) — many draw the waveform shape
directly — so a real audio file is the simplest *correct* source: decode/read it, push PCM
through the normal `projectm_pcm_add_float` path, and the engine derives bands + spectrum +
waveform + beat itself with zero new DSP. Both mixer inputs are then genuine PCM, so the
transition is a true sample crossfade.

**Recommended: bundle a short WAV loop (~60–90 s).** Uncompressed = no decoder dependency
at all (read the header, feed the samples). ~10 MB/min stereo 44.1k/16-bit, trivial on a
desktop appliance. Ship it as a *file* in the deploy / `~/.projectM` seed area (like
presets and palettes — see [[project_appliance_deploy]]), exposed via a config path key so
it's swappable; don't compile it in. Pick a clean-looping track or crossfade the wrap to
avoid a click. Move to **MP3** (vendor minimp3 / dr_mp3, single-header, public-domain) only
if bundle size becomes annoying.

> Alternative (not recommended now): MeltonJohn's static precomputed-bands table
> ([beat_data.cpp](../MeltonJohn/Daisy/src/beat_data.cpp), `const float[][4]` of
> `{bass,mid,treb,vol}` @ 30 fps, ~114 KB `.rodata`). Tiny and decoder-free, but feeds only
> the 4 band scalars — projectM's waveform presets would flatline unless we *synthesize* a
> PCM waveform from the bands. That synthesis looks artificial; only worth it for an
> embedded/no-filesystem target like the Daisy. Shipping a WAV is simpler and faithful.

### Split (per [[project_upstream_strategy]])

- **Library (upstream-able):** a self-contained `MusicGate` class in
  `src/libprojectM/Audio/`. Generic, no projectM-instance dependency beyond
  `MilkdropFFT`/`BeatDetect`. Marked `PROJECTM_CXX_EXPORT` so the host can instantiate
  it standalone on the mic.
- **App (downstream, sdl-test-ui):** owns the two sources — live capture (already there,
  [audioCapture.cpp](src/sdl-test-ui/audioCapture.cpp)) and a decoded canned file — the
  crossfade mixer, and the call into the projectM C API. File decoding, device handling,
  and source selection are app concerns and stay local.

## The detection signal

Reuse what `BeatDetect` already computes; add two cheap features. All operate on the
mono onset envelope / spectrum already produced per hop in
[`BeatDetect::ProcessHop`](src/libprojectM/Audio/BeatDetect.cpp).

1. **Beat-autocorrelation salience** — `BeatDetect::Confidence()` (the normalized
   autocorrelation peak at the winning lag). *Primary cue.* Music has regularly spaced
   onsets that survive background chatter because noise is aperiodic; this is exactly
   what the autocorrelation peak measures. Speech/room noise score low here.

2. **Tempo stability** — variance of `Bpm()` over a ~4 s window. Real music holds a
   steady tempo; noise/speech give a wandering estimate. Cheap (running mean/variance of
   a value already computed). Stable BPM is a strong confirmation.

3. **Spectral flatness (Wiener entropy)** — `exp(mean(log mag)) / mean(mag)` over the
   per-hop magnitude spectrum. Broadband room noise ⇒ flatness near 1; tonal music ⇒
   low flatness. Used as a *veto* ("is this tonal at all"), not a primary cue, since
   speech is also peaky.

Combined score (weights TBD by tuning):

```
musicConfidence = clamp(
      w_beat * beatConf
    + w_tempo * tempoStability        // 1 - normalized BPM variance
    - w_flat  * spectralFlatness,     // penalize broadband noise
    0, 1)
```

> v1 can ship with **beatConf + tempoStability** alone; both are already nearly free.
> Spectral flatness is a small add. A YAMNet/audio-classifier confirm (to reject loud
> TV/talk-radio that fakes rhythm) is explicitly **phase 2** — heavy dep, only if the
> cheap features prove insufficient in the real room.

## Gate state machine

Hysteresis, not a binary threshold — bias hard toward going *live* and be reluctant to
leave it, so real music in the room always wins quickly and brief gaps between tracks
don't flap the mode.

```
            musicConfidence > HI for  T_promote (~1.5 s)
   CANNED ───────────────────────────────────────────────► LIVE
          ◄───────────────────────────────────────────────
            musicConfidence < LO for  T_demote  (~15 s)
```

| Param        | Default | Meaning                                                    |
|--------------|---------|------------------------------------------------------------|
| `HI`         | 0.55    | confidence above this counts as "music"                    |
| `LO`         | 0.30    | confidence below this counts as "no music" (HI>LO = hyst.) |
| `T_promote`  | 1.5 s   | sustained music before switching **to** the live mic       |
| `T_demote`   | 15 s    | sustained no-music before falling back **to** canned       |
| `xfade`      | 0.75 s  | PCM crossfade length on either transition                  |

Start state: `CANNED` (so the wall is alive immediately on launch before any music).

## Source mixer + crossfade (app side)

The mixer holds both PCM streams time-aligned and emits one buffer into
`projectm_pcm_add_float`. On a state change it crossfades the *sample buffer* (not just a
post-hoc feature blend) over `xfade` so the FFT/spectrum the presets see transitions
smoothly instead of snapping:

```
out[i] = (1 - a) * from[i] + a * to[i]   // a ramps 0→1 over xfade
```

The live mic is **always** pushed into the `MusicGate` regardless of mixer state. Canned
loops continuously so it's phase-continuous when selected.

### Timing & sample rate

The frequency mismatch between a 44.1k canned file and a 48k capture device is immaterial
(~8.8% shift in band edges / BPM on an idle visual no one is beat-matching to). The two
issues that *do* matter — **pacing** the file at real-time, and **mixing two rates**
sample-by-sample during the crossfade — share one solution:

**The always-on mic capture callback is the master clock.** It keeps firing even while
canned drives the visuals (the gate needs it), so:

- **Resample the canned file to the capture device's rate once at load** (redo on device
  change). Linear interpolation is plenty for an idle fallback. Now there is a single rate
  everywhere — the crossfade blend is meaningful and projectM sees a steady rate, so
  `BeatDetect` doesn't re-lock on every source switch.
- **For every *M* samples the mic delivers, the mixer emits exactly *M* samples** to
  projectM (from mic, canned, or a per-sample blend). Canned advances in lockstep with real
  captured time — no drift, no separate wall-clock timer, and both streams stay inherently
  sample-aligned.

The 44.1↔48 difference thus collapses into a one-time resample of a file that is never
played aloud, only analyzed. Skipping the resample (feeding canned at native rate, letting
the engine re-infer) is also acceptable — the only cost is a slight BPM/band offset — so
mic-clock + load-time resample is the *clean* version, not a correctness requirement.

## Data-flow changes

- **New:** `src/libprojectM/Audio/MusicGate.{hpp,cpp}` — owns a `BeatDetect`, computes the
  extra features + FSM. API sketch:
  ```cpp
  class PROJECTM_CXX_EXPORT MusicGate {
  public:
      void Push(float monoSample);          // audio thread (feed LIVE mic)
      void Update(double secondsSinceFrame);// render thread, once per frame
      bool  MusicPresent() const;           // debounced gate output
      float Confidence() const;             // raw musicConfidence [0,1] (for UI/tuning)
  };
  ```
- **App:** instantiate one `MusicGate` on the mic; add a canned-file decoder + crossfade
  mixer; route the mixer output into the existing PCM-add path
  ([projectM_SDL_main.cpp](src/sdl-test-ui/projectM_SDL_main.cpp)).
- **No change** to `PCM`/`FrameAudioData` — the engine keeps analyzing whatever single
  stream the mixer hands it.

## Open questions

1. **Canned source** — a single bundled track, a directory shuffled, or reuse the
   `~/.projectM` seed area? (App config key, like the existing Preset/Video paths.)
2. **Expose confidence to presets?** A `music_present` / `music_conf` built-in could let
   presets style the fallback differently. Cheap if wanted; otherwise keep it host-only.
3. **Loopback vs mic** — when the source is system loopback (clean digital), the gate is
   almost unnecessary (silence detection would suffice). Worth auto-relaxing thresholds,
   or only enabling the gate for true mic capture.

## Phasing

1. `MusicGate` with **beatConf + tempoStability**, exported; wire the FSM, log
   `Confidence()` to tune `HI/LO/T_*` against the real room.
2. Canned decoder + crossfade mixer + source selection in the app.
3. Add spectral-flatness veto if cheap features false-positive on loud speech/TV.
4. (Only if needed) YAMNet/classifier confirm — see [[reference_musicality_detection]].

Related: [[project_beat_detect]] (the tracker this reuses), [[reference_musicality_detection]].
