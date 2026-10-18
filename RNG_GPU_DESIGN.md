# A GPU-capable Mersenne-Twister-family RNG for McCode

**Status:** prototype for discussion. Local branch `rng-gpu-mt-design`, built on top of
mccode-dev/McCode#2808. Not pushed, not merged.
**Proposal:** add TinyMT64 as an opt-in generator, `-DRNG_ALG=3`. KISS stays the default, so
nobody's results change unless they choose TinyMT64.

## Why

McCode has two random number generators, selected at compile time with `RNG_ALG`:

| | MT19937 (`RNG_ALG=1`) | KISS (`RNG_ALG=2`, default) |
|---|---|---|
| State | one global 624-word array per process | 7 words stored on each particle |
| Per-ray seeding | none, seeded once at start-up | yes |
| Output | 32 bit (`rand01()` resolution 2^-32) | 64 bit |
| GPU (OpenACC) | **no**: all threads share one state | yes |

So on GPU only KISS is available. We have no second, independently designed generator to
cross-check GPU results against, and the Mersenne Twister option is CPU only.

## Options looked at

**desprng** (J. Carlsson, OpenACC blog 2020, `github.com/JohanCar1sson/desprng`). Despite the
context, it is *not* an OpenACC Mersenne Twister. The author tried MT and dropped it because its
~2.5 kB state is too big per GPU thread. desprng is a counter-based generator: each random number
is a DES encryption of a counter, with a key that is unique per thread. It passes TestU01 Crush.
For McCode it has drawbacks:
- it is slow: about 50× KISS per draw, plus a 2.2 µs key setup per ray (McCode rays use only tens of draws);
- per-particle state is 256-776 bytes, against 56 bytes for KISS;
- it needs a 4 kB lookup table on the device;
- it assumes a 64-bit `unsigned long`, so it fails on Windows/MSVC, and its byte handling depends on endianness;
- the repository has had no commits since 2020 and has no licence file.

**TinyMT64** (M. Saito & M. Matsumoto, 2011; the Mersenne Twister authors; BSD-3;
`github.com/MersenneTwister-Lab/TinyMT`). It is the MT family's own design for "one generator per GPU thread":
- period 2^127−1, 64-bit output;
- the state is just 2 words per particle, and the parameters are compile-time constants;
- about 20 lines of plain integer arithmetic, with no globals and no tables, so CPU and GPU can produce bit-identical streams;
- about 2× the cost of KISS per draw.

**Philox** (Random123, counter-based, used by cuRAND and NumPy) would be the standard choice
*if* we specifically needed a counter-based generator. Rays draw their numbers in sequence, so we
don't need one today.

CPU cost per draw, measured on macOS arm64 with clang -O2 over 1e6 rays × 100 draws:

| Generator | ns per draw | Setup per ray | State per particle |
|---|---|---|---|
| KISS | 3.2 | ~0 | 56 B |
| TinyMT64 | 6.1 | ~10 ns | 16 B |
| desprng | 163 | 2.2 µs | 256-776 B |

## What the prototype changes

107 added lines in three files, plus mccode-antlr later:

| File | Change |
|---|---|
| `mccode/src/cogen.c.in` | `_RNG_ALG_TINYMT 3`, with state type `uint64_t` and `RANDSTATE_LEN 2` |
| `common/lib/share/mccode-r.h.in` | TinyMT branch: `srandom`, `random`, `MC_RAND_MAX`, GPU (`acc routine seq`) declarations. Building `RNG_ALG=1` with OpenACC now stops with an `#error` instead of racing silently. |
| `common/lib/share/mccode-r.c` | `tinymt64_srandom()` and `tinymt64_random()`, with the BSD-3 notice |
| mccode-antlr `c_header.py` | needs the same `#define` block as cogen (not done yet) |

Untouched: the ray-trace loops, components, `mccode_main.c` and the Python tools.

Each ray gets its own TinyMT64 stream, seeded from the per-ray key that #2808 introduces
(a hash of the run seed and the global ray index). All rays use one parameter set, so their streams are
separate starting points on the same 2^127 cycle. With up to 2^50 rays, the chance that any two overlap
is about 2^-67. Giving each thread its own parameter set (TinyMTDC) is possible but not needed.

TinyMT64 relies on #2808's prerequisites: the particle's `randstate` array is now sized by
`RANDSTATE_LEN`, and the per-ray key no longer overlaps across MPI ranks.

### How to select it

Compile with `-DRNG_ALG=3`, for example through the `CFLAGS` entry in `mccode_config.json`, or a
`DEPENDENCY`/`CFLAGS=` line in the instrument. Note that the `CFLAGS` *environment variable* only reaches
the compiler in conda-packaged installs. In a source build, `CFLAGS=-DRNG_ALG=3 mcrun ...` silently
keeps KISS.

## Testing so far

All tests on macOS arm64, clang, CPU:

| Check | Result |
|---|---|
| Runtime functions vs the authors' reference output (`check64.out.txt`, seed 1, 9 values) | match |
| Compiler warnings (`-Wall -Wextra -pedantic`) | none |
| `Test_RNG_rand01`, CI settings (`-s 1000 Ncount=1000`), checksum | KISS `6c203d69…` (unchanged from #2808); TinyMT64 `753159792665dde568a61d3afd8e42ae` (candidate CI reference) |
| TinyMT64, same seed twice | identical output |
| 1e7 rays, (rand01, rand01) in 50×50 bins, χ²/dof (expected 1.000 ± 0.028) | KISS 0.967, TinyMT64 0.989 |
| MPI `-np 4`, 4e6 rays, χ²/dof | KISS 1.021, TinyMT64 1.004 |
| MPI `-np 4` *without* #2808's seeding fix (for comparison) | KISS 1.33, TinyMT64 1.37: the excess comes from the old seeding, not the generator |
| `Unittest_SPLIT`, normal and FUNNEL mode, `RNG_ALG` 1/2/3, ASan + UBSan | clean (after rebase: UBSan + array bounds; before rebase also ASan) |
| `RNG_ALG=1` built with OpenACC | stops with the new `#error` |

**Not yet tested:**
- an nvc/OpenACC GPU build; the key check is that CPU and GPU give bit-identical output for the same seed;
- Linux (gcc, clang) and Windows (MSVC);
- TestU01 BigCrush on per-ray streams.

## Related findings

- **mccode-antlr SPLIT bug** (mccode-dev/mccode-antlr#392): the SPLIT backup restores only the first
  word of the RNG state, so the random streams of SPLIT children are correlated under KISS.
- **`_randminmax()`** returns `rand0max(max−min) + max`; it should be `+ min`. No shipped component
  calls it, but it is public API.
- **`_rand01()` can return exactly 1.0**: it divides a full 64-bit integer by 2^64, so the top values round
  up (p ≈ 2^-54 per draw). Using `(x >> 11) * 2^-53` gives [0, 1) exactly. Fixing it changes results.
- **KISS quality has never been tested** with BigCrush. It is a 64-bit adaptation of a 32-bit design,
  and all four sub-generators are seeded with nearly the same value. A BigCrush run would show whether a
  second generator is nice to have or necessary.

## Questions for the team

1. Is the goal "a Mersenne Twister available on GPU" (TinyMT64) or "a counter-based generator"
   (then Philox, rather than desprng)?
2. Should TinyMT64 stay opt-in, or become the default later? A new default changes results for
   everyone, so it should come only after the #2808 reference re-evaluation, not mixed with it.
3. Who can run the nvc GPU build and the MSVC build?
