# Per-ray RNG seeding and `_uid` in the generated ray-trace loops

## Status

*Proposed* and *implemented* in mccode-dev/McCode#2808 (fixes
mccode-dev/McCode#2807). Needs a matching change in mccode-antlr (mccode-dev/mccode-antlr#392), whose
`translators/c_raytrace.py` holds its own copy of these lines.

## Context

`raytrace_all()` and `raytrace_all_funnel()` (written by `cogen.c.in`)
seed each ray with `srandom(_hash((pidx+1)*(seed+1)))`, and then do
`seed += gpu_innerloop` after every batch. Because the key is a product:

* runs at different seeds share rays: ray `p` at seed 3 is ray `2p+1`
  at seed 1 (50% of rays shared);
* the ranks of one MPI run (`mcseed + rank`) share rays: with
  `-np 8 --seed 1` only ~55% of rays are distinct;
* with 32-bit `unsigned long` (MSVC) the key wraps at 2^32; seed
  2147483647 gives just two different rays.

Also, `_uid` was `pidx` (+ `rank*ncount` with MPI), so it repeated in
every GPU batch. The rank stride `ncount` could also be smaller than
the `loops*gpu_innerloop` rays a rank actually traces.

## Decision

In both loops (KISS RNG, `RNG_ALG=2`, the default):

```c
_particle->_uid = cloop*gpu_innerloop + pidx;
_particle->_uid += mpi_node_rank * loops*gpu_innerloop;          /* USE_MPI */
srandom(_hash(_hash(seed) + cloop*gpu_innerloop + pidx));
```

and drop the per-batch `seed += gpu_innerloop`. The global ray index
replaces it. Keeping it would make seed S batch 1 equal to seed
S+gpu_innerloop batch 0.

`_hash` is the existing 64-bit bijection (`randstate_t` is `uint64_t`
for KISS) and is already `#pragma acc routine seq`. No runtime change and
no new helper: a SplitMix64 mixer was tried and gave the same results.

## Consequences

* **Numerical results change** for every KISS run at a given `--seed`.
  Outputs are not bit-identical to earlier releases. The CI
  `Test_RNG_rand01` checksum was updated to
  `6c203d69205b0656c1391555e6068c93`. Mersenne Twister builds
  (`RNG_ALG=1`) ignore the per-ray seed and are unaffected.
* Different seeds and different MPI ranks no longer share rays. MSVC
  (32-bit `long`) behaves like the other platforms.
* On CPU (`loops=1`, `gpu_innerloop=ncount`) `_uid` values are unchanged.
  On GPU they are unique across batches.
* The particle's `randstate` is now sized by `RANDSTATE_LEN` (was a hard-coded
  `[7]`), and `sort_absorb_last()` (FUNNEL + SPLIT) copies `RANDSTATE_LEN` words
  instead of 7 hand-written ones. No change for KISS (7); the MT struct shrinks to
  1 word. This lets an RNG with another state length be added without
  out-of-bounds copies.
* mccode-antlr must port the same lines by hand (mccode-dev/mccode-antlr#392). Until it does, the two
  code generators give different results for the same seed.

## Behaviour

Measured on macOS arm64, clang, CPU (rays identified by their first draws):

| case | before | after |
|---|---|---|
| seeds 1 and 3, rays shared | 50% | 0 |
| seed 2147483647, 32-bit, distinct rays | 2 | all |
| `mpirun -np 8 --seed 1`, distinct rays | ~55% | 100% |
| same seed twice | identical | identical |

Not tested locally: MSVC, Linux, nvc/OpenACC (batched GPU path).
