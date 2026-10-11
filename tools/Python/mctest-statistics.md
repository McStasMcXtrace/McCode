[&larr; back to overview](README.md) · [mctest](mctest.md)

# Statistical acceptance in mctest

mctest can judge a test value against its target using the Monte Carlo error
bars that every McStas and McXtrace run prints, instead of only a fixed ±20 %
window. This note explains the rule, the assumptions behind it, and where it can
mislead.

## TL;DR

- **Nothing changes unless you ask for it.** Without new options, mctest still
  passes a value within ±20 % of its target.
- **`mctest --nsigma 5`** also passes a value that is within 5 error bars of its
  target. This helps noisy, low-statistics tests that miss ±20 % by bad luck. It
  never fails anything ±20 % would pass, as long as targets carry no error bar
  (and none do yet).
- **Error bars from too few particles are ignored.** Fewer than ~100 effective
  events, and mctest falls back to ±20 %, so a nearly empty run can't slip through.
- **`--pvalue`** is the same threshold for people who think in p-values:
  `--pvalue 5.7e-7` is `--nsigma 5`.
- **`--statfactor F`** runs every test with F times the particles: `0.1` for a
  quick check, `100` for reference-quality numbers.
- **Use 5, not 3.** A full test run judges thousands of values, and at 3σ a few
  of them would fail by chance alone every time.
- **Error bars can lie.** Instruments using `SPLIT` report error bars that are
  too small, and some targets are a few per cent off for non-statistical
  reasons. That's why ±20 % stays in place for now.

## What a test compares

Each test definition in an instrument header gives a target intensity for one
monitor, either as a single value (`%Test:`, also accepted under its older name
`%Example:`) or as one value per point of a parameter scan (`%TestScan:`). A test
run produces, for that monitor, the intensity *I* and its error bar *ERR*, as in
the simulation's own summary line:

    Detector: transmitted_I=2.00752e+08 transmitted_ERR=2.28872e+06 transmitted_N=50752 "transmitted.L_U1"

mctest reads *I* and *ERR* from the monitor file's `# values:` line, from this
stdout line for NeXus output, or from the `NAME_I` and `NAME_ERR` columns of a
scan's `mccode.dat`. In McCode, *I* = Σ p<sub>i</sub> over the N neutron or
photon weights p<sub>i</sub> reaching the monitor, and
ERR² = N/(N−1)·(Σ p<sub>i</sub>² − (Σ p<sub>i</sub>/N)²), which is
Σ p<sub>i</sub>² to a very good approximation for any N worth testing. A single
event gives ERR = *I*.

A target may also carry its own error bar, written the same way:
`Detector: NAME_I=… NAME_ERR=…` on a `%Test:` line, or `NAME_ERR={…}` after the
`NAME_I={…}` list of a `%TestScan:` line. No instrument does this yet.

## The acceptance rule

With Δ = I<sub>test</sub> − I<sub>target</sub> and a threshold *n* given by
`--nsigma n` (or `--pvalue p`, see below), each value, or each point of a scan,
is accepted as follows.

| Threshold given | Target line | Error bars usable | Accepted when |
|---|---|---|---|
| no | any | not used | within ±20 % of the target |
| yes | no `_ERR` | yes | within ±20 % **or** \|Δ\| ≤ n·√(1+f)·ERR<sub>test</sub> |
| yes | has `_ERR` | yes | \|Δ\| ≤ n·√(ERR<sub>test</sub>² + ERR<sub>target</sub>²), error bars alone |
| yes | any | no | within ±20 % of the target |

Here *f* is the `--statfactor` (default 1, giving √2·ERR<sub>test</sub>). A scan
passes only if every point passes and the number of points matches the target
list. mcviewtest colours its report by the same rule, using the threshold and
factor stored with each result.

The ±20 % rule is kept as an equal alternative while targets carry no `_ERR`, so
an error-bar threshold can only widen acceptance, never narrow it. It is dropped
for a target that states its own error bar: such a target is a deliberate
statistical statement and is judged by the error bars alone.

## The target's error bar, when it is not recorded

A comparison of two Monte Carlo estimates needs both variances:
σ<sub>Δ</sub>² = σ<sub>test</sub>² + σ<sub>target</sub>². Today's targets were
recorded without their error bar. mctest assumes the target was produced at the
unscaled ncount of the test, so that its error bar is that of the test run
scaled back by the statistics factor:

    σ_target = σ_test · √f        ⇒        σ_Δ = σ_test · √(1 + f)

Without `--statfactor` this is σ<sub>Δ</sub> = √2·σ<sub>test</sub>. Most targets
were in fact made at higher statistics than the default test, so the assumption
usually overstates σ<sub>target</sub> and errs towards accepting: it reduces
false alarms, while a real regression still shows up as a large deviation. If a
target came from fewer events than the test, the test becomes stricter than
intended. Recording `_ERR` on the target line replaces the assumption with the
real value.

## When error bars are not trusted

An error bar from a handful of particles is itself very uncertain, and a large
one would let almost any value through. For example, with only a hundred
neutrons, a test value at 28 % of its target is only 2.2σ away. mctest therefore
uses error bars only from enough effective events:

    N_eff = (I / ERR)² ≈ (Σ p_i)² / Σ p_i²   ≥ 100

N<sub>eff</sub> is Kish's effective sample size of the weighted particles
reaching the monitor: the number of unweighted events that would give the same
relative error, which is then 1/√N<sub>eff</sub>, at most 10 % here. Below the
limit, the value is judged by the ±20 % rule, and mctest says so in its log. For
a target with `_ERR`, its own N<sub>eff</sub> must pass the limit as well. A zero
result, or a missing error bar, always falls back to the ±20 % rule.

## Choosing the threshold

For a Gaussian-distributed Δ, accepting |Δ| ≤ n·σ<sub>Δ</sub> is the same test as
accepting a two-sided p-value of at least p = 2·(1 − Φ(n)). `--pvalue p` is
converted to *n* with this relation, so the two options are equivalent ways to
set one threshold.

| n (σ) | Two-sided p | Coverage | Expected false failures per 2000 values |
|---:|---:|---:|---:|
| 2.17 | 0.03 | 97 % | 60 |
| 3 | 2.7 × 10⁻³ | 99.73 % | 5.4 |
| 4 | 6.3 × 10⁻⁵ | 99.994 % | 0.13 |
| 5 | 5.7 × 10⁻⁷ | 99.99994 % | 0.001 |

The number of comparisons decides the threshold. A full test suite judges a few
thousand values once every scan point is counted, so a per-value threshold that
sounds strict still produces several spurious failures per run at 3σ. The CI
test suites therefore run with `--nsigma 5`, which keeps the expected number of
false alarms per run far below one.

## Scaling the statistics

`--statfactor f` multiplies the ncount of every test by *f*: the general ncount
(`-n`, default 10⁶) for single-value tests, and the per-point ncount of each
scan (the scan line's own `-n`, else at most 10⁵ per point). Small factors give
quick, rough runs; large factors give runs good enough to record new targets
with their error bars. Run timeouts grow with *f*, and the factor is part of the
result directory's name.

The test run's own ERR follows the reduced or increased statistics
automatically, so nothing needs to be back-projected from the target. Only the
unrecorded target error depends on *f*, through the √(1+f) above.

## Known limitations

- **Correlated weights.** ERR assumes that the particle histories are
  independent. Instruments that use `SPLIT` reuse each history several times,
  so their error bars understate the real scatter between runs. Measured
  examples: SSRL_bl_11_2_white_src (two `SPLIT 10` crystals) scattered 8–12 %
  between seeds at 10⁶ against reported errors of 3–5 %; LLB_6T2 scattered
  5–10 % against a reported 0.2 %. For such instruments, a pure error-bar test
  with a recorded target `_ERR` would fail spuriously.
- **Systematic differences.** A target that is a few per cent off for reasons
  other than statistics, such as an older code version, fails a pure error-bar
  test as soon as the statistics are high. Test_KDSource at full statistics is
  3 % below its target, which is −13σ and −42σ for its high-statistics variants.
  Such tests pass today only through the ±20 % alternative. Record a target's
  `_ERR` only together with a freshly generated, high-statistics target value.
- **Tests whose statistics do not depend on ncount.** Instruments that read
  their particles from a file (MCPL or KDSource input) give the same value and
  error bar whatever the ncount, so `--statfactor` changes only the assumed
  target error for them. The √(1+f) assumption does not apply there.
- **Random streams.** With MPI, each rank seeds from the base seed plus its
  rank, and scans give each point its own seed. Results therefore differ between
  rank counts and from serial runs even with a fixed `--seed`, which is why
  acceptance must allow for statistical scatter in the first place.
- **Gaussian approximation.** The threshold assumes Δ is Gaussian. That holds
  well above the N<sub>eff</sub> limit, but not for a handful of events, which is
  one reason those fall back to the ±20 % rule.

## Results on the CI basic tests

The McStas basic-test instruments at 10⁶ particles with MPI, judged with
`--nsigma 5`. Deviations use the √2·ERR assumption, and only the tests where the
two rules disagree, or come close, are shown.

| Test | % of target | Deviation | N<sub>eff</sub> | ±20 % | Used rule |
|---|---:|---:|---:|---|---|
| BNL_H8_simple | 76 % | −1.0σ | ≈19 | fail | fail (below N<sub>eff</sub> limit, ±20 %) |
| Test_KDSource_3 | 97 % | −13σ | ≈4×10⁵ | pass | pass (±20 %) |
| Test_KDSource_4 | 97 % | −42σ | ≈4×10⁶ | pass | pass (±20 %) |
| ESS_BEER_MCPL | 109 % | +4.6σ | ≈6×10³ | pass | pass |
| BNL_H8 | 96 % | −4.2σ | ≈2×10⁴ | pass | pass |

All other basic-test values, McStas and McXtrace, are within 3.7σ and 9 % of
their targets. BNL_H8_simple has too few effective events at 10⁶ to be judged
by its error bar and is a genuinely noisy test; it needs a higher ncount in its
own test line.

### With scaled statistics

The same McStas tests plus the 41-point SE_example scans, at
`--statfactor 0.1` (10⁵ particles per value, 10⁴ per scan point) and
`--statfactor 10` (10⁷ and 10⁶), with `--nsigma 5`.

| Factor | Result | What happened |
|---:|---|---|
| 0.1 | 5 tests fail | BNL_H8_simple got zero counts, templateSANS_Mantid came out at 149 %, and both SE_example scans had points outside ±20 %. All of these fell below the N<sub>eff</sub> limit and were judged by the ±20 % rule: at this factor the tests lack the statistics to be judged, which the rule reports instead of hiding. |
| 10 | all pass | BNL_H8_simple, which fails ±20 % at the default ncount, comes out at 100.8 %. The scans pass with every point within 11 % of target. |

Test_KDSource and ESS_BEER_MCPL return identical values at every factor, because
they read a fixed particle file. Their reported deviation still changes with *f*
(Test_KDSource_3: −17.9σ at 0.1, −13σ at 1, −5.7σ at 10), purely through the
assumed target error σ<sub>test</sub>·√f. This is the limitation for file-input
tests described above.

## Usage

    mctest --nsigma 5                     # error bars, else ±20 %
    mctest --pvalue 5.7e-7                # the same threshold as a p-value
    mctest --nsigma 5 --statfactor 0.1    # quick run, 10× fewer particles
    mctest --nsigma 5 --statfactor 100    # reference run, e.g. to record targets with _ERR

Without `--nsigma` or `--pvalue`, mctest behaves as before: every value is
judged by the ±20 % rule.
