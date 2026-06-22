# Validation fixtures - sources and licenses

Real-world PDFs used as validation-corpus fixtures. Both are open access under
the Creative Commons Attribution (CC BY) license, which permits redistribution
with attribution.

## plos_timeperception_2011.pdf  (used as a "known-clean" fixture)

- Title: *Stimulus Repetition and the Perception of Time: The Effects of Prior
  Exposure on Temporal Discrimination, Judgment, and Production*
- Journal: PLOS ONE (2011)
- DOI: 10.1371/journal.pone.0019815
- License: CC BY
- Why: statcheck finds several APA statistics, all internally consistent - a
  false-positive guard (RIS must not raise critical flags on a clean paper).

## plos_timeperception_2016.pdf  (used as a "known-inconsistent" fixture)

- Title: *Temporal Regularity of the Environment Drives Time Perception*
- Journal: PLOS ONE (2016)
- DOI: 10.1371/journal.pone.0159842
- License: CC BY
- Why: statcheck finds a genuine reporting inconsistency - `t(18) = -2.3,
  p = .060` recomputes to p ≈ .034, a decision error (significance threshold
  crossed). Confirms RIS catches a real inconsistency in a published paper.
  (A reporting inconsistency is not evidence of misconduct.)

## Documented GRIM cases (numbers only - no PDF needed)

The GRIM corpus cases attributed to the Cornell Food and Brand Lab "pizza"
papers come from the open-access critique:

- van der Zee, T., Anaya, J., & Brown, N. J. L. (2017). *Statistical heartburn:
  an attempt to digest four pizza publications from the Cornell Food and Brand
  Lab.* BMC Nutrition, 3:54. DOI: 10.1186/s40795-017-0167-x (CC BY).

Only the reported means and sample sizes are used; each was re-verified against
`scrutiny::grim()`.
