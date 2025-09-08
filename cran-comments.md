## Test environments

- Local: macOS 15.6.1 (arm64), R 4.4.3
- R-hub v2: Linux, Windows, macOS (arm64 & x86_64), Ubuntu (next/R-devel) — all ✔

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔ (local)

1 NOTE silenced by setting _R_CHECK_SYSTEM_CLOCK_=0:
- "checking for future file timestamps ... NOTE: unable to verify current time"
- Reason: builder time verification issue; not package-related.

## Notes from previous submission
- Version number was dev-style (0.0.0.9000). Changed to 0.1.0.
- Removed broken URL from R/data.R.
