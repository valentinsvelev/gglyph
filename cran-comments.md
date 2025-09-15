## Test environments

- Local: macOS 15.6.1 (arm64), R 4.4.3
- R-hub v2: Linux, Windows, macOS (arm64 & x86_64), Ubuntu (next/R-devel) — all ✔

1 NOTE in win-builder check:
- "Possibly misspelled words in DESCRIPTION:
  Abu (14:5)
  Akel (14:9)
  al (14:17)
  et (14:14)"
- Explanation: This NOTE appears to be a false positive from the spell checker and can therefore be safely ignored.

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔ (local)

1 NOTE silenced by setting _R_CHECK_SYSTEM_CLOCK_=0:
- "checking for future file timestamps ... NOTE: unable to verify current time"
- Reason: builder time verification issue; not package-related.

## Notes from previous submission

### Resubmission #2 (15.09.2025)

- Fixed the title of the package.
- Added a reference in the DESCRIPTION file.
- Fixed examples ("dontrun" vs. "donttest").
- Added resets of user's options and par in vignette.
- Changed the datasets so that they can be used without wrangling.

### Resubmission #1 (08.09.2025)

- Version number was dev-style (0.0.0.9000). Changed to 0.1.0.
- Removed the broken URL from R/data.R.
