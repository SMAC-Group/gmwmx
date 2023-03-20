# Local check on Ubuntu 20.04

── R CMD check results ───────────────── gmwmx 1.0.3 ────
Duration: 55.6s

❯ checking compiled code ... OK
   WARNING
  ‘qpdf’ is needed for checks on size reduction of PDFs

❯ checking for future file timestamps ... NOTE
  unable to verify current time

0 errors ✔ | 1 warning ✖ | 1 note ✖
Error: R CMD check found WARNINGs
Execution halted

Exited with status 1.


## R-CMD-check on GitHub actions 

All jobs pass on 

- macOS-latest (release)
- ubuntu-latest (devel)
- ubuntu-latest (oldrel-1)
- ubuntu-latest (release)
- windows-latest (release)


# Downstream dependencies
There are currently no downstream dependencies for this package.

