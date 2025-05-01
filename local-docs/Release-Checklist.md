# Checklist for preparing a commit, new version, or release a new distribution

[build.pl commands shown in brackets]

- Basic quality control for **all commit** candidates:
  - [t] 'make test' successful
  - [conv] run ``devbin/run_convergence_tests.pl``
  - be sure code is tidied (you can run tidyall -a to do this)
    - note that I have tidyall set to also run perlcritic right now
  - [pc] run perlcritic
  - run NYTProf and check the activity and performance of any changed code
```
    perl -d:NYTProf perltidy.pl -pbp -nst -nse -wn -xci perltidy.pl
    nytprofhtml --open
```
  - retest all past instability cases with blinkers.pl, if appropriate
  - run 'author tests' on a much larger body of code than is covered by the .t
    files.
  - compare formatting with the proposed new version with previous version on all files in test area
  - compare run time with previous version, fix any issues
  - Suggested minimum cpu duration of random testing without irregularities:
   - 3 cpu hours for a **commit** candidate
   - 24 cpu hours for a +0.01 **version bump** candidate
   - 100 cpu hours for a new integer **release candidate** for CPAN
  - review tickets at [rt.cpan.org](https://rt.cpan.org/Public/Dist/Display.html?Name=Perl-Tidy)
  - review the issues at [github](https://github.com/perltidy/perltidy/issues/)
  - be sure builds at github and Appveyor are clean for all version of perl

- Basic tteps to build a **new version**
  - [cl] review/update the CHANGES.md file
  - [scan, man] review code, especially any ## commented out sections and "FIXME's"
  - [pod] run podchecker on all .pod files
  - [spell] run spell checker on all files
  - [v] update VERSION numbers in all modules and some docs
  - [year] check/update copyright date: perltidy and Tidy.pm (2 places)
  - [perlver] run perlver on all modules to check minimum version; should be 5.8.1
    - The first line in Tidy.pm has the required version of Perl
    - This must agree with the version in Makefile.PL
  - [manifest] make manifest
    - check MANIFEST over very carefully
    - sometimes it is necessary to remove MANIFEST and then do "make manifest"
  - [t, dist] make the .tar.gz
    - perl Makefile.PL
    - make
    - make test
    - make dist
  - [dist does this] run 'cpants-lint.pl' on the .tar.gz and check results

- Steps to release a new **distribution** after making a ``.tar.gz`` file:
  - See file ``post-build-checklist.md`` (option ``POST`` in build menu)
