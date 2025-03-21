# Checklist for preparing a commit, new version, or release a new distribution

- Basic quality control for all **commit** candidates:
  - 'make test' successful
  - run ``devbin/run_convergence_tests.pl``
  - be sure code is tidied (you can run tidyall -a to do this)
    - note that I have tidyall set to also run perlcritic right now
  - run perlcritic
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

- Steps to build a new **version** (use build.pl):
  - review/update the CHANGES.md file
  - check/update copyright date (build.pl can do this): perltidy and Tidy.pm (2 places)
  - review code, especially any ## commented out sections and "FIXME's"
  - run podchecker on all .pod files
  - run spell checker on all files
  - review tickets at [rt.cpan.org](https://rt.cpan.org/Public/Dist/Display.html?Name=Perl-Tidy)
  - review the issues at [github](https://github.com/perltidy/perltidy/issues/)
  - be sure builds at github and Appveyor are clean for all version of perl
  - update VERSION numbers in all modules and some docs (build.pl can do this):
  - run perlver on all modules to check minimum version; should be 5.8.1
    - The first line in Tidy.pm has the required version of Perl
    - This must agree with the version in Makefile.PL
  - make manifest
    - check MANIFEST over very carefully
    - sometimes it is necessary to remove MANIFEST and then do "make manifest"
  - make the .tar.gz
    - perl Makefile.PL
    - make
    - make test
    - make dist
  - run 'cpants-lint.pl' on the .tar.gz and check results (build.pl does this)

- Steps to release a new **distribution** after making a ``.tar.gz`` file:
  - See file ``post-build-checklist.md`` (option ``POST`` in build menu)
