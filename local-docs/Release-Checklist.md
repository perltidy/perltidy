# Checklist for preparing a new version and/or release

- Basic quality control for all commit candidates:
  - 'make test' successful
  - run ``devbin/run_convergence_tests.pl``
  - run perlcritic
  - Run tidyall -a to be sure code is tidied
    - note that I have tidyall set to also run perlcritic right now
  - run NYTProf and check the activity and performance of all changed code
```
    perl -d:NYTProf perltidy.pl -pbp -nst -nse -wn -xci perltidy.pl
    nytprofhtml --open
```
  - run random testing minimum test duration without irregularities:
    - 3 cpu hours for a commit candidate
    - 24 cpu hours for a +0.01 version bump candidate
    - 100 cpu hours for a new integer release candidate for CPAN
  - run 'author tests' on a much larger body of code than is covered by the .t
    files.
      - compare results of the current version with previous version

- Steps to create a new version:
  - review tickets at [rt.cpan.org](https://rt.cpan.org/Public/Dist/Display.html?Name=Perl-Tidy)
  - review the issues at [github](https://github.com/perltidy/perltidy/issues/)
  - compare formatting with the proposed new version with previous version on all files in test area
  - compare run time with previous version
  - review/update the CHANGES.md file
  - Check/update copyright date (build.pl can do this): perltidy and Tidy.pm (2 places)
  - Review code, especially any ## commented out sections and "FIXME's"
  - run perlver on all modules to check minimum version; should be 5.8.1
    - The first line in Tidy.pm has the required version of Perl
  - use perlbrew to do local checks and debugging on earlier versions of perl
   - perlbrew list               [show installed versions]
   - perlbrew available --all    [show all available versions]
   - or see https://www.cpan.org/src/README.html
   - perlbrew install perl-5.8.1 [install perl-5.8.1, may need to force]
   - perlbrew use perl-5.8.1     [switch to 5.8.1 in current shell]
  - run podchecker on all .pod files
  - run ispell on all .pod files
  - Be sure builds at github and Appveyor are clean for all version of perl
  - update VERSION numbers in all modules and some docs (build.pl can do this):
  - make manifest
    - check MANIFEST over very carefully
    - sometimes it is necessary to remove MANIFEST and then do "make manifest"
  - make the .tar.gz
    - perl Makefile.PL
    - make
    - make test
    - make dist
  - run 'cpants-lint.pl' on the .tar.gz and check results (build.pl does this)

- Steps to release a version
  - *IMPORTANT:* Now untar the file (in /tmp) and take a look at the
    contents.  Be sure it does not have unwanted files.
    - If necessary, remove MANIFEST, fix MANIFEST.SKIP and run make manifest again
  - commit to github and check test results
  - Do test install with perlbrew use perl-5.8.1
  - Install and test on several systems if possible
  - be sure RPerl still works:
    - build and install the latest Perl::Tidy on development machine
    - install the latest RPerl [currently RPerl-7.000000]
    - perl Makefile.PL, make, make test - should complete ok
  - Review the suggestions in Release::Checklist
     https://metacpan.org/pod/Release::Checklist
    and the Berlin concensus
     https://github.com/Perl-Toolchain-Gang/toolchain-site/blob/master/berlin-consensus.md
  - Upload Release to CPAN
  - Update CPAN tickets
  - Upload release to sourceforge
  - Update web site
  - Be sure to run the RUNME.sh file to tag this version and also push the tags to github. Check that the tags are uploaded at:

   https://github.com/perltidy/perltidy/tags

  - If you forget, or need to modify tags, see the directions in the book:

   https://git-scm.com/book/en/v2/Git-Basics-Tagging
