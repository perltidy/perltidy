# Checklist for preparing a new version and/or release

- basic quality control:
  - 'make test' successful for all commit canditates
  - run 'devbin/run__convergence_tests.pl' for all commit candidates
  - Run perlcritic
  - run NYTProf and check the activity and performance of all changed code
  - IMPORTANT: random testing minimum test duration without irregularities:
    - 3 cpu hours for a commit candidate
    - 24 cpu hours for a +0.01 version bump candidate
    - 100 cpu hours for a new integer release candidate for CPAN
  - run 'author tests' on a much larger body of code than is covered by the .t
    files.
      - compare results of the current version with previous version
- review tickets at [rt.cpan.org](https://rt.cpan.org/Public/Dist/Display.html?Name=Perl-Tidy) 
- review the issues at [github](https://github.com/perltidy/perltidy/issues/)
- test perltidy -D on a large test file
- compare formatting with the new version with previous version on all files in test area
- compare formatting with the new version on selected projects and perltidy itself
- profile with Devel::NYTProf with different parameters and compare with previous version: For example
  perl -d:NYTProf perltidy.pl -pbp -nst -nse -wn -xci perltidy.pl
  nytprofhtml --open
- compare run time with previous version
- review tickets at sourceforge (hardly used now, but possible)
- review/update the CHANGES.md file
- be sure RPerl still works:
  - build and install the latest Perl::Tidy on development machine
  - install the latest RPerl [currently RPerl-7.000000]
  - perl Makefile.PL, make, make test - should complete ok
- Review code, especially any ## commented out sections and "FIXME's"
- run perlver on all modules to check minimum version; should be 5.8.0
  - The first line in Tidy.pm has the required version of Perl
  - use perlbrew to do local checks and debugging on earlier versions of perl if desired
- Run tidyall -a to be sure code is tidied
  - note that I have tidyall set to also run perlcritic right now
- run podchecker on all .pod files
- run ispell on all .pod files
- Be sure builds at github and Appveyor are clean for all version of perl
- update VERSION numbers in all modules and some docs (build.pl can do this):
   - lib/Perl/Tidy.pm
   - lib/Perl/Tidy.pod
   - bin/perltidy
   - CHANGES.md
- make manifest
    - check MANIFEST over very carefully
    - sometimes it is necessary to remove MANIFEST and then do "make manifest"
- make the .tar.gz
   - perl Makefile.PL
   - make 
   - make test
   - make dist
- *IMPORTANT:* Now untar the file (perhaps in /tmp) and take a look at the
  contents.  Be sure it does not have unwanted files.
   - If necessary, remove MANIFEST, fix MANIFEST.SKIP and run make manifest again
- run 'cpants-lint.pl' on the .tar.gz and check results
  (this is an option in build.pl after making a new .tar.gz)
- Do test installs on several systems 
- Review the suggestions in Release::Checklist
     https://metacpan.org/pod/Release::Checklist
   and the Berlin concensus
     https://github.com/Perl-Toolchain-Gang/toolchain-site/blob/master/berlin-consensus.md

- Upload Release to CPAN
- Update CPAN tickets
- Upload release to sourceforge
- Update web sites
- Be sure to run the RUNME.sh file to tag this version and also push the tags to github. Check that the tags are uploaded at:

   https://github.com/perltidy/perltidy/tags

- If you forget, or need to modify tags, see the directions in the book:

   https://git-scm.com/book/en/v2/Git-Basics-Tagging
