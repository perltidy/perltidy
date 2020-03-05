# Checklist of some things to when preparing a new version and/or release

- review tickets at [rt.cpan.org](https://rt.cpan.org/Public/Dist/Display.html?Name=Perl-Tidy) 
- compare the new version with previous version on all files in test area
- run 'author tests' on a much larger body of code than is covered by the .t
  files.
    - compare results of the current version with previous version
- review tickets at sourceforge (hardly used now, but possible)
- review/update the ChangeLog.pod file
- be sure RPerl still works
- Review code, especially any ## commented out sections and "FIXME's"
- run perlver on all modules to check minimum version; should be 5.8.0
  - The first line in Tidy.pm has the required version of Perl
  - travis-CI is setup to test on version 5.8 so we should catch this type of error automatically
  - use perlbrew to do local checks and debugging on earlier versions of perl
- Run tidyall -a to be sure code is tidied
  - note that I have tidyall set to also run perlcritic right now
- Run perlcritic (if not done by tidyall)
- run podchecker on all .pod files
- run ispell on all .pod files
- Be sure build at Travis.CI is clean for all version of perl
- update VERSION numbers in these files (build.pl can do this):
   - lib/Perl/Tidy.pm
   - lib/Perl/Tidy.pod
   - bin/perltidy
   - local-docs/ChangeLog.pod
- make manifest
    - check MANIFEST over carefully
    - sometimes it is necessary to remove MANIFEST and then do "make manifest"
- make the .tar.gz
   - perl Makefile.PL
   - make 
   - make test
   - make dist
- *IMPORTANT:* Now untar the file (perhaps in /tmp) and take a look at the
  contents.  Be sure it does not have unwanted stuff
   - If necessary, remove MANIFEST, fix MANIFEST.SKIP and run make manifest again
- Do test installs on several systems 
- Upload Release to CPAN
- Update CPAN tickets
- Upload release to sourceforge
- Update web sites
- Be sure to run the RUNME.sh file to tag this version and also push the tags to github. Check that the tags are uploaded at:

   https://github.com/perltidy/perltidy/tags

- If you forget, or need to modify tags, see the directions in the book:

   https://git-scm.com/book/en/v2/Git-Basics-Tagging
