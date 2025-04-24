# Steps to release a new **distribution** after making a ``.tar.gz`` file

  - Untar the file (in /tmp) and take a look at the
    contents.  Be sure it does not have unwanted files.
   - If necessary, remove MANIFEST, fix MANIFEST.SKIP and run make manifest again
   - Test with perl-5.8.1
    - perlbrew use perl-5.8.1
    - perl Makefile.PL; make; make test
   - Test RPerl and verify that it still works:
    - build and install the latest Perl::Tidy on development machine
    - install the latest RPerl [currently RPerl-7.000000]
    - perl Makefile.PL, make, make test - should complete ok
  - Commit to github and check test results
  - Install and test on several systems if possible

  - Upload Release to CPAN
    - Select a time when you will be available for a few days in a problem is reported
  - Upload release to sourceforge
  - Update web site
  - Update any relevant git # issues
  - Run the RUNME.sh file to tag this version and also push the tags to github. Check that the tags are uploaded at:
    https://github.com/perltidy/perltidy/tags

  - Some References:
   - Release::Checklist
     https://metacpan.org/pod/Release::Checklist
   - Berlin consensus
     https://github.com/Perl-Toolchain-Gang/toolchain-site/blob/master/berlin-consensus.md
   - Git book
     https://git-scm.com/book/en/v2/Git-Basics-Tagging
