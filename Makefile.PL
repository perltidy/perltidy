use ExtUtils::MakeMaker;

my $mm_ver = $ExtUtils::MakeMaker::VERSION;
if ( $mm_ver =~ /_/ ) {    # developer release/version
    $mm_ver = eval $mm_ver;
    die $@ if $@;
}

#   Minimum version found by perlver:
#
#     ------------------------------------------
#   | file        | explicit | syntax | external |
#   | ------------------------------------------ |
#   | perltidy.pl | v5.8.0   | v5.8.0 | n/a      |
#   | ------------------------------------------ |
#   | Minimum explicit version : v5.8.0          |
#   | Minimum syntax version   : v5.8.0          |
#   | Minimum version of perl  : v5.8.0          |
#     ------------------------------------------

WriteMakefile(
    NAME         => "Perl::Tidy",
    VERSION_FROM => "lib/Perl/Tidy.pm",
    (
        $] >= 5.005
        ? (
            ABSTRACT => 'indent and reformat perl scripts',
            LICENSE  => 'gpl_2',
            AUTHOR   => 'Steve Hancock <perltidy@perltidy.sourceforge.net>'
          )
        : ()
    ),
    (
        $mm_ver >= 6.48 ? ( MIN_PERL_VERSION => 5.008 )
        : ()
    ),

    EXE_FILES  => ['bin/perltidy'],
    dist       => { COMPRESS => 'gzip', SUFFIX => 'gz' },
    META_MERGE => {
        'meta-spec' => { version => 2 },
        resources   => {
            repository => {
                type => 'git',
                url  => 'https://github.com/perltidy/perltidy.git',
                web  => 'https://github.com/perltidy/perltidy',
            },
            bugtracker => {
                "web" => "https://github.com/perltidy/perltidy/issues"
            },
        },
    },
);
