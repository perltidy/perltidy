use ExtUtils::MakeMaker;
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
        },
    },
);
