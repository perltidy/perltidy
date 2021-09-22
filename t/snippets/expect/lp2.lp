# test issue git #74, lost -lp when final anon sub brace followed by '}'
Util::Parser->new(
                   Handlers => {
                                 Init  => sub { $self->init(@_) },
                                 Mid   => { sub { shift; $self->mid(@_) } },
                                 Final => sub { shift; $self->final(@_) }
                   }
)->parse( $_[0] );
