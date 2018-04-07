# a good test problem for -lp; thanks to Ian Stuart
push @contents,
  $c->table(
             { -border => '1' },
             $c->Tr(
                     { -valign => 'top' },
                     $c->td(
                             " Author ",
                             $c->textfield(
                                            -tabindex => "1",
                                            -name     => "author",
                                            -default  => "$author",
                                            -size     => '20'
                             )
                     ),
                     $c->td(
                             $c->strong(" Publication Date "),
                             $c->textfield(
                                            -tabindex => "2",
                                            -name     => "pub_date",
                                            -default  => "$pub_date",
                                            -size     => '20'
                             ),
                     )
             ),
             $c->Tr(
                     { -valign => 'top' },
                     $c->td(
                             { -colspan => '2' },
                             $c->strong("Title"),
                             $c->textfield(
                                            -tabindex => "3",
                                            -name     => "title",
                                            -default  => "$title",
                                            -override => '1',
                                            -size     => '40'
                             ),
                     )
             ),
             $c->Tr(
                  { -valign => 'top' },
                  $c->td(
                          $c->table(
                                     $c->Tr(
                                            $c->td(
                                                   { -valign => 'top' },
                                                   $c->strong(" Document Type ")
                                            ),
                                            $c->td(
                                                { -valign => 'top' },
                                                $c->scrolling_list(
                                                    -tabindex => "4",
                                                    -name     => "doc_type",
                                                    -values => [@docCodeValues],
                                                    -labels => \%docCodeLabels,
                                                    -default => "$doc_type"
                                                )
                                            )
                                     )
                          )
                  ),
                  $c->td(
                      $c->table(
                          $c->Tr(
                              $c->td(
                                  { -valign => 'top' },
                                  $c->strong(
                                      " Relevant Discipline ", $c->br(), "Area "
                                  )
                              ),
                              $c->td(
                                      { -valign => 'top' },
                                      $c->scrolling_list(
                                                     -tabindex => "5",
                                                     -name     => "discipline",
                                                     -values => [@discipValues],
                                                     -labels => \%discipLabels,
                                                     -default => "$discipline"
                                      ),
                              )
                          )
                      )
                  )
             ),
             $c->Tr(
                     { -valign => 'top' },
                     $c->td(
                             { -colspan => '2' },
                             $c->table(
                                  $c->Tr(
                                      $c->td(
                                          { -valign => 'top' },
                                          $c->strong(" Relevant Subject Area "),
                                          $c->br(),
                                          "You may select multiple areas",
                                      ),
                                      $c->td(
                                              { -valign => 'top' },
                                              $c->checkbox_group(
                                                       -tabindex => "6",
                                                       -name     => "subject",
                                                       -values => [@subjValues],
                                                       -labels => \%subjLabels,
                                                       -defaults => [@subject],
                                                       -rows     => "2"
                                              )
                                      )
                                  )
                             )
                     )
             ),
             $c->Tr(
                     { -valign => 'top' },
                     $c->td(
                             { -colspan => '2' },
                             $c->strong("Location<BR>"),
                             $c->small("(ie, where to find it)"),
                             $c->textfield(
                                            -tabindex => "7",
                                            -name     => "location",
                                            -default  => "$location",
                                            -size     => '40'
                             )
                     )
             ),
             $c->Tr(
                     { -valign => 'top' },
                     $c->td(
                             { -colspan => '2' },
                             $c->table(
                                       $c->Tr(
                                           $c->td(
                                               { -valign => 'top' },
                                               "Description", $c->br(),
                                               $c->small("Maximum 750 letters.")
                                           ),
                                           $c->td(
                                                 { -valign => 'top' },
                                                 $c->textarea(
                                                     -tabindex => "8",
                                                     -name     => "description",
                                                     -default => "$description",
                                                     -wrap    => "soft",
                                                     -rows    => '10',
                                                     -columns => '60'
                                                 )
                                           )
                                       )
                             )
                     )
             ),
  );
