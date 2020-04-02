#############################################################
# Problem: break before ;
# (pbp p28-29)
# should allow option to break before final ;
# -bbs (?)
#############################################################

for my $elapsed_time ( 1 .. 10 ) {
    push @steps,
      $steps[-1] +
      $radial_velocity * $elapsed_time +
      $orbital_velocity * ( $phase + $phase_shift ) -
      $DRAG_COEFF * $altitude;
}

