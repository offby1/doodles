package finish_time;

use warnings;
use strict;
use Time::HiRes qw( time sleep );

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;
  my $self  = {};
  $self->{START_TIME}   = undef;
  $self->{WORK_UNITS_COMPLETED} = 0;
  bless ($self, $class);
  return $self;
}

sub note_start {
  my $self = shift;
  $self->{START_TIME} = time ();
}

sub note_work_units {
  my $self = shift;
  $self->{WORK_UNITS_COMPLETED} += shift;
}

sub predict_finish_time {
  my $self = shift;
  my $units_to_do = shift;
  my $now = time ();
  my $units_per_second = $self->{WORK_UNITS_COMPLETED} / ($now - $self->{START_TIME});
  $now + $units_to_do / $units_per_second;
}

1;
