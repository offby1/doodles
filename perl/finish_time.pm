#!/usr/bin/perl

use warnings;
use strict;
use Time::HiRes qw( time sleep );
use Test::Unit;

package finish_time;

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

package main;

sub about_equal {
  my ($a, $b) = @_;

  (abs ($a - $b) <= 2);
}

sub test_about_equal {
  assert (about_equal (100, 101));
  assert (about_equal (101, 100));
  assert (about_equal (-20, -21));
  assert (!about_equal (20, 30));
  }

sub test_it_all {
  my $thing = finish_time->new ();

  $thing->note_start ();
  sleep (1);
  $thing->note_work_units (1);
  my $now = time ();
  my $predicted_finish = $thing->predict_finish_time (10);

  assert (about_equal ($predicted_finish - $now, 10),
          "$predicted_finish isn't about ten seconds past $now");
  $thing->note_work_units (1);
  $predicted_finish = $thing->predict_finish_time (10);

  assert (about_equal ($predicted_finish - $now, 5),
          "$predicted_finish isn't about five seconds past $now");

}

create_suite();
run_suite();

1;
