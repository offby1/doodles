#!/usr/bin/perl

use Test::More tests => 7;

use_ok ('finish_time');

sub about_equal {
  my ($a, $b) = @_;

  (abs ($a - $b) <= 2);
}

ok (about_equal (100, 101));
ok (about_equal (101, 100));
ok (about_equal (-20, -21));
ok (!about_equal (20, 30));

my $thing = finish_time->new ();

$thing->note_start ();
sleep (1);
$thing->note_work_units (1);
my $now = time ();
my $predicted_finish = $thing->predict_finish_time (10);

ok (about_equal ($predicted_finish - $now, 10),
    "$predicted_finish isn't about ten seconds past $now");
$thing->note_work_units (1);
$predicted_finish = $thing->predict_finish_time (10);

ok (about_equal ($predicted_finish - $now, 5),
    "$predicted_finish isn't about five seconds past $now");
