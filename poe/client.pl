#!/usr/bin/env perl

#$Id: init.el,v 1.1 2006/10/13 15:08:20 offby1 Exp $
use warnings;
use strict;
use Data::Dumper;

use POE qw(Component::Client::TCP);

# Basic usage.

POE::Component::Client::TCP->new
  ( RemoteAddress => "127.0.0.1",
    RemotePort    => 12345,
    ServerInput   => sub {
      my $input = $_[ARG0];
      print "from server: $input\n";
    },
    InlineStates => {
                     say_something => sub {
                       my $heap = $_[HEAP];
                       if (!defined ($heap->{count})
                           || ($heap->{count} < 10)) {
                         $heap->{server}->put("Something #" . $heap->{count});
                         $heap->{count}++;
                         POE::Kernel->yield ("say_something");
                       } elsif ($heap->{count} == 10) {
                         POE::Kernel->yield ("shutdown");
                       }
                     }
                    },
    Connected => sub {
      my $heap = $_[HEAP];
       {
        $heap->{server}->put("Hey you, Mr Server sir!!");
        $heap->{count} = 0;
        POE::Kernel->yield ("say_something");
      }
    },
  );

POE::Kernel->run();
exit;
