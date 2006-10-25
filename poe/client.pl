#!/usr/bin/env perl

#$Id: init.el,v 1.1 2006/10/13 15:08:20 offby1 Exp $
use warnings;
use strict;
use Data::Dumper;

use POE qw(Component::Client::TCP);

# Basic usage.

my %statuses;

POE::Component::Client::TCP->new
  ( RemoteAddress => "127.0.0.1",
    RemotePort    => 12345,
    ServerInput   => sub {
      my $input = $_[ARG0];
      print "from server: $input\n";
      if ($input =~ m<^STATUS (.*) (.*)$>) {
        $statuses{$1} = $2;
      }
    },
    InlineStates => {
                     start_remote_prog => sub {
                       my $heap = $_[HEAP];
                       $heap->{server}->put("RUN c:\\windows\\system32\\notepad.exe");
                       POE::Kernel->yield ("get_status");
                     },
                     get_status => sub {
                       warn Data::Dumper->Dump ([\%statuses], [qw(statuses)]);
                       unless (keys %statuses) {
                         POE::Kernel->delay ("get_status", 1);
                       }
                     }
                    },
    Connected => sub {
      my $heap = $_[HEAP];
       {
        $heap->{server}->put("Hey you, Mr Server sir!!");
        $heap->{count} = 0;
        POE::Kernel->yield ("start_remote_prog");
      }
    },
  );

POE::Kernel->run();
exit;
