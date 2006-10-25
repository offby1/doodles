#!/usr/bin/env perl

#$Id: init.el,v 1.1 2006/10/13 15:08:20 offby1 Exp $
use warnings;
use strict;
use Data::Dumper;
use POE qw(Component::Server::TCP);
use Win32::Process;

POE::Component::Server::TCP->new
  ( Port => 12345,
    ClientInput => sub {
      my ( $heap, $input ) = @_[ HEAP, ARG0 ];
      if ($input =~ m<^PROC (.*)$>) {
        warn "Starting '$1'";
        my $process;

        if (Win32::Process::Create ($process, # container for process object
                                    $1, # full path name of executable module
                                    "", # command line args
                                    0, # flag: inherit calling processes handles or not
                                    NORMAL_PRIORITY_CLASS, # flags for creation (see exported vars below)
                                    "." # working dir of new process
                                   )) {
          $heap->{client}->put("Could not launch $1: $! $^E");
        } else {
          my $PID = Win32::Process::GetProcessID($process);
          $heap->{client}->put("Your PID is $PID");
        }
      } else {
        my @reversed = reverse (split (//, $input));
        $heap->{client}->put(join ('', @reversed));
      }
    },
  );

POE::Kernel->run();
exit;
