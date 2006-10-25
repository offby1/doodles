#!/usr/bin/env perl

#$Id: init.el,v 1.1 2006/10/13 15:08:20 offby1 Exp $
use warnings;
use strict;
use Data::Dumper;
use POE qw(Component::Server::TCP);
use Win32::Process;

POE::Component::Server::TCP->new
  ( Port => 12345,
    InlineStates => {
                     wait_for_process => sub {
                       my $heap = $_[ HEAP ];
                       if ($heap->{waited}++ < 5) {
                         $heap->{client}->put ("Waiting $heap->{waited} for process");
                         # TODO -- actually examine the process status
                         # here.  Don't bail out after 5 seconds, but
                         # rather whenever the process has actually
                         # exited.
                         POE::Kernel->delay (wait_for_process => 1);
                       } else {
                         $heap->{client}->put ("STATUS foo bar");
                         POE::Kernel->yield ('shutdown');
                       }
                     }
                    },
    ClientInput => sub {
      my ( $heap, $input ) = @_[ HEAP, ARG0 ];
      if ($input =~ m<^RUN (.*)$>) {
        warn "Starting '$1'";
        my $process;

        if (Win32::Process::Create ($process, # container for process object
                                    $1, # full path name of executable module
                                    "", # command line args
                                    0, # flag: inherit calling processes handles or not
                                    NORMAL_PRIORITY_CLASS, # flags for creation (see exported vars below)
                                    "." # working dir of new process
                                   )) {
          my $PID = Win32::Process::GetProcessID($process);
          $heap->{client}->put("Your PID is $PID");
          POE::Kernel->delay (wait_for_process => 1);
        } else {
          $heap->{client}->put("Could not launch $1: $! $^E");
        }
      } else {
        my @reversed = reverse (split (//, $input));
        $heap->{client}->put(join ('', @reversed));
      }
    },
  );

POE::Kernel->run();
exit;
