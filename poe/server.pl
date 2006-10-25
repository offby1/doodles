#!/usr/bin/env perl

#$Id: init.el,v 1.1 2006/10/13 15:08:20 offby1 Exp $
use warnings;
use strict;
use Data::Dumper;
use POE qw(Component::Server::TCP);

POE::Component::Server::TCP->new
  ( Port => 12345,
    ClientInput => sub {
      my ( $heap, $input ) = @_[ HEAP, ARG0 ];
      my @reversed = reverse (split (//, $input));
      $heap->{client}->put(join ('', @reversed));
    },
  );

POE::Kernel->run();
exit;
