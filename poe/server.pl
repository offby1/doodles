#!/usr/bin/env perl

#$Id: init.el,v 1.1 2006/10/13 15:08:20 offby1 Exp $
use warnings;
use strict;
use Data::Dumper;
use POE qw(Component::Server::TCP);

POE::Component::Server::TCP->new
  ( Port => 12345,
    ClientInput => \&client_input,
  );

POE::Kernel->run();
exit;

sub client_input {
  my ( $heap, $input ) = @_[ HEAP, ARG0 ];
  warn "Ooh!  He said '$input'";
  $heap->{client}->put($input);
}
