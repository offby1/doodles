#!/usr/bin/perl

use warnings;
use strict;

use Data::Dumper;
use HTTP::Server::PSGI;

my $app = sub {
    return [
        '200',
        [ 'Content-Type' => 'text/plain' ],
        [  "app sez " . Data::Dumper->Dump ([@_], [qw(args)])],
        ];
};

my $server = HTTP::Server::PSGI->new(
    host => "127.0.0.1",
    port => 8080,
    timeout => 120,
    );
$server->run($app);
