#!/usr/bin/perl

use warnings;
use strict;

=head1 dirlist_as_sexp

Takes the output of the shell's "dirs" command, and formats the list
of directories that it returns in a way that's easy for Emacs to
parse.

Expects its input to consist of one directory name per line.  This
means that directory names may not contain newlines, but they rarely
do.  However, it also means that we I<can> parse directory names that
contain spaces, which is common on Windows.

=cut

# Characters that need to be quoted in an Emacs Lisp string constant.

my $need_quoting = q(\");

my @list_o_strings;
while (<>) {
  chomp;
  my @input_chars = split ('');
  my @output_chars;
  foreach (@input_chars) {
    if (index ($need_quoting, $_) != -1) {
      push @output_chars, ('\\');
    }
    push @output_chars, ($_);
  }
  push @list_o_strings, ( '"'
                          . join ('', @output_chars)
                          . '"');
}

print "(", join (" ", @list_o_strings), ")\n";

=head1 TODO

Now I gotta rewrite `shell-resync-dirs' in Emacs so that it expects
this output, instead of a simple space-separated list.

=cut
  
