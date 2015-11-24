#!/usr/bin/perl
use strict;
use warnings;

my $file = $ARGV[0];  ##get lada data file
my $out = "statins.txt";   ##name outfile name based on study

##OPEN FILES##
open (IN, '<:encoding(UTF-8)', $file) || die  "Couldn't open file: $!\n";
open (OUT, ">$out") || die "Couldn't open outfile\n";
while(<IN>){
	my $line = $_;
	$line =~ s/\n//g;
    $line =~ s/\r//g;
	#my($subid, $samid, $sit, $gen, $racer, $raceo, $age, $height, $weight, $ind, $comorb, $diab, $heart, $valve, $asp, $ty, $hity, $simvastatin, $atorvastatin, $fluvastatin, $lovastatin, $pravastatin, $pravasttin)= split(/\s+/);
	my @features = split(/\t/, $line);
	my $len = scalar(@features);
	print "$len\n";
}
close(IN);
close(OUT);