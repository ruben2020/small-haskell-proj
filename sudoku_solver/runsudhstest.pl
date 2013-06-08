
use strict;

my $txt = '';
my $succ = 0;
my $fail = 0;
my $tota = 0;
my $perc = 0;
my $num = 0;

open(FIL, "testpuzzles.txt");
#open(FILO,">resulths.txt");
while(<FIL>)
{
	chomp;
	$txt = `./sudsolv $_`;
	$num++;
	#print FILO $txt;
	$succ++ if ($txt =~ /Success!/);
	$fail++ if ($txt =~ /Failed!/);
	print "Already done $num, succ = $succ, fail = $fail\n" if ($num % 50 == 0);
}
#close(FILO);
close(FIL);

$tota = $succ + $fail;
$perc = $succ*100/$tota;
print "\nSuccess = $succ / $tota\nPercentage = $perc %\n";

