#! /usr/bin/perl

use strict;
use warnings;
no warnings 'uninitialized';

use Getopt::Long;

# regular P distribution
my %psmap = (
	0 => .5,
	1 => .3,
	2 => .2,
);

# real/extended P distribution
my %xpsmap = (
	0 => .25,
	2 => .05,
	3 => .3,
	6 => .1,
	7 => .2,
	8 => .1,
);

my $numberofrounds = 200;
my $usevariablerounds = 0;
my $useextendedscores = 0;
my $nonindependent = 0;
my $maxscorestoprint = 10;
my $usemod10 = 0;
my $indistinctpairs = 0;
my $printsingleteamscores = 0;
my $leastlikely = 0;
my $help;

GetOptions(
        "rounds:i" => \$numberofrounds,
        "variablerounds!" => \$usevariablerounds,
        "realscores!" => \$useextendedscores,
        "nonindependent!" => \$nonindependent,
        "maxscorestoprint:i" => \$maxscorestoprint,
        "usemod10!" => \$usemod10,
        "indistinctpairs!" => \$indistinctpairs,
        "printsingleteamscores!" => \$printsingleteamscores,
        "leastlikely!" => \$leastlikely,
        "help!" => \$help,
) or die "Invalid arg";

if ($help) {
print "Usage: $0 
	--rounds <int: default $numberofrounds> 
	--variablerounds <boolean: default $usevariablerounds> ends game with 10% chance starting with round int(rounds/2)
	--realscores <boolean: default $useextendedscores> use large score set
	--nonindependent <boolean: default $nonindependent> only useful with variablerounds, calculates team round scores as non-independent events
	--maxscorestoprint <int: default $maxscorestoprint> number of top scores to print
	--usemod10 <boolean: default $usemod10> use only the last digits of scores
	--indistinctpairs <boolean: default $indistinctpairs> scores of 3-4 and 4-3 are counted as the same outcome (and P for that outcome is doubled)
	--printsingleteamscores <boolean: default $printsingleteamscores> print the P of a single team's score
	--leastlikely <boolean: default $leastlikely> print least likely scores instead of most likely
	--help <boolean: default 0> displays this text and bypasses any calculations
";
exit();
}

my %scorepmap;
if ($useextendedscores) {
	%scorepmap = %xpsmap;
}
else {
	%scorepmap = %psmap;
}

my @a = (sort keys %scorepmap);
my $minscoreperround = $a[0];
@a = (reverse sort keys %scorepmap);
my $maxscoreperround = $a[0];

my %calc_p_map = ();

# calculates P(R, S) (probably of score S in round R)
sub calc_p {
	my ($round, $score) = @_;
	
	# if we've recurred into a nonpositive round, return 0
	if ($round <= 0) {
		return 0;
	}
	
	# if we've recurred into a negative score, return 0
	if ($score < 0) {
		return 0;
	};

	# is score possible?  basically, is score / round < max score per round?
	if ($score > ($round * $maxscoreperround)) {
		return 0;
	}
	
	# if we've already calculated this probability, simply return it
	if ($calc_p_map{"$round,$score"} ne '') {
		return $calc_p_map{"$round,$score"};
	}
	
	# if we are in round 1, return probabilities from the table
	if ($round == 1) {
		if ($score <= $maxscoreperround) {
			$calc_p_map{"$round,$score"} = $scorepmap{$score};
			return $scorepmap{$score} || 0;
		}
		else {
			$calc_p_map{"$round,$score"} = 0;
			return 0;
		}
	}
	# otherwise, calculate the probability of a particular score S in this round
	# by iterating through all possible scores N and determining the probability
	# of achieving a score of S - N in the previous round
	else {
		my $p;
		foreach my $s (keys %scorepmap) {
			$p += ($scorepmap{$s} * calc_p($round - 1, $score - $s));
		}
		$calc_p_map{"$round,$score"} = $p;
		return $p;
	}
}

# calculates Pvar(Rstart, Rend, S) where Rstart is first round where game can end with 10% P
# Rend is final round where game ends with 100% P
# in 10 round game where game ends after round 5 with 10% P and every round thereafter,
# Rstart = 5 and Rend = 10
# This should only be used when Pvar for each team is independent
sub calc_p_var {
	my ($startround, $endround, $score) = @_;
	
	if (($startround + 1) > $endround) {
		return 0;
	}
	
	if ($startround + 1 == $endround) {
		return ((.1 * calc_p($startround, $score)) + (.9 * calc_p($startround + 1, $score)));
	}
	else {
		return ((.1 * calc_p($startround, $score)) + (.9 * calc_p_var($startround + 1, $endround, $score)));
	}
}

# calculates Q (or Qvar) for pair-wise scoring for all possible scores
# stores results in hash keys by "score1 - score2"
sub calc_q {
	my ($rounds) = @_;
	
	my %qmap = ();
	foreach my $score1 ($minscoreperround..($rounds * $maxscoreperround)) {
		foreach my $score2 ($minscoreperround..($rounds * $maxscoreperround)) {
			my $scoreindex1;
			my $scoreindex2;
			
			if ($usemod10) {
				$scoreindex1 = ($score1 % 10);
				$scoreindex2 = ($score2 % 10);
			}
			else {
				$scoreindex1 = $score1;
				$scoreindex2 = $score2;
			}
			
			my ($p1, $p2, $q);
			if ($usevariablerounds) {
				if ($nonindependent) {
					$q = calc_q_var(int($rounds/2), $rounds, $score1, $score2);
				}
				else {
					$p1 = calc_p_var(int($rounds/2), $rounds, $score1);
					$p2 = calc_p_var(int($rounds/2), $rounds, $score2);
					$q = $p1 * $p2;
				}
			}
			else {
				$p1 = calc_p($rounds, $score1);
				$p2 = calc_p($rounds, $score2);
				$q = $p1 * $p2;
			}
			
			my $key;
			
			unless ($indistinctpairs) {
				$key = "$scoreindex1 - $scoreindex2";
			}
			else {
				if ($scoreindex1 < $scoreindex2) {
					$key = "$scoreindex1 - $scoreindex2";
				}
				else {
					$key = "$scoreindex2 - $scoreindex1";
				}
			}
			
			$qmap{$key} += $q;
		}	
	}
	
	return %qmap;
}

# helper function for calc_q that calculates Qvar
# in the event that Pvar for each team is nonindependent
sub calc_q_var {
	my ($startround, $endround, $score1, $score2) = @_;
	
	if (($startround + 1) > $endround) {
		return 0;
	}
	
	if ($startround + 1 == $endround) {
		return ((.1 * calc_p($startround, $score1) * calc_p($startround, $score2)) + (.9 * calc_p($startround + 1, $score1) * calc_p($startround + 1, $score2)));
	}
	else {
		return ((.1 * calc_p($startround, $score1) * calc_p($startround, $score2)) + (.9 * calc_q_var($startround + 1, $endround, $score1, $score2)));
	}
}

# prints single-team score probability for all scores (or digits, if using mod 10)
sub print_scores {
	my ($rounds) = @_;
	

	print "P(score) for ($numberofrounds) rounds, single player";
	print " variable end after round ".int($numberofrounds/2) if $usevariablerounds;
	print " using extended scores" if $useextendedscores;
	print " using only last digit of score" if $usemod10;
	print ":\n";

	my %spmap = ();
	for my $score ($minscoreperround..($rounds * $maxscoreperround)) {
		my $scoreindex;
		if ($usemod10) {
			$scoreindex = ($score % 10);
		}
		else {
			$scoreindex = $score;
		}
		if ($usevariablerounds) {
			$spmap{$scoreindex} += calc_p_var(int($rounds/2), $rounds, $score);
		}
		else {
			$spmap{$scoreindex} += calc_p($rounds, $score);
		}
	}
	
	foreach (sort {$a <=> $b} keys %spmap) {
		print "$_: $spmap{$_}\n";
	}
}

sub calc_squares {
	my ($rounds) = @_;
	
	my %squaremap = calc_q($rounds);

	my $sum_p = 0;
	foreach (values %squaremap) {
		$sum_p += $_;
	}

	print "Top $maxscorestoprint P(score-score) for ($numberofrounds) rounds";
	print " variable end after round ".int($numberofrounds/2) if $usevariablerounds;
	print " using extended scores" if $useextendedscores;
	print " using only last digit of score" if $usemod10;
	print " using non-independent pairs" if $nonindependent;
	print " using indistinct pairs" if $indistinctpairs;
	print "\n";
	print "Total P: $sum_p\n";
	
	my $count = 0;
	if ($leastlikely) {
		foreach (sort {$squaremap{$a} <=> $squaremap{$b} || $a cmp $b} keys %squaremap) {
			print "$_: $squaremap{$_}\n" if $_;
			$count++;
			last if $count == $maxscorestoprint;
		}
	}
	else {
		foreach (reverse sort {$squaremap{$a} <=> $squaremap{$b} || $a cmp $b} keys %squaremap) {
			print "$_: $squaremap{$_}\n" if $_;
			$count++;
			last if $count == $maxscorestoprint;
		}
	}
	
}	

if ($printsingleteamscores) {
	print_scores($numberofrounds);
	print "\n";
}

calc_squares($numberofrounds);
print "\n";
