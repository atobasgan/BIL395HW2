use strict;
use warnings;

my %vars;

while (1) {
    print "Aritmetik bir ifade giriniz: ";
    my $input = <STDIN>;

    chomp($input);

    if ($input eq "exit") {
        last;
    }

    if ($input =~ /^\s*([a-zA-Z]\w*)\s*=\s*(.+)$/) {
        my ($var, $expr) = ($1, $2);

        $expr =~ s/([a-zA-Z]\w*)/
            exists $vars{$1} ? $vars{$1} : die "Hata: '$1' tanimli degil\n" /ge;

        my $val = eval $expr;
        if ($@) {
            print "Hata: Gecersiz ifade ($@)\n";
        } else {
            $vars{$var} = $val;
            print "$var = $val\n";
        }

    } else {
        my $expr = $input;
        $expr =~ s/([a-zA-Z]\w*)/
            exists $vars{$1} ? $vars{$1} : die "Hata: '$1' tanimli degil\n" /ge;

        my $result = eval $expr;
        if ($@) {
            if ($@ =~ /division by zero/) {
                print "Hata: 0'a bolme!\n";
            } else {
                print "Hata: Geçersiz ifade ($@)\n";
            }
        } else {
            print "Sonuç: $result\n";
        }
    }
}
