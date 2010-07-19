#!/usr/bin/perl
use Modern::Perl;
use autodie;
use String::Util qw/ crunch /;

my $text_file = shift;

if (! $text_file || ! -f $text_file ) {
    say "Nothing to compile";
    exit 1;
}

open my $P, '<', $text_file;
my @program = map crunch($_),<$P>;
close $P;

my $opcode_table = opcode_table();

my @opcodes = map { token2opcode($_, $opcode_table) } @program;
say "OPCODES: ", join(', ', @opcodes);
my $binary = twelve_bit_pack(@opcodes);

(my $out_file = $text_file) =~ s/\..*?$//;
$out_file .= '.picc';
open my $O, '>', $out_file;
print $O "$binary";
close $O;

exit(0);

sub token2opcode {
    my $t = shift; # format is "OPCODE ARG1,? ARG2?"
    my $opcode_table = shift;
    my ($op, @args) = extract_token($t);
    my $sub = $opcode_table->{$op};
    return $sub->(@args);
}

sub extract_token {
    my $t = shift;

    my ($op, $args) = $t =~ /^ \s* (\S+) \s+ (\S.*?) \s* $/x;
    $args =~ s/\s+//g;
    my @args = split /,/, $args;
    return ($op, @args);
}

sub twelve_bit_pack {
    my @bits = @_; # List of 12-bit values
    if ( @bits % 2 != 0 ) {
        push @bits, 0; # A no-op bit at the end
    }
    my $r;
    my @bits8;
    while ( my @vals = splice(@bits,0,2) ) {
        my $new_val = ($vals[0] << 12) | $vals[1];
        push @bits8, ($new_val & (0b1111_1111 << 16)) >> 16;
        push @bits8, ($new_val & (0b1111_1111 <<  8)) >> 8;
        push @bits8,  $new_val & (0b1111_1111 <<  0);
    }
    say "BITS: <<",join(',',map{"$_:8"}@bits8),'>>';
    pack('C*', @bits8);
}

sub opcode_table {
    my %opcode_table = (
        NOP    =>  '0000_0000_0000',
        OPTION =>  '0000_0000_0010',
        SLEEP  =>  '0000_0000_0011',
        CLRWDT =>  '0000_0000_0100',
        TRIS   =>  '0000_0000_01ff',
        MOVWF  =>  '0000_001_fffff',
        CLRW   =>  '0000_010_00000', # '0000_010_xxxxx',
        CLRF   =>  '0000_011_fffff',
        SUBWF  =>  '0000_10d_fffff',
        DECF   =>  '0000_11d_fffff',
        IORWF  =>  '0001_00d_fffff',
        ANDWF  =>  '0001_01d_fffff',
        XORWF  =>  '0001_10d_fffff',
        ADDWF  =>  '0001_11d_fffff',
        MOVF   =>  '0010_00d_fffff',
        COMF   =>  '0010_01d_fffff',
        INCF   =>  '0010_10d_fffff',
        DECFSZ =>  '0010_11d_fffff',
        RRF    =>  '0011_00d_fffff',
        RLF    =>  '0011_01d_fffff',
        SWAPF  =>  '0011_10d_fffff',
        INCFSZ =>  '0011_11d_fffff',
        BCF    =>  '0100_bbb_fffff',
        BSF    =>  '0101_bbb_fffff',
        BTFSC  =>  '0110_bbb_fffff',
        BTFSS  =>  '0111_bbb_fffff',
        RETLW  =>  '1000_kkkk_kkkk',
        CALL   =>  '1001_kkkk_kkkk',
        GOTO   =>  '101_kkkkk_kkkk',
        MOVLW  =>  '1100_kkkk_kkkk',
        IORLW  =>  '1101_kkkk_kkkk',
        ANDLW  =>  '1110_kkkk_kkkk',
        XORLW  =>  '1111_kkkk_kkkk',
    );

    for my $key (keys %opcode_table) {
        $opcode_table{$key} = pattern2sub( $opcode_table{$key} );
    }

    return \%opcode_table;
}

sub pattern2sub {
    my $pattern = shift;
    $pattern =~ s/_//g;

    if ( $pattern =~ /^([01]+)(b+|d)?(f+|k+)?/ ) {
        my ($op, $second_arg, $first_arg) = ($1,$2,$3);
        $second_arg ||= '';

        my $f_width = length($first_arg);
        my $s_width = length($second_arg);

        if (!$first_arg && !$second_arg) {
            return sub { oct("0b$op") };
        }

        return sub {
            my ($a1,$a2) = @_;
            if ( $a1 > 1 << $f_width ) { die "Arg1 too big"  }
            if ( $second_arg && !defined($a2) ) { die "Need two args" }
            if ( $a2 && $a2 > 1 << $s_width ) { die "Arg2 too big"  }

            my $format_str = "0b$op"
                           . ( $s_width ? "%0$s_width".'b' : '' )
                           . "%0$f_width".'b';
            return oct(sprintf($format_str, ($s_width ? ($a2,$a1) : $a1) ));
        };
    }
    else {
        say "Can't parse $pattern";
    }
}

