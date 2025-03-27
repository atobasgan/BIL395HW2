{\rtf1\ansi\ansicpg1254\cocoartf2821
\cocoatextscaling0\cocoaplatform0{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
{\*\expandedcolortbl;;}
\paperw11900\paperh16840\margl1440\margr1440\vieww11520\viewh8400\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural\partightenfactor0

\f0\fs24 \cf0 use strict;\
use warnings;\
\
my %vars;\
\
while (1) \{\
    print "Aritmetik bir ifade giriniz: ";\
    my $input = <STDIN>;\
\
\
\
    chomp($input);\
\
    if ($input eq "exit") \{\
        last;\
    \}\
\
    \
    if ($input =~ /^\\s*([a-zA-Z]\\w*)\\s*=\\s*(.+)$/) \{\
        my ($var, $expr) = ($1, $2);\
\
        $expr =~ s/([a-zA-Z]\\w*)/\
            exists $vars\{$1\} ? $vars\{$1\} : die "Hata: '$1' tanimli degil\\n" /ge;\
\
        my $val = eval $expr;\
        if ($@) \{\
            print "Hata: Gecersiz ifade ($@)\\n";\
        \} else \{\
            $vars\{$var\} = $val;\
            print "$var = $val\\n";\
        \}\
\
    \} else \{\
        my $expr = $input;\
        $expr =~ s/([a-zA-Z]\\w*)/\
            exists $vars\{$1\} ? $vars\{$1\} : die "Hata: '$1' tanimli degil\\n" /ge;\
\
        my $result = eval $expr;\
        if ($@) \{\
          \
          if ($@ =~ /division by zero/) \{\
            \
            print "Hata: 0'a bolme!\\n";\
          \} else \{\
            print "Hata: Ge\'e7ersiz ifade ($@)\\n";\
          \}\
        \} else \{\
            print "Sonu\'e7: $result\\n";\
        \}\
    \}\
\}}