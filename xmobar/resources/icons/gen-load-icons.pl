#!/usr/bin/env perl

use warnings;
use strict;

use Image::Xpm;
use POSIX;

my $iwidth     = 8;
my $iheight    = 24;
my $pixels_per = 2;

my $color_none = "none";
my $color_bg   = "#1d2021";
my $color_fg_h = "#cc241d";
my $color_fg_m = "#d79921";
my $color_fg_l = "#98971a";


for ( my $file_num = 0; $file_num <= 8; $file_num++ ) {
    my $fname = "load_$file_num.xpm";
    unlink $fname;
    my $i = Image::Xpm->new(-file => $fname, -width => $iwidth, -height => $iheight);

    my $h = $file_num * $pixels_per;

    for ( my $x = 0; $x < $iwidth; $x++ ) {
        for ( my $y = 0; $y < $iheight; $y++ ) {
            if ($y > $iheight - 6 || $y < 2) {
                $i->xy($x, $y, $color_none);
            }
            else {
                $i->xy($x, $y, $color_bg);
            }
        }
    }

    for ( my $x = 0; $x < $iwidth; $x++ ) {
        for ( my $y = ($iheight - 6); $y >= ($iheight - 6 - $h); $y-- ) {
            my $col = do {
                if ($file_num <= 2) {
                    $color_fg_l;
                } elsif ($file_num <= 5) {
                    $color_fg_m;
                } else {
                    $color_fg_h;
                }
            };
            $i->xy($x, $y, $col);
        }
    }

    $i->save;
}
