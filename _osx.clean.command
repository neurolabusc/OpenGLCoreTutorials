#!/bin/sh

# change to working directory to location of command file: http://hints.macworld.com/article.php?story=20041217111834902
here="`dirname \"$0\"`"
cd "$here" || exit 1

#remove Cocoa as widgetset
sed -i.bak '/BuildModes Active="MacOS"/d' ./basic.lps
sed -i.bak '/BuildModes Active="MacOS"/d' ./cubepro.lps
sed -i.bak '/BuildModes Active="MacOS"/d' ./render.lps
sed -i.bak '/BuildModes Active="MacOS"/d' ./tex.lps
sed -i.bak '/BuildModes Active="MacOS"/d' ./text.lps

rm *.~*
rm  DS_STORE
rm *.dsm
rm *.bak
rm -rf lib
rm -rf backup



cd legacy

sed -i.bak '/BuildModes Active="MacOS"/d' ./textLegacy.lps

rm *.~*
rm  DS_STORE
rm *.dsm
rm *.bak
rm -rf lib
rm -rf backup

