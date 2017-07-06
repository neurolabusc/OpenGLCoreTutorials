#!/bin/sh

# change to working directory to location of command file: http://hints.macworld.com/article.php?story=20041217111834902
here="`dirname \"$0\"`"
cd "$here" || exit 1

rm basic
rm cubepro
rm render
rm tex
rm text


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

rm basicLegacy
rm texLegacy
rm textLegacy
rm cubeLegacy

sed -i.bak '/BuildModes Active="MacOS"/d' ./basicLegacy.lps
sed -i.bak '/BuildModes Active="MacOS"/d' ./cubeLegacy.lps
sed -i.bak '/BuildModes Active="MacOS"/d' ./texLegacy.lps
sed -i.bak '/BuildModes Active="MacOS"/d' ./textLegacy.lps

rm *.~*
rm  DS_STORE
rm *.dsm
rm *.bak
rm -rf lib
rm -rf backup

