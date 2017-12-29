(UTF8 encoding)
V2.01 : Update                               2017/12/27

. Changed OCR library from "SharePoint Designer 2007" MODI
  to "Microsoft OCR Llibrary for Windows".
  Windows10 Version 1511 (2015/11) is required.
  .Net framework version is 4.6.1
. Changed install method from ClicOnce to msi.
. Supports partial extracting in the box by mouse dragging.
. Supports Non-voiced and Voiced Letter replacement by F4.
. Support FrequentWords dialog also on kanaText form
  and took back some shortcut key(Ctrl+Alpha) from system default
  to "FrequentWords" dialog.

V1.15 : Update                               2017/10/27

. Place "Send" button to each lines on Symbol Input Dialog.
. Bug of Cut & Paste on Words Definition Dialog.

V1.14 : Update                               2017/09/27

. Support Kanji file of encoding = UTF-8.
. Words Definition support.
    On Kanji text form, register those words repeatedly used in the document.
    Candidates are kanji character sometimes misrecognition occurs
    at extracting from image, e.g. "三" may be recognized as "一二" 
    When "Key"(One Alphabet Char,Case InSensitive) is registered,
    Ctrl+Key sends "Phrase" to text box.
    When the dialog is opened, "Send" button also send the "Phrase".
. Miscellaneous.
   .Cursor and Caret shape was changed.
   .(Bug)It may scroll to top by Enter key at Kanji Form.
   .(Bug)Form size is not restored at re-open.

V1.03 : Update                               2017/09/23

・Added some characters resembling in shape for Char-Replacing-Key(F5 as Default), 
    ソ(katakana:so) <--> ン(katakana:n)
    十(kanji) <--> ＋(Plus) 

V1.02 : Update                               2017/09/20

・(Bug)Translating line by line on kanji text caused errornouse output at end of line.
・Collectly translates such words  as 3ヵ月,霞ヶ浦 or 三ツ which includes カケヵヶ or ツッ.
・Supports simple dictionary.
・Added some characters resembling in shape for Char-Replacing-Key(F5 as Default), 
  extracting character from image sometimes mis-understands.
  Confirmation Key(Default:F4) reports the charcter type.
    Ri:リ(katakana)<-->り(hiragana)
    He:ヘ(katakana)<-->へ(hiragana)
    Be:ベ(katakana)<-->べ(hiragana)
    工(業:kougyou=industry:kanji) --> エ(ネルギー:e=Energy:katakana) -->ェ(small e:katakana) -->工(業) (back to kanji)
    力(士:rikisi=Sumo wrestler:kanji) --> カ(エル:kaeru=frog:katakana) -->ヵ(small ka:katakana) --> 力(士) (back to kanji)
    夕(焼け:yuuyake=sunset glow:kanji) <--> タ(オル:taoru=towel:katakana)                  
    一(kanji) --> ー(katakana) --> ─(HorizontalBar) --> 一(kanji)  (WrapAround)
    夕(kanji) <--> タ(katakana)                                     
    二(kanji) <--> ニ(katakana) 
    八(kanji) <--> ハ(katakana) 
    口(kanji) <--> ロ(katakana) 

*********************************************************************************************

V1.01 : 2017/06/21 (1st release)

Expand zip file, then execute setup.exe to install, the certificate is test version and issues unknown publisher.
It is installable only once if version number is same.

By "TextExtraction function" + "Kana translation function",
generate Japanese Kana file for WinBES99 extracting text from document file
created by scanner device.
English Document can be extracted, and "Kana translation" do nothing paticularly.

Kana is a collection of Japanese Letter excludes Kanji.
WinBES99 is a text translation tool into Braille.
Windows app "Windows Fax and Scan" can be used to create image file. 
Application may crash depending quality of image file, in that case try re-scan the document.
And displayiong some number of image files may cause memory shortage.

Push Help button on top panel to see usage.

("Microsoft Office Document Imaging" or "MicrosoftOffice Document Scanning"
also enable to scan documents and save to image file, and send extracted text string to "Microsoft Word".)
"Microsoft OneNote"(Free) also supports extracting text from document file.

GitHub sakachin2/VBI2KWRT contains MSVS2017Community project source

Please send your suggestion or bug reports to mail:sakachin2@yahoo.co.jp

