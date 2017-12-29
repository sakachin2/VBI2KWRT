'CID:''+v120R~:#72                          update#=  270;            ''+v120R~
'************************************************************************************''~v026I~
'v120 2017/12/27 Msg:Not valid Ctrl+x is overridden by "" by display the char is target of F5 replacement''+v120I~
'v115 2017/12/26 support dakuon,handakuon key                          ''~v115R~
'v105 2017/12/20 display char type not by f4 but automatically by keyup/mouseclick event''~v105I~
'v101 2017/12/16 Conversion warning                                    ''~v052I~
'v053 2017/09/21 crash by F4,S+F5 at Form1 by V1.02                    ''~v053I~
'v052 2017/09/21 utilize status bar at bottom also on Form1            ''~v052I~
'v037 2017/09/22 assign F4 as query of replacing char                  ''~v037I~
'                Forecolor have to be InactiveCaptureText to get effective for switching Text by language''~v037I~
'v036 2017/09/22 add for F5 ニ,ハ,ロ,ー,ト                             ''~v036I~
'v034 2017/09/21 utilize status bar at bottom for result of F5 on Form5''~v034I~
'v026 2017/09/19 By F5,"り"(hiragana)<-->"リ"(katakana),"工"(kanji)-->"エ"(katakana)-->"ェ"(katakana-small)-->"工" (wrap),"力"(kanji)-->"カ"(katakana)-->"ヵ"(katakana)-->"力"(Wrap)''~v026I~
'************************************************************************************''~v026I~
Public Class FormatBES                                                 ''~7421I~
    'localization done                                                     ''~7618I~

    '   Private Const SIGN_DIGIT = ChrW(&H2460) 'maru-1 "数"c     '&H6570                    ChrW(&H2460)        'maru-1''~7422R~''~7427R~''~7501R~
    Private Const SIGN_DIGIT = "数"c                                           ''~7427I~''~7501R~
    '   Private Const SIGN_ALPHA = ChrW(&H24E9) 'Maru-z "外"c     'ChrW(&H24B6)        'maru-A               ''~7414R~''~7421M~''~7427R~''~7501R~
    Private Const SIGN_ALPHA = "外"c                                           ''~7427I~''~7501R~
    '   Private Const SIGN_UPPER = ChrW(&H24B6) 'Maru-A "大"c     'ChrW(&H5927)                          ''~7422R~''~7427R~''~7501R~
    Private Const SIGN_UPPER = "大"c                                           ''~7427I~''~7501R~
    '   Private Const SIGN_UPPER_END = ChrW(&H24D0) 'maru-a "小"c 'ChrW(&H24d0)    'maru-a''~7427R~''~7501R~
    '   Private Const SIGN_UPPER_END = "小"c                                       ''~7427I~''~7501R~''~7508R~
    Private Const SIGN_UPPER_END = SIGN_ALPHA                          ''~7508I~
    Public Const SIGN_CRLF = ChrW(&H23CE)                                            ''~7413I~''~7414R~''~7421M~''~7501R~''~7502R~
    Public Const SIGN_EOL = "☒"c  'ChrW(&H2612)       'X in rectangle               ''~7421I~''~7501R~''~7509R~
    Public Const SIGN_CHAR_NOTHING = ChrW(&H0)                                ''~7423I~''~7501R~
    Private Const SIGN_TEMPSPACE1 = "単"c   'ChrW(&HFF1F)    '"?" for splitter of digit >4''~7423I~''~7427R~''~7501R~
    Private Const SIGN_TEMPSPACE2 = "位"c   'ChrW(&HFF1F)    '"?" for splitter of digit >4''~7427I~''~7501R~
    Public Const SIGN_NOCHAR = ChrW(&HFE4F)  'wave underscore                 ''~7426R~''~7501R~''~7509R~
    '   Private Const SIGN_TENJI_SBCSPREFIX = ChrW(&Ha0)    '"?" for splitter of digit >4''~7423R~''~7426R~''~7501R~
    '   Private Const SIGN_CONCAT1 = ChrW(&H24F5)      '1st tsunagi=2maru-1         ''~7426I~''~7427R~''~7501R~
    '   Private Const SIGN_CONCAT1 = "継"c                                         ''~7427I~''~7501R~''~7513R~
    Private Const SIGN_CONCAT1 = ChrW(&H2192)      'right allow        ''~7513I~
    '   Private Const SIGN_DAKUON = "濁"c                                         ''~7427I~''~7501R~''~7513R~
    '   Private Const SIGN_DAKUON = ChrW(&H2810)    'tenji                 ''~7513R~
    '   Private Const SIGN_DAKUON = ChrW(&H25cf)    'small black circle    ''~7513R~
    Private Const SIGN_DAKUON = ChrW(&H25BD)    'white down triangle   ''~7513I~
    Private Const SIGN_SPACE_BEFORE_SPECIAL_CHAR = "記"c                        ''~7427I~''~7501R~
    Private Const SIGN_SPACE = ChrW(&H2610)        'white rectangle            ''~7426R~''~7501R~
    Public Const CHAR_SPACE = ChrW(&H3000)                                    ''~7426I~''~7501R~''~7525R~
    Public Const CHAR_SPACE_SBCS = " "c                               ''~7525I~''~7604R~
    Private Const CHAR_PERIOD = ChrW(&HFF0E)                           ''~7501R~
    Private Const CHAR_COMMA = ChrW(&HFF0C)                            ''~7501R~
    Public Const CHAR_CR = ChrW(&HD)                                         ''~7426I~''~7501R~''~7509R~
    Public Const CHAR_LF = ChrW(&HA)                                         ''~7426I~''~7501R~''~7509R~
    Public Const CHAR_KUTEN = "。"c                                           ''~7427I~''~7501R~''~7604R~
    Public Const CHAR_TOUTEN = "、"c                                          ''~7427I~''~7501R~''~7604R~
    Public Const CHAR_KUTEN_HANKANA = "｡"c                            ''~7604I~
    Public Const CHAR_TOUTEN_HANKANA = "､"c                           ''~7604I~
    Public Const CHAR_CHUTEN = "・"c                                          ''~7427I~''~7501R~''~7605R~
    Public Const CHAR_CHUTEN_HANKANA = "･"c                           ''~7605I~
    Private Const SIGN_TENJISPACE = "＠"                                        ''~7427I~''~7501R~
    Public Const CHAR_EXCLAMATION = "！"                                      ''~7430I~''~7501R~''~7605R~
    Public Const CHAR_EXCLAMATION_ASCII = "!"                          ''~7605I~
    Public Const CHAR_QUESTION = "？"                                      ''~7430I~''~7501R~''~7605R~
    Public Const CHAR_QUESTION_ASCII = "?"                            ''~7605I~
    ''~v026I~
    Public Const CHAR_KANJI_KOU = "工"c                                ''~v026I~
    Public Const CHAR_KATAKANA_E = "エ"c                               ''~v026I~
    Public Const CHAR_KATAKANA_E_SMALL = "ェ"c                         ''~v026I~
    Public Const CHAR_KANJI_RIKI = "力"c                                ''~v026I~
    Public Const CHAR_KATAKANA_KA = "カ"c                              ''~v026I~
    Public Const CHAR_KATAKANA_KA_SMALL = "ヵ"c                        ''~v026I~
    Public Const CHAR_KATAKANA_RI = "リ"c                              ''~v026I~
    Public Const CHAR_HIRAGANA_RI = "り"c                              ''~v026I~
    Public Const CHAR_KATAKANA_HE = "ヘ"c                              ''~v026I~
    Public Const CHAR_HIRAGANA_HE = "へ"c                              ''~v026I~
    Public Const CHAR_KATAKANA_BE = "ベ"c                              ''~v026I~
    Public Const CHAR_HIRAGANA_BE = "べ"c                              ''~v026I~
    Public Const CHAR_KANJI_TA = "夕"c                              ''~v026I~
    Public Const CHAR_KATAKANA_TA = "タ"c                              ''~v026I~
    Public Const CHAR_KANJI_NI = "二"c                                 ''~v034I~
    Public Const CHAR_KATAKANA_NI = "ニ"c                              ''~v034I~
    Public Const CHAR_KANJI_HACHI = "八"c                              ''~v034I~
    Public Const CHAR_KATAKANA_HA = "ハ"c                              ''~v034I~
    Public Const CHAR_KANJI_KUCHI = "口"c                              ''~v034I~
    Public Const CHAR_KATAKANA_RO = "ロ"c                              ''~v034I~
    Public Const CHAR_KANJI_ICHI = "一"c                               ''~v034I~
    Public Const CHAR_KATAKANA_CHOON = "ー"c                           ''~v034I~
    Public Const CHAR_KIGO_HBAR = "─"c                                ''~v034I~
    Public Const CHAR_KATAKANA_TO = "ト"c                           ''~v036I~
    Public Const CHAR_KANJI_BOKU = "卜"c                               ''~v036I~
    Public Const CHAR_KIGO_PLUS = "＋"c                                ''~v037I~
    Public Const CHAR_KANJI_JU = "十"c                                 ''~v037I~
    Public Const CHAR_KATAKANA_N = "ン"c                               ''~v037I~
    Public Const CHAR_KATAKANA_SO = "ソ"c                              ''~v037I~
    ''~v026I~
    '   Private Const CHAR_CONCAT1 = ChrW(&H28C0)  '1st tsunagi                  ''~7423I~''~7426R~''~7501R~
    Private Const UC_ASCII_START = &H20   'space                       ''~7501I~
    Private Const UC_ASCII_END = &H7E   '~                           ''~7501I~
    Private Const UC_ASCII_START_DBCS = &HFF00     'dbcs space         ''~7501I~
    Private Const UC_DIGIT0_SBCS = &H30          'ascii-0                    ''~7414R~''~7421M~''~7501R~
    Private Const UC_DIGIT9_SBCS = &H39          'ascii-9                    ''~7414R~''~7421M~''~7501R~
    Private Const UC_SBCS2DBCS = &HFF00                                        ''~7423I~''~7501R~
    Private Const UC_DIGIT0_DBCS = &HFF10        'dbcs-0                     ''~7414R~''~7421M~''~7501R~
    Private Const UC_DIGIT9_DBCS = &HFF19        'dbcs-9                     ''~7414R~''~7421M~''~7501R~
    '    Private Const UC_A_LOWER = &H61          'a                          ''~7414R~''~7421M~''~7427R~''~7501R~
    '    Private Const UC_Z_LOWER = &H7A          'z                          ''~7414R~''~7421M~''~7427R~''~7501R~
    '    Private Const UC_A_UPPER = &H41          'a                          ''~7414R~''~7421M~''~7427R~''~7501R~
    '   Private Const UC_Z_UPPER = &H5A          'z                          ''~7414R~''~7421M~''~7427R~''~7501R~
    Private Const UC_SPACE_SBCS = &H20             ' " "                       ''~7427I~''~7501R~
    Private Const UC_A_LOWER_DBCS = &HFF41          'a-dbcs                    ''~7423I~''~7501R~
    Private Const UC_Z_LOWER_DBCS = &HFF5A          'a-dbcs                    ''~7427I~''~7501R~
    Private Const UC_A_UPPER_DBCS = &HFF21          'A-dbcs                    ''~7423I~''~7501R~
    Private Const UC_Z_UPPER_DBCS = &HFF3A          'A-dbcs                    ''~7427I~''~7501R~
    Private Const UC_ZENKATA_START = &H30A1  'A-small                          ''~7414R~''~7421M~''~7501R~
    Private Const UC_ZENKATA_END = &H30F3 '&H30F4  'VU                             ''~7414R~''~7421M~''~7427R~''~7428R~''~7501R~
    Private Const UC_KANA_START = &H3041  'A-small                             ''~7427I~''~7501R~
    Private Const UC_KANA_END = &H3093 '&H3094  'VU                                    ''~7427I~''~7428R~''~7501R~
    Private STR_ZENKATA2 As String = "ヵヶヽヾ"                            ''~7427R~''~7430R~
    '   private STR_ZENKATA22HIRA As String = "かけゝゞ"                       ''~7427R~''~7428R~''~7430R~
    Private STR_ZENKATA22HIRA As String = "ヵヶゝゞ"                       ''~7428R~''~7430R~
    Private STR_ZENKATA3 As String = "ヴヷヸヹヺ"                            ''~7427I~''~7428R~''~7430R~
    '   private STR_ZENKATA32HIRA As String = "わゐゑを"                       ''~7427I~''~7428R~''~7430R~
    Private STR_KANA2 As String = "・ーゝゞ"                               ''~7427I~''~7430R~
    Private STR_KANA22ZENKATA As String = "・ーヽヾ"                       ''~7428I~''~7430R~
    Private STR_KANA3 As String = "。、"                                   ''~7427I~''~7430R~
    Private Const UC_ZENKATA3_DAKUON = "゛"                                     ''~7427I~''~7501R~
    Private Const UC_HANKATA_START = &HFF66  'wo                               ''~7414R~''~7421M~''~7427R~''~7501R~
    Private Const UC_HANKATA_END = &HFF9D  'n                                ''~7414R~''~7421M~''~7501R~
    Private Const UC_HANKATA_DAKUON = &HFF9E  '                                ''~7414R~''~7421M~''~7428R~''~7501R~
    Private Const CHAR_HANKATA_DAKUON = ChrW(UC_HANKATA_DAKUON)                  ''~7427I~''~7428R~''~7501R~
    Private Const UC_HANKATA_HANDAKUON = &HFF9F  '                             ''~7414R~''~7421M~''~7428R~''~7501R~
    Private Const CHAR_HANKATA_HANDAKUON = ChrW(UC_HANKATA_HANDAKUON)             ''~7427I~''~7428R~''~7501R~
    Private Const CHAR_HIRA_DAKUON = "゛"c                                     ''~7428I~''~7501R~
    Private Const CHAR_HIRA_HANDAKUON = "゜"c                                  ''~7428I~''~7501R~
    Private Const CHAR_DQUOTE_DBCS = ChrW(&HFF02)                       ''~7501I~
    Private STR_HANKATA2 As String = "｡｢｣､･ﾞﾟ"                               ''~7427I~''~7428R~''~7430R~
    Private STR_HANKATA22HIRA As String = "。「」、・゛゜"                     ''~7427I~''~7428R~''~7430R~
    Private STR_PUNCT As String = " !""#$%&'()*+,-./:;<=>?@[\]^_`{|}~" ''~7423I~''~7430R~
    '   Private STR_PUNCT_DBCS As String = "　！＂＃＄％＆’（）＊＋，－．／：；＜＝＞？＠［￥］＾＿｀｛｜｝～" ''~7423R~''~7427R~''~7430R~''~7501R~
    Private STR_PUNCT_DBCS As String = "　！" & CHAR_DQUOTE_DBCS & "＃＄％＆’（）＊＋，－．／：；＜＝＞？＠［￥］＾＿｀｛｜｝～" ''~7501I~
    '   private STR_2TENJI As String = "：？＝＜＞｀＿～"                      ''~7427R~''~7428R~''~7430R~
    Private STR_2TENJI As String = "％＆＃＊：—―／［］＝＜＞＠￥｛｝～｀＿÷" ''~7428R~''~7430R~
    Private Const STR_CHK_FONTW = "あいうえお"                                 ''~7421I~''~7501R~
    Private Const STR_DIGIT2KANA = "あいうえおらりるれろ"                      ''~7421I~''~7501R~
    Private Const STR_DIGIT2ZENKATA = "アイウエオラリルレロ"                   ''~7421I~''~7501R~
    Private Const STR_DIGIT2HANKATA = "ｱｲｳｴｵﾗﾘﾙﾚﾛ"                             ''~7421I~''~7501R~
    Private STR_HANKATA2HIRA As String = "をぁぃぅぇぉゃゅょっーあいうえおかきくけこさしすせそたちつてとなにぬねのはひふへほまみむめもやゆよらりるれろわん" ''~7421I~''~7427R~''~7430R~
    Private STR_HANDAKUON_SRC As String = "はひふへほ"                             ''~7421I~''~7427R~''~7430R~
    Private STR_HANDAKUON_SRC2 As String = "ばびぶべぼ"     '*babibubebo''~v115R~
    Private STR_HANDAKUON_TGT As String = "ぱぴぷぺぽ"                             ''~7421I~''~7427R~''~7430R~
    Private STR_HANDAKUON_SRC_KATAKANA As String = "ハヒフヘホ"        ''~v115R~
    Private STR_HANDAKUON_SRC2_KATAKANA As String = "バビブベボ"     '*babibubebo''~v115R~
    Private STR_HANDAKUON_TGT_KATAKANA As String = "パピプペポ"      '*papipupepo''~v115R~
    Private STR_HANDAKUON_SRC_HANKANA As String = "ﾊﾋﾌﾍﾎ"              ''~v115R~
    Public Shared STR_DAKUON_SRC As String = "かきくけこさしすせそたちつてとはひふへほうわゐゑを"                     ''~7421I~''~7427R~''~7428R~''~7430R~''~7525R~''~7607R~
    Public Shared STR_DAKUON_SRC_KATAKANA As String = "カキクケコサシスセソタチツテトハヒフヘホウワヰヱヲ" ''~7525I~''~7607R~
    '   private STR_DAKUON_TGT As String = "がぎぐげござじずぜぞだぢづでどばびぶべぼヴ" ''~7421I~''~7427R~''~7428R~''~7430R~
    Public Shared STR_DAKUON_TGT As String = "がぎぐげござじずぜぞだぢづでどばびぶべぼヴヷヸヹヺ" ''~7428R~''~7430R~''~7525R~''~7607R~
    Public Shared STR_DAKUON_TGT_KATAKANA As String = "ガギグゲゴザジズゼゾダヂヅデドバビブベボヴヷヸヹヺ" ''~7525I~''~7607R~
    Private STR_COMPOSIT2 As String = "ゃゅょぁぃぅぇぉ"                   ''~7427I~''~7430R~
    Private STR_SMALL_LETTER_HIRA As String = "ぁぃぅぇぉゃゅょっゎゕゖ" ''~7501R~''~7525R~
    Private STR_SMALL_LETTER_KATA As String = "ァィゥェォャュョッヮヵヶ" ''~7501R~''~7525R~
    Private STR_SMALL_LETTER_HANKATA As String = "ｧｨｩｪｫｬｭｮｯ"           ''~7501I~
    Private STR_LARGE_LETTER_HIRA As String = "あいうえおやゆよつわかけ"   ''~7501I~''~7525R~
    Private STR_LARGE_LETTER_KATA As String = "アイウエオヤユヨツワカケ"  ''~7501I~''~7525R~
    Private STR_LARGE_LETTER_HANKATA As String = "ｱｲｳｴｵﾔﾕﾖﾂ"           ''~7501I~
    Private STR_SMALL_LETTER As String = STR_SMALL_LETTER_HIRA & STR_SMALL_LETTER_KATA & STR_SMALL_LETTER_HANKATA ''~7501R~
    Private STR_LARGE_LETTER As String = STR_LARGE_LETTER_HIRA & STR_LARGE_LETTER_KATA & STR_LARGE_LETTER_HANKATA ''~7501R~
    Private STR_LARGE_LETTER_OTHER As String = "うわえ"                  ''~7525I~''~7605R~
    Private STR_SMALL_LETTER_OTHER As String = "ーはへ"                  ''~7525I~''~7605R~
    '   Public Shared STR_REPEAT = "々ゝゞヽヾ" 'U3005,U309d,U309e,u30fd,u30fe   ''~7525I~''~7607R~''~v101R~
    Public Shared STR_REPEAT As String = "々ゝゞヽヾ" 'U3005,U309d,U309e,u30fd,u30fe''~v101R~
    Public Const STR_REPEAT_INDEX_KANJI = 0             ''~7608I~
    Public Const STR_REPEAT_INDEX_HIRAGANA = 1             ''~7608I~
    Public Const STR_REPEAT_INDEX_HIRAGANA_DAKUON = 2             ''~7608I~
    Public Const STR_REPEAT_INDEX_KATAKANA = 3             ''~7608I~
    Public Const STR_REPEAT_INDEX_KATAKANA_DAKUON = 4             ''~7608I~
    Private Const MAX_DIGIT_CONTINUE = 4                                         ''~7423I~''~7501R~
    Private posInTheSplitLine As Integer = -1                            ''~7508I~
    Private posInTheConcatLine As Integer = -1                           ''~7508R~
    Private posHirakataIn As Integer = -1                                ''~7508I~
    Private posHirakataOut As Integer = -1                               ''~7508I~
    Private typeSrc, typeTgt As Integer                                 ''~v034I~
    Private Const TYPE_KANJI = 1                                         ''~v034R~
    Private Const TYPE_HIRA = 2                                          ''~v034R~
    Private Const TYPE_KATA = 4                                          ''~v034R~
    Private Const TYPE_KATA_SMALL = 3                                    ''~v034R~
    Private Const TYPE_KIGO = 5                                          ''~v036R~
    '*******************************************************               ''~7413I~''~7421M~
    Private sbOut, sbSave As System.Text.StringBuilder                             ''~7413I~''~7421R~''~7426R~
    Private cvText, saveText As String                                               ''~7413I~''~7421R~''~7426R~
    Private posOut, posTOL, posText, textSZ, lineWidth As Integer                           ''~7421I~''~7422R~
    Private swCurrentKatakana As Boolean = False                        ''~7501I~
    '*******************************************************           ''~7421I~
    Public Function splitLine(Ptext As String, Ppos As Integer, ByRef Ppposnext As Integer) As String ''~7508I~
        posInTheConcatLine = Ppos                                        ''~7508I~
        posInTheSplitLine = -1                                           ''~7508I~
        Dim txt As String = splitLine(Ptext)                             ''~7508I~
        posInTheConcatLine = -1                                          ''~7508I~
        Ppposnext = posInTheSplitLine                                    ''~7508I~
        Return txt                                                     ''~7508I~
    End Function                                                       ''~7508I~
    Public Function splitLine(Ptext As String) As String                             ''~7412I~''~7413R~''~7421R~
        Dim len, pos As Integer                        ''~7413R~''~7420R~''~7421I~
        lineWidth = Form1.MainForm.lineWidth                         ''~7421I~''~7521R~
        cvText = Ptext                                                 ''~7413I~''~7421M~
        sbOut = New System.Text.StringBuilder(Ptext.Length * 3)                        ''~7413I~''~7421R~''~7422R~''~7423R~
        textSZ = Ptext.Length()                                        ''~7412I~''~7413R~''~7421I~
        posText = 0                                                          ''~7420I~''~7421R~
        While posText < textSZ                                           ''~7420R~''~7421R~
            pos = cvText.IndexOf(vbNewLine, posText)                         ''~7420I~''~7421R~
            If pos = -1 Then                                                 ''~7420I~''~7421R~
                len = textSZ - posText                                       ''~7420I~''~7421R~
            Else                                                       ''~7420I~''~7421M~
                len = pos - posText                                           ''~7420I~''~7421R~
            End If                                                     ''~7420I~''~7421M~
            cvLine(posText, len)                                       ''~7421R~
            appendCRLF()                                               ''~7421I~
            posText += len + vbNewLine.Length                                  ''~7420I~''~7421R~''~7508R~
            If posText = posInTheConcatLine Then                            ''~7513I~
                posInTheSplitLine = sbOut.Length                       ''~7513I~
            End If                                                     ''~7513I~
        End While                                                      ''~7420R~''~7421M~
        Dim strOut = sbOut.ToString()                                    ''~7428I~
        sbOut.Clear()                                                  ''~7428I~
        Return strOut                                                  ''~7428R~
    End Function                                                            ''~7412I~''~7421M~
    Private Sub appendEOL()                                            ''~7421I~
        If posOut <= lineWidth Then                                           ''~7426I~
            sbOut.Append(SIGN_EOL)                                         ''~7421I~''~7426R~
            sbOut.Append(SIGN_EOL)                                     ''~7426I~
        ElseIf posOut = lineWidth + 1 Then                                          ''~7426I~
            sbOut.Append(SIGN_EOL)                                         ''~7421I~''~7426R~
        End If                                                         ''~7426I~
        posOut = 0                                                       ''~7421I~
        sbOut.AppendLine()                                             ''~7421M~
    End Sub                                                            ''~7421I~
    Private Sub appendSpace()                                          ''~7427I~
        appendChar(CHAR_SPACE)                                         ''~7427I~
    End Sub                                                            ''~7427I~
    Private Sub appendSpaceN(Pnum As Integer)                          ''~7430I~
        For ii As Integer = 1 To Pnum                                    ''~7430I~
            appendChar(CHAR_SPACE)                                     ''~7430I~
        Next    ''~7430I~
    End Sub                                                            ''~7430I~
    Private Function appendSpaceFollowing(Pctr As Integer, Ppos As Integer, Ppose As Integer) As Integer ''~7427I~
        'append space if not follwed and return deleted space ctr from input''~7427I~
        Dim readctr, readctr1, ctr, pos As Integer                            ''~7427I~
        Dim ch As Char                                                 ''~7427I~
        pos = Ppos                                                       ''~7427I~
        readctr = 0                                                      ''~7427I~
        ctr = Pctr                                                       ''~7427I~
        For ii As Integer = 0 To Pctr - 1                                  ''~7427I~
            ch = getChar(False, pos, Ppose, readctr1)                             ''~7427I~''~7430R~
            If ch <> CHAR_SPACE Then                                          ''~7427I~
                Exit For
            End If                                                      ''~7427I~
            readctr += readctr1                                          ''~7427I~
            pos += readctr1                                              ''~7427I~
            ctr -= 1                                                     ''~7427I~
        Next                                                           ''~7427I~
        appendSpaceN(ctr)                                              ''~7427I~''~7430R~
        Return readctr                                                 ''~7427I~
    End Function                                                            ''~7427I~
    Private Function removeLastSpace() As Integer                      ''~7423I~
        '*** remove space before CRLF up to TOL                            ''~7423I~
        Dim ctr, len As Integer                                         ''~7423I~
        len = sbOut.Length - 1                                             ''~7423I~
        ctr = 0                                                          ''~7423I~
        While len >= 0                                                   ''~7423I~
            If sbOut.Chars(len) <> SIGN_SPACE Then                            ''~7423I~''~7426R~
                Exit While                                             ''~7423I~
            End If                                                     ''~7423I~
            If ctr > posOut Then                                              ''~7423I~
                Exit While                                             ''~7423I~
            End If                                                     ''~7423I~
            ctr += 1                                                     ''~7423I~
            len -= 1                                                     ''~7426I~
        End While                                                      ''~7423I~
        sbOut.Length -= ctr                                              ''~7423I~
        Return ctr                                                     ''~7423I~
    End Function                                                       ''~7423I~
    Private Function appendChar(Pch As Char) As Boolean                ''~7421R~
        'rc:true if no space to the line                                   ''~7421I~
        Dim rc As Boolean = False                                        ''~7421I~
        Dim ch As Char                                                 ''~7426I~
        ch = Pch                                                         ''~7426I~
        If Pch = CHAR_SPACE Then                                              ''~7423I~
            If posOut >= lineWidth + 2 Then                                      ''~7423I~''~7426R~
                appendEOL()                                            ''~7426I~
                rc = True                                              ''~7426I~
            End If                                                     ''~7423I~
            ch = SIGN_SPACE                                              ''~7426I~
        ElseIf Pch = SIGN_CRLF Then                                           ''~7423I~
            If posOut >= lineWidth + 2 Then                             ''~7426I~
                appendEOL()                                            ''~7426I~
                rc = True                                              ''~7426I~
            End If                                                     ''~7426I~
            '           posOut -= removeLastSpace()                                  ''~7423I~''~7426R~
        ElseIf posOut >= lineWidth Then                                ''~7423I~
            appendEOL()                                                ''~7421R~
            rc = True                                                    ''~7421I~
        End If                                                         ''~7421I~
        If ch <> SIGN_CHAR_NOTHING Then    'avoid prog err to write null      ''~7428I~
            sbOut.Append(ch)                                               ''~7426I~''~7428R~
            posOut += 1                                                      ''~7421I~''~7428R~
        End If                                                         ''~7428I~
        Return rc
    End Function
    Private Function prepareAppendString(Ppos As Integer, Plen As Integer) As Boolean   ''~7421I~''~7422R~
        Dim rc As Boolean = False                                        ''~7421I~
        If Ppos > posTOL AndAlso posOut + Plen > lineWidth Then                                       ''~7421I~''~7422R~
            fillToEOL(SIGN_CHAR_NOTHING, False)                                                ''~7421I~''~7422R~''~7423R~
            rc = True                                                    ''~7421I~
        End If                                                         ''~7421I~
        Return rc                                                      ''~7421I~
    End Function                                                       ''~7421R~
    Private Sub fillToEOL(Pch As Char, PswFillNewLine As Boolean)       ''~7421I~
        '** PswFillNewLine:When Pch was overflowed to next line top,fill the next line''~7422I~
        If Pch <> SIGN_CHAR_NOTHING AndAlso appendChar(Pch) AndAlso Not PswFillNewLine Then  'appended to new line''~7421I~''~7422R~''~7423R~
        Else                                                           ''~7421I~
            For ii As Integer = posOut To lineWidth - 1                  ''~7421I~
                sbOut.Append(SIGN_NOCHAR)                         ''~7421I~''~7426R~
            Next                                                       ''~7421I~
            appendEOL()                                                ''~7421I~
        End If                                                         ''~7421I~
    End Sub                                                            ''~7421I~
    Private Sub appendCRLF()           ''~7413M~                       ''~7421M~
        fillToEOL(SIGN_CRLF, True)                                      ''~7421R~
    End Sub                                                            ''~7413M~''~7421M~
    Private Sub appendDigitSplitter()                                 ''~7427I~
        appendChar(SIGN_TEMPSPACE1)                                 ''~7423I~''~7427I~
        appendChar(SIGN_TEMPSPACE2)                                    ''~7427I~
    End Sub                                                            ''~7427I~
    Private Sub appendConcat1()                                        ''~7426I~
        appendSpaceForTenji(SIGN_CONCAT1)                                       ''~7426I~''~7427R~
    End Sub                                                            ''~7426I~
    Private Sub appendSpaceForTenji(Pch As Char)                       ''~7427I~
        appendChar(Pch)                                              ''~7427I~
    End Sub                                                            ''~7427I~
    Private Sub insertSpace(Ppos As Integer, Plen As Integer) ''~7413M~''~7415R~''~7421R~
        For ii As Integer = 0 To Plen - 1                              ''~7413M~''~7421M~
            sbOut.Insert(Ppos, SIGN_SPACE)                                     ''~7413M~''~7415R~''~7421R~''~7426R~
        Next                                                           ''~7413M~''~7421M~
    End Sub                                                            ''~7413M~''~7421M~
    Private Function getChar(PswNext As Boolean, Ppos As Integer, Ppose As Integer, ByRef Ppreadctr As Integer) As Char               ''~7421R~''~7427R~''~7430R~
        '*** sbcs-->dbcs,Katakana-->hiragana                           ''~7427R~
        Dim ch As Char                                                 ''~7423I~
        Dim asc, type, readctr As Integer                                         ''~7423I~''~7427R~
        ch = cvText.Chars(Ppos)                                          ''~7423I~
        Ppreadctr = 1                                                    ''~7427I~
        asc = AscW(ch)                                                ''~7423I~
        If asc > UC_SPACE_SBCS AndAlso asc < &H7F Then      'space to before DEL     ''~7423R~''~7427R~
            ch = ChrW(asc - UC_SPACE_SBCS + UC_SBCS2DBCS)                         ''~7423R~''~7427R~
        ElseIf asc = UC_SPACE_SBCS Then                                                ''~7423R~''~7427R~
            ch = CHAR_SPACE                                              ''~7423R~
            If Ppos + 1 < Ppose AndAlso AscW(cvText.Chars(Ppos + 1)) = UC_SPACE_SBCS Then ''~7427R~
                Ppreadctr = 2                                            ''~7427I~
            End If                                                     ''~7427I~
        Else                                                           ''~7427I~
            type = isKatakana(ch)                                      ''~7427I~
            If type > 0 Then                                           ''~7427I~
                ch = cvKatakana(type, ch, Ppos, Ppose, readctr)            ''~7427R~
                Ppreadctr = readctr                                       ''~7427I~
            End If                                                      ''~7427I~
        End If                                                         ''~7423I~
        Return ch                                                      ''~7423I~
    End Function                                                       ''~7414I~''~7421M~
    Private Function getCharNext(Ppos As Integer, Ppose As Integer) As Char ''~7421I~
        Dim readctr As Integer                                         ''~7427I~
        If Ppos + 1 >= Ppose Then                                        ''~7421I~
            Return SIGN_CHAR_NOTHING                                   ''~7427R~
        End If                                                         ''~7421I~
        Return getChar(True, Ppos + 1, Ppose, readctr)                                    ''~7421I~''~7423R~''~7427R~''~7430R~
    End Function                                                       ''~7421I~
    Private Function getCharNextHankana(Ppos As Integer, Ppose As Integer) As Char ''+7427I~                                     ''~7427I~
        If Ppos + 1 >= Ppose Then                                      ''~7427I~
            Return SIGN_CHAR_NOTHING                                   ''~7427I~
        End If                                                         ''~7427I~
        Return cvText.Chars(Ppos + 1)                                   ''~7427I~
    End Function                                                       ''~7427I~
    Private Sub removeChar(Ppos As Integer)                            ''~7421I~
        sbOut.Remove(Ppos, 1)                               ''~7414I~  ''~7421I~
    End Sub                                                            ''~7421I~
    '*** char conversion ************************                      ''~7414R~''~7421M~
    Private Function cvLine(Ppos As Integer, Plen As Integer) As Boolean ''~7421R~
        Dim ch As Char                                   ''~7413I~''~7421M~
        Dim pos, pose, posnext, readctr As Integer                               ''~7421R~''~7427R~
        posTOL = Ppos                                                    ''~7422I~
        pos = Ppos                                                       ''~7421R~
        pose = pos + Plen                                                  ''~7421I~
        posOut = 0                                                     ''~7421I~
        While pos < pose                                               ''~7421I~
            ch = getChar(False, pos, pose, readctr)                                          ''~7421R~''~7427R~''~7430R~
            '           If isHiragana(ch) Then                                     ''~7427R~''''~v101R~
            If isHiragana(ch) > 0 Then                                   ''~v101R~
                cvHiragana(pos, pose, posnext)                         ''~7427I~
            ElseIf isDigit(ch) Then                                       ''~7413I~''~7421R~''~7427R~
                cvDigit(pos, pose, posnext)                      ''~7421R~''~7427R~
            ElseIf isAlpha(ch) Then                                  ''~7414I~''~7421R~''~7427R~
                cvAlpha(pos, pose, posnext)             ''~7414R~        ''~7421R~
            ElseIf ch = SIGN_CRLF Then                                        ''~7513R~
                posnext = pos + 1                                          ''~7513I~
            Else                                                       ''~7513I~
                cvOtherChar(ch, pos, pose, posnext)                                              ''~7414I~''~7421R~''~7427R~''~7430R~
            End If                                                     ''~7427I~
            pos = posnext                                                ''~7421I~
            If pos = posInTheConcatLine Then                                  ''~7508I~
                posInTheSplitLine = sbOut.Length                     ''~7508I~
            End If                                                     ''~7513I~
        End While                                                      ''~7421I~
        Return True                                                    ''~7421I~
    End Function                                                       ''~7413I~''~7421R~
    '**** Digit *****************************************************  ''~7414R~''~7421M~
    '**** ASCII-->DBCS digit **                                        ''~7414I~''~7421M~
    '**** Terminater requred before BBCS hirakgana A-O,RA-RO because it is same 6dot as Digit''~7414I~''~7421M~
    '**** Max 4 digit if ",." embedded                                 ''~7423I~
    Private Function isDigit(Pch As Char) As Boolean ''~7413I~''~7421M~''~7427R~
        Dim asc As Integer                                             ''~7413I~''~7421M~
        Dim rc As Boolean = False                                        ''~7413I~''~7421M~
        asc = AscW(Pch)                                                ''~7413I~''~7421M~
        If asc >= UC_DIGIT0_DBCS AndAlso asc <= UC_DIGIT9_DBCS Then               ''~7413I~''~7414R~''~7421M~''~7423R~
            rc = True                                                    ''~7413I~''~7421M~
        End If                                                          ''~7413I~''~7421M~
        Return rc                                                      ''~7413I~''~7421M~
    End Function                                                       ''~7413I~''~7421M~
    Private Function isDigitSequence(Pch As Char) As Integer ''~7423I~ ''~7427R~
        '***rc:1-digit,2:comma/period                                      ''~7423I~
        If Pch = CHAR_PERIOD OrElse Pch = CHAR_COMMA Then                       ''~7423I~
            Return 2                                                   ''~7423I~
        End If                                                         ''~7423I~
        If isDigit(Pch) Then                                           ''~7423I~''~7427R~
            Return 1                                                   ''~7423I~
        End If                                                         ''~7423I~
        Return 0                                                       ''~7423I~
    End Function                                                       ''~7423I~
    Private Function ctrDigit(Ppos As Integer, Ppose As Integer) As Integer ''~7420I~''~7421R~
        '*** split by 4 digit if not split by commma/period                ''~7423I~
        Dim pos, ctr, rc, contctr, readctr As Integer                                         ''~7421R~''~7423R~''~7427R~
        Dim ch As Char                                        ''~7420I~''~7421M~
        pos = Ppos                                                       ''~7421I~
        ctr = 0                                                          ''~7421I~
        contctr = 0                                                      ''~7423I~
        While pos < Ppose                                              ''~7421I~
            ch = getChar(False, pos, Ppose, readctr)                                          ''~7421R~''~7427R~''~7430R~
            rc = isDigitSequence(ch)                            ''~7423R~''~7427R~
            If rc = 0 Then                                                    ''~7423I~
                Exit While                                             ''~7421R~''~7423R~
            End If                                                     ''~7420I~''~7421M~
            If rc = 1 Then  'digit                                            ''~7423I~
                If contctr = MAX_DIGIT_CONTINUE Then                          ''~7423I~
                    Exit While                                         ''~7423I~
                End If                                                 ''~7423I~
                contctr += 1                                             ''~7423I~
            Else ' rc=2                                                ''~7423R~
                contctr = 0                                              ''~7423I~
            End If                                                     ''~7423I~
            ctr += 1                                                      ''~7420I~''~7421M~
            pos += readctr                                                     ''~7421I~''~7427R~
        End While                                                      ''~7421R~
        Return ctr                                                     ''~7420I~''~7421M~
    End Function                                                       ''~7420I~''~7421M~
    Private Function cvDigit(Ppos As Integer, Ppose As Integer, ByRef Ppposnext As Integer) As Integer ''~7413I~''~7420R~''~7421R~''~7427R~
        Dim ch, chnext As Char                                        ''~7413I~''~7421R~
        Dim lenw, breaker, len, pos, longseq, readctr As Integer                   ''~7421R~''~7423R~''~7427R~
        ''~7421I~
        len = ctrDigit(Ppos, Ppose)                                ''~7420I~''~7421R~''~7423R~
        chnext = getCharNext(Ppos + len - 1, Ppose)                           ''~7421R~''~7423R~
        breaker = 0                                                    ''~7423I~
        longseq = 0                                                    ''~7423I~
        If chnext <> SIGN_CHAR_NOTHING Then                                       ''~7423R~''~7427R~
            If isDigit(chnext) Then  'spli4 by 4 digit limit          ''~7423I~''~7427R~
                longseq = 1                                              ''~7423I~
            ElseIf isDigit2kana(chnext) Then  'next is DBCS:A-O,RA-RO  ''~7423I~
                breaker = 1                                            ''~7423I~
            End If                                                     ''~7423I~
        End If                                                         ''~7423I~
        lenw = 1 + len + breaker + longseq                                 ''~7423R~
        prepareAppendString(posOut, lenw)                                      ''~7421I~''~7422R~''~7427R~
        appendChar(SIGN_DIGIT)                                          ''~7413I~''~7414I~''~7420M~''~7421R~
        pos = Ppos                                                       ''~7423I~
        For ii As Integer = 0 To len - 1                                ''~7423R~
            ch = getChar(False, pos, Ppose, readctr)                                          ''~7421R~''~7427R~''~7430R~
            appendChar(ch)                                     ''~7413I~''~7414R~''~7421R~''~7423R~
            pos += readctr                                                     ''~7421I~''~7427R~
        Next                                                      ''~7421R~
        If longseq <> 0 Then                                           ''~7423I~
            appendDigitSplitter()                                      ''~7427I~
        End If                                                         ''~7423I~
        If breaker <> 0 Then                                           ''~7423I~
            appendConcat1()                                  ''~7423R~ ''~7426R~
        End If                                                         ''~7423I~
        Ppposnext = pos                                                ''~7421R~
        Return 0                                                       ''~7421R~
    End Function                                                       ''~7413I~''~7421M~
    Private Function isDigit2kana(Pch As Char) As Boolean              ''~7414I~''~7421M~
        If STR_DIGIT2KANA.IndexOf(Pch) >= 0 Then                        ''~7414I~''~7421M~
            Return True                                                ''~7414I~''~7421M~
        End If                                                         ''~7414I~''~7421M~
        If STR_DIGIT2ZENKATA.IndexOf(Pch) >= 0 Then                    ''~7414R~''~7421M~
            Return True                                                ''~7414I~''~7421M~
        End If                                                         ''~7414I~''~7421M~
        If STR_DIGIT2HANKATA.IndexOf(Pch) >= 0 Then                    ''~7414R~''~7421M~
            Return True                                                ''~7414I~''~7421M~
        End If                                                         ''~7414I~''~7421M~
        Return False                                                   ''~7414I~''~7421M~
    End Function                                                       ''~7414I~''~7421M~
    '**** abcd/ABCD **********************************************     ''~7414R~''~7421M~
    '***prefix for both and upper/lower mixed                          ''~7414I~''~7421M~
    '***prefix-Upper for 1 Upper, 2 prefix Upper for upper string and  ''~7414I~''~7421M~
    '***ast end of Upper String put End of Upper before lower string   ''~7414I~''~7421M~
    '*** or space before not lower char                                ''~7414I~''~7421M~
    Private Function isAlpha(Pch As Char) As Boolean ''~7414I~''~7421M~''~7427R~
        Return isAlphaLower(Pch) OrElse isAlphaUpper(Pch) ''~7414I~''~7421M~''~7427R~
    End Function                                                       ''~7414I~''~7421M~
    Private Function isAlphaLower(Pch As Char) As Boolean ''~7414R~''~7421M~''~7427R~
        Dim asc As Integer                                             ''~7414I~''~7421M~
        Dim rc As Boolean = False                                      ''~7414I~''~7421M~
        asc = AscW(Pch)                                                ''~7414I~''~7421M~
        If asc >= UC_A_LOWER_DBCS AndAlso asc <= UC_Z_LOWER_DBCS Then            ''~7414R~''~7421M~''~7427R~
            rc = True                                                  ''~7414I~''~7421M~
        End If                                                         ''~7414I~''~7421M~
        Return rc                                                      ''~7414I~''~7421M~
    End Function                                                       ''~7414I~''~7421M~
    Private Function isAlphaUpper(Pch As Char) As Boolean ''~7414I~''~7421M~''~7427R~
        Dim asc As Integer                                             ''~7414I~''~7421M~
        Dim rc As Boolean = False                                      ''~7414I~''~7421M~
        asc = AscW(Pch)                                                ''~7414I~''~7421M~
        If asc >= UC_A_UPPER_DBCS AndAlso asc <= UC_Z_UPPER_DBCS Then            ''~7414R~''~7421M~''~7427R~
            rc = True                                                  ''~7414I~''~7421M~
        End If                                                         ''~7414I~''~7421M~
        Return rc                                                      ''~7414I~''~7421M~
    End Function                                                       ''~7414I~''~7421M~
    Private Function cvAlpha(Ppos As Integer, Ppose As Integer, ByRef Ppposnext As Integer) As Integer ''~7414I~''~7421R~
        Dim ch, ch2 As Char                          ''~7414R~''~7421R~''~7427R~
        Dim swLH, pos, ctr, readctr As Integer                                ''~7421R~''~7427R~
        ''~7421I~                                                      ''~7427R~
        '** count word chars                                               ''~7427I~
        swLH = 0                                                       ''~7421I~
        pos = Ppos                                                       ''~7421I~
        ctr = 1   'aplha sign                                            ''~7427I~
        While pos < Ppose                                                ''~7421R~
            ch = getChar(False, pos, Ppose, readctr)                                          ''~7421R~''~7427R~''~7430R~
            If isAlphaLower(ch) Then                          ''~7414I~''~7421R~''~7427R~
                If (swLH > 1) Then     'after UpperString              ''~7421R~
                    ctr += 2 'upper and upper-end                        ''~7427I~
                End If                                                 ''~7414I~''~7421M~
                ctr += 1     'char itself                                ''~7427R~
                swLH = -1                                                ''~7414I~''~7421M~
            ElseIf isAlphaUpper(ch) Then                      ''~7414I~''~7421R~''~7427R~
                If (swLH <= 0) Then   'start  of Upper str or after lower''~7414R~''~7421M~
                    ctr += 1  'upper                                     ''~7427R~
                    swLH = 1     'Upper started                                        ''~7414I~''~7421M~''~7427R~
                    ch2 = getCharNext(pos, Ppose)                         ''~7421R~
                    If ch2 <> SIGN_CHAR_NOTHING Then                              ''~7421I~''~7427R~
                        If isAlphaUpper(ch2) Then            ''~7414I~''~7421R~''~7427R~
                            ctr += 1  'upper for cont upper              ''~7427R~
                            swLH = 2     'start of Upper over 1 column ''~7427R~
                        End If                                         ''~7414I~''~7421M~
                    End If                                             ''~7414I~''~7421M~
                End If ''~7414I~                                       ''~7421M~
                ctr += 1  'char itself                                   ''~7427R~
            Else                                                       ''~7414I~''~7421M~
                If ch <> CHAR_SPACE Then                                      ''~7427R~
                    ctr += 1  'tunagifu                                ''~7427R~
                End If                                                 ''~7427I~
                Exit While                                               ''~7414I~''~7421M~
            End If                                                     ''~7414I~''~7421M~
            pos += readctr                                                     ''~7421I~''~7427R~
        End While                                                      ''~7421R~
        '** output string                                                  ''~7427I~
        prepareAppendString(posOut, ctr)                               ''~7427R~
        swLH = 0                                                       ''~7427I~
        pos = Ppos                                                     ''~7427I~
        appendChar(SIGN_ALPHA)                                         ''~7427I~
        While pos < Ppose                                              ''~7427I~
            ch = getChar(False, pos, Ppose, readctr)                           ''~7427R~''~7430R~
            If isAlphaLower(ch) Then                                   ''~7427R~
                If (swLH > 1) Then     'after UpperString              ''~7427I~
                    '                   appendChar(SIGN_UPPER)                             ''~7427I~''~7508R~
                    appendChar(SIGN_UPPER_END)                         ''~7427I~
                End If                                                 ''~7427I~
                appendChar(ch)                                         ''~7427R~
                swLH = -1                                              ''~7427I~
            ElseIf isAlphaUpper(ch) Then                               ''~7427R~
                If (swLH <= 0) Then   'start  of Upper str or after lower''~7427I~
                    appendChar(SIGN_UPPER)                             ''~7427I~
                    swLH = 1     'Upper started                        ''~7427I~
                    ch2 = getCharNext(pos, Ppose)                      ''~7427I~
                    If ch2 <> SIGN_CHAR_NOTHING Then                     ''~7427I~
                        If isAlphaUpper(ch2) Then                      ''~7427R~
                            appendChar(SIGN_UPPER)                     ''~7427I~
                            swLH = 2     'start of Upper over 1 column ''~7427I~
                        End If                                         ''~7427I~
                    End If                                             ''~7427I~
                End If                                                 ''~7427I~
                appendChar(ch)                                         ''~7427R~
            Else                                                       ''~7427I~
                If ch <> CHAR_SPACE Then                                      ''~7427R~
                    appendSpace()                                      ''~7513R~
                End If                                                 ''~7427I~
                Exit While                                             ''~7427I~
            End If                                                     ''~7427I~
            pos += readctr                                             ''~7427R~
        End While                                                      ''~7427I~
        Ppposnext = pos                                                  ''~7421R~
        Return 0                                                       ''~7421R~
    End Function                                                       ''~7414I~''~7421M~
    Private Sub cvOtherChar(Pch As Char, Ppos As Integer, Ppose As Integer, ByRef Ppposnext As Integer) ''~7430R~
        Ppposnext = Ppos + 1                                               ''~7430I~
        If STR_2TENJI.IndexOf(Pch) >= 0 Then                           ''~7427I~
            prepareAppendString(posOut, 2)                              ''~7427I~
            appendSpaceForTenji(SIGN_SPACE_BEFORE_SPECIAL_CHAR)        ''~7427I~
            appendChar(Pch)                                            ''~7427I~
            Exit Sub                                                   ''~7430R~
        End If                                                         ''~7427I~
        appendChar(Pch)                                                ''~7427I~
        If Pch = CHAR_EXCLAMATION OrElse Pch = CHAR_QUESTION Then              ''~7430I~
            If isEndOfStatement() Then                                      ''~7430R~
                appendSpaceFollowing(2, Ppos + 1, Ppose)                  ''~7430R~
            End If                                                     ''~7430I~
        End If                                                         ''~7430I~
    End Sub                                                            ''~7430R~
    Private Function isEndOfStatement() As Boolean                     ''~7430I~
        Dim chs As Char() = {SIGN_CHAR_NOTHING}                                              ''~7430R~
        Dim len As Integer
        len = sbOut.Length                                               ''~7430I~
        If len < 2 Then                                                       ''~7430I~
            Return False                                               ''~7430I~
        End If                                                         ''~7430I~
        sbOut.CopyTo(len - 2, chs, 0, 1)                                    ''~7430R~
        If chs(0) = SIGN_SPACE OrElse STR_PUNCT_DBCS.IndexOf(chs(0)) >= 0 Then  ''~7430I~
            Return False                                               ''~7430I~
        End If                                                         ''~7430I~
        Return True                                                    ''~7430R~
    End Function                                                       ''~7430I~
    '**** Katakana *************************************************   ''~7414R~''~7421M~
    '**** hankaku-->zenkaku                                            ''~7414I~''~7421M~
    '**** hankaku+dakuten/handakuten-->Zenkaku composit                ''~7414I~''~7421M~
    '**** U+dakuten --> Zenkaku katakan Vv                             ''~7414I~''~7421M~
    Private Function isKatakana(Pch As Char) As Integer                ''~7427R~
        'rc:1,zenkata,2:special zenkata,3:no corresponding hiragana,output 2 char''~7427I~
        '   4,hankana,5 hankana-special                                ''~7428R~
        Dim rc As Integer                                              ''~7427I~
        rc = isKatakanaZenkaku(Pch)                                      ''~7427R~
        If rc = 0 Then                                                        ''~7427I~
            rc = isKatakanaHankaku(Pch)                                  ''~7427I~
        End If                                                         ''~7427I~
        Return rc                                                      ''~7427I~
    End Function                                                       ''~7414I~''~7421M~
    Private Function isKatakanaZenkaku(Pch As Char) As Integer         ''~7427I~
        Dim asc As Integer
        asc = AscW(Pch)                                                ''~7427I~
        If asc >= UC_ZENKATA_START AndAlso asc <= UC_ZENKATA_END Then  ''~7427I~
            Return 1                                                   ''~7427I~
        End If                                                         ''~7427I~
        If STR_ZENKATA2.IndexOf(Pch) >= 0 Then                                 ''~7427I~''~7428R~
            Return 2                                                   ''~7427I~
        End If                                                         ''~7427I~
        If STR_ZENKATA3.IndexOf(Pch) >= 0 Then                                 ''~7427I~''~7428R~
            Return 3                                                   ''~7427I~
        End If                                                         ''~7427I~
        Return 0                                                       ''~7427I~
    End Function                                                       ''~7427I~
    Private Function cvKatakanaZenkaku(Pkanatype As Integer, Pch As Char) As Char ''~7427I~
        Dim asc, idx As Integer                                             ''~7414I~''~7421M~''~7427I~
        Select Case Pkanatype                                          ''~7427I~
            Case 2           'ka,ke,                                              ''~7427I~''~7428R~
                idx = STR_ZENKATA2.IndexOf(Pch)                               ''~7427I~
                Return STR_ZENKATA22HIRA.Chars(idx)                         ''~7427I~
            Case 3    '      'wa"                                              ''~7427I~''~7428R~
                '               idx = STR_ZENKATA3.IndexOf(Pch)                               ''~7427I~''~7428R~
                '               Return STR_ZENKATA32HIRA.Chars(idx)                         ''~7427I~''~7428R~
                Return Pch                                             ''~7428I~
        End Select                                                     ''~7427I~
        asc = AscW(Pch)                                                ''~7427I~
        Return ChrW(asc - UC_ZENKATA_START + UC_KANA_START)              ''~7427R~
    End Function                                                       ''~7414I~''~7421M~''~7427M~
    Private Function isKatakanaHankaku(Pch As Char) As Integer         ''~7427R~
        Dim asc As Integer
        asc = AscW(Pch)                                                ''~7414I~''~7421M~
        If asc >= UC_HANKATA_START AndAlso asc <= UC_HANKATA_END Then  ''~7414R~''~7421M~
            Return 4                                                ''~7414I~''~7421M~''~7427R~
        End If                                                         ''~7414R~''~7421M~
        If STR_HANKATA2.IndexOf(Pch) >= 0 Then                                 ''~7427I~
            Return 5                                                   ''~7427I~
        End If                                                         ''~7427I~
        Return 0                                                      ''~7414I~''~7421M~''~7427R~
    End Function                                                       ''~7414I~''~7421M~
    Private Function cvKatakanaHankaku(Pkanatype As Integer, Pch As Char, Ppos As Integer, Ppose As Integer, ByRef Ppcomposit As Boolean) As Char ''~7427I~
        Dim ch As Char                                            ''~7427I~
        Dim asc, idx As Integer
        Ppcomposit = False                                               ''~7427I~
        Select Case Pkanatype                                          ''~7427I~
            Case 5                                                     ''~7427R~
                idx = STR_HANKATA2.IndexOf(Pch)      'hankana kigou           ''~7427I~
                Return STR_HANKATA22HIRA.Chars(idx)                         ''~7427I~
        End Select                                                     ''~7427I~
        asc = AscW(Pch)                                                ''~7427I~
        idx = asc - UC_HANKATA_START                                   ''~7427I~
        ch = STR_HANKATA2HIRA.Chars(idx)                               ''~7427R~
        Return ch                                                      ''~7427I~
    End Function                                                       ''~7427I~
    Private Function cvKatakanaHankaku(Pkanatype As Integer, Pch As Char, Pchnext As Char, ByRef Ppcomposit As Boolean) As Char ''~7501I~
        ' hankata-->hiragana with composit                                 ''~7501I~
        Dim ch, cvch As Char                                            ''~7501R~
        Dim asc, idx As Integer                                        ''~7501I~
        Ppcomposit = False                                             ''~7501I~
        Select Case Pkanatype                                          ''~7501I~
            Case 5                                                     ''~7501I~
                idx = STR_HANKATA2.IndexOf(Pch)      'hankana kigou    ''~7501I~
                Return STR_HANKATA22HIRA.Chars(idx)                    ''~7501I~
        End Select                                                     ''~7501I~
        asc = AscW(Pch)                                                ''~7501I~
        idx = asc - UC_HANKATA_START                                   ''~7501I~
        ch = STR_HANKATA2HIRA.Chars(idx)                               ''~7501I~
        If compositHankata(ch, Pchnext, cvch) Then                      ''~7501I~
            ch = cvch                                                  ''~7501I~
            Ppcomposit = True                                          ''~7501I~
        End If                                                         ''~7501I~
        Return ch                                                      ''~7501I~
    End Function                                                       ''~7501I~
    Private Function cvKatakana(Ptype As Integer, Pch As Char, Ppos As Integer, Ppose As Integer, ByRef Ppreadctr As Integer) As Char ''~7427R~
        Dim ch, cvch As Char                                           ''~7427I~
        Dim swComposit = False                                         ''~7427I~
        Dim pos, readctr As Integer                              ''~7427R~
        pos = Ppos                                                     ''~7427I~
        ch = Pch                                                       ''~7427R~
        readctr = 1                                                      ''~7427I~
        Select Case Ptype                                              ''~7427R~
            Case 1, 2    'zenkata                                      ''~7427I~
                cvch = cvKatakanaZenkaku(Ptype, ch)                     ''~7427I~
            Case 3     'zenkata no corresponding hiragana              ''~7427I~
                cvch = cvKatakanaZenkaku(Ptype, ch)                     ''~7427I~
            Case 4    'hankata                                         ''~7427I~
                cvch = cvKatakanaHankaku(Ptype, ch, pos, Ppose, swComposit) ''~7427I~
                If swComposit Then                                     ''~7427I~
                    readctr += 1                                        ''~7427I~
                End If                                                 ''~7427I~
            Case 5    'hankata kigou                                   ''~7427I~
                cvch = cvKatakanaHankaku(Ptype, ch, pos, Ppose, swComposit) ''~7427I~
        End Select                                                     ''~7427I~
        Ppreadctr = readctr                                              ''~7427I~
        Return cvch                                                    ''~7427R~
    End Function                                                       ''~7427I~
    Private Function compositHankata(Pch As Char, Pchnext As Char, ByRef Ppch As Char) As Boolean ''~7501R~
        Dim rc As Boolean = False                                       ''~7414I~''~7421M~
        Dim idx As Integer                                             ''~7414I~''~7421M~''~7427R~
        Dim ch2 As Char                                             ''~7427I~
        ch2 = Pchnext                                                  ''~7501R~
        If ch2 = CHAR_HANKATA_HANDAKUON Then                                 ''~7414R~''~7421M~''~7427R~
            idx = STR_HANDAKUON_SRC.IndexOf(Pch)                   ''~7421M~''~7427R~
            If idx >= 0 Then           ''~7414R~                       ''~7421M~
                Ppch = STR_HANDAKUON_TGT.Chars(idx)              ''~7414R~''~7421M~''~7427R~
                rc = True                                                ''~7414I~''~7421M~
            End If                                                     ''~7414I~''~7421M~
        ElseIf ch2 = CHAR_HANKATA_DAKUON Then                          ''~7427R~
            idx = STR_DAKUON_SRC.IndexOf(Pch)                      ''~7414R~''~7421M~''~7427R~
            If idx >= 0 Then                                           ''~7414R~''~7421M~
                Ppch = STR_DAKUON_TGT.Chars(idx)                       ''~7414R~''~7421M~
                rc = True                                              ''~7414R~''~7421M~
            End If                                                     ''~7414R~''~7421M~
        End If                                                         ''~7414I~''~7421M~
        Return rc                                                      ''~7414I~''~7421M~
    End Function                                                       ''~7414I~''~7421M~
    Private Function compositHira(Pch As Char, Ppos As Integer, Ppose As Integer, ByRef Ppchcv As Char, ByRef Ppcomposedctr As Integer) As Integer ''~7427R~''~7428R~
        '**get space for kana composition required column                  ''~7427I~
        '**rc:1:not dakuon,2:dakuon+komoji(2column),3:dakuon(2column)      ''~7427I~
        Dim idx, composedctr As Integer                                             ''~7427I~''~7428R~
        Dim ch, ch2 As Char                                             ''~7427I~''~7428R~
        ch = Pch                                                         ''~7428I~
        ch2 = getCharNext(Ppos, Ppose)                                 ''~7428I~
        composedctr = 0                                                  ''~7428I~
        If ch2 = CHAR_HIRA_DAKUON Then                                        ''~7428I~
            idx = STR_DAKUON_SRC.IndexOf(Pch)                            ''~7428I~
            If idx >= 0 Then                                                  ''~7428I~
                ch = STR_DAKUON_TGT.Chars(idx)                           ''~7428I~
                composedctr = 1                                          ''~7428I~
            End If                                                     ''~7428I~
        ElseIf ch2 = CHAR_HIRA_HANDAKUON Then                                 ''~7428I~
            idx = STR_HANDAKUON_SRC.IndexOf(Pch)                         ''~7428I~
            If idx >= 0 Then                                                  ''~7428I~
                ch = STR_HANDAKUON_TGT.Chars(idx)                        ''~7428I~
                composedctr = 1                                          ''~7428I~
            End If                                                     ''~7428I~
        End If                                                         ''~7428I~
        Ppchcv = ch                                                      ''~7428I~
        Ppcomposedctr = composedctr                                      ''~7428I~
        If composedctr = 0 Then                                               ''~7428I~
            idx = STR_DAKUON_TGT.IndexOf(ch)                              ''~7427R~''~7428R~
            If idx < 0 Then                                                       ''~7427I~''~7428R~
                idx = STR_HANDAKUON_TGT.IndexOf(ch)                       ''~7427I~''~7428R~
            End If                                                         ''~7427I~''~7428R~
        End If                                                         ''~7428I~
        If idx < 0 Then                                                       ''~7427I~
            Return 1                                                   ''~7427I~
        End If                                                         ''~7427I~
        ch2 = getCharNext(Ppos + composedctr, Ppose)                                 ''~7427R~''~7428R~
        idx = STR_COMPOSIT2.IndexOf(ch2)                               ''~7427I~
        If idx >= 0 Then                                               ''~7427R~
            Return 2                                                   ''~7427I~
        End If                                                         ''~7427I~
        Return 3                                                       ''~7427I~
    End Function                                                       ''~7427I~
    Private Function isHiragana(Pch As Char) As Integer                ''~7427I~
        'rc:1:normal,2:chuutenn,3kuten,4:hirakana=katakana                     ''~7427I~''~7428R~
        Dim asc As Integer                                             ''~7427I~
        asc = AscW(Pch)                                                ''~7427I~
        If asc >= UC_KANA_START AndAlso asc <= UC_KANA_END Then        ''~7427I~
            Return 1                                                   ''~7427I~
        End If                                                         ''~7427I~
        If STR_ZENKATA22HIRA.IndexOf(Pch) >= 0 Then                            ''~7427R~''~7428R~
            Return 2                                                   ''~7427I~
        End If                                                         ''~7427I~
        If STR_KANA3.IndexOf(Pch) >= 0 Then   'kuten                   ''~7427R~''~7428R~
            Return 3                                                   ''~7427I~
        End If                                                         ''~7427I~
        If STR_ZENKATA3.IndexOf(Pch) >= 0 Then   'same as kata and hira''~7428I~
            Return 4                                                   ''~7428I~
        End If                                                         ''~7428I~
        Return 0                                                       ''~7427I~
    End Function                                                       ''~7427I~
    Private Function cvHiragana(Ppos As Integer, Ppose As Integer, ByRef Ppposnext As Integer) As Integer ''~7427R~
        Dim ch, chcv As Char                                                ''~7427R~''~7428R~
        Dim swComposit = False                                         ''~7427I~
        Dim pos, type, len, readctr, composedctr As Integer                                  ''~7427I~''~7428R~
        ''~7427I~
        pos = Ppos                                                     ''~7427I~
        len = 0                                                        ''~7427I~
        While pos < Ppose                                              ''~7427I~
            ch = getChar(False, pos, Ppose, readctr)                           ''~7427R~''~7430R~
            type = isHiragana(ch)                                      ''~7427I~
            If type = 0 Then                                                  ''~7427I~
                Exit While                                             ''~7427I~
            End If                                                     ''~7427I~
            Select Case compositHira(ch, pos, Ppose, chcv, composedctr)                   ''~7427R~''~7428R~
                Case 1, 2    'not dakuon,dakuon+komoji                 ''~7427I~
                    len += 1                                           ''~7427I~
                Case Else 'dakuon                                      ''~7427I~
                    len += 2                                           ''~7427I~
            End Select ''~7427I~
            pos += readctr + composedctr                                 ''~7428R~
            Select Case ch                                                  ''~7427I~
                Case CHAR_KUTEN    '2space after kuten(maru)           ''~7427I~
                    Exit While                                         ''~7427I~
                Case CHAR_TOUTEN   '1space after touten(ten)           ''~7427I~
                    Exit While                                         ''~7427I~
                Case CHAR_CHUTEN   '1space after touten(ten)           ''~7427I~
                    Exit While                                         ''~7427I~
                Case Else                                              ''~7427I~
            End Select                                                 ''~7427I~
        End While                                                      ''~7427I~
        ''~7427I~
        prepareAppendString(posOut, len)                               ''~7427I~
        pos = Ppos                                                     ''~7427I~
        While pos < Ppose                                              ''~7427I~
            ch = getChar(False, pos, Ppose, readctr)                           ''~7427R~''~7430R~
            type = isHiragana(ch)                                      ''~7427I~
            If type = 0 Then                                                  ''~7427I~
                Exit While                                             ''~7427I~
            End If                                                     ''~7427I~
            type = compositHira(ch, pos, Ppose, chcv, composedctr)         ''~7428I~
            ch = chcv                                                    ''~7428I~
            Select Case type                                           ''~7428R~
                Case 1, 2    'not dakuon,dakuon+komoji                 ''~7427I~
                    appendChar(ch)                                           ''~7427R~''~7428R~''~7513I~
                Case Else 'dakuon                                      ''~7427I~
                    appendChar(SIGN_DAKUON)                            ''~7427I~
                    appendChar(ch)                                     ''~7513I~
            End Select                                                 ''~7427I~
            pos += readctr + composedctr                                             ''~7427R~''~7428R~
            Select Case ch                                                  ''~7427I~
                Case CHAR_KUTEN    '2space after kuten(maru)           ''~7427I~
                    appendSpaceFollowing(2, pos, Ppose)         ''~7427R~''~7430R~
                    Exit While                                         ''~7427I~
                Case CHAR_TOUTEN   '1space after touten(ten)           ''~7427I~
                    appendSpaceFollowing(1, pos, Ppose)          ''~7427I~''~7430R~
                    Exit While                                         ''~7427I~
                Case CHAR_CHUTEN   '1space after touten(ten)           ''~7427I~
                    appendSpaceFollowing(1, pos, Ppose)          ''~7427I~''~7430R~
                    Exit While                                         ''~7427I~
                Case Else                                              ''~7427I~
            End Select                                                 ''~7427I~
        End While                                                      ''~7427I~
        Ppposnext = pos                                                ''~7427I~
        Return 0                                                       ''~7427I~
    End Function                                                       ''~7427I~
    '********************************************************************* ''~7426I~
    Private Function getCharSave(Ppos As Integer) As Char              ''~7426I~
        Dim ch As Char                                             ''~7426I~
        ch = saveText.Chars(Ppos)                                      ''~7426I~
        Return ch                                                      ''~7426I~
    End Function                                                       ''~7426I~
    Public Function concatLine(Ptext As String, Ppos As Integer, ByRef Ppposnext As Integer) As String ''~7508I~
        '** get pos in the BESinputtext line from pos in the BESformat line''~7508I~
        posInTheSplitLine = Ppos          'get posInTheConcatLine        ''~7508I~
        posInTheConcatLine = -1                                          ''~7508I~
        Dim txt As String = concatLine(Ptext)                            ''~7508I~
        posInTheSplitLine = -1                                           ''~7508I~
        Ppposnext = posInTheConcatLine                                   ''~7508I~
        Return txt                                                     ''~7508I~
    End Function                                                       ''~7508I~
    Public Function concatLine(Ptext As String) As String              ''~7426I~
        Dim len, pos, asc As Integer                                    ''~7426R~
        Dim ch As Char ''~7426I~
        saveText = Ptext                                               ''~7426I~
        sbSave = New System.Text.StringBuilder(Ptext.Length)          ''~7426I~
        len = Ptext.Length()                                            ''~7426I~
        pos = 0                                                          ''~7426I~
        While pos < len                                                ''~7426I~
            If pos = posInTheSplitLine Then                                   ''~7508I~''~7513M~
                posInTheConcatLine = sbSave.Length                       ''~7508I~''~7513M~
            End If                                                     ''~7508I~''~7513M~
            ch = getCharSave(pos)                                        ''~7426I~
            asc = AscW(ch)                                             ''~7426I~
            Select Case ch                                             ''~7426I~
                Case SIGN_DIGIT                                            ''~7426I~
                Case SIGN_ALPHA                                            ''~7426I~
                Case SIGN_UPPER                                            ''~7426I~
                Case SIGN_UPPER_END                                        ''~7426I~
                Case SIGN_CRLF                                             ''~7426I~
                    sbSave.AppendLine()                                ''~7426I~
                    '                   Exit While                                         ''~7508I~''~7509R~
                Case SIGN_EOL                                              ''~7426I~
                Case SIGN_TEMPSPACE1
                    '                    sbSave.Append(SIGN_TENJISPACE)                     ''~7427I~''~7508R~
                Case SIGN_TEMPSPACE2
                    '                    sbSave.Append(SIGN_TENJISPACE)                     ''~7427I~''~7508R~
                Case SIGN_NOCHAR                                       ''~7426I~
                Case SIGN_CONCAT1                                          ''~7426I~
                    '                    sbSave.Append(SIGN_TENJISPACE)                     ''~7427I~''~7508R~
                Case SIGN_DAKUON                                       ''~7427I~
                    '                    sbSave.Append(SIGN_TENJISPACE)                     ''~7427I~''~7508R~
                Case SIGN_SPACE_BEFORE_SPECIAL_CHAR                    ''~7427I~
                Case SIGN_SPACE                                            ''~7426I~
                    sbSave.Append(CHAR_SPACE)                              ''~7426I~
                Case CHAR_CR                                           ''~7426I~
                Case CHAR_LF                                           ''~7426I~
                    '               	if pos=posInTheSplitLine Then                      ''~7508I~''~7509R~
                    '                       sbSave.AppendLine()                            ''~7508I~''~7509R~
                    '   	            End If                                             ''~7508I~''~7509R~
                Case Else                                                  ''~7426I~
                    sbSave.Append(ch)                                      ''~7426I~
            End Select                                                 ''~7426I~
            pos += 1                                                     ''~7426I~''~7513M~
            ''~7426I~
        End While
        Return sbSave.ToString()                                       ''~7426I~
    End Function ''~7426I~
    '*********************************************************************''~7508I~
    '   Public Function cvHirakataAll(Pswkatakana As Boolean, Ptext As String, Ppos As Integer, ByRef Ppposnext As Integer) ''~7508I~''~v101R~
    Public Function cvHirakataAll(Pswkatakana As Boolean, Ptext As String, Ppos As Integer, ByRef Ppposnext As Integer) As String ''~v101R~
        posHirakataIn = Ppos
        posHirakataOut = Ppos ''~7508I~
        Dim txt As String = cvHirakataAll(Pswkatakana, Ptext)              ''~7508I~
        posHirakataIn = -1                                               ''~7508I~
        Ppposnext = posHirakataOut                                       ''~7508I~
        Return txt                                                     ''~7508I~
    End Function                                                       ''~7508I~
    '*********************************************************************''~v052I~
    '   Public Function cvHirakataAll(Pswkatakana As Boolean, Ptext As String) ''~7428R~''~v101R~
    Public Function cvHirakataAll(Pswkatakana As Boolean, Ptext As String) As String ''~v101R~
        Dim sbHirakata As System.Text.StringBuilder                       ''~7428I~
        Dim pos, pose As Integer                                   ''~7428I~
        Dim ch, cvch As Char                                            ''~7428I~
        If Pswkatakana = swCurrentKatakana Then                               ''~7501I~
            Return Ptext                                               ''~7501I~
        End If                                                          ''~7501I~
        '**                                                            ''~7501I~
        cvText = Ptext                                                 ''~7428I~
        pose = Ptext.Length
        sbHirakata = New System.Text.StringBuilder(pose)                ''~7428I~
        pos = 0                                                        ''~7428I~
        If Pswkatakana Then   'hiragana-->katakana                          ''~7428I~
            While pos < pose                                            ''~7428I~
                ch = cvText.Chars(pos)                                 ''~7428R~
                cvch = cvHira_Kata(ch)                        ''~7428I~''~7501R~
                pos += 1                                               ''~7428I~
                sbHirakata.Append(cvch)                                ''~7428I~
                If pos = posHirakataIn Then                                   ''~7508I~
                    posHirakataOut = sbHirakata.Length                   ''~7508I~
                End If                                                 ''~7508I~
            End While                                                  ''~7428I~
        Else     'katakana-->hiragana                                  ''~7428I~
            While pos < pose                                            ''~7428I~
                ch = cvText.Chars(pos)                                 ''~7428I~
                cvch = cvKata_Hira(ch)                           ''~7501I~
                pos += 1                                               ''~7428I~
                sbHirakata.Append(cvch)                                ''~7428I~
                If pos = posHirakataIn Then                                   ''~7508I~
                    posHirakataOut = sbHirakata.Length                   ''~7508I~
                End If                                                 ''~7508I~
            End While                                                  ''~7428I~
        End If                                                         ''~7428I~
        Dim strOut = sbHirakata.ToString()                               ''~7428R~
        sbHirakata.Clear()                                             ''~7428I~
        swCurrentKatakana = Pswkatakana                                  ''~7501I~
        Return strOut                                                  ''~7428I~
    End Function                                                       ''~7428I~
    Private Function cvHira_Kata(Pch As Char) As Char  ''~7428I~       ''~7501I~
        Dim asc, idx As Integer                                         ''~7428I~''~7501M~
        Dim cvch As Char                                               ''~7428I~''~7501M~
        cvch = Pch                                                       ''~7501I~
        asc = AscW(Pch)                                                ''~7428I~''~7501M~
        If asc >= UC_KANA_START AndAlso asc <= UC_KANA_END Then                ''~7501I~
            cvch = ChrW(asc - UC_KANA_START + UC_ZENKATA_START)      ''~7428I~''~7501I~
        Else                                                           ''~7501I~
            idx = STR_ZENKATA22HIRA.IndexOf(Pch)                     ''~7428R~''~7501I~
            If idx >= 0 Then                                                  ''~7501I~
                cvch = STR_ZENKATA2.Chars(idx)                           ''~7428I~''~7501M~
            End If                                                     ''~7501I~
        End If                                                 ''~7428I~''~7501M~
        Return cvch                                                    ''~7428I~''~7501M~
    End Function                                                       ''~7428I~''~7501M~
    Private Function cvKata_Hira(Pch As Char) As Char                  ''~7501I~
        Dim asc, idx As Integer                                        ''~7501I~
        Dim cvch As Char                                               ''~7501I~
        cvch = Pch                                                       ''~7501I~
        asc = AscW(Pch)                                                ''~7501I~
        If asc >= UC_ZENKATA_START AndAlso asc <= UC_ZENKATA_END Then          ''~7501I~
            cvch = ChrW(asc - UC_ZENKATA_START + UC_KANA_START)        ''~7501I~
        Else                                                           ''~7501I~
            idx = STR_ZENKATA2.IndexOf(Pch)                            ''~7501I~
            If idx >= 0 Then                                                  ''~7501I~
                cvch = STR_ZENKATA22HIRA.Chars(idx)                    ''~7501I~
            End If                                                     ''~7501I~
        End If                                                    ''~7501I~
        Return cvch                                                    ''~7501I~
    End Function                                                       ''~7501I~
    Private Function cvKataHanZen_Hira(Pch As Char, Pchnext As Char, ByRef Ppcomposit As Boolean) As Char ''~7501I~
        Dim type As Integer                                  ''~7501I~
        Dim cvch As Char                                               ''~7501I~
        Ppcomposit = False                                               ''~7501I~
        type = isKatakanaHankaku(Pch)                                    ''~7501I~
        If type = 0 Then                                                      ''~7501I~
            Return cvKata_Hira(Pch)                                    ''~7501I~
        End If                                                          ''~7501I~
        cvch = cvKatakanaHankaku(type, Pch, Pchnext, Ppcomposit)    ''~7501I~
        Return cvch                                                    ''~7501I~
    End Function                                                       ''~7501I~
    '   Public Function cvHirakataHankakuAll(Pswkatakana As Boolean, Ptext As String) ''~7501I~''~v101R~
    Public Function cvHirakataHankakuAll(Pswkatakana As Boolean, Ptext As String) As String ''~v101R~
        'katakana(hankaku/zenkaku)<-->hirgana<-->katakana(Zenkaku)         ''~7501I~
        Dim sbHirakata As System.Text.StringBuilder                    ''~7501I~
        Dim pos, pose As Integer                                       ''~7501I~
        Dim ch, cvch, chnext As Char                                    ''~7501I~
        Dim swComposit As Boolean                                      ''~7501I~
        cvText = Ptext                                                 ''~7501I~
        pose = Ptext.Length                                            ''~7501I~
        sbHirakata = New System.Text.StringBuilder(pose)               ''~7501I~
        pos = 0                                                        ''~7501I~
        If Pswkatakana Then   'hiragana-->katakana                     ''~7501I~
            While pos < pose                                           ''~7501I~
                ch = cvText.Chars(pos)                                 ''~7501I~
                cvch = cvHira_Kata(ch)                                 ''~7501I~
                pos += 1                                               ''~7501I~
                sbHirakata.Append(cvch)                                ''~7501I~
            End While                                                  ''~7501I~
        Else     'katakana-->hiragana                                  ''~7501I~
            While pos < pose                                           ''~7501I~
                ch = cvText.Chars(pos)                                 ''~7501I~
                If pos + 1 < pose Then                                          ''~7501I~
                    chnext = cvText.Chars(pos + 1)                         ''~7501I~
                Else                                                   ''~7501I~
                    chnext = SIGN_CHAR_NOTHING
                End If ''~7501I~
                cvch = cvKataHanZen_Hira(ch, chnext, swComposit)         ''~7501I~
                If swComposit Then                                          ''~7501I~
                    pos += 1                                             ''~7501I~
                End If                                                 ''~7501I~
                pos += 1                                               ''~7501I~
                sbHirakata.Append(cvch)                                ''~7501I~
            End While                                                  ''~7501I~
        End If                                                         ''~7501I~
        Dim strOut = sbHirakata.ToString()                             ''~7501I~
        sbHirakata.Clear()                                             ''~7501I~
        Return strOut                                                  ''~7501I~
    End Function                                                       ''~7501I~
    '   Public Function cvEisuAll(Ptext As String)                         ''~7501R~''~v101R~
    Public Function cvEisuAll(Ptext As String) As String               ''~v101R~
        'eisu zenkaku<->hankaku                                        ''~7501I~
        Dim sb As System.Text.StringBuilder                            ''~7501I~
        Dim pos, pose, asc As Integer                                       ''~7501I~
        Dim ch As Char                                       ''~7501R~
        cvText = Ptext                                                 ''~7501I~
        pose = Ptext.Length                                            ''~7501I~
        sb = New System.Text.StringBuilder(pose)
        pos = 0                                                        ''~7501I~
        While pos < pose                                               ''~7501I~
            ch = cvText.Chars(pos)                                     ''~7501I~
            asc = AscW(ch)                                             ''~7501I~
            If asc = UC_SPACE_SBCS Then                                       ''~7501R~
                ch = CHAR_SPACE                                         ''~7501I~
            ElseIf asc > UC_ASCII_START AndAlso asc <= UC_ASCII_END Then    ''~7501R~
                ch = ChrW(asc - UC_ASCII_START + UC_ASCII_START_DBCS)   ''~7501R~
            End If                                                     ''~7501I~
            pos += 1                                                   ''~7501I~
            sb.Append(ch)                                              ''~7501I~
        End While                                                      ''~7501I~
        Dim strOut = sb.ToString()                                     ''~7501I~
        sb.Clear()                                                     ''~7501I~
        Return strOut                                                  ''~7501I~
    End Function                                                       ''~7501I~
    Public Function isLargeLetter(Pch As Char, ByRef Ppcvch As Char) As Boolean ''~7501I~
        Ppcvch = SIGN_CHAR_NOTHING                                       ''~7501I~
        Dim idx As Integer = STR_LARGE_LETTER.IndexOf(Pch)             ''~7501R~
        If idx < 0 Then                                                       ''~7501I~
            Return False                                               ''~7501I~
        End If                                                         ''~7501I~
        Ppcvch = STR_SMALL_LETTER.Chars(idx)                           ''~7501R~
        Return True                                                    ''~7501I~
    End Function                                                       ''~7501I~
    Public Function isLargeLetter(Pch As Char) As Boolean              ''~v037I~
        Dim idx As Integer = STR_LARGE_LETTER.IndexOf(Pch)             ''~v037I~
        If idx < 0 Then                                                ''~v037I~
            Return False                                               ''~v037I~
        End If                                                         ''~v037I~
        Return True                                                    ''~v037I~
    End Function                                                       ''~v037I~
    Public Function isSmallLetter(Pch As Char, ByRef Ppcvch As Char) As Boolean ''~7501R~''~7502R~
        Ppcvch = SIGN_CHAR_NOTHING                                       ''~7501I~
        Dim idx As Integer = STR_SMALL_LETTER.IndexOf(Pch)             ''~7501R~
        If idx < 0 Then                                                       ''~7501I~
            Return False                                               ''~7501I~
        End If                                                         ''~7501I~
        Ppcvch = STR_LARGE_LETTER.Chars(idx)                           ''~7501R~
        Return True                                                    ''~7501I~
    End Function                                                       ''~7501I~
    Public Function isSmallLetter(Pch As Char) As Boolean              ''~v037I~
        Dim idx As Integer = STR_SMALL_LETTER.IndexOf(Pch)             ''~v037I~
        If idx < 0 Then                                                ''~v037I~
            Return False                                               ''~v037I~
        End If                                                         ''~v037I~
        Return True                                                    ''~v037I~
    End Function                                                       ''~v037I~
    '   Public Function changeLetterSmallLarge(Pch As Char, ByRef Ppcvch As Char) As Boolean ''~7502I~''~v026R~
    '*********************************************************************''~v105I~
    Public Function queryLetterSmallLarge(Pch As Char, PswForm1 As Boolean) As Boolean ''~v037I~
        Dim cvch As Char                                               ''~v037I~
        If changeLetterWrap(Pch, cvch, PswForm1) Then                  ''~v037R~
            If Not PswForm1 Then 'form3                                ''~v037I~
                Form1.formText.showStatus(Pch, typeSrc) 'Form3         ''~v037I~
            Else                                                       ''~v037I~
                Form1.MainForm.showStatus(Pch, typeSrc) 'Form1         ''~v037I~
            End If                                                     ''~v037I~
            Return True                                                ''~v037I~
        End If                                                         ''~v037I~
        If isLargeLetter(Pch) OrElse isSmallLetter(Pch) Then           ''~v037R~
            If PswForm1 Then                                                  ''~v037I~
                Form1.MainForm.showStatus(String.Format(Rstr.getStr("STR_MSG_INFO_SMALL_LARGE_TARGET"), Pch)) ''~v037I~
            Else                                                         ''~v037I~
                Form1.formText.showStatus(String.Format(Rstr.getStr("STR_MSG_INFO_SMALL_LARGE_TARGET"), Pch)) ''~v037I~
            End If                                                       ''~v037I~
            Return True                                                ''~v037I~
        End If                                                         ''~v037I~
        If PswForm1 Then                                                      ''~v037I~
            Form1.MainForm.showStatus(String.Format(Rstr.MSG_ERR_SMALL_LARGE, Pch)) ''~v037I~
        Else                                                             ''~v037I~
            Form1.formText.showStatus(String.Format(Rstr.MSG_ERR_SMALL_LARGE, Pch)) ''~v037I~
        End If                                                           ''~v037I~
        Return False                                                   ''~v037I~
    End Function                                                       ''~v037I~
    '*********************************************************************''~v105I~
'*  Public Function queryLetterSmallLargeOfCaret(Pch As Char, PswForm1 As Boolean) As Boolean ''~v105I~''+v120R~
    Public Function queryLetterSmallLargeOfCaret(Pch As Char, PswForm1 As Boolean,PmsgClear as Boolean) As Boolean''+v120I~
        Dim cvch As Char                                               ''~v105I~
        Dim msg As String = ""                                           ''~v105I~
        Dim rc As Boolean = False                                       ''~v105I~
        If changeLetterWrap(Pch, cvch, PswForm1) Then                  ''~v105R~
            Dim strSrc As String = FormatBES.getCharType(typeSrc)      ''~v105I~
            msg = Rstr.getStr("STR_MSG_CHANGELETTERWRAP_QUERY")        ''~v105I~
            msg = String.Format(msg, strSrc, Pch)                      ''~v105I~
            rc = True                                                    ''~v105I~
        End If                                                         ''~v105I~
      if rc orelse PmsgClear                                           ''+v120I~
        If PswForm1 Then                                                    ''~v105I~
            Form1.MainForm.showStatus(msg)                             ''~v105I~
        Else                                                           ''~v105I~
            Form1.formText.showStatus(msg)                             ''~v105I~
        End If                                                         ''~v105I~
      End If                                                           ''+v120I~
        Return rc                                                      ''~v105I~
    End Function                                                       ''~v105I~
    '*********************************************************************''~v105I~
    Public Function changeLetterSmallLarge(Pch As Char, ByRef Ppcvch As Char, PswForm1 As Boolean) As Boolean ''~v026I~
        If changeLetterWrap(Pch, Ppcvch, PswForm1) Then                      ''~v026I~
            If Not PswForm1 Then 'form3                                     ''~v034I~
                Form1.formText.showStatus(Pch, Ppcvch, typeSrc, typeTgt) 'Form3''~v034I~
            Else                                                       ''~v052I~
                Form1.MainForm.showStatus(Pch, Ppcvch, typeSrc, typeTgt) 'Form3''~v052I~
            End If                                                     ''~v034I~
            Return True                                                ''~v026I~
        End If                                                         ''~v026I~
        If isLargeLetter(Pch, Ppcvch) OrElse isSmallLetter(Pch, Ppcvch) Then  ''~7502I~
            Return True                                                ''~7502I~
        End If                                                         ''~7502I~
        '       MessageBox.Show("""" & Pch & """ は大文字小文字 変換対象ではありません") ''~7502I~''~7618R~
        '       MessageBox.Show(Pch, Rstr.MSG_ERR_SMALL_LARGE)                  ''~7618I~''~v034R~
        If PswForm1 Then                                                      ''~v037I~
            Form1.MainForm.showStatus(String.Format(Rstr.MSG_ERR_SMALL_LARGE, Pch)) ''~v037I~
        Else                                                             ''~v037I~
            Form1.formText.showStatus(String.Format(Rstr.MSG_ERR_SMALL_LARGE, Pch)) ''~v034R~
        End If                                                           ''~v037I~
        Return False
    End Function                                                       ''~7502I~
    '*********************************************************************''~v115R~
    Public Function changeLetterDakuon(Pch As Char, ByRef Ppcvch As Char, PswForm1 As Boolean) As Boolean''~v115R~
        '*seion-->dakuon-->handakuon-->seion                           ''~v115R~
        Dim rc As Boolean = True                                       ''~v115R~
        Dim idx As Integer                                             ''~v115R~
        Dim ch As Char = Pch                                           ''~v115R~
        Do                                                             ''~v115R~
            '*hahifuheho hiragana                                      ''~v115R~
            idx = STR_HANDAKUON_SRC2.IndexOf(Pch)   '*hiragana  babibubebo''~v115R~
            If idx >= 0 Then                                           ''~v115R~
                ch = STR_HANDAKUON_TGT.Chars(idx)    '*papipupepo      ''~v115R~
                Exit Do                                                ''~v115R~
            End If                                                     ''~v115R~
            idx = STR_HANDAKUON_TGT.IndexOf(Pch)   '*hiragana  papipupepo''~v115R~
            If idx >= 0 Then                                           ''~v115R~
                ch = STR_HANDAKUON_SRC.Chars(idx)    '*hahifuheho      ''~v115R~
                Exit Do                                                ''~v115R~
            End If                                                     ''~v115R~
            '*hahifuheho katakana                                      ''~v115R~
            idx = STR_HANDAKUON_SRC2_KATAKANA.IndexOf(Pch)   '*katakana  babibubebo''~v115R~
            If idx >= 0 Then                                           ''~v115R~
                ch = STR_HANDAKUON_TGT_KATAKANA.Chars(idx)    '*papipupepo''~v115R~
                Exit Do                                                ''~v115R~
            End If                                                     ''~v115R~
            idx = STR_HANDAKUON_TGT_KATAKANA.IndexOf(Pch)   '*hiragana  papipupepo''~v115R~
            If idx >= 0 Then                                           ''~v115R~
                ch = STR_HANDAKUON_SRC_KATAKANA.Chars(idx)    '*hahifuheho''~v115R~
                Exit Do                                                ''~v115R~
            End If                                                     ''~v115R~
            '*hahifuheho hankana                                       ''~v115R~
            idx = STR_HANDAKUON_SRC_HANKANA.IndexOf(Pch)     '*hankana  hahifuheho''~v115R~
            If idx >= 0 Then                                           ''~v115R~
                ch = STR_HANDAKUON_SRC2_KATAKANA.Chars(idx)    '*babibubebo''~v115R~
                Exit Do                                                ''~v115R~
            End If                                                     ''~v115R~
            '*other hiragana                                           ''~v115R~
            idx = STR_DAKUON_SRC.IndexOf(Pch)   '*hiragana kakikukeko  ''~v115R~
            If idx >= 0 Then                                           ''~v115R~
                ch = STR_DAKUON_TGT.Chars(idx)    '*gagigu...          ''~v115R~
                Exit Do                                                ''~v115R~
            End If                                                     ''~v115R~
            idx = STR_DAKUON_TGT.IndexOf(Pch)   '*hiragana gagigu...   ''~v115R~
            If idx >= 0 Then                                           ''~v115R~
                ch = STR_DAKUON_SRC.Chars(idx)  '*kakiku...            ''~v115R~
                Exit Do                                                ''~v115R~
            End If                                                     ''~v115R~
            '*other katakana                                           ''~v115R~
            idx = STR_DAKUON_SRC_KATAKANA.IndexOf(Pch)   '*katakan kakiku...''~v115R~
            If idx >= 0 Then                                           ''~v115R~
                ch = STR_DAKUON_TGT_KATAKANA.Chars(idx)  '*gagigu...   ''~v115R~
                Exit Do                                                ''~v115R~
            End If                                                     ''~v115R~
            idx = STR_DAKUON_TGT_KATAKANA.IndexOf(Pch)                 ''~v115R~
            If idx >= 0 Then                                           ''~v115R~
                ch = STR_DAKUON_SRC_KATAKANA.Chars(idx)                ''~v115R~
                Exit Do                                                ''~v115R~
            End If                                                     ''~v115R~
            rc = False                                                 ''~v115R~
            Exit Do                                                    ''~v115R~
        Loop                                                           ''~v115R~
        Ppcvch = ch                                                    ''~v115R~
        if not rc                                                      ''~v115I~
            Form1.showStatusForChild(PswForm1,String.Format(My.Resources.STR_MSG_ERR_DAKUON, Pch))''~v115I~
        End If                                                         ''~v115I~
        Return rc                                                      ''~v115R~
    End Function                                                       ''~v115R~
    '*********************************************************************''~v105I~
    Public Function changeLetterWrap(Pch As Char, ByRef Ppcvch As Char, PswForm1 As Boolean) As Boolean ''~v026I~
        Dim ch As Char                                                 ''~v026I~
        Select Case Pch                                                ''~v026I~
            Case CHAR_KANJI_KOU                                        ''~v026I~
                typeSrc = TYPE_KANJI                                     ''~v034I~
                If PswForm1 Then                                            ''~v026I~
                    Return False                                       ''~v026I~
                End If                                                 ''~v026I~
                ch = CHAR_KATAKANA_E                                     ''~v026I~
                typeTgt = TYPE_KATA                                      ''~v034I~
            Case CHAR_KATAKANA_E                                       ''~v026I~
                typeSrc = TYPE_KATA                                      ''~v034I~
                ch = CHAR_KATAKANA_E_SMALL                               ''~v026I~
                typeTgt = TYPE_KATA_SMALL                                ''~v034I~
            Case CHAR_KATAKANA_E_SMALL                                 ''~v026I~
                typeSrc = TYPE_KATA_SMALL                                ''~v034I~
                If PswForm1 Then                                            ''~v026I~
                    ch = CHAR_KATAKANA_E                                 ''~v026I~
                    typeTgt = TYPE_KATA                                  ''~v034I~
                Else                                                   ''~v026I~
                    ch = CHAR_KANJI_KOU                                  ''~v026I~
                    typeTgt = TYPE_KANJI                                 ''~v034I~
                End If                                                  ''~v026I~
            Case CHAR_KANJI_RIKI                                       ''~v026I~
                typeSrc = TYPE_KANJI                                     ''~v034I~
                If PswForm1 Then                                            ''~v026I~
                    Return False                                       ''~v026I~
                End If                                                 ''~v026I~
                ch = CHAR_KATAKANA_KA                                    ''~v026I~
                typeTgt = TYPE_KATA                                      ''~v034I~
            Case CHAR_KATAKANA_KA                                      ''~v026I~
                typeSrc = TYPE_KATA                                      ''~v034I~
                ch = CHAR_KATAKANA_KA_SMALL                              ''~v026I~
                typeTgt = TYPE_KATA_SMALL                                ''~v034I~
            Case CHAR_KATAKANA_KA_SMALL                                ''~v026I~
                typeSrc = TYPE_KATA_SMALL                                ''~v034I~
                If PswForm1 Then                                            ''~v026I~
                    ch = CHAR_KATAKANA_KA                                ''~v026I~
                    typeTgt = TYPE_KATA                                  ''~v034I~
                Else                                                   ''~v026I~
                    ch = CHAR_KANJI_RIKI                                 ''~v026I~
                    typeTgt = TYPE_KANJI                                 ''~v034I~
                End If                                                 ''~v026I~
            Case CHAR_KATAKANA_RI                                ''~v026I~
                typeSrc = TYPE_KATA                                      ''~v034I~
                ch = CHAR_HIRAGANA_RI                                    ''~v026I~
                typeTgt = TYPE_HIRA                                      ''~v034I~
            Case CHAR_HIRAGANA_RI                                ''~v026I~
                typeSrc = TYPE_HIRA                                      ''~v034I~
                ch = CHAR_KATAKANA_RI                                    ''~v026I~
                typeTgt = TYPE_KATA                                      ''~v034I~
            Case CHAR_KATAKANA_HE                                      ''~v026I~
                typeSrc = TYPE_KATA                                      ''~v034I~
                ch = CHAR_HIRAGANA_HE                                  ''~v026I~
                typeTgt = TYPE_HIRA                                      ''~v034I~
            Case CHAR_HIRAGANA_HE                                      ''~v026I~
                typeSrc = TYPE_HIRA                                      ''~v034I~
                ch = CHAR_KATAKANA_HE                                  ''~v026I~
                typeTgt = TYPE_KATA                                      ''~v034I~
            Case CHAR_KATAKANA_BE                                      ''~v026I~
                typeSrc = TYPE_KATA                                      ''~v034I~
                ch = CHAR_HIRAGANA_BE                                  ''~v026I~
                typeTgt = TYPE_HIRA                                      ''~v034I~
            Case CHAR_HIRAGANA_BE                                      ''~v026R~
                typeSrc = TYPE_HIRA                                      ''~v034I~
                ch = CHAR_KATAKANA_BE                                  ''~v026I~
                typeTgt = TYPE_KATA                                      ''~v034I~
            Case CHAR_KANJI_TA                                         ''~v026I~
                typeSrc = TYPE_KANJI                                     ''~v034I~
                If PswForm1 Then                                       ''~v026I~
                    Return False                                       ''~v026I~
                End If                                                 ''~v026I~
                ch = CHAR_KATAKANA_TA                                  ''~v026I~
                typeTgt = TYPE_KATA                                      ''~v034I~
            Case CHAR_KATAKANA_TA                                      ''~v026I~
                typeSrc = TYPE_KATA                                      ''~v034I~
                If PswForm1 Then                                       ''~v026I~
                    Return False                                       ''~v026I~
                End If                                                 ''~v026I~
                ch = CHAR_KANJI_TA                                     ''~v026I~
                typeTgt = TYPE_KANJI                                     ''~v034I~
            Case CHAR_KANJI_NI                                         ''~v036R~
                typeSrc = TYPE_KANJI                                   ''~v036R~
                If PswForm1 Then                                       ''~v036R~
                    Return False                                       ''~v036R~
                End If                                                 ''~v036R~
                ch = CHAR_KATAKANA_NI                                  ''~v036R~
                typeTgt = TYPE_KATA                                    ''~v036R~
            Case CHAR_KATAKANA_NI                                      ''~v036R~
                typeSrc = TYPE_KATA                                    ''~v036R~
                If PswForm1 Then                                       ''~v036R~
                    Return False                                       ''~v036R~
                End If                                                 ''~v036R~
                ch = CHAR_KANJI_NI                                     ''~v036R~
                typeTgt = TYPE_KANJI                                   ''~v036R~
            Case CHAR_KANJI_HACHI                                      ''~v036R~
                typeSrc = TYPE_KANJI                                   ''~v036R~
                If PswForm1 Then                                       ''~v036R~
                    Return False                                       ''~v036R~
                End If                                                 ''~v036R~
                ch = CHAR_KATAKANA_HA                                  ''~v036R~
                typeTgt = TYPE_KATA                                    ''~v036R~
            Case CHAR_KATAKANA_HA                                      ''~v036R~
                typeSrc = TYPE_KATA                                    ''~v036R~
                If PswForm1 Then                                       ''~v036R~
                    Return False                                       ''~v036R~
                End If                                                 ''~v036R~
                ch = CHAR_KANJI_HACHI                                  ''~v036R~
                typeTgt = TYPE_KANJI                                   ''~v036R~
            Case CHAR_KANJI_KUCHI                                      ''~v036R~
                typeSrc = TYPE_KANJI                                   ''~v036R~
                If PswForm1 Then                                       ''~v036R~
                    Return False                                       ''~v036R~
                End If                                                 ''~v036R~
                ch = CHAR_KATAKANA_RO                                  ''~v036R~
                typeTgt = TYPE_KATA                                    ''~v036R~
            Case CHAR_KANJI_BOKU                                       ''~v036I~
                typeSrc = TYPE_KANJI                                   ''~v036I~
                If PswForm1 Then                                       ''~v036I~
                    Return False                                       ''~v036I~
                End If                                                 ''~v036I~
                ch = CHAR_KATAKANA_TO                                  ''~v036I~
                typeTgt = TYPE_KATA                                    ''~v036I~
            Case CHAR_KATAKANA_TO                                      ''~v036I~
                typeSrc = TYPE_KATA                                    ''~v036I~
                If PswForm1 Then                                       ''~v036I~
                    Return False                                       ''~v036I~
                End If                                                 ''~v036I~
                ch = CHAR_KANJI_BOKU                                   ''~v036I~
                typeTgt = TYPE_KANJI                                   ''~v036I~
            Case CHAR_KANJI_KUCHI                                      ''~v036I~
                typeSrc = TYPE_KANJI                                   ''~v036I~
                If PswForm1 Then                                       ''~v036I~
                    Return False                                       ''~v036I~
                End If                                                 ''~v036I~
                ch = CHAR_KATAKANA_RO                                  ''~v036I~
                typeTgt = TYPE_KATA                                    ''~v036I~
            Case CHAR_KATAKANA_RO                                      ''~v036R~
                typeSrc = TYPE_KATA                                    ''~v036R~
                If PswForm1 Then                                       ''~v036R~
                    Return False                                       ''~v036R~
                End If                                                 ''~v036R~
                ch = CHAR_KANJI_KUCHI                                  ''~v036R~
                typeTgt = TYPE_KANJI                                   ''~v036R~
            Case CHAR_KANJI_ICHI                                       ''~v036R~
                typeSrc = TYPE_KANJI                                   ''~v036R~
                If PswForm1 Then                                       ''~v036R~
                    Return False                                       ''~v036R~
                End If                                                 ''~v036R~
                ch = CHAR_KATAKANA_CHOON                               ''~v036R~
                typeTgt = TYPE_KATA                                    ''~v036R~
            Case CHAR_KATAKANA_CHOON                                   ''~v036R~
                typeSrc = TYPE_KATA                                    ''~v036R~
                ch = CHAR_KIGO_HBAR                                    ''~v036R~
                typeTgt = TYPE_KIGO                                    ''~v036R~
            Case CHAR_KIGO_HBAR                                        ''~v036R~
                typeSrc = TYPE_KIGO                                    ''~v036R~
                If PswForm1 Then                                       ''~v036R~
                    ch = CHAR_KATAKANA_CHOON                           ''~v036R~
                    typeTgt = TYPE_KATA                                ''~v036R~
                Else                                                   ''~v036R~
                    ch = CHAR_KANJI_ICHI                               ''~v036R~
                    typeTgt = TYPE_KANJI                               ''~v036R~
                End If                                                 ''~v036R~
            Case CHAR_KATAKANA_SO                                      ''~v037I~
                typeSrc = TYPE_KATA                                    ''~v037I~
                ch = CHAR_KATAKANA_N                                   ''~v037I~
                typeTgt = TYPE_KATA                                    ''~v037I~
            Case CHAR_KATAKANA_N                                       ''~v037R~
                typeSrc = TYPE_KATA                                    ''~v037R~
                ch = CHAR_KATAKANA_SO                                  ''~v037I~
                typeTgt = TYPE_KATA                                    ''~v037I~
            Case CHAR_KANJI_JU                                         ''~v037I~
                typeSrc = TYPE_KANJI                                   ''~v037I~
                ch = CHAR_KIGO_PLUS                                    ''~v037I~
                If PswForm1 Then                                       ''~v037I~
                    Return False                                       ''~v037I~
                End If                                                 ''~v037I~
                typeTgt = TYPE_KIGO                                    ''~v037I~
            Case CHAR_KIGO_PLUS                                        ''~v037I~
                typeSrc = TYPE_KIGO                                    ''~v037I~
                If PswForm1 Then                                       ''~v037I~
                    Return False                                       ''~v037I~
                End If                                                 ''~v037I~
                ch = CHAR_KANJI_JU                                     ''~v037I~
                typeTgt = TYPE_KANJI                                   ''~v037I~
            Case Else                                                  ''~v026I~
                Return False                                           ''~v026I~
        End Select                                                     ''~v026I~
        Ppcvch = ch                                                      ''~v026I~
        Return True                                                    ''~v026I~
    End Function                                                       ''~v026I~
    '*********************************************************************''~v105I~
'   Public Function changeLetterOther(Pch As Char, ByRef Ppcvch As Char) As Integer ''~7525I~''~v101R~
    Public Function changeLetterOther(Pch As Char, ByRef Ppcvch As Char,PswForm1 as Boolean) As Integer''~v101R~
        '  Ha<-->Wa , U<-->cho-on etc                                      ''~7525I~
        ' rc=0 err,1:repeat                                               ''~7525I~
        Dim rc As Integer = 0                                            ''~7525I~
        Dim idx As Integer                                             ''~7525I~
        Ppcvch = SIGN_CHAR_NOTHING                                     ''~7525I~
#If False Then                                                              ''~7608I~
        idx = STR_REPEAT.IndexOf(Pch)                                  ''~7525I~
        If idx >= 0 Then                                                      ''~7525R~
            rc = 1                                                       ''~7525I~
        Else                                                           ''~7525I~
#End If                                                                ''~7608I~
        idx = STR_LARGE_LETTER_OTHER.IndexOf(Pch)       ''~7525I~
        If idx >= 0 Then                                           ''~7525I~
            Ppcvch = STR_SMALL_LETTER_OTHER.Chars(idx)               ''~7525I~
            rc = 2                                                   ''~7525I~
        Else                                                       ''~7525I~
            idx = STR_SMALL_LETTER_OTHER.IndexOf(Pch)   ''~7525I~
            If idx >= 0 Then                                       ''~7525I~
                Ppcvch = STR_LARGE_LETTER_OTHER.Chars(idx)           ''~7525I~
                rc = 3                                               ''~7525I~
            End If                                                 ''~7525I~
        End If                                                     ''~7525I~
#If False Then                                                              ''~7608I~
        End If                                                         ''~7525I~
#End If                                                                ''~7608I~
        If rc = 0 Then                                                        ''~7525I~
            '           MessageBox.Show("""" & STR_LARGE_LETTER_OTHER & """" & "<-->" & """" & STR_SMALL_LETTER_OTHER & """ の変換対象外です") ''~7525I~''~7608R~''~7618R~
            '           MessageBox.Show(Pch, Rstr.MSG_ERR_SMALL_LARGE_OTHER & "(" & STR_LARGE_LETTER_OTHER & "<-->" & STR_SMALL_LETTER_OTHER & ")") ''~7618I~''~v034R~
         if PswForm1                                                   ''~v053I~
            Form1.MainForm.showStatus(String.Format(Rstr.MSG_ERR_SMALL_LARGE_OTHER, Pch) & "(" & STR_LARGE_LETTER_OTHER & "<-->" & STR_SMALL_LETTER_OTHER & ")")''~v053I~
         else                                                          ''~v053I~
            Form1.formText.showStatus(String.Format(Rstr.MSG_ERR_SMALL_LARGE_OTHER, Pch) & "(" & STR_LARGE_LETTER_OTHER & "<-->" & STR_SMALL_LETTER_OTHER & ")") ''~v034R~
         end if                                                        ''~v053I~
        End If                                                         ''~7525I~
        Return rc                                                      ''~7525I~
    End Function                                                       ''~7525I~
#If False Then                                                              ''~7608I~
    Public Function getRepeatStr(Ptext As String, Ppos As Integer) As String ''~7525R~
        'get string before Repeatchar                                      ''~7525I~
        Dim pos As Integer = Ppos                                        ''~7525I~
        Dim len As Integer = 0                                           ''~7525I~
        Dim idx As Integer                                             ''~7525I~
        Dim str As String = Nothing                                      ''~7525I~
        Dim ch As Char = Ptext.Chars(Ppos)                                            ''~7525I~
        idx = STR_REPEAT.IndexOf(ch)                                   ''~7525I~
        If idx < 0 Then                                                       ''~7525I~
            Return Nothing                                             ''~7525I~
        End If                                                         ''~7525I~
        Select Case idx                                                ''~7525I~
            Case 0 'kanji(repeat)                                          ''~7525I~
                While True                                                 ''~7525R~
                    pos -= 1                                               ''~7525R~
                    If pos < 0 Then                                        ''~7525R~
                        Exit While                                         ''~7525R~
                    End If                                                 ''~7525R~
                    Dim ch2 = Ptext.Chars(pos)                      ''~7525R~
                    If ch2 = CHAR_SPACE OrElse ch2 = CHAR_SPACE_SBCS Then    ''~7525R~
                        Exit While                                         ''~7525R~
                    End If                                                 ''~7525R~
                    len += 1                                               ''~7525R~
                End While                                                  ''~7525R~
                If len > 0 Then                                               ''~7525I~
                    str = Ptext.Substring(pos + 1, len)                ''~7525R~
                End If                                                 ''~7525I~
            Case Else                                                      ''~7525I~
                If pos > 0 Then                                            ''~7525I~
                    str = getRepeatChar(idx, Ptext, pos - 1)                     ''~7525I~
                End If                                                     ''~7525I~
        End Select                                                     ''~7525I~
        If isnothing(str) Then                                              ''~7525R~
            Select Case idx                                            ''~7525I~
                Case 0                                                     ''~7525I~
                    MessageBox.Show("エラー (""" & ch & """は前のスペース以降を繰り返します)") ''~7525R~
                Case 1, 3                                                   ''~7525I~
                    MessageBox.Show("エラー (""" & ch & """は前のひらがなの繰り返しです)") ''~7525I~
                Case 2, 4                                                   ''~7525I~
                    MessageBox.Show("エラー (""" & ch & """は前のカタカナの繰り返しです)") ''~7525I~
            End Select                                                 ''~7525I~
            Return Nothing                                             ''~7525I~
        End If                                                         ''~7525I~
        Return str                                                     ''~7525I~
    End Function                                                       ''~7525I~
    Public Function getRepeatChar(Pidx As Integer, Ptext As String, Ppos As Integer) ''~7525I~
        Dim str As String = Nothing                                      ''~7525I~
        Dim ch As Char
        Dim idx As Integer ''~7525I~
        ch = Ptext.Chars(Ppos)                                           ''~7525I~
        If ch = CHAR_SPACE OrElse ch = CHAR_SPACE_SBCS Then         ''~7525I~
            Return Nothing                                             ''~7525I~
        End If                                                         ''~7525I~
        Select Case Pidx                                               ''~7525I~
            Case 1  'hira not Dakuon                                   ''~7525I~
                If isHiragana(ch) = 1 Then                                    ''~7525I~
                    str = Ptext.Substring(Ppos, 1)                     ''~7525I~
                End If                                                 ''~7525I~
            Case 3  'kata not Dakuon                                   ''~7525I~
                If isKatakanaZenkaku(ch) = 1 Then                             ''~7525I~
                    str = Ptext.Substring(Ppos, 1)                     ''~7525I~
                End If                                                 ''~7525I~
            Case 2   ' hira dakuon                                         ''~7525I~
                idx = STR_DAKUON_SRC.IndexOf(ch)                             ''~7525I~
                If idx >= 0 Then                                                  ''~7525I~
                    str = STR_DAKUON_TGT.substring(idx, 1)              ''~7525R~
                End If                                                     ''~7525I~
            Case 4   'kata dakuon                                          ''~7525I~
                idx = STR_DAKUON_SRC_KATAKANA.IndexOf(ch)                    ''~7525I~
                If idx >= 0 Then                                                  ''~7525I~
                    str = STR_DAKUON_TGT_KATAKANA.substring(idx, 1)     ''~7525I~
                End If                                                     ''~7525I~
        End Select                                                     ''~7525I~
        Return str                                                     ''~7525I~
    End Function                                                       ''~7525I~
#End If                                                                ''~7608I~
    Public Shared Function getCharType(Ptype As Integer) As String     ''~v034I~
        Dim str As String                                              ''~v034I~
        Select Case Ptype                                                   ''~v034I~
            Case TYPE_KANJI                                            ''~v034I~
                str = Rstr.getStr("STR_CHAR_TYPE_KANJI")                 ''~v034I~
            Case TYPE_HIRA                                             ''~v034I~
                str = Rstr.getStr("STR_CHAR_TYPE_HIRAGANA")              ''~v034I~
            Case TYPE_KATA                                             ''~v034I~
                str = Rstr.getStr("STR_CHAR_TYPE_KATAKANA")              ''~v034I~
            Case TYPE_KATA_SMALL                                       ''~v034I~
                str = Rstr.getStr("STR_CHAR_TYPE_KATAKANA_SMALL")        ''~v034I~
            Case TYPE_KIGO                                             ''~v034I~
                str = Rstr.getStr("STR_CHAR_TYPE_KIGO")                ''~v034I~
            Case Else                                                  ''~v034I~
                str = Rstr.getStr("STR_CHAR_TYPE_UNKNOWN")               ''~v034I~
        End Select                                                     ''~v034I~
        Return str                                                     ''~v034I~
    End Function                                                       ''~v034I~
End Class
