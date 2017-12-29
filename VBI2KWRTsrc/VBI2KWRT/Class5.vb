''*CID:''+v127R~:#72                          update#=  241;          ''~v126R~''+v127R~
'************************************************************************************''~v001I~
'v127 2017/12/29 conv katakana+kanji+hiragana at once for the case yogore(to)ri''+v127I~
'v126 2017/12/29 (Bug)char positioning err by BESstyle consideration for "..."''~v126I~
'v101 2017/12/16 Conversion warning                                    ''~v101I~
'v062 2017/09/23 kanji repeated char "々" is not treated as kanji      ''~v062I~
'v033 2017/09/21 insert space before katakana only when prev of katakana is josi or 2 sounds string;''~v033I~
'                it is difficult,so no space insert berfore after katakana''~v033I~
'v032 2017/09/21 English document, i2e was not used                    ''~v032I~
'v031 2017/09/21 katakana document, put no space before hiragana       ''~v031R~
'v030 2017/09/21 new option dialog for each document                   ''~v030I~
'v029 2017/09/18 like v025 send kanji+ツ(tsu)/ッ(tsu-small) to kanjiconvg''~v029I~
'v028 2017/09/19 He/Be replacement was supported by v026,so do not auto replacement''~v028I~
'v027 2017/09/19 when all katakana document, all He/Be treate as katakana''~v027I~
'v025 2017/09/18 カ(ka)/ケ(ke)+kanji send to kanjiconvg                ''~v025I~
'v024 2017/09/18 appendspace have to chk space existing                ''~v024I~
'v023 2017/09/18 Not 3人->3hito but 3nin                               ''~v023I~
'v022 2017/09/18 hirakata-Be,kanji+be allows next type(kanji+BE+kata-->kata,kanji+BE+hira->hira)''~v022I~
'v021 2017/09/18 conv katakana+kanji(case of indo-jin)                 ''~v021I~
'v020 2017/09/18 for katakana document,treate katakana same as hiragana''~v020I~
'v019 2017/09/17 Try space before kanji if length>=3                   ''~v019R~
'v016 2017/09/17 apply v014 for not only kanji conv out but also kana(it may by replaced by dictionary)''~v016I~
'v015 2017/09/17 saveed evenif not updated                             ''~v015I~
'v014 2017/09/16 change "う" to "ー" if after "ゅ" or "ょ" (rule is widely after "う" row or "お" row)''~v013I~
'v013 2017/09/16 katakana "ー"(815b=u30fc) have to be assumed as hiraganais for BES99''~v010I~
'v010 2017/09/15 try another kanji->kana conversion(split by delm char,ignore crlf)''~v010I~
'v009 2017/09/12 hirgana Ri and katakana Ri is similar looking         ''~v009I~
'v008 2017/09/12 dictionary support                                    ''~v008I~
'v006 2017/09/12 katakana okurigana is optional                        ''~v006I~
'v005 2017/09/12 u30fc(ー) is katakana                                 ''~v005I~
'v004 2017/09/12 hirgana He/Be and katakana He/Be are similar looking      ''~v004I~''~v009R~
'v003 2017/09/11 dumplicated confirm save override msg for receive text''~v003I~''~v004R~
'v001 2017/09/11 consider continuing word over EOL                     ''~v001I~''~v004R~
'************************************************************************************''~v001I~
Imports System.Runtime.InteropServices                                 ''~7522I~
Imports System.Text                                      ''~v008I~
Public Class ClassKanaText                                             ''~7522R~
    'localization done                                                     ''~7618I~
    '**Kanji-->Kana Conversion                                             ''~7525I~
    ''~7522I~
    Private Const FONTSIZE_INCREASE = 5                                        ''~7522I~''~7608R~
    Private Const FUSEJI_FOLLOWING = "まみむめもマミムメモﾏﾐﾑﾒﾓ"         ''~7608I~
    '   Private Const HIRAGANA_RI = "り"                                   ''~v009I~''~v062R~
    '   Private Const KATAKANA_RI = "リ"                                   ''~v009I~''~v062R~
    '   Private Const HIRAGANA_HE = "へ"                                   ''~v004I~''~v062R~
    Private Const HIRAGANA_HE = "へ"c                                  ''~v062I~
    '   Private Const HIRAGANA_BE = "べ"                                   ''~v004I~''~v062R~
    Private Const HIRAGANA_BE = "べ"c                                  ''~v062I~
    '   Private Const KATAKANA_HE = "ヘ"                                   ''~v004I~''~v062R~
    Private Const KATAKANA_HE = "ヘ"c                                  ''~v062I~
    '   Private Const KATAKANA_BE = "ベ"                                   ''~v004I~''~v062R~
    Private Const KATAKANA_BE = "ベ"c                                  ''~v062I~
    '   Private Const KATAKANA_CHOON = "ー"                                ''~v005I~''~v062R~
    Private Const KATAKANA_CHOON = "ー"c                               ''~v062I~
    '   Private Const DELMCHARS = " ,.　、。"                              ''~v023R~
    Private Const DELMCHARS = " ,.?!　、。・？！｡､･"    '+ hankaku katakana                            ''~v010I~''~v023I~
    Private Const KAKECHARS = "カケヵヶ"                               ''~v025I~
    Private Const TSUCHARS = "ツッ"                                    ''~v029I~
    Private Const CHAR_DQ_DBCS = ChrW(&HFF02)                          ''~v023I~
    Private Const CHAR_DQ_DBCS1 = ChrW(&H201C)                          ''~v023I~
    Private Const CHAR_DQ_DBCS2 = ChrW(&H201D)                          ''~v023I~
    Private Const SPLITCHARS = "()[]{}<>'""（）［］｛｝＜＞｢｣「」『』《》【】" & CHAR_DQ_DBCS & CHAR_DQ_DBCS1 & CHAR_DQ_DBCS2 ''~v023R~''~v024R~
    Private Const HIRAGANA_YU_U = "ゅう"   ' u3085+u3046               ''~v014I~
    Private Const HIRAGANA_YO_U = "ょう"   ' u3087+u3046               ''~v014I~
    Private Const HIRAGANA_YU_CHOON = "ゅー"   ' u3085+u30fc            ''~v014I~
    Private Const HIRAGANA_YO_CHOON = "ょー"   ' u3087+u30fc           ''~v014I~
    Private Const CHAR_NULL = Chr(&H0)                                 ''~v010R~
    Private Const CHARTYPE_DELM = Chr(&H5)                             ''~v010I~
    Private Const CHARTYPE_DELM_ASCW = 5                                 ''~v010I~
    Private Const KANJI_SPACE_THRESHOLD = 3                            ''~v019R~
    ''~v010I~
    Private imageFilename As String = ""                               ''~7522I~
    Private textFilename As String = ""                                ''~7522I~
    Private formWidth As Integer = My.Settings.CfgFormSizeWKanaText    ''~7522I~
    Private formHeight As Integer = My.Settings.CfgFormSizeHKanaText   ''~7522I~
    Private fel As Object                                              ''~7522I~
    Private strKana As String                                          ''~7522I~
    '   Private dictionaryWords As String() = {"飛び地","入会権"}          ''~v008R~
    '   Private dictionaryRepWords As String() = {"とびち","いりあいけん"} ''~v008R~
    Private undoRedo As ClassUndoRedo                                  ''~7522I~
    Public swViewKatakana As Boolean = False                           ''~7522I~
    '   Private dlgOptions As FormOptions                                  ''~7522I~''~7525R~
    Private fmtBES As FormatBES                                        ''~7522I~
    Private swSource As Integer = 0                                    ''~7522I~
    '   Private TB As TextBox '@@@@                                        ''~7522I~''~7618R~
    Private title As String                                            ''~7522I~
    Private swRead As Boolean = False                                    ''~7612I~
    Private repeatChars As Char() = FormatBES.STR_REPEAT.ToCharArray()  ''~7607I~
    Private swEnglishDoc As Boolean                                    ''~7619I~
    Public Shared swMethod2 As Boolean = True                            ''~v010I~
    Public Shared swMethod2KanjiSpace As Boolean = True                ''~v019R~
    Public swSaved As Boolean = False                               ''~v015I~
    '   Private swChngByUndoRedo As Boolean                            ''~7522I~
    ''~7522I~
    Private swException As Boolean = False                               ''~v010I~
    Sub setText(PstrText As String, PswImage As Integer, Pfnm As String) ''~7522I~
        swRead = False                                                   ''~7612I~
        '       If PswImage = 1 Then                                           ''~7522I~''~7615R~
        '           title = "読取りかな変換=" & Pfnm                           ''~7522I~''~7615R~
        '       Else                                                           ''~7522I~''~7615R~
        '           title = "テキストかな変換=" & Pfnm                         ''~7522I~''~7615R~
        '       End If                                                         ''~7522I~''~7615R~
        ''~v032I~
        swEnglishDoc = Form3.swEnglishDoc                              ''~v032M~
        If swEnglishDoc OrElse DocOptions.swEnglishDoc Then              ''~v032I~
            title = Rstr.getStr("STR_ENGLISH_DOC") & Form1.TITLE_SEP & Pfnm ''~v032I~
        Else                                                             ''~v032I~
            title = Rstr.FORM1_TITLE_KANJI2KANA_FILE & Form1.TITLE_SEP & Pfnm ''~7615R~
        End If                                                           ''~v032I~
        swSource = 1  'received                                        ''~7522I~
        swSaved = False                                                  ''~v015I~
        textFilename = Pfnm                                            ''~7522I~
        '       swEnglishDoc = Form1.chkExt(Pfnm, Form1.FILTER_DEFAULT_ENGLISHTEXT) ''~7619R~''~v032R~
        If swEnglishDoc OrElse DocOptions.swEnglishDoc Then                                                ''~7619I~''~v032R~
            strKana = convEnglish(PstrText)                            ''~7618I~
        Else                                                           ''~7618I~
            fel = Activator.CreateInstance(Type.GetTypeFromProgID("MSIME.Japan")) ''~7522I~''~7618R~
            JPReverseConv.Main(1, fel, "")  'open                          ''~7522I~''~7618R~
            '           strKana = conv2Kana(PstrText)                                  ''~7522I~''~7618R~''~v008R~
            Dim strText As String = applyDictionary(PstrText)            ''~v008I~
            If Not swException Then                                    ''~v010I~
                strKana = conv2Kana(strText)                               ''~v008I~''~v010R~
                JPReverseConv.Main(3, fel, "")    'close                       ''~7522I~''~7618R~''~v010R~
            End If                                                     ''~v010I~
        End If                                                         ''~7618I~
        If swException Then                                                 ''~v010I~
            Exit Sub                                                   ''~v010I~
        End If                                                         ''~v010I~
        showKanaText(strKana)                                          ''~7522I~
    End Sub                                                            ''~7522I~
    ''~7522I~
    Public Sub readText(Pfnm As String)                                ''~7522I~
        swSource = 0  'read file                                       ''~7522I~''~7612M~
        swRead = False                                                   ''~7612I~
        swEnglishDoc = False                                           ''~v032I~
        If Pfnm.CompareTo("") = 0 Then                                        ''~7612I~
            Dim fnm As String = Form1.changeExt(Rstr.MENU_NEWTEXT_FILE, Form1.FILTER_DEFAULT_KANATEXT) ''~7612I~
            '       	title = "かなテキスト=" & fnm                              ''~7612R~''~7615R~
            title = Rstr.FORM1_TITLE_KANA_FILE & Form1.TITLE_SEP & fnm ''~7615R~
            textFilename = fnm                                         ''~7612I~
            showKanaText(vbCrLf)                                       ''~7612I~
        Else                                                           ''~7612I~
            '       	title = "かなテキスト=" & Pfnm                                 ''~7522I~''~7612R~''~7615R~
            title = Rstr.FORM1_TITLE_KANA_FILE & Form1.TITLE_SEP & Pfnm ''~7615R~
            textFilename = Pfnm                                            ''~7522I~''~7612I~
            If (Not (System.IO.File.Exists(textFilename))) Then            ''~7522I~''~7612R~
                Form1.NotFound(Pfnm)                                       ''~7522I~''~7612R~
                Exit Sub                                                   ''~7522I~''~7612R~
            End If                                                         ''~7522I~''~7612R~
            Try                                                            ''~7522I~''~7612R~
                Dim text As String = System.IO.File.ReadAllText(textFilename, System.Text.Encoding.Default) ''~7522I~''~7612R~
                showKanaText(text)                                         ''~7522I~''~7612R~
                swRead = True                                            ''~7612I~
            Catch ex As Exception                                          ''~7522I~''~7612R~
                Form1.ReadError(Pfnm, ex)                                  ''~7522I~''~7612R~
            End Try                                                        ''~7522I~''~7612R~
        End If                                                         ''~7612I~
    End Sub                                                            ''~7522I~
    ''~7522I~
    Private Sub showKanaText(Pstr As String)                           ''~7522I~
        undoRedo = Form1.MainForm.undoRedo                             ''~7522I~
        fmtBES = Form1.MainForm.fmtBES                                 ''~7522I~
        '       dlgOptions = Form1.MainForm.dlgOptions                         ''~7522I~''~7525R~
        '       undoRedo = New ClassUndoRedo(ClassUndoRedo.OPT_KANATEXT, TextBox1, ToolStripButtonUndo, ToolStripButtonRedo)''~7522I~
        '       swViewKatakana = dlgOptions.swKatakana                 ''~7522I~
        '        setkatakanaBtn()                                      ''~7522I~
        Form1.MainForm.receiveText(Pstr, title, swSource)              ''~7522I~
    End Sub                                                            ''~7522I~
    ''~7522I~
    Public Function chkDiscard(e As System.EventArgs) As Boolean       ''~7522I~
        ' from from3:kana button                                       ''~7522I~
        ' rc:true=continue process                                     ''~7522I~
        Dim rc As Boolean = True                                       ''~7522I~
        If IsNothing(undoRedo) Then      'file not found or read err            ''~7617I~''~7619R~
            Return True                                                ''~7617I~
        End If                                                         ''~7617I~
        If Not Form1.MainForm.TBBES.Enabled Then                            ''~7619I~
            Return True                                                ''~7619I~
        End If                                                         ''~7619I~
        If Not IsNothing(e) Then 'from form1,new read                  ''~7522I~
            rc = Form1.confirmDiscard(e, Form1.MainForm.Text)          ''~7522I~
        Else                     '                                     ''~7522I~
            If undoRedo.isUpdated() Then                               ''~7522I~
                '           rc = Form1.confirmDiscard(e, Me.Text)      ''~7522I~
                rc = Form1.confirmDiscard(e, Form1.MainForm.Text)      ''~7522I~
            End If                                                     ''~7522I~
        End If                                                         ''~7522I~
        If rc Then                                                     ''~7522I~
            undoRedo.resetForm1()                                      ''~7522I~
        End If                                                         ''~7522I~
        Return rc                                                      ''~7522I~
    End Function                                                       ''~7522I~
    Public Function isUpdated() As Boolean                             ''~v015I~
        If IsNothing(undoRedo) Then      'file not found or read err   ''~v015I~
            Return False                                               ''~v015I~
        End If                                                         ''~v015I~
        If Not Form1.MainForm.TBBES.Enabled Then                       ''~v015I~
            Return False                                               ''~v015I~
        End If                                                         ''~v015I~
        Return undoRedo.isUpdated()                                   ''~v015I~
    End Function                                                       ''~v015I~
    '   Public Function chkDiscard()                                       ''~7522I~''~v101R~
    Public Function chkDiscard() As Boolean                            ''~v101I~
        ' from Form1(read file)                                        ''~7522I~
        ' rc:true:continue process                                     ''~7522I~
        Return chkDiscard(Nothing)                                     ''~7522I~
    End Function                                                       ''~7522I~
    Public Sub save() 'from Form1                                      ''~7522I~
        If swSource = 1 Then    'from imagefile/kanjitext              ''~7522I~
            If swSaved AndAlso Not isUpdated() Then                         ''~v015I~
                Form1.FileNotSaved()                                   ''~v015I~
                Exit Sub                                               ''~v015I~
            End If                                                     ''~v015I~
            If Not Form1.confirmReceivedSave(textFilename) Then        ''~7522I~
                Exit Sub                                               ''~7522I~
            End If                                                     ''~7522I~
            '       End If                                                         ''~7522I~''~v003R~
        Else                                                           ''~v003I~
            If Not swRead Then                                                  ''~7612I~
                If Not Form1.confirmNewText(textFilename) Then                  ''~7612R~
                    Exit Sub                                               ''~7612I~
                End If                                                     ''~7612I~
            Else                                                       ''~v015I~
                If Not isUpdated() Then                                     ''~v015I~
                    Form1.FileNotSaved()                               ''~v015I~
                    Exit Sub                                           ''~v015I~
                End If                                                 ''~v015I~
            End If                                                         ''~7612I~
        End If                                                         ''~v003I~
        saveFile(textFilename)                                         ''~7522I~
    End Sub                                                            ''~7522I~
    ''~7522I~
    Public Sub SaveAs() 'from Form1                                    ''~7522I~
        Dim dlg = Form1.MainForm.SaveFileDialog1                         ''~7522R~
        If dlg.ShowDialog() = DialogResult.OK Then                     ''~7522I~
            Dim fnm As String = dlg.FileName                           ''~7522I~
            saveFile(fnm)                                              ''~7522I~
        End If                                                         ''~7522I~
    End Sub                                                            ''~7522I~
    ''~7522I~
    Private Sub saveFile(Pfnm As String)                               ''~7522I~
        Dim txt As String                                              ''~7522I~
        txt = undoRedo.getTextToSave()                                 ''~7522I~
        Try                                                            ''~7522I~
            System.IO.File.WriteAllText(Pfnm, txt, System.Text.Encoding.Default) ''~7522I~
            Form1.MainForm.insertMRUList(3, Pfnm)                      ''~7522I~
            undoRedo.saved()                                           ''~7522I~
            '           MessageBox.Show(Pfnm & " を保存しました")                  ''~7522I~''~7618R~
            Form1.FileSaved(Pfnm)                                      ''~7618I~
            swSaved = True                                               ''~v015I~
        Catch ex As Exception                                          ''~7522I~
            Form1.WriteError(Pfnm, ex)                                 ''~7522I~
        End Try                                                        ''~7522I~
    End Sub                                                            ''~7522I~
    ''~7522I~
    ''~7522I~
    Function conv2Kana(Pfiletext As String) As String                  ''~7522I~
        '* deplicated, moved to conv2KanaM2 ****                           ''~v101I~
        If swMethod2 Then                                                   ''~v010I~
            Return conv2KanaM2(Pfiletext)                              ''~v010I~
        End If                                                        ''~v010I~
        Dim sb = New System.Text.StringBuilder()                       ''~7522I~
        Dim sbConv = New System.Text.StringBuilder()                   ''~v001I~
        Dim chars() As Char = Pfiletext.ToCharArray()                  ''~7522I~
        Dim charctr As Integer = chars.Length()                        ''~7522I~
        '       Dim ii As Integer = 0, wordctr As Integer = 0, cvctr As Integer = 0, poskanji As Integer = 0 ''~7522I~''~v001R~
        Dim ii As Integer = 0, wordctr As Integer = 0                  ''~v001I~
        Dim kanastr As String                                          ''~7522I~
        Dim chcode As Integer                                          ''~7522I~''~7525R~
        '       Dim prevchcode As Integer                                      ''~v004I~''~v008R~
        Dim ch, chii, chiiprev As Char                                                 ''~7522I~''~7607R~
        Dim swkanji As Boolean = False                                 ''~7525R~
        Dim swprevkana As Boolean                                      ''~7525I~
        Dim swprevhiragana, swprevkatakana As Boolean                   ''~7608I~
        Dim nextpos As Integer                                         ''~7604R~
        '       Dim iinext As Integer                                          ''~v001I~''~v008R~
        Dim swKatakanaDoc As Boolean = DocOptions.swKatakanaDoc    'allow katakana as okurigana''~v006I~''~v030R~
        swException = False                                              ''~v010I~
        Try                                                            ''~7522I~
            chii = Chr(&H0)                                              ''~7607I~
            Do While ii < charctr                                      ''~7522I~
                chiiprev = chii                                          ''~7607I~
                chii = chars(ii)                                        ''~7607I~
                '               iinext = chkEOLCont(chii, chars, charctr, ii, chcode)  'chk continued word over EOL''~v001R~''~v008R~
                '               If iinext > 0 Then  'jump over EOL                            ''~v001I~''~v008R~
                '                   ii = iinext                                          ''~v001I~''~v008R~
                '                   chii = chars(ii)                                   ''~v001I~''~v008R~
                '               End If                                                 ''~v001I~''~v008R~
                If (ii + 1 < charctr) Then                             ''~7522I~
                    ch = getCharType(chii, chars(ii + 1))         ''~7522I~''~7607R~
                Else                                                   ''~7522I~
                    ch = getCharType(chii, Chr(&H0))              ''~7522I~''~7607R~
                End If                                                 ''~7522I~
                swprevhiragana = chcode = 2                            ''~7608R~
                swprevkatakana = chcode = 4                            ''~7608I~
                swprevkana = swprevhiragana OrElse swprevkatakana   'hiragana or katakana''~7608I~
                '               prevchcode=chcode                                      ''~v004I~''~v008R~
                chcode = AscW(ch)                                      ''~7522I~
                '               chii= chkHiraKataChar(chii,chars,charctr,ii,prevchcode,chcode)  'chk hiragana He is katakana He?''~v004R~''~v008R~
                '           If (debug) Then                            ''~7522I~
                '               stdout(String.Format("ch={0} chtype={1}", chii, chcode), 1)''~7522I~''~7607R~
                '           End If                                     ''~7522I~
                '               Trace.swTrace = True                                                           ''~7911I~''~v029R~
                '               Trace.W("conv2kana chii=" & chii & ",chcode=" & chcode & ",prevhira=" & swprevhiragana & ",prevkata=" & swprevkatakana)          '@@@@test''~7619I~''~7911R~''~v029R~
                Select Case chcode                                     ''~7522I~
                    Case 1 'kanji                                      ''~7522I~
                        If swprevkana Then 'after hiragana''~7525R~    ''~7607R~
                            If swkanji Then 'after kanji+kana          ''~7607I~
                                '                               kanastr = strConv(sb, Pfiletext.Substring(poskanji, cvctr)) ''~7522I~''~7525R~''~7605R~''~7607R~''~7608R~''~v001R~
                                kanastr = strConv(sb, sbConv.ToString()) ''~v001I~
                                sb.Append(kanastr)                         ''~7522I~''~7525R~''~7607R~
                                swkanji = False                            ''~7522I~''~7525R~''~7607R~
                            End If                                     ''~7608I~
                            If DocOptions.swBES99 Then             ''~7608I~''~v030R~
                                appendSpace(sb, 1)                      ''~7607I~''~7608R~
                            End If                                         ''~7522I~''~7525R~''~7607R~''~7608R~
                        End If                                         ''~7607I~
                        If (swkanji) Then                              ''~7522I~
                            '                            cvctr += 1                                 ''~7522I~''~v001R~
                            sbConv.Append(chii)                        ''~v001I~
                        Else                                           ''~7522I~
                            '                           poskanji = ii                              ''~7522I~''~v001R~
                            '                           cvctr = 1                                  ''~7522I~''~v001R~
                            sbConv.Clear()                             ''~v001I~
                            sbConv.Append(chii)                        ''~v001I~
                            swkanji = True                             ''~7522I~
                        End If                                         ''~7522I~
                    Case 3 'surrogate(ucs4) kanji                      ''~7522I~
                        '*** SJIS code point dose not correspond to any surrogate-pair char ***''~7608I~
                        '*** Shift_JIS-2004 code point has corresponding surrogate-pair char,but it is not Windows SJIS ***''~7608I~
                        If swprevkana Then 'after hiragana             ''~7607I~
                            If swkanji Then 'after kanji+kana          ''~7607I~
                                '                               kanastr = strConv(sb, Pfiletext.Substring(poskanji, cvctr)) ''~7522I~''~7525R~''~7605R~''~7607R~''~7608R~''~v001R~
                                kanastr = strConv(sb, sbConv.ToString()) ''~v001I~
                                sb.Append(kanastr)                         ''~7522I~''~7525R~''~7607R~
                                swkanji = False                            ''~7522I~''~7525R~''~7607R~
                            End If                                     ''~7608R~
                            If DocOptions.swBES99 Then             ''~7608I~''~v030R~
                                appendSpace(sb, 1)                      ''~7607I~
                            End If                                     ''~7607I~
                        End If                                         ''~7522I~''~7525R~
                        If (swkanji) Then                              ''~7522I~
                            '                           cvctr += 2                                 ''~7522I~''~v001R~
                            sbConv.Append(chii)                        ''~v001I~
                            sbConv.Append(chars(ii + 1))                 ''~v001I~
                        Else                                           ''~7522I~
                            '                           poskanji = ii                              ''~7522I~''~v001R~
                            '                           cvctr = 2                                  ''~7522I~''~v001R~
                            sbConv.Clear()                             ''~v001I~
                            sbConv.Append(chii)                        ''~v001I~
                            sbConv.Append(chars(ii + 1))                 ''~v001I~
                            swkanji = True                             ''~7522I~
                        End If                                         ''~7522I~
                        ii += 1                                        ''~7522I~
                    Case 2  'hiraganaOkurigana                         ''~7522I~
                        chii = chkRepeatKana(chii, chiiprev)              ''~7607I~
                        If swprevkatakana AndAlso DocOptions.swBES99 Then ''~7608R~''~v030R~
                            If (swkanji) Then                          ''~7608I~
                                '                               kanastr = strConv(sb, Pfiletext.Substring(poskanji, cvctr)) ''~7608I~''~v001R~
                                kanastr = strConv(sb, sbConv.ToString()) ''~v001I~
                                sb.Append(kanastr)                     ''~7608I~
                                swkanji = False                        ''~7608I~
                            End If                                     ''~7608I~
                            appendSpace(sb, 1)                          ''~7608I~
                            sb.Append(chii)                            ''~7608I~
                        Else                                           ''~7608I~
                            If (swkanji) Then                              ''~7522I~''~7608R~
                                '                               cvctr += 1                                 ''~7522I~''~7608R~''~v001R~
                                sbConv.Append(chii)                    ''~v001I~
                            Else                                           ''~7522I~''~7608R~
                                sb.Append(chii)                       ''~7522I~''~7607R~''~7608R~
                            End If                                         ''~7522I~''~7608R~
                        End If                                         ''~7608I~
                    Case 4  'katakana                                  ''~7525I~
                        chii = chkRepeatKana(chii, chiiprev)              ''~7607I~
                        If swprevhiragana AndAlso DocOptions.swBES99 Then ''~7608R~''~v030R~
                            If (swkanji) Then                          ''~7608I~
                                '                               kanastr = strConv(sb, Pfiletext.Substring(poskanji, cvctr)) ''~7608I~''~v001R~
                                kanastr = strConv(sb, sbConv.ToString()) ''~v001I~
                                sb.Append(kanastr)                     ''~7608I~
                                swkanji = False                        ''~7608I~
                            End If                                     ''~7608I~
                            appendSpace(sb, 1)                          ''~7608I~
                            sb.Append(chii)                            ''~7608I~
                        ElseIf Not swKatakanaDoc Then  'katakana is not okurigana''~v006I~''~v030R~
                            If (swkanji) Then                          ''~v006I~
                                '                               kanastr = strConv(sb, Pfiletext.Substring(poskanji, cvctr))''~v006I~
                                kanastr = strConv(sb, sbConv.ToString()) ''~v006I~
                                sb.Append(kanastr)                     ''~v006I~
                                swkanji = False                        ''~v006I~
                                If DocOptions.swBES99 Then         ''~v006I~''~v030R~
                                    appendSpace(sb, 1)                 ''~v006R~
                                End If                                 ''~v006I~
                            End If                                     ''~v006I~
                            sb.Append(chii)                            ''~v006I~
                        Else                                           ''~7608I~
                            If (swkanji) Then                              ''~7525I~''~7608R~
                                '                               cvctr += 1                                 ''~7525I~''~7608R~''~v001R~
                                sbConv.Append(chii)                    ''~v001I~
                            Else                                           ''~7525I~''~7608R~
                                sb.Append(chii)                       ''~7525I~''~7607R~''~7608R~
                            End If                                         ''~7525I~''~7608R~
                        End If                                         ''~7608I~
                    Case Else                                          ''~7522I~
                        If (swkanji) Then                              ''~7522I~
                            '                           kanastr = strConv(sb, Pfiletext.Substring(poskanji, cvctr)) ''~7522I~''~7605R~''~7607R~''~7608R~''~v001R~
                            kanastr = strConv(sb, sbConv.ToString())   ''~v001I~
                            sb.Append(kanastr)                         ''~7522I~
                        End If                                         ''~7522I~
                        If (chcode = 0) Then                           ''~7522I~
                            If chkBES99Style(chars, ii, sb, nextpos) Then ''~7604R~
                                ii = nextpos                             ''~7604I~
                            Else                                       ''~7604I~
                                Dim asc As Integer = AscW(chii)       ''~7522I~''~7604R~''~7607R~
                                If (asc >= 32) Then                        ''~7522I~''~7604R~
                                    sb.Append(chii)                   ''~7522I~''~7604R~''~7607R~
                                ElseIf asc = 13 OrElse asc = 10 OrElse asc = 9 Then '0d0a,tab''~7522I~''~7604R~
                                    sb.Append(chii)                   ''~7522I~''~7604R~''~7607R~
                                End If                                     ''~7411R~)''~7522I~''~7604R~
                            End If                                     ''~7411R~)''~7604I~
                        Else                                           ''~7522I~
                            sb.Append(ch)                              ''~7522I~
                        End If                                         ''~7522I~
                        ''~7522I~
                        swkanji = False                                ''~7522I~
                        '                       swkana = False                                 ''~7522I~''~7525R~
                        '                       poskanji = 0                                   ''~7522I~''~v001R~
                        '                       cvctr = 0                                      ''~7522I~''~v001R~
                        sbConv.Clear()                                 ''~v001I~
                End Select                                             ''~7522I~
                ii += 1                                                ''~7522I~
            Loop                                                       ''~7522I~
        Catch ex As Exception                                          ''~7522I~
            '           MessageBox.Show(String.Format("かな/カナ変換に失敗 Source={0},stack={1}", ex.Source, ex.StackTrace.ToString())) ''~7522I~''~7618R~
            '           MessageBox.Show(String.Format("{2}," & vbCrlf & "Source={0}," & vbCrlf & "{2}\n,stack={1}", ex.Source, ex.StackTrace.ToString(),ex.Message), Rstr.MSG_ERR_FAILED_KANJI2KANA_CONV) ''~7618I~''~v010R~
            exceptionMsg(ex, Rstr.MSG_ERR_FAILED_KANJI2KANA_CONV)       ''~v010I~
            swException = True                                           ''~v010I~
            Return ""                                                  ''~7522I~
        End Try                                                        ''~7522I~
        Return sb.ToString()                                       ''~7522I~''~7618I~
    End Function                                                       ''~7522I~
    '****************************************************************************''~v010I~
    Function conv2KanaM2(Pfiletext As String) As String                ''~v010I~
        '** split by delm char(preiod,comma,space), ignore CRLF if not following delm''~v010I~
        Dim sb = New System.Text.StringBuilder()                       ''~v010I~
        Dim sbConv = New System.Text.StringBuilder()                   ''~v010I~
        Dim chars() As Char = Pfiletext.ToCharArray()                  ''~v010I~
        Dim charctr As Integer = chars.Length()                        ''~v010I~
        Dim ii As Integer = 0, wordctr As Integer = 0                  ''~v010I~
        Dim chcode, nextpos As Integer                                          ''~v010I~
        Dim ch, chii, chiiprev As Char                                 ''~v010I~
        Dim swkanji As Boolean = False                                 ''~v010I~
        Dim swprevkana As Boolean                                      ''~v010I~
        Dim swprevkanji As Boolean                                     ''~v010I~
        Dim swprevhiragana, swprevkatakana As Boolean                  ''~v010I~
        Dim swKatakanaDoc As Boolean = DocOptions.swKatakanaDoc    'allow katakana as okurigana''~v010I~''~v030R~
        Dim swKanjiSpace As Boolean = swMethod2KanjiSpace AndAlso DocOptions.swBES99 ''~v019R~''~v030R~
        Dim poskanji As Integer = -1, ctrkanji As Integer = 0, prevchcode As Integer ''~v019R~
        Dim katakanactr As Integer = 0                                 ''~v021I~
        swException = False                                              ''~v010I~
        Trace.swTrace = True                                           ''~v101I~
        Try                                                            ''~v010I~
            chii = Chr(&H0)                                            ''~v010I~
            Do While ii < charctr                                      ''~v010I~
                chiiprev = chii                                        ''~v010I~
                chii = chars(ii)                                       ''~v010I~
                Dim repeatch As Char = chkRepeatKana(chii, chiiprev)     ''~v010I~
                If (repeatch <> chii) Then 'replaced repeat char                 ''~v010I~
                    chii = repeatch    'ch is same as prev                   ''~v010I~
                Else                                                      ''~v010I~
                    If isDelmChar(chii) Then                                      ''~v010I~
                        ch = CHARTYPE_DELM                                     ''~v010I~
                    Else                                                     ''~v010I~
                        If (ii + 1 < charctr) Then                             ''~v010I~
                            ch = getCharType(chii, chars(ii + 1))              ''~v010I~
                        Else                                                   ''~v010I~
                            ch = getCharType(chii, Chr(&H0))                   ''~v010I~
                        End If                                                 ''~v010I~
                    End If                                                   ''~v010I~
                End If                                                    ''~v010I~
                swprevhiragana = chcode = 2                            ''~v010I~
                swprevkatakana = chcode = 4                            ''~v010I~
                swprevkana = swprevhiragana OrElse swprevkatakana   'hiragana or katakana''~v010I~
                swprevkanji = (chcode = 1) OrElse (chcode = 3)              ''~v010I~
                '               prevchcode=chcode                      ''~v010I~
                prevchcode = chcode                                    ''~v019R~
                chcode = AscW(ch)                                      ''~v010I~
                If DocOptions.swBES99 Then                         ''~v013R~''~v030R~
                    If chii = KATAKANA_CHOON Then                             ''~v013R~
                        If swprevhiragana Then                              ''~v013R~
                            chcode = 2                                   ''~v013R~
                        End If                                         ''~v013R~
                    End If                                             ''~v013R~
                End If                                                 ''~v013R~
                '               chii= chkHiraKataChar(chii,chars,charctr,ii,prevchcode,chcode)  'chk hiragana He is katakana He?''~v010I~
                '           If (debug) Then                            ''~v010I~
                '               stdout(String.Format("ch={0} chtype={1}", chii, chcode), 1)''~v010I~
                '           End If                                     ''~v010I~
                '               Trace.swTrace = True                                   ''~v010I~''~v029R~
                '               Trace.W("conv2kanaM2 chii=" & chii & ",chcode=" & chcode & ",prevhira=" & swprevhiragana & ",prevkata=" & swprevkatakana)          '@@@@test''~v010R~''~v029R~
                If swKanjiSpace Then                                   ''~v019R~
                    sbConv = insertKanjiSpace(sbConv, chcode, prevchcode, swprevkanji, poskanji, ctrkanji) ''~v019R~
                End If                                                 ''~v019R~
                Trace.W("Conv2kanaM2 chcode=" & chcode & ",chii=" & chii) ''~v101I~
                Select Case chcode                                     ''~v010I~
                    Case 1, 3 'kanji                                    ''~v010R~
                        '                       If swprevkatakana AndAlso Not swKatakanaDoc Then 'katakana+kanji''~v010I~''~v021R~''~v030R~
                        '                           strConvM2(sb, sbConv, swkanji)   'nodata when prevkatakana         ''~v010I~''~v020R~
                        '                           If DocOptions.swBES99 Then             ''~v010I~''~v021R~''~v030R~
                        '                               appendSpace(sb, 1)                     ''~v010I~''~v021R~
                        '                           End If                                     ''~v010I~''~v021R~
                        '                       End If                                         ''~v010I~''~v021R~
                        swkanji = True                                 ''~v010I~
                        sbConv.Append(chii)                            ''~v010I~
                        '                       Trace.W("conv2kanaM2 kanji chcode=" & chcode & ",appendto SbConv chii=" & chii) ''~v010R~''~v029R~
                        If chcode = 3 Then                                    ''~v010I~
                            sbConv.Append(chars(ii + 1))               ''~v010I~
                            ii += 1                                    ''~v010I~
                        End If                                         ''~v010I~
                    Case 2  'hiraganaOkurigana                         ''~v010I~
                        If swKatakanaDoc Then                            ''~v031I~
                        Else                                             ''~v031I~
#If False Then                                                              ''+v127I~
                            '                       If swprevkatakana Then                              ''~v010R~''~v021R~
                            If katakanactr > 0 Then                               ''~v021I~
                                '                           strConvM2(sb, sbConv, swkanji)                      ''~v010I~''~v021R~
                                strConvM2(sb, sbConv, swkanji, katakanactr) ''~v021I~
                                swkanji = False                              ''~v010I~
                            End If                                         ''~v021I~
                            '                           If swprevkatakana Then                         ''~v021I~''~v033R~
                            '                               If DocOptions.swBES99 Then             ''~v010I~''~v030R~''~v033R~
                            '                                   appendSpace(sb, 1)                     ''~v010I~''~v033R~
                            '                               End If                                     ''~v010R~''~v033R~
                            '                           End If                                         ''~v010I~''~v033R~
#End If                                                                ''+v127I~
                        End If                                           ''~v031I~
                        '                       Trace.W("conv2kanaM2 hira appendto SbConv chii=" & chii)            ''~v010I~''~v029R~
                        sbConv.Append(chii)                            ''~v010I~
                    Case 4  'katakana                                  ''~v010I~
                        '                       If swKatakanaDoc AndAlso (swprevkanji OrElse swprevkatakana) Then ''~v010R~''~v020R~''~v030R~
                        If swKatakanaDoc Then                         ''~v020I~''~v030R~
                            sbConv.Append(chii)                        ''~v010I~
                            '                           Trace.W("conv2kanaM2 kata appendto SbConv chii=" & chii)            ''~v010I~''~v029R~
                        Else                                           ''~v010R~
                            '                           If Not swprevkatakana AndAlso chkKaKe(chii, chars, charctr, ii) Then  'chk Ka/Ke is to be translatec''~v025I~''~v029R~
                            If Not swprevkatakana AndAlso chkKaKe(chii, chars, charctr, ii, prevchcode) Then  'chk Ka/Ke is to be translatec''~v029I~
                                chcode = prevchcode    'not katakana       ''~v025M~
                                sbConv.Append(chii)                        ''~v025M~
                                '                               Trace.W("conv2kanaM2 ka/ke appendto SbConv chii=" & chii) ''~v025M~''~v029R~
                            Else                                           ''~v025M~
                                If Not swprevkatakana Then                 ''~v010I~
                                    '                               strConvM2(sb, sbConv, swkanji)         ''~v010R~''~v021R~
                                    strConvM2(sb, sbConv, swkanji, katakanactr) ''~v021I~
                                    swkanji = False                        ''~v010R~
                                    '                                   If DocOptions.swBES99 Then         ''~v010R~''~v030R~''~v033R~
                                    '                                   appendSpace(sb, 1)                 ''~v010R~''~v024R~
                                    '                                       appendSpaceWhenNoSpace(sb)  ' append one space if prev is not space''~v024I~''~v033R~
                                    '                                   End If                                 ''~v010R~''~v033R~
                                    katakanactr = 0                          ''~v021I~
                                End If                                     ''~v010I~
                                '                           sb.Append(chii)                            ''~v010I~''~v021R~
                                sbConv.Append(chii)                        ''~v021I~
                                katakanactr += 1                             ''~v021I~
                                '                               Trace.W("conv2kanaM2 kata appendto Sb chii=" & chii)                ''~v010I~''~v029R~
                            End If                                         ''~v025M~
                        End If                                         ''~v010I~
                        '                   Case CHARTYPE_DELM_ASCW  'delm char                ''~v010R~
                    Case Else                                          ''~v010M~
                        '                       strConvM2(sb, sbConv, swkanji)                          ''~v010I~''~v021R~
                        Dim swSplit As Boolean = isSplitter(chii) 'string terminator''~v023I~
                        '                       If swSplit OrElse katakanactr Then                  ''~v023R~''~v101R~
                        Trace.W("swSplit=" & swSplit & ",katakanactr=" & katakanactr) ''~v101I~
                        If swSplit OrElse katakanactr <> 0 Then          ''~v101I~
                            strConvM2(sb, sbConv, swkanji, katakanactr)     ''~v021I~
                            swkanji = False                                  ''~v010I~
                        End If                                           ''~v023I~
                        '                       Trace.W("conv2kanaM2 kata appendto Sb chii=" & chii)                ''~v010I~''~v029R~

                        If chkBES99Style(chars, ii, sb, nextpos) Then  ''~v010I~
                            Trace.W("chkBES99Style ii=" & ii & ",nextpos=" & nextpos) ''~v101I~
                            ii = nextpos    'skip exausted space for required by kuten etc''~v010I~
                        Else                                           ''~v010I~
                            If swSplit Then                                   ''~v023I~
                                sb.Append(chii)                            ''~v010R~
                                ''~v101I~
                                Trace.W("conv2kanaM2 swsplit chcode=" & chcode & ",sb append=" & chii) ''~v101R~
                            Else                                         ''~v023I~
                                sbConv.Append(chii)  'translate with following string''~v023I~
                                Trace.W("conv2kanaM2 sbconv.Append chii=" & chii) ''~v101I~
                            End If                                       ''~v023I~
                        End If                                         ''~v010I~
                End Select                                             ''~v010I~
                ii += 1                                                ''~v010I~
            Loop                                                       ''~v010I~
            '           Trace.W("conv2kanaM2 last strConvMe")                               ''~v010I~''~v029R~
            '           strConvM2(sb, sbConv, swkanji)  'last remaining            ''~v010I~''~v021R~
            strConvM2(sb, sbConv, swkanji, katakanactr)  'last remaining''~v021I~
        Catch ex As Exception                                          ''~v010I~
            '           MessageBox.Show(String.Format("かな/カナ変換に失敗 Source={0},stack={1}", ex.Source, ex.StackTrace.ToString()))''~v010I~
            '           MessageBox.Show(String.Format("{2}," & vbCrlf & "Source={0}," & vbCrlf & "stack={1}", ex.Source, ex.StackTrace.ToString(),ex.Message), Rstr.MSG_ERR_FAILED_KANJI2KANA_CONV)''~v010R~
            exceptionMsg(ex, Rstr.MSG_ERR_FAILED_KANJI2KANA_CONV)       ''~v010I~
            swException = True                                           ''~v010I~
            Return ""                                                  ''~v010I~
        End Try                                                        ''~v010I~
        If DocOptions.swBES99 Then                                 ''~v016I~''~v030R~
            sb = applyBES99_U2Choon(sb)                                  ''~v016R~
        End If                                                         ''~v016I~
        Trace.swTrace = False                                          ''~v101I~
        Return sb.ToString()                                           ''~v010I~
    End Function 'conv2KanaM2                                          ''~v010R~
    '********************************************************          ''~v008I~
    Function convEnglish(Pfiletext As String) As String                ''~7618I~
#If False Then                                                              ''~7619I~
        Dim chars() As Char = Pfiletext.ToCharArray()                  ''~7618I~
        Dim charctr As Integer = chars.Length()                        ''~7618I~
        Dim sb = New System.Text.StringBuilder(charctr * 2)              ''~7618I~
        Dim chii, chnext As Char                                 ''~7618I~
        Dim ii As Integer = 0                                          ''~7618I~
        Do While ii < charctr                                      ''~7618I~
            chii = chars(ii)                                       ''~7618I~
            If (ii + 1 < charctr) Then                             ''~7618I~
                chnext = chars(ii + 1)                             ''~7618I~
            Else                                                   ''~7618I~
                chnext = Chr(&H0)                                    ''~7618I~
            End If                                                 ''~7618I~
            sb.Append(chii)                                          ''~7618I~
            Select Case chii                                       ''~7618I~
                Case "."c, ","c                                            ''~7618R~
                    If chnext <> " "c Then                                      ''~7618R~
                        appendSpaceSBCS(sb, 1)                             ''~7618R~
                    End If                                                 ''~7618R~
            End Select                                             ''~7618I~
            ii += 1                                                ''~7618I~
        Loop                                                       ''~7618I~
        Return sb.ToString()                                           ''~7618I~
#Else                                                                  ''~7619I~
        Return Pfiletext                                               ''~7619I~
#End If                                                                ''~7619I~
    End Function                                                       ''~7618I~
    ''~7522I~
    Function getCharType(Pchar As Char, Pchar2 As Char) As Char        ''~7522I~
        'rc 2:hiraganaOkurigana,1:kanji-ucs2,3:kanji-ucs4,4:katakana,0:no conversion,else converted to char''~7522I~''~7525R~
        Dim chtype As Integer = 0                                      ''~7522I~
        ''~7522I~
        If (isHiraganaOkurigana(Pchar)) Then 'hiragana                 ''~7522I~
            Return Chr(&H2) ' hiragana                                 ''~7522I~
        End If                                                         ''~7522I~
        If (isKatakana(Pchar)) Then 'hiragana                          ''~7525I~
            Return Chr(&H4) ' hiragana                                 ''~7525I~
        End If                                                         ''~7525I~
        If (Char.IsHighSurrogate(Pchar)) Then 'surrogate high          ''~7522I~
            If (isKanji4(Pchar, Pchar2)) Then                          ''~7522I~
                Return Chr(&H3) 'surrogate kanji                       ''~7522I~
            End If                                                     ''~7522I~
        End If                                                         ''~7522I~
        If (isKanji2(Pchar)) Then 'kanji                               ''~7522I~
            Return Chr(&H1) 'kanji                                     ''~7522I~
        End If                                                         ''~7522I~
        Return Chr(&H0)                                                ''~7522I~
    End Function                                                       ''~7522I~
    '********************************************************          ''~v019R~
    Private Function insertKanjiSpace(PsbConv As StringBuilder, Pchcode As Integer, Pprevchcode As Integer, Pswprevkanji As Boolean, ByRef Ppposkanji As Integer, ByRef Ppctrkanji As Integer) As StringBuilder ''~v019R~
        Dim sb As StringBuilder = PsbConv                              ''~v019R~
        Dim pos As Integer = Ppposkanji                                ''~v019R~
        Dim ctr As Integer = Ppctrkanji                                ''~v019R~
        If Pchcode = 1 OrElse Pchcode = 3 Then   'kanji                ''~v019R~
            If Pswprevkanji Then                                       ''~v019R~
                If pos >= 0 Then   'not after delm                     ''~v019R~
                    ctr += 1                                           ''~v019R~
                    If ctr >= KANJI_SPACE_THRESHOLD Then               ''~v019R~
                        sb = sb.Insert(pos, FormatBES.CHAR_SPACE)      ''~v019R~
                        pos = -1 'until next top kanji                 ''~v019R~
                    End If                                             ''~v019R~
                Else                                                   ''~v019R~
                End If                                                 ''~v019R~
            Else                                                       ''~v019R~
                '               If Pprevchcode = CHARTYPE_DELM_ASCW OrElse Pprevchcode = 4 Then  ' no need insert space after delm or katakana''~v019R~
                If Pprevchcode <> 2 Then  ' no need insert space after delm or katakana''~v019R~
                    If Pprevchcode = 4 Then  ' katakana                        ''~v021I~
                        pos = sb.Length                                    ''~v021I~
                        ctr = 1                                            ''~v021I~
                    Else                                                 ''~v021I~
                        pos = -1                                           ''~v019R~
                        ctr = 0                                            ''~v019R~
                    End If                                               ''~v021I~
                Else                                                   ''~v019R~
                    pos = sb.Length                                    ''~v019R~
                    ctr = 1                                            ''~v019R~
                End If                                                 ''~v019R~
            End If                                                     ''~v019R~
        Else                                                           ''~v019R~
            pos = -1                                                   ''~v019R~
            ctr = 0                                                    ''~v019R~
        End If                                                         ''~v019R~
        Ppposkanji = pos                                               ''~v019R~
        Ppctrkanji = ctr                                               ''~v019R~
        Return sb                                                      ''~v019R~
    End Function                                                       ''~v019R~
    '********************************************************          ''~v010I~
    '   Private Function strConvM2(Psb As StringBuilder, PsbConv As StringBuilder, PswKanji As Boolean) As Boolean ''~v010I~''~v014R~''~v021R~
    Private Function strConvM2(Psb As StringBuilder, PsbConv As StringBuilder, PswKanji As Boolean, ByRef Ppkatakanactr As Integer) As Boolean ''~v021R~
        '**conv saved str and copy to out sb                               ''~v010I~
        Dim kanastr As String                                          ''~v010M~
        Dim rc As Boolean = False                                        ''~v010I~
        If PsbConv.Length > 0 Then                                            ''~v010I~
            Trace.W("strConvM2 PsbConv input=" & PsbConv.ToString() & vbCrLf & "<<<<") ''~v101R~
            If PswKanji Then 'detected kanji                           ''~v010I~
                Dim kanjistr As String = PsbConv.ToString()              ''~v010I~
                kanastr = strConv(Psb, kanjistr)                         ''~v010I~
                Trace.W("strConvM2 kanji=" & kanjistr & ",kanastr=" & kanastr)          '@@@@test''~v010I~''~v029R~''~v101R~
                '               Debug.WriteLine("strConvM2 kanji=" & kanjistr & ",kanastr=" & kanastr)          '@@@@test''~v033I~''~v101R~
                '               If DocOptions.swBES99 Then                         ''~v014I~''~v016R~''~v030R~
                '                   kanastr = applyBES99_U2Choon(kanastr)                ''~v014I~''~v016R~
                '               End If                                                 ''~v014I~''~v016R~
                If Ppkatakanactr > 0 Then                              ''~v021R~
                    Psb.Append(PsbConv.ToString(0, Ppkatakanactr))     ''~v021R~
                    If DocOptions.swBES99 Then                     ''~v021I~''~v030R~
                        Dim kanjictr As Integer = PsbConv.Length - Ppkatakanactr ''~v021R~
                        If kanjictr > 1 AndAlso kanjictr < KANJI_SPACE_THRESHOLD Then ''~v021I~
                            appendSpaceSBCS(Psb, 1)                    ''~v021I~
                        End If                                         ''~v021I~
                    End If                                             ''~v021I~
                    Psb.Append(kanastr.Substring(Ppkatakanactr, kanastr.Length - Ppkatakanactr)) ''~v021R~
                    Trace.W("strConvM2  kanji katakana append to Psb=" & Psb.ToString()) ''~v101R~
                Else                                                     ''~v021I~
                    Psb.Append(kanastr)                                    ''~v010I~
                    Trace.W("strConvM2  kanji no katakana append to Psb=" & Psb.ToString()) ''~v101R~
                End If                                                    ''~v021I~
                rc = True                                                ''~v010I~
            Else                                                       ''~v010I~
                Psb.Append(PsbConv)                                    ''~v010I~
                Trace.W("strConvM2 No kanji append to Psb=" & Psb.ToString()) ''~v101R~
            End If                                                     ''~v010I~
            PsbConv.Clear()                                            ''~v010I~
            Ppkatakanactr = 0                                             ''~v021I~
        End If                                                         ''~v010I~
        Return rc   'kanji conversion done                             ''~v010I~
    End Function                                                       ''~v010I~
    '********************************************************          ''~v014I~
    Private Function applyBES99_U2Choon(Pstr As String) As String      ''~v014I~
        Dim outstr As String = Pstr                                      ''~v014I~
        If outstr.Contains(HIRAGANA_YU_U) Then                              ''~v014I~
            outstr = outstr.Replace(HIRAGANA_YU_U, HIRAGANA_YU_CHOON)  ''~v014R~
        End If                                                          ''~v014I~
        If outstr.Contains(HIRAGANA_YO_U) Then                              ''~v014I~
            outstr = outstr.Replace(HIRAGANA_YO_U, HIRAGANA_YO_CHOON)  ''~v014R~
        End If                                                          ''~v014I~
        Return outstr                                                  ''~v014I~
    End Function                                                       ''~v014I~
    '********************************************************          ''~v016I~
    Private Function applyBES99_U2Choon(Psb As StringBuilder) As StringBuilder ''~v016I~
        Dim sb As StringBuilder = Psb                                  ''~v016I~
        sb = sb.Replace(HIRAGANA_YU_U, HIRAGANA_YU_CHOON)                ''~v016I~
        sb = sb.Replace(HIRAGANA_YO_U, HIRAGANA_YO_CHOON)                ''~v016I~
        Return sb                                                      ''~v016I~
    End Function                                                       ''~v016I~
    '********************************************************          ''~v010I~
    Function strConv(Psb As System.Text.StringBuilder, Pstr As String) As String ''~7605R~''~7607R~''~7608R~
        Dim kanastr, kanjistr As String                                          ''~7522I~''~7607R~
        kanjistr = chkRepeatKanji(Pstr)                                  ''~7607I~
        kanastr = JPReverseConv.Main(2, fel, kanjistr) 'conv               ''~7522I~''~7607R~
        '       Trace.W("strConv Pstr=" & Pstr & ",kanastr=" & kanastr)          '@@@@test''~7911I~''~v029R~
#If False Then                                                         ''~7608I~
        If DocOptions.swBES99 Then                                ''~7525I~''~7604R~''~v030R~
#If False Then                                                              ''~7607I~
            If Psb.length = 0 Then                                            ''~7605I~
                Return kanastr                                         ''~7605I~
            End If                                                     ''~7605I~
            Dim chars(2) As Char                                       ''~7605I~
            Psb.copyTo(Psb.Length - 1, chars, 0, 1)                         ''~7605I~
            If chars(0) = FormatBES.CHAR_SPACE Then                            ''~7605I~
                Return kanastr                                         ''~7605I~
            End If                                                     ''~7605I~
            Return FormatBES.CHAR_SPACE & kanastr                      ''~7525I~
#Else                                                                  ''~7607I~
            If (Pswnextkanji) Then                                          ''~7607I~
                kanastr = kanastr & FormatBES.CHAR_SPACE                 ''~7607R~
            End If                                                     ''~7607I~
#End If                                                                ''~7607I~
        End If                                                         ''~7525I~
#End If                                                                ''~7608I~
        Return kanastr                                                 ''~7522I~
    End Function                                                       ''~7522I~
    Function chkRepeatKanji(Pstr As String) As String                  ''~7607I~
#If False Then                                                              ''~7608I~
        If Not DocOptions.swBES99 Then                             ''~7608I~''~v030R~
        	return Pstr                                                ''~7608I~
        end if                                                         ''~7608I~
        Dim pos1 As Integer                                            ''~7607I~
        Dim chRepeat As Char                                           ''~7607I~
        Dim strRepeat, strBefore, strAfter As String                     ''~7607I~
        pos1 = Pstr.IndexOf(repeatChars(FormatBES.STR_REPEAT_INDEX_KANJI))                              ''~7607I~''~7608R~
        If pos1 <= 0 Then     'will not starting char                         ''~7607I~
            Return Pstr                                                ''~7607I~
        End If                                                         ''~7607I~
        chRepeat = Pstr.Chars(pos1 - 1)                                    ''~7607I~
        strBefore = Pstr.substring(0, pos1)                            ''~7607R~
        strAfter = Pstr.substring(pos1 + 1)                                ''~7607I~
        strRepeat = strBefore & chRepeat & strAfter
        Return strRepeat ''~7607I~
#Else                                                                  ''~7608I~
        Return Pstr 'MicroSoft supports kanji odoriji                  ''~7608I~
#End If                                                                ''~7608I~
    End Function                                                       ''~7607I~
    Function chkRepeatKana(Pch As Char, PchPrev As Char) As Char        ''~7607R~
        If Not DocOptions.swBES99 Then                             ''~7608I~''~v030R~
            Return Pch                                                 ''~7608I~
        End If                                                         ''~7608I~
        Dim pos2, pos3 As Integer                                       ''~7607R~
        Dim cvch As Char                                               ''~7607I~
        cvch = Pch                                                       ''~7607I~
        pos2 = FormatBES.STR_REPEAT.IndexOf(Pch)                       ''~7607R~
        Select Case pos2                                               ''~7607I~
            Case FormatBES.STR_REPEAT_INDEX_HIRAGANA            'U309d hira                                ''~7607I~''~7608R~
                cvch = PchPrev                                           ''~7607R~
            Case FormatBES.STR_REPEAT_INDEX_HIRAGANA_DAKUON              'U309d hira dakuon                         ''~7607I~''~7608R~
                pos3 = FormatBES.STR_DAKUON_SRC.IndexOf(PchPrev)       ''~7607R~
                If pos3 >= 0 Then                                                 ''~7607I~
                    cvch = FormatBES.STR_DAKUON_TGT.Chars(pos3)          ''~7607R~
                End If                                                     ''~7607I~
            Case FormatBES.STR_REPEAT_INDEX_KATAKANA              'U309d kata                                ''~7607I~''~7608R~
                cvch = PchPrev                                           ''~7607I~
            Case FormatBES.STR_REPEAT_INDEX_KATAKANA_DAKUON             'U309d kata dakuon                         ''~7607I~''~7608R~
                pos3 = FormatBES.STR_DAKUON_SRC_KATAKANA.IndexOf(PchPrev) ''~7607R~
                If pos3 >= 0 Then                                                 ''~7607I~
                    cvch = FormatBES.STR_DAKUON_TGT_KATAKANA.Chars(pos3) ''~7607R~
                End If                                                     ''~7607I~
        End Select                                                     ''~7607I~
        Return cvch                                                    ''~7607R~
    End Function                                                       ''~7607I~
    ''~7522I~
    Function isHiraganaOkurigana(ByVal c As Char) As Boolean           ''~7522I~
        If c = repeatChars(FormatBES.STR_REPEAT_INDEX_HIRAGANA) OrElse c = repeatChars(FormatBES.STR_REPEAT_INDEX_HIRAGANA_DAKUON) Then                    ''~7607I~''~7608R~
            Return True                                                ''~7607I~
        End If                                                         ''~7607I~
        '「ぁ」(u-3041)～「ゖ」(u-3096)まで、conv with kanji                           ''~7522I~''~7525R~
        Return (ChrW(&H3041) <= c AndAlso c <= ChrW(&H3096))           ''~7522I~
    End Function                                                       ''~7522I~
    Function isKatakana(ByVal Pch As Char) As Boolean                  ''~7525R~
        If Pch = repeatChars(FormatBES.STR_REPEAT_INDEX_KATAKANA) OrElse Pch = repeatChars(FormatBES.STR_REPEAT_INDEX_KATAKANA_DAKUON) Then                    ''~7607I~''~7608R~
            Return True                                                ''~7607I~
        End If                                                         ''~7607I~
        '「ァ」(u030a1)～「ヶ」(u-30f6)まで、conv with kanji           ''~7525R~
        If ChrW(&H30A1) <= Pch AndAlso Pch <= ChrW(&H30F6) Then          ''~7525R~Pch
            Return True
        End If ''~7525I~
        If Pch = KATAKANA_CHOON Then   'u-30fc "ー"                           ''~v005I~
            Return True                                                ''~v005I~
        End If                                                         ''~v005I~
        '「ｦ」(uff66) ～「ﾟ」(u-ff9f)まで、conv with kanji             ''~7525I~''~7604R~
        If (ChrW(&HFF66) <= Pch AndAlso Pch <= ChrW(&HFF9F)) Then           ''~7525I~
            Return True
        End If ''~7525I~
        Return False                                                   ''~7525I~
    End Function                                                       ''~7525I~
    Function isKanji2(ByVal c As Char) As Boolean                      ''~7522I~
        'CJK統合漢字、CJK互換漢字、CJK統合漢字拡張Aの範囲にあるか調べる''~7522I~
        Return (ChrW(&H4E00) <= c AndAlso c <= ChrW(&H9FCF)) OrElse
            (ChrW(&HF900) <= c AndAlso c <= ChrW(&HFAFF)) OrElse
            (ChrW(&H3300) <= c AndAlso c <= ChrW(&H33FF)) OrElse
            (ChrW(&H3400) <= c AndAlso c <= ChrW(&H4DBF)) OrElse
            (ChrW(&H3005) = c)          'odoriji
    End Function                                                       ''~7522I~
    Function isKanji4(ByVal c1 As Char, ByVal c2 As Char) As Boolean   ''~7522I~
        'CJK統合漢字拡張Bの範囲にあるか調べるH20000-H2A6DF             ''~7522I~
        Return ((ChrW(&HD840) <= c1 AndAlso c1 <= ChrW(&HD868)) AndAlso
                Char.IsLowSurrogate(c2)) OrElse
            (c1 = ChrW(&HD869) AndAlso (ChrW(&HDC00) <= c2 AndAlso c2 <= ChrW(&HDEDF))) ''~7522I~
    End Function                                                       ''~7522I~
    Private Function chkBES99Style(Pcharray As Char(), Ppos As Integer, Psb As System.Text.StringBuilder, ByRef Ppnextpos As Integer) As Boolean ''~7604I~
        Dim ch As Char = Pcharray(Ppos)                                  ''~7604I~
        Dim ctr As Integer                                             ''~7604I~
        If Not DocOptions.swBES99 Then                             ''~7604I~''~v030R~
            Return False                                               ''~7604I~
        End If                                                         ''~7604I~
        If ch = FormatBES.CHAR_KUTEN OrElse ch = FormatBES.CHAR_KUTEN_HANKANA Then ''~7604I~
            Psb.Append(FormatBES.CHAR_KUTEN)                           ''~7605I~
            ctr = getSpaceCtrForKuten(Pcharray, Ppos, 2)                   ''~7604I~
            Ppnextpos = Ppos + ctr                                       ''~7604R~
            appendSpace(Psb, 2)                                         ''~7604I~
            Return True                                               ''~7604R~
        End If                                                         ''~7604I~
        If ch = FormatBES.CHAR_TOUTEN OrElse ch = FormatBES.CHAR_TOUTEN_HANKANA Then ''~7604I~
            Psb.Append(FormatBES.CHAR_TOUTEN)                          ''~7605I~
            ctr = getSpaceCtrForKuten(Pcharray, Ppos, 1)                   ''~7604I~
            Ppnextpos = Ppos + ctr                                       ''~7604R~
            appendSpace(Psb, 1)                                         ''~7604I~
            Return True                                                ''~7604R~
        End If                                                         ''~7604I~
        If ch = FormatBES.CHAR_CHUTEN OrElse ch = FormatBES.CHAR_CHUTEN_HANKANA Then ''~7605I~
            If isCont3Chuten(Pcharray, Ppos) Then                            ''~7608I~
                Psb.Append(FormatBES.CHAR_CHUTEN)                      ''~7608I~
                Psb.Append(FormatBES.CHAR_CHUTEN)                      ''~7608I~
                Psb.Append(FormatBES.CHAR_CHUTEN)                      ''~7608I~
                '*              Ppnextpos = Ppos + 3                                   ''~7608I~''~v126R~
                Ppnextpos = Ppos + 2 '* ii+1 after exit(cunsume following 2)''~v126I~
                '#if false                                                              ''~7608I~''~7610R~
            ElseIf isFuseji(Pcharray, Ppos) Then                             ''~7608I~
                Psb.Append(FormatBES.CHAR_CHUTEN)                      ''~7608I~
                '#end if                                                                ''~7608I~''~7610R~
            Else                                                       ''~7608I~
                Psb.Append(FormatBES.CHAR_CHUTEN)                          ''~7605I~''~7608R~
                ctr = getSpaceCtrForKuten(Pcharray, Ppos, 1)               ''~7605I~''~7608R~
                Ppnextpos = Ppos + ctr                                       ''~7605I~''~7608R~
                appendSpace(Psb, 1)                                        ''~7605I~''~7608R~
            End If                                                     ''~7608I~
            Return True                                                ''~7605I~
        End If                                                         ''~7605I~
        If ch = FormatBES.CHAR_EXCLAMATION OrElse ch = FormatBES.CHAR_EXCLAMATION_ASCII Then ''~7605I~
            Psb.Append(FormatBES.CHAR_EXCLAMATION)                     ''~7605I~
            ctr = getSpaceCtrForKuten(Pcharray, Ppos, 1)               ''~7605I~
            Ppnextpos = Ppos + ctr                                     ''~7605I~
            appendSpace(Psb, 1)                                        ''~7605I~
            Return True                                                ''~7605I~
        End If                                                         ''~7605I~
        If ch = FormatBES.CHAR_QUESTION OrElse ch = FormatBES.CHAR_QUESTION_ASCII Then ''~7605I~
            Psb.Append(FormatBES.CHAR_QUESTION)                        ''~7605I~
            ctr = getSpaceCtrForKuten(Pcharray, Ppos, 1)               ''~7605I~
            Ppnextpos = Ppos + ctr                                     ''~7605I~
            appendSpace(Psb, 1)                                        ''~7605I~
            Return True                                                ''~7605I~
        End If                                                         ''~7605I~
        Return False                                                   ''~7604R~
    End Function                                                       ''~7604I~
    Private Function isCont3Chuten(Pcharray As Char(), Ppos As Integer) As Boolean ''~7608I~
        'chk 3 continued chuten	                                           ''~7608I~
        If Ppos + 3 > Pcharray.Length Then                                      ''~7608I~
            Return False                                               ''~7608I~
        End If                                                         ''~7608I~
        Dim ch = Pcharray(Ppos + 1)                                        ''~7608I~
        If ch = FormatBES.CHAR_CHUTEN OrElse ch = FormatBES.CHAR_CHUTEN_HANKANA Then ''~7608I~
            ch = Pcharray(Ppos + 2)                                        ''~7608I~
            If ch = FormatBES.CHAR_CHUTEN OrElse ch = FormatBES.CHAR_CHUTEN_HANKANA Then ''~7608I~
                Return True                                            ''~7608I~
            End If                                                     ''~7608I~
        End If                                                         ''~7608I~
        Return False                                                   ''~7608I~
    End Function                                                       ''~7608I~
    '#if false                                                              ''~7608I~''~7610R~
    Private Function isFuseji(Pcharray As Char(), Ppos As Integer) As Boolean ''~7608I~
        'chk chuten is preceding of fuseji                                 ''~7608I~
        If Ppos + 1 > Pcharray.Length Then                                      ''~7608I~
            Return False                                               ''~7608I~
        End If                                                         ''~7608I~
        Dim ch = Pcharray(Ppos + 1)                                        ''~7608I~
        If FUSEJI_FOLLOWING.IndexOf(ch) >= 0 Then                             ''~7608I~
            Return True                                                ''~7608I~
        End If                                                         ''~7608I~
        Return False                                                   ''~7608I~
    End Function                                                       ''~7608I~
    '#end if                                                                ''~7608I~''~7610R~
    Private Function getSpaceCtrForKuten(Pcharray As Char(), Ppos As Integer, Pmax As Integer) As Integer ''~7604I~
        Dim ctr As Integer = 0                                           ''~7604I~
        Dim ch As Char                                                 ''~7604I~
        For ii As Integer = Ppos + 1 To Pcharray.Length - 1                                       ''~7604I~
            ch = Pcharray(ii)                                            ''~7604I~
            If ch <> FormatBES.CHAR_SPACE AndAlso ch <> FormatBES.CHAR_SPACE_SBCS Then ''~7604I~
                Exit For                                               ''~7604I~
            End If                                                     ''~7604I~
            ctr += 1                                                     ''~7604I~
            If ctr = Pmax Then                                                ''~7604I~
                Exit For                                               ''~7604I~
            End If                                                     ''~7604I~
        Next                                                           ''~7604I~
        Return ctr                                                     ''~7604I~
    End Function                                                       ''~7604I~
    Private Sub appendSpace(Psb As System.Text.StringBuilder, Pctr As Integer) ''~7604I~
        For ii As Integer = 0 To Pctr - 1                                  ''~7604I~
            Psb.Append(FormatBES.CHAR_SPACE)                           ''~7604I~
        Next                                                           ''~7604I~
    End Sub                                                            ''~7604I~
    Private Sub appendSpaceWhenNoSpace(Psb As StringBuilder)              ''~v024I~
        Dim len = Psb.Length                                             ''~v024I~
        '       Trace.W("appendSpaceWhenNoSpace len=" & len)                   ''~v024I~''~v029R~
        If len = 0 Then                                                       ''~v024I~
            Psb.Append(FormatBES.CHAR_SPACE)                           ''~v024I~
        Else                                                           ''~v024I~
            Dim lastch As String = Psb.ToString(len - 1, 1)                           ''~v024I~
            '           Trace.W("appendSpaceWhenNoSpace lastch=" & lastch)          ''~v024I~''~v029R~
            If Not (lastch.IndexOf(FormatBES.CHAR_SPACE) = 0 OrElse lastch.IndexOf(FormatBES.CHAR_SPACE_SBCS) = 0 OrElse lastch.IndexOf(FormatBES.CHAR_LF) = 0) Then ''~v024R~
                Psb.Append(FormatBES.CHAR_SPACE)                       ''~v024I~
            End If                                                     ''~v024I~
        End If                                                            ''~v024I~
    End Sub                                                            ''~v024I~
    Private Sub appendSpaceSBCS(Psb As System.Text.StringBuilder, Pctr As Integer) ''~7618I~
        For ii As Integer = 0 To Pctr - 1                              ''~7618I~
            Psb.Append(FormatBES.CHAR_SPACE_SBCS)                      ''~7618I~
        Next                                                           ''~7618I~
    End Sub                                                            ''~7618I~
    '**********************************************                    ''~7522I~
    <ComImport()>
    <Guid("019F7152-E6DB-11D0-83C3-00C04FDDB82E")>
    <InterfaceType(ComInterfaceType.InterfaceIsIUnknown)>
    Private Interface IFELanguage
        Sub Open()
        Sub Close()
        Sub Dummy5()
        ' DO NOT CALL                                                  
        Sub Dummy6()
        ' DO NOT CALL                                                  
        Function GetPhonetic(<MarshalAs(UnmanagedType.BStr)> str As String, start As Integer, length As Integer) As <MarshalAs(UnmanagedType.BStr)> String
        Sub Dummy8()
        ' DO NOT CALL
    End Interface
    Class JPReverseConv
'       Public Shared Function Main(Pcase As Integer, Pfel As Object, args As String)''~v101R~
        Public Shared Function Main(Pcase As Integer, Pfel As Object, args As String) as String''~v101I~
            Dim str As String = ""
            Dim fel = TryCast(Pfel, IFELanguage)
            Select Case Pcase
                Case 1
                    fel.Open()
                Case 2
                    str = fel.GetPhonetic(args, 1, -1)
                Case 3
                    fel.Close()
            End Select
            Return str
        End Function
    End Class
    '**********************************************                   
    Private Sub TBChanged()                                            ''~7522I~
        undoRedo.TBChanged()                                           ''~7522I~
    End Sub                                                            ''~7522I~
    Private Sub TBChanged(Ptext As String)                             ''~7522I~
        undoRedo.TBChanged(Ptext)                                      ''~7522I~
    End Sub                                                            ''~7522I~
    Private Sub TBChanged(Ptext As String, Pswchnghirakata As Boolean) ''~7522I~
        undoRedo.TBChanged(Ptext, Pswchnghirakata) 'reverse swViewKatakana when undo''~7522I~
    End Sub                                                            ''~7522I~
    Public Sub setHirakata(Pswchngkatahira As Boolean)                 ''~7522I~
        'from undroredo                                                ''~7522I~
        If Pswchngkatahira Then                                        ''~7522I~
            '           chngkatakanaSW()    'swap                      ''~7522I~
        End If                                                         ''~7522I~
    End Sub                                                            ''~7522I~
    '**********************************************                    ''~v001I~
    Private Function chkEOLCont(Pchii As Char, Pchars() As Char, Pcharctr As Integer, Pii As Integer, Pchcode As Integer) As Integer ''~v001R~
        If swMethod2 Then                                                   ''~v010M~
            Return chkEOLContM2(Pchii, Pchars, Pcharctr, Pii, Pchcode)   ''~v010I~
        End If                                                        ''~v010M~
'       Dim ii As Integer = Pii, chcode                                 ''~v001R~''~v101R~
        Dim ii As Integer = Pii, chcode as Integer                     ''~v101I~
        Dim ch As Char                                                 ''~v001I~
        If Pchcode = 1 OrElse Pchcode = 3 Then 'prev is kanji           ''~v001R~
        Else                                                           ''~v001I~
            Return 0                                                   ''~v001M~
        End If                                                         ''~v001M~
        If Pchii = FormatBES.SIGN_CRLF Then                                   ''~v001I~
            ii += 2                                                    ''~v001R~
            If ii < Pcharctr AndAlso AscW(Pchars(ii - 1)) = 13 AndAlso AscW(Pchars(ii)) = 10 Then ''~v001R~
            Else                                                       ''~v001I~
                Return 0                                              ''~v001I~
            End If                                                     ''~v001I~
        Else                                                           ''~v001I~
            If AscW(Pchii) = 13 Then                                          ''~v001I~
                ii += 1                                                ''~v001R~
                If ii < Pcharctr AndAlso AscW(Pchars(ii)) = 10 Then    ''~v001R~
                Else                                                   ''~v001I~
                    Return 0                                          ''~v001I~
                End If                                                 ''~v001I~
            Else                                                       ''~v001I~
                Return 0                                              ''~v001I~
            End If                                                     ''~v001I~
        End If                                                         ''~v001I~
        ii += 1                                                          ''~v001I~
        If ii < Pcharctr Then                                               ''~v001I~
        Else                                                           ''~v001I~
            Return 0                                                   ''~v001I~
        End If                                                         ''~v001I~
        If (ii + 1 < Pcharctr) Then                                    ''~v001I~
            ch = getCharType(Pchars(ii), Pchars(ii + 1))               ''~v001I~
        Else                                                           ''~v001I~
            ch = getCharType(Pchars(ii), Chr(&H0))                     ''~v001I~
        End If                                                         ''~v001I~
        chcode = AscW(ch)                                                ''~v001I~
        If chcode >= 1 AndAlso chcode <= 4 Then 'kanji/kana after kanji         ''~v001I~
        Else                                                           ''~v001I~
            Return 0                                                   ''~v001I~
        End If                                                         ''~v001I~
        '       Trace.W("chkEOLCont pchii=" & Pchii & ",pos-in=" & Pii & ",pos out=" & ii) ''~v001I~''~v029R~
        Return ii                                                     ''~v001I~
    End Function 'chkEOLCont                                                       ''~v001I~''~v010R~
    '*************************************************************************''~v010M~
    Private Function chkEOLContM2(Pchii As Char, Pchars() As Char, Pcharctr As Integer, Pii As Integer, Pchcode As Integer) As Integer ''~v010I~
        '** Pchii:current char, Pchars:text char array, Pcharctr:sizeof Pchars, Pii:current pos, Pchcode:prev type''~v010I~
        '** returns next pos or 0(not ignore CRLF)                         ''~v010I~
        Dim ii As Integer                                      ''~v010I~
        Dim prevch As Char                                             ''~v010I~
        ii = Pii                                                         ''~v010I~
        If Pchii = FormatBES.SIGN_CRLF Then                            ''~v010I~
            ii += 2                                                    ''~v010I~
            If ii < Pcharctr AndAlso AscW(Pchars(ii - 1)) = 13 AndAlso AscW(Pchars(ii)) = 10 Then ''~v010I~
                '               prevch = IIf(Pii = 0, CHAR_NULL, Pchars(Pii - 1))  'IIf execute bot,it cause execption''~v010R~
                If Pii = 0 Then                                             ''~v010I~
                    prevch = FormatBES.CHAR_SPACE 'one of delm           ''~v010R~
                Else                                                   ''~v010I~
                    prevch = Pchars(Pii - 1)                           ''~v010I~
                End If                                                 ''~v010I~
                If prevch = FormatBES.CHAR_LF Then 'continued CRLF   ''~v010R~
                    Return 0                                           ''~v010I~
                End If                                                 ''~v010I~
                If isDelmChar(prevch) Then 'accept CRLF if before is delm''~v010I~
                    Return 0                                           ''~v010I~
                End If                                                 ''~v010I~
            Else                                                       ''~v010I~
                Return 0                                               ''~v010I~
            End If                                                     ''~v010I~
        Else                                                           ''~v010I~
            If AscW(Pchii) = 13 Then                                   ''~v010I~
                ii += 1                                                ''~v010I~
                If ii < Pcharctr AndAlso AscW(Pchars(ii)) = 10 Then    ''~v010I~
                    '                   prevch = IIf(Pii = 0, CHAR_NULL, Pchars(Pii - 1))  ''~v010R~
                    If Pii = 0 Then                                         ''~v010I~
                        prevch = FormatBES.CHAR_SPACE 'one of delm       ''~v010R~
                    Else                                               ''~v010I~
                        prevch = Pchars(Pii - 1)                       ''~v010I~
                    End If                                             ''~v010I~
                    If prevch = FormatBES.SIGN_CRLF Then                 ''~v010I~
                        '                       prevch = IIf(Pii < 2, CHAR_NULL, Pchars(Pii - 2))''~v010R~
                        If Pii < 2 Then                                      ''~v010I~
                            prevch = FormatBES.CHAR_SPACE                ''~v010R~
                        Else                                           ''~v010I~
                            prevch = Pchars(Pii - 2)                   ''~v010I~
                        End If                                         ''~v010I~
                    End If                                             ''~v010I~
                    If prevch = FormatBES.CHAR_LF Then 'continued CRLF''~v010M~
                        Return 0                                       ''~v010M~
                    End If                                             ''~v010M~
                    If isDelmChar(prevch) Then 'accept CRLF if before is delm''~v010I~
                        Return 0                                       ''~v010I~
                    End If                                             ''~v010I~
                Else                                                   ''~v010I~
                    Return 0                                           ''~v010I~
                End If                                                 ''~v010I~
            Else                                                       ''~v010I~
                Return 0                                               ''~v010I~
            End If                                                     ''~v010I~
        End If                                                         ''~v010I~
        ii += 1                                                        ''~v010I~
        If ii < Pcharctr Then                                          ''~v010I~
        Else                                                           ''~v010I~
            Return 0                                                   ''~v010I~
        End If                                                         ''~v010I~
        '       Trace.W("chkEOLContM2 char prev=" & prevch & ",curr=" & Pchii & ",pos-in=" & Pii & ",pos out=" & ii) ''~v010R~''~v029R~
        Return ii                                                      ''~v010I~
    End Function '* chkEOLContM2                                       ''~v010I~
    '**********************************************                    ''~v004I~
    Private Function chkHiraKataChar(Pchii As Char, Pchars() As Char, Pcharctr As Integer, Pii As Integer, Pchcode As Integer, ByRef Ppchcode As Integer) As Char ''~v004R~
#If True Then                                                          ''~v028I~
#Else                                                                  ''~v028I~
        Dim chcode As Integer                                      ''~v004I~
        Dim ch, outch As Char                                     ''~v004I~
        Dim katahira As Integer = 0                                      ''~v004I~
#End If                                                                ''~v028I~
        '       Trace.W("chkHiraKataChar pchii=" & Pchii)                      ''~v006I~''~v029R~
        If DocOptions.swKatakanaDoc Then    'katakana document             ''~v027I~''~v030R~
            If Pchii = HIRAGANA_HE Then                                     ''~v027I~
                Return KATAKANA_HE                                     ''~v027I~
            ElseIf Pchii = HIRAGANA_BE Then                            ''~v027R~
                Return KATAKANA_BE                                     ''~v027I~
            Else                                                       ''~v027I~
                Return Pchii                                           ''~v027I~
            End If                                                     ''~v027R~
        End If                                                         ''~v027I~
#If True Then                                                          ''~v028I~
        Return Pchii                                                   ''~v028I~
#Else                                                                  ''~v028I~
                                                                       ''~v028I~
        If Pchii = HIRAGANA_HE OrElse Pchii = HIRAGANA_BE OrElse Pchii = KATAKANA_HE OrElse Pchii = KATAKANA_BE Then ''~v004I~
        Else                                                           ''~v004I~
            If Pchii = HIRAGANA_RI OrElse Pchii = KATAKANA_RI Then       ''~v009I~
            Else                                                         ''~v009I~
                Return Pchii                                               ''~v004I~
            End If                                                       ''~v009I~
        End If                                                         ''~v004I~
        '       If Pii + 2 < Pcharctr Then                                       ''~v004I~''~v022R~
        '           ch = getCharType(Pchars(Pii + 1), Pchars(Pii + 2))           ''~v004I~''~v022R~
        '       ElseIf Pii + 1 < Pcharctr Then                                   ''~v004I~''~v022R~
        '           ch = getCharType(Pchars(Pii + 1), Chr(&H0))                  ''~v004I~''~v022R~
        '       Else                                                           ''~v004I~''~v022R~
        '           ch = Chr(&H0)                                                       ''~v004I~''~v022R~
        '       End If                                                         ''~v022R~
        ch = getNextCharType(Pchars, Pcharctr, Pii)                        ''~v022I~
        outch = Pchii ''~v004I~
        chcode = AscW(ch)                                              ''~v004I~
        Select Case Pchcode  'prev char type                           ''~v004I~
            Case 2   'hiragana                                         ''~v004I~
                If chcode = 2 Then    'hira+?+hira                            ''~v004I~
                    katahira = 2   'assume hiragana                      ''~v004I~
                End If                                                 ''~v004I~
            Case 4   'katagana                                         ''~v004I~
                If chcode = 4 Then    'kata+?+kata                            ''~v004I~
                    katahira = 4   'assume katakana                      ''~v004I~
                End If                                                 ''~v004I~
            Case Else                                                  ''~v004I~
                Dim chcode2 = AscW(getNextCharType(Pchars, Pcharctr, Pii)) ''~v022I~
                If chcode2 = 2 Then    'hira+?+hira                    ''~v022I~
                    katahira = 2   'assume hiragana                    ''~v022I~
                ElseIf chcode2 = 4 Then    'kata+?+kata                ''~v022I~
                    katahira = 4   'assume katakana                    ''~v022I~
                Else                                                   ''~v022I~
                    Return outch   'curent type as is                      ''~v006R~
                End If                                                 ''~v022I~
        End Select                                                     ''~v004I~
        If katahira = 2 Then    'assume hiragana                              ''~v004I~
            If Pchii = KATAKANA_HE Then                                       ''~v004I~
                outch = HIRAGANA_HE                                      ''~v004I~
            ElseIf Pchii = KATAKANA_BE Then                                   ''~v004I~
                outch = HIRAGANA_BE                                      ''~v004I~
            ElseIf Pchii = KATAKANA_RI Then                            ''~v009I~
                outch = HIRAGANA_RI                                    ''~v009I~
            End If                                                     ''~v004I~
            Ppchcode = katahira                                          ''~v004I~
        ElseIf katahira = 4 Then    'kata+?+kata                        ''~v004R~
            If Pchii = HIRAGANA_HE Then                                       ''~v004I~
                outch = KATAKANA_HE                                      ''~v004I~
            ElseIf Pchii = HIRAGANA_BE Then                                   ''~v004I~
                outch = KATAKANA_BE                                      ''~v004I~
            ElseIf Pchii = HIRAGANA_RI Then                            ''~v009I~
                outch = KATAKANA_RI                                    ''~v009I~
            End If                                                     ''~v004I~
            Ppchcode = katahira                                          ''~v004I~
        End If                                                         ''~v004I~
'       Trace.W("chkHiraKataChar pchii=" & Pchii & ",pos-in=" & Pii & ",prev type=" & Pchcode & ",next type=" & chcode & ",char out=" & outch & ",chcodeout=" & Ppchcode) ''~v004R~''~v029R~
        Return outch                                                   ''~v004I~
#End If                                                                ''~v028I~
    End Function                                                       ''~v004I~
    '**********************************************                    ''~v025I~
    '   Private Function chkKaKe(Pchii As Char, Pchars() As Char, Pcharctr As Integer, Pii As Integer) As Boolean ''~v025I~''~v029R~
    Private Function chkKaKe(Pchii As Char, Pchars() As Char, Pcharctr As Integer, Pii As Integer, Pprevchcode As Integer) As Boolean ''~v029I~
        ' chk kanji+tsu, ka/ke+kanji and conv with kanji                   ''~v029I~
        Dim chcode As Integer                                          ''~v025I~
        If TSUCHARS.IndexOf(Pchii) >= 0 Then                            ''~v029I~
            If Pprevchcode = 1 OrElse Pprevchcode = 3 Then 'kanji               ''~v029I~
                Return True                                            ''~v029I~
            End If                                                     ''~v029I~
        End If                                                         ''~v029I~
        If KAKECHARS.IndexOf(Pchii) < 0 Then                                ''~v025R~
            Return False                                               ''~v025I~
        End If                                                         ''~v025I~
        chcode = AscW(getNextCharType(Pchars, Pcharctr, Pii))          ''~v025I~
        If Not (chcode = 1 OrElse chcode = 3) Then  'kanji                 ''~v025I~
            Return False                                               ''~v025I~
        End If                                                         ''~v025I~
        '       Trace.W("chkKaKe pchii=" & Pchii & " return true=")            ''~v025I~''~v029R~
        Return True                                                    ''~v025I~
    End Function                                                       ''~v025I~
    '*************************************************************************''~v022I~
    Function getNextCharType(Pchars() As Char, Pcharctr As Integer, Pii As Integer) As Char ''~v022I~
'       Dim ch                                                        ''~v022I~''~v101R~
        Dim ch as Char                                                 ''~v101I~
        If Pii + 2 < Pcharctr Then                                     ''~v022I~
            ch = getCharType(Pchars(Pii + 1), Pchars(Pii + 2))         ''~v022I~
        ElseIf Pii + 1 < Pcharctr Then                                 ''~v022I~
            ch = getCharType(Pchars(Pii + 1), Chr(&H0))                ''~v022I~
        Else                                                           ''~v022I~
            ch = Chr(&H0)                                              ''~v022I~
        End If                                                         ''~v022I~
        Return ch                                                      ''~v022I~
    End Function                                                       ''~v022I~
    '*************************************************************************''~v008I~
    Function applyDictionary(Pfiletext As String) As String            ''~v008R~
        '       Trace.swTrace = True                                           ''~v008I~''~v029R~
        '       Dim words As String() = dictionaryWords                        ''~v008R~
        '       Dim repwords As String() = dictionaryRepwords                  ''~v008R~
        '       Dim ctrWords As Integer = words.Length                         ''~v008R~
        Dim sb As StringBuilder
        '       Trace.W("applyDictionary in str=" & Pfiletext)                 ''~v008I~''~v029R~
        swException = False                                              ''~v010I~
        Try                                                            ''~v008I~
            sb = suppressEOL(Pfiletext)                                ''~v008R~
            '           For ii As Integer = 0 To ctrWords - 1                      ''~v008R~
            '               If words(ii).Length > 0 Then                           ''~v008R~
            '                   sb.Replace(words(ii), repwords(ii))                ''~v008R~
            '               End If                                                 ''~v008R~
            '           Next                                                       ''~v008R~
            Form11.sharedApplyDictionary(sb)                           ''~v008I~
        Catch ex As Exception                                          ''~v008I~
            '           MessageBox.Show(String.Format("かな/カナ変換に失敗 Source={0},stack={1}", ex.Source, ex.StackTrace.ToString()))''~v008I~
            '           MessageBox.Show(String.Format("{2}," & vbCrlf & "Source={0}," & vbCrlf & "stack={1}", ex.Source, ex.StackTrace.ToString(),ex.Message), Rstr.MSG_ERR_FAILED_KANJI2KANA_CONV) ''~v008I~''~v010R~
            exceptionMsg(ex, Rstr.MSG_ERR_FAILED_KANJI2KANA_CONV)       ''~v010I~
            swException = True                                           ''~v010I~
            Return ""                                                  ''~v008I~
        End Try                                                        ''~v008I~
        Dim str = sb.ToString()
        '       Trace.W("applyDictionary out str=" & str)                      ''~v008R~''~v029R~
        Return str                                           ''~v008I~
    End Function                                                       ''~v008I~
    '*************************************************************************''~v008I~
    Function suppressEOL(Pfiletext As String) As System.Text.StringBuilder ''~v008R~
        Dim sb = New System.Text.StringBuilder()                       ''~v008I~
        Dim chars() As Char = Pfiletext.ToCharArray()                  ''~v008R~
        Dim charctr As Integer = chars.Length()                        ''~v008I~
        Dim ii As Integer = 0, wordctr As Integer = 0                  ''~v008I~
        Dim chcode As Integer                                          ''~v008I~
        Dim prevchcode As Integer                                      ''~v008I~
        Dim ch, chii, chiiprev As Char                                 ''~v008I~
        Dim swkanji As Boolean = False                                 ''~v008I~
        Dim swprevkana As Boolean                                      ''~v008I~
        Dim swprevhiragana, swprevkatakana As Boolean                  ''~v008I~
        Dim iinext As Integer                                          ''~v008I~
        chii = Chr(&H0)                                                ''~v008I~
        Do While ii < charctr                                          ''~v008I~
            chiiprev = chii                                            ''~v008I~
            chii = chars(ii)                                           ''~v008I~
            iinext = chkEOLCont(chii, chars, charctr, ii, chcode)  'chk continued word over EOL''~v008I~
            If iinext > 0 Then  'jump over EOL                         ''~v008I~
                ii = iinext                                            ''~v008I~
                chii = chars(ii)                                       ''~v008I~
            End If                                                     ''~v008I~
            If (ii + 1 < charctr) Then                                 ''~v008I~
                ch = getCharType(chii, chars(ii + 1))                  ''~v008I~
            Else                                                       ''~v008I~
                ch = getCharType(chii, Chr(&H0))                       ''~v008I~
            End If                                                     ''~v008I~
            swprevhiragana = chcode = 2                                ''~v008I~
            swprevkatakana = chcode = 4                                ''~v008I~
            swprevkana = swprevhiragana OrElse swprevkatakana   'hiragana or katakana''~v008I~
            prevchcode = chcode                                          ''~v008I~
            chcode = AscW(ch)                                          ''~v008I~
            chii = chkHiraKataChar(chii, chars, charctr, ii, prevchcode, chcode)  'chk hiragana He is katakana He?''~v008I~
            '           Trace.W("suppressEOL chii=" & chii & ",chcode=" & chcode & ",prevhira=" & swprevhiragana & ",prevkata=" & swprevkatakana)          '@@@@test''~v008I~''~v029R~
            sb.Append(chii)                                            ''~v008I~
            ii += 1                                                    ''~v008I~
        Loop                                                           ''~v008I~
        '       Trace.W("suppressEOL sb=" & sb.ToString())                                ''~v008I~''~v029R~
        Return sb                                                      ''~v008I~
    End Function                                                       ''~v008I~
    '*************************************************************************''~v010I~
    Private Function isDelmChar(Pch As Char) As Boolean                ''~v010R~
        Dim rc As Boolean = DELMCHARS.IndexOf(Pch) >= 0                  ''~v010R~
        Return rc                                                      ''~v010I~
    End Function                                                       ''~v010I~
    '*************************************************************************''~v023I~
    Private Function isSplitter(Pch As Char) As Boolean                ''~v023I~
        If isDelmChar(Pch) Then                                             ''~v023I~
            Return True                                                ''~v023I~
        End If                                                         ''~v023I~
        Dim rc As Boolean = SPLITCHARS.IndexOf(Pch) >= 0               ''~v023I~
        '       Trace.W("splitter=" & SPLITCHARS)                              ''~v023I~''~v029R~
        Return rc                                                   ''~v023I~
    End Function                                                       ''~v023I~
    '*************************************************************************''~v010I~
    Private Sub exceptionMsg(Pex As Exception, Pdesc As String)                   ''~v010I~
        '           MessageBox.Show(String.Format("かな/カナ変換に失敗 Source={0},stack={1}", ex.Source, ex.StackTrace.ToString()))''~v010I~
        MessageBox.Show(String.Format("{2}" & vbCrLf & "Source={0}" & vbCrLf & "stack={1}", Pex.Source, Pex.StackTrace.ToString(), Pex.Message), Pdesc) ''~v010R~
    End Sub                                                            ''~v010I~
End Class
