'*CID:''+v181R~:#72                          update#=  191;          ''+v181R~
'************************************************************************************''~v030I~
'v181 2020/01/26 ReplaceKey:default F2                                 ''+v181I~
'v163 2018/03/03 add string customizability for kata/hira chikan       ''~v163I~
'v115 2017/12/26 support dakuon,handakuon key                          ''~v115I~
'v110 2017/12/22 StringConstant reset required when lang changed       ''~v110I~
'v105 2017/12/20 display char type not by f4 but automatically by keyup/mouseclick event''~v105I~
'v101 2017/12/16 Conversion warning                                    ''~v101I~
'v070 2017/09/26 F3 for find dialog is configurable                    ''~v070I~
'v065 2017/09/24 Word dialog by Ctrl+char(except "1"-"0")              ''~v065I~
'v038 2017/09/22 (Bug)SpecialKey default is also F5 when text cleared  ''~v038I~
'v037 2017/09/22 assign F4 as query of replacing char                  ''~v037I~
'v030 2017/09/21 new option dialog for each document                   ''~v030I~
'************************************************************************************''~v030I~
Imports System.Globalization                                            ''~7613I~
Imports System.ComponentModel                                           ''~7613I~
Imports System.Threading                                               ''~7613I~
Public Class FormOptions
    'locale done                                                           ''~7618I~

    Public Shared keySmallKey As Keys                                          ''~7502I~''~7525M~
#If False Then                                                              ''~v105I~
    Public Shared keySmallKeyQ As Keys                                 ''~v037R~
#End If                                                                ''~v105I~
    Public Shared keyDakuonKey As Keys                                 ''~v115I~
    Public Shared keyWordsKey As Keys                                  ''~v065I~
    Public Shared keyFindKey As Keys                                   ''~v070I~
    Public Shared keyReplaceKey As Keys                                ''+v181I~
    Public Shared keySpecialCharKey As Keys                                   ''~7515I~''~7525M~
    Public Shared swWinBES99 As Boolean                               ''~7525I~''~7604R~
    '   Public Shared swEnglishDoc As Boolean                              ''~7618I~''~7619R~
    Public Shared swLangEN As Boolean                                ''~7613R~
    Private cfgLang As Integer                                         ''~7614I~
    Private Const ID_LANG_DEFAULT = 0                                    ''~7614I~
    Private Const ID_LANG_JP = 1                                    ''~7614I~
    Private Const ID_LANG_EN = 2                                    ''~7614I~
    ''~7525I~
    '   Private Const CONST_REGULER = "標準"                               ''~7508R~''~7618R~
    '   Private Const CONST_BOLD = "太字"                                  ''~7508R~''~7618R~
    '   Private Const CONST_ITALIC = "斜体"                                ''~7508R~''~7618R~
    '   Private CONST_REGULER_JP,CONST_REGULER_EN as String                ''~7618I~
    Private CONST_BOLD_JP, CONST_BOLD_EN As String                      ''~7618I~
    Private CONST_ITALIC_JP, CONST_ITALIC_EN As String                  ''~7618I~
    Private Const CONST_MAX_FKEY = 12                                          ''~7502R~''~7508R~
    Private fontFamily As String
    Private fontSize As Integer                                        ''~7508R~
    Private fontStyle As FontStyle
    Private fontFamilyScr As String                                    ''~7515I~
    Private fontSizeScr As Integer                                     ''~7515I~
    Private fontStyleScr As FontStyle                                  ''~7515I~
    '   Public swKatakana As Boolean = False                               ''~7501R~''~7507R~
    Private Const DEFAULT_KEY_SMALL = 5                                  ''~7502R~''~7525R~
#If False Then                                                              ''~v105I~
    Private Const DEFAULT_KEY_SMALLQ = 4                               ''~v037R~
#End If                                                                ''~v105I~
    Private Const DEFAULT_KEY_DAKUON = 4                               ''~v115I~
    Private Const DEFAULT_KEY_WORDS = 9                                ''~v065R~''~v070R~
    Private Const DEFAULT_KEY_FIND = 3                          ''~v070I~
    Private Const DEFAULT_KEY_REPLACE = 2                              ''+v181I~
    Private Const DEFAULT_KEY_SPECIALCHAR = 6                          ''~7515I~''~7525R~
    Public keySmall As Integer = DEFAULT_KEY_SMALL                      ''~7502R~
#If False Then                                                              ''~v105I~
    Public keySmallQ As Integer = DEFAULT_KEY_SMALLQ                   ''~v065I~
#End If                                                                ''~v105I~
    Public keyDakuon As Integer = DEFAULT_KEY_DAKUON                   ''~v115I~
    Public keyWords As Integer = DEFAULT_KEY_WORDS                     ''~v065I~
    Public keyFind As Integer = DEFAULT_KEY_FIND                       ''~v070I~
    Public keyReplace As Integer = DEFAULT_KEY_REPLACE                 ''+v181I~
    Public keySpecialChar As Integer = DEFAULT_KEY_SPECIALCHAR         ''~7515I~
    Private samePrintFont As Boolean                                   ''~7515I~
    Private swInit As Boolean = False                                    ''~7614I~
    Public swFontChangedScr As Boolean                                ''~7515I~
    Public swLangChanged As Boolean = False                            ''~7618I~

    '************************************************************************''~v110I~
    Sub New()
        swInit = True 'ignore Lang checkchanged                          ''~7614I~
        InitializeComponent()
        hideNonDebug()                                                 ''~v110I~
        Form1.setupTitlebarIcon(Me)                                    ''~7612I~
        initByCFG()                                                    ''~7508I~
        '       CONST_REGULAR_JP=Rstr.getStr(Rstr.FONT_STD,Rstr.ID_LANG_JP)    ''~7618I~
        '       CONST_REGULAR_EN=Rstr.getStr(Rstr.FONT_STD,Rstr.ID_LANG_EN)    ''~7618I~
        CONST_BOLD_JP = Rstr.getStr(Rstr.STRID_FONT_BOLD, Rstr.ID_LANG_JP) ''~7618R~
        CONST_BOLD_EN = Rstr.getStr(Rstr.STRID_FONT_BOLD, Rstr.ID_LANG_EN) ''~7618R~
        CONST_ITALIC_JP = Rstr.getStr(Rstr.STRID_FONT_ITALIC, Rstr.ID_LANG_JP) ''~7618R~
        CONST_ITALIC_EN = Rstr.getStr(Rstr.STRID_FONT_ITALIC, Rstr.ID_LANG_EN) ''~7618R~
        showOptions()
        swInit = False                                                   ''~7614I~
    End Sub
    '************************************************************************''~v110I~
    Public Shared Sub SetLangIDInitial()                               ''~v110I~
        Dim id As Integer = My.Settings.CFGF5_LangID                   ''~v110I~
        swLangEN = setLangEN(id)                                       ''~v110I~
        setCulture(swLangEN)                                           ''~v110I~
    End Sub                                                            ''~v110I~
    '************************************************************************''~v110I~
    Private Sub initByCFG()                                            ''~7508I~
        ''~7508I~
        fontFamilyScr = My.Settings.CFGF5_FontNameScr                  ''~7515I~
        fontSizeScr = My.Settings.CFGF5_FontSizeScr                    ''~7515R~
        fontStyleScr = My.Settings.CFGF5_FontStyleScr                  ''~7515R~
        fontFamily = My.Settings.CFGF5_FontName                          ''~7508I~
        fontSize = My.Settings.CFGF5_FontSize                            ''~7508I~''~7515R~
        fontStyle = My.Settings.CFGF5_FontStyle                          ''~7508I~''~7515R~
        samePrintFont = My.Settings.CFGF5_PrintFontSame                 ''~7515I~
        '       swWinBES99 = My.Settings.CFGF12_swBES99          ''~7525I~''~7604R~''~v030R~
        '       swEnglishDoc = My.Settings.CFGF5_EnglishDoc                    ''~7618R~''~7619R~
        cfgLang = My.Settings.CFGF5_LangID                             ''~7619R~
        swLangEN = setLangEN(cfgLang)                                  ''~7614I~
        keySmall = My.Settings.CFGF5_KeySmall                            ''~7508I~''~7515R~
        If keySmall = 0 Then                                                  ''~7508I~
            keySmall = DEFAULT_KEY_SMALL                                ''~7508I~
        End If                                                         ''~7508I~
        '       keySmallKey = Keys.F1 + keySmall - 1                            ''~7521I~''~v101R~
        keySmallKey = CType(Keys.F1 + keySmall - 1, Keys)               ''~v101I~
        ''~v065I~
#If False Then                                                              ''~v105I~
        keySmallQ = My.Settings.CFGF5_KeySmallQ                        ''~v037R~
        If keySmallQ = 0 Then                                          ''~v037R~
            keySmallQ = DEFAULT_KEY_SMALLQ                             ''~v037R~
        End If                                                         ''~v037R~
        '       keySmallKeyQ = Keys.F1 + keySmallQ - 1                         ''~v037R~''~v101R~
        keySmallKeyQ = CType(Keys.F1 + keySmallQ - 1, Keys)             ''~v101I~
#End If                                                                ''~v105I~
        keyDakuon = My.Settings.CFGF5_KeyDakuon                        ''~v115I~
        If keyDakuon = 0 Then                                          ''~v115I~
            keyDakuon = DEFAULT_KEY_DAKUON                             ''~v115I~
        End If                                                         ''~v115I~
        keyDakuonKey = CType(Keys.F1 + keyDakuon - 1, Keys)            ''~v115R~
        ''~v070R~
        keyWords = My.Settings.CFGF5_KeyWords                          ''~v065I~
        If keyWords = 0 Then                                           ''~v065I~
            keyWords = DEFAULT_KEY_WORDS                               ''~v065I~
        End If                                                         ''~v065I~
        '       keyWordsKey = Keys.F1 + keyWords - 1                           ''~v065R~''~v101R~
        keyWordsKey = CType(Keys.F1 + keyWords - 1, Keys)               ''~v101I~
        ''~v070R~
        keyFind = My.Settings.CFGF5_KeyFind                            ''~v070I~
        If keyFind = 0 Then                                            ''~v070I~
            keyFind = DEFAULT_KEY_FIND                                 ''~v070I~
        End If                                                         ''~v070I~
        keyReplace = My.Settings.CFGF5_KeyReplace                      ''+v181I~
        If keyReplace = 0 Then                                         ''+v181I~
            keyReplace = DEFAULT_KEY_REPLACE                           ''+v181I~
        End If                                                         ''+v181I~
        '       keyFindKey = Keys.F1 + keyFind - 1                             ''~v070I~''~v101R~
        keyFindKey = CType(Keys.F1 + keyFind - 1, Keys)                 ''~v101I~
        keyReplaceKey = CType(Keys.F1 + keyReplace - 1, Keys)          ''+v181I~
        ''~v070I~
        keySpecialChar = My.Settings.CFGF5_KeySpecialChar              ''~v070I~
        If keySpecialChar = 0 Then                                           ''~7515I~''~7521R~
            keySpecialChar = DEFAULT_KEY_SPECIALCHAR                         ''~7515I~''~7521R~
        End If                                                         ''~7515I~
        '       keySpecialCharKey = Keys.F1 + keySpecialChar - 1               ''~7515I~''~v101R~
        keySpecialCharKey = CType(Keys.F1 + keySpecialChar - 1, Keys)   ''~v101I~
        setLang(Me, GetType(FormOptions))                                           ''~7613R~
    End Sub  ' initByCFG                                                          ''~7508I~''~v065R~
    Private Sub saveCFG()                                              ''~7508I~
        My.Settings.CFGF5_FontNameScr = fontFamilyScr                  ''~7515I~
        My.Settings.CFGF5_FontSizeScr = fontSizeScr                    ''~7515R~
        My.Settings.CFGF5_FontStyleScr = fontStyleScr                  ''~7515R~
        My.Settings.CFGF5_FontName = fontFamily                          ''~7508I~
        My.Settings.CFGF5_FontSize = fontSize                            ''~7508I~''~7515R~
        My.Settings.CFGF5_FontStyle = fontStyle                          ''~7508I~''~7515R~
        My.Settings.CFGF5_KeySmall = keySmall                            ''~7508I~''~7515R~
#If False Then                                                              ''~v105I~
        My.Settings.CFGF5_KeySmallQ = keySmallQ                        ''~v037R~
#End If                                                                ''~v105I~
        My.Settings.CFGF5_KeyDakuon = keyDakuon                        ''~v115I~
        My.Settings.CFGF5_KeyWords = keyWords                          ''~v065I~
        My.Settings.CFGF5_KeyFind = keyFind                            ''~v070I~
        My.Settings.CFGF5_KeyReplace = keyReplace                      ''+v181I~
        My.Settings.CFGF5_KeySpecialChar = keySpecialChar              ''~7515I~
        My.Settings.CFGF5_PrintFontSame = samePrintFont                  ''~7515I~
        '       My.Settings.CFGF12_swBES99 = swWinBES99             ''~7525I~''~7604R~''~v030R~
        '       My.Settings.CFGF5_EnglishDoc = swEnglishDoc                    ''~7618R~''~7619R~
        My.Settings.CFGF5_LangID = cfgLang                             ''~7614I~''~7619R~
    End Sub                                                            ''~7508I~
    Private Sub FormOptions_Load(sender As System.Object, e As System.EventArgs) Handles MyBase.Load
        Me.ActiveControl = Me.ButtonOK    'set focus at Load time
        showOptions()
    End Sub
    Private Sub ButtonCancel_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonCancel.Click
        initByCFG()     'restore old value                             ''~7618I~
        Me.Close()
    End Sub 'resize
    Private Sub ButtonOK_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonOK.Click
        If getOptions() Then
            If swLangChanged Then                                           ''~7618I~
                Form1.langChanged()                                    ''~7618I~
            End If                                                     ''~7618I~
            swLangChanged = False                                        ''~7618I~
            Me.Close()
        End If
    End Sub 'resize
    Private Sub RGLang_Changed(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RBLangJP.CheckedChanged, RBLangEN.CheckedChanged, RBLangDefault.CheckedChanged ''~7614R~
        Dim sw As Boolean                                              ''~7614I~
        cfgLang = getRGLang()                                            ''~7614I~
        sw = setLangEN(cfgLang)                                        ''~7614I~
        If sw <> swLangEN Then                                         ''~7614I~
            swLangEN = sw                                              ''~7614I~
            setLang(Me, GetType(FormOptions))                          ''~7614I~
            If Not swInit Then                                              ''~7614I~
                Me.Show()                                              ''~7614R~
                '               Form1.langChanged()                                    ''~7614R~''~7618R~
                swLangChanged = True                                     ''~7618I~
            End If                                                     ''~7614I~
        End If                                                         ''~7614I~
    End Sub                                                            ''~7614I~
    Private Function getRGLang() As Integer                            ''~7614I~
        Dim optLang As Integer                                              ''~7614I~
        Select Case True                                               ''~7614I~
            Case RBLangDefault.Checked                                     ''~7614I~
                optLang = ID_LANG_DEFAULT                                    ''~7614I~
            Case RBLangJP.Checked                                          ''~7614I~
                optLang = ID_LANG_JP                                         ''~7614I~
            Case RBLangEN.Checked                                          ''~7614I~
                optLang = ID_LANG_EN                                         ''~7614I~
            Case Else                                                      ''~7614I~
                optLang = ID_LANG_DEFAULT                                    ''~7614I~
        End Select                                                     ''~7614I~
        Return optLang                                                 ''~7614I~
    End Function                                                    ''~7614I~
    Private Sub setRGLang(PlangID As Integer)                          ''~7614I~
        Select Case PlangID                                                 ''~7614I~
            Case ID_LANG_JP                                                ''~7614I~
                RBLangJP.Checked = True                                      ''~7614I~
            Case ID_LANG_EN                                                ''~7614I~
                RBLangEN.Checked = True                                      ''~7614I~
            Case Else                                                      ''~7614I~
                RBLangDefault.Checked = True                                 ''~7614I~
        End Select                                                     ''~7614I~
    End Sub                                                            ''~7614I~
    Private Sub ButtonHelp_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonHelp.Click ''~7430I~
        showHelp()                                                     ''~7430I~
    End Sub 'resize                                                    ''~7430I~
    Private Sub ButtonPrintFont_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonPrintFont.Click
        dialogFont()
    End Sub 'resize
    Private Sub ButtonScrFont_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonScrFont.Click ''~7515I~
        dialogFontScr()                                                ''~7515I~
    End Sub 'resize                                                    ''~7515I~
    Private Sub ButtonAddString_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonAddString.Click ''~v163I~
        dialogAddString()                                              ''~v163I~
    End Sub 'resize                                                    ''~v163I~
    Private Sub showOptions()
        TextBoxScrFontName.Text = createFontnameScr()                   ''~7508I~''~7515R~
        TextBoxPrintFontname.Text = createFontname()                   ''~7515R~
        CheckBoxPrintFont.Checked = samePrintFont                      ''~7515M~
        '       CheckBoxWinBES99.Checked = swWinBES99              ''~7525I~   ''~7604R~''~v030R~
        '       CheckBoxEnglishDoc.Checked = swEnglishDoc                      ''~7618I~''~7619R~
        setRGLang(cfgLang)                                             ''~7614I~
        TextBoxKeySmallKana.Text = "F" & keySmall                      ''~7508I~
#If False Then                                                              ''~v105I~
        TextBoxKeySmallkanaQ.Text = "F" & keySmallQ                    ''~v037R~
#End If                                                                ''~v105I~
        TextBoxKeyDakuon.Text = "F" & keyDakuon                        ''~v115I~
        TextBoxKeyWords.Text = "F" & keyWords                          ''~v065I~
        TextBoxKeyFind.Text = "F" & keyFind                            ''~v070I~
        TextBoxKeyReplace.Text = "F" & keyReplace                      ''+v181I~
        TextBoxKeySpecialChar.Text = "F" & keySpecialChar              ''~7515I~
    End Sub 'resize
    Private Function getOptions() As Boolean
        Dim sb = New System.Text.StringBuilder()
        Dim sf As String = getScrFont()                                ''~7515R~
        If IsNothing(sf) Then                                          ''~7515I~
            Return False                                               ''~7515I~
        End If                                                         ''~7515I~
        Dim pf As String = getPrintFont()
        If IsNothing(pf) Then
            Return False
        End If
        samePrintFont = CheckBoxPrintFont.Checked                        ''~7515I~
        '       swWinBES99 = CheckBoxWinBES99.Checked                ''~7525I~ ''~7604R~''~v030R~
        '       swEnglishDoc = CheckBoxEnglishDoc.Checked                      ''~7618I~''~7619R~
        '       Dim keyval As Integer = getKeyValue(TextBoxlabelSmallKey.Text, TextBoxKeySmallKana.Text)                ''~7502R~''~7515R~''~v038R~
        Dim keyval As Integer                                          ''~v038I~
        keyval = getKeyValue(TextBoxLabelSmallKey.Text, TextBoxKeySmallKana.Text, DEFAULT_KEY_SMALL) ''~v038I~
        If keyval < 0 Then                                                    ''~7502I~
            Return False                                               ''~7502I~
        End If                                                         ''~7502I~
        keySmall = keyval                                                ''~7502I~
        '       keySmallKey = Keys.F1 + keySmall - 1                           ''~7502I~''~v101R~
        keySmallKey = CType(Keys.F1 + keySmall - 1, Keys)               ''~v101I~
        ''~v065I~
        '       keyval = getKeyValue(TextBoxlabelQueryKey.Text, TextBoxKeySmallKanaQ.Text)''~v037R~''~v038R~
#If False Then                                                              ''~v105I~
        keyval = getKeyValue(TextBoxLabelQueryKey.Text, TextBoxKeySmallkanaQ.Text, DEFAULT_KEY_SMALLQ) ''~v038I~
        If keyval < 0 Then                                             ''~v037R~
            Return False                                               ''~v037R~
        End If                                                         ''~v037R~
        keySmallQ = keyval                                             ''~v037R~
        '       keySmallKeyQ = Keys.F1 + keySmallQ - 1                         ''~v037R~''~v101R~
        keySmallKeyQ = CType(Keys.F1 + keySmallQ - 1, Keys)             ''~v101I~
#End If                                                                ''~v105I~
        keyval = getKeyValue(TextBoxLabelDakuon.Text, TextBoxKeyDakuon.Text, DEFAULT_KEY_DAKUON) ''~v115I~
        If keyval < 0 Then                                             ''~v115I~
            Return False                                               ''~v115I~
        End If                                                         ''~v115I~
        keyDakuon = keyval                                             ''~v115I~
        keyDakuonKey = CType(Keys.F1 + keyDakuon - 1, Keys)            ''~v115R~
        ''~v115I~
        keyval = getKeyValue(TextBoxlabelWords.Text, TextBoxKeyWords.Text, DEFAULT_KEY_WORDS) ''~v065I~
        If keyval < 0 Then                                             ''~v065I~
            Return False                                               ''~v065I~
        End If                                                         ''~v065I~
        keyWords = keyval                                              ''~v065I~
        '       keyWordsKey = Keys.F1 + keyWords - 1                           ''~v065R~''~v101R~
        keyWordsKey = CType(Keys.F1 + keyWords - 1, Keys)               ''~v101I~
        ''~v070I~
        keyval = getKeyValue(TextBoxLabelFind.Text, TextBoxKeyFind.Text, DEFAULT_KEY_FIND) ''~v070I~
        If keyval < 0 Then                                             ''~v070I~
            Return False                                               ''~v070I~
        End If                                                         ''~v070I~
        keyFind = keyval                                               ''~v070I~
        '       keyFindKey = Keys.F1 + keyFind - 1                             ''~v070I~''~v101R~
        keyFindKey = CType(Keys.F1 + keyFind - 1, Keys)                 ''~v101I~
        keyval = getKeyValue(TextBoxLabelReplace.Text, TextBoxKeyReplace.Text, DEFAULT_KEY_REPLACE) ''+v181I~
        If keyval < 0 Then                                             ''+v181I~
            Return False                                               ''+v181I~
        End If                                                         ''+v181I~
        keyReplace = keyval                                            ''+v181I~
        keyReplaceKey = CType(Keys.F1 + keyReplace - 1, Keys)          ''+v181I~
        ''~7515I~
        '       keyval = getKeyValue(TextBoxLabelSpecialChar.Text, TextBoxKeySpecialChar.Text) ''~7515I~''~v038R~
        keyval = getKeyValue(TextBoxLabelSpecialChar.Text, TextBoxKeySpecialChar.Text, DEFAULT_KEY_SPECIALCHAR) ''~v038I~
        If keyval < 0 Then                                             ''~7515I~
            Return False                                               ''~7515I~
        End If                                                         ''~7515I~
        keySpecialChar = keyval                                         ''~7515I~
        '       keySpecialCharKey = Keys.F1 + keySpecialChar - 1               ''~7515I~''~v101R~
        keySpecialCharKey = CType(Keys.F1 + keySpecialChar - 1, Keys)   ''~v101I~
        saveCFG()                                                      ''~7508I~
        Form1.MainForm.optionChanged()                                         ''~7501I~''~7521R~
        swFontChangedScr = False     'notified                           ''~7515I~
        Return True
    End Function
    '   Private Function getKeyValue(Plabel As String, Pstr As String) As Integer ''~7502R~''~7515R~''~v038R~
    Private Function getKeyValue(Plabel As String, Pstr As String, Pdefault As Integer) As Integer ''~v038I~
        Dim val As Integer                                             ''~7501I~
        Dim str As String = Pstr                                         ''~7502I~
        If str.Length > 1 Then                                         ''~7502R~
            Dim ch As Char = str.Chars(0)                                ''~7502I~
            If ch = "F"c OrElse ch = "f"c Then                                  ''~7502I~
                str = str.Substring(1)                                   ''~7502I~
            End If                                                     ''~7502I~
        End If                                                         ''~7502I~
        If Pstr.Length < 1 Then                                              ''~7501I~''~7502R~
            '           Return DEFAULT_KEY_SMALL                                  ''~7502R~''~v038R~
            Return Pdefault                                            ''~v038I~
        End If                                                          ''~7501I~
        If Not getNum(str, val) OrElse val < 1 OrElse val > CONST_MAX_FKEY Then                   ''~7501I~''~7502R~
            '           MessageBox.Show("""" & Plabel & """ には 機能キーFn の番号:1～" & CONST_MAX_FKEY & " の数字を指定してください") ''~7502R~''~7515R~''~7618R~
            MessageBox.Show(Plabel & "(" & str & ")", Rstr.MSG_ERR_FUNCKEY) ''~7618I~
            Return -1                                                  ''~7502R~
        End If                                                         ''~7501I~
        Return val                                                     ''~7502R~
    End Function                                                            ''~7502R~
    Private Function getPrintFont() As String
        Dim str As String = TextBoxPrintFontname.Text
        If (Not parseFontname(str)) Then
            '           MessageBox.Show("印刷フォント名に誤りがあります:" & str)   ''~7618R~
            MessageBox.Show(str, Rstr.MSG_ERR_PRINT_FONT)               ''~7618I~
            Return (Nothing)
        End If
        str = createFontname()
        Return str
    End Function
    Private Function getScrFont() As String                            ''~7515R~
        Dim str As String = TextBoxScrFontName.Text                    ''~7515I~
        If (Not parseFontnameScr(str)) Then                            ''~7515R~
            '           MessageBox.Show("画面フォント名に誤りがあります:" & str)   ''~7515I~''~7618R~
            MessageBox.Show(str, Rstr.MSG_ERR_SCR_FONT)                 ''~7618I~
            Return (Nothing)                                           ''~7515I~
        End If                                                         ''~7515I~
        str = createFontnameScr()                                      ''~7515I~
        Return str                                                     ''~7515I~
    End Function                                                       ''~7515I~
    Private Function createFontname() As String
        Dim style As String
        style = getStyleName(fontStyle)
        Return fontFamily & " ; " & style & " ; " & fontSize
    End Function
    Public Function createFontnameScr() As String                      ''~7515R~
        Dim style As String                                            ''~7515I~
        style = getStyleName(fontStyleScr)                             ''~7515I~
        Return fontFamilyScr & " ; " & style & " ; " & fontSizeScr     ''~7515I~
    End Function                                                       ''~7515I~
    Private Function parseFontname(Pfontname As String) As Boolean
        Dim fnt() As String = Pfontname.Split(";"c)
        Dim val As Integer
        If Not getNum(fnt(2), val) OrElse val <= 0 Then
            Return (False)
        End If
        fontFamily = fnt(0).Trim()                                     ''~7430R~
        fontStyle = getStyleID(fnt(1))
        fontSize = val
        Return True
    End Function
    Private Function parseFontnameScr(Pfontname As String) As Boolean  ''~7515I~
        Dim fnt() As String = Pfontname.Split(";"c)                    ''~7515I~
        Dim val As Integer                                             ''~7515I~
        If Not getNum(fnt(2), val) OrElse val <= 0 Then                ''~7515I~
            Return (False)                                             ''~7515I~
        End If                                                         ''~7515I~
        fontFamilyScr = fnt(0).Trim()                                  ''~7515I~
        fontStyleScr = getStyleID(fnt(1))                              ''~7515I~
        fontSizeScr = val                                              ''~7515I~
        Return True                                                    ''~7515I~
    End Function                                                       ''~7515I~
    Private Function getStyleName(Pfontstyle As FontStyle) As String
        Dim nm As String
        Select Case Pfontstyle
            Case FontStyle.Bold
                '               nm = CONST_BOLD                                        ''~7618R~
                nm = Rstr.FONT_BOLD                                    ''~7618I~
            Case FontStyle.Italic
                '               nm = CONST_ITALIC                                      ''~7618R~
                nm = Rstr.FONT_ITALIC                                  ''~7618I~
            Case (FontStyle.Italic Or FontStyle.Bold)                  ''~7618R~
                nm = Rstr.FONT_BOLD_ITALIC                             ''~7618I~
            Case Else
                '               nm = CONST_REGULER                                     ''~7618R~
                nm = Rstr.FONT_STD                                 ''~7618I~
        End Select
        Return nm
    End Function
    Private Function getStyleID(Pfontstyle As String) As FontStyle
        Dim style As FontStyle
        Dim str As String = Pfontstyle.Trim()                            ''~7430I~
        '       If str.CompareTo(CONST_BOLD) = 0 Then                          ''~7430R~''~7618R~
        If str.CompareTo(CONST_BOLD_JP) = 0 OrElse str.CompareTo(CONST_BOLD_EN) = 0 Then ''~7618R~
            style = FontStyle.Bold
            '       ElseIf str.CompareTo(CONST_ITALIC) = 0 Then                    ''~7430R~''~7618R~
        ElseIf str.CompareTo(CONST_ITALIC_JP) = 0 OrElse str.CompareTo(CONST_ITALIC_EN) = 0 Then ''~7618R~
            style = FontStyle.Italic
        Else
            If (str.IndexOf(CONST_ITALIC_JP) >= 0 AndAlso str.IndexOf(CONST_BOLD_JP) >= 0) _
            OrElse (str.IndexOf(CONST_ITALIC_EN) >= 0 AndAlso str.IndexOf(CONST_BOLD_EN) >= 0) Then
                style = FontStyle.Bold Or FontStyle.Italic             ''~7618I~
            Else                                                       ''~7618I~
                style = FontStyle.Regular                              ''~7618R~
            End If                                                     ''~7618I~
        End If
        Return style
    End Function
    Private Function getNum(Pnumstr As String, ByRef Ppnum As Integer) As Boolean
        Dim val As Integer
        Try
            val = Convert.ToInt32(Pnumstr.Trim())                      ''~7430R~
        Catch ex As Exception
            Return (False)
        End Try
        Ppnum = val
        Return True
    End Function
    Private Sub dialogFont()
        Dim tb As TextBox = TextBoxPrintFontname                       ''~7515R~
        FontDialog1.Font = createFont()
        '       FontDialog1.FixedPitchOnly=true                                ''~7430R~
        If FontDialog1.ShowDialog() = DialogResult.OK Then
            Dim fnt As Font = FontDialog1.Font
            fontFamily = fnt.Name
            '           fontSize = fnt.SizeInPoints                                ''~v101R~
            fontSize = CType(fnt.SizeInPoints, Integer)                 ''~v101I~
            fontStyle = fnt.Style
            tb.Text = createFontname()
        End If
    End Sub
    Private Sub dialogFontScr()                                        ''~7515I~
        Dim tb As TextBox = TextBoxScrFontName                         ''~7515I~
        FontDialog1.Font = createFontScr()                             ''~7515I~
        '       FontDialog1.FixedPitchOnly=true                        ''~7515I~
        If FontDialog1.ShowDialog() = DialogResult.OK Then             ''~7515I~
            Dim fnt As Font = FontDialog1.Font                         ''~7515I~
            fontFamilyScr = fnt.Name                                   ''~7515I~
            '           fontSizeScr = fnt.SizeInPoints                             ''~7515I~''~v101R~
            fontSizeScr = CType(fnt.SizeInPoints, Integer)              ''~v101I~
            fontStyleScr = fnt.Style                                    ''~7515I~
            Dim newfont As String = createFontnameScr()                ''~7515R~
            swFontChangedScr = False                                     ''~7515I~
            '           If newfont.CompareTo(tb.Text) Then                              ''~7515I~''~v101R~
            If newfont.CompareTo(tb.Text) <> 0 Then                      ''~v101I~
                tb.Text = createFontnameScr()                          ''~7515I~
                swFontChangedScr = True                                  ''~7515I~
            End If                                                     ''~7515I~
        End If                                                         ''~7515I~
    End Sub                                                            ''~7515I~
    Public Function createFont() As Font
        Return New Font(fontFamily, Int(fontSize), fontStyle)
    End Function
    Public Function createFontPrint() As Font                          ''~7515I~
        If samePrintFont Then                                               ''~7515I~
            Return createFontScr()                                     ''~7515I~
        End If                                                         ''~7515I~
        Return createFont()                                            ''~7515I~
    End Function                                                       ''~7515I~
    Public Function createFontScr() As Font                            ''~7515I~
        Return New Font(fontFamilyScr, Int(fontSizeScr), fontStyleScr) ''~7515I~
    End Function                                                       ''~7515I~
    Public Function createFontScr(Psize As Integer) As Font            ''~7515I~
        Return New Font(fontFamilyScr, Int(Psize), fontStyleScr)       ''~7515I~
    End Function                                                       ''~7515I~
    Private Sub showHelp()                                             ''~7430I~
        Dim txt As String                                              ''~7430I~
        If swLangEN Then                                                    ''~7613I~
            txt = My.Resources.help_form5E                                  ''~7430I~''~7613R~
        Else                                                           ''~7613I~
            txt = My.Resources.help_form5                              ''~7613I~
        End If                                                         ''~7613I~
        MessageBox.Show(txt, Me.Text)                                           ''~7430I~''~7613R~
    End Sub                                                            ''~7430I~
    '   Private Function setLangEN(PoptLang As Integer) As Boolean         ''~7614I~''~v110R~
    Private Shared Function setLangEN(PoptLang As Integer) As Boolean  ''~v110I~
        Dim sw As Boolean                                              ''~7614I~
        Select Case PoptLang                                           ''~7614I~
            Case ID_LANG_JP                                                ''~7614I~
                sw = False                                                 ''~7614I~
            Case ID_LANG_EN                                                ''~7614I~
                sw = True                                                  ''~7614I~
            Case Else                                                      ''~7614I~
                Dim ci As CultureInfo = CultureInfo.CurrentCulture         ''~7614I~
                sw = ci.Name.CompareTo(Rstr.STR_LANG_JP) <> 0                   ''~7614I~
        End Select                                                     ''~7614I~
        Return sw                                                      ''~7614I~
    End Function                                                       ''~7614I~
    Public Shared Sub setLang(Pform As Control, Ptype As Type)         ''~7613R~
        Dim lang As String = getLangStr()                                ''~7613R~
        setLangAllControl(Pform, Ptype, lang)                           ''~7613R~
    End Sub                                                            ''~7613I~
    Private Shared Sub setLangAllControl(Pform As Control, Ptype As Type, Plang As String) ''~7613R~
        Dim culture As CultureInfo = New CultureInfo(Plang)            ''~7613M~
        '#If True Then                                                               ''~7613R~''~v110R~
#If False Then                                                              ''~v110I~
        Dim rm As System.ComponentModel.ComponentResourceManager = New ComponentResourceManager(Ptype) ''~7613R~
        setLangAllControlSub(rm, Pform, culture)                ''~7613R~
        '       rm.ApplyResources(Pform, Pform.Name, culture)                  ''~7613M~
        '       rm.ApplyResources(Pform, "$this", culture) 'for title          ''~7613R~
#Else                                                                  ''~7613I~
        Thread.CurrentThread.CurrentCulture = culture                     ''~7613I~
        Thread.CurrentThread.CurrentUICulture = culture                 ''~7613R~
#End If                                                                ''~7613I~
    End Sub                                                            ''~7613I~
    Private Shared Sub setLangAllControlSub(Prm As ComponentResourceManager, Pcontrol As Control, Plang As CultureInfo) ''~7613R~
        Dim tb As TextBox                                              ''~7618I~
        Dim swLabel = False                                              ''~7618I~
        If TypeOf Pcontrol Is MenuStrip Then                               ''~7613I~
            setLangMenu(Prm, CType(Pcontrol, MenuStrip), Plang)         ''~7613R~
            Exit Sub                                                   ''~7613I~
        End If                                                          ''~7613I~
        If TypeOf Pcontrol Is ContextMenuStrip Then                    ''~7613M~
            setLangMenu(Prm, CType(Pcontrol, ContextMenuStrip), Plang)  ''~7613M~
            Exit Sub                                                   ''~7613M~
        End If                                                         ''~7613M~
        If TypeOf Pcontrol Is ToolStrip Then                       ''~7613I~
            setLangMenu(Prm, CType(Pcontrol, ToolStrip), Plang)         ''~7613R~
            Exit Sub                                                   ''~7613I~
        End If                                                         ''~7613I~
        If TypeOf Pcontrol Is ListView Then                            ''~7614I~
            setLangListView(Prm, CType(Pcontrol, ListView), Plang)     ''~7614I~
            Exit Sub                                                   ''~7614I~
        End If                                                         ''~7614I~
        If TypeOf Pcontrol Is TextBox Then                             ''~7614I~
            tb = CType(Pcontrol, TextBox)                                 ''~7618I~
            If tb.ForeColor <> SystemColors.InactiveCaptionText Then '*set for localizable textbox such as Special key label''~7618R~
                Exit Sub   'Text is file contents,do not apply resource    ''~7614I~''~7618R~
            End If                                                     ''~7618I~
        End If                                                         ''~7614I~
        If Not IsNothing(Pcontrol.Text) AndAlso Pcontrol.Text.CompareTo("") <> 0 Then ''~7613I~
            If TypeOf Pcontrol Is Form Then                            ''~7613I~
                Prm.ApplyResources(Pcontrol, "$this", Plang)  'for title''~7613I~
            Else                                                       ''~7613I~
                Prm.ApplyResources(Pcontrol, Pcontrol.Name, Plang)     ''~7613R~
            End If                                                     ''~7613I~
        End If                                                         ''~7613I~
        For Each ctl In Pcontrol.Controls                                       ''~7613I~
            '           setLangAllControlSub(Prm, ctl, Plang)                        ''~7613I~''~v101R~
            setLangAllControlSub(Prm, CType(ctl, Control), Plang)       ''~v101I~
        Next ctl                                                       ''~7613I~
    End Sub                                                            ''~7613I~
    Private Shared Sub setLangMenu(Prm As ComponentResourceManager, Pmenu As MenuStrip, Plang As CultureInfo) ''~7613R~
        setLangMenu(Prm, Pmenu.Items, Plang)                             ''~7613I~
    End Sub                                                            ''~7613I~
    Private Shared Sub setLangMenu(Prm As ComponentResourceManager, Pmenu As ToolStrip, Plang As CultureInfo) ''~7613I~
        setLangMenu(Prm, Pmenu.Items, Plang)                             ''~7613I~
    End Sub                                                            ''~7613I~
    Private Shared Sub setLangMenu(Prm As ComponentResourceManager, Pmenu As ContextMenuStrip, Plang As CultureInfo) ''~7613M~
        setLangMenu(Prm, Pmenu.Items, Plang)                             ''~7613I~
    End Sub                                                            ''~7613M~
    Private Shared Sub setLangMenu(Prm As ComponentResourceManager, Pitems As ToolStripItemCollection, Plang As CultureInfo) ''~7613I~
        Dim tsmi As ToolStripMenuItem                                  ''~7613I~
        '       For Each item In Pitems                                        ''~7613I~''~v101R~
        For Each item As ToolStripItem In Pitems                       ''~v101I~
            Prm.ApplyResources(item, item.Name, Plang)                 ''~7613I~
            If TypeOf item Is ToolStripMenuItem Then                   ''~7613I~
                tsmi = CType(item, ToolStripMenuItem)                  ''~7613I~
                setLangMenuDropDown(Prm, tsmi, Plang)                  ''~7613I~
            End If                                                     ''~7613I~
        Next item                                                      ''~7613I~
    End Sub                                                            ''~7613I~
    Private Shared Sub setLangListView(Prm As ComponentResourceManager, Plistview As ListView, Plang As CultureInfo) ''~7614I~
        Dim colhdr As ColumnHeader                                     ''~7614I~
        For Each col In Plistview.Columns                              ''~7614I~
            colhdr = CType(col, ColumnHeader)                           ''~7614I~
            Prm.ApplyResources(colhdr, colhdr.Name, Plang)             ''~7614R~
        Next col                                                       ''~7614I~
    End Sub                                                            ''~7614I~
    Private Shared Sub setLangMenuDropDown(Prm As ComponentResourceManager, Pmenuitem As ToolStripMenuItem, Plang As CultureInfo) ''~7613I~
        '       For Each item In Pmenuitem.DropDownItems                       ''~7613I~''~v101R~
        For Each item As ToolStripItem In Pmenuitem.DropDownItems      ''~v101I~
            Prm.ApplyResources(item, item.Name, Plang)                  ''~7613I~
        Next item                                                      ''~7613I~
    End Sub                                                            ''~7613I~
    Public Shared Sub setLangContextMenu(Pmenu As ContextMenuStrip, Ptype As Type) ''~7613I~
        setLang(CType(Pmenu, Control), Ptype)                            ''~7613I~
    End Sub                                                            ''~7613I~
    Public Shared Function getLangStr() As String                      ''~7613I~
        Dim lang As String                                             ''~7613I~
        If swLangEN Then                                               ''~7613I~
            lang = Rstr.STR_LANG_EN                                         ''~7613I~''~7618R~
        Else                                                           ''~7613I~
            lang = Rstr.STR_LANG_JP                                         ''~7613I~''~7618R~
        End If                                                         ''~7613I~
        Return lang                                                    ''~7613I~
    End Function                                                            ''~7613I~
    Public Shared Sub setLang()                                        ''~7614R~
        Dim lang As String = getLangStr()                                ''~7614I~
        Thread.CurrentThread.CurrentUICulture = New CultureInfo(lang)   ''~7614I~
    End Sub                                                            ''~7614I~

    Private Sub SplitContainer3_Panel1_Paint(sender As System.Object, e As System.Windows.Forms.PaintEventArgs) Handles SplitContainer3.Panel1.Paint

    End Sub

    Private Sub RGLang_Paint(sender As System.Object, e As System.Windows.Forms.PaintEventArgs)

    End Sub

    Private Sub GroupBoxLang_Enter(sender As System.Object, e As System.EventArgs) Handles GroupBoxLang.Enter

    End Sub

    Private Sub Panel2_Paint(sender As System.Object, e As System.Windows.Forms.PaintEventArgs) Handles PanelMiddleLabel.Paint

    End Sub

    Private Sub SplitContainer3_Panel2_Paint(sender As System.Object, e As System.Windows.Forms.PaintEventArgs) Handles SplitContainer3.Panel2.Paint

    End Sub
    '********************************************************************************''~v110I~
    Private Shared Sub setCulture(PswLangEN As Boolean)                           ''~v110I~
        If PswLangEN Then                                                   ''~v110I~
            Dim culture As CultureInfo = New CultureInfo("en-GB")                         ''~v110I~
            Thread.CurrentThread.CurrentCulture = culture              ''~v110I~
            Thread.CurrentThread.CurrentUICulture = culture            ''~v110I~
        End If                                                         ''~v110I~
    End Sub                                                            ''~v110I~
    '********************************************************************************''~v110I~
    Private Sub hideNonDebug()                                         ''~v110I~
#If Not DEBUG Then                                                          ''~v110I~
        GroupBoxLang.visible=False                                     ''~v110I~
        RBLangEN.Visible=False                                         ''~v110I~
        RBLangJP.Visible=False                                         ''~v110I~
        RBLangDefault.Visible=False                                    ''~v110I~
        RBLangEN.Enabled = False                                          ''~v110I~
        RBLangJP.Enabled = False                                          ''~v110I~
        RBLangDefault.Enabled=False                                     ''~v110I~
#End If                                                                ''~v110I~
    End Sub                                                            ''~v110I~
    '********************************************************************************''~v163I~
    Private Sub dialogAddString()                                      ''~v163I~
        Dim dlg As Form6 = New Form6()                                   ''~v163R~
        dlg.showDlg()                                                  ''~v163R~
    End Sub                                                            ''~v163I~
End Class