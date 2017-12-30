'CID:''+v128R~:#72                             update#=  740;         ''~v128R~
'************************************************************************************''~v002I~
'v128 2017/12/30 delete dperecated form(form6,9,10)                    ''~v128I~
'v123 2017/12/29 word/symbol dialog;no change dialog target by shortcut(Ctrl+x),change only by f9,add change button to form''~v123I~
'v122 2017/12/29 show version on form1 title                           ''~v122I~
'v118 2017/12/27 errmsg errmsg for not registered Ctrl+N(send word) on Form1 was not shown when Ctl+n on form1 is first of all''~v118I~
'                swform1 was not set after New() form14 by dlgSymbol=Nothing''~v118I~
'v117 2017/12/27 word dialog from also form1 as single instance        ''~v117I~
'v115 2017/12/26 support dakuon,handakuon key                          ''~v115I~
'v114 2017/12/22 add file menu button to kanji Text  form              ''~v114I~
'v113 2017/12/22 put Zorder Top                                        ''~v113I~
'v112 2017/12/22 add file menu button to Image form                    ''~v112I~
'v110 2017/12/22 StringConstant reset required when lang changed       ''~v110I~
'v105 2017/12/20 display char type not by f4 but automatically by keyup/mouseclick event''~v105I~
'v101 2017/12/16 Conversion warning                                    ''~v101I~
'v076 2017/10/08 Symbol Dialog by DataGridView                         ''~v076I~
'v073 2017/09/27 (Bug)crash when words dialog update, close form3 then replied discard cancel''~v073I~
'v072 2017/09/26 Display doc option at receive text                    ''~v072I~
'v071 2017/09/26 (Bug) clush when send from SpecialCharDialog after form3 closed''~v071I~
'v068 2017/09/26 implement Debug trace                                 ''~v068I~
'v067 2017/09/25 change caret width                                    ''~v067I~
'v061 2017/09/23 Form1 showstatus for INFO_SHOW_TEXT                   ''~v061I~
'v052 2017/09/21 utilize status bar at bottom also on Form1            ''~v052I~
'v032 2017/09/21 English document, i2e was not used                    ''~v032I~
'v015 2017/09/17 saveed evenif not updated                             ''~v015I~
'v012 2017/09/15 Load/Save/SaveAs from/to disctionary file             ''~v012I~
'v011 2017/09/15 (Bug) Exit menu have to chk discard                   ''~v011I~
'v008 2017/09/12 dictionary support                                    ''~v008I~
'v002 2017/09/11 have to confirm when received text remaining          ''~v002I~''~v008R~
'************************************************************************************''~v002I~
Imports System.Globalization                                           ''~7613I~
Imports System.Diagnostics                                              ''~v068I~
Imports System.IO
Imports System.Threading                                               ''~7613I~''~v110I~''~v105I~

Public Class Form1
    Private Const VERSION = "v2.0.2"                                   ''~v122I~''+v128R~
    Private Declare Auto Function CreateCaret Lib "user32.dll" (hWnd As IntPtr, hBitmap As IntPtr, nWidth As Integer, nHeight As Integer) As Boolean ''~v067I~
    Private Declare Auto Function ShowCaret Lib "user32.dll" (hWnd As IntPtr) As Boolean ''~v067I~
    Private caretWidth As Integer = 2                                  ''~v067I~
    Private caretHeight As Integer                                     ''~v067I~
    ''~v067I~
    Public Shared TestOption As Integer = My.Settings.CfgTestOption       ''~7522I~
    Const FONTSIZE_INCREASE = 1                                        ''~7515I~
    ''~7615I~
    Const FILTER_DEFAULT_IMAGE = "bmp"
    Public Const TITLE_SEP = " : "                                      ''~7615I~
    Const INITIAL_SWKATAKANA = False                                     ''~7519I~
    Public Const FILTER_DEFAULT_KANJITEXT = "i2t"                             ''~7412R~''~7513R~''~7521R~
    '   Public Const FILTER_DEFAULT_ENGLISHTEXT = "i2e"                    ''~7619R~''~v032R~
    Public Const FILTER_DEFAULT_KANATEXT = "txt"                       ''~7513I~
    Private Const ERR_CHAR_OUTOFLINE = 1                                ''~7509I~
    Private Const ERR_CRLF_OUTOFLINE = 2                                ''~7509I~
    Private Const ERR_OUTOFTEXT = 3                                     ''~7509I~
    ''~7615I~
#If False Then                                                              ''~7617I~
    '   Private Const MENU_NEWTEXT = "|新規作成|"                          ''~7612R~''~7613R~''~7617M~
    Private MENU_NEWTEXT As String                                     ''~7615I~''~7617M~
    'Public Const MENU_NEWTEXT_FILE = "無題"                                       ''~7412I~''~7509R~''~7612R~''~7615R~''~7617M~
    Public Shared MENU_NEWTEXT_FILE As String                          ''~7615R~''~7617M~
    '   Private Const MENU_NEWFILE = "|新しいファイル|"                    ''~7612R~''~7613R~''~7617M~
    Private MENU_NEWFILE As String                                     ''~7615R~''~7617M~
    '    Const FILTER_IMAGE = "イメージ(*.bmp;*.png;*.gif;*.jpg;*.jpeg;*.tif,*.tiff,)|*.bmp;*.png;*.gif;*.jpg;*.jpeg;*.tif;*.tiff|すべてのファイル(*.*)|*.*"''~7615R~''~7617M~
    Public Shared FILTER_IMAGE As String                               ''~7615I~''~7617M~
    '    Const FILTER_KANJITEXT = "読み取りテキスト(*.i2t)|*.i2t|テキスト(*.txt;*.text)|*.txt;*.text|すべてのファイル(*.*)|*.*" ''~7412R~''~7508R~''~7615R~''~7617M~
    Public Shared FILTER_KANJITEXT As String                           ''~7615I~''~7617M~
    '   Const FILTER_KANATEXT = "かな変換テキスト(*.txt;*.text)|*.txt;*.text|すべてのファイル(*.*)|*.*" ''~7513R~''~7615R~''~7617M~
    Public Shared FILTER_KANATEXT As String                           ''~7615I~''~7617M~
    Private Shared MSG_DISCARD_UPDATE As String   '  "ファイルの変更を破棄しますか？"''~7615I~''~7617M~
    Public Shared FORM1_TITLE_KANA_FILE As String   '  "かなテキスト:" ''~7615R~''~7617M~
    Public Shared FORM1_TITLE_KANJI2KANA_FILE As String   '  "漢字テキスト:"''~7615I~''~7617M~
    Private Shared MSG_ERR_NOT_FOUND As String                         ''~7615R~''~7617M~
    Private Shared MSG_ERR_READ As String                              ''~7615R~''~7617M~
    Private Shared MSG_ERR_WRITE As String                             ''~7615R~''~7617M~
    Private MSG_ERR_PRINT_PAGE As String                               ''~7615I~''~7617M~
    Private MSG_INFO_SHOW_TEXT As String                               ''~7615I~''~7617M~
    Private MSG_NO_TEXT As String    '               "テキストが設定されていません"''~7615R~''~7617M~
    Private Shared MSG_REP_EXISTING As String   '    "ファイルが存在します上書きしますか？"''~7615R~''~7617M~
                                                                       ''~7617I~
#End If                                                                ''~7617I~
    'Options                                                               ''~7412I~
    Public lineWidth As Integer = 32                                 ''~7412R~

    Private bgColor1 As Color = Color.FromArgb(&HFF, &HFF, &HFF)              ''~7415I~''~7421R~
    Private bgColor2 As Color = Color.FromArgb(&HE0, &HE0, &HE0)              ''~7415I~''~7421R~
    Private formWidth As Integer = My.Settings.CfgFormSizeWMain            ''~7411I~''~7421R~
    Private formHeight As Integer = My.Settings.CfgFormSizeHMain           ''~7411I~''~7421R~
    '   Private Debug As Boolean = True                                ''~7421R~''~v068R~
    Private MRUList As New List(Of String)                         ''~7421R~''~7522I~
    '   Private MRU As New ClassMRU()                                  ''~7522I~''~v012R~
    Public MRU As New ClassMRU()                                      ''~v012I~
    Private imageFilename As String = ""                           ''~7421R~
    Private kanjiFilename As String = ""                           ''~7421R~
    Private kanaFilename As String = ""                            ''~7421R~
    Private BESFilename As String = ""                                     ''~7412I~''~7421R~
    Private imageFilterIndex As Integer = 0                        ''~7421R~
    Private kanjiFilterIndex As Integer = 0                        ''~7421R~
    Private kanaFilterIndex As Integer = 0                         ''~7421R~
    Private BESFilterIndex As Integer = 0                                  ''~7412I~''~7421R~
    Public formImage As Form2
    Public Shared formText As Form3                                    ''~7614R~
#If False Then '@                                                      ''~7522I~
    Public formkanaText As Form4
#Else                                                                  ''~7522I~
    Public formkanaText As ClassKanaText                               ''~7522I~
#End If                                                                ''~7522I~
    Private swReceive As Boolean = False                                     ''~7412I~''~7421R~
    Public fmtBES As FormatBES                                        ''~7421I~
    Private swViewKatakana As Boolean = INITIAL_SWKATAKANA         ''~7428I~''~7519R~
    Private printPageText As String                                     ''~7429I~
    Private printFont As Font                                          ''~7429I~
    Private printRange As System.Drawing.Printing.PrintRange                                   ''~7429I~
    Private printPageFrom, printpageTo, printPage As Integer             ''~7429I~
    Public dlgOptions As FormOptions                                  ''~7430I~''~7501R~
    '   Public dlgSpecialKey As Form6                                      ''~7515I~''~v076R~
    '*  Public dlgSpecialKey As Form14 '*keep in Form14                                     ''~v076I~''~v123R~
'*  Public dlgDictionary As Form9                                      ''~v008R~''~v128R~
    Public dlgFind1 As Form8                                            ''~7516I~''~7519R~
    Public dlgFind3 As Form8                                           ''~7519I~
    Public undoRedo As ClassUndoRedo                                  ''~7501I~''~7515R~
    Private posInTheConcatLine As Integer = -1                           ''~7508I~
    Private posInTheSplitLine As Integer = -1                           ''~7508I~
    Private pasteDataLen As Integer = 0                                ''~7509R~
    Private initialTitle, initialText As String                         ''~7519I~
    Public Shared MainForm As Form1                                    ''~7521R~
    Private strResMgr As Rstr                                          ''~7615I~
    Private pendingStatusMsg As String = Nothing                       ''~v052I~
    '   Private swSpecificItem As Boolean = False   'menuitem of Form2       ''~v112I~''~v114R~
    '   Private parmItem As ToolStripMenuItem                               ''~v112I~''~v114R~

    '**************************************************************************************''~v110I~
    Private Sub Form1_Activated(sender As System.Object, e As System.EventArgs) Handles Me.Activated ''~7412I~
        '       If swReceive Then                                                   ''~7412I~''~7516R~
        '       TBBES.DeselectAll()                                     ''~7412I~''~7414R~''~7420R~''~7516R~
        '       End If                                                         ''~7412I~''~7516R~
    End Sub                                                            ''~7412I~
    '**************************************************************************************''~v110I~
    Public Sub New()       'from Main.vb                               ''~v110I~
        setCulture()                                                   ''~v110I~
        InitializeComponent()                                          ''~v110I~
#If DEBUG Then                                                              ''~v117I~
        Trace.swTrace = True                                             ''~v117I~
#End If                                                                ''~v117I~
    End Sub                                                            ''~v110I~
    Private Sub Form1_Load(sender As System.Object, e As System.EventArgs) Handles MyBase.Load
        Try                                                            ''~v111I~
            MainForm = Me                                                    ''~7521I~
            '       Me.Width = formWidth    move to shown event                    ''~7411I~''~7619R~
            '       Me.Height = formHeight                                         ''~7411I~''~7619R~
            FormOptions.SetLangIDInitial()   'before New Rstr(), swLangEN  ''~v110I~
            strResMgr = New Rstr() 'initialize localized resource string getString method''~7615R~
            setupTitlebarIcon(Me)                                          ''~7612I~
            dlgOptions = New FormOptions()                                 ''~7430I~''~7506M~''~7613M~
            setLocale()                                                    ''~7613M~
            loadMRUList()                                                  ''~7522R~
            '       dlgSpecialKey = New Form6()                                    ''~7515R~''~v076R~
            '*          dlgSpecialKey = New Form14()                                   ''~v076I~''~v123R~
            Form14.newForm()                                           ''~v123R~
'*          dlgDictionary = New Form9()     'deprecated to foerm11         ''~v008R~''~v128R~
            fmtBES = New FormatBES()                                         ''~7421I~
            '        swViewKatakana = dlgOptions.swKatakana                            ''~7501I~
            '        setkatakanaBtn()                                               ''~7501I~
            undoRedo = New ClassUndoRedo(ClassUndoRedo.OPT_KANATEXT, TBBES, ToolStripMenuItemUndo, ToolStripMenuItemRedo) ''~7513I~''~7521R~
            '        addContextMenuHandler()                                        ''~7509I~
            TBBES.Font = createFont()                                      ''~7515I~
            '*          initialTitle = Me.Text                                           ''~7519I~''~v122R~
            initialTitle = Me.Text & ":" & VERSION                     ''~v122I~
            Me.Text = initialTitle                                       ''~v122I~
            '       initialText = TBBES.Text                                         ''~7519I~''~7615R~
            TBBES.Text = initialText                                         ''~7615I~
            debugInit()                                                    ''~v068I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 Load", ex)                        ''~v111I~
        End Try                                                        ''~v111I~
    End Sub
    Public Shared Sub setupTitlebarIcon(Pform As Form)                ''~7612R~
        Dim icon = My.Resources.Icon_i2k1                              ''~v101R~
        Pform.Icon = icon                                              ''~7612R~
    End Sub                                                            ''~7612I~
    '    Private Sub contextMenuStrip1_Opening(sender As System.Object, e As System.ComponentModel.CancelEventArgs) Handles ContextMenuStrip1.Opening''~7509R~
    '    End Sub                                                           ''~7509R~
    Private Sub Form1_Shown(sender As System.Object, e As System.EventArgs) Handles Me.Shown ''~7412I~''~7514R~''~7521I~
        Try                                                            ''~v111I~
            '    	TBBES.Focus()                                                  ''~7521R~
            Me.Width = formWidth                                           ''~7619I~
            Me.Height = formHeight                                         ''~7619I~
            showCustomCaret()                                              ''~v067I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 Shown", ex)                       ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7521I~
    Private Sub showCustomCaret()                                      ''~v067I~
        Dim fnt As Font = TBBES.Font                                ''~v067I~
        caretHeight = CInt(fnt.GetHeight())                            ''~v067I~
        CreateCaret(TBBES.Handle, IntPtr.Zero, caretWidth, caretHeight) ''~v067I~
        ShowCaret(TBBES.Handle)                                     ''~v067I~
    End Sub                                                            ''~v067I~
    Private Sub TextBox_GotFocus(sender As System.Object, e As System.EventArgs) Handles TBBES.GotFocus ''~v067I~
        Try                                                            ''~v111I~
            showCustomCaret()                                              ''~v067I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 GotFocus", ex)                    ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~v067I~
    Private Sub Form1_Closing(sender As System.Object, e As System.ComponentModel.CancelEventArgs) Handles MyBase.Closing ''~7501I~
        debugClose()                                                   ''~v076I~
        If Not chkDiscard(e, Me.Text) Then                                          ''~7508R~''~v011R~
            Exit Sub                                                   ''~v011I~
        End If                                                         ''~v011I~
        '       closeForm(dlgFind1)                                            ''~7521I~''~v101R~
        closeForm8(dlgFind1)                                           ''~v101I~
        Trace.fsClose()                                                ''~7619I~
        My.Settings.Save()     'required if using Main.vb              ''~v110I~
    End Sub                                                            ''~7501I~
    Private Sub TBBES_TextChanged(sender As System.Object, e As System.EventArgs) Handles TBBES.TextChanged ''~7501I~
        Try                                                            ''~v111I~
            showStatus("")      'clear                                     ''~v052I~
            If TBBES.Enabled Then    'called by Design.vb initial text setting  ''~7508I~
                TBChanged()                                                    ''~7501I~''~7508R~
                If pendingStatusMsg IsNot Nothing Then                     ''~v052I~
                    showStatus(pendingStatusMsg)                           ''~v052I~
                    pendingStatusMsg = Nothing                             ''~v052I~
                End If                                                     ''~v052I~
            End If                                                         ''~7508I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 TextChanged", ex)                 ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7501I~
    ''~7416I~

    '   Private Sub OpenToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs)''~7509R~
    '   End Sub                                                            ''~7509R~

    Private Sub ImageToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs)
        OpenFileDialog1.Filter = Rstr.FILTER_IMAGE
        OpenFileDialog1.FileName = imageFilename
        OpenFileDialog1.AddExtension = True   'add extension if missing
        OpenFileDialog1.DefaultExt = FILTER_DEFAULT_IMAGE
        OpenFileDialog1.FilterIndex = imageFilterIndex
        If OpenFileDialog1.ShowDialog() = DialogResult.OK Then
            Dim fnm As String = OpenFileDialog1.FileName
            insertMRUList(1, fnm)      '1:imagefile                     ''~7411R~''~7522R~
            Dim basename As String = System.IO.Path.GetFileNameWithoutExtension(fnm)
            imageFilename = basename
            kanjiFilename = basename
            imageFilterIndex = OpenFileDialog1.FilterIndex    'save for next open
            openImageBox(fnm)
        End If
    End Sub
    Private Sub KanjiTextToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs)
        OpenFileDialog1.Filter = Rstr.FILTER_KANJITEXT
        OpenFileDialog1.FilterIndex = 0
        OpenFileDialog1.FileName = imageFilename
        OpenFileDialog1.AddExtension = True   'add extension if missing
        OpenFileDialog1.DefaultExt = FILTER_DEFAULT_KANJITEXT
        OpenFileDialog1.FilterIndex = kanjiFilterIndex
        If OpenFileDialog1.ShowDialog() = DialogResult.OK Then
            Dim fnm As String = OpenFileDialog1.FileName
            insertMRUList(2, fnm)    '2:KanjiTextFile                   ''~7411R~
            Dim basename As String = System.IO.Path.GetFileNameWithoutExtension(fnm)
            kanjiFilename = basename
            kanjiFilterIndex = OpenFileDialog1.FilterIndex    'save for next open
            openTextBox(fnm)
        End If
    End Sub
    Private Sub KanaTextToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs)
        OpenFileDialog1.Filter = Rstr.FILTER_KANATEXT
        OpenFileDialog1.FilterIndex = 0
        OpenFileDialog1.FileName = kanjiFilename
        OpenFileDialog1.AddExtension = True   'add extension if missing
        OpenFileDialog1.DefaultExt = FILTER_DEFAULT_KANATEXT
        OpenFileDialog1.FilterIndex = kanaFilterIndex
        If OpenFileDialog1.ShowDialog() = DialogResult.OK Then
            Dim fnm As String = OpenFileDialog1.FileName
            insertMRUList(3, fnm)          '3:kanaTextFile              ''~7411R~
            Dim basename As String = System.IO.Path.GetFileNameWithoutExtension(fnm)
            kanaFilename = basename
            kanaFilterIndex = OpenFileDialog1.FilterIndex    'save for next open
            openKanaTextBox(fnm)
        End If
    End Sub
    Private Sub ToolStripButtonUndo_Click(ByVal sender As System.Object, ByVal e As EventArgs) Handles ToolStripMenuItemUndo.Click ''~7430R~
        Try                                                            ''~v111I~
            undoText()                                                     ''~7430I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 Undo", ex)                        ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7430I~
    Private Sub ToolStripButtonRedo_Click(ByVal sender As System.Object, ByVal e As EventArgs) Handles ToolStripMenuItemRedo.Click ''~7430R~
        Try                                                            ''~v111I~
            redoText()                                                     ''~7430I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 Redo", ex)                        ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7430I~
    Private Sub ToolStripButtonPrint_Click(ByVal sender As System.Object, ByVal e As EventArgs) Handles ToolStripMenuItemPrint.Click ''~7430I~
        Try                                                            ''~v111I~
            printText()                                                    ''~7430R~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 Print", ex)                       ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7430I~
    Private Sub ToolStripButtonPrintIcon_Click(ByVal sender As System.Object, ByVal e As EventArgs) Handles ToolStripMenuItemPrintIcon.Click ''~7608R~
        Try                                                            ''~v111I~
            printText()                                                    ''~7608I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 Print", ex)                       ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7608I~
    Private Sub ToolStripButtonFontUp_Click(ByVal sender As System.Object, ByVal e As EventArgs)  ''~7515I~
        Try                                                            ''~7515I~
            '           TBBES.Font = New Font(TBBES.Font.FontFamily, Int(TBBES.Font.SizeInPoints + FONTSIZE_INCREASE))''~7515R~
            '           Dim fontSize As Integer = TBBES.Font.SizeInPoints            ''~7515I~''~v101R~
            Dim fontSize As Integer = CType(TBBES.Font.SizeInPoints, Integer) ''~v101I~
            fontSize += FONTSIZE_INCREASE                              ''~7515I~
            '           My.Settings.CfgFontSizeBES = fontSize                      ''~7515R~
            TBBES.Font = createFont(fontSize)                          ''~7515R~
        Catch ex As Exception                                          ''~7515I~
        End Try                                                        ''~7515I~
    End Sub                                                            ''~7515I~
    Private Sub ToolStripButtonFontDown_Click(ByVal sender As System.Object, ByVal e As EventArgs)  ''~7515I~
        Try                                                            ''~7515I~
            '           TBBES.Font = New Font(TBBES.Font.FontFamily, Int(TBBES.Font.SizeInPoints - FONTSIZE_INCREASE))''~7515R~
            '           Dim fontSize As Integer = TBBES.Font.SizeInPoints            ''~7515I~''~v101R~
            Dim fontSize As Integer = CType(TBBES.Font.SizeInPoints, Integer) ''~v101I~
            fontSize -= FONTSIZE_INCREASE                              ''~7515R~
            '           My.Settings.CfgFontSizeBES = fontSize                      ''~7515R~
            TBBES.Font = createFont(fontSize)                          ''~7515R~
        Catch ex As Exception                                          ''~7515I~
        End Try                                                        ''~7515I~
    End Sub                                                            ''~7515I~
    Private Sub ToolStripButtonKata_Click(ByVal sender As System.Object, ByVal e As EventArgs) Handles ToolStripMenuButtonKata.Click ''~7428I~''~7507R~''~7515R~
        Try                                                            ''~v111I~
            chngkatakanaSW(True)                                           ''~7507I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 KataKana", ex)                    ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7428I~
    Private Sub ToolStripButtonHira_Click(ByVal sender As System.Object, ByVal e As EventArgs) Handles ToolStripMenuButtonHira.Click ''~7507I~''~7515R~
        Try                                                            ''~v111I~
            chngkatakanaSW(False)                                          ''~7507I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 HiraGana", ex)                    ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7507I~
    Private Function chkSetText() As Boolean                           ''~7508I~
        If Not TBBES.Enabled Then                                      ''~7508I~
            '           MessageBox.Show("テキストが設定されていません")            ''~7508I~''~7615R~
            MessageBox.Show(Rstr.MSG_NO_TEXT, Me.Text)                       ''~7615R~
            Return False                                               ''~7508I~
        End If                                                         ''~7508I~
        Return True                                                    ''~7508I~
    End Function                                                       ''~7508I~
    Private Sub chngkatakanaSW(Pswkatakana As Boolean)                 ''~7507I~
        If Not chkSetText() Then                                            ''~7508I~
            Exit Sub                                                   ''~7508R~
        End If                                                         ''~7507M~
        Dim swchng As Boolean = swViewKatakana <> Pswkatakana          ''~7507I~
        swViewKatakana = Pswkatakana                                   ''~7507I~
        changeHiraKata(TBBES.Text, swchng, Pswkatakana)              ''~7507I~

    End Sub                                                            ''~7507I~
    Private Sub changeHiraKata(Ptext As String, PswChng As Boolean, Pswkatakana As Boolean) ''~7507I~
        Dim txt = fmtBES.cvHirakataHankakuAll(Pswkatakana, Ptext)      ''~7507I~
        '       setkatakanaBtn()                                               ''~7501M~''~7507R~
        If txt.CompareTo(Ptext) <> 0 Then                                     ''~7507I~
            TBChanged(txt, PswChng)                                    ''~7507R~
        End If                                                         ''~7507I~
    End Sub                                                            ''~7501I~
    Private Sub ToolStripMenuItem_Close(sender As System.Object, e As System.EventArgs) Handles ToolStripMenuItemClose.Click ''~7519R~
        Try                                                            ''~v111I~
            '       closeForm(dlgFind1)                                            ''~7521I~''~v101R~
            closeForm8(dlgFind1)                                           ''~v101I~
            If Not chkDiscard(Nothing, Me.Text) Then                            ''~7519R~
                Exit Sub                                                   ''~7519I~
            End If                                                         ''~7519I~
            TBBES.Enabled = False                                          ''~7519R~
            Me.Text = initialTitle                                           ''~7519I~
            TBBES.Text = initialText                                         ''~7519I~
            swViewKatakana = INITIAL_SWKATAKANA                            ''~7519I~
            undoRedo.resetForm1()                                          ''~7519R~
            swReceive = False                                                ''~v002I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 Close", ex)                       ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7519I~
    Private Sub SaveToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles SaveToolStripMenuItem.Click
        Try                                                            ''~v111I~
            If TBBES.Enabled Then                                               ''~7522I~
                formkanaText.save()                                        ''~7522I~
                swReceive = False                                            ''~v002I~
            End If                                                         ''~7522I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 Save", ex)                        ''~v111I~
        End Try                                                        ''~v111I~
    End Sub
    Private Sub SaveAsToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles SaveAsToolStripMenuItem.Click
        Try                                                            ''~v111I~
            If TBBES.Enabled Then                                               ''~7522I~
                formkanaText.SaveAs()                                      ''~7522R~
                swReceive = False                                            ''~v002I~
            End If                                                         ''~7522I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 SaveAs", ex)                      ''~v111I~
        End Try                                                        ''~v111I~
    End Sub
    Private Sub ToolStripMenuItemSave_Click(sender As System.Object, e As System.EventArgs) Handles ToolStripMenuItemSave.Click ''~7608R~
        Try                                                            ''~v111I~
            If TBBES.Enabled Then                                          ''~7608I~
                formkanaText.save()                                        ''~7608I~
                swReceive = False                                            ''~v002I~
            End If                                                         ''~7608I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 Save", ex)                        ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7608I~
    Private Sub ToolStripMenuItemSaveAs_Click(sender As System.Object, e As System.EventArgs) Handles ToolStripMenuItemSaveAs.Click ''~7608I~
        Try                                                            ''~v111I~
            If TBBES.Enabled Then                                          ''~7608I~
                formkanaText.SaveAs()                                      ''~7608I~
                swReceive = False                                            ''~v002I~
            End If                                                         ''~7608I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 SaveAs", ex)                      ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7608I~

    Private Sub ToolStripMenuItemOptions_Click(sender As System.Object, e As System.EventArgs) Handles ToolStripMenuItemOptions.Click ''~7412I~
        Try                                                            ''~v111I~
            dlgOptions.ShowDialog(Me)  'Modal dialog                       ''~7430I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 Option", ex)                      ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7412I~
    ''~7412I~
    Private Sub ExitToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles ExitXToolStripMenuItem.Click
        If Not chkDiscard(Me.Text) Then 'replyed NO                         ''~v011R~
            Exit Sub                                                   ''~v011I~
        End If                                                         ''~v011I~
        Application.Exit()
    End Sub
    Private Sub HelpToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles ToolStripMenuItemHelp.Click ''~7430I~
        Try                                                            ''~v111I~
            showHelp()                                                     ''~7430I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 Help", ex)                        ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7430I~

    ''~7411I~
    Private Sub Form1_ResizeEnd(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.ResizeEnd ''~7411I~
        '       Dim this As Control = sender                                   ''~7411I~''~v101R~
        Dim this As Control = CType(sender, Control)                    ''~v101I~
        Dim ww As Integer = this.Size.Width                            ''~7411I~
        Dim hh As Integer = this.Size.Height                           ''~7411I~
        My.Settings.CfgFormSizeWMain = ww                              ''~7411I~
        My.Settings.CfgFormSizeHMain = hh                              ''~7411I~
    End Sub                                                            ''~7411I~
    '********************************************************************''~v112R~
    Private Sub setMRUListMenu(Pcase As Integer)                       ''~7411R~
        selectMRUList(Pcase)                                           ''~7411I~''~7522R~
        Dim itemMRU As ToolStripMenuItem = selectMenuItem(Pcase)             ''~7411I~
        setMRUListMenu(Pcase, itemMRU)                                  ''~v112I~
    End Sub                                                            ''~v112I~
    '********************************************************************''~v112I~
    Private Sub setMRUListMenu(PcaseList As Integer, Pcasehandler As Integer, Pitem As ToolStripMenuItem) ''~v112I~
        selectMRUList(PcaseList)                                       ''~v112I~
        setMRUListMenu(Pcasehandler, Pitem)                           ''~v112I~
    End Sub                                                            ''~v112I~
    '********************************************************************''~v112I~
    Private Sub setMRUListMenu(Pcase As Integer, Pitem As ToolStripMenuItem) ''~v112I~
        Dim itemMRU As ToolStripMenuItem = Pitem                       ''~v112I~
        Dim ctr As Integer = MRUList.Count
        '       If ctr = 0 Then                                                       ''~7411I~''~7412R~
        '           Exit Sub                                                   ''~7411I~''~7412R~
        '       End If                                                         ''~7411I~''~7412R~
        itemMRU.DropDownItems.Clear()                                  ''~7411R~
        '       If Pcase <> 1 Then 'image                                      ''~7612R~''~v112R~
        If Pcase <> 1 AndAlso Pcase <> 4 Then 'image                    ''~v112I~
            ctr += 1                                                     ''~7612R~
        End If                                                         ''~7612I~
        For ii As Integer = 0 To ctr                                   ''~7412R~
            If (ii > ClassMRU.MRULISTSZ) Then                          ''~7522R~
                Exit For
            End If
            Dim mruitem As System.Windows.Forms.ToolStripMenuItem
            mruitem = New System.Windows.Forms.ToolStripMenuItem()
            mruitem.Size = New System.Drawing.Size(152, 22)
            '           If Pcase = 1 Then 'image                                          ''~7612I~''~v112R~
            If Pcase = 1 OrElse Pcase = 4 Then 'image 1:Form1,4:Form2    ''~v112I~
                If ii = 0 Then                                               ''~7412I~''~7612R~
                    mruitem.Text = Rstr.MENU_NEWFILE                        ''~7615R~
                Else                                                       ''~7412I~''~7612R~
                    mruitem.Text = MRUList(ii - 1)                           ''~7412R~''~7612R~
                End If                                                     ''~7412I~''~7612R~
            Else                                                       ''~7612I~
                If ii = 0 Then                                         ''~7612I~
                    mruitem.Text = Rstr.MENU_NEWTEXT                        ''~7615R~
                Else                                                   ''~7612I~
                    If ii = 1 Then                                     ''~7612I~
                        mruitem.Text = Rstr.MENU_NEWFILE                    ''~7615R~
                    Else                                               ''~7612I~
                        mruitem.Text = MRUList(ii - 2)                 ''~7612I~
                    End If                                             ''~7612I~
                End If                                                 ''~7612I~
            End If                                                     ''~7612I~
            Select Case Pcase                                          ''~7411I~
                Case 1 'Image                                          ''~7411I~
                    mruitem.Name = "MRUImage"                          ''~7411I~
                    AddHandler mruitem.Click, AddressOf MRUImage_Click ''~7411I~
                Case 2 'KanjiText                                      ''~7411I~
                    mruitem.Name = "MRUText"                           ''~7411I~
                    AddHandler mruitem.Click, AddressOf MRUText_Click  ''~7411I~
                Case 3 'KanaText                                       ''~7411I~
                    mruitem.Name = "MRUKanaText"                       ''~7411I~
                    AddHandler mruitem.Click, AddressOf MRUKanaText_Click ''~7411I~
                Case 4 'Image from Form2                               ''~v112I~
                    mruitem.Name = "MRUImage"                          ''~v112I~
                    AddHandler mruitem.Click, AddressOf MRUImageForm2_Click ''~v112I~
                Case 5 'Image from Form3                               ''~v114I~
                    mruitem.Name = "MRUImage"                          ''~v114I~
                    AddHandler mruitem.Click, AddressOf MRUTextForm3_Click ''~v114I~
            End Select                                                 ''~7411I~
            itemMRU.DropDownItems.Add(mruitem)                         ''~7411R~
        Next
    End Sub
    '**********************************************************************''~v112R~
    Private Sub MRUImage_Click(sender As System.Object, e As System.EventArgs) Handles MRUImage.Click ''~7411R~
        Try                                                            ''~v111I~
            Dim item = DirectCast(sender, ToolStripMenuItem)                ''~7411I~
            Dim fnm As String                                              ''~7411I~
            fnm = item.Text                                                ''~7411I~
            If fnm.CompareTo(Rstr.MENU_NEWFILE) = 0 Then                          ''~7412R~''~7613R~''~7615R~
                ImageToolStripMenuItem_Click(sender, e)                     ''~7412I~
            Else                                                           ''~7412I~
                insertMRUList(1, fnm)      '1:imagefile                        ''~7411I~''~7412R~
                openImageBox(fnm)                                              ''~7411R~''~7412R~
            End If                                                         ''~7412I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 MRU_Image", ex)                   ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7411I~
    '**********************************************************************''~v112I~
    Private Sub MRUImageForm2_Click(sender As System.Object, e As System.EventArgs) Handles MRUImage.Click ''~v112I~
        Try                                                            ''~v112I~
            Dim item = DirectCast(sender, ToolStripMenuItem)           ''~v112I~
            Dim fnm As String                                          ''~v112I~
            fnm = item.Text                                            ''~v112I~
            If fnm.CompareTo(Rstr.MENU_NEWFILE) = 0 Then               ''~v112I~
                ImageToolStripMenuItem_Click(sender, e)                ''~v112I~
            Else                                                       ''~v112I~
                insertMRUList(1, fnm)      '1:imagefile                ''~v112I~
                openImageBoxForm2(fnm)                                 ''~v112I~
            End If                                                     ''~v112I~
            formImage.updateMRUList()                                  ''~v112I~
        Catch ex As Exception                                          ''~v112I~
            Form1.exceptionMsg("Form1 MRU_Image for Form2", ex)        ''~v112I~
        End Try                                                        ''~v112I~
    End Sub                                                            ''~v112I~
    '**********************************************************************''~v112I~
    Private Sub MRUText_Click(sender As System.Object, e As System.EventArgs) Handles MRUText.Click ''~7411R~
        Try                                                            ''~v111I~
            Dim item = DirectCast(sender, ToolStripMenuItem)                ''~7411I~
            Dim fnm As String                                              ''~7411I~
            fnm = item.Text                                                ''~7411I~
            If fnm.CompareTo(Rstr.MENU_NEWTEXT) = 0 Then            ''~7613R~   ''~7615R~
                openTextBox("")                                            ''~7612R~
            ElseIf fnm.CompareTo(Rstr.MENU_NEWFILE) = 0 Then                    ''~7612R~''~7613R~''~7615R~
                KanjiTextToolStripMenuItem_Click(sender, e)                ''~7612I~
            Else                                                           ''~7612I~
                insertMRUList(2, fnm)      '2:kanji text                   ''~7426R~
                openTextBox(fnm)                                               ''~7411I~''~7412R~
            End If                                                         ''~7412I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 MRU_Text", ex)                    ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7411I~
    '**********************************************************************''~v114I~
    Private Sub MRUTextForm3_Click(sender As System.Object, e As System.EventArgs) Handles MRUText.Click ''~v114I~
        Try                                                            ''~v114I~
            Dim item = DirectCast(sender, ToolStripMenuItem)           ''~v114I~
            Dim fnm As String                                          ''~v114I~
            fnm = item.Text                                            ''~v114I~
            If fnm.CompareTo(Rstr.MENU_NEWTEXT) = 0 Then               ''~v114I~
                openTextBox("")                                        ''~v114I~
            ElseIf fnm.CompareTo(Rstr.MENU_NEWFILE) = 0 Then           ''~v114I~
                KanjiTextToolStripMenuItem_Click(sender, e)            ''~v114I~
            Else                                                       ''~v114I~
                insertMRUList(2, fnm)      '2:kanji text               ''~v114I~
                openTextBoxForm3(fnm)                                  ''~v114I~
            End If                                                     ''~v114I~
            formText.updateMRUList()                                   ''~v114I~
        Catch ex As Exception                                          ''~v114I~
            Form1.exceptionMsg("Form MRU_Text for Form3", ex)          ''~v114I~
        End Try                                                        ''~v114I~
    End Sub                                                            ''~v114I~
    Private Sub MRUKanaText_Click(sender As System.Object, e As System.EventArgs) Handles MRUKanaText.Click ''~7411R~
        Try                                                            ''~v111I~
            Dim item = DirectCast(sender, ToolStripMenuItem)                ''~7411I~
            Dim fnm As String                                              ''~7411I~
            fnm = item.Text                                                ''~7411I~
            If fnm.CompareTo(Rstr.MENU_NEWTEXT) = 0 Then                        ''~7615R~
                openKanaTextBox("")                                        ''~7612I~
            ElseIf fnm.CompareTo(Rstr.MENU_NEWFILE) = 0 Then                    ''~7612R~''~7613R~''~7615R~
                KanaTextToolStripMenuItem_Click(sender, e)                 ''~7612I~
            Else                                                           ''~7612I~
                insertMRUList(3, fnm)      '3:kana text                    ''~7426R~
                openKanaTextBox(fnm)                                           ''~7411I~''~7412R~
            End If                                                         ''~7412I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 MRU_KanaText", ex)                ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7411I~

    Private Sub selectMRUList(Pcase As Integer)                        ''~7522R~
        MRUList = MRU.selectMRUList(Pcase)                               ''~7522I~
    End Sub                                                            ''~7522R~
    Private Function selectMenuItem(Pcase As Integer) As ToolStripMenuItem ''~7411I~
        Dim item As ToolStripMenuItem                                      ''~7411I~
        Select Case Pcase                                              ''~7411I~
            Case 1 'Image                                              ''~7411I~
                item = ImageToolStripMenuItem                            ''~7411I~
            Case 2 'KanjiText                                          ''~7411I~
                item = KanjiTextToolStripMenuItem                        ''~7411I~
            Case 3 'KanaText                                           ''~7411I~
                item = KanaTextToolStripMenuItem
            Case Else
                item = Nothing
        End Select                                                     ''~7411I~
        Return item                                                    ''~7411I~
    End Function                                                            ''~7411I~
    ''~7411I~
    Public Sub insertMRUList(Pcase As Integer, Pfnm As String)        ''~7411I~''~7429R~''~7522R~
        MRU.insertMRUList(Pcase, Pfnm)      '                           ''~7522M~
        setMRUListMenu(Pcase)                                          ''~7411I~
        saveMRUList(Pcase)                                             ''~7411I~
    End Sub                                                            ''~7411I~

    Private Sub loadMRUList()                                          ''~7522R~
        loadMRUListSub(1)                                              ''~7522I~
        loadMRUListSub(2)                                              ''~7522I~
        loadMRUListSub(3)                                              ''~7522I~
    End Sub
    Private Sub setMRUListMenu()                                       ''~7617I~
        setMRUListMenu(1)                                              ''~7617I~
        setMRUListMenu(2)                                              ''~7617I~
        setMRUListMenu(3)                                              ''~7617I~
    End Sub                                                            ''~7617I~
    ''~7411I~
    Private Sub loadMRUListSub(Pcase As Integer)                       ''~7522R~
        MRU.loadMRUListSub(Pcase)                                      ''~7522R~
        setMRUListMenu(Pcase)                                          ''~7522I~
    End Sub                                                            ''~7522I~
    ''~7522R~
    Private Sub saveMRUList(Pcase As Integer)                          ''~7411R~
        MRU.saveMRUList(Pcase)                                         ''~7522I~
    End Sub
    '*************************************************************
    Private Sub openImageBox(Pfnm As String)
        '       If formImage Is Nothing OrElse formImage.IsDisposed Then       ''~v113R~
        '           formImage = New Form2()                                    ''~v113R~
        '       End If                                                         ''~v113R~
        Try
            Dim swNew As Boolean = newForm(formImage, formImage)          ''~v113R~
            If Not formImage.setImage(Pfnm) Then                            ''~7428R~
                Return                                                 ''~7428I~
            End If                                                     ''~7428I~
            '           formImage.Show()                                           ''~v113R~
            showForm(formImage, swNew)                                  ''~v113I~
            '       	formImage.Dispose()
        Catch ex As Exception
            ReadError(Pfnm, ex)                                   ''~7428R~
        End Try
    End Sub
    '*************************************************************     ''~v112I~
    Private Sub openImageBoxForm2(Pfnm As String)                      ''~v112I~
        '       If formImage Is Nothing OrElse formImage.IsDisposed Then       ''~v112I~
        '           formImage = New Form2()                                    ''~v112I~
        '       End If                                                         ''~v112I~
        Try                                                            ''~v112I~
            If Not formImage.setImage(Pfnm) Then                       ''~v112I~
                Return                                                 ''~v112I~
            End If                                                     ''~v112I~
            formImage.Show()                                           ''~v112I~
            '       	formImage.Dispose()                            ''~v112I~
        Catch ex As Exception                                          ''~v112I~
            ReadError(Pfnm, ex)                                        ''~v112I~
        End Try                                                        ''~v112I~
    End Sub                                                            ''~v112I~
    '*************************************************************
    Private Sub openTextBox(Pfnm As String)
        If formText Is Nothing OrElse formText.IsDisposed Then
            formText = New Form3()
        Else                                                           ''~7508I~
            If Not formText.chkDiscard() Then                               ''~7508I~
                Exit Sub                                               ''~7508I~
            End If                                                     ''~7508I~
        End If
        Try
            formText.setText(Pfnm)
            formText.Show() 'Moderess
        Catch ex As Exception
            ReadError(Pfnm, ex)                                   ''~7428I~
        End Try
    End Sub
    '*************************************************************     ''~v114I~
    Private Sub openTextBoxForm3(Pfnm As String)                       ''~v114I~
        Try                                                            ''~v114I~
            '       If formText Is Nothing OrElse formText.IsDisposed Then         ''~v114I~
            '           formText = New Form3()                                     ''~v114I~
            '       Else                                                           ''~v114I~
            If Not formText.chkDiscard() Then                          ''~v114I~
                Exit Sub                                               ''~v114I~
            End If                                                     ''~v114I~
            '       End If                                                         ''~v114I~
            formText.setText(Pfnm)                                     ''~v114I~
            formText.Show() 'Moderess                                  ''~v114I~
        Catch ex As Exception                                          ''~v114I~
            ReadError(Pfnm, ex)                                        ''~v114I~
        End Try                                                        ''~v114I~
    End Sub                                                            ''~v114I~
    '*************************************************************
    Private Sub openKanaTextBox(Pfnm As String)
        If formkanaText Is Nothing Then                                     ''~7522I~
            '           formkanaText = New ClassKanaText                           ''~7522I~''~v032R~
            formkanaText = New ClassKanaText()                         ''~v032I~
        Else                                                           ''~7508I~
            If Not formkanaText.chkDiscard() Then                           ''~7508R~
                Exit Sub                                               ''~7508I~
            End If                                                     ''~7508I~
        End If
        Try
            formkanaText.readText(Pfnm)
        Catch ex As Exception
            ReadError(Pfnm, ex)                                   ''~7428I~
        End Try
    End Sub
    Public Shared Function createFont() As Font                              ''~7412I~''~7515R~''~7521R~
        '       Return New Font(fontFamily, Int(fontSize), fontStyle)          ''~7412I~''~7416R~''~7515R~
        Return MainForm.dlgOptions.createFontScr()                              ''~7515I~''~7521R~
    End Function                                                       ''~7412I~
    Public Shared Function createFont(Psize As Integer) As Font               ''~7515I~''~7521R~
        Return MainForm.dlgOptions.createFontScr(Psize)                         ''~7515I~''~7521R~
    End Function                                                       ''~7515I~
    Private Sub getFontSize()                                          ''~7416I~
    End Sub                                                            ''~7416I~
    '*************************************************************     ''~7412I~
    Public Sub receiveText(Ptext As String, Ptitle As String, Pswsource As Integer) ''~7514I~
        If Not chkDiscard(Nothing, Me.Text) Then                             ''~7508R~
            Exit Sub                                                   ''~7508I~
        End If                                                         ''~7508I~
        If Pswsource = 1 Then                                                 ''~7514I~
            swReceive = True                                                 ''~7513I~''~7514R~
        Else                                                           ''~7514I~
            swReceive = False                                          ''~7514I~
        End If                                                         ''~7514I~
        showText(Ptext)                                                ''~7412I~
        '        Dim title As String = changeExt(Ptitle, FILTER_DEFAULT_KANATEXT) ''~7513I~''~7521R~
        Dim title As String = Ptitle                                    ''~7521I~
        Me.Text = title                                                ''~7513R~
        If swReceive Then                                                  ''~7514I~
            '           MessageBox.Show(Me.Text & " を表示します")                 ''~7514R~''~7615R~
            '           MessageBox.Show(Me.Text, Rstr.MSG_INFO_SHOW_TEXT)                ''~7615R~''~v061R~
            '           showStatus(Rstr.MSG_INFO_SHOW_TEXT)                        ''~v061I~''~v072R~
            showStatus(Rstr.MSG_INFO_SHOW_TEXT & " : " & DocOptions.getDocOptions()) ''~v072I~
        End If                                                         ''~7514I~
        Me.Activate()                                                  ''~7514I~
    End Sub                                                            ''~7412I~
    Private Sub showText(Ptext As String)                              ''~7412I~
        'initial set text                                                  ''~7501I~
        '       undoRedo = New ClassUndoRedo(ClassUndoRedo.OPT_KANATEXT, TBBES, ToolStripMenuItemUndo, ToolStripMenuItemRedo) ''~7513I~''~7521R~
        TBBES.Font = createFont()                                   ''~7412I~''~7414R~''~7420R~
        getFontSize()                                                  ''~7416I~
        Dim txt As String = Ptext                                      ''~7513I~
        undoRedo.showText(txt)   'after clear                                      ''~7501R~''~7502R~
        TBBES.Enabled = True                                             ''~7508I~''~7521M~
        TBBES.Focus()                                                  ''~7521I~
        TBBES.Select(0, 0)                                              ''~7521R~
    End Sub                                                            ''~7412I~
    Public Shared Sub ReadError(Pfnm As String, Pex As Exception)              ''~7428I~''~7521R~
        '       MessageBox.Show("ファイル（" & Pfnm & "）が読めません:" & Pex.Message) ''~7428R~''~7615R~
        MessageBox.Show(Pfnm & " : " & Pex.Message, Rstr.MSG_ERR_READ)      ''~7615I~
    End Sub                                                            ''~7428I~
    Public Shared Sub WriteError(Pfnm As String, Pex As Exception)             ''~7428I~''~7521R~
        '       MessageBox.Show("ファイル(" & Pfnm & ") への出力エラー:" & Pex.Message) ''~7428I~''~7615R~
        MessageBox.Show(Pfnm & " : " & Pex.Message, Rstr.MSG_ERR_WRITE)     ''~7615I~
    End Sub                                                            ''~7428I~
    Public Shared Sub NotFound(Pfnm As String)                                ''~7428I~''~7521R~
        '       MessageBox.Show("ファイル（" & Pfnm & "）がみつかりません")    ''~7428I~''~7615R~
        MessageBox.Show(Pfnm, Rstr.MSG_ERR_NOT_FOUND)                       ''~7615I~
    End Sub                                                            ''~7428I~
    Public Shared Sub FileSaved(Pfnm As String)                        ''~7617I~
        '       MessageBox.Show(Pfnm, Rstr.MSG_INFO_SAVED)                     ''~7617I~''~v052R~
        Form1.MainForm.showStatus(Rstr.MSG_INFO_SAVED & ":" & Pfnm)   ''~v052R~
    End Sub                                                            ''~7617I~
    Public Shared Sub FileNotSaved()                                   ''~v015I~
        MessageBox.Show(Rstr.getStr("STR_MSG_INFO_NOT_SAVED_BY_NOUPDATE")) ''~v015R~
    End Sub                                                            ''~v015I~
    Public Function chkDiscard(Ptitle As String) As Boolean            ''~v011I~
        ' rc:true=continue process                                     ''~v011I~
        Dim rc As Boolean = True                                       ''~v011I~
        If Not IsNothing(undoRedo) AndAlso undoRedo.isUpdated() Then   ''~v011I~
            rc = confirmDiscard(Ptitle)                                ''~v011I~
        Else                                                           ''~v011I~
            If swReceive Then                                          ''~v011I~
                rc = confirmDiscard(Ptitle)                            ''~v011I~
            End If                                                     ''~v011I~
        End If                                                         ''~v011I~
        Return rc                                                      ''~v011I~
    End Function                                                       ''~v011I~
    Public Function chkDiscard(e As System.ComponentModel.CancelEventArgs, Ptitle As String) As Boolean ''~7508R~
        ' rc:true=continue process                                     ''~7508I~
        Dim rc As Boolean = True                                         ''~7508I~
        If Not IsNothing(undoRedo) AndAlso undoRedo.isUpdated() Then                                   ''~7508I~''~7521R~
            rc = confirmDiscard(e, Ptitle)                                ''~7508R~
        Else                                                           ''~v002I~
            If swReceive Then                                               ''~v002I~
                rc = confirmDiscard(e, Ptitle)                         ''~v002I~
            End If                                                     ''~v002I~
        End If                                                         ''~7508I~
        Return rc                                                      ''~7508I~
    End Function                                                       ''~7508I~
    Public Shared Function confirmDiscard(Ptitle As String) As Boolean ''~v011I~
        ' rc:true=continue close                                       ''~v011I~
        If MessageBox.Show(Ptitle, Rstr.MSG_DISCARD_UPDATE, MessageBoxButtons.YesNo) = DialogResult.No Then ''~v011I~
            Return False                                               ''~v011I~
        End If                                                         ''~v011I~
        Return True                                                    ''~v011I~
    End Function                                                       ''~v011I~
    Public Shared Function confirmDiscard(e As System.ComponentModel.CancelEventArgs, Ptitle As String) As Boolean ''~7429I~''~7508R~''~7521R~
        ' rc:true=continue close                                           ''~7429I~
        '       If MessageBox.Show(Ptitle, "ファイルの変更を破棄しますか？", MessageBoxButtons.YesNo) = DialogResult.No Then ''~7429I~''~7508R~''~7615R~
        If MessageBox.Show(Ptitle, Rstr.MSG_DISCARD_UPDATE, MessageBoxButtons.YesNo) = DialogResult.No Then ''~7615I~
            If Not IsNothing(e) Then                                        ''~7508I~
                e.Cancel = True                                              ''~7429I~''~7508R~
            End If                                                     ''~7508I~
            Return False                                               ''~7429I~
        End If                                                         ''~7429I~
        Return True                                                    ''~7429I~
    End Function                                                       ''~7429I~
    Public Shared Function confirmDiscard(e As System.EventArgs, Ptitle As String) As Boolean ''~7521I~
        ' rc:true=continue process                                     ''~7521I~
        '       If MessageBox.Show(Ptitle, "ファイルの変更を破棄しますか？", MessageBoxButtons.YesNo) = DialogResult.No Then''~7615I~
        If MessageBox.Show(Ptitle, Rstr.MSG_DISCARD_UPDATE, MessageBoxButtons.YesNo) = DialogResult.No Then ''~7521I~''~7615R~
            Return False                                               ''~7521I~
        End If                                                         ''~7521I~
        Return True                                                    ''~7521I~
    End Function                                                       ''~7521I~
    Public Shared Function changeExt(Pfnm As String, Pext As String) As String ''~7513I~''~7521R~
        Dim idx As Integer = Pfnm.LastIndexOf("."c)                    ''~7513I~
        Dim fnm As String                                              ''~7513I~
        If idx >= 0 Then                                               ''~7513I~
            fnm = Pfnm.Substring(0, idx + 1) & Pext                    ''~7513I~
        Else                                                           ''~7513I~
            fnm = Pfnm & "."c & Pext                                   ''~7513I~
        End If                                                         ''~7513I~
        Return fnm                                                     ''~7513I~
    End Function                                                       ''~7513I~
    Public Shared Function chkExt(Pfnm As String, Pext As String) As Boolean ''~7619I~
        Dim pos1, pos2 As Integer                                        ''~7619I~
        Dim rc As Boolean = False                                        ''~7619I~
        pos1 = Pfnm.LastIndexOf("."c)                                   ''~7619I~
        pos2 = Pfnm.LastIndexOf(Pext)                                   ''~7619I~
        rc = (pos2 > 0) AndAlso (pos2 = pos1 + 1)                              ''~7619I~
        Return rc                                                      ''~7619I~
    End Function                                                       ''~7619I~
    Public Shared Function confirmReceivedSave(Pfnm As String) As Boolean     ''~7513R~''~7521R~
        'rc:false :reply:No                                                ''~7513I~
        If Not System.IO.File.Exists(Pfnm) Then                        ''~7513R~
            Return True                                                ''~7513R~
        End If                                                         ''~7513I~
        '       If MessageBox.Show(Pfnm, "ファイルが存在します上書きしますか？", MessageBoxButtons.YesNo) = DialogResult.No Then ''~7513I~''~7615R~
        If MessageBox.Show(Pfnm, Rstr.MSG_REP_EXISTING, MessageBoxButtons.YesNo) = DialogResult.No Then ''~7615I~
            Return False                                               ''~7513R~
        End If                                                         ''~7513I~
        Return True                                                    ''~7513R~
    End Function                                                       ''~7513I~
    Public Shared Function confirmNewText(Pfnm As String) As Boolean   ''~7612I~
        'rc:false :reply:No                                            ''~7612I~
        If Not System.IO.File.Exists(Pfnm) Then                        ''~7612I~
            Return True                                                ''~7612I~
        End If                                                         ''~7612I~
        '       If MessageBox.Show(Pfnm, "ファイルが存在します上書きしますか？", MessageBoxButtons.YesNo) = DialogResult.No Then ''~7612I~''~7615R~
        If MessageBox.Show(Pfnm, Rstr.MSG_REP_EXISTING, MessageBoxButtons.YesNo) = DialogResult.No Then ''~7615I~
            Return False                                               ''~7612I~
        End If                                                         ''~7612I~
        Return True                                                    ''~7612I~
    End Function                                                       ''~7612I~
    ''~7429I~
    Public Sub printDoc(Pdocname As String, Ptext As String) ''~7429I~ ''~7430R~
        Dim pd As System.Drawing.Printing.PrintDocument             ''~7429I~
        printPageText = Ptext                                            ''~7429I~
        '       printFont = dlgOptions.createFontPrint()                            ''~7430I~''~7515R~
        printFont = dlgOptions.createFontPrint()                       ''~7515I~
        pd = New System.Drawing.Printing.PrintDocument()           ''~7429I~
        pd.DocumentName = Pdocname                                 ''~7429I~
        AddHandler pd.PrintPage, AddressOf printTextHandler       ''~7429I~''~7618R~
        Dim dlg As New PrintDialog                                     ''~7429I~
        dlg.Document = pd                                                ''~7429I~
        dlg.AllowSomePages = True                                  ''~7429I~
        If dlg.ShowDialog() = DialogResult.OK Then                            ''~7429I~
            If dlg.PrinterSettings.PrintRange = System.Drawing.Printing.PrintRange.SomePages Then ''~7429R~
                printPageFrom = dlg.PrinterSettings.FromPage             ''~7429I~
                printpageTo = dlg.PrinterSettings.ToPage                 ''~7429I~
            Else                                                       ''~7429I~
                printPageFrom = 0                                      ''~7429R~
            End If                                                     ''~7429I~
            printPage = 0                                                ''~7429I~
            pd.Print()                                                 ''~7429I~
        End If                                                         ''~7429I~
    End Sub                                                            ''~7429I~
    Private Sub printTextHandler(ByVal sender As Object, ByVal e As System.Drawing.Printing.PrintPageEventArgs) ''~7429R~''~7618R~
        Dim chctrPage, linectrPage As Integer                           ''~7429I~
        chctrPage = 0                                                    ''~7429I~
        printPage += 1                                                 ''~7429I~
        If printPageFrom > 0 Then                                      ''~7429R~
            While printPage < printPageFrom                            ''~7429M~
                e.Graphics.MeasureString(printPageText, printFont, e.MarginBounds.Size, StringFormat.GenericTypographic, chctrPage, linectrPage) ''~7429M~
                printPageText = printPageText.Substring(chctrPage)     ''~7429M~
                If printPageText.Length <= 0 Then                      ''~7429M~
                    e.Cancel = True                                      ''~7429R~
                    '                   MessageBox.Show("印刷は " & printPage & " ページが最後です") ''~7429I~''~7615R~
                    MessageBox.Show(printPage & Rstr.MSG_ERR_PRINT_PAGE)    ''~7615I~
                    Exit Sub                                           ''~7429M~
                End If                                                 ''~7429M~
                printPage += 1
            End While ''~7429M~
        End If                                                         ''~7429I~
        e.Graphics.MeasureString(printPageText, printFont, e.MarginBounds.Size, StringFormat.GenericTypographic, chctrPage, linectrPage) ''~7429R~
        e.Graphics.DrawString(printPageText, printFont, Brushes.Black, e.MarginBounds, StringFormat.GenericTypographic) ''~7429R~
        printPageText = printPageText.Substring(chctrPage)             ''~7429R~
        If printPage = printpageTo Then                                ''~7429R~
            e.HasMorePages = False                                     ''~7429R~
        Else                                                           ''~7429R~
            e.HasMorePages = printPageText.Length > 0                  ''~7429R~
        End If                                                         ''~7429R~
    End Sub                                                            ''~7429I~
    Private Sub showHelp()                                             ''~7430I~
        Dim txt As String                                              ''~7430I~
        If FormOptions.swLangEN Then                                   ''~7613R~
            txt = My.Resources.help_form1E                             ''~7613I~
        Else                                                           ''~7613I~
            txt = My.Resources.help_form1U8                                  ''~7430I~''~7501R~''~7613R~
        End If                                                         ''~7613I~
        '       MessageBox.Show(txt, initialTitle)                                      ''~7430I~''~7613R~''~7615R~''~7621R~
        MsgBox.ShowMsg(txt, initialTitle)                              ''~7621I~
    End Sub                                                            ''~7430I~

    Private Sub MenuStrip1_ItemClicked(sender As System.Object, e As System.Windows.Forms.ToolStripItemClickedEventArgs) Handles MenuStrip1.ItemClicked

    End Sub
    Private Sub printText()                                            ''~7430R~
        printDoc(Me.Text, TBBES.Text)                                  ''~7430R~
    End Sub                                                            ''~7430I~
    Private Sub TBChanged()                                            ''~7501I~
        undoRedo.TBChanged()                                           ''~7501I~
    End Sub                                                            ''~7416I~''~7501M~
    Private Sub TBChanged(Ptext As String)                             ''~7501I~
        undoRedo.TBChanged(Ptext)                                      ''~7501I~
    End Sub                                                            ''~7501I~
    Private Sub TBChanged(Ptext As String, Pswchnghirakata As Boolean) ''~7507I~
        undoRedo.TBChanged(Ptext, Pswchnghirakata) 'reverse swViewKatakana when undo''~7507I~
    End Sub                                                            ''~7507I~
    Private Sub redoText()                                             ''~7430I~''~7501I~
        undoRedo.redoText()                                           ''~7430I~''~7501I~
    End Sub                                                            ''~7430I~''~7501M~
    Private Sub undoText()                                             ''~7430I~''~7501M~
        undoRedo.undoText()                                            ''~7430I~''~7501I~
    End Sub                                                            ''~7430I~''~7501M~
    Public Sub optionChanged()                                         ''~7501R~
        If dlgOptions.swFontChangedScr Then                                 ''~7515I~
            TBBES.Font = createFont()                                  ''~7515I~
        End If                                                         ''~7515I~
        If formText Is Nothing OrElse formText.IsDisposed Then         ''~7515I~
        Else                                                           ''~7515I~
            formText.optionChanged()                                   ''~7515I~
        End If                                                         ''~7515I~
    End Sub                                                            ''~7501I~
    Private Sub CMCut_Click(sender As System.Object, e As System.EventArgs) Handles CMCut.Click ''~7514R~
        Try                                                            ''~v111I~
            undoRedo.CMCut(0)    'del crlf with SIGN_CRLF is last          ''~7514R~
            TBBES.Cut()                                                    ''~7509I~
            undoRedo.CMCut(1)    'del crlf with SIGN_CRLF is last          ''~7514I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 Cut", ex)                         ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7509I~
    Private Sub CMCopy_Click(sender As System.Object, e As System.EventArgs) Handles CMCopy.Click ''~7514R~
        Try                                                            ''~v111I~
            TBBES.Copy()                                                   ''~7509I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 Copy", ex)                        ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7509I~
    Private Sub CMPaste_Click(sender As System.Object, e As System.EventArgs) Handles CMPaste.Click ''~7514R~
        Try                                                            ''~v111I~
            TBBES.Paste()                                         ''~7509R~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 Paste", ex)                       ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7509I~
    Private Sub CMFind_Click(sender As System.Object, e As System.EventArgs) Handles CMFind.Click ''~7519R~
        Try                                                            ''~v111I~
            showFindDialog()                                               ''~7519I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 Find", ex)                        ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7519I~
    Private Sub CMSelectAll_Click(sender As System.Object, e As System.EventArgs) Handles CMSelectAll.Click ''~7514R~
        Try                                                            ''~v111I~
            TBBES.SelectAll()                                              ''~7509I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 SelectAll", ex)                   ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7509I~
    Private Sub TB_KeyUpEvent(sender As System.Object, e As System.Windows.Forms.KeyEventArgs) Handles TBBES.KeyUp ''~7513I~
        Try                                                            ''~v111I~
            undoRedo.TB_KeyUp(e)                                           ''~7513I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 KeyUp", ex)                       ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7513I~
    Private Sub TB_KeyDownEvent(sender As System.Object, e As System.Windows.Forms.KeyEventArgs) Handles TBBES.KeyDown ''~7513I~
        Try                                                            ''~v111I~
            undoRedo.TB_KeyDown(e)                                         ''~7513I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 KeyDown", ex)                     ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7513I~
    Private Sub TB_KeyPressEvent(sender As System.Object, e As System.Windows.Forms.KeyPressEventArgs) Handles TBBES.KeyPress ''~7513I~
        Try                                                            ''~v111I~
            undoRedo.TB_KeyPress(e)                                        ''~7513I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 KeyPress", ex)                    ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7513I~
    Private Sub TB_MouseClickEvent(sender As System.Object, e As System.Windows.Forms.MouseEventArgs) Handles TBBES.MouseClick ''~v105M~
        Try                                                            ''~v111I~
            undoRedo.TB_MouseClick(e)                                      ''~v105M~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 MouseClick", ex)                  ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~v105M~
    Public Sub showSpecialKeyDialog()                                  ''~7515I~
        '       dlgSpecialKey.showForForm1(Me)                                 ''~7515R~''~v123R~
        Form14.dlgSymbol.showForForm1(Me)                              ''~v123R~
    End Sub                                                            ''~7515I~
    Private Sub ToolStripMenuItemSpecialChar_Click(sender As System.Object, e As System.EventArgs) Handles ToolStripMenuItemSpecialChar.Click ''~7515I~
        Try                                                            ''~v111I~
            showSpecialKeyDialog()                                       ''~7515I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 SpecialChar", ex)                 ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7515I~
    Private Sub ToolStripMenuItemWords_Click(sender As System.Object, e As System.EventArgs) Handles ToolStripMenuItemWords.Click ''~v117I~
        Try                                                            ''~v117I~
            '       Form13.sharedShowDlg()                             ''~v117I~
            Form13.sharedShowDlg(True)      '*True:Form1               ''~v117I~
        Catch ex As Exception                                          ''~v117I~
            Form1.exceptionMsg("Form1 MenuWords", ex)                  ''~v117I~
        End Try                                                        ''~v117I~
    End Sub                                                            ''~v117I~
    Public Function showDlgSpecialKey(Pswform1 As Boolean, Pform As Form) As Boolean ''~7523I~
        '*      If dlgSpecialKey.IsDisposed Then                                    ''~7523I~''~v123R~
        If Form14.dlgSymbol.IsDisposed Then                            ''~v123R~
            '           dlgSpecialKey = New Form6()                                ''~7523I~''~v076R~
            '*          dlgSpecialKey = New Form14()                               ''~v076I~''~v123R~
            Form14.newForm()                                           ''~v123I~
            If Pswform1 Then                                                ''~7523I~
                Dim frm = DirectCast(Pform, Form1)                       ''~7523I~
                '*              dlgSpecialKey.showForForm1(frm)                        ''~7523I~''~v123R~
                Form14.dlgSymbol.showForForm1(frm)                     ''~v123R~
            Else                                                       ''~7523I~
                Dim frm = DirectCast(Pform, Form3)                       ''~7523I~
                '*              dlgSpecialKey.showForForm3(frm)                        ''~7523I~''~v123R~
                Form14.dlgSymbol.showForForm3(frm)                     ''~v123R~
            End If                                                     ''~7523I~
            Return True 'instance re-created                           ''~7523I~
        End If                                                         ''~7523I~
        Return False                                                   ''~7523I~
    End Function                                                            ''~7523I~
    '   Public Function getSpecialStr(Pindex As Integer)                   ''~7525I~''~v101R~
    '*  Public Function getSpecialStr(Pindex As Integer) As String         ''~v101I~''~v118R~
    '***************************************************************************''~v123I~
    Public Function getSpecialStr(Pindex As Integer, PswForm1 As Boolean) As String ''~v118I~
        '       If dlgSpecialKey.IsDisposed Then                               ''~7525I~''~v123R~
        If Form14.dlgSymbol.IsDisposed Then                            ''~v123R~
            '           dlgSpecialKey = New Form6()                                ''~7525I~''~v076R~
            '*          dlgSpecialKey = New Form14()                               ''~v076I~''~v123R~
            Form14.newForm()                                           ''~v123R~
            '*      End If                                                         ''~7525I~''~v123R~
            Dim frm As Form                                                ''~v118I~
            If PswForm1 Then                                               ''~v118I~
                frm = CType(MainForm, Form)                                ''~v118I~
            Else                                                           ''~v118I~
                frm = CType(formText, Form)                                ''~v118I~
            End If                                                         ''~v118I~
            '*      dlgSpecialKey.setParent(PswForm1, frm)                          ''~v118I~''~v123R~
            Form14.dlgSymbol.setParent(PswForm1, frm)                      ''~v123R~
        End If                                                         ''~v123I~
        '       Return dlgSpecialKey.getSpecialStr(Pindex)                     ''~7525I~''~v123R~
        '*      Return dlgSpecialKey.getSpecialStr(Pindex,PswForm1)            ''~v123R~
        Return Form14.dlgSymbol.getSpecialStr(Pindex, PswForm1)         ''~v123R~
    End Function                                                       ''~7525I~
    '***********************************************************       ''~v101I~
    '   Private Function newDlgFind(ByRef Ppform As Form) As Boolean       ''~7521R~''~v101R~
    Private Function newDlgFind(ByRef Ppform As Form8) As Boolean      ''~v101I~
        If Ppform Is Nothing OrElse Ppform.IsDisposed Then             ''~7521I~
            Ppform = New Form8()                                       ''~7521I~
            Return True                                                ''~7521I~
        End If                                                         ''~7521I~
        Return False                                                   ''~7521I~
    End Function                                                            ''~7516I~
    '***********************************************************       ''~v101I~
    Public Shared Function closeForm8(ByRef Ppform As Form8) As Boolean ''~v101I~
        Dim frm As Form = Ppform                                         ''~v101I~
        Dim rc = closeForm(frm)                                          ''~v101I~
        Ppform = CType(frm, Form8)                                        ''~v101R~
        Return rc                                                      ''~v101I~
    End Function                                                       ''~v101I~
    Public Shared Function closeForm13(ByRef Ppform As Form13) As Boolean ''~v101I~
        Dim frm As Form = Ppform                                         ''~v101I~
        Dim rc = closeForm(frm)                                          ''~v101I~
        Ppform = CType(frm, Form13)                                       ''~v101I~
        Return rc                                                      ''~v101I~
    End Function                                                       ''~v101I~
    '***********************************************************       ''~v101I~
    '   Public Shared Sub closeForm(ByRef Ppform As Form)                  ''~v073R~
    Public Shared Function closeForm(ByRef Ppform As Form) As Boolean  ''~v073I~
        '** rc true:closed or already closed, false close canceled         ''~v073I~
        If Ppform Is Nothing OrElse Ppform.IsDisposed Then             ''~7521I~
            '           Exit Sub                                                   ''~7521I~''~v073R~
            Return True                                                ''~v073I~
        End If                                                         ''~7521I~
        Ppform.Close()                                                 ''~7521I~
        If Ppform.DialogResult = DialogResult.No Then                         ''~v073I~
            Return False                                               ''~v073I~
        End If                                                         ''~v073R~
        Ppform = Nothing                                                 ''~7521I~
        Return True                                                    ''~v073I~
        '   End Sub                                                       ''~7521I~''~v073R~
    End Function                                                       ''~v073I~
    '***********************************************************       ''~v101I~
    Public Shared Function closeForm14(ByRef Ppform As Form14, Pclear As Boolean) As Boolean ''~v101I~
        Dim frm As Form = Ppform                                         ''~v101I~
        Dim rc = closeForm(frm, Pclear)                                   ''~v101I~
        Ppform = CType(frm, Form14)                                       ''~v101I~
        Return rc                                                      ''~v101I~
    End Function                                                       ''~v101I~
    '***********************************************************       ''~v101I~
    '   Public Shared Sub closeForm(ByRef Ppform As Form, Pclear As Boolean) ''~v071I~''~v073R~
    Public Shared Function closeForm(ByRef Ppform As Form, Pclear As Boolean) As Boolean ''~v073I~
        '** rc true:closed, false close canceled                           ''~v073I~
        Dim rc As Boolean                                                  ''~v073I~
        If Pclear Then                                                      ''~v071I~
            '           closeForm(Ppform)                                          ''~v071I~''~v073R~
            rc = closeForm(Ppform)                                       ''~v073I~
        Else                                                           ''~v071I~
            Dim tmp As Form = Ppform  ' avoid set Nothing                ''~v071I~
            '           closeForm(tmp)                                             ''~v071I~''~v073R~
            rc = closeForm(tmp)                                          ''~v073I~
        End If                                                         ''~v071I~
        Return rc                                                      ''~v073I~
        '   End Sub                                                            ''~v071I~''~v073R~
    End Function                                                       ''~v073I~
    Public Sub showFindDialog()                                        ''~7521I~
        newDlgFind(dlgFind1)                                           ''~7521I~
        dlgFind1.showForForm1(Me)                                      ''~7521I~
    End Sub                                                            ''~7521I~
    Public Sub showForForm3(Pform3 As Form3)                                    ''~7519R~''~7521R~
        newDlgFind(dlgFind3)                                           ''~7521I~
        dlgFind3.showForForm3(Pform3)                                  ''~7519R~
    End Sub                                                            ''~7519I~
    Public Sub findNext(PswUp As Boolean)       ''~7519I~              ''~7521R~
        If newDlgFind(dlgFind1) Then                                        ''~7521R~
            dlgFind1.showForForm1(Me)                                  ''~7521I~
        Else                                                           ''~7521I~
            dlgFind1.FindNext(PswUp)                                   ''~7521R~
        End If                                                         ''~7521I~
    End Sub
    Public Sub findNext(Pform3 As Form3, PswUp As Boolean)              ''~7521I~
        If newDlgFind(dlgFind3) Then   'alrweady dlg opened                 ''~7521R~
            dlgFind3.showForForm3(Pform3)                              ''~7521I~
        Else                                                           ''~7521I~
            dlgFind3.FindNext(PswUp)                                   ''~7521I~
        End If                                                         ''~7521I~
    End Sub                                                            ''~7521I~
#If False Then                                                         ''~7521I~
    Private Sub TBGotFocus(sender As System.Object, e As System.EventArgs) Handles TBBES.GotFocus''~7521I~
    End Sub                                                            ''~7521I~
    Private Sub TBLostFocus(sender As System.Object, e As System.EventArgs) Handles TBBES.LostFocus''~7521I~
    End Sub                                                            ''~7521I~
    Private Sub TBLeave(sender As System.Object, e As System.EventArgs) Handles TBBES.Leave''~7521I~
    End Sub                                                            ''~7521I~
    Private Sub TVEnter(sender As System.Object, e As System.EventArgs) Handles TBBES.Enter''~7521I~
    End Sub                                                            ''~7521I~
#End If                                                                ''~7521I~
    Private Sub setLocale()                                            ''~7613I~
        Dim title As String = Me.Text                                    ''~7615I~
        FormOptions.setLang(Me, GetType(Form1))                              ''~7613I~
        setLocaleConst(title)                                          ''~7615R~
    End Sub                                                            ''~7613I~
    Public Shared Sub langChanged()                                    ''~7613I~
        MainForm.setLocale()                                                    ''~7613I~
        MainForm.setMRUListMenu()                                               ''~7617I~
        MainForm.Refresh()      'textbox was cleared                            ''~7613I~''~7614R~
        If MainForm.formImage Is Nothing OrElse MainForm.formImage.IsDisposed Then       ''~7619I~
        Else                                                           ''~7619I~
            MainForm.formImage.setLocale(True)    'refresh                      ''~7619I~
        End If                                                         ''~7619I~
        If formText Is Nothing OrElse formText.IsDisposed Then         ''~7614I~
        Else                                                           ''~7614I~
            formText.setLocale(True)    'refresh                       ''~7614I~
        End If                                                         ''~7614I~
    End Sub                                                            ''~7613I~

    Private Sub SaveFileDialog1_FileOk(sender As System.Object, e As System.ComponentModel.CancelEventArgs) Handles SaveFileDialog1.FileOk

    End Sub

    Private Sub ContextMenuSpecialChar_Opening(sender As System.Object, e As System.ComponentModel.CancelEventArgs) Handles ContextMenuSpecialChar.Opening
        Try                                                            ''~v111I~
            Dim menu As ContextMenuStrip = CType(sender, ContextMenuStrip)    ''~7613I~
            FormOptions.setLangContextMenu(menu, GetType(Form1))           ''~7613R~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form1 SpecialChar", ex)                 ''~v111I~
        End Try                                                        ''~v111I~
    End Sub
#If False Then                                                              ''~7615I~
    Private Function getLocaleStr(Pcase As Integer) As String          ''~7613R~
        Dim str As String = ""                                           ''~7613I~
        '       My.Application.ChangeUICulture(FormOptions.getLangStr())       ''~7613R~
        Dim culture As CultureInfo = New CultureInfo(Formoptions.getLangStr()) ''~7613I~
        My.Resources.Culture = culture                                   ''~7613I~
        Select Case Pcase                                              ''~7613I~
            Case 1                                                         ''~7613I~
                str = My.Resources.STR_NEWTEXT                                 ''~7613I~
            Case 2                                                         ''~7613I~
                str = My.Resources.STR_NEWFILE                             ''~7613I~
        End Select                                                     ''~7613I~
        Return str                                                     ''~7613I~
    End Function                                                       ''~7613I~
    Private Function getLocaleStr(Pstrid As String) As String          ''~7615I~
        Return RStr.getStr(Pstrid)                                     ''~7615I~
    End Function                                                       ''~7615I~
#End If                                                                ''~7617I~
    Private Sub setLocaleConst(Ptitle As String)                       ''~7615R~
#If False Then                                                         ''~7617I~
        MENU_NEWTEXT = getLocaleStr(Rstr.STRID_NEWTEXT)                  ''~7615I~
        MENU_NEWTEXT_FILE = getLocaleStr(Rstr.STRID_NEWTEXT_FILE)       ''~7615R~
        MENU_NEWFILE = getLocaleStr(Rstr.STRID_NEWFILE)                ''~7615I~
        FILTER_IMAGE = getLocaleStr(Rstr.STRID_FILTER_IMAGE)           ''~7615I~
        FILTER_KANJITEXT = getLocaleStr(Rstr.STRID_FILTER_KANJITEXT)   ''~7615I~
        FILTER_KANATEXT = getLocaleStr(Rstr.STRID_FILTER_KANATEXT)     ''~7615I~
        MSG_NO_TEXT = getLocaleStr(Rstr.STRID_MSG_NO_TEXT)              ''~7615I~
        MSG_REP_EXISTING = getLocaleStr(Rstr.STRID_MSG_REP_EXISTING)   ''~7615I~
        MSG_DISCARD_UPDATE = getLocaleStr(Rstr.STRID_MSG_DISCARD_UPDATE) ''~7615I~
        FORM1_TITLE_KANA_FILE = getLocaleStr(Rstr.STRID_FORM1_TITLE_KANA_FILE) ''~7615R~
        FORM1_TITLE_KANJI2KANA_FILE = getLocaleStr(Rstr.STRID_FORM1_TITLE_KANJI2KANA_FILE) ''~7615I~
        MSG_ERR_NOT_FOUND = getLocaleStr(Rstr.STRID_MSG_ERR_NOT_FOUND) ''~7615I~
        MSG_ERR_READ = getLocaleStr(Rstr.STRID_MSG_ERR_READ)           ''~7615I~
        MSG_ERR_WRITE = getLocaleStr(Rstr.STRID_MSG_ERR_WRITE)         ''~7615I~
        MSG_ERR_PRINT_PAGE = getLocaleStr(Rstr.STRID_MSG_ERR_PRINT_PAGE) ''~7615I~
        MSG_INFO_SHOW_TEXT = getLocaleStr(Rstr.STRID_MSG_INFO_SHOW_TEXT) ''~7615I~
#Else                                                                  ''~7617I~
        Rstr.setupStrings() 'already called at Rstr.New()              ''~7617I~''
#End If                                                                 ''~7617I~
        initialText = Rstr.FORM1_INITIAL_TEXT      ''~7615I~           ''~7617R~
        ''~7615I~
        Dim pos As Integer = Ptitle.IndexOf(TITLE_SEP)                      ''~7615I~
        If pos > 0 Then                                                       ''~7615I~
            If swReceive Then                                               ''~7615I~
                Me.Text = Rstr.FORM1_TITLE_KANJI2KANA_FILE & Ptitle.Substring(pos) ''~7615R~
            Else                                                       ''~7615I~
                Me.Text = Rstr.FORM1_TITLE_KANA_FILE & Ptitle.Substring(pos) ''~7615I~
            End If                                                     ''~7615I~
        End If                                                         ''~7615I~
        If Not TBBES.Enabled Then                                      ''~7615I~
            TBBES.Text = initialText                                        ''~7615I~
        End If                                                         ''~7615I~
    End Sub                                                            ''~7615I~

    Private Sub OpenFileDialog1_FileOk(sender As System.Object, e As System.ComponentModel.CancelEventArgs) Handles OpenFileDialog1.FileOk

    End Sub
    ''~v052I~
    Public Sub showStatus(Pmsg As String)                              ''~v052I~
        ToolStripStatusLabel1.Text = Pmsg                              ''~v052I~
    End Sub                                                            ''~v052I~
    Public Sub showStatus(PswLater As Boolean, Pmsg As String)         ''~v052I~
        If (PswLater) Then                                             ''~v052I~
            pendingStatusMsg = Pmsg                                    ''~v052I~
        Else                                                           ''~v052I~
            showStatus(Pmsg)                                           ''~v052I~
        End If                                                         ''~v052I~
    End Sub                                                            ''~v052I~
    Public Sub showStatus(Pch As Char, Pchcv As Char, PtypeSrc As Integer, PtypeTgt As Integer) ''~v052I~
        Dim msg, strSrc, strTgt As String                              ''~v052I~
        strSrc = FormatBES.getCharType(PtypeSrc)                       ''~v052I~
        strTgt = FormatBES.getCharType(PtypeTgt)                       ''~v052I~
        msg = Rstr.getStr("STR_MSG_CHANGELETTERWRAP")                  ''~v052I~
        pendingStatusMsg = String.Format(msg, strSrc, Pch, strTgt, Pchcv) ''~v052I~
    End Sub                                                            ''~v052I~
    Public Sub showStatus(Pch As Char, PtypeSrc As Integer)            ''~v052I~
        '** from class1 * F4(queryKey) target info                     ''~v052I~
        Dim msg, strSrc As String                                      ''~v052I~
        strSrc = FormatBES.getCharType(PtypeSrc)                       ''~v052I~
        msg = Rstr.getStr("STR_MSG_CHANGELETTERWRAP_QUERY")            ''~v052I~
        msg = String.Format(msg, strSrc, Pch)                          ''~v052I~
        showStatus(msg)     'imediately put msg                        ''~v052I~
    End Sub                                                            ''~v052I~
    Public shared Sub showStatusForChild(PswForm1 as Boolean,Pmsg as String)''~v115I~
        '** showStatus for Form1 or form3                              ''~v115I~
        If PswForm1 Then                                               ''~v115I~
            MainForm.showStatus(Pmsg)    '*Form1                       ''~v115I~
        Else                                                           ''~v115I~
            formText.showStatus(Pmsg)    '*Form3                       ''~v115I~
        End If                                                         ''~v115I~
    End Sub                                                            ''~v115I~
    Private Sub debugInit()                                            ''~v068I~
#If DEBUG Then                                                              ''~v068I~
        Debug.Listeners.Remove("Default")                              ''~v068I~
        '       Dim lstener as TextWriterTraceListener(System.IO.File.createText("Debug.txt"))''~v068I~
        Dim stream As New StreamWriter("Debug.txt")                    ''~v068I~
        stream.AutoFlush = True                                          ''~v068I~
        Dim writer As TextWriter = TextWriter.Synchronized(stream)   ''~v068I~
        Dim listener As New TextWriterTraceListener(writer)                 ''~v068I~
        Debug.Listeners.Add(listener)                                  ''~v068I~
#End If                                                                ''~v068I~
    End Sub                                                            ''~v068I~
    Private Sub debugClose()                                           ''~v076I~
#If DEBUG Then                                                              ''~v076I~
        Debug.Flush()                                                  ''~v076I~
        Debug.Close()                                                  ''~v076I~
#End If                                                                ''~v076I~
    End Sub                                                            ''~v076I~
    '*************************************************************************''~v111R~
    Public Shared Sub exceptionMsg(Pfunction As String, Pex As Exception) ''~v111R~
        MessageBox.Show(Pex.Message & vbCrLf & Pex.StackTrace, "Exception at " & Pfunction) ''~v111R~
    End Sub                                                            ''~v111R~
    '*************************************************************     ''~v110I~
    Public Sub setCulture()                                            ''~v110I~
        FormOptions.SetLangIDInitial() 'set swLangEN and currentCulture      ''~v110R~
    End Sub                                                            ''~v110I~
    '*************************************************************     ''~v112I~
    Public Sub loadMRUListForm2(Pitem As ToolStripMenuItem)            ''~v112R~
        '* From Fom2 at form load                                          ''~v112R~
        setMRUListMenu(1, 4, Pitem)                                      ''~v112I~
    End Sub                                                            ''~v112I~
    '*************************************************************     ''~v114I~
    Public Sub loadMRUListForm3(Pitem As ToolStripMenuItem)            ''~v114I~
        '* From Fom3 at form load                                      ''~v114I~
        setMRUListMenu(2, 5, Pitem)                                    ''~v114I~
    End Sub                                                            ''~v114I~
    '**********************************************************************''~v114I~
    Public Sub isrtMRUListSaveImage(Pfnm as String)                    ''~v114I~
    	insertMRUList(1,Pfnm)                                          ''~v114I~
        formImage.updateMRUList()   'To form2                          ''~v114I~
    End Sub                                                            ''~v114I~
    '*************************************************************     ''~v112I~
    Public Shared Sub showTop(Pform As Form)                           ''~v113I~
        Pform.BringToFront()     'ZorderTop                            ''~v113I~
    End Sub                                                            ''~v113I~
    '*************************************************************     ''~v113I~
    Public Shared Function newForm(Of T As {New, Form})(Pform As T, ByRef PpnewForm As T) As Boolean ''~v113R~
        '* create form,return swNew                                        ''~v113R~
        Dim swNew As Boolean = False                                     ''~v113I~
        Dim frm As Form = CType(Pform, Form)                           ''~v113R~
        Dim frmT As T = Pform                                          ''~v113R~
        If frm Is Nothing OrElse frm.IsDisposed Then                   ''~v113R~
            swNew = True                                                 ''~v113I~
            frmT = New T()                                             ''~v113R~
        End If                                                         ''~v113I~
        PpnewForm = frmT                                                 ''~v113I~
        Return swNew                                                   ''~v113R~
    End Function                                                          ''~v113I~
    '*************************************************************     ''~v113I~
    Public Shared Sub showForm(Of T As {Form})(Pform As T, PswNew As Boolean) ''~v113I~
        '* newrly show or showtop                                      ''~v113I~
        Dim frm As Form = CType(Pform, Form)                           ''~v113I~
        frm.Show()                                                     ''~v113I~
        If Not PswNew Then                                                  ''~v113I~
            showTop(frm)                                               ''~v113I~
        End If                                                         ''~v113I~
    End Sub                                                            ''~v113I~
End Class
