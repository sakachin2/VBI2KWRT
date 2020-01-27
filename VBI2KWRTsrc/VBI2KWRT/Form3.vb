'CID:''+v181R~:#72                             update#=  406;        ''~v181R~
'************************************************************************************''~v006I~
'v181 2020/01/26 ReplaceKey:default F2                                 ''~v181I~
'v179 2020/01/22 move LetterReplacement from Form5(setting) to From1/Fom3 Menu''~v179I~
'v175 2018/09/13 (Bug)"OK" on partial extracted form, scroll to top page of Form3''~v175I~
'v171 2018/03/16 Do Paste by Ctrl+V if not registered as wordsRep key  ''~v171I~
'v168 2018/03/05 paste from partial by mouse, new SelectionStart position is short, consider CRLF sign''~v168I~
'v167 2018/03/04 partial text send;target reverse length is short by CRLF sign''~v167I~
'v165 2018/03/04 show caret even if focus lost by selectionStart/Length''~v165I~
'v162 2018/02/26 set filter for savefiledialog of i2k and txt          ''~v162I~
'v123 2017/12/29 word/symbol dialog;no change dialog target by shortcut(Ctrl+x),change only by f9,add change button to form''~v123I~
'v114 2017/12/22 add file menu button to kanji Text  form              ''~v114I~
'v113 2017/12/22 put Zorder Top                                        ''~v113I~
'v111 2017/12/22 embed handler by try-catch                            ''~v111I~
'v106 2017/12/20 partially extract from image(box by mouse dragging)   ''~v106I~
'v105 2017/12/20 display char type not by f4 but automatically by keyup/mouseclick event''~v105I~
'v101 2017/12/16 Conversion warning                                    ''~v101I~
'v100 2017/12/15 porting from MOD to Microsoft Ocr Library for Windows ''~v100I~
'v073 2017/09/27 (Bug)crash when words dialog update, close form3 then replied discard cancel''~v073I~
'v071 2017/09/26 (Bug) clush when send from SpecialCharDialog after form3 closed''~v071I~
'v067 2017/09/25 change caret width                                    ''~v067I~
'v065 2017/09/24 Word dialog by Ctrl+char(except "1"-"0")              ''~v065I~
'v064 2017/09/24 (Bug) Doc option is not initialy applied              ''~v064I~
'v063 2017/09/24 support kanji file encoded by UTF8                    ''~v063I~
'v050 2017/09/23 kanji form,saved msg to status bar                    ''~v050I~
'v037 2017/09/22 assign F4 as query of replacing char                  ''~v037I~
'                Forecolor have to be InactiveCaptureText to get effective for switching Text by language''~v037I~
'v035 2017/09/21 additional to v017 when discard old=yes               ''~v035I~
'v034 2017/09/21 utilize status bar at bottom for result of F5 on Form5''~v034I~
'v032 2017/09/21 English document, i2e was not used                    ''~v032I~
'v030 2017/09/21 new option dialog for each document                   ''~v030I~
'v017 2017/09/17 Image2kanji issues already processed msg when saved even updated,it dose not allow re-i2k''~v017I~
'v015 2017/09/17 saveed evenif not updated                             ''~v015I~
'v008 2017/09/12 dictionary support                                    ''~v008I~
'v007 2017/09/12 duplicated confirm discard msg                        ''~v007I~
'v006 2017/09/12 katakana okurigana is optional                        ''~v006I~
'************************************************************************************''~v006I~
'Imports System.Runtime.InteropServices                                 ''~7411I~''~7610R~
Public Class Form3
    '**image 2 kanji by MODI                                               ''~7619I~
    'localized                                                             ''~7617I~
    '*** kanji text file                                                   ''~7612I~
    ''~v067I~
    Private Declare Auto Function CreateCaret Lib "user32.dll" (hWnd As IntPtr, hBitmap As IntPtr, nWidth As Integer, nHeight As Integer) As Boolean ''~v067I~
    Private Declare Auto Function ShowCaret Lib "user32.dll" (hWnd As IntPtr) As Boolean ''~v067I~
    Private caretWidth As Integer = 2                                    ''~v067R~
    Private caretHeight As Integer                                     ''~v067M~
    ''~v067I~
    Private Const FONTSIZE_INCREASE = 1                                          ''~7411R~''~7412R~''~7429M~''~7614R~
    '   Private TITLE_READ = "テキストファイル"                            ''~7614I~''~7617R~
    Private STR_PUNCT As String = " 　,、｡。?？"                           ''~7428I~''~7430R~
    Private imageFilename As String = ""                               ''~7430R~
    Private textFilename As String = ""                                ''~7430R~
    Private imageTextFilename As String = ""                           ''~7513I~
    '   Private saveFilename = ""                                          ''~7607I~''~v101R~
    Private saveFilename As String = ""                                ''~v101I~
    Private formWidth As Integer = My.Settings.CfgFormSizeWText             ''~7411I~''~7430R~
    Private formHeight As Integer = My.Settings.CfgFormSizeHText            ''~7411I~''~7430R~
    Private fel As Object                                                  ''~7411I~''~7430R~
    Private swSource As Integer = 0                                          ''~7411I~''~7430R~
    Private swNewText As Boolean = False                                 ''~7612I~
    Private swRead As Boolean = False                                    ''~7612I~
    Public undoRedo As ClassUndoRedo                                  ''~7430R~''~7515R~
    Public Shared swEnglishDoc As Boolean = False                         ''~v032R~
#If False Then                                                              ''~v100M~
    Private Shared nestCtr As Integer = 0                                 ''~7619I~
#End If                                                                ''~v100I~
    '   Public Shared swKatakanaOkurigana As Boolean = False                 ''~v006R~''~v030R~
    Public swSaved As Boolean = False                                  ''~v015I~
    Private pendingStatusMsg As String = Nothing                              ''~v034I~
    Private TBF As TBFocus                                             ''~v165I~

    Sub New()                                                          ''~v064I~
        InitializeComponent()                                          ''~v064I~
        DocOptions.initByCFG()  ' load English doc option etc          ''~v064I~
    End Sub                                                            ''~v064I~
    Private Sub Form3_Load(sender As System.Object, e As System.EventArgs) Handles MyBase.Load ''~7412I~''~7514R~
        Try                                                            ''~v111I~
            '       ToolStripButtonUndo.Enabled = False                              ''~7429I~''~v114R~
            '       ToolStripButtonRedo.Enabled = False                              ''~7429I~''~v114R~
            ToolStripMenuItemUndo.Enabled = False                          ''~v114I~
            ToolStripMenuItemRedo.Enabled = False                          ''~v114I~
            Form1.setupTitlebarIcon(Me)                                    ''~7612I~
            Dim title As String                                               ''~7614I~
            title = Me.Text    'save before update by lang                 ''~7614R~
            setLocale(False)   'change title for extracted                 ''~7614R~
            updateTitle(title)                                             ''~7614I~
            '       DocOptions.initByCFG()  ' load English doc option etc          ''~v032I~''~v064R~
            loadMRUList()                                              ''~v114I~
            TBF = New TBFocus(TextBox1)                                  ''~v165I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 Load", ex)                        ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7412I~
    Private Sub Form3_Shown(sender As System.Object, e As System.EventArgs) Handles Me.Shown ''~7412I~''~7514R~
        Try                                                            ''~v111I~
            Me.Width = formWidth                                           ''~7619I~
            Me.Height = formHeight                                         ''~7619I~
            showCustomCaret()                                              ''~v067R~
            ''~v067I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 Shown", ex)                       ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7412I~
    Private Sub showCustomCaret()                                      ''~v067I~
        Dim fnt As Font = TextBox1.Font                                  ''~v067I~
        caretHeight = CInt(fnt.GetHeight())                              ''~v067I~
        CreateCaret(TextBox1.Handle, IntPtr.Zero, caretWidth, caretHeight) ''~v067I~
        ShowCaret(TextBox1.Handle)                                     ''~v067I~
    End Sub                                                            ''~v067I~
    Private Sub TextBox_GotFocus(sender As System.Object, e As System.EventArgs) Handles TextBox1.GotFocus ''~v067I~
        Try                                                            ''~v111I~
            showCustomCaret()                                              ''~v067I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 TextBoxGetFocus", ex)             ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~v067I~
    Private Sub Form3_Activated(sender As System.Object, e As System.EventArgs) Handles Me.Activated ''~7514I~
        '        TextBox1.DeSelectAll()                                         ''~7514I~''~7519R~
    End Sub                                                            ''~7514I~
    Private Sub TextBox_Changed(sender As System.Object, e As System.EventArgs) Handles TextBox1.TextChanged ''~7429I~
        Try                                                            ''~v111I~
            showStatus("")      'clear                                     ''~v034I~
            TBChanged()                                                  ''~7429I~
            If pendingStatusMsg IsNot Nothing Then                             ''~v034I~
                showStatus(pendingStatusMsg)                               ''~v034I~
                pendingStatusMsg = Nothing                                   ''~v034I~
            End If                                                         ''~v034I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 TextBoxChanged", ex)              ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7429I~
    Private Sub Form3_Closing(sender As System.Object, e As System.ComponentModel.CancelEventArgs) Handles MyBase.Closing ''~7429I~
        Try                                                            ''~v111I~
            chkDiscard(e)                                                  ''~7508I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 Closing", ex)                     ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7429I~
    Private Sub ContextMenu_Opening(sender As System.Object, e As System.ComponentModel.CancelEventArgs) Handles ContextMenuWordSelection.Opening, ContextMenuWordSelection.Opening ''~7614I~
        Try                                                            ''~v111I~
            '       swKatakanaOkurigana = My.Settings.CFGF3_KatakanaOkurigana      ''~v006I~''~v030R~
            Dim menu As ContextMenuStrip = CType(sender, ContextMenuStrip) ''~7614I~
            FormOptions.setLangContextMenu(menu, GetType(Form3))           ''~7614I~
            '       Me.ToolStripMenuItem_DocOptions.Checked = swKatakanaOkurigana ''~v006R~''~v030R~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 ContextMenuOpen", ex)             ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7614I~
    Public Function chkDiscard(e As System.ComponentModel.CancelEventArgs) As Boolean ''~7508R~
        ' rc:true=continue process                                     ''~7508I~
        '       If Not Form1.closeForm(Form13.dlgWord) Then 'canceled               ''~v073I~''~v101R~
        If Not Form1.closeForm13(Form13.dlgWord) Then 'canceled        ''~v101I~
            If e IsNot Nothing Then                                         ''~v073I~
                e.Cancel = True                                          ''~v073I~
            End If                                                     ''~v073I~
            Return True   ' continue process                           ''~v073I~
        End If                                                         ''~v073I~
        '       Form1.closeForm(Form1.dlgFind3)                                ''~7521I~''~v101R~
        Form1.closeForm8(Form1.dlgFind3)                               ''~v101I~
        '       Form1.closeForm(Form13.dlgWord)                                ''~v065I~''~v073R~
        '       Form1.closeForm(Form1.MainForm.dlgSpecialKey, False) 'keep dlgSpecialKey''~v071R~''~v101R~
        '*      Form1.closeForm14(Form1.MainForm.dlgSpecialKey, False) 'keep dlgSpecialKey''~v101I~''~v123R~
        Form1.closeForm14(Form14.dlgSymbol, False) 'keep valiable      ''~v123I~
        Dim rc As Boolean = True                                       ''~7508I~
        If IsNothing(undoRedo) Then      'fine not found or read err   ''~7617I~
            Return True                                                ''~7617I~
        End If                                                         ''~7617I~
        If undoRedo.isUpdated() Then                                   ''~7508I~
            rc = Form1.confirmDiscard(e, Me.Text)                      ''~7508I~
        End If
        Return rc ''~7508I~
    End Function                                                       ''~7508I~
    Public Function chkDiscard2(ByRef PswDoI2K As Boolean) As Boolean  ''~v017R~
        '** from form2                                                 ''~v017R~
        '** rc:true=continue process                                   ''~v017I~
        '*** if not updated chk once saved                                 ''~v017I~
        '       Form1.closeForm(Form1.dlgFind3)                                ''~v017I~''~v101R~
        Form1.closeForm8(Form1.dlgFind3)                               ''~v101I~
        Dim rc As Boolean = True                                       ''~v017I~
        PswDoI2K = False                                                 ''~v017I~
        If IsNothing(undoRedo) Then      'fine not found or read err   ''~v017I~
            Return True                                                ''~v017I~
        End If                                                         ''~v017I~
        If undoRedo.isUpdated() Then                                   ''~v017I~
            rc = Form1.confirmDiscard(Nothing, Me.Text)                      ''~v017I~
            If rc Then   'discard:Yes                                  ''~v035I~
                PswDoI2K = True                                        ''~v035I~
            End If                                                     ''~v035I~
        Else                                                           ''~v017I~
            If swSaved Then                                                 ''~v017I~
                rc = True                                                ''~v017I~
                PswDoI2K = True                                          ''~v017I~
            End If                                                     ''~v017I~
        End If                                                         ''~v017I~
        Return rc                                                      ''~v017I~
    End Function                                                       ''~v017I~
    Public Function isUpdated() As Boolean                             ''~v015I~
        If IsNothing(undoRedo) Then      'fine not found or read err   ''~v015I~
            Return False                                               ''~v015I~
        End If                                                         ''~v015I~
        Return undoRedo.isUpdated()                                    ''~v015I~
    End Function                                                       ''~v015I~
    Public Function chkDiscard() As Boolean                                       ''~7508I~''~v101R~
        ' rc:true:continue process                                     ''~7508I~
        Return chkDiscard(Nothing)                                     ''~7508I~
    End Function                                                       ''~7508I~
#If False Then                                                              ''~v100I~
    Function setImage(Pfnm As String, PenglishDoc As Boolean) As Boolean                                       ''~7411R~''~7619R~
        swEnglishDoc = PenglishDoc                                       ''~v032I~
        '       If PenglishDoc Then                                                ''~7619I~''~v032R~
        '           imageTextFilename = Form1.changeExt(Pfnm, Form1.FILTER_DEFAULT_ENGLISHTEXT) ''~7619I~''~v032R~
        '       Else                                                           ''~7619I~''~v032R~
        imageTextFilename = Form1.changeExt(Pfnm, Form1.FILTER_DEFAULT_KANJITEXT) ''~7513R~''~7619R~
        '       End If                                                         ''~7619I~''~v032R~
        saveFilename = imageTextFilename                                 ''~7607I~
        '       Me.Text = "イメージ読取り:" & imageTextFilename                ''~7513R~''~7614R~
        If swEnglishDoc Then                                                 ''~v032I~
            Me.Text = Rstr.getStr("STR_ENGLISH_DOC_EXTRACTED") & Form1.TITLE_SEP & imageTextFilename ''~v032I~
        Else                                                             ''~v032I~
            Me.Text = Rstr.FORM3_TITLE    ''~7614R~''~7617R~''~7619R~  ''+v181R~
        End If                                                           ''~v032I~
        swSource = 1                                                     ''~7411I~
        swSaved = False                                                  ''~v017I~
        swRead = False                                                   ''~7612I~
        imageFilename = Pfnm                                             ''~7411I~
        Return readImage(Pfnm, PenglishDoc)                                            ''~7411R~''~7609R~''~7619R~
        '       Me.Width = formWidth                                       ''~7411R~''~7609R~''~7619R~
        '       Me.Height = formHeight                                     ''~7411R~''~7609R~''~7619R~
    End Function                                                       ''~7619R~
#Else                                                                  ''~v100I~
    Public Sub setImage(Pfnm As String, Ptext As String, PenglishDoc As Boolean)   ''~v100R~
        swEnglishDoc = PenglishDoc                                     ''~v100I~
        imageTextFilename = Form1.changeExt(Pfnm, Form1.FILTER_DEFAULT_KANJITEXT) ''~v100I~
        saveFilename = imageTextFilename                               ''~v100I~
        Me.Text = Rstr.FORM3_TITLE_RECEIVED & Form1.TITLE_SEP & imageTextFilename ''~v100I~
        swSource = 1                                                   ''~v100I~
        swSaved = False                                                ''~v100I~
        swRead = False                                                 ''~v100I~
        imageFilename = Pfnm                                           ''~v100I~
        readImage(Pfnm, Ptext)                                         ''~v100R~
    End Sub                                                            ''~v100I~
#End If                                                                ''~v100I~
    '***************************************************************************''~v106I~
    Public Sub setImagePartial(PswNew As Boolean, Pfnm As String, Ptext As String, PenglishDoc As Boolean) ''~v106I~
        '* PswNew:Nt yet full extraction                                   ''~v106I~
        If PswNew Then                                                      ''~v106I~
            setImage(Pfnm, Ptext, PenglishDoc)                           ''~v106I~
            Exit Sub                                                   ''~v106I~
        End If                                                         ''~v106I~
        TBF.restoreSelection()                                         ''~v165I~
        updateText(Ptext)                                              ''~v106I~
    End Sub                                                            ''~v106I~
    '***************************************************************************''~v106I~
    Sub updateText(Ptext As String)                                    ''~v106I~
        Dim pos As Integer = TextBox1.SelectionStart                   ''~v106I~
        Dim cutlen As Integer = TextBox1.SelectionLength               ''~v106I~
        Dim txtold As String = TextBox1.Text                           ''~v106I~
        Dim txtoldsz As Integer = txtold.Length                        ''~v106I~
        Dim txtadd As String = Ptext                                   ''~v106I~
        Dim txtnew As String                                           ''~v106I~
        pos = Math.Min(pos, txtoldsz)                                  ''~v106I~
        Dim pos2 As Integer = Math.Min(pos + cutlen, txtoldsz)         ''~v106I~
        txtnew = txtold.Substring(0, pos) & txtadd & txtold.Substring(pos2, txtoldsz - pos2) ''~v106I~
        TextBox1.Text = txtnew                                           ''~v106I~
        '*      TextBox1.Select(pos, txtadd.Length) 'pos and length             ''~v106I~''~v167R~
        Dim revlen As Integer = txtadd.Length                            ''~v167I~
        revlen += getSubstrCount(txtadd, vbCrLf)                       ''~v167R~
        TextBox1.Select(pos, revlen) 'pos and length                   ''~v167I~
        '        TextBox1.Invalidate()                                           ''~v106I~
        TextBox1.ScrollToCaret()  '*above set text set caretr to top page,ajust to insterted text pos''~v175I~
    End Sub                                                            ''~v106I~
    '***************************************************************************''~v106I~
    Sub setText(Pfnm As String)                                    ''~7411I~
#If False Then                                                              ''~v100I~
        swEnglishDoc = False                                             ''~v032I~
#End If                                                                ''~v100I~
        swSource = 2                                                     ''~7411I~''~7612M~
        swSaved = False                                                  ''~v017I~
        swRead = False                                                   ''~7612I~
        '       Dim prefix As String = TITLE_READ                               ''~7614I~''~7617R~
        Dim prefix As String = Rstr.FORM3_TITLE                        ''~7617I~
        If Pfnm.CompareTo("") = 0 Then                                        ''~7612I~
            Dim fnm As String = Form1.changeExt(Rstr.MENU_NEWTEXT_FILE, Form1.FILTER_DEFAULT_KANJITEXT) ''~7612I~
            Me.Text = prefix & Form1.TITLE_SEP & fnm                        ''~7612R~''~7614R~''~7619R~
            swNewText = True                                             ''~7612I~
            readText(fnm)                                              ''~7612R~
            swNewText = False                                            ''~7612I~
            showStatus(Rstr.getStr("STR_MSG_TEXT_NEW"))                ''~v063R~
        Else                                                           ''~7612I~
            Me.Text = prefix & Form1.TITLE_SEP & Pfnm                        ''~7614R~''~7619R~
            readText(Pfnm)                                                 ''~7411I~''~7612I~
            '           Dim encoding As String = IIf(DocOptions.swUTF8, System.Text.Encoding.UTF8.EncodingName, System.Text.Encoding.Default.EncodingName) ''~v063R~''~v101R~
            Dim encoding As String = CType(IIf(DocOptions.swUTF8, System.Text.Encoding.UTF8.EncodingName, System.Text.Encoding.Default.EncodingName), String) ''~v101I~
            showStatus(String.Format(Rstr.getStr("STR_MSG_TEXT_READ"), encoding)) ''~v063R~
        End If                                                          ''~7612I~
        '       Me.Width = formWidth                                             ''~7411R~''~7619R~
        '       Me.Height = formHeight                                           ''~7411R~''~7619R~
    End Sub                                                            ''~7411I~
    '***************************************************************************''~v106I~
    Sub changedEncoding()                                              ''~v063I~
        If Not chkDiscard(Nothing) Then                                     ''~v063I~
            Exit Sub                                                   ''~v063I~
        End If                                                         ''~v063I~
        setText(textFilename)                                          ''~v063I~
    End Sub                                                            ''~v063I~
    '***************************************************************************''~v106I~

    Sub readText(Pfnm As String)                                       ''~7411R~
        Dim text As String = ""                                          ''~7609I~
        textFilename = Pfnm                                            ''~7411R~
        saveFilename = textFilename                                      ''~7607I~
        TextBox1.Font = createFont()                                   ''~7411I~''~7612M~
        If swNewText Then                                                   ''~7612I~
            text = vbCrLf                                                ''~7612I~
        Else                                                           ''~7612I~
            If (Not (System.IO.File.Exists(textFilename))) Then            ''~7410I~''~7612R~
                Form1.NotFound(Pfnm)                                       ''~7428R~''~7612R~
                Exit Sub                                                   ''~7410I~''~7612R~
            End If                                                         ''~7410I~''~7612R~
            Try                                                        ''~7612R~
                If DocOptions.swUTF8 Then                                     ''~v063I~
                    text = System.IO.File.ReadAllText(textFilename, System.Text.Encoding.UTF8) ''~v063I~
                Else                                                     ''~v063I~
                    text = System.IO.File.ReadAllText(textFilename, System.Text.Encoding.Default) ''~7410I~''~7609R~''~7612R~
                End If                                                   ''~v063I~
                swRead = True                                            ''~7612I~
            Catch ex As Exception                                      ''~7612R~
                Form1.ReadError(Pfnm, ex)                                   ''~7428I~''~7612R~
            End Try                                                    ''~7612R~
        End If                                                         ''~7612I~
        showText(text)                                             ''~7429I~''~7609I~
    End Sub

    Private Sub Form3_ResizeEnd(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.ResizeEnd ''~7411R~
        '       Dim this As Control = sender                                   ''~7411I~''~v101R~
        Dim this As Control = CType(sender, Control)                    ''~v101I~
        My.Settings.CfgFormSizeWText = this.Size.Width                   ''~7411R~
        My.Settings.CfgFormSizeHText = this.Size.Height                  ''~7411R~
    End Sub 'resize                                                    ''~7411I~
    ''~7411I~

    '   Private Sub ToolStripButtonHelp_Click(ByVal sender As System.Object, ByVal e As EventArgs)  ''~7429I~''~v114R~
    Private Sub On_Help_click(sender As Object, e As EventArgs) Handles ToolStripMenuItemHelp.Click ''~v114M~
        Try                                                            ''~v111I~
            showHelp()                                                     ''~7429I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 Help", ex)                        ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7429I~
    '   Private Sub ToolStripButtonUndo_Click(ByVal sender As System.Object, ByVal e As EventArgs)  ''~7429R~''~v114R~
    Private Sub On_Undo_click(sender As Object, e As EventArgs) Handles ToolStripMenuItemUndo.Click ''~v114M~
        Try                                                            ''~v111I~
            undoText()                                                     ''~7429R~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 Undo", ex)                        ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7429I~
    '   Private Sub ToolStripButtonRedo_Click(ByVal sender As System.Object, ByVal e As EventArgs)  ''~7429R~''~v114R~
    Private Sub On_Redo_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItemRedo.Click ''~v114M~
        Try                                                            ''~v111I~
            redoText()                                                     ''~7429I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 Redo", ex)                        ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7429I~
    '   Private Sub ToolStripButtonSave_Click(ByVal sender As System.Object, ByVal e As EventArgs)  ''~7410I~''~v114R~
    Private Sub On_Save_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItemSave.Click ''~v114M~
        Try                                                            ''~v111I~
            If swSource = 1 Then    'from imagefile                             ''~7513I~
                If swSaved AndAlso Not isUpdated() Then                    ''~v015I~
                    Form1.FileNotSaved()                                   ''~v015I~
                    Exit Sub                                               ''~v015I~
                End If                                                     ''~v015I~
                If Not Form1.confirmReceivedSave(imageTextFilename) Then        ''~7513I~
                    Exit Sub                                               ''~7513I~
                End If                                                     ''~7513I~
                '       End If                                                         ''~7513I~''~v015R~
            Else                                                           ''~v015I~
                If Not swRead Then                                                  ''~7612I~
                    If Not Form1.confirmNewText(textFilename) Then                  ''~7612I~
                        Exit Sub                                               ''~7612I~
                    End If                                                     ''~7612I~
                Else                                                           ''~v015I~
                    If Not isUpdated() Then                                         ''~v015I~
                        Form1.FileNotSaved()                                   ''~v015I~
                        Exit Sub                                               ''~v015I~
                    End If                                                     ''~v015I~
                End If                                                         ''~7612I~
            End If                                                         ''~v015I~
            saveFile(saveFilename)                                         ''~7410I~''~7607R~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 Save", ex)                        ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7410I~
    '********************************************************************************''~v162I~
    '   Private Sub ToolStripButtonSaveAs_Click(ByVal sender As System.Object, ByVal e As EventArgs)  ''~7410I~''~v114R~
    Private Sub On_SaveAs_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItemSaveAs.Click ''~v114I~
        SaveFileDialog1.Filter = Rstr.getStr("STR_FILTER_KANJITEXT")   ''~v162I~
        Try                                                            ''~v111I~
            If SaveFileDialog1.ShowDialog() = DialogResult.OK Then         ''~7410I~
                '           MessageBox.Show(SaveFileDialog1.FileName)      ''~7410I~
                Dim fnm As String = SaveFileDialog1.FileName               ''~7410I~
                saveFile(fnm)                                              ''~7410I~
            End If                                                         ''~7410I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 SaveAs", ex)                      ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7410I~
    '********************************************************************************''~v162I~
    Private Sub saveFile(Pfnm As String)                               ''~7410I~
        Dim txt As String                                              ''~7429R~
        txt = undoRedo.getTextToSave()                                   ''~7430I~
        Try                                                            ''~7410I~
            System.IO.File.WriteAllText(Pfnm, txt, System.Text.Encoding.Default) ''~7410I~''~7429R~
            Form1.MainForm.insertMRUList(2, Pfnm)                                ''~7429I~''~7521R~
            undoRedo.saved()                                           ''~7430I~
            '           MessageBox.Show(Pfnm & " を保存しました")                  ''~7513I~''~7617R~
            '           MessageBox.Show(Pfnm, Rstr.MSG_INFO_SAVED)                       ''~7617I~''~v050R~
            showStatus(Rstr.MSG_INFO_SAVED & ":" & Pfnm)              ''~v050I~
            swSaved = True                                             ''~v015I~
        Catch ex As Exception                                          ''~7410I~
            Form1.WriteError(Pfnm, ex)                                  ''~7428I~
        End Try                                                        ''~7410I~
    End Sub                                                            ''~7410I~

    '   Private Sub ToolStripButtonPrint_Click(ByVal sender As System.Object, ByVal e As EventArgs)''~v114R~
    Private Sub On_Print_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItemPrint.Click ''~v114M~
        Try                                                            ''~v111I~
            printText()                                                    ''~7429I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 Print", ex)                       ''~v111I~
        End Try                                                        ''~v111I~
    End Sub
    ''~7411I~
    ''~7411I~
    Private Function createFont() As Font                              ''~7411I~
        '       Return New Font(fontFamily, Int(fontSize), fontStyle)          ''~7411I~''~7515R~
        Return Form1.createFont()                                      ''~7515I~
    End Function                                                            ''~7411I~
    ''~7411I~
    ''~7411I~
    ''~7411I~
    '   Private Sub ToolStripButton2kana_Click(ByVal sender As System.Object, ByVal e As EventArgs)  ''~7411I~''~v114R~
    Private Sub On_Conv_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItemKanaConv.Click ''~v114M~
        Try                                                            ''~v111I~
#If False Then                                                         ''~7522I~
        If Form1.MainForm.formkanaText Is Nothing OrElse Form1.MainForm.formkanaText.IsDisposed Then ''~7411R~''~7521R~
            Form1.MainForm.formkanaText = New Form4()                             ''~7411R~''~7521R~
#Else                                                                  ''~7522I~
            If Form1.MainForm.formkanaText Is Nothing Then                      ''~7522I~
                Form1.MainForm.formkanaText = New ClassKanaText()          ''~7522I~
#End If                                                                ''~7522I~
            Else                                                           ''~7508I~
                '           If Not Form1.MainForm.formkanaText.chkDiscard(e) Then         ''~7508I~''~7521R~''~v007R~
                '               Exit Sub                                               ''~7508I~''~v007R~
                '           End If                                                     ''~7508I~''~v007R~
            End If                                                         ''~7411I~
            Try                                                            ''~7411I~
                '           Dim fnm                                                    ''~7411I~''~v101R~
                Dim fnm As String                                          ''~v101I~
                If swSource = 1 Then       'from imagefile                 ''~7513R~
                    fnm = Form1.changeExt(imageFilename, Form1.FILTER_DEFAULT_KANATEXT) ''~7513I~
                Else                                                       ''~7411I~
                    fnm = Form1.changeExt(textFilename, Form1.FILTER_DEFAULT_KANATEXT) ''~7513I~
                End If                                                      ''~7411I~
                '           swKatakanaOkurigana = My.Settings.CFGF3_KatakanaOkurigana  ''~v006I~''~v030R~
                Form1.MainForm.formkanaText.setText(TextBox1.Text, swSource, fnm)     ''~7411R~''~7521R~
                Form1.showTop(Form1.MainForm)                              ''~v113I~
            Catch ex As Exception                                          ''~7411I~
                '           MessageBox.Show("かな変換に失敗:" & ex.Message)    ''~7411I~''~7428R~''~7617R~
                MessageBox.Show(ex.Message, Rstr.MSG_ERR_KANA_CONV)              ''~7617I~
            End Try                                                        ''~7411I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 Kana", ex)                        ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7411I~
    '*************************************************************         ''~7411I~
#If False Then                                                              ''~v100I~
    Private Function readImage(Pfnm As String, PenglishDoc As Boolean) As Boolean                                      ''~7411R~''~7428R~''~7619R~
        'VB(A)では Microsoft Office Document Imaging 11.0 Type Library を参照設定''~7411I~
        nestCtr += 1                               '@@@@test           ''~7619M~
        '       Trace.W("MODI before nestctr=" & nestCtr & ",english=" & PenglishDoc)          '@@@@test''~7619I~''~v017R~
        If nestCtr > 1 Then                                                   ''~7619I~
            nestCtr -= 1                                                 ''~7619I~
            Return False                                               ''~7619I~
        End If                                                         ''~7619I~
        Dim oImage ' As MODI.Image                                     ''~7411I~
        Dim oLayout ' As MODI.Layout                                   ''~7411I~
        Dim langid As Integer                                          ''~7618R~
        If PenglishDoc Then                                    ''~7618I~''~7619R~
            langid = MODI.MiLANGUAGES.miLANG_ENGLISH  '=9              ''~7618I~
        Else                                                           ''~7618I~
            langid = MODI.MiLANGUAGES.miLANG_JAPANESE '=17             ''~7618I~
        End If                                                         ''~7618I~
        Dim txt As String = ""                                          ''~7609I~
        Try                                                            ''~7609I~
            '           oDocument = CreateObject("MODI.Document")                      ''~7411I~''~7609R~''~7610R~
            Dim oDocument As New MODI.Document()                         ''~7411I~''~7610I~
            oDocument.Create(Pfnm) ' GIFや Jpeg画像を読ませる              ''~7411R~''~7609R~
            oDocument.OCR(langid, True, True) ' adjust rotation,adjust distortion''~7610R~
            oImage = oDocument.Images(0) ' 複数ページのドキュメント対応？1ページ目''~7411I~''~7609R~
            oLayout = oImage.Layout                                        ''~7411I~''~7609R~
            txt = splitLineI2K(oLayout, PenglishDoc)                                   ''~7411M~''~7428R~''~7429R~''~7609R~''~7619R~
            oLayout = Nothing                                              ''~7411I~''~7609I~
            oImage = Nothing                                               ''~7411I~''~7609I~
            oDocument.Close()                                              ''~7411I~''~7609I~
            oDocument = Nothing                                            ''~7411I~''~7609I~
            '           MessageBox.Show("イメージファイル（" & imageFilename & "）読取りました") ''~7514I~''~7609M~''~7617R~
            '           MessageBox.Show(imageFilename, Rstr.MSG_INFO_EXTRACTED)     ''~7617I~''~v034R~
            showStatus(True, Rstr.MSG_INFO_EXTRACTED)                   ''~v034I~
        Catch ex As Exception                                          ''~7609I~
            '           MessageBox.Show("イメージファイル（" & Pfnm & "）のテキスト読み取り失敗:" & ex.Message & "Stack:" & ex.StackTrace) ''~7609R~''~7617R~
            MessageBox.Show(Pfnm & " : " & ex.Message, Rstr.MSG_ERR_EXTRACT) ''~7617R~
            txt = Rstr.MSG_ERR_EXTRACT                                   ''~7617R~
        End Try                                                        ''~7609I~
        nestCtr -= 1                               '@@@@test             ''~7619I~
        '       Trace.W("MODI after nestctr=" & nestCtr)          '@@@@test    ''~7619I~''~v017R~
        TextBox1.Font = createFont()                                   ''~7411M~''~7609I~
        showText(txt)                                                  ''~7429I~
        '       Trace.W("after showtext =" & nestCtr)          '@@@@test       ''~7619I~''~v017R~
        ''~7411I~
        Return True                                                    ''~7619I~
    End Function                                                            ''~7411M~
#Else                                                                  ''~v100I~
    Private Function readImage(Pfnm As String, Ptext As String) As Boolean ''~v100R~
        Dim txt As String = Ptext                                      ''~v100I~
        showStatus(True, Rstr.MSG_INFO_EXTRACTED)                      ''~v100I~
        TextBox1.Font = createFont()                                   ''~v100I~
        showText(txt)                                                  ''~v100I~
        Return True                                                    ''~v100I~
    End Function                                                       ''~v100I~
#End If                                                                ''~v100I~
#If False Then                                                              ''~v100I~
    '*************************************************************     ''~v100I~
    Private Function splitLineI2K(Playout As MODI.Layout, PenglishDoc As Boolean) As String    ''~7428R~''~7619R~
        '** split by RegionID                                          ''~7619R~
        Dim word As MODI.Word                                       ''~7411I~''~7428I~
        Dim len, id, idold As Integer                        ''~7428R~  ''~7429R~
        Dim sbOut As System.Text.StringBuilder
        len = Playout.Text.Length
        sbOut = New System.Text.StringBuilder(len * 2)
        idold = -1                                                 ''~7428R~''~7429R~
        For ii As Integer = 0 To Playout.Words.Count - 1               ''~7411I~''~7428I~
            word = Playout.Words.Item(ii)                              ''~7428I~
            '           id= word.RegionId                                     ''~7428R~''~7429R~
            '           Trace.W("lindid=" & word.LineId & ",word=" & word.Text & ",regionID=" & word.RegionId & ",ID=" & word.Id) ''~7619I~''~v017R~
            '           id = word.LineId                                            ''~7429I~''~7619R~
            id = word.RegionId                                         ''~7619I~
            If id <> idold Then                                   ''~7428R~''~7429R~
                If idold <> -1 Then                                ''~7428R~''~7429R~
                    sbOut.AppendLine()                                 ''~7428R~
                End If                                                 ''~7428I~
                idold = id                                             ''~7429R~
            End If                                                     ''~7428I~
            sbOut.Append(word.Text)                                    ''~7428I~
            If PenglishDoc Then                                             ''~7619I~
                sbOut.Append(FormatBES.CHAR_SPACE_SBCS) 'between english word''~7619R~
            Else                                                       ''~7619I~
                'append space insert for each char                         ''~7619I~
            End If                                                     ''~7619I~
        Next
        sbOut.AppendLine()
        Dim str = sbOut.ToString()                                        ''~7428I~
        sbOut.Clear()                                                     ''~7428I~
        Return str                                                     ''~7428I~
    End Function                                                       ''~7428I~
#End If                                                                ''~v100I~
    '*************************************************************     ''~v100I~
    Private Sub showText(Ptext As String)                              ''~7429I~
        '       undoRedo = New ClassUndoRedo(ClassUndoRedo.OPT_KANJITEXT, TextBox1, ToolStripButtonUndo, ToolStripButtonRedo) ''~7430I~''~7501R~''~7506R~''~v114R~
        undoRedo = New ClassUndoRedo(ClassUndoRedo.OPT_KANJITEXT, TextBox1, ToolStripMenuItemUndo, ToolStripMenuItemRedo) ''~v114I~
        undoRedo.showText(Ptext)                                       ''~7430I~
        Me.Activate()                                                  ''~7514I~
        TextBox1.Select(0, 0)                                           ''~7521I~
    End Sub                                                            ''~7429I~
    '*************************************************************     ''~v100I~
    Private Sub showHelp()                                             ''~7429I~
        Dim txt As String                                              ''~7429I~
        If FormOptions.swLangEN Then                                   ''~7614I~
            txt = My.Resources.help_form3E                             ''~7614I~
        Else                                                           ''~7614I~
            txt = My.Resources.help_form3                                  ''~7507R~''~7614R~
        End If                                                         ''~7614I~
        MessageBox.Show(txt, Rstr.FORM3_TITLE_RECEIVED)                                  ''~7429I~''~7614R~''~7617R~
    End Sub                                                            ''~7429I~
    '*************************************************************     ''~v100I~
    Private Sub TBChanged()                                          ''~7429I~''~7430M~
        undoRedo.TBChanged()                                           ''~7430I~
    End Sub                                                            ''~7429I~''~7430M~
    Private Sub redoText()                                             ''~7429R~
        undoRedo.redoText()                                            ''~7430I~
    End Sub                                                            ''~7429I~
    Private Sub undoText()                                             ''~7429I~
        undoRedo.undoText()                                            ''~7430I~
    End Sub                                                            ''~7429I~
    Private Sub printText()                                            ''~7429I~
        Form1.MainForm.printDoc(Me.Text, TextBox1.Text)             ''~7429I~   ''~7430R~''~7521R~
    End Sub                                                            ''~7429I~
    Private Sub TB_KeyUpEvent(sender As System.Object, e As System.Windows.Forms.KeyEventArgs) Handles TextBox1.KeyUp ''~7513I~
        Try                                                            ''~v111I~
            undoRedo.TB_KeyUp(e)                                           ''~7513I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 KeyUp", ex)                       ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7513I~
    Private Sub TB_KeyDownEvent(sender As System.Object, e As System.Windows.Forms.KeyEventArgs) Handles TextBox1.KeyDown ''~7513I~
        Try                                                            ''~v111I~
            undoRedo.TB_KeyDown(e)                                         ''~7513I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 KeyDown", ex)                     ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7513I~
    Private Sub TB_KeyPressEvent(sender As System.Object, e As System.Windows.Forms.KeyPressEventArgs) Handles TextBox1.KeyPress ''~7513I~
        Try                                                            ''~v111I~
            undoRedo.TB_KeyPress(e)                                        ''~7513I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 KeyPress", ex)                    ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7513I~
    Private Sub TB_MouseClickEvent(sender As System.Object, e As System.Windows.Forms.MouseEventArgs) Handles TextBox1.MouseClick ''~v105I~
        Try                                                            ''~v111I~
            undoRedo.TB_MouseClick(e)                                      ''~v105I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 MouseClick", ex)                  ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~v105I~
    Private Sub CMCut_Click(sender As System.Object, e As System.EventArgs) Handles CMCut.Click ''~7514I~
        Try                                                            ''~v111I~
#If False Then                                                              ''~v171I~
            undoRedo.CMCut(0)    'del crlf with SIGN_CRLF is last          ''~7514I~
            TextBox1.Cut()                                                 ''~7514I~
            undoRedo.CMCut(1)    'del crlf with SIGN_CRLF is last          ''~7514I~
#Else                                                                  ''~v171I~
            undoRedo.CutTB(True)    'del crlf with SIGN_CRLF is last   ''~v171I~
#End If                                                                ''~v171I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 MenuCut", ex)                     ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7514I~
    Private Sub CMCopy_Click(sender As System.Object, e As System.EventArgs) Handles CMCopy.Click ''~7514I~
        Try                                                            ''~v111I~
#If False Then                                                              ''~v171I~
            TextBox1.Copy()                                                ''~7514I~
#Else                                                                  ''~v171I~
            undoRedo.CopyTB(True)    'del crlf with SIGN_CRLF is last  ''~v171I~
#End If                                                                ''~v171I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 MenuCopy", ex)                    ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7514I~
    Private Sub CMPaste_Click(sender As System.Object, e As System.EventArgs) Handles CMPaste.Click ''~7514I~
#If False Then                                                              ''~v171I~
        Dim crlfctr As Integer = 0                                       ''~v168I~
        Try                                                            ''~v111I~
            If Clipboard.ContainsText() Then           ''~v168I~
                Dim str As String = Clipboard.GetText()               ''~v168I~
                If str.IndexOf(FormatBES.SIGN_CRLF) < 0 Then  'not from form3/form1      ''~v168I~
                    crlfctr = getSubstrCount(str, vbCrLf)                 ''~v168I~
                End If                                                 ''~v168I~
            End If                                                     ''~v168I~
            Dim poso As Integer = TextBox1.SelectionStart                ''~v168I~
            TextBox1.Paste()                                               ''~7514I~
'*            Trace.W("CMPaste_Click old pos=" & TextBox1.SelectionStart & ",len=" & TextBox1.SelectionLength & ",crlfctr=" & crlfctr) ''~v168I~''~v171R~
            Dim posn As Integer = TextBox1.SelectionStart + crlfctr        ''~v168I~
            TextBox1.Select(poso, posn - poso)                             ''~v168R~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 MenuPaste", ex)                   ''~v111I~
        End Try                                                        ''~v111I~
#Else                                                                  ''~v171I~
        Try                                                            ''~v171I~
            undoRedo.PasteTB(True)    '* true:contextMenu              ''~v171R~
        Catch ex As Exception                                          ''~v171I~
            Form1.exceptionMsg("Form3 MenuPaste", ex)                  ''~v171I~
        End Try                                                        ''~v171I~
#End If                                                                ''~v171I~
    End Sub                                                            ''~7514I~
    Private Sub CMSelectAll_Click(sender As System.Object, e As System.EventArgs) Handles CMSelectAll.Click ''~7514I~
        Try                                                            ''~v111I~
            TextBox1.SelectAll()                                           ''~7514I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 MenuSelectAll", ex)               ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7514I~
    Public Sub optionChanged()                                         ''~7515I~
        If Form1.MainForm.dlgOptions.swFontChangedScr Then                      ''~7515I~''~7521R~
            TextBox1.Font = createFont()                                ''~7515I~
        End If                                                         ''~7515I~
    End Sub                                                            ''~7515I~
    Public Sub showSpecialKeyDialog()                                  ''~7515I~
        '*      Form1.MainForm.dlgSpecialKey.showForForm3(Me)                           ''~7515R~''~7521R~''~v123R~
        Form14.dlgSymbol.showForForm3(Me)                              ''~v123I~
    End Sub                                                            ''~7515I~
    Private Sub ToolStripMenuItemSpecialChar_Click(sender As System.Object, e As System.EventArgs) Handles ToolStripMenuItemSpecialChar.Click ''~7515I~
        Try                                                            ''~v111I~
            showSpecialKeyDialog()                                       ''~7515I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 MenuSpecialChar", ex)             ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7515I~
    Public Sub showFindDialog()                                        ''~7516I~
        Form1.MainForm.showForForm3(Me)                                 ''~7516I~''~7519R~''~7521R~
    End Sub                                                            ''~7516I~
    Private Sub ToolStripMenuItemDocOptions_Click(sender As System.Object, e As System.EventArgs) Handles ToolStripMenuItem_DocOptions.Click ''~v030R~
        Try                                                            ''~v111I~
            showDocOptionsDialog()                                         ''~v030I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 MenuDocOption", ex)               ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~v030I~
    Public Sub showDocOptionsDialog()                                  ''~v030I~
        '       DocOptions.showDlg(Me)                                         ''~v030I~''~v063R~
        Dim oldUTF8 As Boolean = DocOptions.swUTF8                       ''~v063I~
        '       Dim rc As Integer = DocOptions.showDlg(Me)                       ''~v063I~''~v101R~
        Dim rc As Integer = CType(DocOptions.showDlg(Me), Integer)      ''~v101I~
        If rc = DialogResult.OK Then                                          ''~v063I~
            If DocOptions.swUTF8 <> oldUTF8 Then                              ''~v063I~
                If swRead Then 'file read                                   ''~v063I~
                    changedEncoding()                                  ''~v063I~
                End If                                                 ''~v063I~
            End If                                                     ''~v063I~
        End If                                                         ''~v063I~
    End Sub                                                            ''~v030I~
    '    Private Sub ToolStripMenuItemkatakanaOkurigana_Click(sender As System.Object, e As System.EventArgs) Handles ToolStripMenuItem_DocOptions.Click ''~7516I~''~v006R~''~v030R~
    '        swapKatakanaOkurigana()                                               ''~7516I~''~v006R~''~v030R~
    '    End Sub                                                            ''~7516I~''~v030R~
    Private Sub ToolStripMenuItemDictionary_Click(sender As System.Object, e As System.EventArgs) Handles ToolStripMenuItemDictionary.Click ''~v007I~''~v008I~
        Try                                                            ''~v111I~
            '       Form1.MainForm.dlgDictionary.showDlg()                         ''~v008R~
            Form11.sharedShowDlg()                                         ''~v008R~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 MenuDictionary", ex)              ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~v008I~
    Private Sub ToolStripMenuItemWords_Click(sender As System.Object, e As System.EventArgs) Handles ToolStripMenuItemWords.Click ''~v065I~
        Try                                                            ''~v111I~
            '       Form13.sharedShowDlg()                                         ''~v065R~
            Form13.sharedShowDlg(False)      'not Form1                    ''~v065I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 MenuWords", ex)                   ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~v065I~
    Private Sub ToolStripMenuItemFind_Click(sender As System.Object, e As System.EventArgs) Handles ToolStripMenuItemFind.Click ''~v006I~
        Try                                                            ''~v111I~
            showFindDialog()                                               ''~v006I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form3 MenuFind", ex)                    ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~v006I~
    '   Public Sub findNext(PswUp)       ''~7519I~                         ''~7521I~''~v101R~
    Public Sub findNext(PswUp As Boolean)                              ''~v101I~
        Form1.MainForm.findNext(Me, PswUp)                             ''~7521R~
    End Sub                                                            ''~7521I~
    Public Sub findNextReplace(PswUp As Boolean)                       ''~v181I~
        Form1.MainForm.findNextReplace(Me, PswUp)                      ''~v181I~
    End Sub                                                            ''~v181I~
    Public Sub setLocale(Prefresh As Boolean)                          ''~7614R~
        Dim title As String = Me.Text                                    ''~7614I~
        FormOptions.setLang(Me, GetType(Form3))                        ''~7614I~
        updateTitle(title)                                             ''~7614I~
        If Prefresh Then                                                    ''~7614I~
            Me.Refresh()                                               ''~7614I~
        End If                                                         ''~7614I~
    End Sub                                                            ''~7614I~
    Public Sub updateTitle(Pold As String)                             ''~7614I~
        Dim pos As Integer                                         ''~7614I~''~7617I~
        pos = Pold.IndexOf(Form1.TITLE_SEP)                             ''~7614I~''~7617I~''~7619R~
        If pos >= 0 Then                                           ''~7614I~''~7617I~
            If swSource = 2 Then   'Read file,Title was set                       ''~7614I~''~7617I~
                Me.Text = Rstr.FORM3_TITLE & Pold.Substring(pos)           ''~7614I~''~7617I~
            Else                                                       ''~7617I~
                Me.Text = Rstr.FORM3_TITLE_RECEIVED & Pold.Substring(pos) ''~7617I~
            End If                                                     ''~7614I~''~7617I~
        End If                                                         ''~7614I~
    End Sub                                                            ''~7614I~
    Public Shared Function getTitleFilename() As String                ''~7618I~
        If Form1.formText Is Nothing OrElse Form1.formText.IsDisposed Then ''~7618I~
            Return Nothing                                             ''~7618I~
        End If                                                         ''~7618I~
        Dim pos As Integer = Form1.formText.Text.IndexOf(Form1.TITLE_SEP)            ''~7618I~''~7619R~
        If pos > 0 Then                                                ''~7618I~
            Return Form1.formText.Text.Substring(pos + Form1.TITLE_SEP.Length) ''~7618I~''~7619R~
        End If                                                         ''~7618I~
        Return Nothing                                                 ''~7618I~
    End Function                                                            ''~7618I~
    '    Private Sub swapKatakanaOkurigana()                                ''~v006I~''~v030R~
    '        Dim onof As String                                             ''~v006I~''~v030R~
    '        swKatakanaOkurigana = Not swKatakanaOkurigana                    ''~v006I~''~v030R~
    '        My.Settings.CFGF3_KatakanaOkurigana = swKatakanaOkurigana        ''~v006I~''~v030R~
    '        If swKatakanaOkurigana Then                                         ''~v006I~''~v030R~
    '            onof = ":ON"                                                 ''~v006I~''~v030R~
    '        Else                                                           ''~v006I~''~v030R~
    '            onof = ":OFF"                                                ''~v006I~''~v030R~
    '        End If                                                          ''~v006I~''~v030R~
    '        MessageBox.Show(onof, Rstr.getStr("STR_MSG_KATAKANAOKURIGANA")) ''~v006I~''~v030R~
    '    End Sub                                                            ''~v006I~''~v030R~
    Public Sub showStatus(Pmsg As String)                              ''~v034I~
        ToolStripStatusLabel1.Text = Pmsg                                ''~v034I~
        '*      Trace.W("Form3:showStatus =" & Pmsg)                            ''~v114I~''~v123R~
    End Sub                                                       ''~v034I~
    Public Sub showStatus(PswLater As Boolean, Pmsg As String)          ''~v034I~
        '*      Trace.W("Form3:showStatus swlater=" & PswLater & ",msg=" & Pmsg) ''~v114I~''~v123R~
        If (PswLater) Then                                                  ''~v034I~
            pendingStatusMsg = Pmsg                                    ''~v034I~
        Else                                                           ''~v034I~
            showStatus(Pmsg)                                           ''~v034I~
        End If                                                         ''~v034I~
    End Sub                                                            ''~v034I~
    Public Sub showStatus(Pch As Char, Pchcv As Char, PtypeSrc As Integer, PtypeTgt As Integer) ''~v034I~
        Dim msg, strSrc, strTgt As String                            ''~v034R~
        strSrc = FormatBES.getCharType(PtypeSrc)                         ''~v034I~
        strTgt = FormatBES.getCharType(PtypeTgt)                         ''~v034I~
        msg = Rstr.getStr("STR_MSG_CHANGELETTERWRAP")           ''~v034R~
        pendingStatusMsg = String.Format(msg, strSrc, Pch, strTgt, Pchcv) ''~v034R~
    End Sub                                                       ''~v034I~
    Public Sub showStatus(Pch As Char, PtypeSrc As Integer)            ''~v037I~
        '** from class1 * F4(queryKey) target info                         ''~v037I~
        Dim msg, strSrc As String                            ''~v037I~
        strSrc = FormatBES.getCharType(PtypeSrc)                       ''~v037I~
        msg = Rstr.getStr("STR_MSG_CHANGELETTERWRAP_QUERY")            ''~v037I~
        msg = String.Format(msg, strSrc, Pch)                          ''~v037R~
        showStatus(msg)     'imediately put msg                        ''~v037I~
    End Sub                                                            ''~v037I~
    '*************************************************************     ''~v114I~
    Private Sub loadMRUList()                                          ''~v114I~
        Dim item As ToolStripMenuItem = ToolStripMenuItemFile          ''~v114I~
        Form1.MainForm.loadMRUListForm3(item)                          ''~v114I~
    End Sub                                                            ''~v114I~
    '*************************************************************     ''~v114I~
    'from Form1 when menuitem selected                                 ''~v114I~
    Public Sub updateMRUList()                                         ''~v114I~
        loadMRUList()           'MRUList(1) is update,propagate to form2''~v114I~
    End Sub                                                            ''~v114I~

    Private Sub MenuStrip1_ItemClicked(sender As Object, e As ToolStripItemClickedEventArgs) Handles MenuStrip1.ItemClicked

    End Sub
    '*************************************************************     ''~v167I~
    '*count substring contained intgt str                              ''~v167I~
    Public Shared Function getSubstrCount(Pstr As String, Psubstr As String) As Integer ''~v167I~
        Dim ctr As Integer = 0                                           ''~v167I~
        Dim pos As Integer = 0                                           ''~v167I~
        Dim lentgt As Integer = Pstr.Length                              ''~v167I~
        Dim lensrc As Integer = Psubstr.Length                           ''~v167I~
        Do While True                                                  ''~v167I~
            If pos >= lentgt Then                                             ''~v167I~
                Exit Do                                              ''~v167I~
            End If                                                     ''~v167I~
            pos = Pstr.IndexOf(Psubstr, pos)                              ''~v167R~
            If (pos < 0) Then                                                 ''~v167I~
                Exit Do                                              ''~v167I~
            End If                                                     ''~v167I~
            ctr += 1                                                     ''~v167I~
            pos += lensrc                                                ''~v167I~
        Loop                                                           ''~v167I~
        Return ctr                                                     ''~v167I~
    End Function                                                       ''~v167I~
    '*************************************************************     ''~v171I~
    Public Sub restoreSelection()                                      ''~v171I~
        TBF.restoreSelection()                                         ''~v171I~
    End Sub                                                            ''~v171I~

    Private Sub ToolStripMenuItemLetterReplacement_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItemLetterReplacement.Click''~v179R~
        Try                                                            ''~v179R~
            Form6.sharedShowDlg()      '*True:Form1                    ''~v179R~
        Catch ex As Exception                                          ''~v179R~
            Form1.exceptionMsg("Form3 Menu.LetterReplacement", ex)     ''~v179R~
        End Try                                                        ''~v179R~
    End Sub
End Class