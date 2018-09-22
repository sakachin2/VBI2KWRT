'CID:''+v174R~:#72                             update#=  167;         ''~v159R~''+v174R~
'************************************************************************************''~v008I~
'v174 2018/09/13 (Bug by v165) SendButton from WordDialog always replace a char on csr''+v174I~
'v159 2018/02/24 load dicctionary file is not restore (it is not written)''~v159I~
'v123 2017/12/29 word/symbol dialog;no change dialog target by shortcut(Ctrl+x),change only by f9,add change button to form''~v123I~
'v117 2017/12/27 word dialog from also form1 as single instance        ''~v117I~
'v103 2017/12/16 (BUG)did not closed file for Dialog(Dictionary,Word,Symbol) at format err''~v103I~
'v102 2017/12/16 (BUG)Enable/Disable check  of Dislog(Dictionary,Word,Symbol)''~v102I~
'v101 2017/12/16 Conversion warning                                    ''~v101I~
'v080 2017/10/10 (BUG)2nd paste after cut remove cut pos twice         ''~v080I~
'v078 2017/10/09 dialog status bar                                     ''~v078I~
'v077 2017/10/08 Commit required when cut/copy                         ''~v077I~
'v075 2017/10/08 Set background color to Word dialog gridview          ''~v075I~
'v074 2017/10/08 (Bug)Word Dialog cut&Paste word was not copyed(requires set button text)''~v074I~
'v073 2017/09/27 (Bug)crash when words dialog update, close form3 then replied discard cancel''~v073I~
'v065 2017/09/24 Word dialog by Ctrl+char(except "1"-"0")              ''~v065I~
'************************************************************************************''~v008I~
Imports System.Runtime.InteropServices.Marshal                         ''~v008I~
Imports System.Text
Imports System.IO ''~v008I~
Public Class Form13                                                    ''~v008R~''~v012R~
    '** Word Dialog                                              ''~v008I~''~v012R~
    ''~v008I~
    Private Const COLNO = 3                                            ''~v008I~''~v065R~
    Private Const COLNO_DEL = 5                                        ''~v008I~''~v065R~
    Private Const CELLNO_ENABLE = 0                                      ''~v008I~
    Private Const CELLNO_CHARKEY = 1                                       ''~v008I~''~v065R~
    Private Const CELLNO_SEND = 2                                        ''~v008I~''~v065R~
    Private Const CELLNO_PHRASE = 3                                    ''~v065I~
    Private Const CELLNO_DELETE = 4                                    ''~v008I~''~v065R~
    Private Const ENABLEID_ON = "1"                                      ''~v008I~
    Private Const ENABLEID_OFF = "0"                                     ''~v008I~''~v012R~
    Private Const DEFAULT_EXT = "wrd"                                  ''~v012R~
    Private Const CHAR_SENDID = ChrW(&H2190)                             ''~v065I~
    ''~v012I~
    '   Private fileEncoding As System.Text.Encoding = System.Text.Encoding.UTF8''~v012R~
    Private fileEncoding As System.Text.Encoding = System.Text.Encoding.Default ''~v012R~
    Private cfgKeys As String = My.Settings.CFGF13_Words               ''~v065R~
    Private DGV As DataGridView                                        ''~v008R~
    Public Shared dlgWord As Form13                              ''~v008I~''~v012R~
    Private filterIndex As Integer = 0                                 ''~v012I~
    Private ListData() As String                                       ''~v008I~
    '   Private mruID = ClassMRU.ID_WORDS                                  ''~v065R~''~v101R~
    Private mruID As Integer = ClassMRU.ID_WORDS                       ''~v101I~
    Private MRU As ClassMRU = Form1.MainForm.MRU                         ''~v012I~
    Private saveFilename As String                                     ''~v012I~
    Private iDGV As KDGV   'DataGridView wrapper class                 ''~v078I~
    '***************************************************************************''~v008I~
    Private swUpdated As Boolean = False                               ''~v008I~
    Private swInvalid As Boolean = False                               ''~v065I~
    Private swCommitting As Boolean = False                            ''~v065I~
    Private swShown As Boolean = False                                 ''~v008I~
    Private swFilled As Boolean = False                                ''~v008I~
    Private MRUList As New List(Of String)                             ''~v012I~
    Private CPenable As Boolean                                        ''~v013I~
    Private CPcharkey, CPphrase As String                                   ''~v013I~''~v065R~
    Private CPButtonLabel As String                              ''~v074I~
    Private CPstatus As Integer = 0                                      ''~v013I~
    Private Const CPSTATUS_NONE = 0                                      ''~v013I~
    Private Const CPSTATUS_CUT = 1                                       ''~v013I~
    Private Const CPSTATUS_COPY = 2                                      ''~v013I~
    Private Const CPSTATUS_CUTDONE = 3                                 ''~v080I~
    Private CPcutrow As Integer                                        ''~v013I~
    Private swForm1 As Boolean = False                                   ''~v065I~
    '   Private strSendID = CHAR_SENDID.toString                             ''~v065I~''~v101R~
    Private strSendID As String = CHAR_SENDID.ToString                 ''~v101I~
    Private swDirty As Boolean = False                                   ''~v065I~
    Private swNotEnabled As Boolean = False                              ''~v065I~
    Private SB As SBM     'StatusBar                                   ''~v078I~
    Public callerForm1 As Form1                                        ''~v076I~''~v117I~
    Public callerForm3 As Form3                                        ''~v076I~''~v117I~
    Private BGColor As Color                                           ''~v117I~
    '***************************************************************************''~v008I~
    Public Sub setParent(PswForm1 As Boolean)           ''~v117I~
        '* from form1:to send word without showing by shortcutkey      ''~v117I~
        swForm1 = PswForm1                                             ''~v117I~
        If PswForm1 Then                                               ''~v117I~
            callerForm1 = Form1.MainForm                               ''~v117I~
            BGColor = callerForm1.TBBES.BackColor                      ''~v117I~
        Else                                                           ''~v117I~
            callerForm3 = Form1.formText                                ''~v117I~
            BGColor = callerForm3.TextBox1.BackColor                   ''~v117I~
        End If                                                         ''~v117I~
    End Sub                                                            ''~v117I~
    Public Shared Sub sharedShowDlg(PswForm1 As Boolean)                                  ''~v008R~
        If dlgWord Is Nothing OrElse dlgWord.IsDisposed() Then                                     ''~v008I~''~v012R~''~v117R~
            dlgWord = New Form13()                               ''~v008R~''~v012R~
        End If                                                         ''~v008I~
        dlgWord.setParent(PswForm1)                                           ''~v117I~
        dlgWord.showDlg(PswForm1)                                        ''~v008I~''~v012R~
    End Sub                                                            ''~v008I~
    Public Shared Function sharedApplyWords(Pcharkey As Char, PswForm1 As Boolean) As String ''~v065R~
        Dim word As String = Nothing                                     ''~v065M~
        '*      If PswForm1 Then                                                    ''~v065M~''~v117R~
        '*          Return word                                                ''~v065M~''~v117R~
        '*      End If                                                         ''~v065M~''~v117R~
        If dlgWord Is Nothing Then                               ''~v008I~''~v012R~
            dlgWord = New Form13()                               ''~v008I~''~v012R~
            '*          dlgWord.getCfg()    'setup ListData from '* New() do it             ''~v008I~''~v012R~''~v117R~
            dlgWord.setParent(PswForm1)                                ''~v117I~
        End If                                                         ''~v008I~
        Dim asckey As Integer = AscW(Pcharkey)                           ''~v065I~
        word = dlgWord.applyWords(asckey, PswForm1)                     ''~v065R~
        Return word                                                      ''~v008I~''~v065R~
    End Function                                                            ''~v008I~
    Sub New()                                                          ''~v008I~
        swFilled = False                                                 ''~v008I~
        swUpdated = False                                                 ''~v008I~
        initDlg()                                                      ''~v008I~
    End Sub                                                            ''~v008I~
    Private Sub initDlg()                                              ''~v008I~
        setLang()   'should set CurrentUICulture before InitializeComponent''~v008I~
        InitializeComponent()                                          ''~v008I~
        Form1.setupTitlebarIcon(Me)                                    ''~v008I~
        SB = New SBM(ToolStripStatusLabel1)                              ''~v078I~
        iDGV = New KDGV(DataGridViewWords)                             ''~v078I~
        getCfg()                                                       ''~v008I~
    End Sub                                                            ''~v008I~
    '   Public Sub showDlg()                                               ''~v008I~''~v065R~
    Public Sub showDlg(PswForm1 As Boolean)                            ''~v065I~
        If swShown Then                                                ''~v008I~
            If Not IsDisposed() Then                                   ''~v008I~
                DGV.DefaultCellStyle.BackColor = BGColor               ''~v117I~
                Me.Focus()                                             ''~v008I~
                Exit Sub                                               ''~v008I~
            End If                                                     ''~v008I~
            '*          dlgWord = New Form13()                               ''~v008R~''~v012R~''~v117R~
            '*          dlgWord.showDlg(PswForm1)                                    ''~v008R~''~v012R~''~v117R~
            sharedShowDlg(PswForm1)                                    ''~v117I~
            Exit Sub                                                   ''~v008I~
        End If                                                         ''~v008I~
        initListView()                                                 ''~v008I~
        setSwitchButtonText(swForm1)                                   ''~v123R~
        Show()    'moderess                                            ''~v008I~
        swShown = True                                                 ''~v008I~
        '*      swForm1 = PswForm1                                               ''~v065I~''~v117R~
    End Sub                                                            ''~v008I~
    Private Sub Form13_Closing(sender As System.Object, e As System.ComponentModel.CancelEventArgs) Handles MyBase.Closing ''~v065I~
        '        chkDiscard(e)                                                  ''~v065I~''~v073R~
        Dim rc As Boolean = chkDiscard(e)                                   ''~v073I~
        If rc Then                                                          ''~v073I~
            Me.DialogResult = DialogResult.Yes                           ''~v073I~
        Else                                                           ''~v073I~
            Me.DialogResult = DialogResult.No                            ''~v073I~
        End If                                                         ''~v073I~
    End Sub                                                            ''~v065I~
    Private Sub SaveToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles SaveToolStripMenuItem.Click ''~v012I~
        Try                                                            ''~v117I~
            saveFile()                                                     ''~v012I~
        Catch ex As Exception                                          ''~v117I~
            Form1.exceptionMsg("Form13 Save", ex)                      ''~v117I~
        End Try                                                        ''~v117I~
    End Sub                                                            ''~v012I~
    Private Sub SaveAsToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles SaveAsToolStripMenuItem.Click ''~v012I~
        Try                                                            ''~v117I~
            saveAsFile()                                                   ''~v012I~
        Catch ex As Exception                                          ''~v117I~
            Form1.exceptionMsg("Form13 SaveAs", ex)                    ''~v117I~
        End Try                                                        ''~v117I~
    End Sub                                                            ''~v012I~
    Private Sub CutToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles CutToolStripMenuItem.Click ''~v013I~
        cutRow()                                                       ''~v013I~
    End Sub                                                            ''~v013I~
    Private Sub CopyToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles CopyToolStripMenuItem.Click ''~v013I~
        copyRow()                                                      ''~v013I~
    End Sub                                                            ''~v013I~
    Private Sub PasteToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles PasteToolStripMenuItem.Click ''~v013I~
        pasteRow()                                                     ''~v013I~
    End Sub                                                            ''~v013I~
    '*******************************************************************''~v123I~
    Private Sub SwitchTarget_Click(sender As System.Object, e As System.EventArgs) Handles ToolStripMenuItemSwitchTarget.Click''~v123R~
        Try                                                            ''~v123R~
            switchTarget()                                             ''~v123R~
        Catch ex As Exception                                          ''~v123R~
            Form1.exceptionMsg("Form13 SwitchTarget", ex)              ''~v123R~
        End Try                                                        ''~v123R~
    End Sub                                                            ''~v123R~
    '*******************************************************************''~v123I~
    Private Sub ButtonCancel_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonCancel.Click ''~v065R~
        '       If swUpdated Then                 'Closing will chk discard         ''~v008I~''~v065R~
        '           If Not confirmDiscard() Then                               ''~v008I~''~v065R~
        '               Exit Sub                                               ''~v008I~''~v065R~
        '           End If                                                     ''~v008I~''~v065R~
        '       End If                                                         ''~v008I~''~v065R~
        Me.Close()                                                     ''~v008I~
    End Sub 'resize                                                    ''~v008I~
    Private Sub ButtonOK_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonOK.Click ''~v008R~
        Dim str As String = Nothing                                    ''~v008I~
        If swUpdated Then                                              ''~v008I~
            If Not putCFG() Then                                            ''~v008R~
                Exit Sub                                               ''~v008I~
            End If                                                     ''~v008I~
            swUpdated = False                                            ''~v065I~
        End If                                                         ''~v008I~
        Me.Close()                                                     ''~v008I~
    End Sub 'resize                                                    ''~v008I~
    Private Sub ButtonHelp_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonHelp.Click ''~v008R~
        showHelp()                                                     ''~v008I~
    End Sub                                                            ''~v008I~
    Private Sub Cell_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles DataGridViewWords.CellClick ''~v008M~
        Dim pos As Integer = e.RowIndex                                ''~v008I~
        Dim col As Integer = e.ColumnIndex                             ''~v008I~
        If pos <> -1 Then 'not header                                         ''~v008I~
            Exit Sub                                                   ''~v008I~
        End If                                                         ''~v008I~
        If col = CELLNO_ENABLE OrElse col = CELLNO_CHARKEY Then                  ''~v008I~''~v065R~
            sortDGV(col)                                               ''~v008I~
        End If                                                         ''~v008I~
    End Sub                                                            ''~v008M~
    Private Sub DataGridViewWords_CellContentClick(sender As System.Object, e As System.Windows.Forms.DataGridViewCellEventArgs) Handles DataGridViewWords.CellContentClick ''~v065M~
        Try                                                            ''~v123R~
            Dim col As Integer = e.ColumnIndex                             ''~v065I~
            If col = CELLNO_SEND Then  ' send button                            ''~v065I~
                Dim row As Integer = e.RowIndex                            ''~v065I~
                sendWord(row)                                              ''~v065I~
            End If                                                         ''~v065I~
        Catch ex As Exception                                          ''~v123R~
            Form1.exceptionMsg("Form13 Send", ex)                      ''~v123R~
        End Try                                                        ''~v123R~
    End Sub                                                            ''~v065M~
    Private Sub CellDirtyStateChanged(ByVal sender As System.Object, ByVal e As EventArgs) Handles DataGridViewWords.CurrentCellDirtyStateChanged ''~v012I~
        '** chkbox on dose not immediately committed until current forcus change, so delete flag is ignored at save file''~v012R~
        '** CommitEdit cause CellValuedChanged event                   ''~v012I~
        Dim col = DGV.CurrentCellAddress.X 'column                     ''~v012R~
        If col = CELLNO_ENABLE OrElse col = CELLNO_DELETE Then 'checkbox ''~v012R~
            '           If DGV.IsCurrentCellDirty Then                                  ''~v012I~''~v065R~
            '               DGV.CommitEdit(DataGridViewDataErrorContexts.Commit)    ''~v012I~''~v065R~
            '               swDirty = False                                        ''~v065R~
            '           End If                                                     ''~v012I~''~v065R~
            commitDGV()                                                ''~v065I~
        Else                                                           ''~v065I~
            If DGV.IsCurrentCellDirty Then                             ''~v065I~
                swDirty = True                                           ''~v065I~
            End If                                                     ''~v065I~
        End If                                                         ''~v012I~
        SB.show(SBM.MSGID.CLEAR, "")                                   ''~v078I~
        swUpdated = True                                               ''~v065I~
    End Sub                                                            ''~v012I~
    Private Sub CellValueChanged(ByVal sender As System.Object, ByVal e As DataGridViewCellEventArgs) Handles DataGridViewWords.CellValueChanged ''~v008I~
        ' e.ColumnIndex, e.RowIndex                                        ''~v008I~
        If Not swFilled Then                                                ''~v008I~
            Exit Sub                                                   ''~v008I~
        End If                                                         ''~v008I~
        Dim pos As Integer = e.RowIndex                                ''~v008R~
        If Not swCommitting Then                                         ''~v065I~
            chkValueValidity(pos, False) 'skip errmsg dialog at chkValueValidity''~v065I~
        Else                                                             ''~v065I~
            chkValueValidity(pos, True)                                     ''~v008I~
        End If                                                           ''~v065I~
        swUpdated = True                                                  ''~v008I~
    End Sub                                                            ''~v008I~
    Private Sub showHelp()                                             ''~v008I~
        Dim txt As String                                              ''~v008I~
        If FormOptions.swLangEN Then                                   ''~v008I~
            txt = My.Resources.help_form13E                            ''~v008I~''~v012R~
        Else                                                           ''~v008I~
            txt = My.Resources.help_form13                             ''~v008I~''~v012R~
        End If                                                         ''~v008I~
        MessageBox.Show(txt, Me.Text)                                  ''~v008I~
    End Sub                                                            ''~v008I~
    Private Sub initListView()                                         ''~v008I~
        DGV = DataGridViewWords                                     ''~v008I~
        fillColumn()                                                   ''~v008I~
        '       DGV.DefaultCellStyle.BackColor=Color.PaleGreen                 ''~v075R~
        '*      DGV.DefaultCellStyle.BackColor = Form1.formText.TextBox1.BackColor ''~v075I~''~v117R~
        DGV.DefaultCellStyle.BackColor = BGColor                       ''~v117I~
    End Sub                                                            ''~v008I~
    Private Sub fillColumn()                                           ''~v008I~
        clearDGV()                                                     ''~v008R~
        '       For ii As Integer = 0 To ListData.Length / COLNO - 1           ''~v008I~''~v101R~
        For ii As Integer = 0 To CType(ListData.Length / COLNO, Integer) - 1 ''~v101I~
            Dim enable As Boolean                                      ''~v008I~
            Dim enableid As String = ListData(ii * COLNO)                ''~v008I~
            '           enable = IIf(enableid.CompareTo(ENABLEID_ON) = 0, True, False) ''~v008I~''~v101R~
            enable = CType(IIf(enableid.CompareTo(ENABLEID_ON) = 0, True, False), Boolean) ''~v101I~
            '           DGV.Rows.Add(enable, ListData(ii * COLNO + 1), ListData(ii * COLNO + 2), False) ''~v008R~''~v065R~
            DGV.Rows.Add(enable, ListData(ii * COLNO + 1), strSendID, ListData(ii * COLNO + 2), False) ''~v065I~
        Next                                                           ''~v008I~
        swFilled = True                                                  ''~v008I~
    End Sub                                                            ''~v008I~
    Private Function getListData() As Integer                          ''~v008R~
        Dim rowctr As Integer = getRowCount()                            ''~v008I~
        Dim newrow As Integer = DGV.NewRowIndex                        ''~v008I~
        Dim listctr As Integer = rowctr                                ''~v008I~
        Dim delctr As Integer = 0                                        ''~v008I~
        If newrow >= 0 AndAlso newrow < rowctr Then                           ''~v008I~
            listctr -= 1                                                 ''~v008I~
        End If                                                          ''~v008I~
        If listctr <= 0 Then                                                  ''~v008I~
            Return -1                                                  ''~v008I~
        End If                                                         ''~v008I~
        Dim tmp(listctr * COLNO - 1) As String                            ''~v008R~
        Dim rc2 As Integer, errctr As Integer = 0, errpos As Integer = -1    ''~v008I~
        For ii As Integer = 0 To rowctr - 1                              ''~v008R~
            If ii = newrow Then                                               ''~v008I~
                Continue For                                           ''~v008I~
            End If                                                     ''~v008I~
            '           Dim charkey As String = DGV.Rows(ii).Cells(CELLNO_CHARKEY).Value ''~v008I~''~v065I~''~v101R~
            Dim charkey As String = CType(DGV.Rows(ii).Cells(CELLNO_CHARKEY).Value, String) ''~v101I~
            '           Dim phrase As String = DGV.Rows(ii).Cells(CELLNO_PHRASE).Value ''~v008I~''~v065R~''~v101R~
            Dim phrase As String = CType(DGV.Rows(ii).Cells(CELLNO_PHRASE).Value, String) ''~v101I~
            '           Dim del As Boolean = DGV.Rows(ii).Cells(CELLNO_DELETE).Value ''~v008I~''~v101R~
            Dim del As Boolean = CType(DGV.Rows(ii).Cells(CELLNO_DELETE).Value, Boolean) ''~v101I~
            If charkey Is Nothing Then                                      ''~v065R~
                charkey = ""                                             ''~v065I~
            End If                                                     ''~v065I~
            If phrase Is Nothing Then                                       ''~v065I~
                phrase = ""                                              ''~v065I~
            End If                                                     ''~v065I~
            If charkey.Length = 0 AndAlso phrase.Length = 0 Then                ''~v065R~
                del = True                                               ''~v065I~
            End If                                                     ''~v065I~
            If del Then                                                     ''~v008I~
                delctr += 1                                              ''~v008I~
                Continue For                                           ''~v008I~
            End If                                                     ''~v008I~
            If phrase.Length = 0 Then                                         ''~v065I~
                phrase = " "    ' to get 3item at split                  ''~v065I~
            End If                                                     ''~v065I~
            '           Dim enable As Boolean = DGV.Rows(ii).Cells(CELLNO_ENABLE).Value    ''~v008I~''~v101R~
            Dim enable As Boolean = CType(DGV.Rows(ii).Cells(CELLNO_ENABLE).Value, Boolean) ''~v101I~
            Dim enableid As String                                     ''~v008I~
            '           enableid = IIf(enable, ENABLEID_ON, ENABLEID_OFF)              ''~v008I~''~v101R~
            enableid = CType(IIf(enable, ENABLEID_ON, ENABLEID_OFF), String) ''~v101I~
            rc2 = chkValueValidity(ii, False)                            ''~v008R~
            If rc2 > 0 Then                                                   ''~v008I~
                If errpos < 0 Then                                            ''~v008I~
                    errpos = ii                                          ''~v008I~
                End If                                                  ''~v008I~
                errctr += 1                                              ''~v008I~
            Else                                                       ''~v008I~
                Dim jj As Integer = ii - delctr                            ''~v008I~
                tmp(jj * COLNO) = enableid                             ''~v008R~
                tmp(jj * COLNO + 1) = charkey                            ''~v008R~''~v065R~
                tmp(jj * COLNO + 2) = phrase                             ''~v008R~''~v065R~
            End If                                                     ''~v008I~
        Next                                                           ''~v008I~
        If delctr > 0 Then                                                    ''~v008I~
            Dim newctr As Integer = (listctr - delctr) * COLNO             ''~v008I~
            Dim tmp2(newctr - 1) As String                               ''~v008I~
            System.Array.Copy(tmp, tmp2, newctr)                         ''~v008R~
            ListData = tmp2                                            ''~v008I~
        Else                                                           ''~v008I~
            ListData = tmp                                             ''~v008R~
        End If                                                         ''~v008I~
        If errctr > 0 Then                                                    ''~v008I~
            errIgnoredRow(errctr, errpos)                               ''~v008I~
        End If                                                         ''~v008I~
        Return errctr                                                  ''~v008I~
    End Function                                                       ''~v008R~
    Private Function getDGVword(Pasckey As Integer) As String          ''~v065I~
        Dim word As String = Nothing                                     ''~v065I~
        Dim rowctr As Integer = getRowCount()                          ''~v065I~
        Dim newrow As Integer = DGV.NewRowIndex                        ''~v065I~
        Dim asckey As Integer = Pasckey + 64          'x40, A=x41      ''~v065I~
        ''~v065I~
        If swDirty Then                                                     ''~v065I~
            commitDGV()                                                ''~v065I~
        End If                                                         ''~v065I~
        For ii As Integer = 0 To rowctr - 1                            ''~v065I~
            If ii = newrow Then                                        ''~v065I~
                Continue For                                           ''~v065I~
            End If                                                     ''~v065I~
            '           Dim del As Boolean = DGV.Rows(ii).Cells(CELLNO_DELETE).Value ''~v065I~''~v101R~
            Dim del As Boolean = CType(DGV.Rows(ii).Cells(CELLNO_DELETE).Value, Boolean) ''~v101I~
            If del Then                                                     ''~v065I~
                Continue For                                           ''~v065I~
            End If                                                     ''~v065I~
            '           Dim strkey As String = DGV.Rows(ii).Cells(CELLNO_CHARKEY).Value ''~v065I~''~v101R~
            Dim strkey As String = CType(DGV.Rows(ii).Cells(CELLNO_CHARKEY).Value, String) ''~v101I~
            If strkey Is Nothing OrElse strkey.Length = 0 Then                ''~v065I~
                Continue For                                           ''~v065I~
            End If                                                     ''~v065I~
            Dim charkey As Char = strkey(0)                            ''~v065I~
            Dim asc As Integer = AscW(charkey)                         ''~v065I~
            If asc = asckey OrElse (asc = asckey + 32) Then 'upper or lower case''~v065R~
                '               Dim enable As Boolean = DGV.Rows(ii).Cells(CELLNO_ENABLE).Value ''~v065I~''~v101R~
                Dim enable As Boolean = CType(DGV.Rows(ii).Cells(CELLNO_ENABLE).Value, Boolean) ''~v101I~
                If Not enable Then                                       ''~v065I~
                    swNotEnabled = True                                      ''~v065I~
                Else                                                     ''~v065I~
                    '                   word = DGV.Rows(ii).Cells(CELLNO_PHRASE).Value         ''~v065I~''~v101R~
                    word = CType(DGV.Rows(ii).Cells(CELLNO_PHRASE).Value, String) ''~v101I~
                    If word Is Nothing Then                                     ''~v065I~
                        word = " "                                           ''~v065I~
                    End If                                                 ''~v065I~
                    Exit For                                               ''~v065I~
                End If                                                   ''~v065I~
            End If                                                     ''~v065I~
        Next                                                           ''~v065I~
        Return word                                                    ''~v065I~
    End Function ' getDGVword                                          ''~v065I~
    Private Sub clearDGV()                                             ''~v008I~
        '       DGV.RowCount = 0         '>=1 if AllowUserToAddRows is true(default)''~v008R~
        DGV.Rows.Clear()                          ''~v008I~
    End Sub                                                            ''~v008I~
    '   Private Function getRowCount()                                     ''~v008I~''~v101R~
    Private Function getRowCount() As Integer                          ''~v101I~
        Return DGV.RowCount                                            ''~v008I~
    End Function                                                       ''~v008I~
    Private Function getSelectedPos() As Integer                       ''~v008I~
        Return DGV.CurrentCell.RowIndex                                ''~v008R~
    End Function                                                       ''~v008I~
    Private Function getValidCPos(Pallownewpos As Boolean) As Integer  ''~v013I~
        Dim cpos, maxctr As Integer                                     ''~v013I~
        cpos = getSelectedPos()                                          ''~v013I~
        maxctr = getRowCount()                                        ''~v013I~
        If cpos < 0 OrElse cpos >= maxctr Then                                  ''~v013I~
            Return -1                                                  ''~v013I~
        End If                                                         ''~v013I~
        If Not Pallownewpos Then                                            ''~v013I~
            If cpos = DGV.NewRowIndex Then                                   ''~v013I~
                Return -1                                              ''~v013I~
            End If                                                     ''~v013I~
        End If                                                         ''~v013I~
        Return cpos                                                    ''~v013I~
    End Function                                                       ''~v013I~
    Private Function getValidCPos(Pallownewpos As Boolean, Prow As Integer) As Integer ''~v065I~
        Dim cpos, maxctr As Integer                                    ''~v065I~
        cpos = Prow                                                    ''~v065I~
        maxctr = getRowCount()                                         ''~v065I~
        If cpos < 0 OrElse cpos >= maxctr Then                         ''~v065I~
            Return -1                                                  ''~v065I~
        End If                                                         ''~v065I~
        If Not Pallownewpos Then                                       ''~v065I~
            If cpos = DGV.NewRowIndex Then                             ''~v065I~
                Return -1                                              ''~v065I~
            End If                                                     ''~v065I~
        End If                                                         ''~v065I~
        Return cpos                                                    ''~v065I~
    End Function                                                       ''~v065I~
    Private Function putCFG() As Boolean                               ''~v008R~
        Dim errctr = getListData()                                       ''~v008I~
        If errctr > 0 Then                                                    ''~v008I~
            Return False                                               ''~v008R~
        End If                                                         ''~v008I~
        If errctr < 0 Then     'null                                         ''~v008I~
            cfgKeys = ""                                                  ''~v008I~
        Else                                                           ''~v008I~
            cfgKeys = String.Join(";", ListData)                       ''~v008R~
        End If                                                         ''~v008I~
        My.Settings.CFGF13_Words = cfgKeys                         ''~v008I~''~v065R~
        Return True                                                    ''~v008I~
    End Function                                                       ''~v008R~
    Private Sub getCfg()                                               ''~v008I~
        If cfgKeys.Length = 0 Then                              ''~v008I~''~v065R~
            ListData = {""}                                            ''~v008I~
        Else                                                           ''~v008I~
            ListData = cfgKeys.Split(";"c)                             ''~v008I~
        End If                                                         ''~v008I~
    End Sub                                                            ''~v008I~
    Private Sub setLang()                                              ''~v008I~
        FormOptions.setLang()                                          ''~v008I~
    End Sub                                                            ''~v008I~
    Private Function confirmDiscard() As Boolean                       ''~v008I~
        If MessageBox.Show(Rstr.getStr("STR_MSG_CONFIRM_DISCARD_WORDS_UPDATE"), Me.Text, MessageBoxButtons.YesNo) = DialogResult.No Then ''~v008I~''~v065R~
            Return False                                               ''~v008I~
        End If                                                         ''~v008I~
        Return True                                                    ''~v008I~
    End Function                                                       ''~v008I~
    Private Function chkValueValidity(Ppos As Integer, PswHandler As Boolean) As Integer ''~v008R~
        '       Dim charkey As String = DGV.Rows(Ppos).Cells(CELLNO_CHARKEY).Value   ''~v008I~''~v065R~''~v101R~
        Dim charkey As String = CType(DGV.Rows(Ppos).Cells(CELLNO_CHARKEY).Value, String) ''~v101I~
        '       Dim phrase As String = DGV.Rows(Ppos).Cells(CELLNO_PHRASE).Value       ''~v008I~''~v065R~''~v101R~
        Dim phrase As String = CType(DGV.Rows(Ppos).Cells(CELLNO_PHRASE).Value, String) ''~v101I~
        Dim rc As Integer = 0                                            ''~v008R~
        Dim errstr As String = ""                                           ''~v008I~
        Dim cell As Integer = 0                                        ''~v078I~
        If charkey Is Nothing OrElse charkey.Trim().Length = 0 Then               ''~v008R~''~v012R~''~v065R~
        Else                                                           ''~v008I~
            If charkey.IndexOf(";"c) >= 0 OrElse charkey.Length > 1 Then        ''~v008I~''~v065R~
                errstr = charkey                                           ''~v008I~''~v065R~
                cell = CELLNO_CHARKEY                                  ''~v078I~
                rc = 4                                                 ''~v008R~
            End If                                                     ''~v008R~
        End If                                                         ''~v008I~
        If phrase Is Nothing OrElse phrase.Trim().Length = 0 Then                 ''~v008I~''~v012R~''~v065R~
        Else                                                           ''~v008I~
            If phrase.IndexOf(";"c) >= 0 Then                            ''~v008I~''~v065R~
                If rc < 4 Then                                                ''~v008I~
                    errstr = phrase                                      ''~v008R~''~v065R~
                    cell = CELLNO_PHRASE                                   ''~v078I~
                    rc = 4                                             ''~v008R~
                End If                                                 ''~v008I~
            End If                                                     ''~v008R~
        End If                                                         ''~v008I~
        If rc = 4 Then                                               ''~v008R~''~v065I~
            '           If PswHandler Then                                             ''~v008I~''~v065R~''~v078R~
            Dim errinfo As String = "Row-" & (Ppos + 1)                ''~v008I~
            '               MessageBox.Show(String.Format(Rstr.getStr("STR_ERR_MSG_WORDS_ERRVALUE"), errstr), Me.Text) ''~v008I~''~v065R~''~v078R~
            '           End If                                                      ''~v008I~''~v078R~
            Dim msg As String = String.Format(Rstr.getStr("STR_ERR_MSG_WORDS_ERRVALUE"), errstr) ''~v078I~
            SB.show(msg, True)   ' delayed set text after cleared      ''~v078I~
            iDGV.setSelectedPos(Ppos, cell)                            ''~v078I~
            swInvalid = True                                             ''~v065I~
        End If
        Return rc ''~v008I~
    End Function                                                            ''~v008I~
    Private Sub errIgnoredRow(Perrctr As Integer, Perrrow As Integer)   ''~v008I~
        MessageBox.Show(String.Format(Rstr.getStr("STR_ERR_MSG_WORDS_ROW_IGNORED"), Perrrow + 1, Perrctr), Me.Text) ''~v008I~''~v012R~''~v065R~
    End Sub                                                            ''~v008I~
    Private Sub sortDGV(Pcolumn As Integer)                            ''~v008I~
        DGV.Sort(New DGVComparer(Pcolumn))                              ''~v008I~
    End Sub                                                            ''~v008I~
    Private Function applyWords(Pasckey As Integer, PswForm1 As Boolean) As String ''~v065R~
        Dim word As String = Nothing                                     ''~v065I~
        If Not (Pasckey >= 1 AndAlso Pasckey <= 26) Then                       ''~v065I~
            Return word                                                ''~v065I~
        End If                                                         ''~v065I~
        Dim asckey As Integer = Pasckey + 64          'x40, A=x41                                ''~v065I~
        swNotEnabled = False                                             ''~v065I~
        If swShown AndAlso Not IsDisposed() Then                      ''~v065I~
            word = getDGVword(Pasckey)                                   ''~v065I~
        Else                                                           ''~v065I~
            '           For ii As Integer = 0 To ListData.Length / COLNO - 1           ''~v008I~''~v101R~
            For ii As Integer = 0 To CType(ListData.Length / COLNO, Integer) - 1 ''~v101I~
                Dim enableid As String = ListData(ii * COLNO)              ''~v008I~
                Dim strkey As String = ListData(ii * COLNO + 1)           ''~v008I~''~v065R~
                If strkey.Length > 0 Then                                     ''~v065I~
                    Dim charkey As Char = strkey(0)                    ''~v065R~
                    Dim asc As Integer = AscW(charkey)                 ''~v065R~
                    If asc = asckey OrElse (asc = asckey + 32) Then 'upper or lower case''~v065R~
                        If enableid.CompareTo(ENABLEID_ON) = 0 Then                         ''~v008I~''~v065M~
                            word = ListData(ii * COLNO + 2)                ''~v065R~
                            Exit For                                       ''~v065R~
                        Else                                                   ''~v065I~
                            swNotEnabled = True                                  ''~v065I~
                        End If                                                     ''~v008I~''~v065M~
                    End If                                             ''~v065R~
                End If                                                 ''~v065I~
            Next                                                           ''~v008I~
        End If                                                         ''~v065I~
        If word Is Nothing Then                                             ''~v065I~
            If swNotEnabled Then                                            ''~v065I~
                errNotEnabledKey(PswForm1, asckey)                         ''~v065I~
            Else                                                       ''~v065I~
                errNotRegisteredKey(PswForm1, asckey)                       ''~v065R~
            End If                                                     ''~v065I~
        End If                                                         ''~v065I~
        Return word                                                    ''~v065R~
    End Function    'applyWords                                                        ''~v008I~''~v065R~
    '************************************************************************************''~v008I~
    Class DGVComparer                                                  ''~v008I~
        Implements System.Collections.IComparer                       ''~v008R~
        Private comp As Comparer                                       ''~v008I~
        Private col As Integer                                         ''~v008I~
        Public Sub New(Pcolumn As Integer)                             ''~v008I~
            col = Pcolumn                                                ''~v008I~
            comp = New Comparer(System.Globalization.CultureInfo.CurrentCulture) ''~v008I~
        End Sub                                                        ''~v008I~
        Public Function Compare(Prow1 As Object, Prow2 As Object) As Integer Implements System.Collections.IComparer.Compare ''~v008I~
            Dim rc As Integer                                          ''~v008I~
            Dim row1 As DataGridViewRow = CType(Prow1, DataGridViewRow)   ''~v008I~
            Dim row2 As DataGridViewRow = CType(Prow2, DataGridViewRow)   ''~v008I~
            If col = CELLNO_ENABLE Then                                        ''~v008I~
                '               Dim enable1 = row1.Cells(col).Value                      ''~v008I~''~v101R~
                '               Dim enable2 = row2.Cells(col).Value                      ''~v008I~''~v101R~
                Dim enable1 As Boolean = CType(row1.Cells(col).Value, Boolean) ''~v101I~
                Dim enable2 As Boolean = CType(row2.Cells(col).Value, Boolean) ''~v101I~
                '               rc = IIf(enable1 = enable2, 0, IIf(enable1, -1, 1))            ''~v008I~''~v101R~
                rc = CType(IIf(enable1 = enable2, 0, CType(IIf(enable1, -1, 1), Integer)), Integer) ''~v101I~
            Else                                                       ''~v008I~
                '               Dim str1 = row1.Cells(col).Value                         ''~v008I~''~v101R~
                '               Dim str2 = row2.Cells(col).Value                         ''~v008I~''~v101R~
                Dim str1 As String = CType(row1.Cells(col).Value, String) ''~v101I~
                Dim str2 As String = CType(row2.Cells(col).Value, String) ''~v101I~
                If str1 Is Nothing Then                                      ''~v008I~
                    '                   rc = IIf(str2 Is Nothing, 0, 1)                        ''~v008I~''~v101R~
                    rc = CType(IIf(str2 Is Nothing, 0, 1), Integer)     ''~v101I~
                ElseIf str2 Is Nothing Then                                 ''~v008I~
                    rc = -1                                              ''~v008I~
                Else                                                   ''~v008I~
                    rc = System.String.Compare(str1, str2, True, System.Globalization.CultureInfo.CurrentCulture) 'true ignore case''~v008I~
                End If                                                 ''~v008I~
            End If
            Return rc ''~v008I~
        End Function                                                   ''~v008I~
    End Class                                                          ''~v008I~
    '************************************************************************************''~v008I~
    Private Sub Form13_Load(sender As System.Object, e As System.EventArgs) Handles MyBase.Load ''~v012R~
        loadMRUList()                                                  ''~v012I~
    End Sub                                                            ''~v012I~
    Private Sub loadMRUList()                                          ''~v012I~
        MRU.clearMRUList(mruID)    ' loadMRUList use add method        ''~v012R~
        MRU.loadMRUListSub(mruID)                                      ''~v012I~
        setMRUListMenu()                                               ''~v012I~
    End Sub                                                            ''~v012I~
    Private Sub setMRUListMenu()                                       ''~v012I~
        selectMRUList()                                                ''~v012I~
        Dim itemMRU As ToolStripMenuItem = selectMenuItem()            ''~v012I~
        Dim ctr As Integer = MRUList.Count                             ''~v012I~
        itemMRU.DropDownItems.Clear()                                  ''~v012I~
        For ii As Integer = 0 To ctr                                   ''~v012I~
            If (ii > ClassMRU.MRULISTSZ) Then                          ''~v012I~
                Exit For                                               ''~v012I~
            End If                                                     ''~v012I~
            Dim mruitem As System.Windows.Forms.ToolStripMenuItem      ''~v012I~
            mruitem = New System.Windows.Forms.ToolStripMenuItem()     ''~v012I~
            mruitem.Size = New System.Drawing.Size(152, 22)            ''~v012I~
            If ii = 0 Then                                             ''~v012I~
                mruitem.Text = Rstr.MENU_NEWFILE                       ''~v012I~
            Else                                                       ''~v012I~
                mruitem.Text = MRUList(ii - 1)                         ''~v012I~
            End If                                                     ''~v012I~
            mruitem.Name = "MRUWordsFile"                         ''~v012I~''~v065R~
            AddHandler mruitem.Click, AddressOf MRU_Click              ''~v012I~
            itemMRU.DropDownItems.Add(mruitem)                         ''~v012I~
        Next                                                           ''~v012I~
    End Sub                                                            ''~v012I~
    Private Sub selectMRUList()                                        ''~v012I~
        MRUList = MRU.selectMRUList(mruID)                             ''~v012I~
    End Sub                                                            ''~v012I~
    Private Function selectMenuItem() As ToolStripMenuItem             ''~v012I~
        Dim item As ToolStripMenuItem                                  ''~v012I~
        item = OpenToolStripMenuItem                                   ''~v012I~
        Return item                                                    ''~v012I~
    End Function                                                       ''~v012I~
    Private Sub MRU_Click(sender As System.Object, e As System.EventArgs) ' Handles MRUDictonaryFile.Click''~v012R~
        Dim item = DirectCast(sender, ToolStripMenuItem)               ''~v012I~
        Dim fnm As String                                              ''~v012I~
        fnm = item.Text                                                ''~v012I~
        If fnm.CompareTo(Rstr.MENU_NEWFILE) = 0 Then                   ''~v012I~
            openNewWordsFile()                                    ''~v012I~''~v065R~
        Else                                                           ''~v012I~
            insertMRUList(fnm)                                         ''~v012I~
            openFile(fnm)                                              ''~v012I~
        End If                                                         ''~v012I~
    End Sub                                                            ''~v012I~
    Private Sub openNewWordsFile()                                ''~v012R~''~v065R~
        OpenFileDialog1.Filter = Rstr.getStr("STR_FILTER_WORDS")  ''~v012I~''~v065R~
        OpenFileDialog1.FileName = ""                                  ''~v012R~
        OpenFileDialog1.AddExtension = True   'add extension if missing''~v012I~
        OpenFileDialog1.DefaultExt = DEFAULT_EXT                       ''~v012I~
        OpenFileDialog1.FilterIndex = filterIndex                      ''~v012I~
        If OpenFileDialog1.ShowDialog() = DialogResult.OK Then         ''~v012I~
            Dim fnm As String = OpenFileDialog1.FileName               ''~v012I~
            insertMRUList(fnm)                                         ''~v012I~
            Dim basename As String = System.IO.Path.GetFileNameWithoutExtension(fnm) ''~v012I~
            filterIndex = OpenFileDialog1.FilterIndex    'save for next open''~v012I~
            openFile(fnm)                                              ''~v012I~
        End If                                                         ''~v012I~
    End Sub                                                            ''~v012I~
    Public Sub insertMRUList(Pfnm As String)                           ''~v012I~
        MRU.insertMRUList(mruID, Pfnm)      '                          ''~v012I~
        setMRUListMenu()                                               ''~v012I~
        saveMRUList()                                                  ''~v012I~
    End Sub                                                            ''~v012I~
    Private Sub saveMRUList()                                          ''~v012I~
        MRU.saveMRUList(mruID)                                         ''~v012I~
    End Sub                                                            ''~v012I~
    '*************************************************************     ''~v012I~
    Private Sub openFile(Pfnm As String)                               ''~v012I~
        Dim tmp As String() = readText(Pfnm)                             ''~v012I~
        If tmp Is Nothing Then                                              ''~v012I~
            Exit Sub                                                   ''~v012I~
        End If                                                         ''~v012I~
        ListData = tmp                                                   ''~v012I~
        fillColumn()                                                   ''~v012I~
        swUpdated = True                                               ''~v159I~
        setTitle(Pfnm)                                                 ''~v012I~
        'MessageBox.Show(Pfnm, Rstr.getStr("STR_INFO_MSG_WORDS_LOADED")) ''~v013R~''~v065R~''~v078R~
        SB.show(SBM.MSGID.LOAD, Pfnm)                                   ''~v078I~
    End Sub                                                            ''~v012I~
    Private Sub setTitle(Pfnm As String)                               ''~v012I~
        Dim newtitle As String, oldtitle As String = Me.Text           ''~v012I~
        Dim pos As Integer = oldtitle.IndexOf("="c)                    ''~v012I~
        If pos > 0 Then                                                ''~v012I~
            newtitle = oldtitle.Substring(0, pos + 1) & Pfnm           ''~v012I~
        Else                                                           ''~v012I~
            newtitle = oldtitle & "=" & Pfnm                           ''~v012I~
        End If                                                         ''~v012I~
        Me.Text = newtitle                                             ''~v012M~
    End Sub                                                            ''~v012I~
    '*************************************************************     ''~v012I~
    Private Function readText(Pfnm As String) As String()              ''~v012I~
        Dim sr As StreamReader                                         ''~v012I~
        Dim linectr As Integer = 0                                              ''~v012I~
        Dim line As String                                             ''~v012I~
        Dim rc As String() = Nothing                                     ''~v012I~
        saveFilename = Pfnm                                    ''~v012I~
        If (Not (System.IO.File.Exists(Pfnm))) Then                    ''~v012I~
            Form1.NotFound(Pfnm)                                      ''~v012I~
            Return Nothing                                            ''~v012I~
        End If                                                         ''~v012I~
        Try                                                            ''~v012I~
            sr = New StreamReader(Pfnm, fileEncoding)                  ''~v012R~
            Do While sr.Peek() >= 0                                      ''~v012I~
                line = sr.ReadLine()                                     ''~v012I~
                linectr += 1                                             ''~v012I~
            Loop                                                       ''~v012I~
            sr.Close()                                                 ''~v012I~
            Dim tmp(linectr * COLNO - 1) As String                     ''~v012I~
            sr = New StreamReader(Pfnm, fileEncoding)                   ''~v012R~
            linectr = 0                                                  ''~v012I~
            Do While sr.Peek() >= 0                                      ''~v012I~
                line = sr.ReadLine()                                     ''~v012I~
                Dim tmp2 As String() = line.Split(";"c)                  ''~v012I~
                If Not formatChk(tmp2) Then  'err                           ''~v012I~
                    sr.Close()                                         ''~v103I~
                    errLineFormat(linectr + 1, line)                      ''~v012I~
                    Return Nothing                                     ''~v012I~
                End If                                                 ''~v012I~
                tmp(linectr * COLNO) = tmp2(0)                             ''~v012I~
                tmp(linectr * COLNO + 1) = tmp2(1)                           ''~v012I~
                tmp(linectr * COLNO + 2) = tmp2(2)                           ''~v012I~
                linectr += 1                                             ''~v012I~
            Loop                                                       ''~v012I~
            sr.Close()                                                 ''~v012I~
            rc = tmp                                                    ''~v012I~
        Catch ex As Exception                                          ''~v012I~
            Form1.ReadError(Pfnm, ex)                                  ''~v012I~
            Return Nothing                                             ''~v012I~
        End Try                                                        ''~v012I~
        Return rc                                                      ''~v012I~
    End Function                                                       ''~v012I~
    '*************************************************************     ''~v012I~
    Private Function formatChk(Pdata As String()) As Boolean         ''~v012I~
        If Pdata.Length < 3 Then                                              ''~v012I~
            Return False                                               ''~v012I~
        End If                                                         ''~v012I~
        Dim enableid = Pdata(0)                                          ''~v012I~
        '       If enableid.CompareTo(ENABLEID_ON) OrElse enableid.CompareTo(ENABLEID_OFF) Then ''~v012I~''~v102R~
        '       Else                                                           ''~v012I~''~v102R~
        If enableid.CompareTo(ENABLEID_ON) <> 0 AndAlso enableid.CompareTo(ENABLEID_OFF) <> 0 Then ''~v102I~
            Return False                                               ''~v012I~
        End If                                                         ''~v012I~
        If (Pdata(1).Trim()).Length > 1 Then                           ''~v012I~''~v065R~
            Return False                                               ''~v012I~
        End If                                                         ''~v012I~
        If Pdata(2).Length = 0 Then                           ''~v012I~''~v065R~
            Return False                                               ''~v012I~
        End If                                                         ''~v012I~
        Return True                                                    ''~v012I~
    End Function                                                       ''~v012I~
    '*************************************************************     ''~v012I~
    Private Sub errLineFormat(Plineno As Integer, Pline As String)      ''~v012I~
        MessageBox.Show(Pline, String.Format(Rstr.getStr("STR_ERR_MSG_WORDS_FILE_LINE_FORMAT"), Plineno + 1)) ''~v012R~''~v065R~
    End Sub                                                            ''~v012I~
    '*************************************************************     ''~v012I~
    Private Sub saveFile()                                             ''~v012I~
        If saveFilename Is Nothing Then                                     ''~v012I~
            MessageBox.Show(Rstr.getStr("STR_ERR_MSG_WORDS_FILE_NO_SAVE_FILE"), Me.Text) ''~v012R~''~v065R~
            Exit Sub                                                   ''~v012I~
        End If                                                         ''~v012I~
        saveFile(saveFilename)                                         ''~v012R~
        SB.show(SBM.MSGID.SAVE, saveFilename)                           ''~v078I~
    End Sub                                                            ''~v012I~
    Private Sub saveAsFile()                             ''~v012I~
        SaveFileDialog1.Filter = Rstr.getStr("STR_FILTER_WORDS")  ''~v012I~''~v065R~
        If SaveFileDialog1.ShowDialog() = DialogResult.OK Then         ''~v012I~
            Dim fnm As String = SaveFileDialog1.FileName               ''~v012I~
            If saveFile(fnm) Then                                           ''~v012R~
                saveFilename = fnm                                       ''~v012I~
                setTitle(fnm)                                          ''~v012I~
                SB.show(SBM.MSGID.SAVEAS, saveFilename)                ''~v078I~
            End If                                                     ''~v012I~
        End If                                                         ''+v012I~                                                      ''~v012I~
    End Sub                                                            ''~v012I~
    Private Function commitDGV() As Boolean                            ''~v065I~
        Try                                                            ''~v065I~
            If DGV.IsCurrentCellDirty Then                             ''~v065I~
                swCommitting = True                                          ''~v065I~
                DGV.CommitEdit(DataGridViewDataErrorContexts.Commit)   ''~v065I~
                swCommitting = False                                         ''~v065I~
                swDirty = False                                          ''~v065I~
            End If                                                     ''~v065I~
        Catch ex As Exception                                          ''~v065I~
            Return False                                               ''~v065I~
        End Try                                                        ''~v065I~
        Return True
    End Function ''~v065I~
    Private Function saveFile(Pfnm As String) As Boolean               ''~v012R~
        swInvalid = False                                                ''~v065I~
        commitDGV()                                                    ''~v065I~
        If swInvalid Then ' set chkvalidity() from cellchanged                ''~v065I~
            Return False                                               ''~v065I~
        End If                                                         ''~v065I~
        Try                                                            ''~v012I~
            Dim str As String = getFileData()                             ''~v012I~
            If str IsNot Nothing Then                                       ''~v065I~
                System.IO.File.WriteAllText(Pfnm, str, fileEncoding)       ''~v012R~''~v065R~
                insertMRUList(Pfnm)                                        ''~v012I~''~v065R~
                '               MessageBox.Show(Pfnm, Rstr.MSG_INFO_SAVED)                 ''~v012I~''~v065R~''~v078R~
            End If                                                     ''~v065I~
        Catch ex As Exception                                          ''~v012I~
            Form1.WriteError(Pfnm, ex)                                 ''~v012I~
            Return False                                               ''~v012I~
        End Try                                                        ''~v012I~
        Return True                                                    ''~v012I~
    End Function                                                            ''~v012I~
    Private Function getFileData() As String                           ''~v012I~
        Dim errctr = getListData()                                     ''~v012I~
        Dim str As String                                              ''~v012I~
        Dim sb As New StringBuilder()                                  ''~v012I~
        If errctr > 0 Then                                             ''~v012I~
            Return Nothing                                               ''~v012I~''~v065R~
        End If                                                         ''~v012I~
        If errctr < 0 Then     'null                                   ''~v012I~
            str = ""                                                   ''~v012I~
        Else                                                           ''~v012I~
            '           For ii As Integer = 0 To ListData.Length / COLNO - 1       ''~v012I~''~v101R~
            For ii As Integer = 0 To CType(ListData.Length / COLNO, Integer) - 1 ''~v101I~
                sb.Append(ListData(ii * COLNO) & ";" & ListData(ii * COLNO + 1) & ";" & ListData(ii * COLNO + 2) & vbCrLf) ''~v012I~
            Next                                                       ''~v012I~
            str = sb.ToString()                                          ''~v012I~
        End If                                                         ''~v012I~
        Return str                                                     ''~v012I~
    End Function                                                       ''~v012I~
    '*************************************************************     ''~v013I~
    Private Sub cutRow()                                               ''~v013I~
        commitDGV()                                                    ''~v077I~
        '       Dim cpos As Integer = getCurrentRowData(CPenable, CPcharkey, CPphrase) ''~v013R~''~v065R~''~v074R~
        Dim cpos As Integer = getCurrentRowData(CPenable, CPcharkey, CPButtonLabel, CPphrase) ''~v074I~
        If cpos >= 0 Then                                                     ''~v013I~
            CPstatus = CPSTATUS_CUT                                      ''~v013I~
            CPcutrow = cpos                                              ''~v013I~
            SB.show(SBM.MSGID.CUT, CPphrase)                           ''~v078I~
        End If                                                         ''~v013I~
    End Sub                                                            ''~v013I~
    Private Sub copyRow()                                              ''~v013I~
        commitDGV()                                                    ''~v077I~
        '       If getCurrentRowData(CPenable, CPcharkey, CPphrase) >= 0 Then             ''~v013R~''~v065R~''~v074R~
        If getCurrentRowData(CPenable, CPcharkey, CPButtonLabel, CPphrase) >= 0 Then ''~v074I~
            CPstatus = CPSTATUS_COPY                                     ''~v013I~
            SB.show(SBM.MSGID.COPY, CPphrase)                          ''~v078I~
        End If                                                         ''~v013I~
    End Sub                                                            ''~v013I~
    Private Sub pasteRow()                                             ''~v013I~
        Dim cutrow As Integer
        If CPstatus = CPSTATUS_NONE Then                                      ''~v013I~
            Exit Sub                                                   ''~v013I~
        End If                                                         ''~v013I~
        Dim cpos As Integer = getValidCPos(True) ' true:allow addrow    ''~v013I~
        '       DGV.Rows.Insert(cpos, CPenable, CPcharkey, CPphrase, False) 'false:not deleted''~v013I~''~v065R~''~v074R~
        DGV.Rows.Insert(cpos, CPenable, CPcharkey, CPButtonLabel, CPphrase, False) 'false:not deleted''~v074I~
        swUpdated = True                                               ''~v013I~
        If CPstatus = CPSTATUS_CUT Then                                ''~v013I~
            If cpos <= CPcutrow Then                                          ''~v013I~
                cutrow = CPcutrow + 1                                      ''~v013I~
            Else                                                       ''~v013I~
                cutrow = CPcutrow                                        ''~v013I~
            End If                                                     ''~v013I~
            DGV.Rows.RemoveAt(cutrow)                                  ''~v013I~
            SB.show(SBM.MSGID.CUT_PASTE, CPphrase)                      ''~v078I~
            CPstatus = CPSTATUS_CUTDONE                                ''~v080I~
        Else                                                           ''~v078I~
            SB.show(SBM.MSGID.COPY_PASTE, CPphrase)                     ''~v078I~
        End If                                                         ''~v013I~
    End Sub                                                            ''~v013I~
    '   Private Function getCurrentRowData(ByRef Ppenable As Boolean, ByRef Ppcharkey As String, ByRef Ppphrase As String) As Integer ''~v013R~''~v065R~''~v074R~
    Private Function getCurrentRowData(ByRef Ppenable As Boolean, ByRef Ppcharkey As String, ByRef PpbuttonLabel As String, ByRef Ppphrase As String) As Integer ''~v074I~
        Dim cpos As Integer = getValidCPos(False) ' false:not allow addrow''~v013I~
        If cpos < 0 Then                                                      ''~v013I~
            Return -1                                                  ''~v013R~
        End If                                                         ''~v013I~
        '       Ppenable = DGV.Rows(cpos).Cells(CELLNO_ENABLE).Value               ''~v013I~''~v101R~
        '       Ppcharkey = DGV.Rows(cpos).Cells(CELLNO_CHARKEY).Value                 ''~v013I~''~v065R~''~v101R~
        '       Ppphrase = DGV.Rows(cpos).Cells(CELLNO_PHRASE).Value                 ''~v013I~''~v065R~''~v101R~
        '       PpbuttonLabel = DGV.Rows(cpos).Cells(CELLNO_SEND).Value         ''~v074I~''~v101R~
        Ppenable = CType(DGV.Rows(cpos).Cells(CELLNO_ENABLE).Value, Boolean) ''~v101I~
        Ppcharkey = CType(DGV.Rows(cpos).Cells(CELLNO_CHARKEY).Value, String) ''~v101I~
        Ppphrase = CType(DGV.Rows(cpos).Cells(CELLNO_PHRASE).Value, String) ''~v101I~
        PpbuttonLabel = CType(DGV.Rows(cpos).Cells(CELLNO_SEND).Value, String) ''~v101I~
        Return cpos                                                    ''~v013R~
    End Function                                                            ''~v013I~
    Private Sub errNotRegisteredKey(PswForm1 As Boolean, Pasckey As Integer) ''~v065I~
        Dim msg As String = String.Format(Rstr.getStr("STR_ERR_MSG_NOT_REGISTERED_KEY"), ChrW(Pasckey)) ''~v065I~
        '*        If Not PswForm1 Then                                                ''~v065I~''~v117R~
        '*            If Form1.formText IsNot Nothing Then                            ''~v065I~''~v117R~
        '*                Form1.formText.showStatus(msg)                         ''~v065I~''~v117R~
        '*            End If                                                     ''~v065I~''~v117R~
        '*        End If                                                         ''~v065I~''~v117R~
        Form1.showStatusForChild(PswForm1, msg)                         ''~v117I~
    End Sub                                                            ''~v065I~
    Private Sub errNotEnabledKey(PswForm1 As Boolean, Pasckey As Integer) ''~v065I~
        Dim msg As String = String.Format(Rstr.getStr("STR_ERR_MSG_NOT_ENABLED_KEY"), ChrW(Pasckey)) ''~v065I~
        '*        If Not PswForm1 Then                                           ''~v065I~''~v117R~
        '*            If Form1.formText IsNot Nothing Then                       ''~v065I~''~v117R~
        '*                Form1.formText.showStatus(msg)                         ''~v065I~''~v117R~
        '*            End If                                                     ''~v065I~''~v117R~
        '*        End If                                                         ''~v065I~''~v117R~
        Form1.showStatusForChild(PswForm1, msg)                         ''~v117I~
    End Sub                                                            ''~v065I~
    '   Private Function sendWord(Prow As Integer)                          ''~v065I~''~v101R~
    Private Function sendWord(Prow As Integer) As Boolean              ''~v101I~
        '** by send button                                                 ''~v065I~
        Dim row As Integer = getValidCPos(False, Prow)                                   ''~v065I~
        If row < 0 Then                                                       ''~v065I~
            Return False                                               ''~v065I~
        End If                                                         ''~v065I~
        '       Dim phrase As String = DGV.Rows(row).Cells(CELLNO_PHRASE).Value ''~v065R~''~v101R~
        Dim phrase As String = CType(DGV.Rows(row).Cells(CELLNO_PHRASE).Value, String) ''~v101I~
        If phrase Is Nothing OrElse phrase.Length = 0 Then                      ''~v065I~
            Return False                                               ''~v065I~
        End If                                                         ''~v065I~
        If swForm1 Then   'when showdialog
            '*          Return False                                               ''~v117R~
            callerForm1.restoreSelection()                             ''+v174I~
            callerForm1.undoRedo.setWord(phrase)                      ''~v117I~
        Else                                                           ''~v065I~
            '*          Form1.formText.undoRedo.setWord(phrase)                    ''~v065R~''~v117R~
            callerForm3.restoreSelection()                             ''+v174I~
            callerForm3.undoRedo.setWord(phrase)                      ''~v117I~
        End If
        SB.show(SBM.MSGID.SEND, phrase)                                 ''~v078I~
        Return True ''~v065I~
    End Function                                                            ''~v065I~
    Public Function chkDiscard(e As System.ComponentModel.CancelEventArgs) As Boolean ''~v065I~
        ' rc:true=continue process                                     ''~v065I~
        Dim rc As Boolean = True                                       ''~v065I~
        If swUpdated Then                                              ''~v065I~
            rc = Form1.confirmDiscard(e, Me.Text)                      ''~v065I~
        End If                                                         ''~v065I~
        Return rc                                                      ''~v065I~
    End Function                                                       ''~v065I~
    '*************************************************************************''~v123R~
    Private Sub switchTarget()                                         ''~v123R~
        If swForm1 Then                                                ''~v123R~
        	Dim frm as Form3=Form1.formText                            ''~v123R~
            If IsNothing(frm) OrElse frm.IsDisposed() Then             ''~v123R~
                showStatus(My.Resources.STR_MSG_ERR_NOT_FORM_OPENED)   ''~v123R~
                Exit Sub                                               ''~v123R~
            End If                                                     ''~v123R~
            sharedShowDlg(False)                                       ''~v123R~
            setSwitchButtonText(True)                                  ''~v123R~
        Else                                                           ''~v123R~
        	if Not Form1.MainForm.TBBES.Enabled                        ''~v123R~
                showStatus(My.Resources.STR_MSG_ERR_NOT_FORM_OPENED)   ''~v123R~
                Exit Sub                                               ''~v123R~
            End If                                                     ''~v123R~
            sharedShowDlg(True)                                        ''~v123R~
            setSwitchButtonText(False)                                 ''~v123R~
        End If                                                         ''~v123R~
    End Sub                                                            ''~v123R~
    '*************************************************************************''~v123R~
    Private Sub setSwitchButtonText(PswForm1 As Boolean)               ''~v123R~
        '* PswForm1:current form value                                 ''~v123R~
        Dim col As Color                                               ''~v123R~
        If swForm1 Then                                                ''~v123R~
            col = callerForm1.TBBES.BackColor  '*use myselt because tgt may not opened''~v123R~
        Else                                                           ''~v123R~
            col = callerForm3.TextBox1.BackColor                       ''~v123R~
        End If                                                         ''~v123R~
        ToolStripMenuItemSwitchTarget.BackColor = col                  ''~v123R~
    End Sub                                                            ''~v123R~
    '*************************************************************************''~v123R~
    Private Sub showStatus(Pmsg As String)                             ''~v123R~
        ToolStripStatusLabel1.Text = Pmsg                              ''~v123R~
    End Sub                                                            ''~v123R~
End Class