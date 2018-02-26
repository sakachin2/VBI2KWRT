'CID:''+v158R~:#72                             update#=  123;         ''~v158R~
'************************************************************************************''~v008I~
'v158 2018/02/24 support specification of no translation symbol sush as "─", it may be translated to keisen''~v158I~
'v157 2018/02/24 allow dictionary translation to space(sbcs and dbcs)  ''~v156I~
'v156 2018/02/24 applydictionary may cause invalid object reference    ''~v103I~
'v103 2017/12/16 (BUG)did not closed file for Dialog(Dictionary,Word,Symbol) at format err''~v103I~
'v102 2017/12/16 (BUG)Enable/Disable check  of Dislog(Dictionary,Word,Symbol)''~v102I~
'v101 2017/12/16 Conversion warning                                    ''~v101I~
'v080 2017/10/10 (BUG)2nd paste after cut remove cut pos twice         ''~v080I~
'v078 2017/10/09 dialog status bar                                     ''~v078I~
'v077 2017/10/08 Commit required when cut/copy                         ''~v077I~
'v066 2017/09/25 (Bug)Dictionary dialog;if err line exist saveFile write "False"''~v066I~
'v013 2017/09/16 Cut&PAste support for Dictionary                      ''~v013I~
'v012 2017/09/15 Load/Save/SaveAs from/to disctionary file             ''~v012I~
'v008 2017/09/12 dictionary support                                    ''~v008I~
'************************************************************************************''~v008I~
Imports System.Runtime.InteropServices.Marshal                         ''~v008I~
Imports System.Text
Imports System.IO ''~v008I~
Public Class Form11                                                    ''~v008R~
    '** Dictionary Dialog                                              ''~v008I~
    ''~v008I~
    Private Const COLNO = 3                                            ''~v008I~
    Private Const COLNO_DEL = 4                                        ''~v008I~
    Private Const CELLNO_ENABLE = 0                                      ''~v008I~
    Private Const CELLNO_KANJI = 1                                       ''~v008I~
    Private Const CELLNO_KANA = 2                                        ''~v008I~
    Private Const CELLNO_DELETE = 3                                    ''~v008I~
    Private Const ENABLEID_ON = "1"                                      ''~v008I~
    Private Const ENABLEID_OFF = "0"                                     ''~v008I~''~v012R~
    Private Const DEFAULT_EXT = "dct"                                   ''~v012I~
    Private Const NULL_VALUE = "(null)"                                ''~v157I~
    Private Const SYMBOL_VALUE = "(symbol)"                            ''~v158I~
    Private Shared SdictionarySymbol As String = ""                    ''~v158R~
    ''~v012I~
    '   Private fileEncoding As System.Text.Encoding = System.Text.Encoding.UTF8''~v012R~
    Private fileEncoding As System.Text.Encoding = System.Text.Encoding.Default ''~v012R~
    Private cfgKeys As String = My.Settings.CFGF9_Dictionary           ''~v008I~
    Private DGV As DataGridView                                        ''~v008R~
    Public Shared dlgDictionary As Form11                              ''~v008I~''~v012R~
    Private filterIndex As Integer = 0                                 ''~v012I~
    Private ListData() As String                                       ''~v008I~
    '   Private mruID = ClassMRU.ID_DICTIONARY                          ''~v012I~''~v101R~
    Private mruID As Integer = ClassMRU.ID_DICTIONARY                  ''~v101I~
    Private MRU As ClassMRU = Form1.MainForm.MRU                         ''~v012I~
    Private saveFilename As String                                     ''~v012I~
    Private iDGV As KDGV   'DataGridView wrapper class                 ''~v078I~
    '***************************************************************************''~v008I~
    Private swUpdated As Boolean = False                               ''~v008I~
    Private swShown As Boolean = False                                 ''~v008I~
    Private swFilled As Boolean = False                                ''~v008I~
    Private MRUList As New List(Of String)                             ''~v012I~
    Private CPenable As Boolean                                        ''~v013I~
    Private CPkanji, CPkana As String                                   ''~v013I~
    Private CPstatus As Integer = 0                                      ''~v013I~
    Private Const CPSTATUS_NONE = 0                                      ''~v013I~
    Private Const CPSTATUS_CUT = 1                                       ''~v013I~
    Private Const CPSTATUS_COPY = 2                                      ''~v013I~
    Private Const CPSTATUS_CUTDONE = 3                                 ''~v080I~
    Private CPcutrow As Integer                                        ''~v013I~
    Private swCommitting As Boolean = False                            ''~v066I~
    Private swInvalid As Boolean = False                               ''~v066I~
    Private SB As SBM     'StatusBar                                   ''~v078I~
    '***************************************************************************''~v008I~
    Public Shared Sub sharedShowDlg()                                  ''~v008R~
        If dlgDictionary Is Nothing Then                                     ''~v008I~
            dlgDictionary = New Form11()                               ''~v008R~
        End If                                                         ''~v008I~
        dlgDictionary.showDlg()                                        ''~v008I~
    End Sub                                                            ''~v008I~
    Public Shared Function sharedApplyDictionary(Psbtext As StringBuilder) As StringBuilder ''~v008R~
        If dlgDictionary Is Nothing Then                               ''~v008I~
            dlgDictionary = New Form11()                               ''~v008I~
            dlgDictionary.getCfg()    'setup ListData from             ''~v008I~
        End If                                                         ''~v008I~
        Dim sb As StringBuilder = dlgDictionary.applyDictionary(Psbtext)    ''~v008R~
        Return sb                                                      ''~v008I~
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
        SB = New SBM(ToolStripStatusLabel1)                            ''~v078I~
        iDGV = New KDGV(DataGridViewDictionary)                        ''~v078I~
        getCfg()                                                       ''~v008I~
    End Sub                                                            ''~v008I~
    Public Sub showDlg()                                               ''~v008I~
        If swShown Then                                                ''~v008I~
            If Not IsDisposed() Then                                   ''~v008I~
                Me.Focus()                                             ''~v008I~
                Exit Sub                                               ''~v008I~
            End If                                                     ''~v008I~
            dlgDictionary = New Form11()                               ''~v008R~
            dlgDictionary.showDlg()                                    ''~v008R~
            Exit Sub                                                   ''~v008I~
        End If                                                         ''~v008I~
        initListView()                                                 ''~v008I~
        Show()    'moderess                                            ''~v008I~
        swShown = True                                                 ''~v008I~
    End Sub                                                            ''~v008I~
    Private Sub SaveToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles SaveToolStripMenuItem.Click ''~v012I~
        saveFile()                                                     ''~v012I~
    End Sub                                                            ''~v012I~
    Private Sub SaveAsToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles SaveAsToolStripMenuItem.Click ''~v012I~
        saveAsFile()                                                   ''~v012I~
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
    Private Sub ButtonCancel_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonCancel.Click ''~v008R~
        If swUpdated Then                                              ''~v008I~
            If Not confirmDiscard() Then                               ''~v008I~
                Exit Sub                                               ''~v008I~
            End If                                                     ''~v008I~
        End If                                                         ''~v008I~
        Me.Close()                                                     ''~v008I~
    End Sub 'resize                                                    ''~v008I~
    Private Sub ButtonOK_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonOK.Click ''~v008R~
        Dim str As String = Nothing                                    ''~v008I~
        If swUpdated Then                                              ''~v008I~
            If Not putCFG() Then                                            ''~v008R~
                Exit Sub                                               ''~v008I~
            End If                                                     ''~v008I~
            swUpdated = False                                          ''+v158I~
        End If                                                         ''~v008I~
        Me.Close()                                                     ''~v008I~
    End Sub 'resize                                                    ''~v008I~
    Private Sub ButtonHelp_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonHelp.Click ''~v008R~
        showHelp()                                                     ''~v008I~
    End Sub                                                            ''~v008I~
    Private Sub Cell_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles DataGridViewDictionary.CellClick ''~v008M~
        Dim pos As Integer = e.RowIndex                                ''~v008I~
        Dim col As Integer = e.ColumnIndex                             ''~v008I~
        If pos <> -1 Then 'not header                                         ''~v008I~
            Exit Sub                                                   ''~v008I~
        End If                                                         ''~v008I~
        If col = CELLNO_ENABLE OrElse col = CELLNO_KANJI Then                  ''~v008I~
            sortDGV(col)                                               ''~v008I~
        End If                                                         ''~v008I~
    End Sub                                                            ''~v008M~
    Private Sub CellDirtyStateChanged(ByVal sender As System.Object, ByVal e As EventArgs) Handles DataGridViewDictionary.CurrentCellDirtyStateChanged ''~v012I~
        '** chkbox on dose not immediately committed until current forcus change, so delete flag is ignored at save file''~v012R~
        '** CommitEdit cause CellValuedChanged event                   ''~v012I~
        Dim col = DGV.CurrentCellAddress.X 'column                     ''~v012R~
        If col = CELLNO_ENABLE OrElse col = CELLNO_DELETE Then 'checkbox ''~v012R~
            If DGV.IsCurrentCellDirty Then                                  ''~v012I~
                DGV.CommitEdit(DataGridViewDataErrorContexts.Commit)    ''~v012I~
            End If                                                     ''~v012I~
        End If                                                         ''~v012I~
        SB.show(SBM.MSGID.CLEAR, "")                                   ''~v078I~
    End Sub                                                            ''~v012I~
    '*****************************************************************************************''~v157I~
    Private Sub CellValueChanged(ByVal sender As System.Object, ByVal e As DataGridViewCellEventArgs) Handles DataGridViewDictionary.CellValueChanged ''~v008I~
        ' e.ColumnIndex, e.RowIndex                                        ''~v008I~
        If Not swFilled Then                                                ''~v008I~
            Exit Sub                                                   ''~v008I~
        End If                                                         ''~v008I~
        Dim pos As Integer = e.RowIndex                                ''~v008R~
        If Not swCommitting Then                                         ''~v066I~
            chkValueValidity(pos, False)    'skip errmsg if committing     ''~v066I~
        Else                                                             ''~v066I~
            chkValueValidity(pos, True)                                     ''~v008I~
        End If                                                           ''~v066I~
        swUpdated = True                                                  ''~v008I~
    End Sub                                                            ''~v008I~
    '*****************************************************************************************''~v157I~
    Private Sub chkKanaNull(Prow As Integer)                           ''~v157I~
        Dim kana As String = CType(DGV.Rows(Prow).Cells(CELLNO_KANA).Value, String) ''~v157I~
        If kana Is Nothing OrElse kana.CompareTo("") = 0 Then          ''~v157I~
            DGV.Rows(Prow).Cells(CELLNO_KANA).Value = NULL_VALUE       ''~v157I~
        Else                                                           ''~v157I~
            If kana.Trim().CompareTo("") = 0 Then                      ''~v157I~
                DGV.Rows(Prow).Cells(CELLNO_KANA).Value = "(" & kana & ")" ''~v157I~
            End If                                                     ''~v157I~
        End If                                                         ''~v157I~
    End Sub                                                            ''~v157I~
    '*****************************************************************************************''~v158I~
    Private Sub getSymbolValidate(Prow As Integer)                     ''~v158I~
        Dim kanji As String = CType(DGV.Rows(Prow).Cells(CELLNO_KANJI).Value, String) ''~v158I~
        Dim kana As String = CType(DGV.Rows(Prow).Cells(CELLNO_KANA).Value, String) ''~v158I~
        getSymbol(kanji, kana)                                          ''~v158I~
    End Sub                                                            ''~v158I~
    '*****************************************************************************************''~v157I~
    Private Sub RowLeave(ByVal sender As System.Object, ByVal e As DataGridViewCellEventArgs) Handles DataGridViewDictionary.RowLeave ''~v157I~
        '       chkKanaNull(e.RowIndex)                                        ''~v157I~
    End Sub                                                            ''~v157I~
    '*****************************************************************************************''~v157I~
    Private Sub Rowvalidated(ByVal sender As System.Object, ByVal e As DataGridViewCellEventArgs) Handles DataGridViewDictionary.RowValidated ''~v157I~
        chkKanaNull(e.RowIndex)                                        ''~v157I~
        getSymbolValidate(e.RowIndex)                                  ''~v158I~
    End Sub                                                            ''~v157I~
    '*****************************************************************************************''~v157I~
    Private Sub showHelp()                                             ''~v008I~
        Dim txt As String                                              ''~v008I~
        If FormOptions.swLangEN Then                                   ''~v008I~
            txt = My.Resources.help_form11E                            ''~v008I~
        Else                                                           ''~v008I~
            txt = My.Resources.help_form11                             ''~v008I~
        End If                                                         ''~v008I~
        MessageBox.Show(txt, Me.Text)                                  ''~v008I~
    End Sub                                                            ''~v008I~
    Private Sub initListView()                                         ''~v008I~
        DGV = DataGridViewDictionary                                     ''~v008I~
        fillColumn()                                                   ''~v008I~
    End Sub                                                            ''~v008I~
    '*****************************************************************************************''~v158I~
    Private Sub getSymbol(Pkanji As String, Pkana As String)            ''~v158I~
        If Pkanji Is Nothing Then                                           ''~v158I~
            Exit Sub                                                   ''~v158I~
        End If                                                         ''~v158I~
        If Pkana Is Nothing Then                                            ''~v158I~
            Exit Sub                                                   ''~v158I~
        End If                                                         ''~v158I~
        If String.Compare(Pkanji, SYMBOL_VALUE, True) = 0 Then                    ''~v158I~
            SdictionarySymbol = Pkana                                    ''~v158I~
            If String.Compare(Pkana, NULL_VALUE, True) = 0 Then          ''~v158I~
                SdictionarySymbol = ""                                   ''~v158I~
            End If                                                     ''~v158I~
        End If                                                         ''~v158I~
    End Sub                                                            ''~v158I~
    '*****************************************************************************************''~v158I~
    Public Shared Function chkSymbol(Psymbol As Char) As Boolean        ''~v158I~
        Return SdictionarySymbol.IndexOf(Psymbol) >= 0                   ''~v158I~
    End Function                                                            ''~v158I~
    '*****************************************************************************************''~v158I~
    Private Sub fillColumn()                                           ''~v008I~
        clearDGV()                                                     ''~v008R~
        '       For ii As Integer = 0 To ListData.Length / COLNO - 1           ''~v008I~''~v101R~
        Dim entno As Integer = 0                                        ''~v158I~
        For ii As Integer = 0 To CType(ListData.Length / COLNO, Integer) - 1 ''~v101I~
            Dim enable As Boolean                                      ''~v008I~
            Dim enableid As String = ListData(ii * COLNO)                ''~v008I~
            '           enable = IIf(enableid.CompareTo(ENABLEID_ON) = 0, True, False) ''~v008I~''~v101R~
            enable = CType(IIf(enableid.CompareTo(ENABLEID_ON) = 0, True, False), Boolean) ''~v101I~
            getSymbol(ListData(ii * COLNO + 1), ListData(ii * COLNO + 2)) ''~v158I~
            DGV.Rows.Add(enable, ListData(ii * COLNO + 1), ListData(ii * COLNO + 2), False) ''~v008R~
            entno += 1                                                   ''~v158I~
        Next                                                           ''~v008I~
        If entno = 0 Then                                                     ''~v158I~
            DGV.Rows.Add(True, SYMBOL_VALUE, NULL_VALUE, False)         ''~v158I~
        End If                                                         ''~v158I~
        swFilled = True                                                  ''~v008I~
    End Sub                                                            ''~v008I~
    '*****************************************************************************************''~v157I~
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
            '           Dim del As Boolean = DGV.Rows(ii).Cells(CELLNO_DELETE).Value ''~v008I~''~v101R~
            Dim del As Boolean = CType(DGV.Rows(ii).Cells(CELLNO_DELETE).Value, Boolean) ''~v101I~
            If del Then                                                     ''~v008I~
                delctr += 1                                              ''~v008I~
                Continue For                                           ''~v008I~
            End If                                                     ''~v008I~
            '           Dim enable As Boolean = DGV.Rows(ii).Cells(CELLNO_ENABLE).Value    ''~v008I~''~v101R~
            Dim enable As Boolean = CType(DGV.Rows(ii).Cells(CELLNO_ENABLE).Value, Boolean) ''~v101I~
            Dim enableid As String                                     ''~v008I~
            '           enableid = IIf(enable, ENABLEID_ON, ENABLEID_OFF)              ''~v008I~''~v101R~
            enableid = CType(IIf(enable, ENABLEID_ON, ENABLEID_OFF), String) ''~v101I~
            '           Dim kanji As String = DGV.Rows(ii).Cells(CELLNO_KANJI).Value ''~v008I~''~v101R~
            Dim kanji As String = CType(DGV.Rows(ii).Cells(CELLNO_KANJI).Value, String) ''~v101I~
            '           Dim kana As String = DGV.Rows(ii).Cells(CELLNO_KANA).Value ''~v008I~''~v101R~
            Dim kana As String = CType(DGV.Rows(ii).Cells(CELLNO_KANA).Value, String) ''~v101I~
            rc2 = chkValueValidity(ii, False)                            ''~v008R~
            If rc2 = 2 Then 'kana is Nothing or null                          ''~v157I~
                DGV.Rows(ii).Cells(CELLNO_KANA).Value = NULL_VALUE     ''~v157R~
                rc2 = 0                                                  ''~v157I~
            End If                                                     ''~v157I~
            If rc2 > 0 Then                                                   ''~v008I~
                If errpos < 0 Then                                            ''~v008I~
                    errpos = ii                                          ''~v008I~
                End If                                                  ''~v008I~
                errctr += 1                                              ''~v008I~
            Else                                                       ''~v008I~
                Dim jj As Integer = ii - delctr                            ''~v008I~
                tmp(jj * COLNO) = enableid                             ''~v008R~
                tmp(jj * COLNO + 1) = kanji                            ''~v008R~
                tmp(jj * COLNO + 2) = kana                             ''~v008R~
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
        My.Settings.CFGF9_Dictionary = cfgKeys                         ''~v008I~
        Return True                                                    ''~v008I~
    End Function                                                       ''~v008R~
    Private Sub getCfg()                                               ''~v008I~
        If cfgKeys.CompareTo("") = 0 Then                              ''~v008I~
            ListData = {""}                                            ''~v008I~
        Else                                                           ''~v008I~
            ListData = cfgKeys.Split(";"c)                             ''~v008I~
        End If                                                         ''~v008I~
    End Sub                                                            ''~v008I~
    Private Sub setLang()                                              ''~v008I~
        FormOptions.setLang()                                          ''~v008I~
    End Sub                                                            ''~v008I~
    Private Function confirmDiscard() As Boolean                       ''~v008I~
        If MessageBox.Show(Rstr.getStr("STR_MSG_CONFIRM_DISCARD_DICTIONARY_UPDATE"), Me.Text, MessageBoxButtons.YesNo) = DialogResult.No Then ''~v008I~
            Return False                                               ''~v008I~
        End If                                                         ''~v008I~
        Return True                                                    ''~v008I~
    End Function                                                       ''~v008I~
    '**********************************************************************************''~v157I~
    '*rc:1 err,4:splitter=";" it is used as splitter of save file,2:kana string is null(accept as nullify)''~v157I~
    '**********************************************************************************''~v157I~
    Private Function chkValueValidity(Ppos As Integer, PswHandler As Boolean) As Integer ''~v008R~
        '       Dim kanji As String = DGV.Rows(Ppos).Cells(CELLNO_KANJI).Value   ''~v008I~''~v101R~
        Dim kanji As String = CType(DGV.Rows(Ppos).Cells(CELLNO_KANJI).Value, String) ''~v101I~
        '       Dim kana As String = DGV.Rows(Ppos).Cells(CELLNO_KANA).Value       ''~v008I~''~v101R~
        Dim kana As String = CType(DGV.Rows(Ppos).Cells(CELLNO_KANA).Value, String) ''~v101I~
        Dim rc As Integer = 0                                            ''~v008R~
        Dim errstr As String = ""                                           ''~v008I~
        Dim cell As Integer = 0                                        ''~v078I~
        If kanji Is Nothing OrElse kanji.Trim().CompareTo("") = 0 Then               ''~v008R~''~v012R~
            rc = 1                                                       ''~v008I~
        Else                                                           ''~v008I~
            If kanji.IndexOf(";"c) >= 0 Then                           ''~v008I~
                errstr = kanji                                           ''~v008I~
                cell = CELLNO_KANJI                                    ''~v078I~
                rc = 4                                                 ''~v008R~
            End If                                                     ''~v008R~
        End If                                                         ''~v008I~
        If kana Is Nothing OrElse kana.Trim().CompareTo("") = 0 Then                 ''~v008I~''~v012R~
            If rc = 0 Then                                                    ''~v008I~
                '               rc = 1                                                 ''~v008R~''~v157R~
                If kana Is Nothing OrElse kana.Length = 0 Then          ''~v157R~
                    rc = 2    'id of null                                ''~v157I~
                End If                                                 ''~v157I~
            End If                                                     ''~v008I~
        Else                                                           ''~v008I~
            If kana.IndexOf(";"c) >= 0 Then                            ''~v008I~
                If rc < 4 Then                                                ''~v008I~
                    errstr = kana                                      ''~v008R~
                    cell = CELLNO_KANA                                 ''~v078I~
                    rc = 4                                             ''~v008R~
                End If                                                 ''~v008I~
            End If                                                     ''~v008R~
        End If                                                         ''~v008I~
        If rc = 4 Then                                                   ''~v066I~
            '           If PswHandler Then                                             ''~v008I~''~v078R~
            '           If rc = 4 Then                                               ''~v008R~''~v066R~
            Dim errinfo As String = "Row-" & (Ppos + 1)                ''~v008I~
            '               MessageBox.Show(String.Format(Rstr.getStr("STR_ERR_MSG_DICTIONARY_ERRVALUE"), errstr), Me.Text) ''~v008I~''~v078R~
            '           End If                                                      ''~v008I~''~v066R~
            Dim msg As String = String.Format(Rstr.getStr("STR_ERR_MSG_DICTIONARY_ERRVALUE"), errstr) ''~v078I~
            SB.show(msg, True)   ' delayed set text after cleared      ''~v078I~
            iDGV.setSelectedPos(Ppos, cell)                            ''~v078I~
            '           End If                                                     ''~v078R~
            swInvalid = True                                               ''~v066I~
        End If                                                           ''~v066I~
        Return rc ''~v008I~
    End Function                                                            ''~v008I~
    Private Sub errIgnoredRow(Perrctr As Integer, Perrrow As Integer)   ''~v008I~
        MessageBox.Show(String.Format(Rstr.getStr("STR_ERR_MSG_DICTIONARY_ROW_IGNORED"), Perrrow + 1, Perrctr), Me.Text) ''~v008I~''~v012R~
    End Sub                                                            ''~v008I~
    Private Sub sortDGV(Pcolumn As Integer)                            ''~v008I~
        DGV.Sort(New DGVComparer(Pcolumn))                              ''~v008I~
    End Sub                                                            ''~v008I~
    Private Function applyDictionary(Psbtext As StringBuilder) As StringBuilder ''~v008R~
        Dim applyctr As Integer = 0                                      ''~v008I~
        Dim sb As StringBuilder = Psbtext                                ''~v008R~
        '       For ii As Integer = 0 To ListData.Length / COLNO - 1           ''~v008I~''~v101R~
        Dim ii2 As Integer = 0                                         ''~v103R~
        Try                                                            ''~v156R~
            For ii As Integer = 0 To CType(ListData.Length / COLNO, Integer) - 1 ''~v101I~''~v103R~
                ii2 = ii                                                 ''~v103I~
                Dim enableid As String = ListData(ii * COLNO)              ''~v008I~
                If enableid.CompareTo(ENABLEID_ON) = 0 Then                         ''~v008I~
                    Dim kanji As String = ListData(ii * COLNO + 1)           ''~v008I~
                    If String.Compare(kanji, SYMBOL_VALUE, True) = 0 Then         ''~v158I~
                        Continue For                                   ''~v158I~
                    End If                                             ''~v158I~
                    Dim kana As String = ListData(ii * COLNO + 2)            ''~v008I~
                    If (kana Is Nothing) Then                               ''~v157I~
                        kana = ""                                        ''~v157I~
                    Else                                               ''~v157I~
                        If String.Compare(kana, NULL_VALUE, True) = 0 Then ''~v158I~
                            kana = ""                                  ''~v157I~
                        Else                                           ''~v157I~
                            If kana.Chars(0) = "("c AndAlso kana.Chars(kana.Length - 1) = ")"c AndAlso Trim(kana.Substring(1, kana.Length - 2)).Length = 0 Then ''~v157I~
                                kana = kana.Substring(1, kana.Length - 2)   ''~v157I~
                            End If                                     ''~v157I~
                        End If                                         ''~v157I~
                    End If                                             ''~v157I~
                    sb = sb.Replace(kanji, kana)                              ''~v008I~
                End If                                                     ''~v008I~
            Next                                                           ''~v008I~
        Catch ex As Exception                                          ''~v156R~
            Form1.exceptionMsg("Form11:applyDictionary entryNo=" & ii2, ex)''~v156R~
        End Try                                                        ''~v156R~
        Return sb                                                      ''~v008I~
    End Function                                                            ''~v008I~
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
    Private Sub Form11_Load(sender As System.Object, e As System.EventArgs) Handles MyBase.Load ''~v012I~
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
            mruitem.Name = "MRUDictionaryFile"                         ''~v012I~
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
            openNewDictionaryFile()                                    ''~v012I~
        Else                                                           ''~v012I~
            insertMRUList(fnm)                                         ''~v012I~
            openFile(fnm)                                              ''~v012I~
        End If                                                         ''~v012I~
    End Sub                                                            ''~v012I~
    Private Sub openNewDictionaryFile()                                ''~v012R~
        OpenFileDialog1.Filter = Rstr.getStr("STR_FILTER_DICTIONARY")  ''~v012I~
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
        swUpdated=true                                                 ''~v158I~
        setTitle(Pfnm)                                                 ''~v012I~
        '       MessageBox.Show(Pfnm, Rstr.getStr("STR_INFO_MSG_DICTIONARY_LOADED")) ''~v013R~''~v078R~
        SB.show(SBM.MSGID.LOAD, Pfnm)                                  ''~v078I~
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
        If (Pdata(1).Trim()).CompareTo("") = 0 Then                           ''~v012I~
            Return False                                               ''~v012I~
        End If                                                         ''~v012I~
        If (Pdata(2).Trim()).CompareTo("") = 0 Then                           ''~v012I~
            Return False                                               ''~v012I~
        End If                                                         ''~v012I~
        Return True                                                    ''~v012I~
    End Function                                                       ''~v012I~
    '*************************************************************     ''~v012I~
    Private Sub errLineFormat(Plineno As Integer, Pline As String)      ''~v012I~
        MessageBox.Show(Pline, String.Format(Rstr.getStr("STR_ERR_MSG_DICTIONARY_FILE_LINE_FORMAT"), Plineno + 1)) ''~v012R~
    End Sub                                                            ''~v012I~
    '*************************************************************     ''~v012I~
    Private Sub saveFile()                                             ''~v012I~
        If saveFilename Is Nothing Then                                     ''~v012I~
            MessageBox.Show(Rstr.getStr("STR_ERR_MSG_DICTIONARY_FILE_NO_SAVE_FILE"), Me.Text) ''~v012R~
            Exit Sub                                                   ''~v012I~
        End If                                                         ''~v012I~
        saveFile(saveFilename)                                         ''~v012R~
        SB.show(SBM.MSGID.SAVE, saveFilename)                          ''~v078I~
    End Sub                                                            ''~v012I~
    Private Sub saveAsFile()                             ''~v012I~
        SaveFileDialog1.Filter = Rstr.getStr("STR_FILTER_DICTIONARY")  ''~v012I~
        If SaveFileDialog1.ShowDialog() = DialogResult.OK Then         ''~v012I~
            Dim fnm As String = SaveFileDialog1.FileName               ''~v012I~
            If saveFile(fnm) Then                                           ''~v012R~
                saveFilename = fnm                                       ''~v012I~
                setTitle(fnm)                                          ''~v012I~
                SB.show(SBM.MSGID.SAVEAS, saveFilename)                ''~v078I~
            End If                                                     ''~v012I~
        End If                                                         ''+v012I~                                                      ''~v012I~
    End Sub                                                            ''~v012I~
    Private Function commitDGV() As Boolean                            ''~v066I~
        Try                                                            ''~v066I~
            If DGV.IsCurrentCellDirty Then                             ''~v066I~
                swCommitting = True                                    ''~v066I~
                DGV.CommitEdit(DataGridViewDataErrorContexts.Commit)   ''~v066I~
                swCommitting = False                                   ''~v066I~
            End If                                                     ''~v066I~
        Catch ex As Exception                                          ''~v066I~
            Return False                                               ''~v066I~
        End Try                                                        ''~v066I~
        Return True                                                    ''~v066I~
    End Function                                                       ''~v066I~
    Private Function saveFile(Pfnm As String) As Boolean               ''~v012R~
        swInvalid = False                                              ''~v066I~
        commitDGV()                                                    ''~v066I~
        If swInvalid Then ' set chkvalidity() from cellchanged         ''~v066I~
            Return False                                               ''~v066I~
        End If                                                         ''~v066I~
        Try                                                            ''~v012I~
            Dim str As String = getFileData()                             ''~v012I~
            If str IsNot Nothing Then                                       ''~v066I~
                System.IO.File.WriteAllText(Pfnm, str, fileEncoding)       ''~v012R~
                insertMRUList(Pfnm)                                        ''~v012I~
                '           MessageBox.Show(Pfnm, Rstr.MSG_INFO_SAVED)                 ''~v012I~''~v078R~
            End If                                                     ''~v066I~
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
            '           Return False                                               ''~v012I~''~v066R~
            Return Nothing                                             ''~v066I~
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
        Dim cpos As Integer = getCurrentRowData(CPenable, CPkanji, CPkana) ''~v013R~
        If cpos >= 0 Then                                                     ''~v013I~
            CPstatus = CPSTATUS_CUT                                      ''~v013I~
            CPcutrow = cpos                                              ''~v013I~
            SB.show(SBM.MSGID.CUT, CPkanji)                           ''~v078I~
        End If                                                         ''~v013I~
    End Sub                                                            ''~v013I~
    Private Sub copyRow()                                              ''~v013I~
        commitDGV()                                                    ''~v077I~
        If getCurrentRowData(CPenable, CPkanji, CPkana) >= 0 Then             ''~v013R~
            CPstatus = CPSTATUS_COPY                                     ''~v013I~
            SB.show(SBM.MSGID.COPY, CPkanji)                          ''~v078I~
        End If                                                         ''~v013I~
    End Sub                                                            ''~v013I~
    Private Sub pasteRow()                                             ''~v013I~
        Dim cutrow As Integer
        If CPstatus = CPSTATUS_NONE Then                                      ''~v013I~
            Exit Sub                                                   ''~v013I~
        End If                                                         ''~v013I~
        Dim cpos As Integer = getValidCPos(True) ' true:allow addrow    ''~v013I~
        DGV.Rows.Insert(cpos, CPenable, CPkanji, CPkana, False) 'false:not deleted''~v013I~
        swUpdated = True                                               ''~v013I~
        If CPstatus = CPSTATUS_CUT Then                                ''~v013I~
            If cpos <= CPcutrow Then                                          ''~v013I~
                cutrow = CPcutrow + 1                                      ''~v013I~
            Else                                                       ''~v013I~
                cutrow = CPcutrow                                        ''~v013I~
            End If                                                     ''~v013I~
            DGV.Rows.RemoveAt(cutrow)                                  ''~v013I~
            SB.show(SBM.MSGID.CUT_PASTE, CPkanji)                     ''~v078I~
        	CPstatus = CPSTATUS_CUTDONE                                ''~v080I~
        Else                                                           ''~v078I~
            SB.show(SBM.MSGID.COPY_PASTE, CPkanji)                    ''~v078I~
        End If                                                         ''~v013I~
    End Sub                                                            ''~v013I~
    Private Function getCurrentRowData(ByRef Ppenable As Boolean, ByRef Ppkanji As String, ByRef Ppkana As String) As Integer ''~v013R~
        Dim cpos As Integer = getValidCPos(False) ' false:not allow addrow''~v013I~
        If cpos < 0 Then                                                      ''~v013I~
            Return -1                                                  ''~v013R~
        End If                                                         ''~v013I~
'       Ppenable = DGV.Rows(cpos).Cells(CELLNO_ENABLE).Value               ''~v013I~''~v101R~
'       Ppkanji = DGV.Rows(cpos).Cells(CELLNO_KANJI).Value                 ''~v013I~''~v101R~
'       Ppkana = DGV.Rows(cpos).Cells(CELLNO_KANA).Value                 ''~v013I~''~v101R~
        Ppenable = CType(DGV.Rows(cpos).Cells(CELLNO_ENABLE).Value,Boolean)''~v101I~
        Ppkanji = CType(DGV.Rows(cpos).Cells(CELLNO_KANJI).Value,String)''~v101I~
        Ppkana = CType(DGV.Rows(cpos).Cells(CELLNO_KANA).Value,String) ''~v101I~
        Return cpos                                                    ''~v013R~
    End Function                                                            ''~v013I~
End Class