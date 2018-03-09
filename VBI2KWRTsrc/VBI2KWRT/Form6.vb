'CID:''+v169R~:#72                             update#=  202;         ''~v169R~
'************************************************************************************''~v163I~
'v169 2018/03/08 support string replacement by /str1/str2/ fmt(enable contains space)''~v169I~
'v164 2018/03/04 refresh required to toolstrip in any case?            ''~v164I~
'v163 2018/03/03 add string customizability for kata/hira chikan       ''~v163I~
'************************************************************************************''~v163I~
Imports System.Text                                                    ''~v163I~
Imports System.IO                                                      ''~v163I~
Public Class Form6                                                     ''~v158R~
    '***add translation chracter dialog 	                                   ''~v158I~''~v163R~
    Private Const COLNO = 3          'ListData/savefile column number(enabble,shift,chars)''~v163R~
    Private Const CELLNO_ENABLE = 0                                    ''~v163I~
    Private Const CELLNO_SHIFT = 1                                   ''~v163R~
    Private Const CELLNO_CHARS = 2                                    ''~v163R~
    Private Const CELLNO_DELETE = 3                                    ''~v163R~
    Private Const ENABLEID_ON = "1"                                    ''~v163I~
    Private Const ENABLEID_OFF = "0"                                   ''~v163I~
    Private Const DEFAULT_EXT = "tch"                                  ''~v163R~
    Private Const SPLITTER = ";"c                                      ''~v163R~
    Private Const SPLITTER_STRING = "/"c                               ''~v169I~
    ''~v163I~
    Private fileEncoding As System.Text.Encoding = System.Text.Encoding.Default ''~v163I~
    Private cfgKeys As String = My.Settings.CFGF6_TransChars           ''~v163R~
    Private DGV As DataGridView                                        ''~v163I~
    Public Shared dlgTransChar As Form6                                ''~v163R~
    Private filterIndex As Integer = 0                                 ''~v163I~
    Private ListData() As String                                       ''~v163I~
    Private mruID As Integer = ClassMRU.ID_TRANSCH                     ''~v163R~
    Private MRU As ClassMRU = Form1.MainForm.MRU                       ''~v163I~
    Private saveFilename As String                                     ''~v163I~
    Private iDGV As KDGV   'DataGridView wrapper class                 ''~v163I~
    '***************************************************************************''~v163I~
    Private swUpdated As Boolean = False                               ''~v163I~
    Private swInvalid As Boolean = False                               ''~v163I~
    Private swCommitting As Boolean = False                            ''~v163I~
    Private swFilled As Boolean = False                                ''~v163I~
    Private MRUList As New List(Of String)                             ''~v163I~
    Private CPenable As Boolean                                        ''~v163I~
    Private CPshift As Boolean                                        ''~v163I~
    Private CPchars As String                                         ''~v163R~
    Private CPstatus As Integer = 0                                    ''~v163I~
    Private Const CPSTATUS_NONE = 0                                    ''~v163I~
    Private Const CPSTATUS_CUT = 1                                     ''~v163I~
    Private Const CPSTATUS_COPY = 2                                    ''~v163I~
    Private Const CPSTATUS_CUTDONE = 3                                 ''~v163I~
    Private CPcutrow As Integer                                        ''~v163I~
    Private swForm1 As Boolean = False                                 ''~v163I~
    Private swDirty As Boolean = False                                 ''~v163I~
    Private swNotEnabled As Boolean = False                            ''~v163I~
    Private SB As SBM     'StatusBar                                   ''~v163I~
    Public callerForm1 As Form1                                        ''~v163I~
    Public callerForm3 As Form3                                        ''~v163I~
    Private BGColor As Color                                           ''~v163I~
    Private Shared Schars As String = ""                               ''~v163R~
    Private Shared Sshiftchars As String = ""                          ''~v163R~
    Private Shared Sstrchars As String = ""                            ''~v169I~
    Private Shared Sshiftstrchars As String = ""                       ''~v169I~
    Private Shared Sstrstr1 As List(Of String) = New List(Of String)()        ''~v169I~
    Private Shared Sstrstr2 As List(Of String) = New List(Of String)()                        ''~v169I~
    Private Shared Sshiftstrstr1 As List(Of String) = New List(Of String)()                    ''~v169I~
    Private Shared Sshiftstrstr2 As List(Of String) = New List(Of String)()                    ''~v169I~
    Private Shared Sstrstrquery As List(Of String) = New List(Of String)() ''~v169I~
    Private Shared Sshiftstrstrquery As List(Of String) = New List(Of String)() ''~v169I~
    '*******************************************************************************************************''~v163I~
    '*  Public Shared Function sharedAdditionalChangeLetter(PswForm1 As Boolean, Pch As Char, Pshift As Boolean, ByRef Ppch As Char, ByRef Ppstr As String) As Integer ''~v163R~''~v169R~
    Public Shared Function sharedAdditionalChangeLetter(PswForm1 As Boolean, Pch As Char, Pshift As Boolean, ByRef Ppch As Char, Ptext As String, Ppos As Integer, ByRef Ppcvstr As String, ByRef Ppcvsrclen As Integer) As Integer ''~v169I~
        '*rc 0:not target,1:single char,2:output is string(length>1)       ''~v163I~
        If dlgTransChar Is Nothing Then                                ''~v163R~
            dlgTransChar = New Form6()                                 ''~v163R~
        End If                                                         ''~v163I~
        Dim querystr As String = ""                                      ''~v169I~
        '*      Dim rc As Integer = dlgTransChar.additionalChangeLetter(Pch, Pshift, Ppch, Ppstr)  ''~v163R~''~v169R~
        Dim rc As Integer = dlgTransChar.additionalChangeLetter(Pch, Pshift, Ppch, Ptext, Ppos, Ppcvstr, Ppcvsrclen, querystr) ''~v169R~
        If rc > 0 Then                                                        ''~v163I~
            If PswForm1 Then                                           ''~v163I~
                If Ppcvsrclen = 0 Then                                        ''~v169I~
                    Form1.MainForm.showStatus(True, String.Format(Rstr.getStr("STR_MSG_CHANGELETTER_ADDITIONAL"), Pch, Ppch)) ''~v163R~''+v169R~
                Else                                                   ''~v169I~
                    Form1.MainForm.showStatus(True, String.Format(Rstr.getStr("STR_MSG_CHANGELETTER_ADDITIONAL_STR"), querystr)) ''~v169R~
                End If                                                 ''~v169I~
            Else                                                       ''~v163I~
                If Ppcvsrclen = 0 Then                                        ''~v169I~
                    Form1.formText.showStatus(True, String.Format(Rstr.getStr("STR_MSG_CHANGELETTER_ADDITIONAL"), Pch, Ppch)) ''~v163R~''+v169R~
                Else                                                   ''~v169I~
                    Form1.formText.showStatus(True, String.Format(Rstr.getStr("STR_MSG_CHANGELETTER_ADDITIONAL_STR"), querystr)) ''~v169R~
                End If                                                 ''~v169I~
            End If                                                     ''~v163I~
        End If                                                         ''~v163I~
        Return rc                                                    ''~v163I~
    End Function                                                       ''~v163I~
    '*******************************************************************************************************''~v163I~
    '*  Public Shared Function sharedQueryAdditionalChangeLetter(PswForm1 As Boolean, Pmsgclear As Boolean, Pch As Char, ByRef Ppch As Char, ByRef Ppstr As String) As Integer ''~v163I~''~v169R~
    Public Shared Function sharedQueryAdditionalChangeLetter(PswForm1 As Boolean, Pmsgclear As Boolean, Pch As Char, ByRef Ppch As Char, Ptext As String, Ppos As Integer) As Integer ''~v169I~
        '*rc 0:not target,1:single char,2:output is string(length>1)   ''~v163I~
        Dim charswos As String = ""                                         ''~v163I~
        Dim charsws As String = ""                                         ''~v163I~
        Dim msg As String = ""                                           ''~v163I~
        Dim rc As Integer
        Try                                                            ''~v169I~
            If dlgTransChar Is Nothing Then                                ''~v163I~
                dlgTransChar = New Form6()                                 ''~v163I~
            End If                                                         ''~v163I~
            rc = dlgTransChar.queryAdditionalChangeLetter(Pch, charswos, charsws, Ptext, Ppos) ''~v163I~''~v169R~
            If rc > 0 Then                                                        ''~v163I~
                If charswos.Length > 0 Then                                       ''~v163I~
                    If charsws.Length > 0 Then                         ''~v169I~
                        msg = charswos & " and " & charsws & "(Shift)"    ''~v169I~
                    Else                                               ''~v169I~
                        msg &= charswos                                        ''~v163I~
                    End If                                             ''~v169I~
                    '*              End If                                                     ''~v163I~''~v169R~
                Else                                                   ''~v169I~
                    If charsws.Length > 0 Then                                        ''~v163I~
                        msg &= charsws & "(WithShiftKey)"                      ''~v163I~
                    End If                                                     ''~v163I~
                End If                                                 ''~v169I~
                If PswForm1 Then                                           ''~v163I~
                    Form1.MainForm.showStatus(msg)                         ''~v163I~
                Else                                                       ''~v163I~
                    Form1.formText.showStatus(msg)                         ''~v163I~
                End If                                                     ''~v163I~
            End If                                                         ''~v163I~
        Catch ex As Exception                                          ''~v169I~
            Form1.exceptionMsg("Form6 sharedQueryAdditionalChangeLetter", ex) ''~v169I~
            Return 0                                                       ''~v169I~
        End Try                                                        ''~v169I~
        Return rc                                                      ''~v163I~
    End Function                                                       ''~v163I~
    '*******************************************************************************************************''~v163I~
    Sub New()                                                          ''~v163I~
        swFilled = False                                               ''~v163I~
        swUpdated = False                                              ''~v163I~
        initDlg()                                                      ''~v163I~
    End Sub                                                            ''~v163I~
    Private Sub initDlg()                                              ''~v163I~
        setLang()   'should set CurrentUICulture before InitializeComponent''~v163I~
        InitializeComponent()                                          ''~v163I~
        SB = New SBM(ToolStripStatusLabel1, StatusStrip1)               ''~v164I~
        iDGV = New KDGV(DataGridViewAddStr)                            ''~v163R~
        getCfg()                                                       ''~v163I~
    End Sub                                                            ''~v163I~
    Public Sub showDlg()                                               ''~v163R~
        initListView()                                                 ''~v163R~
        ShowDialog()    'moderess                                            ''~v163R~''~v169R~
    End Sub                                                            ''~v163R~
    Private Sub Form6_Closing(sender As System.Object, e As System.ComponentModel.CancelEventArgs) Handles MyBase.Closing ''~v163R~
        '        chkDiscard(e)                                         ''~v163I~
        Dim rc As Boolean = chkDiscard(e)                              ''~v163I~
        If rc Then                                                     ''~v163I~
            Me.DialogResult = DialogResult.Yes                         ''~v163I~
        Else                                                           ''~v163I~
            Me.DialogResult = DialogResult.No                          ''~v163I~
        End If                                                         ''~v163I~
    End Sub                                                            ''~v163I~
    Private Sub ToolStripMenuItemSave_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItemSave.Click ''~v163M~
        Try                                                            ''~v163I~
            saveFile()                                                 ''~v163I~
        Catch ex As Exception                                          ''~v163I~
            Form1.exceptionMsg("Form6 Save", ex)                       ''~v163R~
        End Try                                                        ''~v163I~
    End Sub                                                            ''~v163I~
    Private Sub ToolStripMenuItemSaveAS_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItemSaveAS.Click ''~v163M~
        Try                                                            ''~v163I~
            saveAsFile()                                               ''~v163I~
        Catch ex As Exception                                          ''~v163I~
            Form1.exceptionMsg("Form6 SaveAs", ex)                     ''~v163R~
        End Try                                                        ''~v163I~
    End Sub                                                            ''~v163I~
    Private Sub ToolStripMenuItemCut_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItemCut.Click ''~v163M~
        cutRow()                                                       ''~v163I~
    End Sub                                                            ''~v163I~
    Private Sub ToolStripMenuItemCopy_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItemCopy.Click ''~v163M~
        copyRow()                                                      ''~v163I~
    End Sub                                                            ''~v163I~
    Private Sub ToolStripMenuItemPaste_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItemPaste.Click ''~v163M~
        pasteRow()                                                     ''~v163I~
    End Sub                                                            ''~v163I~
    '*******************************************************************''~v163I~
    Private Sub ButtonCancel_Click(sender As Object, e As EventArgs) Handles ButtonCancel.Click ''~v163M~
        Me.Close()                                                     ''~v163I~
    End Sub 'resize                                                    ''~v163I~
    Private Sub ButtonOK_Click(sender As Object, e As EventArgs) Handles ButtonOK.Click ''~v163M~
        Dim str As String = Nothing                                    ''~v163I~
        If swUpdated Then                                              ''~v163I~
            If Not putCFG() Then                                       ''~v163I~
                Exit Sub                                               ''~v163I~
            End If                                                     ''~v163I~
            swUpdated = False                                          ''~v163I~
        End If                                                         ''~v163I~
        Me.Close()                                                     ''~v163I~
    End Sub 'resize                                                    ''~v163I~
    Private Sub ButtonHelp_Click(sender As Object, e As EventArgs) Handles ButtonHelp.Click ''~v163M~
        showHelp()                                                     ''~v163I~
    End Sub                                                            ''~v163I~
    Private Sub CellDirtyStateChanged(ByVal sender As System.Object, ByVal e As EventArgs) Handles DataGridViewAddStr.CurrentCellDirtyStateChanged ''~v163R~
        '** chkbox on dose not immediately committed until current forcus change, so delete flag is ignored at save file''~v163I~
        '** CommitEdit cause CellValuedChanged event                   ''~v163I~
        Dim col = DGV.CurrentCellAddress.X 'column                     ''~v163I~
        '*      Trace.W("Form6:CellDirtyStateChanged entry")                   ''~v163I~''~v164R~
        If col = CELLNO_ENABLE OrElse col = CELLNO_DELETE Then 'checkbox''~v163I~
            commitDGV()                                                ''~v163I~
        Else                                                           ''~v163I~
            If DGV.IsCurrentCellDirty Then                             ''~v163I~
                swDirty = True                                         ''~v163I~
            End If                                                     ''~v163I~
        End If                                                         ''~v163I~
        '*      Trace.W("Form6:CellDirtyStateChanged call SB.Clear")           ''~v163I~''~v164R~
        SB.show(SBM.MSGID.CLEAR, "")                                   ''~v163I~
        swUpdated = True                                               ''~v163I~
    End Sub                                                            ''~v163I~
    Private Sub CellValueChanged(ByVal sender As System.Object, ByVal e As DataGridViewCellEventArgs) Handles DataGridViewAddStr.CellValueChanged ''~v163R~
        ' e.ColumnIndex, e.RowIndex                                    ''~v163I~
        If Not swFilled Then                                           ''~v163I~
            Exit Sub                                                   ''~v163I~
        End If                                                         ''~v163I~
        Dim pos As Integer = e.RowIndex                                ''~v163I~
        If Not swCommitting Then                                       ''~v163I~
            chkValueValidity(pos, False) 'skip errmsg dialog at chkValueValidity''~v163I~
        Else                                                           ''~v163I~
            chkValueValidity(pos, True)                                ''~v163I~
        End If                                                         ''~v163I~
        swUpdated = True                                               ''~v163I~
    End Sub                                                            ''~v163I~
    Private Sub showHelp()                                             ''~v163I~
        Dim txt As String                                              ''~v163I~
        If FormOptions.swLangEN Then                                   ''~v163I~
            txt = My.Resources.help_form6E                             ''~v163R~
        Else                                                           ''~v163I~
            txt = My.Resources.help_form6                              ''~v163R~
        End If                                                         ''~v163I~
        MessageBox.Show(txt, Me.Text)                                  ''~v163I~
    End Sub                                                            ''~v163I~
    Private Sub initListView()                                         ''~v163I~
        DGV = DataGridViewAddStr                                       ''~v163R~
        fillColumn()                                                   ''~v163I~
        DGV.DefaultCellStyle.BackColor = BGColor                       ''~v163I~
    End Sub                                                            ''~v163I~
    Private Sub fillColumn()                                           ''~v163I~
        clearDGV()                                                     ''~v163I~
        For ii As Integer = 0 To CType(ListData.Length / COLNO, Integer) - 1 ''~v163I~
            Dim enable As Boolean                                      ''~v163I~
            Dim enableid As String = ListData(ii * COLNO)              ''~v163I~
            Dim shift As Boolean                                      ''~v163I~
            Dim shiftid As String = ListData(ii * COLNO + 1)            ''~v163I~
            enable = CType(IIf(enableid.CompareTo(ENABLEID_ON) = 0, True, False), Boolean) ''~v163I~
            shift = CType(IIf(shiftid.CompareTo(ENABLEID_ON) = 0, True, False), Boolean) ''~v163I~
            DGV.Rows.Add(enable, shift, ListData(ii * COLNO + 2), False) ''~v163R~
        Next                                                           ''~v163I~
        swFilled = True                                                ''~v163I~
    End Sub                                                            ''~v163I~
    '***********************************************************************''~v163I~
    Private Function getListData() As Integer                          ''~v163I~
        Dim rowctr As Integer = getRowCount()                          ''~v163I~
        Dim newrow As Integer = DGV.NewRowIndex                        ''~v163I~
        Dim listctr As Integer = rowctr                                ''~v163I~
        Dim delctr As Integer = 0                                      ''~v163I~
        If newrow >= 0 AndAlso newrow < rowctr Then                    ''~v163I~
            listctr -= 1                                               ''~v163I~
        End If                                                         ''~v163I~
        If listctr <= 0 Then                                           ''~v163I~
            Return -1                                                  ''~v163I~
        End If                                                         ''~v163I~
        Dim tmp(listctr * COLNO - 1) As String                         ''~v163I~
        Dim rc2 As Integer, errctr As Integer = 0, errpos As Integer = -1 ''~v163I~
        For ii As Integer = 0 To rowctr - 1                            ''~v163I~
            Try                                                            ''~v169I~
                If ii = newrow Then                                        ''~v163I~
                    Continue For                                           ''~v163I~
                End If                                                     ''~v163I~
                Dim del As Boolean = CType(DGV.Rows(ii).Cells(CELLNO_DELETE).Value, Boolean) ''~v163I~
                If del Then                                                ''~v163I~
                    delctr += 1                                            ''~v163I~
                    Continue For                                           ''~v163I~
                End If                                                     ''~v163I~
                Dim enable As Boolean = CType(DGV.Rows(ii).Cells(CELLNO_ENABLE).Value, Boolean) ''~v163M~
                Dim enableid As String = CType(IIf(enable, ENABLEID_ON, ENABLEID_OFF), String) ''~v163R~
                Dim shift As Boolean = CType(DGV.Rows(ii).Cells(CELLNO_SHIFT).Value, Boolean) ''~v163M~
                Dim shiftid As String = CType(IIf(shift, ENABLEID_ON, ENABLEID_OFF), String) ''~v163R~
                Dim transch As String = CType(DGV.Rows(ii).Cells(CELLNO_CHARS).Value, String) ''~v163M~
                If transch Is Nothing Then                                 ''~v163M~''~v169M~
                    transch = ""                                           ''~v163M~''~v169M~
                End If                                                     ''~v163M~''~v169M~
                If transch.Length = 0 Then                                 ''~v163R~
                    transch = " "    ' to get 3item at split               ''~v163R~
                End If                                                     ''~v163I~
                rc2 = chkValueValidity(ii, False)                          ''~v163I~
                If rc2 > 0 Then                                            ''~v163I~
                    If errpos < 0 Then                                     ''~v163I~
                        errpos = ii                                        ''~v163I~
                    End If                                                 ''~v163I~
                    errctr += 1                                            ''~v163I~
                Else                                                       ''~v163I~
                    Dim jj As Integer = ii - delctr                        ''~v163I~
                    tmp(jj * COLNO) = enableid                             ''~v163I~
                    tmp(jj * COLNO + 1) = shiftid                          ''~v163R~
                    tmp(jj * COLNO + 2) = transch                          ''~v163R~
                End If                                                     ''~v163I~
            Catch ex As Exception                                          ''~v169I~
                Form1.exceptionMsg("Form6 GetListData", ex)                ''~v169I~
                Return Nothing                                             ''~v169I~
            End Try                                                        ''~v169I~
        Next                                                           ''~v163I~
        If delctr > 0 Then                                             ''~v163I~
            Dim newctr As Integer = (listctr - delctr) * COLNO         ''~v163I~
            Dim tmp2(newctr - 1) As String                             ''~v163I~
            System.Array.Copy(tmp, tmp2, newctr)                       ''~v163I~
            ListData = tmp2                                            ''~v163I~
        Else                                                           ''~v163I~
            ListData = tmp                                             ''~v163I~
        End If                                                         ''~v163I~
        If errctr > 0 Then                                             ''~v163I~
            errIgnoredRow(errctr, errpos)                              ''~v163I~
        End If                                                         ''~v163I~
        Return errctr                                                  ''~v163I~
    End Function                                                       ''~v163I~
    '***********************************************************************''~v163I~
    Private Sub clearDGV()                                             ''~v163I~
        DGV.Rows.Clear()                                               ''~v163I~
    End Sub                                                            ''~v163I~
    Private Function getRowCount() As Integer                          ''~v163I~
        Return DGV.RowCount                                            ''~v163I~
    End Function                                                       ''~v163I~
    Private Function getSelectedPos() As Integer                       ''~v163I~
        Return DGV.CurrentCell.RowIndex                                ''~v163I~
    End Function                                                       ''~v163I~
    Private Function getValidCPos(Pallownewpos As Boolean) As Integer  ''~v163I~
        Dim cpos, maxctr As Integer                                    ''~v163I~
        cpos = getSelectedPos()                                        ''~v163I~
        maxctr = getRowCount()                                         ''~v163I~
        If cpos < 0 OrElse cpos >= maxctr Then                         ''~v163I~
            Return -1                                                  ''~v163I~
        End If                                                         ''~v163I~
        If Not Pallownewpos Then                                       ''~v163I~
            If cpos = DGV.NewRowIndex Then                             ''~v163I~
                Return -1                                              ''~v163I~
            End If                                                     ''~v163I~
        End If                                                         ''~v163I~
        Return cpos                                                    ''~v163I~
    End Function                                                       ''~v163I~
    Private Function getValidCPos(Pallownewpos As Boolean, Prow As Integer) As Integer ''~v163I~
        Dim cpos, maxctr As Integer                                    ''~v163I~
        cpos = Prow                                                    ''~v163I~
        maxctr = getRowCount()                                         ''~v163I~
        If cpos < 0 OrElse cpos >= maxctr Then                         ''~v163I~
            Return -1                                                  ''~v163I~
        End If                                                         ''~v163I~
        If Not Pallownewpos Then                                       ''~v163I~
            If cpos = DGV.NewRowIndex Then                             ''~v163I~
                Return -1                                              ''~v163I~
            End If                                                     ''~v163I~
        End If                                                         ''~v163I~
        Return cpos                                                    ''~v163I~
    End Function                                                       ''~v163I~
    Private Function putCFG() As Boolean                               ''~v163I~
        Dim errctr = getListData()                                     ''~v163I~
        If errctr > 0 Then                                             ''~v163I~
            Return False                                               ''~v163I~
        End If                                                         ''~v163I~
        If errctr < 0 Then     'null                                   ''~v163I~
            cfgKeys = ""                                               ''~v163I~
        Else                                                           ''~v163I~
            cfgKeys = String.Join(SPLITTER, ListData)                  ''~v163I~
        End If                                                         ''~v163I~
        My.Settings.CFGF6_TransChars = cfgKeys                         ''~v163I~
        getChars()  'save to shread variable                           ''~v163I~
        Return True                                                    ''~v163I~
    End Function                                                       ''~v163I~
    '**************************************************************************''~v163I~
    Private Sub getCfg()                                               ''~v163I~
        If cfgKeys.Length = 0 Then                                     ''~v163I~
            ListData = {""}                                            ''~v163I~
        Else                                                           ''~v163I~
            ListData = cfgKeys.Split(SPLITTER)                         ''~v163I~
            getChars()                                                 ''~v163I~
        End If                                                         ''~v163I~
    End Sub                                                            ''~v163I~
    '**************************************************************************''~v163I~
    Private Sub getChars()                                             ''~v163I~
        Dim tmpchars As String = ""                                      ''~v163I~
        Dim tmpshiftchars As String = ""                                 ''~v163I~
        clearStringTranslation()                                       ''~v169I~
        For ii As Integer = 0 To CType(ListData.Length / COLNO, Integer) - 1 ''~v163I~
            Dim enableid As String = ListData(ii * COLNO)              ''~v163I~
            If enableid.CompareTo(ENABLEID_ON) = 0 Then                ''~v163I~
                Dim shiftid As String = ListData(ii * COLNO + 1)        ''~v163I~
                Dim str As String = ListData(ii * COLNO + 2)             ''~v163I~
                If shiftid.CompareTo(ENABLEID_ON) = 0 Then         ''~v163I~
                    If Not saveStringTranslation(str, True) Then '*not str type translation''~v169R~
                        tmpshiftchars &= str                           ''~v163I~
                    End If                                               ''~v169I~
                Else                                               ''~v163I~
                    If Not saveStringTranslation(str, False) Then '*not str type translation''~v169R~
                        tmpchars &= str                                ''~v163I~
                    End If                                               ''~v169I~
                End If                                             ''~v163I~
            End If                                                     ''~v163I~
        Next                                                           ''~v163I~
        Schars = tmpchars                                                ''~v163I~
        Sshiftchars = tmpshiftchars                                      ''~v163I~
    End Sub                                                            ''~v163I~
    '**************************************************************************''~v163I~
    Private Sub setLang()                                              ''~v163I~
        FormOptions.setLang()                                          ''~v163I~
    End Sub                                                            ''~v163I~
    Private Function confirmDiscard() As Boolean                       ''~v163I~
        If MessageBox.Show(Rstr.getStr("STR_MSG_CONFIRM_DISCARD_WORDS_UPDATE"), Me.Text, MessageBoxButtons.YesNo) = DialogResult.No Then ''~v163I~
            Return False                                               ''~v163I~
        End If                                                         ''~v163I~
        Return True                                                    ''~v163I~
    End Function                                                       ''~v163I~
    '**************************************************************************''~v169I~
    Private Function chkValueValidity(Ppos As Integer, PswHandler As Boolean) As Integer ''~v163I~
        Dim transch As String = CType(DGV.Rows(Ppos).Cells(CELLNO_CHARS).Value, String) ''~v163R~
        Dim rc2 As Integer = chkValueValidityString(transch, Ppos, PswHandler) ''~v169I~
        If rc2 >= 0 Then '* String Then type                                      ''~v169I~
            Return rc2                                                 ''~v169I~
        End If                                                         ''~v169I~
        Dim rc As Integer = 0                                          ''~v163I~
        Dim errstr As String = ""                                      ''~v163I~
        Dim cell As Integer = 0                                        ''~v163I~
        If transch Is Nothing OrElse transch.Trim().Length = 0 Then    ''~v163I~
        Else                                                           ''~v163I~
            If transch.Trim().Length < 2 Then                           ''~v163I~
                errstr = transch                                       ''~v163I~
                cell = CELLNO_CHARS                                    ''~v163I~
                rc = 4                                                 ''~v163I~
            End If                                                     ''~v163I~
            If transch.IndexOf(SPLITTER) >= 0 Then                     ''~v163I~
                If rc < 4 Then                                         ''~v163I~
                    errstr = transch                                   ''~v163I~
                    cell = CELLNO_CHARS                                ''~v163I~
                    rc = 4                                             ''~v163I~
                End If                                                 ''~v163I~
            End If                                                     ''~v163I~
        End If                                                         ''~v163I~
        If rc = 4 Then                                                 ''~v163I~
            '           If PswHandler Then                             ''~v163I~
            '*          Dim errinfo As String = "Row-" & (Ppos + 1)                ''~v163I~''~v169R~
            '               MessageBox.Show(String.Format(Rstr.getStr("STR_ERR_MSG_WORDS_ERRVALUE"), errstr), Me.Text)''~v163I~
            '           End If                                         ''~v163I~
            Dim msg As String = String.Format(Rstr.getStr("STR_ERR_MSG_ADDTRANS_ERRVALUE"), errstr) ''~v163R~
            SB.show(msg, True)   ' delayed set text after cleared       ''~v163R~
            iDGV.setSelectedPos(Ppos, cell)                            ''~v163I~
            swInvalid = True                                           ''~v163I~
        End If                                                         ''~v163I~
        Return rc                                                      ''~v163I~
    End Function                                                       ''~v163I~
    '**************************************************************************''~v169I~
    '*/str1/str2/ fmt specification                                    ''~v169I~
    '*rc=-1:not str type                                               ''~v169I~
    '**************************************************************************''~v169I~
    Private Function chkValueValidityString(Pstr As String, Ppos As Integer, PswHandler As Boolean) As Integer ''~v169I~
        Dim rc As Integer                                              ''~v169I~
        Dim errstr As String = ""                                      ''~v169I~
        Dim cell As Integer = CELLNO_CHARS                             ''~v169I~
        If Pstr Is Nothing Then                                             ''~v169I~
            Return -1                                                  ''~v169I~
        End If                                                         ''~v169I~
        If Pstr.Chars(0) <> SPLITTER_STRING Then                            ''~v169I~
            Return -1                                                  ''~v169I~
        End If                                                         ''~v169I~
        Dim len As Integer = Pstr.Length                                 ''~v169I~
        If Pstr.Chars(len - 1) <> SPLITTER_STRING Then                        ''~v169I~
            Return -1                                                  ''~v169I~
        End If                                                         ''~v169I~
        rc = 4                                                           ''~v169I~
        Dim str1 As String = ""                                          ''~v169I~
        Dim str2 As String = ""                                          ''~v169I~
        While True                                                     ''~v169I~
            If len < 5 Then                                                   ''~v169I~
                Exit While                                             ''~v169I~
            End If                                                     ''~v169I~
            If Pstr.IndexOf(SPLITTER) >= 0 Then                        ''~v169I~
                Exit While                                             ''~v169I~
            End If                                                     ''~v169I~
            If Form3.getSubstrCount(Pstr, SPLITTER_STRING) <> 3 Then           ''~v169I~
                Exit While                                             ''~v169I~
            End If                                                     ''~v169I~
            Dim pos As Integer = Pstr.IndexOf(SPLITTER_STRING, 1) '*string splitter''~v169I~
            str1 = Pstr.Substring(1, pos - 1)                          ''~v169R~
            str2 = Pstr.Substring(pos + 1, len - pos - 2)                       ''~v169I~
            If str1.Length = 0 Then                                           ''~v169I~
                Exit While                                             ''~v169I~
            End If                                                     ''~v169I~
            rc = 0                                                       ''~v169I~
            Exit While                                                 ''~v169I~
        End While                                                      ''~v169I~
        If rc <> 0 Then                                                 ''~v169I~
            Dim msg As String = String.Format(Rstr.getStr("STR_ERR_MSG_ADDTRANS_ERRVALUE"), Pstr) ''~v169R~
            SB.show(msg, True)   ' delayed set text after cleared      ''~v169I~
            iDGV.setSelectedPos(Ppos, cell)                            ''~v169I~
            swInvalid = True                                           ''~v169I~
        End If                                                         ''~v169I~
        Return rc                                                      ''~v169I~
    End Function                                                       ''~v169I~
    '**************************************************************************''~v169I~
    Private Sub errIgnoredRow(Perrctr As Integer, Perrrow As Integer)  ''~v163I~
        MessageBox.Show(String.Format(Rstr.getStr("STR_ERR_MSG_WORDS_ROW_IGNORED"), Perrrow + 1, Perrctr), Me.Text) ''~v163I~
    End Sub                                                            ''~v163I~
    '*****************************************************************************************************''~v163I~
    '*  Private Function additionalChangeLetter(Pch As Char, Pshift As Boolean, ByRef Ppch As Char, ByRef Ppstr As String) As Integer ''~v163R~''~v169R~
    Private Function additionalChangeLetter(Pch As Char, Pshift As Boolean, ByRef Ppch As Char, Ptext As String, Ppos As Integer, ByRef Ppcvstr As String, ByRef Ppcvsrclen As Integer, ByRef Ppquerystr As String) As Integer ''~v169R~
        Ppcvsrclen = 0  '*not str type                                 ''~v169I~
        Dim rc2 As Boolean = additionalChangeString(Ptext, Ppos, Pshift, Ppcvstr, Ppcvsrclen, Ppquerystr) ''~v169R~
        If rc2 Then                                                      ''~v169I~
            Return 1                                                 ''~v169I~
        End If                                                         ''~v169I~
        Dim rc As Integer = 1                                            ''~v163R~
        Dim outch As Char                                                  ''~v163I~
        Dim str As String                                                  ''~v163I~
        If Pshift Then                                                          ''~v163I~
            str = Sshiftchars                                          ''~v163M~
        Else                                                               ''~v163I~
            str = Schars                                               ''~v163M~
        End If                                                             ''~v163I~
        Dim pos As Integer = str.IndexOf(Pch)                                ''~v163I~
        If pos < 0 Then                                                           ''~v163I~
            Return 0 ' Not target Char                                  ''~v163I~
        End If                                                             ''~v163I~
        Dim len = str.Length                                                 ''~v163I~
        If len < 2 Then                                                           ''~v163I~
            Return 0 ' Not target Char                                  ''~v163I~
        End If                                                             ''~v163I~
        If pos = len - 1 OrElse str.Chars(pos + 1) = SPLITTER Then                      ''~v163I~
            For ii As Integer = pos To 0 Step -1                             ''~v163I~
                If ii = 0 Then                                         ''~v163R~
                    pos = 0                                            ''~v163I~
                    Exit For                                               ''~v163I~
                End If                                                     ''~v163I~
                If str.Chars(ii) = SPLITTER Then                       ''~v163R~
                    pos = ii + 1                                         ''~v163R~
                    Exit For                                               ''~v163I~
                End If                                                     ''~v163I~
            Next                                                           ''~v163I~
            outch = str.Chars(pos)                                           ''~v163I~
        Else                                                               ''~v163I~
            outch = str.Chars(pos + 1)                                         ''~v163I~
        End If                                                             ''~v163I~
        Ppch = outch                                                     ''~v163I~
        Return rc                                                     ''~v163I~
    End Function                                                       ''~v164R~
    '*****************************************************************************************************''~v169I~
    Private Function additionalChangeString(Ptext As String, Ppos As Integer, Pshift As Boolean, ByRef Ppcvstr As String, ByRef Ppcvsrclen As Integer, ByRef Ppquerystr As String) As Boolean ''~v169R~
        Ppcvsrclen = 0                                                 ''~v169I~
        Dim reslen As Integer = Ptext.Length - Ppos                    ''~v169I~
        Dim rc As Boolean = False                                      ''~v169I~
        If reslen < 1 Then                                             ''~v169I~
            Return False                                               ''~v169I~
        End If                                                         ''~v169I~
        Dim str As String                                              ''~v169I~
        Dim str1 As String = ""                                         ''~v169R~
        Dim str2 As String = ""                                          ''~v169R~
        Dim str3 As String = ""                                        ''~v169I~
        If Pshift Then                                                      ''~v169I~
            str = Sshiftstrchars                                       ''~v169I~
        Else                                                           ''~v169I~
            str = Sstrchars                                            ''~v169I~
        End If                                                         ''~v169I~
        Dim ch As Char = Ptext.Chars(Ppos)                             ''~v169I~
        While True                                                     ''~v169I~
            Dim idx As Integer = str.IndexOf(ch, idx)                  ''~v169I~
            If idx < 0 Then                                            ''~v169I~
                Exit While                                             ''~v169I~
            End If                                                     ''~v169I~
            If Pshift Then                                                  ''~v169I~
                str1 = Sshiftstrstr1(idx)                    ''~v169I~
                str2 = Sshiftstrstr2(idx)                    ''~v169I~
                str3 = Sshiftstrstrquery(idx)                          ''~v169I~
            Else                                                       ''~v169I~
                str1 = Sstrstr1(idx)                         ''~v169I~
                str2 = Sstrstr2(idx)                         ''~v169I~
                str3 = Sstrstrquery(idx)                               ''~v169I~
            End If                                                     ''~v169I~
            If str1.Length <= reslen Then                              ''~v169R~
                If Ptext.IndexOf(str1, Ppos) = Ppos Then               ''~v169I~
                    rc = True                                          ''~v169I~
                    Exit While                                         ''~v169I~
                End If                                                 ''~v169I~
            End If                                                     ''~v169I~
            idx += 1                                                   ''~v169I~
        End While                                                      ''~v169I~
        If rc Then                                                     ''~v169I~
            Ppcvstr = str2                                             ''~v169R~
            Ppcvsrclen = str1.Length                                     ''~v169I~
            Ppquerystr = str3                                            ''~v169I~
        End If                                                         ''~v169I~
        Return rc                                                      ''~v169I~
    End Function                                                       ''~v169I~
    '*****************************************************************************************************''~v163I~
    '*  Private Function queryAdditionalChangeLetter(Pch As Char, ByRef Ppchars As String, ByRef PpcharsShift As String) As Integer ''~v163I~''~v169R~
    Private Function queryAdditionalChangeLetter(Pch As Char, ByRef Ppchars As String, ByRef PpcharsShift As String, Ptext As String, Ppos As Integer) As Integer ''~v169I~
        '*rc 0:not target,1:single char,2:output is string(length>1)   ''~v163I~
        Dim rc2 As Integer = queryAdditionalChangeString(Ptext, Ppos, Ppchars, PpcharsShift) ''~v169I~
        If rc2 > 0 Then                                                       ''~v169I~
            Return rc2                                                 ''~v169I~
        End If                                                         ''~v169I~
        Dim rc As Integer = 1                                          ''~v163I~
        Dim str As String                                              ''~v163I~
        Dim pos1 As Integer = 0                                          ''~v163I~
        Dim pos2 As Integer = 0                                          ''~v163I~
        Dim str1 As String = ""                                          ''~v163I~
        Dim str2 As String = ""                                          ''~v163I~
        ''~v163I~
        str = Schars                                                   ''~v163I~
        For jj As Integer = 0 To 1                                       ''~v163I~
            Dim pos As Integer = str.IndexOf(Pch)                      ''~v163I~
            If pos >= 0 Then                                           ''~v163R~
                Dim len = str.Length                                   ''~v163R~
                Dim pos0 = pos                                         ''~v163R~
                For ii As Integer = pos0 To len - 1                    ''~v163R~
                    If ii = len - 1 Then                               ''~v163R~
                        pos2 = ii                                      ''~v163R~
                        Exit For                                       ''~v163R~
                    End If                                             ''~v163R~
                    If str.Chars(ii) = SPLITTER Then                   ''~v163R~
                        pos2 = pos - 1                                 ''~v163R~
                        Exit For                                       ''~v163R~
                    End If                                             ''~v163R~
                Next                                                   ''~v163R~
                For ii As Integer = pos0 To 0 Step -1                  ''~v163R~
                    If ii = 0 Then                                     ''~v163R~
                        pos1 = 0                                       ''~v163R~
                        Exit For                                       ''~v163R~
                    End If                                             ''~v163R~
                    If str.Chars(ii) = SPLITTER Then                   ''~v163R~
                        pos1 = ii + 1                                  ''~v163R~
                        Exit For                                       ''~v163R~
                    End If                                             ''~v163R~
                Next                                                   ''~v163R~
                If jj = 0 Then                                         ''~v163R~
                    str1 = str.Substring(pos1, pos2 - pos1 + 1)        ''~v163R~
                Else                                                   ''~v163R~
                    str2 = str.Substring(pos1, pos2 - pos1 + 1)        ''~v163R~
                End If                                                 ''~v163R~
            End If                                                     ''~v163I~
            str = Sshiftchars                                          ''~v163I~
        Next                                                           ''~v163I~
        Ppchars = str1                                                   ''~v163I~
        PpcharsShift = str2                                              ''~v163I~
        Return rc                                                      ''~v163I~
    End Function    'query                                             ''~v163I~
    '*****************************************************************************************************''~v169I~
    '*display on statusbar /str1/str2/ fmt specification               ''~v169I~
    '*****************************************************************************************************''~v169I~
    Private Function queryAdditionalChangeString(Ptext As String, Ppos As Integer, ByRef Ppoutstr As String, ByRef Ppoutstrshift As String) As Integer ''~v169R~
        Dim reslen As Integer = Ptext.Length - Ppos                       ''~v169I~
        Dim rc As Integer = 0                                            ''~v169R~
        If reslen < 1 Then                                                    ''~v169I~
            Return rc                                                  ''~v169R~
        End If                                                         ''~v169I~
        Dim str As String = Sstrchars
        Dim ch As Char = Ptext.Chars(Ppos) ''~v169I~
        For jj As Integer = 0 To 1                                     ''~v169I~
            Dim idx As Integer = 0                                                      ''~v169I~
            While True                                                 ''~v169I~
                idx = str.IndexOf(ch, idx)              ''~v169I~
                If idx < 0 Then                                          ''~v169I~
                    Exit While                                         ''~v169I~
                    End If                                             ''~v169R~
                Dim str1 As String = Sstrstr1(idx)                                      ''~v169I~
                If str1.Length <= reslen Then                          ''~v169R~
                    If Ptext.IndexOf(str1, Ppos) = Ppos Then                   ''~v169I~
                        rc = 1                                         ''~v169R~
                        If jj = 0 Then                                        ''~v169I~
                            Ppoutstr = Sstrstrquery(idx)               ''~v169R~
                        Else                                           ''~v169I~
                            Ppoutstrshift = Sshiftstrstrquery(idx)      ''~v169R~
                        End If                                         ''~v169I~
                        Exit While                                     ''~v169I~
                    End If                                             ''~v169I~
                End If                                                 ''~v169I~
                idx += 1                                                 ''~v169I~
            End While                                                  ''~v169I~
            str = Sshiftstrchars                                       ''~v169I~
        Next '*w/o shift and with shift                                ''~v169I~
        Return rc                                                      ''~v169I~
    End Function    'query                                             ''~v169I~

    '************************************************************************************''~v163I~
    Class DGVComparer                                                  ''~v163I~
        Implements System.Collections.IComparer                        ''~v163I~
        Private comp As Comparer                                       ''~v163I~
        Private col As Integer                                         ''~v163I~
        Public Sub New(Pcolumn As Integer)                             ''~v163I~
            col = Pcolumn                                              ''~v163I~
            comp = New Comparer(System.Globalization.CultureInfo.CurrentCulture) ''~v163I~
        End Sub                                                        ''~v163I~
        Public Function Compare(Prow1 As Object, Prow2 As Object) As Integer Implements System.Collections.IComparer.Compare ''~v163I~
            Dim rc As Integer                                          ''~v163I~
            Dim row1 As DataGridViewRow = CType(Prow1, DataGridViewRow) ''~v163I~
            Dim row2 As DataGridViewRow = CType(Prow2, DataGridViewRow) ''~v163I~
            If col = CELLNO_ENABLE Then                                ''~v163I~
                Dim enable1 As Boolean = CType(row1.Cells(col).Value, Boolean) ''~v163I~
                Dim enable2 As Boolean = CType(row2.Cells(col).Value, Boolean) ''~v163I~
                rc = CType(IIf(enable1 = enable2, 0, CType(IIf(enable1, -1, 1), Integer)), Integer) ''~v163I~
            Else                                                       ''~v163I~
                Dim str1 As String = CType(row1.Cells(col).Value, String) ''~v163I~
                Dim str2 As String = CType(row2.Cells(col).Value, String) ''~v163I~
                If str1 Is Nothing Then                                ''~v163I~
                    rc = CType(IIf(str2 Is Nothing, 0, 1), Integer)    ''~v163I~
                ElseIf str2 Is Nothing Then                            ''~v163I~
                    rc = -1                                            ''~v163I~
                Else                                                   ''~v163I~
                    rc = System.String.Compare(str1, str2, True, System.Globalization.CultureInfo.CurrentCulture) 'true ignore case''~v163I~
                End If                                                 ''~v163I~
            End If                                                     ''~v163I~
            Return rc                                                  ''~v163I~
        End Function                                                   ''~v163I~
    End Class                                                          ''~v163I~
    '************************************************************************************''~v163I~
    Private Sub Form6_Load(sender As System.Object, e As System.EventArgs) Handles MyBase.Load ''~v163R~
        loadMRUList()                                                  ''~v163I~
    End Sub                                                            ''~v163I~
    Private Sub loadMRUList()                                          ''~v163I~
        MRU.clearMRUList(mruID)    ' loadMRUList use add method        ''~v163I~
        MRU.loadMRUListSub(mruID)                                      ''~v163I~
        setMRUListMenu()                                               ''~v163I~
    End Sub                                                            ''~v163I~
    Private Sub setMRUListMenu()                                       ''~v163I~
        selectMRUList()                                                ''~v163I~
        Dim itemMRU As ToolStripMenuItem = selectMenuItem()            ''~v163I~
        Dim ctr As Integer = MRUList.Count                             ''~v163I~
        itemMRU.DropDownItems.Clear()                                  ''~v163I~
        For ii As Integer = 0 To ctr                                   ''~v163I~
            If (ii > ClassMRU.MRULISTSZ) Then                          ''~v163I~
                Exit For                                               ''~v163I~
            End If                                                     ''~v163I~
            Dim mruitem As System.Windows.Forms.ToolStripMenuItem      ''~v163I~
            mruitem = New System.Windows.Forms.ToolStripMenuItem()     ''~v163I~
            mruitem.Size = New System.Drawing.Size(152, 22)            ''~v163I~
            If ii = 0 Then                                             ''~v163I~
                mruitem.Text = Rstr.MENU_NEWFILE                       ''~v163I~
            Else                                                       ''~v163I~
                mruitem.Text = MRUList(ii - 1)                         ''~v163I~
            End If                                                     ''~v163I~
            mruitem.Name = "MRUWordsFile"                              ''~v163I~
            AddHandler mruitem.Click, AddressOf MRU_Click              ''~v163I~
            itemMRU.DropDownItems.Add(mruitem)                         ''~v163I~
        Next                                                           ''~v163I~
    End Sub                                                            ''~v163I~
    Private Sub selectMRUList()                                        ''~v163I~
        MRUList = MRU.selectMRUList(mruID)                             ''~v163I~
    End Sub                                                            ''~v163I~
    Private Function selectMenuItem() As ToolStripMenuItem             ''~v163I~
        Dim item As ToolStripMenuItem                                  ''~v163I~
        item = ToolStripMenuItemOpen                                   ''~v163I~
        Return item                                                    ''~v163I~
    End Function                                                       ''~v163I~
    Private Sub MRU_Click(sender As System.Object, e As System.EventArgs) ' Handles MRUDictonaryFile.Click''~v163I~
        Dim item = DirectCast(sender, ToolStripMenuItem)               ''~v163I~
        Dim fnm As String                                              ''~v163I~
        fnm = item.Text                                                ''~v163I~
        If fnm.CompareTo(Rstr.MENU_NEWFILE) = 0 Then                   ''~v163I~
            openNewWordsFile()                                         ''~v163I~
        Else                                                           ''~v163I~
            insertMRUList(fnm)                                         ''~v163I~
            openFile(fnm)                                              ''~v163I~
        End If                                                         ''~v163I~
    End Sub                                                            ''~v163I~
    Private Sub openNewWordsFile()                                     ''~v163I~
        OpenFileDialog1.Filter = Rstr.getStr("STR_FILTER_TCH")         ''~v163R~
        OpenFileDialog1.FileName = ""                                  ''~v163I~
        OpenFileDialog1.AddExtension = True   'add extension if missing''~v163I~
        OpenFileDialog1.DefaultExt = DEFAULT_EXT                       ''~v163I~
        OpenFileDialog1.FilterIndex = filterIndex                      ''~v163I~
        If OpenFileDialog1.ShowDialog() = DialogResult.OK Then         ''~v163I~
            Dim fnm As String = OpenFileDialog1.FileName               ''~v163I~
            insertMRUList(fnm)                                         ''~v163I~
            Dim basename As String = System.IO.Path.GetFileNameWithoutExtension(fnm) ''~v163I~
            filterIndex = OpenFileDialog1.FilterIndex    'save for next open''~v163I~
            openFile(fnm)                                              ''~v163I~
        End If                                                         ''~v163I~
    End Sub                                                            ''~v163I~
    Public Sub insertMRUList(Pfnm As String)                           ''~v163I~
        MRU.insertMRUList(mruID, Pfnm)      '                          ''~v163I~
        setMRUListMenu()                                               ''~v163I~
        saveMRUList()                                                  ''~v163I~
    End Sub                                                            ''~v163I~
    Private Sub saveMRUList()                                          ''~v163I~
        MRU.saveMRUList(mruID)                                         ''~v163I~
    End Sub                                                            ''~v163I~
    '*************************************************************     ''~v163I~
    Private Sub openFile(Pfnm As String)                               ''~v163I~
        Dim tmp As String() = readText(Pfnm)                           ''~v163I~
        If tmp Is Nothing Then                                         ''~v163I~
            Exit Sub                                                   ''~v163I~
        End If                                                         ''~v163I~
        ListData = tmp                                                 ''~v163I~
        fillColumn()                                                   ''~v163I~
        swUpdated = True                                               ''~v163I~
        setTitle(Pfnm)                                                 ''~v163I~
        'MessageBox.Show(Pfnm, Rstr.getStr("STR_INFO_MSG_ADDTRANS_LOADED"))''~v163R~
        SB.show(SBM.MSGID.LOAD, Pfnm)                                  ''~v163I~
    End Sub                                                            ''~v163I~
    Private Sub setTitle(Pfnm As String)                               ''~v163I~
        Dim newtitle As String, oldtitle As String = Me.Text           ''~v163I~
        Dim pos As Integer = oldtitle.IndexOf("="c)                    ''~v163I~
        If pos > 0 Then                                                ''~v163I~
            newtitle = oldtitle.Substring(0, pos + 1) & Pfnm           ''~v163I~
        Else                                                           ''~v163I~
            newtitle = oldtitle & "=" & Pfnm                           ''~v163I~
        End If                                                         ''~v163I~
        Me.Text = newtitle                                             ''~v163I~
    End Sub                                                            ''~v163I~
    '*************************************************************     ''~v163I~
    Private Function readText(Pfnm As String) As String()              ''~v163I~
        Dim sr As StreamReader                                         ''~v163I~
        Dim linectr As Integer = 0                                     ''~v163I~
        Dim line As String                                             ''~v163I~
        Dim rc As String() = Nothing                                   ''~v163I~
        saveFilename = Pfnm                                            ''~v163I~
        If (Not (System.IO.File.Exists(Pfnm))) Then                    ''~v163I~
            Form1.NotFound(Pfnm)                                       ''~v163I~
            Return Nothing                                             ''~v163I~
        End If                                                         ''~v163I~
        Try                                                            ''~v163I~
            sr = New StreamReader(Pfnm, fileEncoding)                  ''~v163I~
            Do While sr.Peek() >= 0                                    ''~v163I~
                line = sr.ReadLine()                                   ''~v163I~
                linectr += 1                                           ''~v163I~
            Loop                                                       ''~v163I~
            sr.Close()                                                 ''~v163I~
            Dim tmp(linectr * COLNO - 1) As String                     ''~v163I~
            sr = New StreamReader(Pfnm, fileEncoding)                  ''~v163I~
            linectr = 0                                                ''~v163I~
            Do While sr.Peek() >= 0                                    ''~v163I~
                line = sr.ReadLine()                                   ''~v163I~
                Dim tmp2 As String() = line.Split(SPLITTER)            ''~v163R~
                If Not formatChk(tmp2) Then  'err                      ''~v163I~
                    sr.Close()                                         ''~v163I~
                    errLineFormat(linectr + 1, line)                   ''~v163I~
                    Return Nothing                                     ''~v163I~
                End If                                                 ''~v163I~
                tmp(linectr * COLNO) = tmp2(0)                         ''~v163I~
                tmp(linectr * COLNO + 1) = tmp2(1)                     ''~v163I~
                tmp(linectr * COLNO + 2) = tmp2(2)                     ''~v163I~
                linectr += 1                                           ''~v163I~
            Loop                                                       ''~v163I~
            sr.Close()                                                 ''~v163I~
            rc = tmp                                                   ''~v163I~
        Catch ex As Exception                                          ''~v163I~
            Form1.ReadError(Pfnm, ex)                                  ''~v163I~
            Return Nothing                                             ''~v163I~
        End Try                                                        ''~v163I~
        Return rc                                                      ''~v163I~
    End Function                                                       ''~v163I~
    '*************************************************************     ''~v163I~
    Private Function formatChk(Pdata As String()) As Boolean           ''~v163I~
        If Pdata.Length < 3 Then                                       ''~v163I~
            Return False                                               ''~v163I~
        End If                                                         ''~v163I~
        Dim enableid = Pdata(0)                                        ''~v163I~
        If enableid.CompareTo(ENABLEID_ON) <> 0 AndAlso enableid.CompareTo(ENABLEID_OFF) <> 0 Then ''~v163I~
            Return False                                               ''~v163I~
        End If                                                         ''~v163I~
        Dim shiftid = Pdata(1)                                        ''~v163I~
        If shiftid.CompareTo(ENABLEID_ON) <> 0 AndAlso shiftid.CompareTo(ENABLEID_OFF) <> 0 Then ''~v163I~
            Return False                                               ''~v163I~
        End If                                                         ''~v163I~
        If Pdata(2).Length < 2 Then                                    ''~v163I~
            Return False                                               ''~v163I~
        End If                                                         ''~v163I~
        Return True                                                    ''~v163I~
    End Function                                                       ''~v163I~
    '*************************************************************     ''~v163I~
    Private Sub errLineFormat(Plineno As Integer, Pline As String)     ''~v163I~
        MessageBox.Show(Pline, String.Format(Rstr.getStr("STR_ERR_MSG_WORDS_FILE_LINE_FORMAT"), Plineno + 1)) ''~v163I~
    End Sub                                                            ''~v163I~
    '*************************************************************     ''~v163I~
    Private Sub saveFile()                                             ''~v163I~
        If saveFilename Is Nothing Then                                ''~v163I~
            MessageBox.Show(Rstr.getStr("STR_ERR_MSG_WORDS_FILE_NO_SAVE_FILE"), Me.Text) ''~v163I~
            Exit Sub                                                   ''~v163I~
        End If                                                         ''~v163I~
        saveFile(saveFilename)                                         ''~v163I~
        SB.show(SBM.MSGID.SAVE, saveFilename)                          ''~v163I~
    End Sub                                                            ''~v163I~
    Private Sub saveAsFile()                                           ''~v163I~
        SaveFileDialog1.Filter = Rstr.getStr("STR_FILTER_TCH")         ''~v163R~
        If SaveFileDialog1.ShowDialog() = DialogResult.OK Then         ''~v163I~
            Dim fnm As String = SaveFileDialog1.FileName               ''~v163I~
            If saveFile(fnm) Then                                      ''~v163I~
                saveFilename = fnm                                     ''~v163I~
                setTitle(fnm)                                          ''~v163I~
                SB.show(SBM.MSGID.SAVEAS, saveFilename)                ''~v163I~
            End If                                                     ''~v163I~
        End If                                                         ''~v163I~
    End Sub                                                            ''~v163I~
    Private Function commitDGV() As Boolean                            ''~v163I~
        Try                                                            ''~v163I~
            If DGV.IsCurrentCellDirty Then                             ''~v163I~
                swCommitting = True                                    ''~v163I~
                DGV.CommitEdit(DataGridViewDataErrorContexts.Commit)   ''~v163I~
                swCommitting = False                                   ''~v163I~
                swDirty = False                                        ''~v163I~
            End If                                                     ''~v163I~
        Catch ex As Exception                                          ''~v163I~
            Return False                                               ''~v163I~
        End Try                                                        ''~v163I~
        Return True                                                    ''~v163I~
    End Function                                                       ''~v163I~
    Private Function saveFile(Pfnm As String) As Boolean               ''~v163I~
        swInvalid = False                                              ''~v163I~
        commitDGV()                                                    ''~v163I~
        If swInvalid Then ' set chkvalidity() from cellchanged         ''~v163I~
            Return False                                               ''~v163I~
        End If                                                         ''~v163I~
        Try                                                            ''~v163I~
            Dim str As String = getFileData()                          ''~v163I~
            If str IsNot Nothing Then                                  ''~v163I~
                System.IO.File.WriteAllText(Pfnm, str, fileEncoding)   ''~v163I~
                insertMRUList(Pfnm)                                    ''~v163I~
                '               MessageBox.Show(Pfnm, Rstr.MSG_INFO_SAVED)''~v163I~
            End If                                                     ''~v163I~
        Catch ex As Exception                                          ''~v163I~
            Form1.WriteError(Pfnm, ex)                                 ''~v163I~
            Return False                                               ''~v163I~
        End Try                                                        ''~v163I~
        Return True                                                    ''~v163I~
    End Function                                                       ''~v163I~
    Private Function getFileData() As String                           ''~v163I~
        Dim errctr = getListData()                                     ''~v163I~
        Dim str As String                                              ''~v163I~
        Dim sb As New StringBuilder()                                  ''~v163R~
        If errctr > 0 Then                                             ''~v163I~
            Return Nothing                                             ''~v163I~
        End If                                                         ''~v163I~
        If errctr < 0 Then     'null                                   ''~v163I~
            str = ""                                                   ''~v163I~
        Else                                                           ''~v163I~
            For ii As Integer = 0 To CType(ListData.Length / COLNO, Integer) - 1 ''~v163R~
                sb.Append(ListData(ii * COLNO) & ";" & ListData(ii * COLNO + 1) & ";" & ListData(ii * COLNO + 2) & vbCrLf) ''~v163R~
            Next                                                       ''~v163R~
            str = sb.ToString()                                        ''~v163R~
        End If                                                         ''~v163I~
        Return str                                                     ''~v163I~
    End Function                                                       ''~v163I~
    '*************************************************************     ''~v163I~
    Private Sub cutRow()                                               ''~v163I~
        commitDGV()                                                    ''~v163I~
        Dim cpos As Integer = getCurrentRowData(CPenable, CPshift, CPchars) ''~v163R~
        If cpos >= 0 Then                                              ''~v163I~
            CPstatus = CPSTATUS_CUT                                    ''~v163I~
            CPcutrow = cpos                                            ''~v163I~
            SB.show(SBM.MSGID.CUT, CPchars)                            ''~v163R~
        End If                                                         ''~v163I~
    End Sub                                                            ''~v163I~
    Private Sub copyRow()                                              ''~v163I~
        commitDGV()                                                    ''~v163I~
        If getCurrentRowData(CPenable, CPshift, CPchars) >= 0 Then     ''~v163R~
            CPstatus = CPSTATUS_COPY                                   ''~v163I~
            SB.show(SBM.MSGID.COPY, CPchars)                           ''~v163R~
        End If                                                         ''~v163I~
    End Sub                                                            ''~v163I~
    Private Sub pasteRow()                                             ''~v163I~
        Dim cutrow As Integer                                          ''~v163I~
        If CPstatus = CPSTATUS_NONE Then                               ''~v163I~
            Exit Sub                                                   ''~v163I~
        End If                                                         ''~v163I~
        Dim cpos As Integer = getValidCPos(True) ' true:allow addrow   ''~v163I~
        DGV.Rows.Insert(cpos, CPenable, CPshift, CPchars, False) 'false:not deleted''~v163I~
        swUpdated = True                                               ''~v163I~
        If CPstatus = CPSTATUS_CUT Then                                ''~v163I~
            If cpos <= CPcutrow Then                                   ''~v163I~
                cutrow = CPcutrow + 1                                  ''~v163I~
            Else                                                       ''~v163I~
                cutrow = CPcutrow                                      ''~v163I~
            End If                                                     ''~v163I~
            DGV.Rows.RemoveAt(cutrow)                                  ''~v163I~
            SB.show(SBM.MSGID.CUT_PASTE, CPchars)                      ''~v163R~
            CPstatus = CPSTATUS_CUTDONE                                ''~v163I~
        Else                                                           ''~v163I~
            SB.show(SBM.MSGID.COPY_PASTE, CPchars)                     ''~v163R~
        End If                                                         ''~v163I~
    End Sub                                                            ''~v163I~
    '************************************************************************''~v164R~
    Private Function getCurrentRowData(ByRef Ppenable As Boolean, ByRef Ppshift As Boolean, ByRef Pptransch As String) As Integer ''~v163R~
        Dim cpos As Integer = getValidCPos(False) ' false:not allow addrow''~v163I~
        If cpos < 0 Then                                               ''~v163I~
            Return -1                                                  ''~v163I~
        End If                                                         ''~v163I~
        Ppenable = CType(DGV.Rows(cpos).Cells(CELLNO_ENABLE).Value, Boolean) ''~v163I~
        Ppshift = CType(DGV.Rows(cpos).Cells(CELLNO_SHIFT).Value, Boolean) ''~v163R~
        Pptransch = CType(DGV.Rows(cpos).Cells(CELLNO_CHARS).Value, String) ''~v163R~
        Return cpos                                                    ''~v163I~
    End Function                                                       ''~v163I~
    Public Function chkDiscard(e As System.ComponentModel.CancelEventArgs) As Boolean ''~v163I~
        ' rc:true=continue process                                     ''~v163I~
        Dim rc As Boolean = True                                       ''~v163I~
        If swUpdated Then                                              ''~v163I~
            rc = Form1.confirmDiscard(e, Me.Text)                      ''~v163I~
        End If                                                         ''~v163I~
        Return rc                                                      ''~v163I~
    End Function                                                       ''~v163I~
    Private Sub showStatus(Pmsg As String)                             ''~v163I~
        ToolStripStatusLabel1.Text = Pmsg                              ''~v163I~
        '*      Trace.W("Form6:showStatus" & Pmsg)                             ''~v163I~''~v164R~
    End Sub                                                            ''~v163I~
    '************************************************************************''~v169I~
    '*save /str1/str2/ format specification                            ''~v169I~
    '*rc:true:saved                                                    ''~v169I~
    '************************************************************************''~v169I~
    Private Function saveStringTranslation(Pstr As String, Pshift As Boolean) As Boolean ''~v169R~
        If Pstr Is Nothing Then                                        ''~v169I~
            Return False                                               ''~v169I~
        End If                                                         ''~v169I~
        If Pstr.Chars(0) <> SPLITTER_STRING Then                       ''~v169I~
            Return False                                                      ''~v169I~
        End If                                                         ''~v169I~
        Dim len As Integer = Pstr.Length                               ''~v169I~
        If Pstr.Chars(len - 1) <> SPLITTER_STRING Then                 ''~v169I~
            Return False                                                      ''~v169I~
        End If                                                         ''~v169I~
        Dim pos As Integer = Pstr.IndexOf(SPLITTER_STRING, 1) '*string splitter''~v169I~
        Dim str1 As String = Pstr.Substring(1, pos - 1)                ''~v169R~
        Dim str2 As String = Pstr.Substring(pos + 1, len - pos - 2)    ''~v169I~
        Dim str1ch0 As Char = str1.Chars(0)                              ''~v169I~
        If Pshift Then                                                      ''~v169I~
            Sshiftstrchars &= str1ch0                                 ''~v169I~
            Sshiftstrstr1.Add(str1)                                    ''~v169I~
            Sshiftstrstr2.Add(str2)                                    ''~v169I~
            Sshiftstrstrquery.Add(Pstr)                                ''~v169I~
        Else                                                           ''~v169I~
            Sstrchars &= str1ch0                                       ''~v169I~
            Sstrstr1.Add(str1)                                         ''~v169I~
            Sstrstr2.Add(str2)                                         ''~v169I~
            Sstrstrquery.Add(Pstr)                                     ''~v169I~
        End If                                                         ''~v169I~
        Return True                                                    ''~v169I~
    End Function                                                       ''~v169I~
    '************************************************************************''~v169I~
    Private Sub clearStringTranslation()                               ''~v169I~
        Sstrchars = ""                                                ''~v169I~
        Sshiftstrchars = ""                                            ''~v169I~
        Sstrstr1.Clear()                                               ''~v169I~
        Sstrstr2.Clear()                                               ''~v169I~
        Sstrstrquery.Clear()                                           ''~v169I~
        Sshiftstrstr1.Clear()                                           ''~v169I~
        Sshiftstrstr2.Clear()                                           ''~v169I~
        Sshiftstrstrquery.Clear()                                      ''~v169I~
    End Sub                                                            ''~v169I~
End Class