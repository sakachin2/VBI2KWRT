''*CID:''+v113R~:#72                          update#=  123;          ''~v105R~''+v113R~
'************************************************************************************''~7C16I~
'v113 2017/12/22 put Zorder Top                                        ''+v113I~
'v105 2017/12/20 support srch/rep word enclosed by quotation           ''~v101I~
'v101 2017/12/16 Conversion warning                                    ''~7C16I~
'************************************************************************************''~7C16I~
Imports System.Runtime.InteropServices.Marshal                         ''~7612I~
Public Class Form8                                                     ''~7516R~
    'localization done                                                     ''~7618I~
    '**Find/Replace                                                    ''~7523R~
    Private Const SB_VERT = 1                                          ''~7612I~
    Private Const WM_VSCROLL = &H115                                   ''~7612I~
    Private Const SB_LINEUP = 0                                        ''~7612I~
    Private Const SB_LINEDOWN = 1                                      ''~7612I~
    Private Const SB_PAGEUP = 2                                        ''~7612I~
    Private Const SB_PAGEDOWN = 3                                      ''~7612I~
    Private Const SB_THUMBPOSITION = 4                                 ''~7612I~
    Private Const SB_THUMBTRACK = 5                                    ''~7612I~
    Private Const SB_TOP = 6                                           ''~7612I~
    Private Const SB_ENDSCROLL = 8                                     ''~7612I~
    '   Private Const SB_BOTTOM        = 7                             ''~7612I~
    Private Const SIF_RANGE = &H1                                      ''~7612I~
    Private Const SIF_PAGE = &H2                                       ''~7612I~
    Private Const SIF_POS = &H4                                        ''~7612I~
    Private Const SIF_TRACKPOS = &H10                                  ''~7612I~
    Private Const SIF_ALL As Integer = SIF_RANGE Or SIF_PAGE Or SIF_POS Or SIF_TRACKPOS ''~7612I~
    Public Structure SCROLLINFO                                       ''~7612M~
        Public cbSize, fMask, nMin, nMax, nPage, nPos, nTrackPos As Integer ''~7612M~
    End Structure                                                      ''~7612M~
    Declare Auto Function GetScrollPos Lib "user32.dll" (hWnd As IntPtr, nBar As Integer) As Integer ''~7612I~
    Declare Auto Function SendMessage Lib "user32.dll" (hWnd As IntPtr, wMsg As Integer, wParam As Integer, lparam As Integer) As Integer ''~7612I~
    Declare Auto Function GetScrollInfo Lib "user32.dll" Alias "GetScrollInfo" (ByVal hWnd As IntPtr, ByVal n As Integer, ByRef lpScrollInfo As SCROLLINFO) As Integer ''~7612I~
    ''~7612I~
    Private Const SPLITTER = ChrW(&H1)                                 ''~7517R~
    Private Const MAXWORDCTR = 10                                        ''~7517I~
    Private callerForm1 As Form1                                       ''~7517I~
    Private callerForm3 As Form3                                       ''~7516R~
    Private swForm1 As Boolean                                         ''~7516R~
    Private TBText As String
    Private TB As TextBox                                                 ''~7516R~
    Private caretPos, posFound As Integer                               ''~7516R~
    Private swcase As Boolean = My.Settings.CfgCase                     ''~7516R~
    Private swUp As Boolean = My.Settings.CfgFindUp                      ''~7516I~
    Private repPos As Integer = -1                                       ''~7524R~
    Private swRepAll As Boolean                                        ''~7524I~
    Private posRepAllLast As Integer                                   ''~7612I~
    '   Private cfgWords = My.Settings.CfgFindWord                               ''~7516R~''~7517R~''~v101R~
    Private cfgWords As String = My.Settings.CfgFindWord               ''~v101R~
    '   Private cfgRepWords = My.Settings.CfgRepWord                             ''~7516R~''~7517R~''~v101R~
    Private cfgRepWords As String = My.Settings.CfgRepWord             ''~v101R~
    '   Private word = " "                                                   ''~7519R~''~v101R~
    Private word As String = " "                                       ''~v101R~
    Private wordParm As String = " "                                   ''~v105I~
    '   Private swGetOption = False                                          ''~7521I~''~v101R~
    Private swGetOption As Boolean = False                             ''~v101R~
    Private repWord As String                                          ''~7519I~
    '   Private listSearchWord = New StackList(" ", MAXWORDCTR)            ''~7517R~''~v101R~
    Private listSearchWord As StackList = New StackList(" ", MAXWORDCTR) ''~v101R~
    '   Private listRepWord = New StackList(" ", MAXWORDCTR)               ''~7517R~''~v101R~
    Private listRepWord As StackList = New StackList(" ", MAXWORDCTR)  ''~v101R~
    Sub New()                                                          ''~7516I~
        initDlg()                                                      ''~7516I~
    End Sub                                                            ''~7516I~
    Private Sub initDlg()                                              ''~7516I~
        setLang()   'should set CurrentUICulture before InitializeComponent''~7614R~
        InitializeComponent()                                          ''~7516I~
        Form1.setupTitlebarIcon(Me)                                    ''~7612I~
        '        Dim title = My.Resources.MenuID_New '@@@@                     ''~7614R~
    End Sub                                                            ''~7516I~
    Public Sub showForForm1(Pform As Form1)                            ''~7516I~
        swForm1 = True                                                 ''~7516I~
        callerForm1 = Pform                                            ''~7516I~
        TB = Pform.TBBES ''~7516R~
        TBText = TB.Text                                               ''~7618I~
        posFound = -1                                                    ''~7516I~
        initDialog()                                                   ''~7516M~''~7521R~
        Show()                                              ''~7516M~  ''~7519R~''~7521R~
        Form1.showTop(CType(Me,Form))                                  ''+v113I~
        '       TB.Focus()                                                     ''~7523I~''~7524R~
    End Sub                                                            ''~7516I~
    Public Sub showForForm3(Pform As Form3)                            ''~7516I~
        swForm1 = False                                                ''~7516I~
        callerForm3 = Pform                                            ''~7516I~
        TB = Pform.TextBox1                                            ''~7516R~
        TBText = TB.Text                                                 ''~7516I~
        posFound = -1                                                    ''~7516I~
        initDialog()                                                   ''~7516I~''~7521R~
        Show()                                              ''~7516M~  ''~7519R~''~7521R~
        Form1.showTop(CType(Me,Form))                                  ''+v113I~
        '       TB.Focus()                                                     ''~7523I~''~7524R~
    End Sub                                                            ''~7516I~
    Public Sub Form8_Shown(ByVal sender As Object, ByVal e As EventArgs) Handles Me.Shown ''~7519I~
        '        TB.Focus()                                                    ''~7519I~
    End Sub                                                            ''~7519I~
    Private Sub Form8_Closing(sender As System.Object, e As System.ComponentModel.CancelEventArgs) Handles MyBase.Closing ''~7519I~
    End Sub                                                            ''~7519I~
    Private Sub initDialog()                                           ''~7516I~
        CheckBoxCase.Checked = swcase                                    ''~7516I~
        CheckBoxUp.Checked = swUp                                        ''~7516I~
        ''~7517I~
        listSearchWord.Load(cfgWords, SPLITTER)                        ''~7517R~
        listRepWord.Load(cfgRepWords, SPLITTER)                        ''~7517M~
        If TB.SelectionStart >= 0 AndAlso TB.SelectionLength > 0 Then          ''~7517I~
            listSearchWord.insert(TBText.Substring(TB.SelectionStart, TB.SelectionLength)) ''~7517I~
        End If                                                         ''~7517I~
        updateComboBox()                                               ''~7517R~
        If swForm1 Then                                                     ''~7519I~
            Me.BackColor = callerForm1.TBBES.BackColor                 ''~7521R~
        Else                                                           ''~7519I~
            Me.BackColor = callerForm3.TextBox1.BackColor              ''~7521R~
        End If                                                           ''~7519I~
    End Sub                                                            ''~7516I~
    Private Sub updateComboBox()                                       ''~7517I~
        Dim strarray() As String                                       ''~7517I~
        strarray = listSearchWord.toArray()                            ''~7517I~
        ComboBoxSearchWord.Items.Clear()                               ''~7517I~
        ComboBoxSearchWord.Text = " "                                  ''~7517I~
        If strarray.Length > 0 Then                                    ''~7517I~
            ComboBoxSearchWord.Text = strarray(0)                      ''~7517I~
            For ii As Integer = 1 To strarray.Length - 1               ''~7517I~
                ComboBoxSearchWord.Items.Add(strarray(ii))             ''~7517I~
            Next                                                       ''~7517I~
        End If                                                         ''~7517I~
        ''~7517I~
        strarray = listRepWord.toArray()                               ''~7517I~
        ComboBoxRepWord.Items.Clear()                                  ''~7517I~
        ComboBoxRepWord.Text = " "                                     ''~7517I~
        If strarray.Length > 0 Then                                    ''~7517I~
            ComboBoxRepWord.Text = strarray(0)                         ''~7517I~
            For ii As Integer = 1 To strarray.Length - 1               ''~7517I~
                ComboBoxRepWord.Items.Add(strarray(ii))                ''~7517I~
            Next                                                       ''~7517I~
        End If                                                         ''~7517I~
    End Sub                                                            ''~7517I~
    Private Sub ButtonFind_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonFind.Click ''~7516I~
        getOption()                                                    ''~7612I~
        searchWord()                                                   ''~7516I~
    End Sub 'resize                                                    ''~7516I~
    Private Sub ButtonReplace_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonReplace.Click ''~7516I~
        getOption()                                                    ''~7516I~
        replaceWord()                                                  ''~7516I~
    End Sub 'resize                                                    ''~7516I~
    Private Sub ButtonReplaceAll_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonReplaceAll.Click ''~7516I~
        getOption()                                                    ''~7516I~
        replaceWordAll()                                                  ''~7516I~''~7524R~
    End Sub 'resize                                                    ''~7516I~
    Private Function getOption() As Boolean                                            ''~7516I~''~7524R~
        swcase = CheckBoxCase.Checked                                    ''~7516I~
        swUp = CheckBoxUp.Checked                                        ''~7516I~
        word = ComboBoxSearchWord.Text                                    ''~7516R~''~7517R~
        repWord = ComboBoxRepWord.Text                                ''~7516I~''~7517R~
        listSearchWord.insert(word)                                    ''~7517I~
        listRepWord.insert(repWord)                                    ''~7517I~
        updateComboBox()                                               ''~7517I~
        My.Settings.CfgCase = swcase                                     ''~7516I~
        My.Settings.CfgFindUp = swUp                                     ''~7516I~
        cfgWords = listSearchWord.list2String(SPLITTER)                ''~7517R~
        cfgRepWords = listRepWord.list2String(SPLITTER)                ''~7517R~
        My.Settings.CfgFindWord = cfgWords                             ''~7517I~
        My.Settings.CfgRepWord = cfgRepWords                           ''~7517I~
        wordParm = word 'for MsgBox                                    ''~v105I~
        word = dropQuotationEnclosing(word)                              ''~v105I~
        repWord = dropQuotationEnclosing(repWord)                        ''~v105I~
        swGetOption = True                                               ''~7521I~
        Return True                                                    ''~7524I~
    End Function 'resize                                                    ''~7516I~
    Private Function searchWord() As Boolean                                           ''~7516I~''~7524R~
        TBText = TB.Text                                                 ''~7516I~''~7521M~
        Dim pos As Integer                                             ''~7516I~
        '       If word.Trim().CompareTo("") = 0 Then                                 ''~7516I~''~v105R~
        If word.Length = 0 Then                                               ''~v105I~
            '           MessageBox.Show("検索文字列が空白です")                    ''~7516I~''~7618R~
            MessageBox.Show(Rstr.MSG_ERR_FIND_NULL, Me.Text)            ''~7618I~
            Return False                                               ''~7524R~
        End If                                                         ''~7516I~
        caretPos = TB.SelectionStart                                     ''~7516I~
        If swUp Then                                                        ''~7516I~
            pos = caretPos - 1                                       ''~7516R~''~7521R~
            If repPos >= 0 Then 'from repword,startpos was specified     ''~7524I~
                pos = repPos                                             ''~7524I~
            End If                                                      ''~7524I~
            If pos < 0 Then                                                   ''~7516I~
                posFound = -1                                            ''~7516I~
            Else                                                       ''~7516I~
                If swcase Then                                         ''~7516R~
                    posFound = TBText.LastIndexOf(word, pos)           ''~7516R~
                Else                                                   ''~7516R~
                    posFound = incaseSearchUp(pos, word)              ''~7516R~
                End If                                                 ''~7516R~
            End If                                                     ''~7516I~
        Else                                                           ''~7516I~
            If repPos >= 0 Then 'from repword,startpos was specified     ''~7524I~
                pos = repPos                                             ''~7524I~
            Else                                                       ''~7524I~
                If caretPos = posFound Then                                       ''~7521R~
                    pos = caretPos + 1                                     ''~7521I~''~7524R~
                Else                                                       ''~7521I~
                    pos = caretPos                                         ''~7521R~
                End If                                                     ''~7521I~
            End If                                                      ''~7524I~
            If pos >= TBText.Length Then                                      ''~7516I~
                posFound = -1                                            ''~7516I~
            Else                                                       ''~7516I~
                If swcase Then                                         ''~7516R~
                    posFound = TBText.IndexOf(word, pos)               ''~7516R~
                Else                                                   ''~7516R~
                    posFound = incaseSearchDown(pos, word)             ''~7516R~
                End If                                                 ''~7516R~
            End If                                                     ''~7516I~
        End If                                                         ''~7516I~
        If posFound < 0 Then                                           ''~7516R~
            If Not swRepAll Then                                            ''~7524I~
                Dim line, posinline As Integer                             ''~7521R~
                Dim rcline As Boolean                                      ''~7519I~
                If swForm1 Then                                                 ''~7519I~
                    rcline = callerForm1.undoRedo.getLinePos(caretPos, line, posinline) ''~7519R~
                Else                                                       ''~7519I~
                    rcline = callerForm3.undoRedo.getLinePos(caretPos, line, posinline) ''~7519R~
                End If                                                     ''~7519I~
                Dim dest, strcase As String                                 ''~7521I~
                If swUp Then                                                    ''~7521I~
                    dest = Rstr.MSG_INFO_FIND_UP                                        ''~7521I~''~7618R~
                Else                                                       ''~7521I~
                    '                   dest = "下 方向、"                                 ''~7618R~
                    dest = Rstr.MSG_INFO_FIND_DOWN                     ''~7618I~
                End If    ''~7521I~
                If swcase Then                                                  ''~7521I~
                    '                   strcase = "大小区別 あり、"                                ''~7521I~''~7618R~
                    strcase = Rstr.MSG_INFO_FIND_CASE_SENSITIVE        ''~7618I~
                Else                                                       ''~7521I~
                    '                   strcase = "大小区別 なし、"                        ''~7618R~
                    strcase = Rstr.MSG_INFO_FIND_CASE_INSENSE          ''~7618I~
                End If ''~7521I~
                ''~7521I~
                If rcline Then                                                  ''~7519R~
                    '                   MessageBox.Show(word, "テキストは見つかりません (" & dest & strcase & "現在位置: " & line + 1 & " 行,カラム " & posinline + 1 & ")", MessageBoxButtons.OK) ''~7516R~''~7519R~''~7521R~''~7618R~
                    '                   MessageBox.Show(word, Rstr.MSG_ERR_FIND_NOT_FOUND & dest & strcase & Rstr.MSG_ERR_NOT_FOUND_CPOS & line + 1 & Rstr.MSG_ERR_NOT_FOUND_LINE_COL & posinline + 1 & ")", MessageBoxButtons.OK) ''~7618R~''~v105R~
                    MessageBox.Show(Rstr.MSG_ERR_FIND_NOT_FOUND & dest & strcase & Rstr.MSG_ERR_NOT_FOUND_CPOS & line + 1 & Rstr.MSG_ERR_NOT_FOUND_LINE_COL & posinline + 1 & ")", wordParm, MessageBoxButtons.OK) ''~v105I~
                Else                                                       ''~7519R~
                    '                   MessageBox.Show(word, "テキストは見つかりません (" & dest & strcase & "現在位置:不明)", MessageBoxButtons.OK) ''~7519R~''~7521R~''~7618R~
                    '                   MessageBox.Show(word, Rstr.MSG_ERR_FIND_NOT_FOUND & dest & strcase & Rstr.MSG_ERR_NOT_FOUND_NO_CPOS, MessageBoxButtons.OK) ''~7618R~''~v105R~
                    MessageBox.Show(Rstr.MSG_ERR_FIND_NOT_FOUND & dest & strcase & Rstr.MSG_ERR_NOT_FOUND_NO_CPOS, wordParm, MessageBoxButtons.OK) ''~v105I~
                End If                                                     ''~7519I~
            End If                                                     ''~7524I~
            TB.Focus()                                                 ''~7521I~
            Return False                                               ''~7524I~
        Else                                                           ''~7519I~
            TB.Select(posFound, word.Length)                                ''~7516R~''~7519I~
            If Not swRepAll Then                                            ''~7612I~
                scrollToWord(TB, posFound)                                  ''~7612I~
            End If                                                     ''~7612I~
        End If                                                         ''~7516I~
        TB.Focus()                                                     ''~7519I~
        Return True                                                    ''~7524I~
    End Function 'resize                                                    ''~7516I~
    Private Function incaseSearchDown(Ppos As Integer, Pword As String) As Integer ''~7516I~
        Dim ch As Char = Pword.Chars(0)                                  ''~7516I~
        Dim chU, chL As Char                                            ''~7516I~
        Dim pos, posu, posl, len As Integer                             ''~7516R~
        chU = UCase(ch)                                                  ''~7516I~
        chL = LCase(ch)                                                  ''~7516I~
        pos = Ppos                                                       ''~7516I~
        len = word.Length                                                ''~7516I~
        If chU <> chL Then                                                    ''~7516I~
            While True                                                 ''~7516I~
                posu = TBText.IndexOf(chU, pos)                       ''~7516R~
                posl = TBText.IndexOf(chL, pos)                           ''~7516R~
                pos = -1                                                 ''~7516I~
                If posu >= 0 AndAlso (posl < 0 OrElse posu < posl) Then           ''~7516I~
                    pos = posu                                           ''~7516I~
                End If                                                 ''~7516I~
                If posl >= 0 AndAlso (posu < 0 OrElse posl < posu) Then           ''~7516I~
                    pos = posl                                           ''~7516I~
                End If                                                 ''~7516I~
                If pos < 0 Then                                               ''~7516I~
                    Exit While                                         ''~7516I~
                End If                                                 ''~7516I~
                If String.Compare(word, 0, TBText, pos, len, True) = 0 Then    'case insesitive''~7516I~
                    Exit While                                         ''~7516I~
                End If                                                 ''~7516I~
                pos += 1                                               ''~7516I~
                If pos >= TBText.Length Then                                  ''~7516I~
                    Exit While                                         ''~7516I~
                End If                                                 ''~7516I~
            End While                                                  ''~7516I~
        Else                                                           ''~7516I~
            While True                                                 ''~7516I~
                pos = TBText.IndexOf(chU, pos)                            ''~7516R~
                If pos < 0 Then                                               ''~7516I~
                    Exit While                                         ''~7516I~
                End If                                                 ''~7516I~
                If String.Compare(word, 0, TBText, pos, len, True) = 0 Then    'case insesitive''~7516I~
                    Exit While                                         ''~7516I~
                End If                                                 ''~7516I~
                pos += 1                                                 ''~7516I~
                If pos >= TBText.Length Then                                  ''~7516I~
                    Exit While                                         ''~7516I~
                End If                                                 ''~7516I~
            End While                                                  ''~7516I~
        End If                                                         ''~7516I~
        Return pos                                                     ''~7516I~
    End Function                                                       ''~7516I~
    Private Function incaseSearchUp(Ppos As Integer, Pword As String) As Integer ''~7516I~
        Dim ch As Char = Pword.Chars(0)                                  ''~7516I~
        Dim chU, chL As Char                                            ''~7516I~
        Dim pos, posu, posl, len As Integer                             ''~7516R~
        chU = UCase(ch)                                                  ''~7516I~
        chL = LCase(ch)                                                  ''~7516I~
        pos = Ppos                                                       ''~7516I~
        len = word.Length                                                ''~7516I~
        If chU <> chL Then                                                    ''~7516I~
            While True                                                 ''~7516I~
                posu = TBText.LastIndexOf(chU, pos)                       ''~7516R~
                posl = TBText.LastIndexOf(chL, pos)                       ''~7516R~
                pos = -1                                                 ''~7516I~
                If posu >= 0 AndAlso (posl < 0 OrElse posu > posl) Then           ''~7516I~
                    pos = posu                                           ''~7516I~
                End If                                                 ''~7516I~
                If posl >= 0 AndAlso (posu < 0 OrElse posl > posu) Then           ''~7516I~
                    pos = posl                                           ''~7516I~
                End If                                                 ''~7516I~
                If pos < 0 Then                                               ''~7516I~
                    Exit While                                         ''~7516I~
                End If                                                 ''~7516I~
                If String.Compare(word, 0, TBText, pos, len, True) = 0 Then    'case insesitive''~7516I~
                    Exit While                                         ''~7516I~
                End If                                                 ''~7516I~
                pos -= 1                                               ''~7516I~
                If pos < 0 Then                                               ''~7516I~
                    Exit While                                         ''~7516I~
                End If                                                 ''~7516I~
            End While                                                  ''~7516I~
        Else                                                           ''~7516I~
            While True                                                 ''~7516I~
                pos = TBText.LastIndexOf(chU, pos)                        ''~7516R~
                If pos < 0 Then                                               ''~7516I~
                    Exit While                                         ''~7516I~
                End If                                                 ''~7516I~
                If String.Compare(word, 0, TBText, pos, len, True) = 0 Then    'case insesitive''~7516R~
                    Exit While                                         ''~7516I~
                End If                                                 ''~7516I~
                pos -= 1                                                 ''~7516I~
                If pos < 0 Then                                               ''~7516I~
                    Exit While                                         ''~7516I~
                End If                                                 ''~7516I~
            End While                                                  ''~7516I~
        End If                                                         ''~7516I~
        Return pos                                                     ''~7516I~
    End Function                                                       ''~7516I~
    Private Function replaceWord() As Boolean                                          ''~7516I~''~7524R~
        Dim prevword As String                                         ''~7524I~
        If repWord.CompareTo(word) = 0 Then                                   ''~7524I~
            '           MessageBox.Show("置換文字列が検索文字列と同じです")        ''~7524I~''~7618R~
            MessageBox.Show(Rstr.MSG_ERR_REPSTR_SAME, Me.Text)          ''~7618I~
            Return False                                               ''~7524I~
        End If                                                         ''~7524I~
        TBText = TB.Text                                               ''~7524I~
        Dim pos, len, posstart As Integer                                ''~7524I~
        pos = TB.SelectionStart                                      ''~7524I~
        len = TB.SelectionLength                                     ''~7524I~
        posstart = -1                                                    ''~7524I~
        If len > 0 Then     'word selected by cursor or prev find/rep         ''~7524I~
            prevword = TBText.Substring(pos, len)                      ''~7524I~
            If swUp Then                                               ''~7524I~
                If prevword.CompareTo(word) = 0 Then                   ''~7524I~
                    posstart = pos       'do rep at there              ''~7524R~
                ElseIf prevword.CompareTo(repWord) = 0 Then  'after prev rep''~7524I~
                    posstart = pos - 1                                 ''~7524R~
                End If                                                 ''~7524I~
            Else                                                       ''~7524I~
                If prevword.CompareTo(word) = 0 Then                   ''~7524R~
                    posstart = pos       'do rep at there              ''~7524R~
                ElseIf prevword.CompareTo(repWord) = 0 Then  'after prev rep''~7524R~
                    posstart = pos + len     'serach from next of repword''~7524R~
                End If                                                 ''~7524R~
            End If                                                     ''~7524I~
        End If                                                         ''~7524I~
        repPos = posstart                                                ''~7524R~
        If Not searchWord() Then                                       ''~7524R~
            repPos = -1                                                  ''~7524I~
            Return False                                               ''~7524I~
        End If                                                         ''~7524I~
        posRepAllLast = TB.SelectionStart    'remember because setText reset to 0''~7612I~
        repPos = -1                                                      ''~7524I~
        If swForm1 Then                                                ''~7524I~
            callerForm1.undoRedo.repWord(repWord, swRepAll, 0)  '0:each repword''~7612R~
        Else                                                           ''~7524I~
            callerForm3.undoRedo.repWord(repWord, swRepAll, 0)                      ''~7524I~''~7612R~
        End If                                                         ''~7524I~
        Return True                                                    ''~7524I~
    End Function 'repAll                                               ''~7612R~
    Private Sub replaceWordAll()                                       ''~7516I~
        Dim swFound As Boolean = False                                   ''~7612I~
        If swForm1 Then                                                ''~7612I~
            callerForm1.undoRedo.repWord(repWord, True, 1)  '1:start of repall,save cpos''~7612R~
        Else                                                           ''~7612I~
            callerForm3.undoRedo.repWord(repWord, True, 1)             ''~7612R~
        End If                                                         ''~7612I~
        TB.SelectionStart = 0                                            ''~7524I~
        TB.SelectionLength = 0                                           ''~7524I~
        swUp = False                                                     ''~7524I~
        swRepAll = True                                                  ''~7524I~
        While True                                                     ''~7524I~
            If Not replaceWord() Then                                       ''~7524I~
                Exit While                                             ''~7524I~
            End If                                                     ''~7524I~
            swFound = True                                               ''~7612I~
        End While                                                      ''~7524I~
        swRepAll = False                                                 ''~7524I~
        If swFound Then                                                     ''~7612I~
            TB.SelectionStart = posRepAllLast 'last found pos          ''~7612I~
            If swForm1 Then                                            ''~7612I~
                callerForm1.undoRedo.repWord(repWord, True, 2) 'Repall,last''~7612I~
            Else                                                       ''~7612I~
                callerForm3.undoRedo.repWord(repWord, True, 2)           ''~7612I~
            End If                                                     ''~7612I~
            scrollToWord(TB, TB.SelectionStart)                         ''~7612I~
        Else                                                           ''~7612I~
            '           MessageBox.Show(word, "テキストはひとつも見つかりません", MessageBoxButtons.OK) ''~7612I~''~7618R~
            MessageBox.Show(word, Rstr.MSG_ERR_NOT_FOUND_ALL, MessageBoxButtons.OK) ''~7618I~
        End If                                                         ''~7612I~
    End Sub 'repAll                                                    ''~7612R~
    Public Sub FindNext(PswUp As Boolean)    'by F3 key                ''~7519R~
        If Not swGetOption Then                                             ''~7521I~
            getOption()                                                ''~7521I~
        End If                                                         ''~7521I~
        '       If word.Trim().CompareTo("") = 0 Then                          ''~7519I~''~v105R~
        If word.Length = 0 Then                                        ''~v105I~
            '           MessageBox.Show("検索文字列が設定されていません")          ''~7519I~''~7618R~
            MessageBox.Show(Rstr.MSG_ERR_FIND_NULL, Me.Text)            ''~7618I~
            Exit Sub                                                   ''~7519I~
        End If                                                         ''~7519I~
        swUp = PswUp                                                     ''~7519I~
        searchWord()                                                   ''~7519I~
    End Sub 'resize                                                    ''~7519I~
    Private Sub KeyDownEvent(sender As System.Object, e As System.Windows.Forms.KeyEventArgs) Handles Me.KeyDown ''~7521I~
        Select Case e.KeyCode                                          ''~7521I~
            Case Keys.F3                                               ''~7521I~
                Dim swUp As Boolean = ((e.Modifiers And Keys.Shift) = Keys.Control) ''~7521I~''~7609R~
                FindNext(swUp)                                         ''~7521I~
        End Select                                                     ''~7521I~
    End Sub                                                            ''~7521I~
    Private Sub scrollToWord(Ptb As TextBox, Ppos As Integer)           ''~7612I~
        Dim cline, pagetop, pagesz As Integer                            ''~7612I~
        cline = getLineNo(Ptb, Ppos)                                      ''~7612I~
        getVScrollInfo(Ptb, pagetop, pagesz)                           ''~7612R~
        If cline >= pagetop AndAlso (cline - pagetop) < pagesz Then               ''~7612I~
            Exit Sub                                                   ''~7612I~
        End If                                                         ''~7612I~
        setVScrollPos(Ptb, cline)                                      ''~7612R~
    End Sub                                                            ''~7612I~
    Private Function getLineNo(Ptb As TextBox, Ppos As Integer) As Integer ''~7612I~
        Dim cline As Integer                                           ''~7612I~
        cline = Ptb.GetLineFromCharIndex(Ppos)                           ''~7612I~
        Return cline                                                   ''~7612I~
    End Function                                                            ''~7612I~
    Private Function initVSI() As SCROLLINFO                           ''~7612I~
        Dim vsi As SCROLLINFO = New SCROLLINFO()                       ''~7612I~
        vsi.cbSize = SizeOf(vsi)                                       ''~7612I~
        Return vsi                                                     ''~7612I~
    End Function                                                            ''~7612I~
    Private Sub getVScrollInfo(Ptb As TextBox, ByRef Pppagetop As Integer, ByRef Pppagesz As Integer) ''~7612R~
        Dim vsi As SCROLLINFO = initVSI()                                ''~7612I~
        vsi.fMask = SIF_ALL                                            ''~7612I~
        GetScrollInfo(Ptb.Handle, SB_VERT, vsi)                        ''~7612I~
        Pppagetop = vsi.nPos                                             ''~7612I~
        Pppagesz = vsi.nPage                                           ''~7612I~
    End Sub                                                            ''~7612I~
    Private Sub setVScrollPos(Ptb As TextBox, Ppos As Integer)         ''~7612R~
#If True Then                                                          ''~7612I~
        Dim pos As Integer                                             ''~7612I~
#If False Then                                                         ''~7612I~
        pos = (Ppos << 16) Or SB_THUMBPOSITION                         ''~7612I~
        '        pos = (Ppos << 16) Or SB_THUMBTRACK                   ''~7612I~
        SendMessage(Ptb.Handle, WM_VSCROLL, pos, 0)                    ''~7612I~
#Else                                                                  ''~7612I~
        pos = SB_TOP                                                   ''~7612I~
        SendMessage(Ptb.Handle, WM_VSCROLL, pos, 0)                     ''~7612I~
        '       pos =  SB_BOTTOM                                       ''~7612I~
        '       pos =  SB_PAGEDOWN                                     ''~7612I~
        pos = SB_LINEDOWN                                              ''~7612I~
        For ii As Integer = 0 To Ppos - 1                              ''~7612I~
            SendMessage(Ptb.Handle, WM_VSCROLL, pos, 0)                ''~7612I~
        Next                                                           ''~7612I~
        '       pos = SB_ENDSCROLL                                     ''~7612I~
        '       SendMessage(Ptb.Handle, WM_VSCROLL, pos, 0)            ''~7612I~
#End If                                                                ''~7612I~
#Else                                                                  ''~7612I~
        vsi.fMask = SIF_POS                                            ''~7612I~
        vsi.nPos = Ppos                                                ''~7612I~
        Dim newpos As Integer = SetScrollInfo(Ptb.Handle, SB_VERT, vsi, True)''~7612I~
'       LV.Invalidate()                                                ''~7612I~
#End If                                                                ''~7612I~
    End Sub                                                            ''~7612I~
    Private Sub setLang()                                              ''~7614R~
        FormOptions.setLang()                                          ''~7614R~
    End Sub                                                            ''~7614I~
    Private Function dropQuotationEnclosing(Pword As String) As String ''~v105I~
        Dim str As String                                              ''~v105I~
        str = Pword.Trim()                                               ''~v105I~
        If str.StartsWith("""") AndAlso str.EndsWith("""") Then         ''~v105I~
            str = str.Substring(1, str.Length - 2)                          ''~v105I~
        End If                                                         ''~v105I~
        Return str                                                     ''~v105I~
    End Function                                                       ''~v105I~
End Class