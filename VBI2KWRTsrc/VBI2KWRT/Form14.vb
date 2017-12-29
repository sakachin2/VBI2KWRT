'CID:''+v123R~:#72                             update#=   98;         ''~v123R~
'************************************************************************************''~v076I~
'v123 2017/12/29 word/symbol dialog;no change dialog target by shortcut(Ctrl+x),change only by f9,add change button to form''~v123I~
'v118 2017/12/27 errmsg errmsg for not registered Ctrl+N(send word) on Form1 was not shown when Ctl+n on form1 is first of all''~v118I~
'                swform1 was not set after New() form14 by dlgSymbol=Nothing''~v118I~
'v103 2017/12/16 (BUG)did not closed file for Dialog(Dictionary,Word,Symbol) at format err''~v103I~
'v102 2017/12/16 (BUG)Enable/Disable check  of Dislog(Dictionary,Word,Symbol)''~v102I~
'v101 2017/12/16 Conversion warning                                    ''~v101I~
'v079 2017/10/10 class DataGridView                                    ''~v079I~
'v076 2017/10/08 Symbol Dialog by DataGridView                         ''~v076I~
'************************************************************************************''~v076I~
Imports System.Text                                                    ''~v076I~
Imports System.IO                                                      ''~v076I~
Public Class Form14                                                    ''~v076R~
    '** Symbol Dialog                                                  ''~v076I~
    Private Const ZEN_DQ1 = ChrW(&H201C)                               ''~v076I~
    Private Const ZEN_DQ2 = ChrW(&H201D)                               ''~v076I~
    Private Const ZEN_SQ1 = ChrW(&H2018)                               ''~v076I~
    Private Const ZEN_SQ2 = ChrW(&H2019)                               ''~v076I~
    Private Const CELLNO_ENABLE = 0                                    ''~v076I~
    Private Const CELLNO_CHARKEY = 1                                   ''~v076I~
    Private Const CELLNO_SEND = 2                                      ''~v076I~
    Private Const CELLNO_SYMBOL = 3                                    ''~v076I~
    Private Const CELLNO_COMMENT = 4                                    ''~v076I~
    Private Const CELLNO_COMMENT_E = 5                                 ''~v076I~
    Private Const CELLNO_DELETE = 6                                    ''~v076R~
    Private Const LISTDATA_ENABLE = 0                                    ''~v076I~
    Private Const LISTDATA_CHARKEY = 1                                   ''~v076I~
    Private Const LISTDATA_SYMBOL = 2                                    ''~v076I~
    Private Const LISTDATA_COMMENT = 3                                   ''~v076I~
    Private Const LISTDATA_COMMENT_E = 4
    Private Const LISTDATA_COLNO = 5         'enable,key,symbol,comment,eNGLISHcOMMENT''~v076R~
    Private Const ENABLEID_ON = "1"                                    ''~v076I~
    Private Const ENABLEID_OFF = "0"                                   ''~v076I~
    Private Const DEFAULT_EXT = "sym"                                  ''~v076I~
    Private Const CHAR_SENDID = ChrW(&H2190)                           ''~v076I~
    Private Const CHAR_ZERO = ChrW(&H30)                             ''~v076I~
    Private Const ASC_ZERO = 48         'x30="0"                       ''~v076I~
    ''~v076I~
    '   Private fileEncoding As System.Text.Encoding = System.Text.Encoding.UTF8''~v076I~
    Private fileEncoding As System.Text.Encoding = System.Text.Encoding.Default ''~v076I~
    Private cfgKeys As String = My.Settings.CFGF14_Symbol              ''~v076R~
    Private DGV As DataGridView                                        ''~v076I~
    Public Shared dlgSymbol As Form14                                  ''~v076I~
    Private filterIndex As Integer = 0                                 ''~v076I~
    Private ListData() As String                                       ''~v076I~
    '   Private mruID = ClassMRU.ID_SYMBOL                                 ''~v076I~''~v101R~
    Private mruID As Integer = ClassMRU.ID_SYMBOL                      ''~v101I~
    Private MRU As ClassMRU = Form1.MainForm.MRU                       ''~v076I~
    Private saveFilename As String                                     ''~v076I~
    Private SB As SBM     'StatusBar                                   ''~v076I~
    Private iDGV As KDGV   'DataGridView wrapper class                 ''~v079I~
    '***************************************************************************''~v076I~
    Private swUpdated As Boolean = False                               ''~v076I~
    Private swInvalid As Boolean = False                               ''~v076I~
    Private swCommitting As Boolean = False                            ''~v076I~
    Private swShown As Boolean = False                                 ''~v076I~
    Private swFilled As Boolean = False                                ''~v076I~
    Private MRUList As New List(Of String)                             ''~v076I~
    Private CPlist As ArrayList                                        ''~v079I~
    '   Private CPenable As Boolean                                        ''~v076I~''~v079R~
    '   Private CPcharkey, CPsymbol, CPcomment As String                   ''~v076I~''~v079R~
    '   Private CPButtonLabel As String                              ''~v076I~''~v079R~
    Private CPstatus As Integer = 0                                    ''~v076I~
    Private Const CPSTATUS_NONE = 0                                    ''~v076I~
    Private Const CPSTATUS_CUT = 1                                     ''~v076I~
    Private Const CPSTATUS_COPY = 2                                    ''~v076I~
    Private Const CPSTATUS_CUTDONE = 3                                 ''~v079I~
    Private CPcutrow As Integer                                        ''~v076I~
    Private swForm1 As Boolean = False                                 ''~v076I~
    '   Private strSendID = CHAR_SENDID.ToString                           ''~v076I~''~v101R~
    Private strSendID As String = CHAR_SENDID.ToString                 ''~v101I~
    Private swDirty As Boolean = False                                 ''~v076I~
    Private swNotEnabled As Boolean = False                            ''~v076I~
    Public callerForm1 As Form1                                        ''~v076I~
    Public callerForm3 As Form3                                        ''~v076I~
    Private BGColor As Color                                           ''~v076I~
    Private numKeys As String = "1234567890"                             ''~v076I~
    ''~v076I~
    Private listDataDefault() As String = {
 "1", " ", "。", "句点", "Japanese period",
 "1", " ", "、", "読点", "Japanese comma",
 "1", " ", "・", "中点", "Punc. of middle point",
 "1", " ", "「", "第１鍵", "1st parenthesis bent",
 "1", " ", "」", "第１鍵", "1st parenthesis bent",
 "1", " ", "＜", "第２鍵", "2nd parenthesis bent",
 "1", " ", "＞", "第２鍵", "2nd parenthesis bent",
 "1", " ", "｢｢", "ふたえ鍵(『)", "Doble parentheses bent",
 "1", " ", "｣｣", "ふたえ鍵( 』)", "Double parentheses bent",
 "1", " ", "｛", "第２括弧", "2nd parenthesis",
 "1", " ", "｝", "第２括弧", "2nd parenthesis",
 "1", " ", "((", "２重括弧(《)", "Double parentheses",
 "1", " ", "))", "２重括弧(》)", "Double parentheses",
 "1", " ", "［", "第１指示符(')", "1st instruction sign",
 "1", " ", "］", "第１指示符(')", "1st instruction sign",
 "1", " ", "[_", "第２指示符("")", "2nd instruction sign",
 "1", " ", "_]", "第２指示符("")", "2nd instruction sign",
 "1", " ", "[[", "第３指示符(【)", "3rd instruction sign",
 "1", " ", "]]", "第３指示符(】)", "3rd instruction sign",
 "1", " ", ZEN_DQ1, "外国語引用符", "Double quotation",
 "1", " ", ZEN_DQ2, "外国語引用符", "Double quotation",
 "1", " ", "－", "マイナス(ハイフォン)", "Minus(hiphenation)",
 "1", " ", "－－", "ダッシュ(２マイナス)", "Dash(2 minus)",
 "1", " ", "ーー", "棒線(２長音符)", "Horizontal bar",
 "1", " ", "･･･", "点線(３半カナ中点)", "Dotted line",
 "1", " ", "<--", "左向き矢印(半角)", "Left arrow",
 "1", " ", "-->", "右向き矢印(半角)", "Right arrow",
 "1", " ", "<-->", "両向き矢印(半角)", "Both dir. arrow",
 "1", " ", "[==]", "空欄記号(半角)", "Sign of blank",
 "1", " ", "~ﾟ[", "シングルクォート(" & ZEN_SQ1 & ")", "Single quotation",
 "1", " ", "~ﾟ]", "シングルクォート(" & ZEN_SQ2 & ")", "Single quotation",
 "1", " ", "||", "詩行符(／)", "Poem line splitter",
 "1", " ", "~ﾟ||", "二重詩行符(／／)", "Double poem line splitter",
 "1", " ", "~ﾟﾏﾙ", "伏せ字の○", "Consored word:○",
 "1", " ", "~ﾟｻﾝｶ", "伏せ字の△", "Consored word:△",
 "1", " ", "~ﾟｼｶｸ", "伏せ字の□", "Consored word:□",
 "1", " ", "~ﾟｶｹ", "伏せ字の×", "Consored word:×",
 "1", " ", "~ﾟｿﾉﾀ", "その他の伏せ字", "Consored word:Other"
 }
#If False Then
    "・ま", "伏せ字の○", _                                            ''~v076R~
    "・み", "伏せ字の△", _                                            ''~v076R~
    "・む", "伏せ字の□", _                                            ''~v076R~
    "・め", "伏せ字の×", _                                            ''~v076R~
    "・も", "伏せ字のその他", _                                        ''~v076R~
#End If
    '   "【", "第３指示符", _                                          
    '   "】", "第３指示符", _                                          
    '   ZEN_SQ1, "第１指示符(')", _                                    
    '   ZEN_SQ2, "第１指示符(')", _                                    
    '   ZEN_DQ1, "第２指示符(""), _                                    
    '   ZEN_DQ2, "第２指示符("")", _                                   
    '   {"\xf2\xd7", "「第１ストレス符」"}, _                          
    '   {"\xf2\xb7", "「第２ストレス符」"}, _                          
    '	"\xf2\xd7フフニ",	"「空欄記号」",                            
    '   "\xf2\xaf\xf2\xdd",	"「その他の伏せ字」",                      
    ' "/__", "詩行符 (半角)", _                                        
    ' "＋", "プラス", _                                                
    ' "÷", "ワル", _                                                  
    ' "＝", "イコール", _                                              
    '  "‘", "シングルクオート", _                                     
    ' "’", "シングルクオート", _                                      
    ' """", "Quote(u-22)", _                                           
    ' "'", "apostorophy(u-27)", _                                      
    ' "〝", "doublemute(u301d)", _                                     
    ' "〟", "doublemute(u301f)", _                                     
    ' "〞", "doublemute(u301e)", _                                     
    '***************************************************************************''~v076I~
    Public Shared Sub newForm()                                        ''~v123I~
        dlgSymbol = New Form14()                                         ''~v123I~
    End Sub                                                            ''~v123I~
    '***************************************************************************''~v123I~
    Public Sub setParent(PswForm1 As Boolean)                          ''+v123I~
        '* from form1:to send word without showing by shortcutkey      ''+v123I~
        If PswForm1 Then                                               ''+v123I~
            setParent(PswForm1, Form1.MainForm)              ''+v123I~
        Else                                                           ''+v123I~
            setParent(PswForm1, Form1.formText)              ''+v123I~
        End If                                                         ''+v123I~
    End Sub                                                            ''+v123I~
    '***************************************************************************''+v123I~
    Public Sub setParent(PswForm1 As Boolean, Pform As Form)            ''~v118I~
        '* from form1:to send word without showing by shortcutkey          ''~v118I~
        If PswForm1 Then                                                    ''~v118I~
            swForm1 = True                                             ''~v118I~
            callerForm1 = CType(Pform, Form1)                           ''~v118I~
            BGColor = callerForm1.TBBES.BackColor                      ''~v118I~
        Else                                                           ''~v118I~
            swForm1 = False                                            ''~v118I~
            callerForm3 = CType(Pform, Form3)                           ''~v118I~
            BGColor = callerForm3.TextBox1.BackColor                   ''~v118I~
        End If                                                         ''~v118I~
        '*      dlgSymbol = CType(Me, Form14)                                   ''~v118I~''~v123R~
        If swShown Then                                                ''~v118I~
            showDlg(swForm1)                                           ''~v118I~
        End If                                                         ''~v118I~
    End Sub                                                            ''~v118I~
    Public Sub showForForm1(Pform As Form1)                            ''~v076I~
        If Form1.MainForm.showDlgSpecialKey(True, Pform) Then 'New() if disposed''~v076I~
            Exit Sub                                                   ''~v076I~
        End If                                                         ''~v076I~
        swForm1 = True                                                 ''~v076I~
        callerForm1 = Pform                                            ''~v076I~
        '*      dlgSymbol = Form1.MainForm.dlgSpecialKey                         ''~v076I~''~v123R~
        BGColor = callerForm1.TBBES.BackColor                           ''~v076I~
        Form14.sharedShowDlg(swForm1)                                  ''~v076I~
    End Sub                                                            ''~v076I~
    Public Sub showForForm3(Pform As Form3)                            ''~v076I~
        swForm1 = False                                                ''~v076I~
        If Form1.MainForm.showDlgSpecialKey(False, Pform) Then         ''~v076I~
            Exit Sub                                                   ''~v076I~
        End If                                                         ''~v076I~
        callerForm3 = Pform                                            ''~v076I~
        '*      dlgSymbol = Form1.MainForm.dlgSpecialKey                         ''~v076I~''~v123R~
        BGColor = callerForm3.TextBox1.BackColor                         ''~v076I~
        Form14.sharedShowDlg(swForm1)                                  ''~v076I~
    End Sub                                                            ''~v076I~
    '*  Public Shared Sub sharedShowDlg(PswForm1 As Boolean)               ''~v076I~''~v123R~
    Private Shared Sub sharedShowDlg(PswForm1 As Boolean)              ''~v123I~
        '*      If dlgSymbol Is Nothing Then                                   ''~v076I~''~v123R~
        '*          dlgSymbol = New Form14()                                   ''~v076I~''~v123R~
        '*      End If                                                         ''~v076I~''~v123R~
        dlgSymbol.setParent(PswForm1)                                  ''~v123I~
        dlgSymbol.showDlg(PswForm1)                                    ''~v076I~
    End Sub                                                            ''~v076I~
    '*  Public Function getSpecialStr(Pindex As Integer) As String         ''~v076I~''~v123R~
    Public Function getSpecialStr(Pindex As Integer, PswForm1 As Boolean) As String ''~v123I~
        'By Ctrl+Num key                                               ''~v076I~
        Dim ch As Char                                                 ''~v076I~
        If Pindex < 0 OrElse Pindex >= numKeys.Length Then                      ''~v076I~
            Return ""                                                  ''~v076I~
        End If                                                         ''~v076I~
        ch = numKeys(Pindex)                                           ''~v076R~
        '*      Dim rc As String = sharedApplyWords(ch, swForm1)                  ''~v076I~''~v123R~
        Dim rc As String = sharedApplyWords(ch, PswForm1)              ''~v123I~
        If rc Is Nothing Then                                               ''~v076I~
            rc = ""                                                      ''~v076I~
        End If                                                         ''~v076I~
        Return rc                                                      ''~v076I~
    End Function                                                       ''~v076I~
    '*  Public Shared Function sharedApplyWords(Pcharkey As Char, PswForm1 As Boolean) As String ''~v076I~''~v123R~
    Private Shared Function sharedApplyWords(Pcharkey As Char, PswForm1 As Boolean) As String ''~v123I~
        Dim word As String = Nothing                                   ''~v076I~
        '       If PswForm1 Then                                               ''~v076R~
        '           Return word                                                ''~v076R~
        '       End If                                                         ''~v076R~
        '*      If dlgSymbol Is Nothing Then '*Form1 calls New() and  setParent() if first of all''~v123R~
        '*          dlgSymbol = New Form14()                                   ''~v076I~''~v123R~
        '*          dlgSymbol.getCfg()    'setup ListData from                 ''~v076I~''~v123R~
        '*      End If                                                         ''~v076I~''~v123R~
        Dim asckey As Integer = AscW(Pcharkey) - ASC_ZERO              ''~v076R~
        word = dlgSymbol.applyWords(asckey, PswForm1)                  ''~v076I~
        Return word                                                    ''~v076I~
    End Function                                                       ''~v076I~
    Sub New()                                                          ''~v076I~
        swFilled = False                                               ''~v076I~
        swUpdated = False                                              ''~v076I~
        initDlg()                                                      ''~v076I~
    End Sub                                                            ''~v076I~
    Private Sub initDlg()                                              ''~v076I~
        setLang()   'should set CurrentUICulture before InitializeComponent''~v076I~
        InitializeComponent()                                          ''~v076I~
        Form1.setupTitlebarIcon(Me)                                    ''~v076I~
        SB = New SBM(ToolStripStatusLabel1)                            ''~v076I~
        iDGV = New KDGV(DataGridViewSymbol)                            ''~v079I~
        getCfg()                                                       ''~v076I~
    End Sub                                                            ''~v076I~
    '   Public Sub showDlg()                                           ''~v076I~
    Public Sub showDlg(PswForm1 As Boolean)                            ''~v076I~
        If swShown Then                                                ''~v076I~
            If Not IsDisposed() Then                                   ''~v076I~
                DGV.DefaultCellStyle.BackColor = BGColor               ''~v076I~
                Me.Focus()                                             ''~v076I~
                Exit Sub                                               ''~v076I~
            End If                                                     ''~v076I~
            '*          dlgSymbol = New Form14()                                   ''~v076I~''~v123R~
            newForm()                                                  ''~v123I~
            dlgSymbol.showDlg(PswForm1)                                ''~v076I~
            Exit Sub                                                   ''~v076I~
        End If                                                         ''~v076I~
        initListView()                                                 ''~v076I~
        setSwitchButtonText(swForm1)                                   ''~v123I~
        Show()    'moderess                                            ''~v076I~
        swShown = True                                                 ''~v076I~
        swForm1 = PswForm1                                             ''~v076I~
    End Sub                                                            ''~v076I~
    Private Sub Form14_Closing(sender As System.Object, e As System.ComponentModel.CancelEventArgs) Handles MyBase.Closing ''~v076I~
        '        chkDiscard(e)                                         ''~v076I~
        Dim rc As Boolean = chkDiscard(e)                              ''~v076I~
        If rc Then                                                     ''~v076I~
            Me.DialogResult = DialogResult.Yes                         ''~v076I~
        Else                                                           ''~v076I~
            Me.DialogResult = DialogResult.No                          ''~v076I~
        End If                                                         ''~v076I~
    End Sub                                                            ''~v076I~
    Private Sub SaveToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles SaveToolStripMenuItem.Click ''~v076I~
        saveFile()                                                     ''~v076I~
    End Sub                                                            ''~v076I~
    Private Sub SaveAsToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles SaveAsToolStripMenuItem.Click ''~v076I~
        saveAsFile()                                                   ''~v076I~
    End Sub                                                            ''~v076I~
    Private Sub CutToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles CutToolStripMenuItem.Click ''~v076I~
        cutRow()                                                       ''~v076I~
    End Sub                                                            ''~v076I~
    Private Sub CopyToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles CopyToolStripMenuItem.Click ''~v076I~
        copyRow()                                                      ''~v076I~
    End Sub                                                            ''~v076I~
    Private Sub PasteToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles PasteToolStripMenuItem.Click ''~v076I~
        pasteRow()                                                     ''~v076I~
    End Sub                                                            ''~v076I~
    Private Sub ButtonCancel_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonCancel.Click  ''~v076I~
        Me.Close()                                                     ''~v076I~
    End Sub 'resize                                                    ''~v076I~
    '*******************************************************************''~v123I~
    Private Sub SwitchTarget_Click(sender As System.Object, e As System.EventArgs) Handles ToolStripMenuItemSwitchTarget.Click''~v123I~
        Try                                                            ''~v123I~
            switchTarget()                                             ''~v123I~
        Catch ex As Exception                                          ''~v123I~
            Form1.exceptionMsg("Form14 SwitchTarget", ex)              ''~v123I~
        End Try                                                        ''~v123I~
    End Sub                                                            ''~v123I~
    '*******************************************************************''~v123I~
    Private Sub ButtonOK_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonOK.Click  ''~v076I~
        Dim str As String = Nothing                                    ''~v076I~
        If swUpdated Then                                              ''~v076I~
            If Not putCFG() Then                                       ''~v076I~
                Exit Sub                                               ''~v076I~
            End If                                                     ''~v076I~
            swUpdated = False                                          ''~v076I~
        End If                                                         ''~v076I~
        Me.Close()                                                     ''~v076I~
    End Sub 'resize                                                    ''~v076I~
    Private Sub ButtonHelp_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonHelp.Click  ''~v076I~
        showHelp()                                                     ''~v076I~
    End Sub                                                            ''~v076I~
    Private Sub ButtonDefault_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonDefault.Click  ''~v076I~
        If MessageBox.Show(Rstr.MSG_CONFIRM_RESTORE_DEFAULT, Me.Text, MessageBoxButtons.YesNo) = DialogResult.No Then ''~v076I~
            Exit Sub                                                   ''~v076I~
        End If                                                         ''~v076I~
        mergeListData(listDataDefault)                                 ''~v076R~
        putCFG()                                                       ''~v076I~
        initListView()                                                 ''~v076I~
    End Sub                                                            ''~v076I~
    Private Sub Cell_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles DataGridViewSymbol.CellClick ''~v076R~
        Dim pos As Integer = e.RowIndex                                ''~v076I~
        Dim col As Integer = e.ColumnIndex                             ''~v076I~
        If pos <> -1 Then 'not header                                  ''~v076I~
            Exit Sub                                                   ''~v076I~
        End If                                                         ''~v076I~
        '       If col = CELLNO_ENABLE OrElse col = CELLNO_CHARKEY Then        ''~v076R~
        '           sortDGV(col)                                               ''~v076R~
        '       End If                                                         ''~v076R~
    End Sub                                                            ''~v076I~
    Private Sub DataGridViewSymbol_CellContentClick(sender As System.Object, e As System.Windows.Forms.DataGridViewCellEventArgs) Handles DataGridViewSymbol.CellContentClick ''~v076R~
        Try                                                            ''~v123I~
        Dim col As Integer = e.ColumnIndex                             ''~v076I~
        If col = CELLNO_SEND Then  ' send button                       ''~v076I~
            Dim row As Integer = e.RowIndex                            ''~v076I~
            sendWord(row)                                              ''~v076I~
        End If                                                         ''~v076I~
        Catch ex As Exception                                          ''~v123I~
            Form1.exceptionMsg("Form14 Send", ex)                      ''~v123I~
        End Try                                                        ''~v123I~
    End Sub                                                            ''~v076I~
    Private Sub CellDirtyStateChanged(ByVal sender As System.Object, ByVal e As EventArgs) Handles DataGridViewSymbol.CurrentCellDirtyStateChanged ''~v076R~
        '** chkbox on dose not immediately committed until current forcus change, so delete flag is ignored at save file''~v076I~
        '** CommitEdit cause CellValuedChanged event                   ''~v076I~
        Dim col = DGV.CurrentCellAddress.X 'column                     ''~v076I~
        If col = CELLNO_ENABLE OrElse col = CELLNO_DELETE Then 'checkbox''~v076I~
            commitDGV()                                                ''~v076I~
        Else                                                           ''~v076I~
            If DGV.IsCurrentCellDirty Then                             ''~v076I~
                swDirty = True                                         ''~v076I~
            End If                                                     ''~v076I~
        End If                                                         ''~v076I~
        SB.show(SBM.MSGID.CLEAR, "")                                   ''~v076I~
        swUpdated = True                                               ''~v076I~
    End Sub                                                            ''~v076I~
    Private Sub CellValueChanged(ByVal sender As System.Object, ByVal e As DataGridViewCellEventArgs) Handles DataGridViewSymbol.CellValueChanged ''~v076R~
        ' e.ColumnIndex, e.RowIndex                                    ''~v076I~
        If Not swFilled Then                                           ''~v076I~
            Exit Sub                                                   ''~v076I~
        End If                                                         ''~v076I~
        Dim pos As Integer = e.RowIndex                                ''~v076I~
        If Not swCommitting Then                                       ''~v076I~
            chkValueValidity(pos, False) 'skip errmsg dialog at chkValueValidity''~v076I~
        Else                                                           ''~v076I~
            chkValueValidity(pos, True)                                ''~v076I~
        End If                                                         ''~v076I~
        swUpdated = True                                               ''~v076I~
    End Sub                                                            ''~v076I~
    Private Sub showHelp()                                             ''~v076I~
        Dim txt As String                                              ''~v076I~
        If FormOptions.swLangEN Then                                   ''~v076I~
            txt = My.Resources.help_form14E                            ''~v076R~
        Else                                                           ''~v076I~
            txt = My.Resources.help_form14                             ''~v076R~
        End If                                                         ''~v076I~
        MessageBox.Show(txt, Me.Text)                                  ''~v076I~
    End Sub                                                            ''~v076I~
    Private Sub initListView()                                         ''~v076I~
        DGV = DataGridViewSymbol                                        ''~v076I~
        fillColumn()                                                   ''~v076I~
        DGV.DefaultCellStyle.BackColor = BGColor                         ''~v076I~
    End Sub                                                            ''~v076I~
    Private Sub fillColumn()                                           ''~v076I~
        iDGV.clearDGV()                                                     ''~v076I~''~v079R~
        If FormOptions.swLangEN Then                                   ''~v076I~
            ColumnComment.Visible = False                              ''~v076I~
            CommentE.Visible = True                                    ''~v076I~
        Else                                                           ''~v076I~
            ColumnComment.Visible = True                               ''~v076I~
            CommentE.Visible = False                                   ''~v076I~
        End If                                                         ''~v076I~
        '       For ii As Integer = 0 To ListData.Length / LISTDATA_COLNO - 1  ''~v076R~''~v101R~
        For ii As Integer = 0 To CType(ListData.Length / LISTDATA_COLNO, Integer) - 1 ''~v101I~
            Dim pos As Integer = ii * LISTDATA_COLNO                      ''~v076I~
            Dim enable As Boolean                                      ''~v076I~
            Dim enableid As String = ListData(pos)                     ''~v076R~
            enable = CType(IIf(enableid.CompareTo(ENABLEID_ON) = 0, True, False), Boolean) ''~v076I~''~v101R~
            '           DGV.Rows.Add(enable, ListData(pos + LISTDATA_CHARKEY), strSendID, ListData(pos + LISTDATA_SYMBOL), ListData(pos + LISTDATA_COMMENT), ListData(pos + LISTDATA_COMMENT_E), False) ''~v076I~''~v079R~
            addRow(ListData, pos)                                       ''~v079I~
        Next                                                           ''~v076I~
        swFilled = True                                                ''~v076I~
    End Sub                                                            ''~v076I~
    Private Function getListData() As Integer                          ''~v076I~
        Dim rowctr As Integer = iDGV.getRowCount()                          ''~v076I~''~v079R~
        Dim newrow As Integer = DGV.NewRowIndex                        ''~v076I~
        Dim listctr As Integer = rowctr                                ''~v076I~
        Dim delctr As Integer = 0                                      ''~v076I~
        If newrow >= 0 AndAlso newrow < rowctr Then                    ''~v076I~
            listctr -= 1                                               ''~v076I~
        End If                                                         ''~v076I~
        If listctr <= 0 Then                                           ''~v076I~
            Return -1                                                  ''~v076I~
        End If                                                         ''~v076I~
        Dim tmp(listctr * LISTDATA_COLNO - 1) As String                ''~v076R~
        Dim rc2 As Integer, errctr As Integer = 0, errpos As Integer = -1 ''~v076I~
        For ii As Integer = 0 To rowctr - 1                            ''~v076I~
            If ii = newrow Then                                        ''~v076I~
                Continue For                                           ''~v076I~
            End If                                                     ''~v076I~
            Dim lst As ArrayList = getTheRowData(ii)                     ''~v079R~
            '           Dim charkey As String = lst(CELLNO_CHARKEY)                ''~v079R~''~v101R~
            '           Dim phrase As String = lst(CELLNO_SYMBOL)                  ''~v079R~''~v101R~
            '           Dim del As Boolean = lst(CELLNO_DELETE)                    ''~v079R~''~v101R~
            '           Dim comment As String = lst(CELLNO_COMMENT)                ''~v079R~''~v101R~
            '           Dim commentE As String = lst(CELLNO_COMMENT_E)             ''~v079R~''~v101R~
            Dim charkey As String = CType(lst(CELLNO_CHARKEY), String)  ''~v101I~
            Dim phrase As String = CType(lst(CELLNO_SYMBOL), String)    ''~v101I~
            Dim del As Boolean = CType(lst(CELLNO_DELETE), Boolean)     ''~v101I~
            Dim comment As String = CType(lst(CELLNO_COMMENT), String)  ''~v101I~
            Dim commentE As String = CType(lst(CELLNO_COMMENT_E), String) ''~v101I~
            If charkey Is Nothing Then                                 ''~v076I~
                charkey = ""                                           ''~v076I~
            End If                                                     ''~v076I~
            If phrase Is Nothing Then                                  ''~v076I~
                phrase = ""                                            ''~v076I~
            End If                                                     ''~v076I~
            If comment Is Nothing Then                                 ''~v076I~
                comment = ""                                            ''~v076I~
            End If                                                     ''~v076I~
            If charkey.Length = 0 AndAlso phrase.Length = 0 Then       ''~v076I~
                del = True                                             ''~v076I~
            End If                                                     ''~v076I~
            If del Then                                                ''~v076I~
                delctr += 1                                            ''~v076I~
                Continue For                                           ''~v076I~
            End If                                                     ''~v076I~
            If phrase.Length = 0 Then                                  ''~v076I~
                phrase = " "    ' to get 3item at split                ''~v076I~
            End If                                                     ''~v076I~
            '           Dim enable As Boolean = lst(CELLNO_ENABLE)                 ''~v079R~''~v101R~
            Dim enable As Boolean = CType(lst(CELLNO_ENABLE), Boolean)  ''~v101I~
            Dim enableid As String                                     ''~v076I~
            '           enableid = IIf(enable, ENABLEID_ON, ENABLEID_OFF)          ''~v076I~''~v101R~
            enableid = CType(IIf(enable, ENABLEID_ON, ENABLEID_OFF), String) ''~v101I~
            rc2 = chkValueValidity(ii, False)                          ''~v076I~
            If rc2 > 0 Then                                            ''~v076I~
                If errpos < 0 Then                                     ''~v076I~
                    errpos = ii                                        ''~v076I~
                End If                                                 ''~v076I~
                errctr += 1                                            ''~v076I~
            Else                                                       ''~v076I~
                Dim jj As Integer = ii - delctr                        ''~v076I~
                Dim pos As Integer = jj * LISTDATA_COLNO                           ''~v076I~
                tmp(pos) = enableid                                    ''~v076R~
                tmp(pos + LISTDATA_CHARKEY) = charkey                  ''~v076R~
                tmp(pos + LISTDATA_SYMBOL) = phrase                    ''~v076R~
                tmp(pos + LISTDATA_COMMENT) = comment                  ''~v076R~
                tmp(pos + LISTDATA_COMMENT_E) = commentE               ''~v076R~
            End If                                                     ''~v076I~
        Next                                                           ''~v076I~
        If delctr > 0 Then                                             ''~v076I~
            Dim newctr As Integer = (listctr - delctr) * LISTDATA_COLNO ''~v076R~
            Dim tmp2(newctr - 1) As String                             ''~v076I~
            System.Array.Copy(tmp, tmp2, newctr)                       ''~v076I~
            ListData = tmp2                                            ''~v076I~
        Else                                                           ''~v076I~
            ListData = tmp                                             ''~v076I~
        End If                                                         ''~v076I~
        If errctr > 0 Then                                             ''~v076I~
            errIgnoredRow(errctr, errpos)                              ''~v076I~
        End If                                                         ''~v076I~
        Return errctr                                                  ''~v076I~
    End Function                                                       ''~v076I~
    Private Sub mergeListData(Pdefault() As String)                    ''~v076I~
        Dim listsrc() As String = Pdefault                              ''~v076I~
        Dim listtgt() As String = ListData                               ''~v076I~
        Dim ctrtgt, ctrsrc, possrc As Integer         ''~v076R~
        Dim rowctr As Integer = iDGV.getRowCount()                          ''~v076I~''~v079R~
        ctrtgt = rowctr                                                ''~v076R~
        ctrsrc = (listsrc.Length + LISTDATA_COLNO - 1) \ LISTDATA_COLNO        ''~v076I~
        Dim newrow As Integer = DGV.NewRowIndex                        ''~v079I~
        For ii As Integer = 0 To ctrsrc - 1                            ''~v076I~
            possrc = ii * LISTDATA_COLNO                                   ''~v076I~
            Dim src = listsrc(possrc + LISTDATA_SYMBOL)                    ''~v076I~
            Dim swFound = False                                          ''~v076I~
            For jj As Integer = 0 To rowctr - 1                        ''~v076R~
                If jj = newrow Then                                    ''~v079I~
                    Continue For                                       ''~v079I~
                End If                                                 ''~v079I~
                Dim lst As ArrayList = getTheRowData(jj)                 ''~v079I~
                '               Dim symbol As String = lst(CELLNO_SYMBOL)              ''~v079R~''~v101R~
                Dim symbol As String = CType(lst(CELLNO_SYMBOL), String) ''~v101I~
                If symbol IsNot Nothing AndAlso src.CompareTo(symbol) = 0 Then ''~v076R~
                    updateRow(jj, lst, listsrc, possrc)                   ''~v079I~
                    swFound = True                                       ''~v076I~
                    Exit For                                           ''~v076I~
                End If                                                 ''~v076I~
            Next                                                       ''~v076I~
            If Not swFound Then                                             ''~v076I~
                '               DGV.Rows.Add(True, "", strSendID, listsrc(possrc + LISTDATA_SYMBOL), listsrc(possrc + LISTDATA_COMMENT), listsrc(possrc + LISTDATA_COMMENT_E), False) ''~v076R~''~v079R~
                addRow(listsrc, possrc)                                 ''~v079I~
            End If                                                     ''~v076I~
        Next                                                           ''~v076I~
    End Sub                                                            ''~v076I~
    Private Function getDGVword(Pasckey As Integer) As String          ''~v076I~
        Dim word As String = Nothing                                   ''~v076I~
        Dim rowctr As Integer = iDGV.getRowCount()                          ''~v076I~''~v079R~
        Dim newrow As Integer = DGV.NewRowIndex                        ''~v076I~
        Dim asckey As Integer = Pasckey                                ''~v076R~
        ''~v076I~
        If swDirty Then                                                ''~v076I~
            commitDGV()                                                ''~v076I~
        End If                                                         ''~v076I~
        For ii As Integer = 0 To rowctr - 1                            ''~v076I~
            If ii = newrow Then                                        ''~v076I~
                Continue For                                           ''~v076I~
            End If                                                     ''~v076I~
            Dim lst As ArrayList = getTheRowData(ii)                     ''~v079I~
            '           Dim del As Boolean = lst(CELLNO_DELETE)                    ''~v079R~''~v101R~
            Dim del As Boolean = CType(lst(CELLNO_DELETE), Boolean)     ''~v101I~
            If del Then                                                ''~v076I~
                Continue For                                           ''~v076I~
            End If                                                     ''~v076I~
            '           Dim strkey As String = lst(CELLNO_CHARKEY)                 ''~v079R~''~v101R~
            Dim strkey As String = CType(lst(CELLNO_CHARKEY), String)   ''~v101I~
            If strkey Is Nothing OrElse strkey.Length = 0 Then         ''~v076I~
                Continue For                                           ''~v076I~
            End If                                                     ''~v076I~
            Dim charkey As Char = strkey(0)                            ''~v076I~
            Dim asc As Integer = AscW(charkey) - ASC_ZERO                ''~v076R~
            If asc = asckey Then                                            ''~v076R~
                '               Dim enable As Boolean = lst(CELLNO_ENABLE)             ''~v079R~''~v101R~
                Dim enable As Boolean = CType(lst(CELLNO_ENABLE), Boolean) ''~v101R~
                If Not enable Then                                     ''~v076I~
                    swNotEnabled = True                                ''~v076I~
                Else                                                   ''~v076I~
                    '                   word = lst(CELLNO_SYMBOL)                          ''~v079R~''~v101R~
                    word = CType(lst(CELLNO_SYMBOL), String)            ''~v101I~
                    If word Is Nothing Then                            ''~v076I~
                        word = " "                                     ''~v076I~
                    End If                                             ''~v076I~
                    Exit For                                           ''~v076I~
                End If                                                 ''~v076I~
            End If                                                     ''~v076I~
        Next                                                           ''~v076I~
        Return word                                                    ''~v076I~
    End Function ' getDGVword                                          ''~v076I~
    Private Function putCFG() As Boolean                               ''~v076I~
        Dim errctr = getListData()                                     ''~v076I~
        If errctr > 0 Then                                             ''~v076I~
            Return False                                               ''~v076I~
        End If                                                         ''~v076I~
        If errctr < 0 Then     'null                                   ''~v076I~
            cfgKeys = ""                                               ''~v076I~
        Else                                                           ''~v076I~
            cfgKeys = String.Join(";", ListData)                       ''~v076I~
        End If                                                         ''~v076I~
        Debug.WriteLine("Form14.putcfg =" & cfgKeys)                   ''~v076I~
        My.Settings.CFGF14_Symbol = cfgKeys                             ''~v076R~
        Return True                                                    ''~v076I~
    End Function                                                       ''~v076I~
    Private Sub getCfg()                                               ''~v076I~
        If cfgKeys.Length = 0 Then                                     ''~v076I~
            ListData = listDataDefault                                 ''~v076I~
            Debug.WriteLine("Form14.getcfg deefault" & ListData.ToString())       ''~v076I~
        Else                                                           ''~v076I~
            ListData = cfgKeys.Split(";"c)                             ''~v076I~
            Debug.WriteLine("Form14.getcfg cfgKey=" & cfgKeys)         ''~v076I~
        End If                                                         ''~v076I~
    End Sub                                                            ''~v076I~
    Private Sub setLang()                                              ''~v076I~
        FormOptions.setLang()                                          ''~v076I~
    End Sub                                                            ''~v076I~
    Private Function confirmDiscard() As Boolean                       ''~v076I~
        If MessageBox.Show(Rstr.getStr("STR_MSG_CONFIRM_DISCARD_WORDS_UPDATE"), Me.Text, MessageBoxButtons.YesNo) = DialogResult.No Then ''~v076I~
            Return False                                               ''~v076I~
        End If                                                         ''~v076I~
        Return True                                                    ''~v076I~
    End Function                                                       ''~v076I~
    Private Function chkValueValidity(Ppos As Integer, PswHandler As Boolean) As Integer ''~v076I~
        Dim lst As ArrayList = getTheRowData(Ppos)                       ''~v079I~
        '       Dim charkey As String = lst(CELLNO_CHARKEY)                    ''~v079R~''~v101R~
        '       Dim phrase As String = lst(CELLNO_SYMBOL)                      ''~v079R~''~v101R~
        '       Dim cmt As String = lst(CELLNO_COMMENT)                        ''~v079R~''~v101R~
        '       Dim cmtE As String = lst(CELLNO_COMMENT_E)                     ''~v079R~''~v101R~
        Dim charkey As String = CType(lst(CELLNO_CHARKEY), String)      ''~v101I~
        Dim phrase As String = CType(lst(CELLNO_SYMBOL), String)        ''~v101I~
        Dim cmt As String = CType(lst(CELLNO_COMMENT), String)          ''~v101I~
        Dim cmtE As String = CType(lst(CELLNO_COMMENT_E), String)       ''~v101I~
        Dim rc As Integer = 0                                          ''~v076I~
        Dim errstr As String = ""                                      ''~v076I~
        Dim cell As Integer = 0                                          ''~v076I~
        If charkey Is Nothing OrElse charkey.Trim().Length = 0 Then    ''~v076I~
        Else                                                           ''~v076I~
            If charkey.IndexOf(";"c) >= 0 OrElse charkey.Length > 1 Then ''~v076I~
                errstr = charkey                                       ''~v076I~
                cell = CELLNO_CHARKEY                                    ''~v076I~
                rc = 4                                                 ''~v076I~
            End If                                                     ''~v076I~
        End If                                                         ''~v076I~
        If phrase Is Nothing OrElse phrase.Trim().Length = 0 Then      ''~v076I~
        Else                                                           ''~v076I~
            If phrase.IndexOf(";"c) >= 0 Then                          ''~v076I~
                If rc < 4 Then                                         ''~v076I~
                    errstr = phrase                                    ''~v076I~
                    cell = CELLNO_SYMBOL                                 ''~v076I~
                    rc = 4                                             ''~v076I~
                End If                                                 ''~v076I~
            End If                                                     ''~v076I~
        End If                                                         ''~v076I~
        If cmt Is Nothing OrElse cmt.Trim().Length = 0 Then            ''~v076I~
        Else                                                           ''~v076I~
            If cmt.IndexOf(";"c) >= 0 Then                             ''~v076I~
                If rc < 4 Then                                         ''~v076I~
                    errstr = cmt                                       ''~v076I~
                    cell = CELLNO_COMMENT                                ''~v076I~
                    rc = 4                                             ''~v076I~
                End If                                                 ''~v076I~
            End If                                                     ''~v076I~
        End If                                                         ''~v076I~
        If cmtE Is Nothing OrElse cmtE.Trim().Length = 0 Then          ''~v076I~
        Else                                                           ''~v076I~
            If cmtE.IndexOf(";"c) >= 0 Then                            ''~v076I~
                If rc < 4 Then                                         ''~v076I~
                    errstr = cmtE                                      ''~v076I~
                    cell = CELLNO_COMMENT_E                              ''~v076I~
                    rc = 4                                             ''~v076I~
                End If                                                 ''~v076I~
            End If                                                     ''~v076I~
        End If                                                         ''~v076I~
        If rc = 4 Then                                                 ''~v076I~
            '           If PswHandler Then                                         ''~v076R~
            Dim errinfo As String = "Row-" & (Ppos + 1)            ''~v076I~
            '               MessageBox.Show(String.Format(Rstr.getStr("STR_ERR_MSG_WORDS_ERRVALUE"), errstr), Me.Text)''~v076R~
            Dim msg As String = String.Format(Rstr.getStr("STR_ERR_MSG_WORDS_ERRVALUE"), errstr) ''~v076I~
            SB.show(msg, True)   ' delayed set text after cleared   ''~v076R~
            iDGV.setSelectedPos(Ppos, cell)                                  ''~v076R~
            '           End If                                                     ''~v076R~
            swInvalid = True                                           ''~v076I~
        End If                                                         ''~v076I~
        Return rc                                                      ''~v076I~
    End Function                                                       ''~v076I~
    Private Sub errIgnoredRow(Perrctr As Integer, Perrrow As Integer)  ''~v076I~
        MessageBox.Show(String.Format(Rstr.getStr("STR_ERR_MSG_WORDS_ROW_IGNORED"), Perrrow + 1, Perrctr), Me.Text) ''~v076I~
    End Sub                                                            ''~v076I~
    Private Sub sortDGV(Pcolumn As Integer)                            ''~v076I~
        DGV.Sort(New DGVComparer(Pcolumn))                             ''~v076I~
    End Sub                                                            ''~v076I~
    '******************************************************************''~v123I~
    Private Function applyWords(Pasckey As Integer, PswForm1 As Boolean) As String ''~v076I~
        Dim word As String = Nothing                                   ''~v076I~
        If Not (Pasckey >= 0 AndAlso Pasckey <= 9) Then                ''~v076R~
            Return word                                                ''~v076I~
        End If                                                         ''~v076I~
        Dim asckey As Integer = Pasckey + ASC_ZERO          'x30       ''~v076R~
        swNotEnabled = False                                           ''~v076I~
        If swShown AndAlso Not IsDisposed() Then                       ''~v076I~
            word = getDGVword(Pasckey)                                 ''~v076I~
        Else                                                           ''~v076I~
            '           For ii As Integer = 0 To ListData.Length / LISTDATA_COLNO - 1 ''~v076R~''~v101R~
            For ii As Integer = 0 To CType(ListData.Length / LISTDATA_COLNO, Integer) - 1 ''~v101I~
                Dim pos As Integer = ii * LISTDATA_COLNO                   ''~v076I~
                Dim enableid As String = ListData(pos)                 ''~v076R~
                Dim strkey As String = ListData(pos + LISTDATA_CHARKEY) ''~v076R~
                If strkey.Length > 0 Then                              ''~v076I~
                    Dim charkey As Char = strkey(0)                    ''~v076I~
                    Dim asc As Integer = AscW(charkey)                 ''~v076I~
                    If asc = asckey Then                                    ''~v076R~
                        If enableid.CompareTo(ENABLEID_ON) = 0 Then    ''~v076I~
                            word = ListData(pos + LISTDATA_SYMBOL)     ''~v076R~
                            Exit For                                   ''~v076I~
                        Else                                           ''~v076I~
                            swNotEnabled = True                        ''~v076I~
                        End If                                         ''~v076I~
                    End If                                             ''~v076I~
                End If                                                 ''~v076I~
            Next                                                       ''~v076I~
        End If                                                         ''~v076I~
        If word Is Nothing Then                                        ''~v076I~
            If swNotEnabled Then                                       ''~v076I~
                errNotEnabledKey(PswForm1, asckey)                     ''~v076I~
            Else                                                       ''~v076I~
                errNotRegisteredKey(PswForm1, asckey)                  ''~v076I~
            End If                                                     ''~v076I~
        End If                                                         ''~v076I~
        Return word                                                    ''~v076I~
    End Function    'applyWords                                        ''~v076I~
    '************************************************************************************''~v076I~
    Class DGVComparer                                                  ''~v076I~
        Implements System.Collections.IComparer                        ''~v076I~
        Private comp As Comparer                                       ''~v076I~
        Private col As Integer                                         ''~v076I~
        Public Sub New(Pcolumn As Integer)                             ''~v076I~
            col = Pcolumn                                              ''~v076I~
            comp = New Comparer(System.Globalization.CultureInfo.CurrentCulture) ''~v076I~
        End Sub                                                        ''~v076I~
        Public Function Compare(Prow1 As Object, Prow2 As Object) As Integer Implements System.Collections.IComparer.Compare ''~v076I~
            Dim rc As Integer                                          ''~v076I~
            Dim row1 As DataGridViewRow = CType(Prow1, DataGridViewRow) ''~v076I~
            Dim row2 As DataGridViewRow = CType(Prow2, DataGridViewRow) ''~v076I~
            If col = CELLNO_ENABLE Then                                ''~v076I~
                '               Dim enable1 = row1.Cells(col).Value                    ''~v076I~''~v101R~
                '               Dim enable2 = row2.Cells(col).Value                    ''~v076I~''~v101R~
                Dim enable1 As Boolean = CType(row1.Cells(col).Value, Boolean) ''~v101I~
                Dim enable2 As Boolean = CType(row2.Cells(col).Value, Boolean) ''~v101I~
                '               rc = IIf(enable1 = enable2, 0, IIf(enable1, -1, 1))    ''~v076I~''~v101R~
                rc = CType(IIf(enable1 = enable2, 0, CType(IIf(enable1, -1, 1), Integer)), Integer) ''~v101I~
            Else                                                       ''~v076I~
                '               Dim str1 = row1.Cells(col).Value                       ''~v076I~''~v101R~
                '               Dim str2 = row2.Cells(col).Value                       ''~v076I~''~v101R~
                Dim str1 As String = CType(row1.Cells(col).Value, String) ''~v101I~
                Dim str2 As String = CType(row2.Cells(col).Value, String) ''~v101I~
                If str1 Is Nothing Then                                ''~v076I~
                    '                   rc = IIf(str2 Is Nothing, 0, 1)                    ''~v076I~''~v101R~
                    rc = CType(IIf(str2 Is Nothing, 0, 1), Integer)     ''~v101I~
                ElseIf str2 Is Nothing Then                            ''~v076I~
                    rc = -1                                            ''~v076I~
                Else                                                   ''~v076I~
                    rc = System.String.Compare(str1, str2, True, System.Globalization.CultureInfo.CurrentCulture) 'true ignore case''~v076I~
                End If                                                 ''~v076I~
            End If                                                     ''~v076I~
            Return rc                                                  ''~v076I~
        End Function                                                   ''~v076I~
    End Class                                                          ''~v076I~
    '************************************************************************************''~v076I~
    Private Sub Form14_Load(sender As System.Object, e As System.EventArgs) Handles MyBase.Load ''~v076I~
        loadMRUList()                                                  ''~v076I~
    End Sub                                                            ''~v076I~
    Private Sub loadMRUList()                                          ''~v076I~
        MRU.clearMRUList(mruID)    ' loadMRUList use add method        ''~v076I~
        MRU.loadMRUListSub(mruID)                                      ''~v076I~
        setMRUListMenu()                                               ''~v076I~
    End Sub                                                            ''~v076I~
    Private Sub setMRUListMenu()                                       ''~v076I~
        selectMRUList()                                                ''~v076I~
        Dim itemMRU As ToolStripMenuItem = selectMenuItem()            ''~v076I~
        Dim ctr As Integer = MRUList.Count                             ''~v076I~
        itemMRU.DropDownItems.Clear()                                  ''~v076I~
        For ii As Integer = 0 To ctr                                   ''~v076I~
            If (ii > ClassMRU.MRULISTSZ) Then                          ''~v076I~
                Exit For                                               ''~v076I~
            End If                                                     ''~v076I~
            Dim mruitem As System.Windows.Forms.ToolStripMenuItem      ''~v076I~
            mruitem = New System.Windows.Forms.ToolStripMenuItem()     ''~v076I~
            mruitem.Size = New System.Drawing.Size(152, 22)            ''~v076I~
            If ii = 0 Then                                             ''~v076I~
                mruitem.Text = Rstr.MENU_NEWFILE                       ''~v076I~
            Else                                                       ''~v076I~
                mruitem.Text = MRUList(ii - 1)                         ''~v076I~
            End If                                                     ''~v076I~
            mruitem.Name = "MRUSymbolFile"                              ''~v076I~''~v079R~
            AddHandler mruitem.Click, AddressOf MRU_Click              ''~v076I~
            itemMRU.DropDownItems.Add(mruitem)                         ''~v076I~
        Next                                                           ''~v076I~
    End Sub                                                            ''~v076I~
    Private Sub selectMRUList()                                        ''~v076I~
        MRUList = MRU.selectMRUList(mruID)                             ''~v076I~
    End Sub                                                            ''~v076I~
    Private Function selectMenuItem() As ToolStripMenuItem             ''~v076I~
        Dim item As ToolStripMenuItem                                  ''~v076I~
        item = OpenToolStripMenuItem                                   ''~v076I~
        Return item                                                    ''~v076I~
    End Function                                                       ''~v076I~
    Private Sub MRU_Click(sender As System.Object, e As System.EventArgs) ' Handles MRUDictonaryFile.Click''~v076I~
        Dim item = DirectCast(sender, ToolStripMenuItem)               ''~v076I~
        Dim fnm As String                                              ''~v076I~
        fnm = item.Text                                                ''~v076I~
        If fnm.CompareTo(Rstr.MENU_NEWFILE) = 0 Then                   ''~v076I~
            openNewWordsFile()                                         ''~v076I~
        Else                                                           ''~v076I~
            insertMRUList(fnm)                                         ''~v076I~
            openFile(fnm)                                              ''~v076I~
        End If                                                         ''~v076I~
    End Sub                                                            ''~v076I~
    Private Sub openNewWordsFile()                                     ''~v076I~
        OpenFileDialog1.Filter = Rstr.getStr("STR_FILTER_SYMBOL")      ''~v076R~
        OpenFileDialog1.FileName = ""                                  ''~v076I~
        OpenFileDialog1.AddExtension = True   'add extension if missing''~v076I~
        OpenFileDialog1.DefaultExt = DEFAULT_EXT                       ''~v076I~
        OpenFileDialog1.FilterIndex = filterIndex                      ''~v076I~
        If OpenFileDialog1.ShowDialog() = DialogResult.OK Then         ''~v076I~
            Dim fnm As String = OpenFileDialog1.FileName               ''~v076I~
            insertMRUList(fnm)                                         ''~v076I~
            Dim basename As String = System.IO.Path.GetFileNameWithoutExtension(fnm) ''~v076I~
            filterIndex = OpenFileDialog1.FilterIndex    'save for next open''~v076I~
            openFile(fnm)                                              ''~v076I~
        End If                                                         ''~v076I~
    End Sub                                                            ''~v076I~
    Public Sub insertMRUList(Pfnm As String)                           ''~v076I~
        MRU.insertMRUList(mruID, Pfnm)      '                          ''~v076I~
        setMRUListMenu()                                               ''~v076I~
        saveMRUList()                                                  ''~v076I~
    End Sub                                                            ''~v076I~
    Private Sub saveMRUList()                                          ''~v076I~
        MRU.saveMRUList(mruID)                                         ''~v076I~
    End Sub                                                            ''~v076I~
    '*************************************************************     ''~v076I~
    Private Sub openFile(Pfnm As String)                               ''~v076I~
        Dim tmp As String() = readText(Pfnm)                           ''~v076I~
        If tmp Is Nothing Then                                         ''~v076I~
            Exit Sub                                                   ''~v076I~
        End If                                                         ''~v076I~
        ListData = tmp                                                 ''~v076I~
        fillColumn()                                                   ''~v076I~
        setTitle(Pfnm)                                                 ''~v076I~
        '       MessageBox.Show(Pfnm, Rstr.getStr("STR_INFO_MSG_WORDS_LOADED"))''~v076R~
        SB.show(SBM.MSGID.LOAD, Pfnm)                                  ''~v076I~
    End Sub                                                            ''~v076I~
    Private Sub setTitle(Pfnm As String)                               ''~v076I~
        Dim newtitle As String, oldtitle As String = Me.Text           ''~v076I~
        Dim pos As Integer = oldtitle.IndexOf("="c)                    ''~v076I~
        If pos > 0 Then                                                ''~v076I~
            newtitle = oldtitle.Substring(0, pos + 1) & Pfnm           ''~v076I~
        Else                                                           ''~v076I~
            newtitle = oldtitle & "=" & Pfnm                           ''~v076I~
        End If                                                         ''~v076I~
        Me.Text = newtitle                                             ''~v076I~
    End Sub                                                            ''~v076I~
    '*************************************************************     ''~v076I~
    Private Function readText(Pfnm As String) As String()              ''~v076I~
        Dim sr As StreamReader                                         ''~v076I~
        Dim linectr As Integer = 0                                     ''~v076I~
        Dim line As String                                             ''~v076I~
        Dim rc As String() = Nothing                                   ''~v076I~
        saveFilename = Pfnm                                            ''~v076I~
        If (Not (System.IO.File.Exists(Pfnm))) Then                    ''~v076I~
            Form1.NotFound(Pfnm)                                       ''~v076I~
            Return Nothing                                             ''~v076I~
        End If                                                         ''~v076I~
        Try                                                            ''~v076I~
            sr = New StreamReader(Pfnm, fileEncoding)                  ''~v076I~
            Do While sr.Peek() >= 0                                    ''~v076I~
                line = sr.ReadLine()                                   ''~v076I~
                linectr += 1                                           ''~v076I~
            Loop                                                       ''~v076I~
            sr.Close()                                                 ''~v076I~
            Dim tmp(linectr * LISTDATA_COLNO - 1) As String            ''~v076R~
            sr = New StreamReader(Pfnm, fileEncoding)                  ''~v076I~
            linectr = 0                                                ''~v076I~
            Do While sr.Peek() >= 0                                    ''~v076I~
                line = sr.ReadLine()                                   ''~v076I~
                Dim tmp2 As String() = line.Split(";"c)                ''~v076I~
                If Not formatChk(tmp2) Then  'err                      ''~v076I~
                    sr.Close()                                         ''~v103M~
                    errLineFormat(linectr + 1, line)                   ''~v076I~
                    Return Nothing                                     ''~v076I~
                End If                                                 ''~v076I~
                Dim pos As Integer = linectr * LISTDATA_COLNO              ''~v076I~
                tmp(pos) = tmp2(0)                                     ''~v076R~
                tmp(pos + LISTDATA_CHARKEY) = tmp2(LISTDATA_CHARKEY)   ''~v076R~
                tmp(pos + LISTDATA_SYMBOL) = tmp2(LISTDATA_SYMBOL)      ''~v076R~
                If tmp2.Length > LISTDATA_COMMENT Then                        ''~v076I~
                    tmp(pos + LISTDATA_COMMENT) = tmp2(LISTDATA_COMMENT) ''~v076R~
                Else                                                   ''~v076I~
                    tmp(pos + LISTDATA_COMMENT) = ""                   ''~v076I~
                End If                                                 ''~v076I~
                If tmp2.Length > LISTDATA_COMMENT_E Then                      ''~v076I~
                    tmp(pos + LISTDATA_COMMENT_E) = tmp2(LISTDATA_COMMENT_E) ''~v076R~
                Else                                                   ''~v076I~
                    tmp(pos + LISTDATA_COMMENT_E) = ""                 ''~v076I~
                End If                                                 ''~v076I~
                linectr += 1                                           ''~v076I~
            Loop                                                       ''~v076I~
            sr.Close()                                                 ''~v076I~
            rc = tmp                                                   ''~v076I~
        Catch ex As Exception                                          ''~v076I~
            Form1.ReadError(Pfnm, ex)                                  ''~v076I~
            Return Nothing                                             ''~v076I~
        End Try                                                        ''~v076I~
        Return rc                                                      ''~v076I~
    End Function                                                       ''~v076I~
    '*************************************************************     ''~v076I~
    Private Function formatChk(Pdata As String()) As Boolean           ''~v076I~
        If Pdata.Length < 3 Then                                       ''~v076I~
            Return False                                               ''~v076I~
        End If                                                         ''~v076I~
        Dim enableid = Pdata(0)                                        ''~v076I~
        '       If enableid.CompareTo(ENABLEID_ON) OrElse enableid.CompareTo(ENABLEID_OFF) Then ''~v076I~''~v102R~
        '       Else                                                           ''~v076I~''~v102R~
        If enableid.CompareTo(ENABLEID_ON) <> 0 AndAlso enableid.CompareTo(ENABLEID_OFF) <> 0 Then ''~v102I~
            Return False                                               ''~v076I~
        End If                                                         ''~v076I~
        If (Pdata(LISTDATA_CHARKEY).Trim()).Length > 1 Then            ''~v076R~
            Return False                                               ''~v076I~
        End If                                                         ''~v076I~
        If Pdata(LISTDATA_SYMBOL).Length = 0 Then                      ''~v076R~
            Return False                                               ''~v076I~
        End If                                                         ''~v076I~
        Return True                                                    ''~v076I~
    End Function                                                       ''~v076I~
    '*************************************************************     ''~v076I~
    Private Sub errLineFormat(Plineno As Integer, Pline As String)     ''~v076I~
        MessageBox.Show(Pline, String.Format(Rstr.getStr("STR_ERR_MSG_WORDS_FILE_LINE_FORMAT"), Plineno + 1)) ''~v076I~
    End Sub                                                            ''~v076I~
    '*************************************************************     ''~v076I~
    Private Sub saveFile()                                             ''~v076I~
        If saveFilename Is Nothing Then                                ''~v076I~
            MessageBox.Show(Rstr.getStr("STR_ERR_MSG_WORDS_FILE_NO_SAVE_FILE"), Me.Text) ''~v076I~
            Exit Sub                                                   ''~v076I~
        End If                                                         ''~v076I~
        saveFile(saveFilename)                                         ''~v076I~
        SB.show(SBM.MSGID.SAVE, saveFilename)                          ''~v076I~
    End Sub                                                            ''~v076I~
    Private Sub saveAsFile()                                           ''~v076I~
        SaveFileDialog1.Filter = Rstr.getStr("STR_FILTER_SYMBOL")      ''~v076R~
        If SaveFileDialog1.ShowDialog() = DialogResult.OK Then         ''~v076I~
            Dim fnm As String = SaveFileDialog1.FileName               ''~v076I~
            If saveFile(fnm) Then                                      ''~v076I~
                saveFilename = fnm                                     ''~v076I~
                setTitle(fnm)                                          ''~v076I~
                SB.show(SBM.MSGID.SAVEAS, saveFilename)                ''~v076I~
            End If                                                     ''~v076I~
        End If                                                         ''~v076I~
    End Sub                                                            ''~v076I~
    Private Function commitDGV() As Boolean                            ''~v076I~
        Try                                                            ''~v076I~
            If DGV.IsCurrentCellDirty Then                             ''~v076I~
                swCommitting = True                                    ''~v076I~
                DGV.CommitEdit(DataGridViewDataErrorContexts.Commit)   ''~v076I~
                swCommitting = False                                   ''~v076I~
                swDirty = False                                        ''~v076I~
            End If                                                     ''~v076I~
        Catch ex As Exception                                          ''~v076I~
            Return False                                               ''~v076I~
        End Try                                                        ''~v076I~
        Return True                                                    ''~v076I~
    End Function                                                       ''~v076I~
    Private Function saveFile(Pfnm As String) As Boolean               ''~v076I~
        swInvalid = False                                              ''~v076I~
        commitDGV()                                                    ''~v076I~
        If swInvalid Then ' set chkvalidity() from cellchanged         ''~v076I~
            Return False                                               ''~v076I~
        End If                                                         ''~v076I~
        Try                                                            ''~v076I~
            Dim str As String = getFileData()                          ''~v076I~
            If str IsNot Nothing Then                                  ''~v076I~
                System.IO.File.WriteAllText(Pfnm, str, fileEncoding)   ''~v076I~
                insertMRUList(Pfnm)                                    ''~v076I~
                '               MessageBox.Show(Pfnm, Rstr.MSG_INFO_SAVED)             ''~v076R~
            End If                                                     ''~v076I~
        Catch ex As Exception                                          ''~v076I~
            Form1.WriteError(Pfnm, ex)                                 ''~v076I~
            Return False                                               ''~v076I~
        End Try                                                        ''~v076I~
        Return True                                                    ''~v076I~
    End Function                                                       ''~v076I~
    Private Function getFileData() As String                           ''~v076I~
        Dim errctr = getListData()                                     ''~v076I~
        Dim str As String                                              ''~v076I~
        Dim sb As New StringBuilder()                                  ''~v076I~
        If errctr > 0 Then                                             ''~v076I~
            Return Nothing                                             ''~v076I~
        End If                                                         ''~v076I~
        If errctr < 0 Then     'null                                   ''~v076I~
            str = ""                                                   ''~v076I~
        Else                                                           ''~v076I~
            '           For ii As Integer = 0 To ListData.Length / LISTDATA_COLNO - 1 ''~v076R~''~v101R~
            For ii As Integer = 0 To CType(ListData.Length / LISTDATA_COLNO, Integer) - 1 ''~v101I~
                Dim pos As Integer = ii * LISTDATA_COLNO                   ''~v076I~
                sb.Append(ListData(pos) & ";" & ListData(pos + LISTDATA_CHARKEY) & ";" & ListData(pos + LISTDATA_SYMBOL) & ";" & ListData(pos + LISTDATA_COMMENT) & ";" & ListData(pos + LISTDATA_COMMENT_E) & vbCrLf) ''~v076R~
            Next                                                       ''~v076I~
            str = sb.ToString()                                        ''~v076I~
        End If                                                         ''~v076I~
        Return str                                                     ''~v076I~
    End Function                                                       ''~v076I~
    '*************************************************************     ''~v076I~
    Private Sub cutRow()                                               ''~v076I~
        commitDGV()                                                    ''~v076I~
        Dim cpos As Integer = getCurrentRowData(CPlist) ''~v076I~      ''~v079R~
        If cpos >= 0 Then                                              ''~v076I~
            CPstatus = CPSTATUS_CUT                                    ''~v076I~
            CPcutrow = cpos                                            ''~v076I~
            '           SB.show(SBM.MSGID.CUT, CPList(CELLNO_SYMBOL))                           ''~v076I~''~v079R~''~v101R~
            SB.show(SBM.MSGID.CUT, CType(CPlist(CELLNO_SYMBOL), String)) ''~v101I~
        End If                                                         ''~v076I~
    End Sub                                                            ''~v076I~
    Private Sub copyRow()                                              ''~v076I~
        commitDGV()                                                    ''~v079R~
        If getCurrentRowData(CPlist) >= 0 Then ''~v076I~               ''~v079R~
            CPstatus = CPSTATUS_COPY                                   ''~v076I~
            '           SB.show(SBM.MSGID.COPY, CPList(CELLNO_SYMBOL))                          ''~v076I~''~v079R~''~v101R~
            SB.show(SBM.MSGID.COPY, CType(CPlist(CELLNO_SYMBOL), String)) ''~v101I~
        End If                                                         ''~v076I~
    End Sub                                                            ''~v076I~
    Private Sub pasteRow()                                             ''~v076I~
        Dim cutrow As Integer                                          ''~v076I~
        If CPstatus = CPSTATUS_NONE Then                               ''~v076I~
            Exit Sub                                                   ''~v076I~
        End If                                                         ''~v076I~
        Dim cpos As Integer = iDGV.getValidCPos(True) ' true:allow addrow   ''~v076I~''~v079R~
        '       DGV.Rows.Insert(cpos, CPenable, CPcharkey, CPsymbol, False) 'false:not deleted''~v076I~
        insertRow(cpos, CPlist, False) 'false:Delete flag                                        ''~v079I~
        swUpdated = True                                               ''~v076I~
        If CPstatus = CPSTATUS_CUT Then                                ''~v076I~
            If cpos <= CPcutrow Then                                   ''~v076I~
                cutrow = CPcutrow + 1                                  ''~v076I~
            Else                                                       ''~v076I~
                cutrow = CPcutrow                                      ''~v076I~
            End If                                                     ''~v076I~
            iDGV.removeRow(cutrow)                                  ''~v076I~''~v079R~
            CPstatus = CPSTATUS_CUTDONE                                  ''~v079I~
            '           SB.show(SBM.MSGID.CUT_PASTE, CPList(CELLNO_SYMBOL))                     ''~v076I~''~v079R~''~v101R~
            SB.show(SBM.MSGID.CUT_PASTE, CType(CPlist(CELLNO_SYMBOL), String)) ''~v101I~
        Else                                                           ''~v079R~
            '           SB.show(SBM.MSGID.COPY_PASTE, CPlist(CELLNO_SYMBOL))                    ''~v076I~''~v079R~''~v101R~
            SB.show(SBM.MSGID.COPY_PASTE, CType(CPlist(CELLNO_SYMBOL), String)) ''~v101I~
        End If                                                         ''~v076I~
    End Sub                                                            ''~v076I~
    Private Function getTheRowData(Ppos As Integer) As ArrayList       ''~v079R~
        Dim lst As ArrayList = iDGV.getTheRowData(Ppos)                ''~v079I~
        Return lst                                                     ''~v079R~
    End Function                                                       ''~v079I~
    Private Function getCurrentRowData(ByRef Pplist As ArrayList) As Integer ''~v076I~''~v079R~
        Pplist = Nothing                                                 ''~v079I~
        Dim cpos As Integer                                            ''~v079I~
        Dim lst As ArrayList = iDGV.getCurrentRowData(cpos)              ''~v079I~
        If cpos < 0 Then                                               ''~v076I~
            Return -1                                                  ''~v076I~
        End If                                                         ''~v076I~
        Pplist = lst                                                     ''~v079I~
        Return cpos                                                    ''~v076I~
    End Function                                                       ''~v076I~
    Private Sub insertRow(Ppos As Integer, Plist As ArrayList, PswDelete As Boolean) ''~v079I~
        '       Dim swdel As Boolean = Plist(CELLNO_DELETE)                      ''~v079I~''~v101R~
        Dim swdel As Boolean = CType(Plist(CELLNO_DELETE), Boolean)     ''~v101R~
        Plist(CELLNO_DELETE) = False                                     ''~v079I~
        iDGV.insertRow(Ppos, Plist)                                     ''~v079I~
        Plist(CELLNO_DELETE) = swdel                                     ''~v079I~
    End Sub                                                            ''~v079I~
    Private Function createArrayList(Psrc() As String, Ppos As Integer) As ArrayList ''~v079I~
        Dim enable As Boolean                                          ''~v079I~
        Dim enableid As String = Psrc(Ppos)                            ''~v079R~
        Dim lst As New ArrayList()                                     ''~v079I~
        '       enable = IIf(enableid.CompareTo(ENABLEID_ON) = 0, True, False) ''~v079I~''~v101R~
        enable = CType(IIf(enableid.CompareTo(ENABLEID_ON) = 0, True, False), Boolean) ''~v101I~
        lst.Add(enable)                                                ''~v079I~
        lst.Add(Psrc(Ppos + LISTDATA_CHARKEY))                           ''~v079I~
        lst.Add(strSendID)                                             ''~v079I~
        lst.Add(Psrc(Ppos + LISTDATA_SYMBOL))                            ''~v079I~
        lst.Add(Psrc(Ppos + LISTDATA_COMMENT))                           ''~v079I~
        lst.Add(Psrc(Ppos + LISTDATA_COMMENT_E))                         ''~v079I~
        lst.Add(False)
        Return lst ''~v079I~
    End Function                                                       ''~v079I~
    Private Sub updateRow(Prow As Integer, Plist As ArrayList, Psrc() As String, Ppos As Integer) ''~v079I~
        '       Dim lst As ArrayList = Plist.Clone()                       ''~v079I~''~v101R~
        Dim lst As ArrayList = CType(Plist.Clone(), ArrayList)          ''~v101I~
        lst(CELLNO_ENABLE) = True                                        ''~v079I~
        lst(CELLNO_COMMENT) = Psrc(Ppos + LISTDATA_COMMENT)              ''~v079I~
        lst(CELLNO_COMMENT_E) = Psrc(Ppos + LISTDATA_COMMENT_E)          ''~v079I~
        lst(CELLNO_DELETE) = False                                       ''~v079I~
        iDGV.updateRow(Prow, lst)                                     ''~v079I~
    End Sub                                                            ''~v079I~
    Private Sub addRow(Plistdata() As String, Ppos As Integer)          ''~v079I~
        Dim lst As ArrayList = createArrayList(Plistdata, Ppos)           ''~v079I~
        iDGV.addRow(lst)                                               ''~v079I~
    End Sub                                                            ''~v079I~
    Private Sub errNotRegisteredKey(PswForm1 As Boolean, Pasckey As Integer) ''~v076I~
        Dim msg As String = String.Format(Rstr.getStr("STR_ERR_MSG_NOT_REGISTERED_KEY_SYMBOL"), ChrW(Pasckey)) ''~v076R~
        '*        If Not PswForm1 Then                                           ''~v076I~''~v123R~
        '*            If Form1.formText IsNot Nothing Then                       ''~v076I~''~v123R~
        '*                Form1.formText.showStatus(msg)                         ''~v076I~''~v123R~
        '*            End If                                                     ''~v076I~''~v123R~
        '*        Else                                                           ''~v076I~''~v123R~
        '*            Form1.MainForm.showStatus(msg)                             ''~v076I~''~v123R~
        '*        End If                                                         ''~v076I~''~v123R~
        Form1.showStatusForChild(PswForm1, msg)                        ''~v123I~
    End Sub                                                            ''~v076I~
    Private Sub errNotEnabledKey(PswForm1 As Boolean, Pasckey As Integer) ''~v076I~
        Dim msg As String = String.Format(Rstr.getStr("STR_ERR_MSG_NOT_ENABLED_KEY_SYMBOL"), ChrW(Pasckey)) ''~v076R~
        '*        If Not PswForm1 Then                                           ''~v076I~''~v123R~
        '*            If Form1.formText IsNot Nothing Then                       ''~v076I~''~v123R~
        '*                Form1.formText.showStatus(msg)                         ''~v076I~''~v123R~
        '*            End If                                                     ''~v076I~''~v123R~
        '*        Else                                                           ''~v076I~''~v123R~
        '*            Form1.MainForm.showStatus(msg)                             ''~v076I~''~v123R~
        '*        End If                                                         ''~v076I~''~v123R~
        Form1.showStatusForChild(PswForm1, msg)                        ''~v123I~
    End Sub                                                            ''~v076I~
    Private Function sendWord(Prow As Integer) As Boolean                         ''~v076I~''~v079R~
        '** by send button                                             ''~v076I~
        '       Dim row As Integer = iDGV.getValidCPos(False, Prow)                 ''~v076I~''~v079R~
        '       If row < 0 Then                                                ''~v076I~''~v079R~
        '           Return False                                               ''~v076I~''~v079R~
        '       End If                                                         ''~v076I~''~v079R~
        '       Dim symbol As String = DGV.Rows(row).Cells(CELLNO_SYMBOL).Value ''~v076R~''~v079R~
        Dim symbol As String = iDGV.getRowDataString(Prow, CELLNO_SYMBOL) ''~v079I~
        If symbol Is Nothing OrElse symbol.Length = 0 Then             ''~v076I~
            Return False                                               ''~v076I~
        End If                                                         ''~v076I~
        If swForm1 Then   'when showdialog                             ''~v076I~
            callerForm1.undoRedo.setSpecialChar(symbol)                ''~v076I~
        Else                                                           ''~v076I~
            callerForm3.undoRedo.setSpecialChar(symbol)                ''~v076R~
        End If                                                         ''~v076I~
        SB.show(SBM.MSGID.SEND, symbol)                                ''~v076I~
        Return True                                                    ''~v076I~
    End Function                                                       ''~v076I~
    Public Function chkDiscard(e As System.ComponentModel.CancelEventArgs) As Boolean ''~v076I~
        ' rc:true=continue process                                     ''~v076I~
        Dim rc As Boolean = True                                       ''~v076I~
        If swUpdated Then                                              ''~v076I~
            rc = Form1.confirmDiscard(e, Me.Text)                      ''~v076I~
        End If                                                         ''~v076I~
        Return rc                                                      ''~v076I~
    End Function                                                       ''~v076I~
    '*************************************************************************''~v123I~
    Private Sub switchTarget()                                         ''~v123I~
        If swForm1 Then                                                ''~v123I~
        	Dim frm as Form3=Form1.formText                            ''~v123I~
            If IsNothing(frm) OrElse frm.IsDisposed() Then             ''~v123I~
                showStatus(My.Resources.STR_MSG_ERR_NOT_FORM_OPENED)   ''~v123I~
                Exit Sub                                               ''~v123I~
            End If                                                     ''~v123I~
            sharedShowDlg(False)                                       ''~v123I~
            setSwitchButtonText(True)                                  ''~v123I~
        Else                                                           ''~v123I~
        	if Not Form1.MainForm.TBBES.Enabled                        ''~v123I~
                showStatus(My.Resources.STR_MSG_ERR_NOT_FORM_OPENED)   ''~v123I~
                Exit Sub                                               ''~v123I~
            End If                                                     ''~v123I~
            sharedShowDlg(True)                                        ''~v123I~
            setSwitchButtonText(False)                                 ''~v123I~
        End If                                                         ''~v123I~
    End Sub                                                            ''~v123I~
    '*************************************************************************''~v123I~
    Private Sub setSwitchButtonText(PswForm1 As Boolean)               ''~v123I~
        '* PswForm1:current form value                                 ''~v123I~
        Dim col As Color                                               ''~v123I~
        If swForm1 Then                                                ''~v123I~
            col = callerForm1.TBBES.BackColor  '*use myselt because tgt may not opened''~v123I~
        Else                                                           ''~v123I~
            col = callerForm3.TextBox1.BackColor                       ''~v123I~
        End If                                                         ''~v123I~
        ToolStripMenuItemSwitchTarget.BackColor = col                  ''~v123I~
    End Sub                                                            ''~v123I~
    '*************************************************************************''~v123I~
    Private Sub showStatus(Pmsg As String)                             ''~v123I~
        ToolStripStatusLabel1.Text = Pmsg                              ''~v123I~
    End Sub                                                            ''~v123I~

End Class