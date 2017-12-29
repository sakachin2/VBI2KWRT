'CID:''+v101R~:#72                             update#=  22;        ''~v101R~
'************************************************************************************''~v030I~
'v101 2017/12/16 Conversion warning                                    ''~v101I~
'v072 2017/09/26 Display doc option at receive text                    ''~v072I~
'v063 2017/09/24 support kanji file encoded by UTF8                    ''~v063I~
'v030 2017/09/21 new option dialog for each document                   ''~v030I~
'************************************************************************************''~v030I~
'*** Document option     **************                                ''~v030I~
Public Class DocOptions                                                ''~v030R~
    Public Shared swBES99 As Boolean                                   ''~v030I~
    Public Shared swKatakanaDoc As Boolean                             ''~v030I~
    Public Shared swEnglishDoc As Boolean                              ''~v030I~
    Public Shared swUTF8 As Boolean                              ''~v063I~
    Public Shared dlgDocOptions As DocOptions                          ''~v072I~
    '****************************************************************      ''~v030I~
    '   Public Shared Sub showDlg(Pform3 As Form3)                         ''~v030R~''~v063R~
'   Public Shared Function showDlg(Pform3 As Form3)                    ''~v063I~''~v101R~
    Public Shared Function showDlg(Pform3 As Form3) as Integer         ''~v101I~
        '*** from Form3                                                    ''~v030I~
        Dim dlg As DocOptions = New DocOptions()                         ''~v030I~
        dlgDocOptions = dlg                                              ''~v072I~
        '       dlg.show()                                                     ''~v030I~''~v063R~
        Dim rc As Integer = dlg.showDialog()                             ''~v063I~
        Return rc                                                      ''~v063I~
        '   End Sub                                                            ''~v030I~''~v063R~
    End Function                                                       ''~v063I~
    Sub New()                                                          ''~v030I~
        setLang()   'should set CurrentUICulture before InitializeComponent''~v030I~
        InitializeComponent()                                          ''~v030I~
        Form1.setupTitlebarIcon(Me)                                    ''~v030I~
        initByCFG()                                                    ''~v030I~
        showOptions()                                                  ''~v030I~
    End Sub                                                            ''~v030I~
    Private Sub setLang()                                              ''~v030I~
        FormOptions.setLang()                                          ''~v030I~
    End Sub                                                            ''~v030I~
    Public Shared Sub initByCFG()                                      ''~v030R~
        swBES99 = My.Settings.CFGF12_swBES99                            ''~v030I~
        swKatakanaDoc = My.Settings.CFGF12_swKatakanaDoc                ''~v030I~
        swEnglishDoc = My.Settings.CFGF12_swEnglishDoc                 ''~v030I~
        swUTF8 = My.Settings.CFGF12_swUTF8                       ''~v063I~
    End Sub                                                            ''~v030I~
    Private Sub saveCFG()                                              ''~v030I~
        My.Settings.CFGF12_swBES99 = swBES99                             ''~v030I~
        My.Settings.CFGF12_swKatakanaDoc = swKatakanaDoc                 ''~v030I~
        My.Settings.CFGF12_swEnglishDoc = swEnglishDoc                 ''~v030I~
        My.Settings.CFGF12_swUTF8 = swUTF8                       ''~v063I~
    End Sub                                                            ''~v030I~
    Private Sub FormOptions_Load(sender As System.Object, e As System.EventArgs) Handles MyBase.Load ''~v030I~
        Me.ActiveControl = Me.ButtonOK    'set focus at Load time      ''~v030I~
    End Sub                                                            ''~v030I~
    Private Sub ButtonOK_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonOK.Click ''~v030I~
        getOptions()  'also saveCFG                                    ''~v030I~
        Me.Close()                                                     ''~v030I~
        Me.DialogResult = DialogResult.OK                                ''~v063I~
    End Sub 'resize                                                    ''~v030I~
    Private Sub ButtonCancel_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonCancel.Click ''~v030I~
        Me.Close()                                                     ''~v030I~
        Me.DialogResult = DialogResult.CANCEL                            ''~v063I~
    End Sub                                                            ''~v030I~
    Private Sub ButtonHelp_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonHelp.Click ''~v030I~
        showHelp()                                                     ''~v030I~
    End Sub                                                            ''~v030I~
    Private Sub showOptions()                                          ''~v030I~
        CheckBoxBES99.Checked = swBES99                                ''~v030I~
        CheckBoxKatakanaDoc.Checked = swKatakanaDoc                    ''~v030I~
        CheckBoxEnglishDoc.Checked = swEnglishDoc                      ''~v030I~
        CheckBoxUTF8.Checked = swUTF8                                  ''~v063I~
        Me.DialogResult = DialogResult.None                            ''~v063I~
    End Sub 'resize                                                    ''~v030I~
    Private Function getOptions() As Boolean                           ''~v030I~
        swBES99 = CheckBoxBES99.Checked                                ''~v030I~
        swKatakanaDoc = CheckBoxKatakanaDoc.Checked                    ''~v030I~
        swEnglishDoc = CheckBoxEnglishDoc.Checked                      ''~v030I~
        swUTF8 = CheckBoxUTF8.Checked                                  ''~v063I~
        saveCFG()                                                      ''~v030I~
        Return True                                                    ''~v030I~
    End Function                                                       ''~v030I~
    Private Sub showHelp()                                             ''~v030I~
        Dim txt As String                                              ''~v030I~
        If FormOptions.swLangEN Then                                   ''~v030I~
            txt = My.Resources.help_form12E                            ''~v030I~
        Else                                                           ''~v030I~
            txt = My.Resources.help_form12                             ''~v030I~
        End If                                                         ''~v030I~
        MessageBox.Show(txt, Me.Text)                                  ''~v030I~
    End Sub                                                            ''~v030I~
    Public Shared Function getDocOptions() As String                   ''~v072I~
        '** from Form1 to display status msg                               ''~v072I~
        Dim strOptions, strBES99, strKatakanaDoc, strEnglishDoc As String ''~v072I~
        Dim dlg As DocOptions = dlgDocOptions                             ''~v072I~
        if dlg is Nothing                                              ''~v072I~
        	dlg=New DocOptions()                                       ''~v072I~
        end if                                                         ''~v072I~
'       strBES99 = IIF(swBES99, "On", "Off")                               ''~v072I~''+v101R~
        strBES99 = CType(IIF(swBES99, "On", "Off"),String)             ''+v101I~
'       strKatakanaDoc = IIF(swKatakanaDoc, "On", "Off")                   ''~v072I~''+v101R~
        strKatakanaDoc = CType(IIF(swKatakanaDoc, "On", "Off"),String) ''+v101I~
'       strEnglishDoc = IIF(swEnglishDoc, "On", "Off")                     ''~v072I~''+v101R~
        strEnglishDoc = CType(IIF(swEnglishDoc, "On", "Off"),String)   ''+v101I~
        strOptions = dlg.CheckBoxBES99.Text & "-" & strBES99 & "," _
      & dlg.CheckBoxKatakanaDoc.Text & "-" & strKatakanaDoc & "," _
      & dlg.CheckBoxEnglishDoc.Text & "-" & strEnglishDoc  ''~v072R~
        Return strOptions                                              ''~v072I~
    End Function                                                       ''~v072I~

End Class