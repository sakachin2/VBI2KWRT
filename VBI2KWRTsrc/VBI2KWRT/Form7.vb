''*CID:''+dateR~:#72                          update#=   12;          ''+7618R~
Public Class Form7                                                     ''~7515R~
'localization done                                                     ''~7618I~
    '** add special key                                                    ''~7604I~
    Public strKey As String                                            ''~7515R~
    Public strComment As String                                        ''~7515I~
    ''~7515I~
    Sub New()                                                          ''~7515I~
        initDlg()                                                      ''~7515I~
    End Sub                                                            ''~7515I~
    Private Sub initDlg()                                              ''~7515I~
        setLang()   'should set CurrentUICulture before InitializeComponent''~7614I~
        InitializeComponent()                                          ''~7515I~
        Form1.setupTitlebarIcon(Me)                                    ''~7612I~
    End Sub                                                            ''~7515I~
    Private Sub ButtonCancel_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonCancel.Click ''~7515I~
        Me.Close()                                                     ''~7515I~
    End Sub 'resize                                                    ''~7515I~
    Private Sub ButtonOK_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonOK.Click ''~7515I~
        Dim str As String = TextBox1.Text                                ''~7515I~
        Me.DialogResult = DialogResult.NONE                              ''~7516I~
        If str.IndexOf(";"c) >= 0 Then                                           ''~7515I~
            '           MessageBox.Show(""";"" は使用できません")                   ''~7515I~''~7618R~
            MessageBox.Show(Rstr.MSG_ERR_SPECIAL_KEY)                      ''~7618I~
            Exit Sub                                                   ''~7515I~
        End If                                                         ''~7515I~
        If str.Trim.CompareTo("") = 0 Then                             ''~7516I~
            '           MessageBox.Show("文字の指定がありません")                  ''~7516I~''~7618R~
            MessageBox.Show(Rstr.MSG_ERR_SPECIAL_KEY_NULL)                  ''~7618I~
            Exit Sub                                                   ''~7516I~
        End If                                                         ''~7516I~
        strKey = str                                                     ''~7515I~
        str = TextBox2.Text                                             ''~7515I~
        strCommenT = str                                                 ''~7515I~
        Me.DialogResult = DialogResult.OK                                ''~7516I~
        Me.Close()                                                     ''~7515I~
    End Sub 'resize                                                    ''~7515I~
    Private Sub ButtonHelp_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ButtonHelp.Click ''~7515I~
        showHelp()                                                     ''~7515I~
    End Sub                                                            ''~7515I~
    Private Sub showHelp()                                             ''~7515I~
        Dim txt As String                                              ''~7515I~
        If FormOptions.swLangEN Then                                   ''~7614I~
            txt = My.Resources.help_form7E                             ''~7614I~
        Else                                                           ''~7614I~
            txt = My.Resources.help_form7                                  ''~7515I~''~7614R~
        End If                                                         ''~7614I~
        MessageBox.Show(txt, Me.Text)                                           ''~7515I~''~7614R~
    End Sub                                                            ''~7515I~
    Private Sub setLang()                                              ''~7614I~
        FormOptions.setLang()                                          ''~7614I~
    End Sub                                                            ''~7614I~
End Class