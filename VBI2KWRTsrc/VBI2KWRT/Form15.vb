'CID:''+v166R~:#72                             update#=  200;         ''~v166R~
'************************************************************************************''~v106I~
'v166 2018/03/04 partial text send;consider selection start/length     ''~v166I~
'v113 2017/12/22 put Zorder Top                                        ''~v113I~
'v111 2017/12/22 embed handler by try-catch                            ''~v111I~
'v106 2017/12/20 partially extract from image(box by mouse dragging)   ''~v106I~
'************************************************************************************''~v106I~
Public Class Form15                                                    ''~v106I~
    '* partial extraction textbox                                      ''~v113I~
    Private parentFrm As Form2                                         ''~v106R~
    '**************************************************                ''~v111I~
'   Public Sub New(PparentFrm As Form2)                                ''~v106I~''~v113R~
'       parentFrm = PparentFrm                                         ''~v106I~''~v113R~
'       InitializeComponent()                                          ''~v106I~''~v113R~
'   End Sub                                                            ''~v106I~''~v113R~
    '**************************************************                ''~v111I~
    Public Sub setParent(PparentFrm As Form2)                          ''~v111I~
    '* generic function New requires New(void)                         ''~v111I~
        parentFrm = PparentFrm                                         ''~v111I~
    End Sub                                                            ''~v111I~
    '**************************************************                ''~v106I~
    Private Sub ToolStripButtonOK_Click(ByVal sender As System.Object, ByVal e As EventArgs) Handles ToolStripButtonOK.Click''~v106I~
    	try                                                            ''~v111I~
        sendText()                                                     ''~v106I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form15 OK",ex)                         ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~v106I~
    '**************************************************                ''~v106I~
    Private Sub ToolStripButtonHelp_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButtonHelp.Click''~v106I~
    	try                                                            ''~v111I~
        showHelp()                                                     ''~v106I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form15 Help",ex)                       ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~v106I~
    '*************************************************************     ''~v106I~
    Private Sub showHelp()                                             ''~v106I~
        Dim txt As String                                              ''~v106I~
        If FormOptions.swLangEN Then                                   ''~v106I~
            txt = My.Resources.help_Form15E                            ''~v106M~
        Else                                                           ''~v106I~
            txt = My.Resources.help_Form15                             ''~v106M~
        End If                                                         ''~v106I~
        MessageBox.Show(txt, Me.Text)                                  ''~v106I~
    End Sub                                                            ''~v106I~
    '**************************************************                ''~v106I~
    Public Sub setText(Ptext As String)                                ''~v106I~
        TextBox.Text = Ptext                                           ''~v106I~
        TextBox.Select(0,0)    'start=Length=0                         ''~v106R~
        Show()                                                         ''~v106I~
    End Sub                                                            ''~v106I~
    '**************************************************                ''~v106I~
    Public Sub sendText()                                              ''~v106I~
    	Dim spos as integer=TextBox.SelectionStart                     ''~v166I~
    	Dim len as integer=TextBox.SelectionLength                     ''~v166I~
        if spos>=0 andalso len>0                                       ''~v166I~
            Dim epos As Integer = Math.Min(TextBox.Text.Length, spos + len)      ''~v166I~
            len =epos-spos                                              ''~v166I~
            parentFrm.receivePartialText(TextBox.Text.Substring(spos, len)) ''~v166I~
        Else                                                           ''~v166I~
        parentFrm.receivePartialText(TextBox.Text)                     ''~v106I~
        end if                                                         ''~v166I~
    End sub                                                            ''~v106I~

End Class