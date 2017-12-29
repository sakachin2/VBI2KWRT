'*CID:''+dateR~:#72                          update#=  165;           ''~7621I~
Public Class MsgBox                                                    ''~7621R~
    ''~7621I~
    Public Shared Sub ShowMsg(Pmsg As String, Ptitle As String)           ''~7621R~
        Dim mb As MsgBox                                               ''~7621I~
        mb = New MsgBox()                                                ''~7621I~
        mb.Text = Ptitle                                               ''~7621R~
        mb.TextBoxMessage.Text = Pmsg                                         ''~7621R~
        mb.Show()                                                      ''~7621R~
    End Sub                                                            ''~7621R~
    Private Sub Form4_Shown(sender As System.Object, e As System.EventArgs) Handles Me.Shown ''+7621I~
        TextBoxMessage.DeselectAll()                                ''+7621M~
    End Sub                                                            ''+7621I~
End Class