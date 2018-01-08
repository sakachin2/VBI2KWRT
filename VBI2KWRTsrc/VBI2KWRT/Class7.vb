'CID:''+v100R~:#72                          update#= 17;              ''~7619R~''~v100R~
'************************************************************************************''~v078I~''~v100I~
'v100 2017/12/15 porting from MOD to Microsoft Ocr Library for Windows ''~v100I~
'************************************************************************************''~v078I~''~v100I~
Imports System.IO                                                      ''~7619I~
Public Class Trace                                                     ''~7619R~
    Public Shared swTrace As Boolean = False                           ''~7619R~
    Private Shared fs As System.IO.TextWriter                          ''~7619I~
    Private Shared fnm As String                                       ''~7619I~
    Public Shared Sub W(Ptext As String)                               ''~7619R~
#If DEBUG Then                                                              ''~v100I~
        If Not swTrace                                                 ''~7619I~
            Exit Sub                                                   ''~7619I~
        End If                                                         ''~7619I~
        Try                                                            ''~7619I~
            If IsNothing(fs) Then                                               ''~7619I~
                fsOpen()                                                   ''~7619I~
            End If                                                         ''~7619I~
            fsWrite(Ptext)                                                 ''~7619I~
        Catch ex As IOException                                        ''~7619I~
            MessageBox.Show(ex.ToString, "Trace")                            ''~7619I~
        End Try                                                        ''~7619I~
#End If                                                                ''~v100I~
    End Sub                                                            ''~7619R~
    Public Shared Sub fsOpen(Pfnm As String)                           ''~7619I~
#If DEBUG Then                                                              ''~v100I~
        fs = New StreamWriter(Pfnm)                                      ''~7619I~
#End If                                                                ''~v100I~
    End Sub                                                            ''~7619I~
    Private Shared Sub fsOpen()                                        ''~7619M~
        fnm = ".\VBI2KWRT.trc"                                            ''~7619R~''~v100R~
        fs = New StreamWriter(fnm)                                       ''~7619I~
    End Sub                                                            ''~7619M~
    Public Shared Sub fsClose()                                        ''~7619I~
#If DEBUG Then                                                              ''~v100I~
        If Not IsNothing(fs) Then                                      ''~7619R~
            fs.Close()                                                 ''~7619I~
            fs = Nothing                                                 ''~7619I~
        End If                                                         ''~7619I~
#End If                                                                ''~v100I~
    End Sub                                                            ''~7619I~
    Private Shared Sub fsWrite(Ptext As String)                        ''~7619I~
        If Not IsNothing(fs) Then                                      ''~7619I~
            fs.WriteLine(Ptext)                                            ''~7619I~
            fs.Flush()                                                     ''~7619I~
        End If                                                         ''~7619I~
    End Sub                                                            ''~7619I~
    Public Shared Sub setOn()                                          ''+v100R~
        swTrace = True                                                   ''~v100I~
    End Sub                                                            ''~v100I~
    Public Shared Sub setOff()                                         ''+v100R~
        swTrace = False                                                  ''~v100I~
    End Sub                                                            ''~v100I~
End Class
