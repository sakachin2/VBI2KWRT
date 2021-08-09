'*CID:''+dateR~:#72                          update#=   29;          ''~1807I~
Public Class FormMS                                                    ''~1807I~
    Private ctrFiles As Integer                                        ''~1807R~
    Private imageFiles() As String                                     ''~1807I~
    Private swFromForm2 As Boolean                                     ''~1807I~
    Private Const DEFAULT_TEXT_UNIT = 1024                              ''~1807I~
    Private iOCR As Cocr = Nothing                                     ''~1807I~
    Private idxLang As Integer                                         ''~1807I~
    Private tbLang As ToolStripTextBox                                 ''~1807I~
    Private tbOutFile As TextBox                                       ''~1807I~
    Private tbInputFiles As TextBox                                    ''~1807I~
    Private cbLang As ToolStripComboBox                                ''~1807I~
    Private swInitialized As Boolean = False                           ''~1807I~
    Private aForm2 As Form2                                            ''~1807I~
    Private aForm1 As Form1                                            ''~1808I~
    Private aaForm2 As Form2                                           ''~1808I~
    Private langTag As String                                          ''~1807I~
    Private swEnglishDoc As Boolean = False                            ''~1807I~
    Private xText As String                                            ''~1807I~
    '**************************************************                ''~1807I~
    Private Sub FormMS_Load(sender As System.Object, e As System.EventArgs) Handles MyBase.Load ''~1807I~
        '* Trace.W("FormMS_Load entry")                                   ''~1807M~''+1809R~
        Try                                                            ''~1807M~
            Form1.setupTitlebarIcon(Me)                                ''~1807M~
            Me.ActiveControl = TextBoxOutputName   'set focus at Load time''~1807I~
        Catch ex As Exception                                          ''~1807I~
            Form1.exceptionMsg("FormMS Load", ex)                      ''~1807I~
        End Try                                                        ''~1807I~
        '* Trace.W("FormMS_Load exit")                                    ''~1807I~''+1809R~
    End Sub                                                            ''~1807I~
    '**************************************************                ''~1807I~
    Private Sub setupComboBoxLang()                                    ''~1807I~
        idxLang = iOCR.setupComboBoxLang(cbLang, idxLang)              ''~1807I~
    End Sub                                                            ''~1807I~
    '**************************************************                ''~1807I~
    Public Sub setImage(Pfnms() As String, PswForm2 As Boolean)         ''~1807R~
        '* Trace.W("FormMS.ShowMS entry")                                 ''~1807I~''+1809R~
        aForm1 = Form1.MainForm                                        ''~1808I~
        cbLang = ToolStripComboBoxLang                                 ''~1807I~
        ctrFiles = Pfnms.Length                                        ''~1807R~
        imageFiles = Pfnms                                              ''~1807I~
        swFromForm2 = PswForm2                                           ''~1807I~
        If (swFromForm2) Then                                                   ''~1807I~
            aForm2 = aForm1.formImage                            ''~1807I~''~1808R~
            aaForm2 = aForm2                                           ''~1808I~
        Else                                                           ''~1807I~
            aForm2 = Nothing
            Form1.newForm(Form2, aaForm2)                               ''~1808I~
        End If ''~1807I~
        setupForm()                                                    ''~1807R~
        swInitialized = True                                           ''~1807M~
        '* Trace.W("FormMS.ShowMS exit")                                  ''~1807R~''+1809R~
    End Sub                                                            ''~1807I~
    '**************************************************                ''~1807I~
    Public Sub setupForm()                                             ''~1807R~
        '* Trace.W("FormMS.setupForm entry swFromForm2=" & swFromForm2)   ''~1807R~''+1809R~
        If Not swFromForm2 Then                                             ''~1808I~
            aaForm2.iOCR = New Cocr()                                    ''~1808I~
        End If                                                         ''~1808I~
        iOCR = aaForm2.iOCR                                            ''~1808I~
        tbOutFile = TextBoxOutputName                                   ''~1807I~
        tbInputFiles = TextBoxInputFiles                                ''~1807R~
        setFilename()                                                  ''~1807I~
        idxLang = My.Settings.CFGF2_LangIndex                          ''~1807M~
        setupComboBoxLang()                                            ''~1807M~
        '* Trace.W("FormMS.setupForm ix\dxLang=" & idxLang)               ''~1807R~''+1809R~
    End Sub                                                            ''~1807I~
    '**************************************************                ''~1807I~
    Private Sub setFilename()                                          ''~1807I~
        '* Trace.W("FormMS.setFilename entry")                            ''~1807I~''+1809R~
        Dim sbText As System.Text.StringBuilder = New System.Text.StringBuilder(ctrFiles * 256) ''~1807I~
        For ii As Integer = 0 To ctrFiles - 1                          ''~1807I~
            Dim fnm As String = imageFiles(ii)                         ''~1807I~
            sbText.Append(fnm + vbCrLf)                                  ''~1807I~
        Next                                                           ''~1807I~
        Dim ifnms As String = sbText.ToString()                                       ''~1807I~
        tbInputFiles.Text = ifnms                                            ''~1807I~
        Dim fnm1 = imageFiles(0)                                         ''~1808R~
        Dim basename1 As String = System.IO.Path.GetFileNameWithoutExtension(fnm1) ''~1808I~
        Dim basename2 As String = System.IO.Path.GetFileNameWithoutExtension(imageFiles(ctrFiles - 1)) ''~1807I~
        Dim path As String = System.IO.Path.GetDirectoryName(fnm1)       ''~1808I~
        Dim ofnm = path & "\"c & basename1 & "_" & basename2 & "." & Form1.FILTER_DEFAULT_KANJITEXT ''~1807I~''~1808R~
        tbOutFile.Text = ofnm                                               ''~1807I~
        '* Trace.W("FormMS.setFilename exit input=" & ifnms)              ''~1807I~''+1809R~
    End Sub                                                            ''~1807I~
    '**************************************************                ''~v110I~''~1807I~
    Private Sub ToolStripComboBoxlang_Change(sender As System.Object, e As System.EventArgs) Handles ToolStripComboBoxLang.SelectedIndexChanged ''~V110R~''~v112I~''~1807I~
        If swInitialized Then                                          ''~v110I~''~1807I~
            iOCR.getSelectedLangTag(cbLang, idxLang)                   ''~v110I~''~1807I~
            My.Settings.CFGF2_LangIndex = idxLang                      ''~v110I~''~1807I~
        End If                                                         ''~v110I~''~1807I~
    End Sub                                                            ''~v110I~''~1807I~
    '**************************************************                ''~1807I~
    Private Sub On_Extract_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItemExtract.Click  ''~1807I~
        '* Trace.W("FormMS.On_extract_Click entry")                       ''~1807I~''+1809R~
        Try                                                            ''~1807I~
            image2Text()                                               ''~1807R~
            updateMRU()                                               ''~1807I~
            Dispose()                                                  ''~1807I~
        Catch ex As Exception                                          ''~1807I~
            Form1.exceptionMsg("Form2 Extract", ex)                    ''~1807I~
        End Try                                                        ''~1807I~
        '* Trace.W("FormMS.On_extract_Click exit")                        ''~1807I~''+1809R~
    End Sub                                                            ''~1807I~
    '**************************************************                ''~1807I~
    Private Sub On_Help_click(sender As Object, e As EventArgs) Handles ToolStripMenuHelp.Click, ToolStripMenuHelp.Click ''~1807I~
        '* Trace.W("FormMS.On_Hexp_Click entry")                          ''~1807I~''+1809R~
        Try                                                            ''~1807I~
            showHelp()                                                 ''~1807I~
        Catch ex As Exception                                          ''~1807I~
            Form1.exceptionMsg("FormMS Help", ex)                      ''~1807I~
        End Try                                                        ''~1807I~
        '* Trace.W("FormMS.On_Hexp_Click exit")                           ''~1807I~''+1809R~
    End Sub                                                            ''~1807I~
    '*************************************************************     ''~1807I~
    Private Sub showHelp()                                             ''~1807I~
        Dim txt As String                                              ''~1807I~
        If FormOptions.swLangEN Then                                   ''~1807I~
            txt = My.Resources.help_formMSE                            ''~1807I~
        Else                                                           ''~1807I~
            txt = My.Resources.help_formMS                             ''~1807I~
        End If                                                         ''~1807I~
        Dim title As String = Rstr.getStr("STR_MULTI_SELECT_FILE")      ''~1807I~
        MessageBox.Show(txt, title)                                     ''~1807I~
    End Sub                                                            ''~1807I~
    '*************************************************************     ''~1807I~
    Private Sub updateMRU()                                            ''~1807I~
        '* Trace.W("FormMS.updateMRU entry")                              ''~1807I~''+1809R~
        For ii As Integer = 0 To ctrFiles - 1                          ''~1807I~
            Dim fnm As String = imageFiles(ii)                                       ''~1807I~
            aForm1.insertMRUList(1, fnm)      '1:imagefile                    ''~1807I~''~1808R~
            '* Trace.W("FormMS.updateMRU insert fnm=" & fnm)              ''~1807I~''+1809R~
        Next                                                           ''~1807I~
        '* Trace.W("FormMS.updateMRU exit")                               ''~1807I~''+1809R~
    End Sub                                                            ''~1807I~
    '*************************************************************     ''~1807I~
    Private Sub image2Text()                                           ''~1807I~
        '* Trace.W("FormMS.image2Text entry ctrFiles=" & ctrFiles)        ''~1807I~''+1809R~
        langTag = getSelectedLangTag()                                 ''~1807I~
        Dim sbText As System.Text.StringBuilder = New System.Text.StringBuilder(ctrFiles * DEFAULT_TEXT_UNIT) ''~1807I~
        For ii As Integer = 0 To ctrFiles - 1                          ''~1807I~
            Dim fnm As String = imageFiles(ii)                         ''~1807I~
            image2Text(fnm)                                            ''~1807I~
            If ii <> 0 Then                                                   ''~1808I~
                sbText.Append(vbCrLf & vbCrLf)                         ''~1808I~
            End If                                                     ''~1808I~
            sbText.Append("*****(" & (ii + 1) & ")***** " & fnm & vbCrLf & vbCrLf) ''~1807I~''~1808R~
            sbText.Append(xText)                                       ''~1807I~
        Next                                                           ''~1807I~
        sbText.Append(vbCrLf & vbCrLf & "********** total " & ctrFiles & " files extracted. **********" & vbCrLf & vbCrLf) ''~1807I~''~1808R~
        showOutput(sbText.ToString())                                  ''~1807I~
        '* Trace.W("FormMS.image2Text exit")                              ''~1807I~''+1809R~
    End Sub                                                            ''~1807I~
    '*************************************************************     ''~1807I~
    Private Sub showOutput(Ptext As String)                            ''~1807I~
        Dim ofnm As String = tbOutFile.Text                           ''~1807I~
        Dim swDoI2K As Boolean = False                                 ''~1808I~
        Dim swNewForm3 As Boolean                                      ''~1807R~
        swNewForm3 = Form1.formText Is Nothing OrElse Form1.formText.IsDisposed ''~1808R~''~1809R~
        If swNewForm3 Then                                             ''~1807I~''~1808R~
            Form1.formText = New Form3()   ' open after extraction succeeded''~1807I~''~1808R~''~1809R~
            Form1.formText.setImage(ofnm, Ptext, swEnglishDoc)         ''~1807I~''~1808R~''~1809R~
        Else                                                           ''~1807I~
            If Not Form1.formText.chkDiscard2(swDoI2K) Then           ''~1808R~''~1809R~
                Exit Sub                                               ''~1808I~
            End If                                                     ''~1808I~
            Form1.formText.setImage(ofnm, Ptext, swEnglishDoc)         ''~1807I~''~1808R~''~1809R~
            Form1.showTop(Form1.formText)                              ''~1807R~''~1808R~''~1809R~
        End If                                                         ''~1807I~
        Form1.formText.Show()                                          ''~1807R~''~1808R~''~1809R~
    End Sub                                                            ''~1807I~
    '*************************************************************     ''~1807I~
    Private Sub image2Text(Pfnm As String)                             ''~1807I~
        '* Trace.W("FormMS.image2Text entry fnm=" & Pfnm)                 ''~1807I~''+1809R~
        xText = aaForm2.Image2TextMultiSelect(Pfnm, langTag)                      ''~1807I~''~1808R~
        '* Trace.W("FormMS.image2Text exit fnm=" & Pfnm & ",text=" & xText) ''~1807R~''+1809R~
    End Sub                                                            ''~1807I~
    '**************************************************                ''~1807I~
    Private Function getSelectedLangTag() As String                    ''~1807I~
        Dim tag As String = iOCR.getSelectedLangTag(cbLang, idxLang)   ''~1807I~
        swEnglishDoc = Not tag.StartsWith(Form2.LANG_TAG_JP)                 ''~1807I~
        Return tag                                                     ''~1807I~
    End Function                                                       ''~1807I~
End Class