'CID:''+va05R~:#72                             update#=  159;         ''~va04R~''~va05R~
'************************************************************************************''~v026I~''~v100I~
'va05 2017/12/26 ext name Jpeg-->jpg,icon-->ico,tiff->tif              ''~va05I~
'va04 2017/12/25 save cut image to file                                ''~va04I~
'v110 2017/12/22 StringConstant reset required when lang changed       ''~v110I~
'v106 2017/12/20 partially extract from image(box by mouse dragging)   ''~v106I~
'v104 2017/12/20 drop space between chars of extracted text            ''~v104I~
'v100 2017/12/15 porting from MOD to Microsoft Ocr Library for Windows ''~v100I~
'************************************************************************************''~v100I~
Imports System.Drawing.Imaging
Imports System.IO
Imports System.Runtime.CompilerServices
Imports System.Threading.Tasks
Imports System.Text
Imports Windows.Foundation
Imports Windows.Globalization
Imports Windows.Graphics.Imaging
Imports Windows.Media.Ocr
Imports Windows.Storage
Imports Windows.Storage.Pickers
Imports Windows.Storage.Streams
''~v@@@I~
Public Class Cocr

    Private Const CNM = "Cocr:"
    Private softBitmap As SoftwareBitmap
    Dim tbLang As New DataTable()
    Public Const LANG_TAG = "tag"
    Public Const LANG_NAME = "name"
    Private imageFilename As String
    Private xText As String
    Private fileBMP As Bitmap
    Private result As OcrResult
    Private swOK As Boolean
    Private pendingStatusMsg As String = Nothing                       ''~v100I~
    Public statusMsg As String                                         ''~v100R~
    ''~v106I~
    Private clipRect As Rectangle                                      ''~v106I~
    Private swRectBMP As Boolean                                       ''~v106I~
    Private bmpRect As Bitmap                                          ''~v106I~
    Private scaleNew As Double                                         ''~v106I~
    '**************************************************************************************
    Public Function setupComboBoxLang(Pcb As ToolStripComboBox, Pidxcfg As Integer) As Integer ''~v100R~
        Dim cb As ToolStripComboBox = Pcb
        Dim defaulttag As String = Language.CurrentInputMethodLanguageTag
        setupDataTableLang() 'setup tbLang
        cb.ComboBox.DataSource = tbLang
        cb.ComboBox.DisplayMember = LANG_NAME
        cb.ComboBox.ValueMember = LANG_TAG
        '       cb.Text = "Language"
        Dim idx As Integer = 0                                   ''~v100I~
        For Each row As DataRow In tbLang.Rows
            Dim tag As String = CType(row(LANG_TAG), String)            ''~v100R~
            If (Pidxcfg < 0) Then ' Not selected Then initially                   ''~v100I~
                If tag.CompareTo(defaulttag) = 0 Then
                    cb.Text = CType(row(LANG_NAME), String)                 ''~v100R~
                    cb.SelectedIndex = idx

                End If
            Else                                                         ''~v100I~
                If idx = Pidxcfg Then    'previously selected                ''~v100I~
                    cb.Text = CType(row(LANG_NAME), String)                ''~v100I~
                    cb.SelectedIndex = idx
                End If                                                     ''~v100I~
            End If                                                       ''~v100I~
            idx += 1                                                     ''~v100I~
        Next
        Return cb.SelectedIndex
    End Function
    '**************************************************
    Private Sub setupDataTableLang()
        Dim tb As New DataTable()
        tb.Columns.Add(LANG_TAG, GetType(String))
        tb.Columns.Add(LANG_NAME, GetType(String))
        Dim langlist = OcrEngine.AvailableRecognizerLanguages
        For Each item As Language In langlist
            Dim row As DataRow = tb.NewRow()
            row(LANG_TAG) = item.LanguageTag
            If FormOptions.swLangEN Then                                     ''~v110I~
                row(LANG_NAME) = item.NativeName                       ''~v110I~
            Else                                                       ''~v110I~
                row(LANG_NAME) = item.DisplayName
            End If                                                     ''~v110I~
            tb.Rows.Add(row)
        Next
        tb.AcceptChanges()
        tbLang = tb
    End Sub
    '**************************************************
    Public Function getSelectedLangTag(Pcb As ToolStripComboBox, ByRef Ppidxlang As Integer) As String ''~v100R~
        Dim cb As ToolStripComboBox = Pcb
        Dim idx As Integer = cb.SelectedIndex
        Dim tag As String = CType(tbLang.Rows(idx)(LANG_TAG), String)   ''~v100R~
        Ppidxlang = idx                                                  ''~v100I~
        Return tag
    End Function
    '**************************************************                ''~v106I~
    '* set clip box info before extact                                 ''~v106I~
    Public Sub setRect(PswRectBMP As Boolean, PbmpRect As Bitmap, PscaleNew As Double, PclipRect As Rectangle) ''~v106I~
        swRectBMP = PswRectBMP                                         ''~v106I~
        bmpRect = PbmpRect                                             ''~v106I~
        scaleNew = PscaleNew                                           ''~v106I~
        clipRect = PclipRect                                           ''~v106I~
    End Sub                                                            ''~v106I~
    '**************************************************                ''~v106I~
    Public Function cutBMPRect(PorgBMP As Bitmap) As Bitmap            ''~v106I~
        Dim xx, yy, ww, hh As Integer                                  ''~v106I~
        xx = CType(clipRect.X / scaleNew, Integer) 'dest and src position''~v106I~
        yy = CType(clipRect.Y / scaleNew, Integer)                     ''~v106I~
        ww = CType(clipRect.Width / scaleNew, Integer)                 ''~v106I~
        hh = CType(clipRect.Height / scaleNew, Integer)                ''~v106I~
        Dim tgtRect As Rectangle = New Rectangle(xx, yy, ww, hh)       ''~v106I~
#If False Then                                                         ''~va04I~
        Dim bmp As Bitmap = New Bitmap(PorgBMP.Width, PorgBMP.Height)  ''~v106I~
        Dim g = Graphics.FromImage(bmp)                                ''~v106I~
        Dim unit As GraphicsUnit = GraphicsUnit.Pixel                    ''~v106I~
        g.DrawImage(PorgBMP, tgtRect, xx, yy, ww, hh, unit)              ''~v106I~
        g.Dispose()                                                    ''~v106I~
#Else                                                                  ''~va04I~
        Dim bmp As Bitmap = cutImage(PorgBMP, tgtRect)                 ''~va04I~
#End If                                                                ''~va04I~
'*      Trace.W("cutBMPRect org W=" & PorgBMP.Width & ",H=" & PorgBMP.Height) ''~v106I~''+va05R~
'*      Trace.W("cutBMPRect clipRect X=" & clipRect.X & ",Y=" & clipRect.Y & ",W=" & clipRect.Width & ",H=" & clipRect.Height) ''~v106I~''+va05R~
'*      Trace.W("cutBMPRect scale=" & scaleNew)                        ''~v106I~''+va05R~
'*      Trace.W("cutBMPRect xx=" & xx & ",yy=" & yy & ",ww=" & ww & ",hh=" & hh) ''~v106I~''+va05R~
'*      Trace.W("cutBMPRect clip W=" & bmp.Width & ",H=" & bmp.Height) ''~v106I~''+va05R~
        '       bmp.Save("W:\cutbmprect.bmp", ImageFormat.BMP) '@@@@test       ''~v106I~
        Return bmp                                                     ''~v106I~
    End Function                                                       ''~v106I~
    '**************************************************                ''~va04I~
    Public Function saveImage(Pbasename As String, Pextname As String, PswRectBMP As Boolean, PorgBMP As Bitmap, Pscale As Double, Prect As Rectangle) as String''~va04R~
    '* from Form2                                                      ''~va04I~
        Dim bmp As Bitmap                                              ''~va04I~
    	if PswRectBMP 'clipped                                         ''~va04I~
            Dim xx, yy, ww, hh As Integer                              ''~va04I~
            xx = CType(Prect.X / Pscale, Integer) 'dest and src position''~va04I~
            yy = CType(Prect.Y / Pscale, Integer)                      ''~va04I~
            ww = CType(Prect.Width / Pscale, Integer)                  ''~va04I~
            hh = CType(Prect.Height / Pscale, Integer)                 ''~va04I~
            Dim tgtRect As Rectangle = New Rectangle(xx, yy, ww, hh)   ''~va04I~
            bmp = cutImage(PorgBMP, tgtRect)                           ''~va04I~
        else                                                           ''~va04I~
        	bmp=PorgBMP                                                ''~va04I~
        end if                                                         ''~va04I~
        Dim fmt As ImageFormat = str2Fmt(Pextname)                     ''~va04I~
        Dim fnm as String=saveImage(bmp, Pbasename, fmt)               ''~va04R~
        if PswRectBMP                                                  ''~va04I~
        	bmp.Dispose()                                              ''~va04I~
        end if                                                         ''~va04I~
        return fnm                                                     ''~va04I~
    End Function                                                       ''~va04R~
#If False Then
    '*************************************************************
    Public Function extractText(Pfnm As String, PfileBMP As Bitmap, Ptag As String, ByRef Pptext As String) As Boolean
        imageFilename = Pfnm
        fileBMP = PfileBMP
        xText = ""
        result = Nothing
        swOK = await extractTextAsync(Pfnm, Ptag)
        Pptext = xText
        Return swOK
    End Function
#Else
    '*************************************************************
    Public Function extractText(Pfnm As String, PfileBMP As Bitmap, Ptag As String, ByRef Pptext As String) As Boolean
        imageFilename = Pfnm
        fileBMP = PfileBMP
        If swRectBMP Then                                              ''~v106M~
            fileBMP = cutBMPRect(fileBMP)                              ''~v106M~
        End If                                                         ''~v106M~
        xText = ""
        result = Nothing
        statusMsg=Nothing                                              ''~v100R~
        Dim t As Task = Task.Run(Async Function()
                                     swOK = Await extractTextAsync(Pfnm, Ptag)
                                 End Function)
        t.Wait()
        Pptext = xText
        Return swOK
    End Function
#End If
    '*************************************************************
    Private Async Function extractTextAsync(Pfnm As String, Ptag As String) As Task(Of Boolean)
        Try
            softBitmap = Await LoadImage(Pfnm)
            If softBitmap Is Nothing Then
                Return False
            End If
            result = Await callOCR(Pfnm, softBitmap, Ptag)
            If result Is Nothing Then
                xText = "Extract failed"
                Return False
            End If
            xText = result.Text
            xText = makeLines(xText.Length) 'insert crlf between lines
            swOK = True
        Catch ex As Exception
            showStatus(CNM & "extractText exception:" & ex.Message)
            xText = ex.Message
        End Try
        Return True
    End Function
    '*************************************************************
    Private Async Function LoadImage(Pfnm As String) As Task(Of SoftwareBitmap)
        Dim buff As Byte() = getImageBuff()
        Dim softbmp As SoftwareBitmap = Nothing
        Try
            Dim mem As MemoryStream = New MemoryStream(buff)
            mem.Position = 0
            Dim stream = Await ConvertToRandomAccessStream(mem)
            softbmp = Await LoadImage(stream)
        Catch ex As Exception
            showStatus(CNM & "LoadImage file exception:" & Pfnm & ":" & ex.Message)
        End Try
        Return softbmp
    End Function
    '*************************************************************
    Private Function getImageBuff() As Byte()
        Dim buff As Byte() = Nothing
        Try
            Dim ms As MemoryStream = New MemoryStream()                ''~v100R~
            Dim bmp As Bitmap = fileBMP                                ''~v100R~
            Dim fmt As ImageFormat
            fmt = ImageFormat.Bmp
            bmp.Save(ms, fmt)
            ms.Close()
            buff = ms.ToArray()
        Catch ex As Exception
            showStatus(CNM & "getImageBuff exception:" & ex.Message)
        End Try
        Return buff
    End Function
    '*************************************************************
    Private Async Function ConvertToRandomAccessStream(Pms As MemoryStream) As Task(Of IRandomAccessStream)

        Dim randomAccessStream As InMemoryRandomAccessStream = New InMemoryRandomAccessStream()
        Dim outputStream As IOutputStream = randomAccessStream.GetOutputStreamAt(0)
        Dim dw As DataWriter = New DataWriter(outputStream)
        Try
            dw.WriteBytes(Pms.ToArray())
            Dim memtask = New Task(Sub()
                                       dw.WriteBytes(Pms.ToArray())
                                   End Sub)
            memtask.Start()
            Await memtask
            Await dw.StoreAsync()
            Await outputStream.FlushAsync()
        Catch ex As Exception
            showStatus(CNM & "ConvertToRandomAccess:" & ex.Message)
        End Try
        Return randomAccessStream
    End Function
    '*************************************************************
    Private Async Function LoadImage(Pstream As IRandomAccessStream) As Task(Of SoftwareBitmap)
        Try
            Dim decoder = Await BitmapDecoder.CreateAsync(Pstream)
            Dim softbmp = Await decoder.GetSoftwareBitmapAsync()
            Return softbmp
        Catch ex As Exception
            showStatus(CNM & "LoadImage stream :" & ex.Message)
        End Try
        Return Nothing
    End Function
    '*************************************************************
    Private Async Function callOCR(Pfnm As String, Pbmp As SoftwareBitmap, PlangTag As String) As Task(Of OcrResult)
        ' to support en languagepack will be set by ControlPanel
        Dim lng As Language = New Language(PlangTag)
        Dim engine As OcrEngine = OcrEngine.TryCreateFromLanguage(lng)
        Dim result As OcrResult = Nothing
        Try
            result = Await engine.RecognizeAsync(Pbmp)
        Catch ex As Exception
            showStatus(CNM & "Extract failed:" & imageFilename & ":" & ex.Message)
        End Try
        Return result
    End Function
    '*************************************************************
    Public Function markWords(Pbmp As Bitmap) As Boolean
        '** avoid exceotion:Indexed Pixel at Graphics.FromImage for mono color image
        Try
            '********************
            Dim bmpDraw As Bitmap = Pbmp
            Dim g = Graphics.FromImage(bmpDraw)
            Dim br As Brush = New SolidBrush(System.Drawing.Color.FromArgb(&H20, System.Drawing.Color.Blue))
            '           Dim text As String = ""
            For Each line As OcrLine In result.Lines
                '               text += line.Text & " "
                '               Trace.W("Line Text=" & line.Text)
                For Each word As OcrWord In line.Words
                    Dim brect As Windows.Foundation.Rect = word.BoundingRect
                    Dim rect As Rectangle = New System.Drawing.Rectangle(CType(brect.X, Integer), CType(brect.Y, Integer), CType(brect.Width, Integer), CType(brect.Height, Integer)) ''~v100R~
                    '                   Trace.W("Word Text=" & word.Text & ",X=" & brect.X & ",Y=" & brect.Y & ",W=" & brect.Width & ",H=" & brect.Height)
                    g.FillRectangle(br, rect)
                    g.DrawRectangle(Pens.Red, rect)
                    '                   text &= word.Text & " "
                Next
            Next
            g.Dispose()
        Catch ex As Exception
            showStatus(CNM & "markWords :" & ex.Message)
            Return False
        End Try
        Return True
    End Function
    '*************************************************************
    Public Function makeLines(Plen As Integer) As String
        Dim sb = New StringBuilder(Plen * 2)
        Try
            For Each line As OcrLine In result.Lines
'               sb.Append(line.Text)                                   ''~v104R~
                For Each word As OcrWord In line.Words                 ''~v104I~
  	            	sb.Append(word.Text)                               ''~v104I~
                Next                                                   ''~v104I~
                sb.Append(vbCrLf)
            Next
        Catch ex As Exception
            showStatus(CNM & "makeLines :" & ex.Message)
            Return "Failed to Extract Lines"
        End Try
        Return sb.ToString()
    End Function
    '*************************************************************
    Private Sub showStatus(Pmsg As String)                         ''~v100R~
    	if statusMsg is Nothing                                        ''~v100I~
	        statusMsg=Pmsg                                             ''~v100R~
        end if                                                         ''~v100I~
    End Sub
    '*************************************************************     ''~v110I~''~va04I~
    Public Sub saveCutImage(Pbmp As Bitmap, Prect As Rectangle, Pfnm As String, Pfmt As ImageFormat)''~va04I~
        Dim cutbmp As Bitmap = cutImage(Pbmp, Prect)                   ''~va04I~
        saveImage(cutbmp, Pfnm, Pfmt)                                  ''~va04I~
    End Sub                                                            ''~va04I~
    '*************************************************************     ''~va04I~
    Public Function saveImage(Pbmp As Bitmap, Pfnm As String, Pfmt As ImageFormat) As String ''~va04R~
        '* from form2                                                      ''~va04I~
        Dim fnm As String = ""                                           ''~va04I~
        Try                                                            ''~va04I~
            Dim ext As String = getImageFormat(Pfmt)                   ''~va04R~
            fnm = Pfnm & "." & ext                                       ''~va04R~
            Pbmp.Save(fnm, Pfmt)                                       ''~va04I~
        Catch ex As Exception                                          ''~va04I~
            Form1.exceptionMsg("Form2 SaveImage", ex)                  ''~va04I~
            Return ""                                                  ''~va04I~
        End Try                                                        ''~va04I~
        Return fnm                                                     ''~va04I~
    End Function                                                        ''~va04I~
    '*************************************************************     ''~va04I~
    Public Function cutImage(Pbmp As Bitmap, Prect As Rectangle) As Bitmap''~va04I~
        Dim bmp As Bitmap = Pbmp.Clone(Prect, Pbmp.PixelFormat)        ''~va04I~
        Return bmp                                                     ''~va04I~
    End Function                                                       ''~va04I~
    '*************************************************************     ''~va04I~
    Public Function getImageFormat(Pfmt As ImageFormat) As String      ''~va04I~
        If Pfmt.equals(ImageFormat.Jpeg) Then                          ''~va05I~
            Return "jpg"                                               ''~va05I~
        End If                                                         ''~va05I~
        If Pfmt.equals(ImageFormat.Icon) Then                          ''~va05I~
            Return "ico"                                               ''~va05I~
        End If                                                         ''~va05I~
        If Pfmt.equals(ImageFormat.Tiff) Then                          ''~va05I~
            Return "tif"                                               ''~va05I~
        End If                                                         ''~va05I~
        If Pfmt.equals(ImageFormat.Png) Then                           ''~va05I~
            Return "png"       '*lowercase                             ''~va05I~
        End If                                                         ''~va05I~
        If Pfmt.equals(ImageFormat.Bmp) Then                           ''~va05I~
            Return "bmp"       '*lowercase                             ''~va05I~
        End If                                                         ''~va05I~
        If Pfmt.equals(ImageFormat.Gif) Then                           ''~va05I~
            Return "gif"       '*lowercase                             ''~va05I~
        End If                                                         ''~va05I~
        Dim fmt As String = Pfmt.ToString()                            ''~va04I~
        Return fmt                                                     ''~va04I~
    End Function                                                       ''~va04I~
    '*************************************************************     ''~va04I~
    Public Function str2Fmt(Pstrfmt As String) As ImageFormat          ''~va04I~
        If String.Compare(Pstrfmt, "bmp", True) = 0 Then   'true:ignotre case''~va04I~
            Return ImageFormat.Bmp                                     ''~va04I~
        End If                                                         ''~va04I~
        If String.Compare(Pstrfmt, "gif", True) = 0 Then   'true:ignotre case''~va04I~
            Return ImageFormat.Gif                                     ''~va04I~
        End If                                                         ''~va04I~
        If String.Compare(Pstrfmt, "jpg", True) = 0 Then   'true:ignotre case''~va04I~
            Return ImageFormat.Jpeg                                    ''~va04I~
        End If                                                         ''~va04I~
        If String.Compare(Pstrfmt, "jpeg", True) = 0 Then   'true:ignotre case''~va04I~
            Return ImageFormat.Jpeg                                    ''~va04I~
        End If                                                         ''~va04I~
        If String.Compare(Pstrfmt, "png", True) = 0 Then   'true:ignotre case''~va04I~
            Return ImageFormat.Png                                     ''~va04I~
        End If                                                         ''~va04I~
        If String.Compare(Pstrfmt, "tiff", True) = 0 Then   'true:ignotre case''~va04I~
            Return ImageFormat.Tiff                                    ''~va04I~
        End If                                                         ''~va04I~
        If String.Compare(Pstrfmt, "tif", True) = 0 Then   'true:ignotre case''~va04I~
            Return ImageFormat.Tiff                                    ''~va04I~
        End If                                                         ''~va04I~
        If String.Compare(Pstrfmt, "icon", True) = 0 Then   'true:ignotre case''~va04I~
            Return ImageFormat.Icon                                    ''~va04I~
        End If                                                         ''~va04I~
        If String.Compare(Pstrfmt, "ico", True) = 0 Then   'true:ignotre case''~va04I~
            Return ImageFormat.Icon                                    ''~va04I~
        End If                                                         ''~va04I~
        Return ImageFormat.Bmp                                         ''~va04I~
    End Function                                                       ''~va04I~
End Class
'*************************************************************
' Windows manual
'Public Delegate Sub AsyncOperationCompletehandler(IAsyncOperation, AsyncStatus)
'*************************************************************
Public Module TaskExtensionModule
    '***add to the class/Interface(1st parm of Function/Sub) extended Function/Sub with <Extension> prefix
    '*************************************************************
    <Extension()>
    Public Function AsTask(Of TResult)(Poper As IAsyncOperation(Of TResult)) As Task(Of TResult)
        Dim mTCS = New TaskCompletionSource(Of TResult)()
        '       Dim notifier = New DelegateNotifier(Of T)(AddressOf TaskNotifier(Of T))
        '       Poper.Completed = New AsyncOperationCompletedHandler(Of T)(Sub(Poper2, Pstatus2)
        '                                                                      TaskNotifier(Of T)(Poper2, Pstatus2)
        '                                                                  End Sub)
        Try
            Poper.Completed = New AsyncOperationCompletedHandler(Of TResult)(Sub(Poper2, Pstatus2)
                                                                                 TaskNotifier(Poper2, Pstatus2, mTCS)
                                                                             End Sub)  'void AsyncOperationCompletionHandler(IAsyncOperation,AsyncStatus)
        Catch ex As Exception
            Form1.showStatus("AsTask :" & ex.Message)
        End Try
        Return mTCS.Task
    End Function
    '*************************************************************
    <Extension()>
    Public Function GetAwaiter(Of TResult)(Poper As IAsyncOperation(Of TResult)) As TaskAwaiter(Of TResult)
        '       Return Poper.AsTask().GetAwaiter()
        Dim tsk As Task(Of TResult) = Poper.AsTask()
        Dim w As TaskAwaiter(Of TResult) = tsk.GetAwaiter()
        Return w
    End Function
    '   <Extension()>
    Public Sub TaskNotifier(Of TResult)(Poper As IAsyncOperation(Of TResult), Pstatus As AsyncStatus, PmTCS As TaskCompletionSource(Of TResult))
        Select Case Pstatus
            Case AsyncStatus.Completed
                PmTCS.SetResult(Poper.GetResults())
                Poper.Close() 'IAsyncOperation Interface inherit IAsyncInfo, IAsyncInfo has Close() it is requires adter GetResult()
            Case AsyncStatus.Error
                PmTCS.SetException(Poper.ErrorCode) 'ErrorCode is in IAsyncInfo
            Case AsyncStatus.Canceled
                PmTCS.SetCanceled()
        End Select
    End Sub
End Module
