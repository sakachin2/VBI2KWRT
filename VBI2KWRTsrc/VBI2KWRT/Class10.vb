'CID:''+v155R~:#72                             update#=  324;         ''~v154R~''~v155R~
'************************************************************************************''~v026I~''~v100I~
'v155 2018/01/10 markword should use not sorted lines but result.OcrLines''~v155I~
'v154 2018/01/10 have to chk also horizontal                           ''~v154R~
'v153 2018/01/08 point size change consider small char, drop from average''~v153I~
'v148 2018/01/07 mark line requirs ajustment by textangle?             ''~v148I~
'v147 2018/01/06 OcrWord.Boundingrect is readonly                      ''~v147I~
'v146 2018/01/06 point change chk by not max H/W but by average        ''~v146I~
'v145 2018/01/06 determin horizontal or vertical for 1 char line by distance to prev/next char''~v145I~
'v144 2018/01/06 chk short line but sameline,do not inser crlf but insert space''~v144I~
'v143 2018/01/05 (Bug) word may contains multiple char for english     ''~v143I~
'v141 2018/01/04 add selection of japanese writing direction for sortline''~v141I~
'v140 2018/01/03 sort option by x or y of line                         ''~v140R~
'v139 2018/01/03 markWord position invalid when cliprect               ''~v139I~
'v138 2018/01/02 keep EOL for short line(add space at last if last is not delm)''~v138I~
'va05 2017/12/26 ext name Jpeg-->jpg,icon-->ico,tiff->tif              ''~va05I~
'va04 2017/12/25 save cut image to file                                ''~va04I~
'v110 2017/12/22 StringConstant reset required when lang changed       ''~v110I~
'v106 2017/12/20 partially extract from image(box by mouse dragging)   ''~v106I~
'v104 2017/12/20 drop space between chars of extracted text            ''~v104I~
'v100 2017/12/15 porting from MOD to Microsoft Ocr Library for Windows ''~v100I~
'************************************************************************************''~v100I~
Imports System.Drawing.Imaging
Imports System.Drawing                                                 ''~v138I~
Imports System.IO
Imports System.Runtime.CompilerServices
Imports System.Threading.Tasks
Imports System.Text
Imports System.Math                                                    ''~v138I~
Imports Windows.Foundation
Imports Windows.Globalization
Imports Windows.Graphics.Imaging
Imports Windows.Media.Ocr
Imports Windows.Storage
Imports Windows.Storage.Pickers
Imports Windows.Storage.Streams
'************************************************************************************''~v138I~
Public Class Cocr

    Private Const CNM = "Cocr:"
    Private Const LS_HORIZONTAL = &H1                                  ''~v138I~
    Private Const LS_EOL_DELM = &H2                                  ''~v138I~
    Private Const LS_ADD_CRLF = &H4                                  ''~v138I~
    Private Const LS_ADD_SPACE = &H8                                 ''~v138R~
    Private Const LS_CHANGE_DIR = &H100                              ''~v138I~
    Private Const LS_CHANGE_POINT = &H200                              ''~v138I~
    Private Const LS_CHANGE_LINESPACE = &H400                          ''~v140I~
    Private Const LS_REP_CRLF = &H800                          ''~v144I~
    Private Const RATE_POINT_CHANGED = 0.1 '*add crlf if char point changed 20%''~v138R~
    Private Const RATE_LENGTH_CHANGED = 0.8 '*add crlf if line length shorter than max of group''~v138I~
    Private Const RATE_LINESPACING = 2.5 '*add crlf lines spacing of both side is over 1.0 of char width/height''~v140R~''~v147R~
    Private Const SORT_SCALE = 10000.0    '* sortkey for X and Y     ''~v140I~
    Private Const RATE_SMALL_CHAR = 0.5     'drop from average char width/height if charsize is small''~v153I~
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
    Private lineStyle As Integer()                                     ''~v138I~
    Private jpWriting() As String = {"ja", "日本語", "ja", "右縦書き", "ja", "左縦書き", "ja", "横書き右", "ja", "横書き左"} ''~v141I~
    Private jpWritingE() As String = {"ja", "Japanese", "ja", "Jp-VR2L", "ja", "Jp-VL2R", "ja", "Jp-HR2L", "ja", "Jp-HL2R"} ''~v141I~
    Private Const SORT_VLINE_RTL = 1       '*sort vertical line by X:largeX-->smallX''~v140I~
    Private Const SORT_VLINE_LTR = 2       '*sort horizontal line by Y:smallY-->largeY''~v140I~''~v141R~
    Private Const SORT_HLINE_RTL = 3       '*sort horizontal line by Y:smallY-->largeY''~v141R~
    Private Const SORT_HLINE_LTR = 4       '*sort horizontal line by Y:smallY-->largeY''~v141I~
    Private sortOption As Integer = SORT_VLINE_RTL                     ''~v140R~''~v138R~''~v140R~
    '*  Private sortOption As Integer = 0                                  ''~v138I~''~v140R~
    Private listLines As List(Of OcrLine)                            ''~v140R~
    Private swPointChangeByMax As Boolean = True                         ''~v153I~
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
        Dim jpw() As String                                            ''~v141I~
        If FormOptions.swLangEN Then                                   ''~v141I~
            jpw = jpWritingE                                           ''~v141R~
        Else                                                           ''~v141I~
            jpw = jpWriting                                            ''~v141R~
        End If                                                         ''~v141I~
        For Each item As Language In langlist
            Dim row As DataRow = tb.NewRow()
            If item.LanguageTag.StartsWith("ja") Then                         ''~v141I~
                For ii As Integer = 0 To jpw.Count - 1 Step 2          ''~v141R~
                    If ii > 0 Then                                            ''~v141I~
                        row = tb.NewRow()                              ''~v141I~
                    End If                                             ''~v141I~
                    row(LANG_TAG) = jpw(ii)                            ''~v141R~
                    row(LANG_NAME) = jpw(ii + 1)                       ''~v141R~
                    tb.Rows.Add(row)                                       ''~v141I~
                Next                                                       ''~v141I~
            Else                                                         ''~v141I~
                row(LANG_TAG) = item.LanguageTag
                If FormOptions.swLangEN Then                                     ''~v110I~
                    row(LANG_NAME) = item.NativeName                       ''~v110I~
                Else                                                       ''~v110I~
                    row(LANG_NAME) = item.DisplayName
                End If                                                     ''~v110I~
                tb.Rows.Add(row)
            End If                                                       ''~v141I~
        Next
        tb.AcceptChanges()
        tbLang = tb
    End Sub
    '**************************************************
    Public Function getSelectedLangTag(Pcb As ToolStripComboBox, ByRef Ppidxlang As Integer) As String ''~v100R~
        Dim cb As ToolStripComboBox = Pcb
        Dim idx As Integer = cb.SelectedIndex
        Dim tag As String = CType(tbLang.Rows(idx)(LANG_TAG), String)   ''~v100R~
        Dim name As String = CType(tbLang.Rows(idx)(LANG_NAME), String) ''~v141I~
        setWritingOption(name)                                         ''~v141I~
        Ppidxlang = idx                                                  ''~v100I~
        Return tag
    End Function
    '**************************************************                ''~v141I~
    Public Sub setWritingOption(Pname As String)                      ''~v141I~
        Dim sortopt As Integer = 0                                       ''~v141I~
        Dim jpw() As String                                            ''~v141I~
        If FormOptions.swLangEN Then                                   ''~v141I~
            jpw = jpWritingE                                           ''~v141R~
        Else                                                           ''~v141I~
            jpw = jpWriting                                            ''~v141R~
        End If                                                         ''~v141I~
        For ii As Integer = 0 To jpw.Count - 1 Step 2                  ''~v141R~
            If Pname.CompareTo(jpw(ii + 1)) = 0 Then                   ''~v141R~
                sortopt = CType(ii / 2, Integer)                       ''~v141R~
            End If                                                     ''~v141I~
        Next                                                           ''~v141I~
        sortOption = sortopt                                             ''~v141I~
    End Sub                                                            ''~v141I~
    '**************************************************                ''~v106I~
    '* set clip box info before extact                                 ''~v106I~
    Public Sub setRect(PswRectBMP As Boolean, PbmpRect As Bitmap, PscaleNew As Double, PclipRect As Rectangle) ''~v106I~
        swRectBMP = PswRectBMP                                         ''~v106I~
        bmpRect = PbmpRect                                             ''~v106I~
        scaleNew = PscaleNew                                           ''~v106I~
        clipRect = PclipRect                                           ''~v106I~
'*      Trace.W("iOCR:setrect cliprect X=" & clipRect.X & ",Y=" & clipRect.Y & ",W=" & clipRect.Width & ",H=" & clipRect.Height)''+v155R~
    End Sub                                                            ''~v106I~
    '**************************************************                ''~v106I~
    Public Function cutBMPRect(PorgBMP As Bitmap) As Bitmap            ''~v106I~
        Dim xx, yy, ww, hh As Integer                                  ''~v106I~
        xx = CType(clipRect.X / scaleNew, Integer) 'dest and src position''~v106I~
        yy = CType(clipRect.Y / scaleNew, Integer)                     ''~v106I~
        ww = CType(clipRect.Width / scaleNew, Integer)                 ''~v106I~
        hh = CType(clipRect.Height / scaleNew, Integer)                ''~v106I~
'*      Trace.W("cutBMPRect xx=" & xx & ",yy=" & yy & ",ww=" & ww & ",hh=" & hh)''+v155R~
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
        '*      Trace.W("cutBMPRect org W=" & PorgBMP.Width & ",H=" & PorgBMP.Height) ''~v106I~''~va05R~
        '*      Trace.W("cutBMPRect clipRect X=" & clipRect.X & ",Y=" & clipRect.Y & ",W=" & clipRect.Width & ",H=" & clipRect.Height) ''~v106I~''~va05R~
        '*      Trace.W("cutBMPRect scale=" & scaleNew)                        ''~v106I~''~va05R~
        '*      Trace.W("cutBMPRect xx=" & xx & ",yy=" & yy & ",ww=" & ww & ",hh=" & hh) ''~v106I~''~va05R~
        '*      Trace.W("cutBMPRect clip W=" & bmp.Width & ",H=" & bmp.Height) ''~v106I~''~va05R~
        '       bmp.Save("W:\cutbmprect.bmp", ImageFormat.BMP) '@@@@test       ''~v106I~
        Return bmp                                                     ''~v106I~
    End Function                                                       ''~v106I~
    '**************************************************                ''~va04I~
    Public Function saveImage(Pbasename As String, Pextname As String, PswRectBMP As Boolean, PorgBMP As Bitmap, Pscale As Double, Prect As Rectangle) As String ''~va04R~
        '* from Form2                                                      ''~va04I~
        Dim bmp As Bitmap                                              ''~va04I~
'*      Trace.W("iOCR:saveImage fnm=" & Pbasename & ",swRectBMP=" & PswRectBMP & ",scale=" & Pscale)''+v155R~
'*      Trace.W("iOCR:saveImage cliprect X=" & Prect.X & ",Y=" & Prect.Y & ",W=" & Prect.Width & ",H=" & Prect.Height)''+v155R~
        If PswRectBMP Then 'clipped                                         ''~va04I~
            Dim xx, yy, ww, hh As Integer                              ''~va04I~
            xx = CType(Prect.X / Pscale, Integer) 'dest and src position''~va04I~
            yy = CType(Prect.Y / Pscale, Integer)                      ''~va04I~
            ww = CType(Prect.Width / Pscale, Integer)                  ''~va04I~
            hh = CType(Prect.Height / Pscale, Integer)                 ''~va04I~
            Dim tgtRect As Rectangle = New Rectangle(xx, yy, ww, hh)   ''~va04I~
'*          Trace.W("saveImage X=" & xx & ",Y=" & yy & ",ww=" & ww & ",hh=" & hh)''+v155R~
'*          Trace.W("saveImage bmp W=" & PorgBMP.Width & ",H=" & PorgBMP.Height)''+v155R~
            bmp = cutImage(PorgBMP, tgtRect)                           ''~va04I~
        Else                                                           ''~va04I~
            bmp = PorgBMP                                                ''~va04I~
        End If                                                         ''~va04I~
        Dim fmt As ImageFormat = str2Fmt(Pextname)                     ''~va04I~
        Dim fnm As String = saveImage(bmp, Pbasename, fmt)               ''~va04R~
        If PswRectBMP Then                                                  ''~va04I~
            bmp.Dispose()                                              ''~va04I~
        End If                                                         ''~va04I~
        Return fnm                                                     ''~va04I~
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
        statusMsg = Nothing                                              ''~v100R~
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
'*          Trace.W("all text=" & xText)                               ''~v140I~''+v155R~
            listLines = toList(result.Lines)                           ''~v140R~
            xText = makeLines(xText.Length) 'insert crlf between lines
            swOK = True
        Catch ex As Exception
            '*          showStatus(CNM & "extractText exception:" & ex.Message)    ''~v140R~
            Form1.exceptionMsg("OCR:extractTextAsync", ex)             ''~v140I~
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
            '*          showStatus(CNM & "LoadImage file exception:" & Pfnm & ":" & ex.Message)''~v140R~
            Form1.exceptionMsg("OCR:LoadImage file", ex)               ''~v140I~
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
            '*          showStatus(CNM & "getImageBuff exception:" & ex.Message)   ''~v140R~
            Form1.exceptionMsg("OCR:getImageBuff", ex)                 ''~v140I~
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
            '           showStatus(CNM & "ConvertToRandomAccess:" & ex.Message)    ''~v140R~
            Form1.exceptionMsg("OCR:ConvertToRandomAccess", ex)        ''~v140I~
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
            '*          showStatus(CNM & "LoadImage stream :" & ex.Message)        ''~v140R~
            Form1.exceptionMsg("OCR:LoadImage Strem", ex)              ''~v140I~
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
            '*          showStatus(CNM & "Extract failed:" & imageFilename & ":" & ex.Message)''~v140R~
            Form1.exceptionMsg("OCR:callOCR", ex)                      ''~v140I~
        End Try
        Return result
    End Function
    '*************************************************************
    Public Function markWords(Pbmp As Bitmap) As Boolean
        '** avoid exceotion:Indexed Pixel at Graphics.FromImage for mono color image
        Dim xx0 = 0, yy0 = 0                                               ''~v139I~
        Trace.setOn()                                                  ''~va05R~
        Try
            '********************
            If swRectBMP Then                                          ''~v139I~
                xx0 = CType(clipRect.X / scaleNew, Integer) 'dest and src position''~v139I~
                yy0 = CType(clipRect.Y / scaleNew, Integer)            ''~v139I~
            End If                                                     ''~v139I~
            Dim bmpDraw As Bitmap = Pbmp
            Dim g = Graphics.FromImage(bmpDraw)
            Dim br As Brush = New SolidBrush(System.Drawing.Color.FromArgb(&H20, System.Drawing.Color.Blue))
            '           Dim text As String = ""
'*          Trace.W("Class10:MarkWORDs: TextAngle=" & result.TextAngle & ",linectr=" & listLines.Count & ",bmpH=" & Pbmp.Height & ",bmpW=" & Pbmp.Width) ''~v138R~''~v140R~''~v148R~''+v155R~
            '*          For Each line As OcrLine In result.Lines                   ''~v140R~
            Dim angle As Double = CType(result.TextAngle, Double)                      ''~v148I~
            Dim msin As Double                                         ''~v148I~
            Dim mcos As Double                                         ''~v148I~
            If angle <> 0 Then                                                ''~v148I~
                adjustMarkingBoxInit(angle, msin, mcos) '*get sin cosin''~v148R~
            End If                                                     ''~v148I~
'*          For Each line As OcrLine In listLines                      ''~v140I~''~v155R~
            For Each line As OcrLine In result.Lines                   ''~v155I~
                '               text += line.Text & " "
'*              Trace.W("Class10:MarkWords:  word count=" & line.Words.Count & ",Line Text=" & line.Text)             ''~va05R~''~v138R~''+v155R~
                For Each word As OcrWord In line.Words
                    Dim brect As Windows.Foundation.Rect = word.BoundingRect
                    Dim rect As Rectangle = New System.Drawing.Rectangle(CType(brect.X, Integer), CType(brect.Y, Integer), CType(brect.Width, Integer), CType(brect.Height, Integer)) ''~v100R~
                    If angle <> 0 Then                                        ''~v148I~
'*                      Trace.W("Ajust angle angle=" & angle & ",text=" & word.Text) ''~v148I~''+v155R~
                        adjustMarkingBox(bmpDraw, rect, msin, mcos, swRectBMP, clipRect) '*get sin cosin''~v148R~
                    End If                                             ''~v148I~
                    rect.X += xx0                                       ''~v139I~
                    rect.Y += yy0                                       ''~v139I~
'*                  Trace.W("Class10:markWords: X=" & brect.X & ",Y=" & brect.Y & ",W=" & brect.Width & ",H=" & brect.Height & ",text=" & word.Text) ''~va05R~''+v155R~
'*                  Trace.W("Class10:markWords rect: X=" & rect.X & ",Y=" & rect.Y & ",W=" & rect.Width & ",H=" & rect.Height & ",text=" & word.Text) ''~v147I~''+v155R~
                    g.FillRectangle(br, rect)
                    g.DrawRectangle(Pens.Red, rect)
                    '                   text &= word.Text & " "
                Next
            Next
            g.Dispose()
        Catch ex As Exception
            '           showStatus(CNM & "markWords :" & ex.Message)               ''~v140R~
            Form1.exceptionMsg("OCR:markWords", ex)                    ''~v140I~
            Return False
        End Try
        '*        Trace.setOff()                                                 ''~va05R~
        Return True
    End Function
    '*************************************************************     ''~v148I~
    Private Sub adjustMarkingBoxInit(Pangle As Double, ByRef Ppsin As Double, ByRef Ppcos As Double) ''~v148R~
        Dim rad As Single = CSng((Pangle / 180) * Math.PI)          ''~v148I~
        Dim msin As Double = Math.Sin(rad)                             ''~v148R~
        Dim mcos As Double = Math.Cos(rad)                             ''~v148R~
        Ppsin = msin                                                     ''~v148I~
        Ppcos = mcos                                                     ''~v148I~
'*      Trace.W("ajustangleInit  msin=" & msin & ",mcos=" & mcos)      ''~v148I~''+v155R~
    End Sub                                                            ''~v148I~
    '*************************************************************     ''~v148I~
    '*x2=x1cos-y1sin, y2=x1sin+y1cos;center is (x/2,y/2)=x0,y0         ''~v148I~
    '*==>x2=x0+(x1-x0)cos-(y1-y0)sin, y2=y0+(x1-x0)sin+(y1-y0)cos      ''~v148I~
    '*************************************************************     ''~v148I~
    Private Sub adjustMarkingBox(Pbmp As Bitmap, ByRef Pprect As Rectangle, Psin As Double, Pcos As Double, PswrectBMP As Boolean, PclipRect As Rectangle) ''~v148R~
        Dim x0 As Double                                               ''~v148R~
        Dim y0 As Double                                               ''~v148R~
        If PswrectBMP Then                                                  ''~v148I~
            x0 = PclipRect.Width / 2                                     ''~v148I~
            y0 = PclipRect.Height / 2                                    ''~v148I~
        Else                                                           ''~v148I~
            x0 = Pbmp.Width / 2                                        ''~v148M~
            y0 = Pbmp.Height / 2                                       ''~v148M~
        End If                                                         ''~v148I~
        Dim x1 As Double = Pprect.X                                    ''~v148R~
        Dim y1 As Double = Pprect.Y                                    ''~v148I~
        Dim dx As Double = x1 - x0                                       ''~v148I~
        Dim dy As Double = y1 - y0                                       ''~v148I~
        Dim x2 As Double = x0 + dx * Pcos - dy * Psin                          ''~v148R~
        Dim y2 As Double = y0 + dx * Psin + dy * Pcos                          ''~v148I~
        Pprect.X = CType(x2, Integer)                                  ''~v148R~
        Pprect.Y = CType(y2, Integer)                                  ''~v148R~
'*      Trace.W("ajustangle  xx=" & x1 & ",yy=" & y1 & ",xxnew=" & x2 & ",yynew=" & y2) ''~v148R~''+v155R~
    End Sub                                                            ''~v148I~
    '*************************************************************     ''~v145I~
    '*chk horizontal for the case 1word/line,1char/word                ''~v145I~
    '*************************************************************     ''~v145I~
    Public Function getDestOfOneCharLine(Pline As OcrLine, Pword As OcrWord, Plineno As Integer) As Integer ''~v145R~
        Dim type As Integer = 0                                          ''~v145I~
        Dim pl As OcrLine = Nothing                                      ''~v145I~
        Dim prect As Rect                                              ''~v145I~
        Dim crect As Rect                                              ''~v145I~
        Dim dx As Double                                               ''~v145R~
        Dim dy As Double                                               ''~v145R~
        crect = Pword.BoundingRect                                       ''~v145I~
        Do                                                             ''~v145I~
            If Plineno > 0 Then                                        ''~v145R~
                pl = listLines(Plineno - 1)                            ''~v145R~
                prect = pl.Words.Item(0).BoundingRect                  ''~v145R~
                dy = Abs(crect.Y - prect.Y)                                ''~v145R~
                dx = Abs(crect.X - prect.X)                                ''~v145I~
'*              Trace.W("1charline dx=" & dx & ",dy=" & dy & ",text=" & Pline.Text) ''~v145I~''+v155R~
'*              Trace.W("1charline chH=" & crect.Height & ",chW=" & crect.Width & "prev text=" & pl.Text) ''~v145I~''+v155R~
                If dy < crect.Height Then                                     ''~v145I~
                    type = LS_HORIZONTAL                               ''~v145I~
                    Exit Do                                            ''~v145I~
                End If                                                 ''~v145I~
                If dx < crect.Width Then                                      ''~v145I~
                    Exit Do                                            ''~v145I~
                End If                                                 ''~v145I~
            End If                                                     ''~v145I~
            If sortOption = SORT_HLINE_RTL OrElse sortOption = SORT_HLINE_LTR Then ''~v145I~
                type = LS_HORIZONTAL                                   ''~v145I~
            End If                                                     ''~v145I~
            Exit Do                                                    ''~v145I~
        Loop                                                           ''~v145I~
'*      Trace.W("1charline type=" & type)                              ''~v145I~''+v155R~
        Return type                                                    ''~v145I~
    End Function                                                       ''~v145I~
    '*************************************************************     ''~v138I~
    Public Sub chkLineStyle()                                          ''~v138R~
        '       Dim ctrline As Integer = result.Lines.Count()                    ''~v138I~''~v140R~
        Dim ctrline As Integer = listLines.Count()                     ''~v140I~
        Dim ls As Integer() = New Integer(ctrline) {}                      ''~v138I~
        Dim linesz As Double() = New Double(ctrline) {}               ''~v138R~
        Dim charH As Double() = New Double(ctrline) {}                 ''~v138R~
        Dim charW As Double() = New Double(ctrline) {}                 ''~v138I~
        Dim lineSpacing As Double() = New Double(ctrline) {}           ''~v140I~
        Dim baseline As Double() = New Double(ctrline) {}            ''~v147I~
        Dim brect As Rect                                              ''~v138I~
        Dim brect1 As Rect                                        ''~v138I~
        Dim brect2 As Rect                                        ''~v138I~
        Dim lineno As Integer                                          ''~v138I~
        Dim type As Integer                                            ''~v138I~
        Dim ww As Double                                               ''~v138R~
        Dim hh As Double                                               ''~v138I~
        Dim sz As Double                                               ''~v138R~
        '*************************************                         ''~v138I~
        Dim minLineHeight As Double = 1000.0                            ''~v140I~
        Dim minLineWidth As Double = 1000.0                              ''~v140I~
        lineno = 0                                                       ''~v138I~
        '*      For Each line As OcrLine In result.Lines                       ''~v138I~''~v140R~
        For Each line As OcrLine In listLines                          ''~v140I~
            Dim ctrword As Integer = line.Words.Count                    ''~v138I~
            Dim word1 As OcrWord = line.Words.Item(0)                    ''~v138I~
            Dim word2 As OcrWord = line.Words.Item(ctrword - 1)            ''~v138I~
            brect1 = word1.BoundingRect                                 ''~v138I~
            brect2 = word2.BoundingRect                                 ''~v138I~
            Dim lenx As Double = Abs(brect2.X - brect1.X) + CType(IIf(brect2.X > brect1.X, brect2.Width, brect1.Width), Double) ''~v138R~
            Dim leny As Double = Abs(brect2.Y - brect1.Y) + brect2.Height ''~v138R~
            If line.Words.Count = 1 AndAlso word1.Text.Length = 1 Then                    ''~v145I~
                type = getDestOfOneCharLine(line, word1, lineno)       ''~v145R~
            Else                                                         ''~v145I~
                If lenx > leny Then                                               ''~v138I~
                    type = LS_HORIZONTAL     '*horizontal line               ''~v138I~
                    linesz(lineno) = lenx                                    ''~v138I~
                Else                                                       ''~v138I~
                    type = 0                                                 ''~v138I~
                    linesz(lineno) = leny                                    ''~v138I~
                End If                                                     ''~v138I~
            End If                                                       ''~v145I~
'*          Trace.W("Class10:chkLineStyle type=" & type & ",lenx=" & lenx & ",leny=" & leny) ''~v138I~''+v155R~
'*          Trace.W("Class10:chkLineStyle type=" & type & ",lineno=" & lineno & ",X=" & brect1.X & ",txt=" & line.Text) ''~v138I~''+v155R~
            Dim txt = word2.Text                                        ''~v138I~
            Dim txtlen As Integer = txt.Length                           ''~v138I~
            If txtlen > 0 Then                                                ''~v138I~
                Dim ch As Char = txt.Chars(txtlen - 1)                     ''~v138I~
                If ClassKanaText.isDelmCharEOL(ch) Then                     ''~v138I~
'*                  Trace.W("EOL_DELM text=" & line.Text)              ''~v140I~''+v155R~
                    type = type Or LS_EOL_DELM     '* 0x02 line end with delm''~v138I~
                End If                                                 ''~v138I~
            End If                                                     ''~v138I~
            '*get char size                                                        ''~v138I~
            Dim ptmaxw As Double = 0.0                                 ''~v138R~
            Dim ptmaxh As Double = 0.0                                 ''~v138I~
            Dim ptmaxwMax As Double = 0.0                              ''~v153I~
            Dim ptmaxhMax As Double = 0.0                              ''~v153I~
            Dim wordctr = 0                                              ''~v146I~
            Dim base As Double = 0.0                                     ''~v147I~
            For Each word As OcrWord In line.Words                     ''~v138I~
                brect = word.BoundingRect                                ''~v138I~
'*              Trace.W("Class10:chkLineStyle Words: X=" & brect.X & ",Y=" & brect.Y & ",W=" & brect.Width & ",H=" & brect.Height & ",btext=" & word.Text) ''~v138I~''+v155R~
                hh = brect.Height                                      ''~v138R~
                ww = brect.Width                                       ''~v138R~
                '*              brect.Y += hh             '*change to baseline for line spacing''~v140I~''~v146R~''~v147R~
'*              Trace.W("Class10:chkLineStyle Words: new baseline Y=" & brect.Y & ",text=" & word.Text) ''~v140I~''~v146R~''~v147R~''+v155R~
                If word.Text.Length > 1 Then                                  ''~v143I~
                    If (type And LS_HORIZONTAL) <> 0 Then     '*horizontal line''~v154I~
                        ww /= word.Text.Length                               ''~v143I~
                    Else                                                 ''~v154I~
                        hh /= word.Text.Length                             ''~v154I~
                    End If                                                 ''~v143I~
                End If                                                 ''~v154I~
                ptmaxhMax = Max(hh, ptmaxhMax)                               ''~v138R~''~v146R~''~v153R~
                ptmaxwMax = Max(ww, ptmaxwMax)                               ''~v138I~''~v146R~''~v153R~
                Dim wordlen = word.Text.Length                           ''~v147I~
                ptmaxh += hh * wordlen                                    ''~v146I~''~v147R~
                ptmaxw += ww * wordlen                                    ''~v146I~''~v147R~
                base = Max(base, brect.Y + hh)                           ''~v147R~
                wordctr += wordlen                                       ''~v146I~''~v147R~
            Next                                                       ''~v138I~
            ''~v153I~
            charH(lineno) = ptmaxh / wordctr                             ''~v146I~
            charW(lineno) = ptmaxw / wordctr                             ''~v146I~
            baseline(lineno) = base '*for line spacing chk             ''~v147R~
'*          Trace.W("average avaerage baseline=" & baseline(lineno) & ",sum=" & base & ",ctr=" & wordctr) ''~v147I~''~v153M~''+v155R~
            ''~v153I~
            Dim avh2 As Double = charH(lineno) * RATE_SMALL_CHAR          ''~v153I~
            Dim avw2 As Double = charW(lineno) * RATE_SMALL_CHAR          ''~v153I~
'*          Trace.W("all average ptmaxh=" & charH(lineno) & ",ptmaxw=" & charW(lineno) & ",smallrateH=" & avh2 & ",smallrateW=" & avw2 & ",text=" & line.Text) ''~v153I~''+v155R~
            ptmaxw = 0.0                                               ''~v153I~
            ptmaxh = 0.0                                               ''~v153I~
            Dim wordctrH As Integer = 0                                  ''~v153R~
            Dim wordctrW As Integer = 0                                  ''~v153I~
            For Each word As OcrWord In line.Words                     ''~v153I~
                brect = word.BoundingRect                              ''~v153I~
                hh = brect.Height                                      ''~v153I~
                ww = brect.Width                                       ''~v153I~
                If word.Text.Length > 1 Then                           ''~v153I~
                    ww /= word.Text.Length                             ''~v153I~
                End If                                                 ''~v153I~
                Dim wordlen = word.Text.Length                         ''~v153M~
                If hh > avh2 Then                                           ''~v153R~
                    wordctrH += 1                                        ''~v153I~
                    ptmaxh += hh * wordlen                             ''~v153I~
                End If                                                 ''~v153I~
                If ww > avw2 Then                                           ''~v153I~
                    wordctrW += 1                                        ''~v153I~
                    ptmaxw += ww * wordlen                             ''~v153I~
                End If                                                 ''~v153I~
'*              Trace.W("Class10:chkLineStyle largeChar Words: X=" & brect.X & ",Y=" & brect.Y & ",W=" & brect.Width & ",H=" & brect.Height & ",btext=" & word.Text) ''~v153I~''+v155R~
            Next                                                       ''~v153I~
            If wordctrH > 0 Then                                              ''~v153R~
                charH(lineno) = ptmaxh / wordctrH                      ''~v153R~
            End If                                                     ''~v153I~
            If wordctrW > 0 Then                                              ''~v153I~
                charW(lineno) = ptmaxw / wordctrW                      ''~v153R~
            End If                                                     ''~v153I~
'*          Trace.W("average large char ctrH=" & wordctrH & ",ctrW=" & wordctrW & ",average W=" & charH(lineno) & ",H=" & charW(lineno) & ",text=" & line.Text) ''~v153I~''+v155R~
            If (type And LS_HORIZONTAL) <> 0 Then     '*horizontal line       ''~v140I~
                minLineHeight = Min(minLineHeight, charH(lineno))      ''~v140R~
            Else                                                       ''~v140I~
                minLineWidth = Min(minLineWidth, charW(lineno))        ''~v140R~
            End If                                                     ''~v140I~
'*          Trace.W("minLineHeigh=" & minLineHeight & ",width=" & minLineWidth) ''~v154I~''+v155R~
            If swPointChangeByMax Then                                      ''~v153I~''~v154R~
                charW(lineno) = ptmaxwMax                              ''~v153I~''~v154R~
                charH(lineno) = ptmaxhMax                              ''~v153I~''~v154R~
            End If                                                     ''~v153I~''~v154R~
            ls(lineno) = type     '*vertical line                        ''~v138I~
            lineno += 1                                                  ''~v138I~
        Next                                                           ''~v138I~
        '***************************************************           ''~v140I~
        '*      sortLines(listLines, ls, linesz, charH, charW, minLineHeight, minLineWidth) ''~v140R~''~v147R~
        sortLines(listLines, ls, linesz, charH, charW, minLineHeight, minLineWidth, baseline) ''~v147I~
        '***************************************************           ''~v140I~
        '*chk char size changed                                                ''~v138I~
        Dim hho As Double = 0.0                                        ''~v138R~
        Dim wwo As Double = 0.0                                        ''~v138I~
        Dim dirHo = False                                              ''~v138R~
        Dim poso As Double = 0.0                                         ''~v140I~
        For ii As Integer = 0 To ctrline - 1                               ''~v138I~
'*          Trace.W("chkLineStyle Line text=" & listLines.Item(ii).Text) ''~v138I~''~v140R~''+v155R~
            type = ls(ii)                                                ''~v138I~
            hh = charH(ii)                                              ''~v138R~
            ww = charW(ii)                                              ''~v138I~
            Dim dirH As Boolean = (type And LS_HORIZONTAL) <> 0       '*horizontal''~v138R~''~v140R~
            Dim word As OcrWord = listLines.Item(ii).Words.Item(0)            ''~v140I~
            Dim pos As Double                                          ''~v140I~
            If dirH Then '*horizontal                                       ''~v140I~
                '*              pos = word.BoundingRect.Y                       ''~v140I~''~v147R~
                pos = baseline(ii) '*baseline                          ''~v147I~
            Else                                                       ''~v140I~
                pos = word.BoundingRect.X                       ''~v140I~
            End If                                                     ''~v140I~
            lineSpacing(ii) = Abs(pos - poso)                           ''~v140R~
'*          Trace.W("linespacing from prev=" & lineSpacing(ii) & ",pos=" & pos & ",poso=" & poso) ''~v140I~''~v146R~''+v155R~
            poso = pos                                                   ''~v140I~
            If ii > 0 Then                                                    ''~v138I~
                If dirHo <> dirH Then           '*direction change            ''~v138I~
                    ls(ii - 1) = ls(ii - 1) Or LS_ADD_CRLF             '*add crlf at eol''~v138R~
                    ls(ii) = ls(ii) Or (LS_CHANGE_DIR)                 '*top of dir change''~v138I~
'*                  Trace.W("chkLineStyle Direction changed dirH=" & dirH & ",dirHo=" & dirHo & ",txt=" + listLines.Item(ii).Text) ''~v138R~''~v140R~''+v155R~
                Else                                                   ''~v138I~
                    Dim diffh As Double = Abs(hh - hho)                ''~v138R~
                    Dim diffw As Double = Abs(ww - wwo)                ''~v138I~
                    Dim rateh As Double = diffh / hho                 ''~v138R~
                    Dim ratew As Double = diffw / wwo                 ''~v138I~
'*                  Trace.W("chkLineStyle Direction point ww=" & ww & ",wwo=" & wwo & ",diffw=" & diffw & ",ratew=" & ratew) ''~v138R~''+v155R~
'*                  Trace.W("chkLineStyle Direction point hh=" & hh & ",hho=" & hho & ",diffh=" & diffh & ",rateh=" & rateh) ''~v138I~''+v155R~
                    If Min(ratew, rateh) > RATE_POINT_CHANGED Then      ''~v138R~
'*                      Trace.W("chkLineStyle Direction pointchange")  ''~v138R~''+v155R~
                        ls(ii - 1) = ls(ii - 1) Or LS_ADD_CRLF  '* add crlf to prev line''~v138R~
                        ls(ii) = ls(ii) Or LS_CHANGE_POINT            ''~v138I~
'*                      Trace.W("chkLineStyle Direction pointchange  without EOL_DELM add CRLF to Prev line") ''~v138R~''~v145R~''+v155R~
                    End If                                             ''~v138I~
                End If                                                 ''~v138I~
            End If                                                     ''~v138I~
            dirHo = dirH                                                 ''~v138I~
            hho = hh
            wwo = ww                                                     ''~v138I~
        Next                                                           ''~v138I~
        '*chk short line/line spacing                                                       ''~v138I~''~v140R~
        Dim grouptop As Integer = 0                                    ''~v138R~
        Dim groupmax As Double = 0.0                                    ''~v138R~
        Dim groupctr As Integer = 0                                      ''~v138I~
        For ii As Integer = 0 To ctrline - 1                               ''~v138I~
            '*          chkSameLine(ii, ls, minLineHeight, minLineWidth)            ''~v144I~''~v147R~
            chkSameLine(ii, ls, minLineHeight, minLineWidth, baseline)  ''~v147I~
            chkLineSpacing(ii, ls, lineSpacing, charH, charW)              ''~v140I~
            type = ls(ii)                                                ''~v138I~
'*          Trace.W("chkLineStyle lineno=" & ii & ",type=" & type & ",txt=" & listLines.Item(ii).Text) ''~v138R~''~v140R~''+v155R~
'*          Trace.W("chkLineStyle and=" & (type And (LS_CHANGE_DIR + LS_CHANGE_POINT))) ''~v138M~''+v155R~
            sz = linesz(ii)                                             ''~v138I~
            If (type And (LS_CHANGE_DIR + LS_CHANGE_POINT)) <> 0 Then    '*direction/charsize change''~v138R~
'*              Trace.W("chkLineStyle grouptop=" & grouptop & ",ctr=" & groupctr) ''~v138R~''+v155R~
                If groupctr > 1 Then      '* >= 2 line group            ''~v138I~''~v140R~''~v138R~
                    chkShortLine(listLines, linesz, groupmax, grouptop, groupctr, ls) ''~v138I~
                End If                                             ''~v138I~
                grouptop = ii     '* this line's point changed with prev,but same width may follow''~v138I~
                groupmax = sz                                        ''~v138I~
                groupctr = 1                                             ''~v138R~
            Else                                                   ''~v138I~
                If groupctr = 0 Then                                   ''~v138R~
                    grouptop = ii                                        ''~v138I~
                    groupmax = sz                                      ''~v138I~
                Else                                                   ''~v138I~
                    groupmax = Max(groupmax, sz)                       ''~v138I~
                End If                                                 ''~v138I~
                groupctr += 1                                            ''~v138I~
            End If                                                 ''~v138I~
        Next                                                           ''~v138I~
        If groupctr > 1 Then      '* >= 2 line group                   ''~v138I~
'*          Trace.W("last chkshortLine")                               ''~v138I~''+v155R~
            chkShortLine(listLines, linesz, groupmax, grouptop, groupctr, ls) ''~v138I~
        End If                                                         ''~v138I~
        lineStyle = ls                                                   ''~v138I~
    End Sub                                                            ''~v138R~
    '*************************************************************     ''~v140I~
    Private Sub chkLineSpacing(Plineno As Integer, Pls() As Integer, PlineSpacing() As Double, PcharH() As Double, PcharW() As Double) ''~v140I~
        If Plineno = 0 Then                                                     ''~v140I~
            Exit Sub                                                   ''~v140I~
        End If                                                         ''~v140I~
        If Plineno - 1 >= Pls.Count Then                                          ''~v140I~
            Exit Sub                                                   ''~v140I~
        End If                                                         ''~v140I~
        Dim ii As Integer = Plineno                                      ''~v140I~
        Dim type As Integer = Pls(ii)                                    ''~v140I~
        Dim typeo As Integer = Pls(ii - 1)                                 ''~v140I~
        Dim typen As Integer = Pls(ii + 1)                                 ''~v140I~
        Dim dirH As Boolean = (type And LS_HORIZONTAL) <> 0       '*horizontal''~v140I~
        Dim dirHo As Boolean = (typeo And LS_HORIZONTAL) <> 0       '*horizontal''~v140I~
        Dim dirHn As Boolean = (typen And LS_HORIZONTAL) <> 0       '*horizontal''~v140I~
        If dirH <> dirHo OrElse dirH <> dirHn Then   '3 line continued direction''~v140I~
            Exit Sub                                                   ''~v140I~
        End If                                                         ''~v140I~
        Dim prevSpace = PlineSpacing(ii)                                 ''~v140I~
        Dim nextSpace = PlineSpacing(ii + 1)                               ''~v140I~
        Dim charsz As Double                                                     ''~v140I~
        If dirH Then                                                        ''~v140I~
            charsz = PcharH(ii)                                          ''~v140I~
        Else                                                           ''~v140I~
            charsz = PcharW(ii)                                          ''~v140I~
        End If                                                         ''~v140I~
'*      Trace.W("chkLineSpacing dirH=" & dirH & ",charH=" & PcharH(ii) & ",charW=" & PcharW(ii) & ",charsz pixel=" & charsz) ''~v140R~''+v155R~
'*      Trace.W("chkLineSpacing charsz=" & charsz & ",prev=" & prevSpace & ",next=" & nextSpace & ",text=" & listLines.Item(ii).Text) ''~v140I~''+v155R~
        charsz *= RATE_LINESPACING '*2.0 if over 1 line spacing (linespacing include the line itself''~v154I~
        If prevSpace > charsz AndAlso nextSpace > charsz Then                  ''~v140I~
'*          Trace.W("chkLineSpacing changelinespace add CRLF")         ''~v140I~''+v155R~
            Pls(ii) = Pls(ii) Or (LS_ADD_CRLF + LS_CHANGE_LINESPACE)       ''~v140I~
        End If                                                         ''~v140I~
    End Sub                                                            ''~v140I~
    '*************************************************************     ''~v144I~
    '*   Private Sub chkSameLine(Plineno As Integer, Pls() As Integer, PminLineHeight As Double, PminLineWidth As Double) ''~v144I~''~v147R~
    Private Sub chkSameLine(Plineno As Integer, Pls() As Integer, PminLineHeight As Double, PminLineWidth As Double, Pbaseline() As Double) ''~v147I~
        If Plineno = 0 Then                                            ''~v144I~
            Exit Sub                                                   ''~v144I~
        End If                                                         ''~v144I~
        Dim ii As Integer = Plineno                                    ''~v144I~
        Dim type As Integer = Pls(ii)                                  ''~v144I~
        Dim typeo As Integer = Pls(ii - 1)                             ''~v144I~
        Dim dirH As Boolean = (type And LS_HORIZONTAL) <> 0       '*horizontal''~v144I~
        Dim dirHo As Boolean = (typeo And LS_HORIZONTAL) <> 0       '*horizontal''~v144I~
'*      Trace.W("chkSameLine dirH=" & dirH & ",dirHo=" & dirHo & ",text=" & listLines(ii).Text) ''~v145I~''+v155R~
        If dirH <> dirHo Then    'dir changed                               ''~v144I~
            Exit Sub                                                   ''~v144I~
        End If                                                         ''~v144I~
        Dim line As OcrLine = listLines(ii)                            ''~v144I~
        Dim lineo As OcrLine = listLines(ii - 1)                         ''~v144I~
        Dim word As OcrWord = line.Words.Item(0)                               ''~v144I~
        Dim wordo As OcrWord = lineo.Words.Item(0)                             ''~v144I~
        Dim base As Double                                             ''~v144I~
        Dim baseo As Double                                            ''~v144I~
        If dirH Then                                                   ''~v144I~
            '*          base = word.BoundingRect.Y                                  ''~v144I~''~v147R~
            base = Pbaseline(ii)                                       ''~v147R~
            base /= PminLineHeight                                        ''~v144I~
            '*          baseo = wordo.BoundingRect.Y                                ''~v144I~''~v147R~
            baseo = Pbaseline(ii - 1)                                  ''~v147R~
            baseo /= PminLineHeight                                       ''~v144I~
        Else   '*vline                                                 ''~v144I~
            base = word.BoundingRect.X                                  ''~v144I~
            base /= PminLineWidth                                         ''~v144I~
            baseo = wordo.BoundingRect.X                                ''~v144I~
            baseo /= PminLineWidth                                        ''~v144I~
        End If                                                         ''~v144I~
        Dim lineid As Integer = CType(Math.Floor(base), Integer)                       ''~v144R~
        Dim lineido As Integer = CType(Math.Floor(baseo), Integer)                     ''~v144R~
'*      Trace.W("chkSameLine dirH=" & dirH & ",lineid=" & lineid & ",lineido=" & lineido & ",base=" & base & ",baseo=" & baseo & ",text=" & line.Text & ",texto=" & lineo.Text) ''~v144I~''~v145R~''+v155R~
        If lineid = lineido Then                                              ''~v144I~
'*          Trace.W("chkSamelineSame prevtext=" & lineo.Text)           ''~v144R~''+v155R~
            typeo = typeo Or LS_REP_CRLF                                 ''~v144I~
            Pls(ii - 1) = typeo                                          ''~v144I~
        End If                                                         ''~v144I~
    End Sub                                                            ''~v144I~
    '*************************************************************     ''~v138I~
    Private Sub chkShortLine(PlistLines As List(Of OcrLine), Plinesz() As Double, Pgroupmax As Double, Pgrouptop As Integer, Pgroupctr As Integer, Pls() As Integer) ''~v138I~
#If False Then    '* same linechk insert space between OcrLine on the same line''~v145I~
'*      Trace.W("chkShortLine grouptop=" & Pgrouptop & "groupctr=" & Pgroupctr & ",groupmax=" & Pgroupmax) ''~v138I~''+v155R~
        For jj As Integer = Pgrouptop To Pgrouptop + Pgroupctr - 1         ''~v138I~
'*          Trace.W("chkShortLine grouptext=" & PlistLines.Item(jj).Text) ''~v138I~''+v155R~
            Dim ratesz As Double = Plinesz(jj) / Pgroupmax             ''~v138I~
            If ratesz < RATE_LENGTH_CHANGED Then '*short line          ''~v138I~
'*              Trace.W("chkShortLine lengthchanged len=" & Plinesz(jj) & ",groupmax=" & Pgroupmax & ",rate=" & ratesz) ''~v138I~''+v155R~
                If (Pls(jj) And LS_EOL_DELM) = 0 Then                  ''~v138I~
                    Pls(jj) = Pls(jj) Or LS_ADD_SPACE                  ''~v138I~
'*                  Trace.W("chkShortline lengthchanged addspace") ''~v138I~''~v140R~''+v155R~
                End If                                                 ''~v138I~
            End If                                                     ''~v138I~
        Next                                                           ''~v138I~
#End If                                                                ''~v145I~
    End Sub                                                            ''~v138I~
    '*************************************************************
    Public Function makeLines(Plen As Integer) As String
        Dim sb = New StringBuilder(Plen * 2)
        Try
            chkLineStyle()    '*set lineStyle()                        ''~v138R~
            Dim lineno As Integer = 0                                    ''~v138I~
            '*          For Each line As OcrLine In result.Lines                   ''~v140R~
            For Each line As OcrLine In listLines                     ''~v140I~
                '               sb.Append(line.Text)                                   ''~v104R~
                For Each word As OcrWord In line.Words                 ''~v104I~
                    sb.Append(word.Text)                               ''~v104I~
                Next                                                   ''~v104I~
                Dim type = lineStyle(lineno)                             ''~v138I~
'*              Trace.W("makeLines type=" & type & ",lineText=" & line.Text) ''~v138I~''+v155R~
                If (type And LS_REP_CRLF) <> 0 Then  '*sameline          ''~v144I~
                    sb.Append(" "c)  '* double CRLF to avoid eol concatination''~v144I~
                Else                                                     ''~v144I~
                    If (type And LS_EOL_DELM) = 0 Then                     ''~v138R~
                        If (type And LS_ADD_CRLF) <> 0 Then                ''~v138R~
                            sb.Append(vbCrLf)  '* double CRLF to avoid eol concatination''~v138I~
'*                          Trace.W("makeLines addcrlf")                  ''~v138I~''+v155R~
                        Else                                               ''~v138I~
                            If (type And LS_ADD_SPACE) <> 0 Then           ''~v138R~
                                sb.Append(" "c)  '* double CRLF to avoid eol concatination''~v138I~
'*                              Trace.W("makeLines addspace")             ''~v138I~''+v155R~
                            End If                                         ''~v138R~
                        End If                                             ''~v138I~
                    End If    ''~v138I~
                    sb.Append(vbCrLf)
                End If                                                   ''~v144I~
                lineno += 1                                              ''~v138I~
            Next
        Catch ex As Exception
            '*          showStatus(CNM & "makeLines :" & ex.Message)               ''~v140R~
            Form1.exceptionMsg("OCR:makeLines", ex)                    ''~v140I~
            Return "Failed to Extract Lines"
        End Try
        Return sb.ToString()
    End Function
    '*************************************************************
    Private Sub showStatus(Pmsg As String)                         ''~v100R~
        If statusMsg Is Nothing Then                                        ''~v100I~
            statusMsg = Pmsg                                             ''~v100R~
        End If                                                         ''~v100I~
    End Sub
    '*************************************************************     ''~v110I~''~va04I~
    Public Sub saveCutImage(Pbmp As Bitmap, Prect As Rectangle, Pfnm As String, Pfmt As ImageFormat) ''~va04I~
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
            Form1.exceptionMsg("OCR:SaveImage", ex)                  ''~va04I~''~v140R~
            Return ""                                                  ''~va04I~
        End Try                                                        ''~va04I~
        Return fnm                                                     ''~va04I~
    End Function                                                        ''~va04I~
    '*************************************************************     ''~va04I~
    Public Function cutImage(Pbmp As Bitmap, Prect As Rectangle) As Bitmap ''~va04I~
        Dim bmp As Bitmap = Pbmp.Clone(Prect, Pbmp.PixelFormat)        ''~va04I~
'*      Trace.W("cutimage Prect X=" & Prect.X & ",Y=" & Prect.Y & ",W=" & Prect.Width & ",H=" & Prect.Height)''+v155R~
        Return bmp                                                     ''~va04I~
    End Function                                                       ''~va04I~
    '*************************************************************     ''~va04I~
    Public Function getImageFormat(Pfmt As ImageFormat) As String      ''~va04I~
        If Pfmt.Equals(ImageFormat.Jpeg) Then                          ''~va05I~
            Return "jpg"                                               ''~va05I~
        End If                                                         ''~va05I~
        If Pfmt.Equals(ImageFormat.Icon) Then                          ''~va05I~
            Return "ico"                                               ''~va05I~
        End If                                                         ''~va05I~
        If Pfmt.Equals(ImageFormat.Tiff) Then                          ''~va05I~
            Return "tif"                                               ''~va05I~
        End If                                                         ''~va05I~
        If Pfmt.Equals(ImageFormat.Png) Then                           ''~va05I~
            Return "png"       '*lowercase                             ''~va05I~
        End If                                                         ''~va05I~
        If Pfmt.Equals(ImageFormat.Bmp) Then                           ''~va05I~
            Return "bmp"       '*lowercase                             ''~va05I~
        End If                                                         ''~va05I~
        If Pfmt.Equals(ImageFormat.Gif) Then                           ''~va05I~
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
    '*************************************************************     ''~v140I~
    Private Function toList(Plist As IReadOnlyList(Of OcrLine)) As List(Of OcrLine) ''~v140I~
        Dim lst = New List(Of OcrLine)                                 ''~v140I~
        For Each line As OcrLine In Plist                              ''~v140I~
            lst.Add(line)                                              ''~v140I~
        Next                                                           ''~v140I~
        Return lst                                                     ''~v140I~
    End Function                                                       ''~v140I~
    '*************************************************************         ''~v140I~
    '*  Private Sub sortLines(PlistLines As List(Of OcrLine), Pls() As Integer, Plinesz() As Double, PcharH() As Double, PcharW() As Double, PminLineHeight As Double, PminLineWidth As Double) ''~v140R~''~v147R~
    Private Sub sortLines(PlistLines As List(Of OcrLine), Pls() As Integer, Plinesz() As Double, PcharH() As Double, PcharW() As Double, PminLineHeight As Double, PminLineWidth As Double, Pbaseline() As Double) ''~v147I~
        If sortOption = 0 Then                                              ''~v140I~
            Exit Sub                                                   ''~v140I~
        End If                                                         ''~v140I~
        Dim lineno As Integer = 0                                        ''~v140I~
        Dim sList As New SortedList(Of Integer, Integer)                ''~v140R~
        For Each line As OcrLine In PlistLines                         ''~v140R~
'*          Trace.W("sortVRTL Line text=" & line.Text)                 ''~v140I~''+v155R~
            Dim type As Integer = Pls(lineno)                   ''~v140R~
'*          Dim wordctr As Integer = line.Words.Count                    ''~v140I~''~v155R~
            Dim word As OcrWord = line.Words.Item(0)                     ''~v140I~
            Dim brect As Rect = word.BoundingRect                        ''~v140I~
            brect.Y = Pbaseline(lineno)                                  ''~v147I~
'*          Trace.W("Class10:sortLines WordsTop: X=" & brect.X & ",Y=" & brect.Y & ",W=" & brect.Width & ",H=" & brect.Height & ",btext=" & word.Text) ''~v140R~''+v155R~
            Dim key As Double                                          ''~v140I~
            Dim chW As Double = PminLineWidth                          ''~v140R~
            Dim chH As Double = PminLineHeight                         ''~v140R~
            Select Case sortOption                                     ''~v141I~
                Case SORT_VLINE_RTL  '*= 1       '*sort vertical line by X:largeX-->smallX''~v141I~
                    If (type And LS_HORIZONTAL) <> 0 Then                                ''~v140I~
                        key = CType(brect.Y / chH, Integer) * SORT_SCALE + brect.X    '*Y:small(Up)-->large(Down) and X:left->right''~v140R~
                    Else                                                       ''~v140I~
                        key = -(CType(brect.X / chW, Integer) * SORT_SCALE) + brect.Y  '*X:large minus(Right)-->small minus(Left)  and Y:up-->Down''~v140R~
                    End If                                                     ''~v140I~
                Case SORT_VLINE_LTR  '*= 2       '*sort horizontal line by Y:smallY-->largeY''~v141I~
                    If (type And LS_HORIZONTAL) <> 0 Then                      ''~v141I~
                        key = CType(brect.Y / chH, Integer) * SORT_SCALE + brect.X    '*Y:small(Up)-->large(Down) and X:left->right''~v140R~
                    Else                                                       ''~v141I~
                        key = -CType((SORT_SCALE - brect.X) / chW, Integer) * SORT_SCALE + brect.Y  '*X:large minus(Left)-->small minus(Right) and Y:small(Up)-->Down''~v140R~
                    End If                                                     ''~v141I~
                Case SORT_HLINE_RTL  '*= 3       '*sort horizontal line by Y:smallY-->largeY''~v141I~''~v140R~
                    If (type And LS_HORIZONTAL) <> 0 Then              ''~v141I~
                        key = CType(brect.Y / chH, Integer) * SORT_SCALE + brect.X   '*Y:small(Up)->Down, X:large minus(Right)->Left''~v140R~
                    Else                                               ''~v141I~
                        key = CType((SORT_SCALE - brect.X) / chW, Integer) * SORT_SCALE + brect.Y   '*X:small(Right)->Left Y:small(Up)-->Down''~v140R~
                    End If                                             ''~v141I~
                Case SORT_HLINE_LTR  '*= 4       '*sort horizontal line by Y:smallY-->largeY''~v141I~''~v140R~
                    If (type And LS_HORIZONTAL) <> 0 Then                      ''~v141I~
                        key = CType(brect.Y / chH, Integer) * SORT_SCALE + brect.X   '*Y:small(Up)->Down, X:small(Left)->Right''~v140R~
                    Else                                                       ''~v141I~
                        key = CType((SORT_SCALE + brect.X) / chW, Integer) * SORT_SCALE + brect.Y   '*Y:small(Up)->Down, X:large minus(Right)->Left      '*after vertical line''~v140R~
                    End If                                                     ''~v141I~
            End Select                                                   ''~v141I~
            Dim intkey As Integer = CType(key, Integer)                   ''~v140I~
'*          Trace.W("sort key=" & intkey & ",chW=" & chW & ",chH=" & chH & ",text=" & line.Text) ''~v140R~''+v155R~
            Try                                                        ''~v140I~
                sList.Add(intkey, lineno)                              ''~v140R~
            Catch ex As Exception                                      ''~v140I~
                Form1.showStatus("OCR:sortLine :" & ex.Message)        ''~v140I~
            End Try                                                    ''~v140I~
            lineno += 1                                                ''~v140I~
        Next                                                           ''~v140I~
        '*recreate sorted listLines                                        ''~v140I~
        Dim listlines As List(Of OcrLine) = New List(Of OcrLine)(PlistLines) ''~v140R~
        Dim ls() As Integer = CType(Pls.Clone(), Integer())             ''~v140I~
        Dim linesz() As Double = CType(Plinesz.Clone(), Double())       ''~v140I~
        Dim charH() As Double = CType(PcharH.Clone(), Double())         ''~v140I~
        Dim charW() As Double = CType(PcharW.Clone(), Double())         ''~v140I~
        Dim ii As Integer = 0                                            ''~v140I~
        Dim baseline() As Double = CType(Pbaseline.Clone(), Double())  ''~v147I~
        For Each ent As KeyValuePair(Of Integer, Integer) In sList       ''~v140I~
            Dim idx As Integer = CType(ent.Value, Integer)             ''~v140R~
            PlistLines(ii) = listlines.Item(idx)                         ''~v140I~
            Pls(ii) = ls(idx)                                            ''~v140I~
            Plinesz(ii) = linesz(idx)                                    ''~v140I~
            PcharH(ii) = charH(idx)                                      ''~v140I~
            PcharW(ii) = charW(idx)                                      ''~v140I~
            Pbaseline(ii) = baseline(idx)                              ''~v147I~
'*          Trace.W("Sorted ii=" & ii & ",type=" & Pls(ii) & ",linesz=" & Plinesz(ii) & ",charH=" & PcharH(ii) & ",charW=" & PcharW(ii)) ''~v140R~''+v155R~
'*          Trace.W("baseline.Y=" & Pbaseline(ii) & ",orgY=" & PlistLines(ii).Words.Item(0).BoundingRect.Y & "text=" & PlistLines(ii).Text) ''~v146I~''~v147R~''+v155R~
            ii += 1                                                      ''~v140I~
        Next                                                           ''~v140I~
    End Sub                                                    ''~v140I~
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
            Form1.showStatus("OCR:AsTask :" & ex.Message)              ''~v140R~
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
