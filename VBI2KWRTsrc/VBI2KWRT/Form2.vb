'CID:''+v210R~:#72                             update#=  273;        ''+v210R~
'************************************************************************************''~v017I~
'v210 2021/08/06 support imagefile multi selection                     ''+v210I~
'v152 2018/01/08 zoom also for rotated any degree                      ''~v151I~
'v151 2018/01/07 show rotate degree on status bar                      ''~v151I~
'v150 2018/01/07 cliprect shuld be adjusted when degree rotation because source bmp was expanded''~v150I~
'v149 2018/01/07 box should be cleared by rotate                       ''~v149I~
'va08 2018/01/05 Try rotate image any degree                           ''~va08I~
'v142 2018/01/05 keep rect after extact for repeated extract button,clear box at next mousedown''~v142I~
'va06 2017/12/26 ajust filter index by original extension              ''~va06I~
'va04 2017/12/25 save cut image to file                                ''~va04I~
'va01 2017/12/25 mousedown did not cleared swRectBMP(do partial extracting)''~va01I~''~v142R~
'v113 2017/12/22 put Zorder Top                                        ''~v112I~
'v112 2017/12/22 add file menu button to Image form                    ''~v110I~
'v111 2017/12/22 embed handler by try-catch                            ''~v111I~
'v110 2017/12/22 StringConstant reset required when lang changed       ''~v110I~
'v106 2017/12/20 partially extract from image(box by mouse dragging)   ''~v106I~
'v100 2017/12/15 porting from MOD to Microsoft Ocr Library for Windows ''~v100I~
'v032 2017/09/21 English document, i2e was not used                    ''~v032I~
'v018 2017/09/17 requires activate to move ImageView to top of z-order when next image open''~v018I~
'v017 2017/09/17 Image2kanji issues already processed msg when saved even updated,it dose not allow re-i2k''~v017I~
'************************************************************************************''~v017I~
Public Class Form2
    '*** image file processing                                         ''~v032R~
    Const SCROLLBAR_MARGIN = 0                                            ''~7408R~
    Const SCALELIMIT_LOW = 100     'smallest pixcell                  ''~7408I~
#If False Then                                                         ''~v100I~
    Private formWidth As Integer = My.Settings.CfgFormSizeWImage            ''~7411I~''~7430R~
    Private formHeight As Integer = My.Settings.CfgFormSizeHImage           ''~7411I~''~7430R~
#End If                                                                ''~v100I~
    ''~7411I~
    Private bitmapZoom As Bitmap                                           ''~7409I~''~7430R~
    Private bmpForRect As Bitmap                                       ''~v106I~
    Private imageFilename As String = ""                               ''~7430R~
#If False Then                                                         ''~v100R~
    Private image As System.Drawing.Image                              ''~7430R~
    Private imageH As Integer, imageW As Integer                       ''~7430R~
    Private imageHorg As Integer, imageWorg As Integer                 ''~7430R~
#End If                                                                ''~v100I~
    Private posPanelY As Integer                                           ''~7409R~''~7430R~
    Private borderSize As Integer '= 0 ' SystemInformation.FrameBorderSize.Height''~7409R~''~7430R~
    Private captionHeight As Integer = SystemInformation.CaptionHeight     ''~7408R~''~7430R~
#If False Then                                                         ''~v100I~
    Private resizeBeginW, resizeBeginH As Integer                           ''~7407I~''~7430R~
#End If                                                                ''~v100I~
    Private scaleNew As Double = 1.0                                   ''~7430R~
    Private scaleRate As Double = 0.1      ' 10%                       ''~7430R~
    Private scrollbarH As Integer = SystemInformation.HorizontalScrollBarHeight ''~7430R~
    Private scrollbarW As Integer = SystemInformation.VerticalScrollBarWidth ''~7430R~
    '*  Private Debug As Boolean = True                                    ''~7430R~''~va06R~
    Private rotation As Integer = 0                                          ''~7409I~''~7430R~
    Private titlePrefix As String = ""                                   ''~7613R~
#If False Then                                                              ''~v100R~
    Private Shared extractingEnglishDoc As Boolean = False               ''~7618I~
#Else                                                                  ''~v100R~
    Private Shared extractingEnglishDoc As String = Nothing              ''~v100R~
#End If                                                                ''~v100R~
    '**********                                                        ''~v100R~
    Private cbLang As ToolStripComboBox                                ''~v100R~
    Private orgBMP As Bitmap = Nothing                                 ''~v100R~
    Private wordBMP As Bitmap = Nothing                                ''~v100R~
    Private rotateAnyBMP As Bitmap = Nothing                           ''~va08I~
    Private extractedBMP As Bitmap = Nothing                           ''~va08I~
    Private langTag As String                                          ''~v100R~
    Private swWordBMP As Boolean = False                               ''~v100R~
'*  Private iOCR As Cocr = Nothing                                     ''~v100R~''+v210R~
    Public iOCR As Cocr = Nothing                                      ''+v210I~
    Const SCALE_INITIAL = 1.0                                          ''~v100R~
    Const SCALE_RATE = 0.1                                             ''~v100R~
    Const SCALE_LIMIT_LOW = 0.01                                       ''~v100R~
'*  Const LANG_TAG_JP = "ja"                                             ''~v@@@I~''~v100I~''+v210R~
    Public Const LANG_TAG_JP = "ja"                                    ''+v210I~
    Private xText As String = ""                                       ''~v100I~
    Private swEnglishDoc As Boolean = False                            ''~v100I~
    Private swInitialized As Boolean = False                            ''~v110I~
    Private idxLang As Integer                                         ''~v100I~
    '   Private swRotated As Boolean                                       ''~v100I~''~v106R~
    ''~v106I~
    Private iIC As ImageCut = Nothing                                  ''~v106I~
    Private swSaveRectImage As Boolean                                 ''~v106I~
    Private swRectBMP As Boolean 'PictureBox.Image is rect drawn,partially extract in the rect of the image''~v106I~
    Private clipRect As Rectangle 'box on PictureBox image             ''~v106I~
    Private Shared formClip As Form15       'partial text display form ''~v106I~
    Private imageSaveFileFilter As String = "Bitmap|*.bmp|Jpeg|*.jpg|Png|*.png|Tiff|*.tif|Icon|*.ico|All Files|*.*" ''~va04R~
    Private imageSaveFilterIndex As Integer = 1                          ''~va04R~
    Private BKC_DegreeOff As Color = System.Drawing.SystemColors.ButtonShadow ''~va08I~
    Private BKC_DegreeON As Color = System.Drawing.Color.Yellow        ''~va08I~
    Private swDegree As Boolean = False                                ''~va08I~
    Private ctrDegree As Integer = 0                                   ''~va08I~
    Private ctrDegreeMsg As Integer = 0                                ''~v151I~
    Public swNewForm3 As Boolean                                       ''+v210I~
    '************************************************
    Private Sub Form2_Load(sender As System.Object, e As System.EventArgs) Handles MyBase.Load ''~7619R~
        Try                                                            ''~v111I~
            '       called at show                                                 ''~7408R~
            '    	Form1.setCulture()                                             ''~v110R~
            cbLang = ToolStripComboBoxLang                                 ''~v100R~
            iOCR = New Cocr()                                              ''~v100R~
            iIC = New ImageCut(PictureBox1)                                ''~v106I~
            idxLang = My.Settings.CFGF2_LangIndex                            ''~v100I~
            setupComboBoxLang()                                            ''~v100R~
            Form1.setupTitlebarIcon(Me)                                    ''~7612I~
            setLocale(False)                                                    ''~7613I~''~7619R~
            titlePrefix = Me.Text                                            ''~7613R~
            Me.Text = titlePrefix & " : " & imageFilename                  ''~7613R~
            loadMRUList()                                              ''~v112R~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form2 Load", ex)                        ''~v111I~
        End Try                                                        ''~v111I~
        swInitialized = True                                             ''~v110I~
        ToolStripMenuItemDegree1.BackColor = BKC_DegreeOff             ''~va08I~
    End Sub
    Private Sub Form2_Shown(sender As System.Object, e As System.EventArgs) Handles Me.Shown ''~7619I~
#If False Then                                                              ''~v100I~
        Me.Width = formWidth                                           ''~7619I~
        Me.Height = formHeight                                         ''~7619I~
        Me.Visible = True                                                ''~v017I~
#End If                                                                ''~v100I~
    End Sub                                                            ''~7619I~
    Private Sub Form2_Disposed(sender As System.Object, e As System.EventArgs) Handles Me.Disposed ''~7428I~''~v100R~
        Try                                                            ''~v111I~
            Dim image As Bitmap = orgBMP                                   ''~v100R~
            If Not IsNothing(image) Then                                        ''~7428I~
                image.Dispose()                                            ''~7428R~
            End If                                                          ''~7428I~
            image = wordBMP                                                ''~v100R~
            If Not IsNothing(image) Then                                   ''~v100I~
                image.Dispose()                                            ''~v100I~
            End If                                                         ''~v100I~
            disposePictureBox()                                            ''~7513I~
            '           My.Settings.CFGF2_LangIndex = idxLang                            ''~v100I~''~v112R~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form2 Disposed", ex)                    ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7428I~
    '**************************************************                ''~v110I~
    Private Sub ToolStripComboBoxlang_Change(sender As System.Object, e As System.EventArgs) Handles ToolStripComboBoxLang.SelectedIndexChanged ''~V110R~''~v112I~
        If swInitialized Then                                          ''~v110I~
            iOCR.getSelectedLangTag(cbLang, idxLang)                   ''~v110I~
            My.Settings.CFGF2_LangIndex = idxLang                      ''~v110I~
        End If                                                         ''~v110I~
    End Sub                                                            ''~v110I~
    Private Sub On_SaveImage_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItemSaveImage.Click ''~va04R~
        Try                                                            ''~va04R~
            saveImage()                                                ''~va04R~
        Catch ex As Exception                                          ''~va04R~
            Form1.exceptionMsg("Form2 SaveImage_Click", ex)            ''~va04R~
        End Try                                                        ''~va04R~
    End Sub                                                            ''~va04R~
    '**************************************************                ''~v106I~
    Private Sub PictureBox1_MouseDown(sender As Object, e As MouseEventArgs) Handles PictureBox1.MouseDown ''~v112I~
        Try                                                            ''~v111I~
            PBmouseDown(e)                                                 ''~v106I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form2 MouseDown", ex)                   ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~v106I~
    '**************************************************                ''~v106I~
    Private Sub PictureBox1_MouseUp(sender As Object, e As MouseEventArgs) Handles PictureBox1.MouseUp ''~v112I~
        Try                                                            ''~v111I~
            PBmouseUp(e)                                                   ''~v106I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form2 MouseUp", ex)                     ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~v106I~
    '**************************************************                ''~v106I~
    Private Sub PictureBox1_MouseMove(sender As Object, e As MouseEventArgs) Handles PictureBox1.MouseMove ''~v112I~
        Try                                                            ''~v111I~
            PBmouseMove(e)                                                 ''~v106I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form2 MouseMove", ex)                   ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~v106I~
    '*************************************************************     ''~v100I~
#If FalseThen Then                                                          ''~v100R~
    Private Function CreateImage(Pfnm As String) As Boolean 'for sharing vioration at readtext caused by Image.FromFile''~7428I~
#Else                                                                  ''~v100R~
    Private Function CreateImage(Pfnm As String, ByRef Ppbitmap As Bitmap) As Boolean 'for sharing vioration at readtext caused by Image.FromFile''~v100R~
        Dim image As Bitmap = Nothing                                    ''~v100R~
#End If                                                                ''~v100R~
        '       image = System.Drawing.Image.FromFile(imageFilename)           ''~7428I~
        Dim rc As Boolean = False                                        ''~7428I~
        If (Not (System.IO.File.Exists(Pfnm))) Then                    ''~7428I~
            Form1.NotFound(Pfnm)                                             ''~7428I~
            Return False                                               ''~7428I~
        End If                                                         ''~7428I~
        Try                                                            ''~7428I~
#If FalseThen Then                                                     ''~v100I~
            Dim fs As New System.IO.FileStream(imageFilename, System.IO.FileMode.Open, System.IO.FileAccess.Read) ''~7428I~
            image = CType(System.Drawing.Image.FromStream(fs), Bitmap)                 ''~7428I~
            fs.Close()                                                 ''~7428I~
#Else                                                                  ''~v100I~
            image = CType(System.Drawing.Image.FromFile(imageFilename), Bitmap) ''~v100I~
#End If                                                                ''~v100I~
            rc = True                                                    ''~7428I~
        Catch ex As Exception                                          ''~7428I~
            '           MessageBox.Show("イメージファイル（" & Pfnm & "）からのテキスト読み取り失敗:" & ex.Message) ''~7428I~''~7617R~
            Form1.ReadError(Pfnm, ex)                                   ''~7617I~
        End Try                                                        ''~7428I~
        Ppbitmap = image                                                 ''~v100R~
        swRectBMP = False    '*initial                                 ''~v142I~
        ctrDegree = 0                                                  ''~va08I~
        Return rc                                                      ''~7428I~
    End Function                                                       ''~7428I~
    '*************************************************************     ''~v100I~
    '* from Form1:openImageBox                                         ''~v100I~
    Function setImage(Pfnm As String) As Boolean                       ''~7428R~
        If titlePrefix.CompareTo("") <> 0 Then  'opened image file already opened''~7613I~
            Me.Text = titlePrefix & " : " & Pfnm                       ''~7613R~
        End If                                                         ''~7613I~
        imageFilename = Pfnm                                            ''~7411R~
#If False Then                                                              ''~v100R~
        If Not CreateImage(Pfnm) Then                                           ''~7428I~
#Else                                                                  ''~v100R~
        Dim image As Bitmap = Nothing                                            ''~v100R~
        If Not CreateImage(Pfnm, image) Then                            ''~v100R~
#End If                                                                ''~v100R~
            Return False
        End If ''~7428I~
#If False Then                                                         ''~v100I~
        imageH = image.Height
        imageW = image.Width
        imageHorg = imageH
        imageWorg = imageW
        getTitleHeight()                                               ''~7408I~
#End If                                                                ''~v100I~
#If False Then                                                         ''~v100I~
        If (formHeight = 0) Then                                              ''~7411I~
            formWidth = CType(My.Computer.Screen.Bounds.Size.Width / 2, Integer)           ''~7411I~
            formHeight = CType(My.Computer.Screen.Bounds.Size.Height / 2, Integer)         ''~7411I~
        End If                                                         ''~7411I~
        setLayout(formWidth, formHeight)                                      ''~7408I~''~7411R~
#End If                                                                ''~v100I~
#If False Then                                                              ''~v100R~
        Dim rh As Double = formHeight / imageH                            ''~7513I~
        Dim rw As Double = formWidth / imageW                            ''~7513I~
        scaleNew = math.Min(rh, rw)                                     ''~7513R~
        zoomImage(0)                                                   ''~7513I~
#Else                                                                  ''~v100M~
        saveOrgBMP(image)                                              ''~v100M~
        scaleNew = adjustScale(image, scaleNew)                        ''~v100M~
        drawZoom(image, scaleNew)                                      ''~v100M~
#End If                                                                ''~v100M~
        Return True                                                    ''~7428I~
    End Function                                                       ''~7428R~
    '*************************************************************     ''~v100I~
#If False Then                                                         ''~v100I~
    Private Sub getTitleHeight()                                       ''~7408I~
        posPanelY = Me.Height - Me.ClientSize.Height + PanelPictureBox.Bounds.Y   ''~7409R~''~v100R~
        borderSize = CType((Me.Width - PanelPictureBox.Width) / 2, Integer)                       ''~7409I~''~v100R~
    End Sub                                                            ''~7408I~
#End If                                                                ''~v100I~
    '*************************************************************     ''~v100I~
#If False Then                                                         ''~v100M~
    Private Sub setLayout(Pww As Integer, Phh As Integer)
        Dim ww As Integer = System.Math.Min(imageW, Pww)
        Dim hh As Integer = System.Math.Min(imageH, Phh - posPanelY)        ''~7408R~''~7409R~
        '        Dim rc As Integer                                     ''~7407R~
        PictureBox1.SizeMode = PictureBoxSizeMode.AutoSize             ''~7408R~
        PanelPictureBox.AutoScroll = True                                       ''~7408I~''~v100R~
        PanelPictureBox.SetAutoScrollMargin(CType(SCROLLBAR_MARGIN / 2, Integer), CType(SCROLLBAR_MARGIN / 2, Integer))  ''~7408I~''~v100R~
        scrollbarH += SCROLLBAR_MARGIN
        scrollbarW += SCROLLBAR_MARGIN
        PanelPictureBox.Height = hh                                             ''~7408M~''~v100R~
        PanelPictureBox.Width = ww                                              ''~7408M~''~v100R~
        Me.Width = ww + borderSize * 2
        Me.Height = hh + posPanelY                                     ''~7409R~
    End Sub    'setLayout                                              ''~7408R~
#End If                                                                ''~v100M~

    Private Sub Form2_ResizeBegin(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.ResizeBegin ''~7407I~
#If False Then                                                              ''~v100I~
        Dim this As Control = CType(sender, Control)                                   ''~7407I~
        resizeBeginW = this.Size.Width                                  ''~7407I~
        resizeBeginH = this.Size.Height                                 ''~7407I~
#End If                                                                ''~v100I~
    End Sub                                                            ''~7407I~
    ''~7408R~
    Private Sub Form2_ResizeEnd(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.ResizeEnd
        '        Dim rc As Integer                                              ''~7407M~
        Dim this As Control = CType(sender, Control)
        Dim ww As Integer = this.Size.Width
        Dim hh As Integer = this.Size.Height
#If False Then                                                         ''~v100I~
        My.Settings.CfgFormSizeWImage = ww                               ''~7411I~
        My.Settings.CfgFormSizeHImage = hh                               ''~7411I~
#End If                                                                ''~v100I~
#If False Then                                                              ''~v100I~
        Dim diffH = hh - resizeBeginH         'expanded H              ''~7408R~
        Dim diffW = ww - resizeBeginW         'expanded W                           ''~7407I~''~7408R~
        '        Dim visibleVS, visibleHS As Boolean                              ''~7408I~
        ''~7408I~
        If diffH = 0 AndAlso diffW = 0 Then                                ''~7407M~''~7411R~
            Exit Sub 'not resize but form moved                        ''~7407M~
        End If
        '        Exit Sub ''~7407M~
        ww -= borderSize * 2   'panel width                           ''~7408R~''~7409R~
        hh -= posPanelY        'panel height                ''~7408R~  ''~7409R~
        '       ww = Me.ClientSize.Width                                         ''~7408R~''~7409R~
        '       hh = Me.ClientSize.Height-panelPictureBox.Bounds.Y                      ''~7408R~''~7409R~''~v100R~
        PanelPictureBox.Height = hh                                             ''~7407M~''~v100R~
        PanelPictureBox.Width = ww                                              ''~7407M~''~v100R~
        PanelPictureBox.Refresh()                                               ''~7408I~''~v100R~
#End If                                                                ''~v100I~
    End Sub 'resize                                                    ''~7408R~

    '    Private Sub PictureBox1_Resize(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Resize''~7408R~
    '        Dim this As Control = sender                                  ''~7408R~
    '        '        PictureBox1.Size = New System.Drawing.Size(New Point(this.Width, this.Height))''~7408R~
    '    End Sub                                                           ''~7408R~
    ''~7408I~
#If False Then                                                         ''~v100I~
    Private Function chkScrollbar(Pww As Integer, Phh As Integer) As Integer ''~7408R~
        'rc 1:Vertical 2,horizontal,3 both
        Dim rc As Integer = 0
        If Pww < imageW Then                              ''~7408R~
            rc += 2         'Horizontal scrollbar required             ''~7408R~
        End If
        If Phh < imageH Then                              ''~7408R~
            rc += 1         'Vertical scrollbar required               ''~7408R~
        End If
        Return rc
    End Function
#End If                                                                ''~v100I~

    Private Sub On_ZoomIn_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItemZoomIn.Click ''~v112I~
        Try                                                            ''~v111I~
#If False Then                                                              ''~v100R~
        zoomImage(1)                                                   ''~7408I~
        If rotation > 0 Then                                                  ''~7513I~
            rotateImage(0)                                             ''~7513I~
        End If                                                         ''~7513I~
#Else                                                                  ''~v100R~
            showStatus("")                                                 ''~v100I~
            drawZoom(1)                                                    ''~v100R~
#End If                                                                ''~v100R~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form2 ZoomUp", ex)                      ''~v111R~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7408I~
    Private Sub On_ZoomOut_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItemZoomOut.Click ''~v112I~
        Try                                                            ''~v111I~
#If False Then                                                              ''~v100R~
        zoomImage(-1)                                                  ''~7408I~
        If rotation > 0 Then                                                  ''~7513I~
            rotateImage(0)                                             ''~7513I~
        End If                                                         ''~7513I~
#Else                                                                  ''~v100R~
            showStatus("")                                                 ''~v100I~
            drawZoom(-1)                                                   ''~v100R~
#End If                                                                ''~v100R~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form2 ZoomDown", ex)                    ''~v111R~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7408I~
    Private Sub On_RotateLeft_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItemRotateLeft.Click ''~v112I~
        Try                                                            ''~v111I~
            showStatus("")                                                 ''~v100I~
            swRectBMP = False                                            ''~v149I~
            If swDegree Then                                               ''~va08I~
                rotateAny(-1)                                              ''~va08I~
            Else                                                           ''~va08I~
                ctrDegree = 0                                              ''~va08I~
                rotateImage(3)   '90*3                                         ''~7409R~
            End If                                                         ''~va08I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form2 RotateLeft", ex)                  ''~v111R~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7409I~
    Private Sub On_RotateRight_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItemRotateRight.Click ''~v112I~
        Try                                                            ''~v111I~
            showStatus("")                                                 ''~v100I~
            swRectBMP = False                                            ''~v149I~
            If swDegree Then                                               ''~va08I~
                rotateAny(1)                                               ''~va08I~
            Else                                                           ''~va08I~
                ctrDegree = 0                                          ''~v150I~
                rotateImage(1)   '90*1                                         ''~7409R~
            End If                                                         ''~va08I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form2 RotateRight", ex)                 ''~v111R~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7409I~
    '**************************************************                ''~va08I~
    Private Sub ToolStripMenuItemDegree1_Click(ByVal sender As System.Object, ByVal e As EventArgs) Handles ToolStripMenuItemDegree1.Click ''~va08I~
        If swDegree Then                                               ''~va08I~
            swDegree = False                                           ''~va08I~
            ToolStripMenuItemDegree1.BackColor = BKC_DegreeOff         ''~va08I~
            showStatus("One degree mode rotation:Off")                 ''~v151R~
        Else                                                           ''~va08I~
            swDegree = True                                            ''~va08I~
            ToolStripMenuItemDegree1.BackColor = BKC_DegreeON          ''~va08I~
            showStatus("One degree mode rotation:On")                  ''~v151R~
        End If                                                         ''~va08I~
    End Sub                                                            ''~va08I~
    '**************************************************                ''~va08I~
    Private Sub On_Extract_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItemExtract.Click ''~v112I~
        '#If True   Then '@@@@test                                             ''~v112R~
        '        Dim frm As Form16=New Form16()                                ''~v112R~
        '        Image2Text(imageFilename)                                     ''~v112R~
        '        frm.setText(CType(PictureBox1.Image,Bitmap))                  ''~v112R~
        '        frm.Show()                                                    ''~v112R~
        '#Else                                                                 ''~v112R~
        Try                                                            ''~v111I~
#If False Then                                                         ''~v100R~
        Image2Text(imageFilename, False)                                      ''~7410R~''~7619R~
#Else                                                                  ''~v100R~
            showStatus("")                                                 ''~v100I~
            Image2Text(imageFilename)                                      ''~v100R~
#End If                                                                ''~v100R~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form2 Extract", ex)                     ''~v111R~
        End Try                                                        ''~v111I~
        '#End If                                                               ''~v112R~
    End Sub                                                            ''~7410I~
#If False Then                                                         ''~v100R~
    Private Sub ToolStripButtonEnglishDoc_Click(ByVal sender As System.Object, ByVal e As EventArgs) Handles ToolStripButtonEnglishDoc.Click ''~7619I~
        Image2Text(imageFilename, True)                                 ''~7619I~
    End Sub                                                            ''~7619I~
#End If                                                                ''~v100R~
    Private Sub On_Help_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItemHelp.Click ''~v112I~
        Try                                                            ''~v111I~
            showStatus("")                                                 ''~v100I~
            showHelp()                                                     ''~7429I~
        Catch ex As Exception                                          ''~v111I~
            Form1.exceptionMsg("Form2 Help", ex)                        ''~v111I~
        End Try                                                        ''~v111I~
    End Sub                                                            ''~7429I~

#If False Then                                                         ''~v100R~
    '*************************************************************     ''~v032I~
    Private Function drawImage(Pww As Integer, Phh As Integer) As Boolean ''~7409M~
        '                                                              ''~7409M~
        'draw zoomed image                                             ''~7409M~
        '                                                              ''~7409M~
        Dim rc As Boolean = True                                       ''~7409M~
        Try                                                            ''~7409M~
            disposePictureBox()                                        ''~7513M~
            bitmapZoom = New Bitmap(image, Pww, Phh)               ''~7408R~''~7409M~
            PictureBox1.Image = bitmapZoom                             ''~7409M~
            '           If (Debug) Then                                            ''~7407R~''~7409M~
            '               Dim msg As String = String.Format("scaleNew={0},new imageH={1},new imageW={2}", scaleNew, Phh, Pww)''~7407R~''~7408R~''~7409M~
            '               MessageBox.Show(msg)                                   ''~7407R~''~7409M~
            '           End If                                                     ''~7407R~''~7409M~
        Catch ex As Exception                                          ''~7409M~
            '           MessageBox.Show("ズームできません:" & ex.Message)  ''~7409M~''~7428R~''~7617R~
            MessageBox.Show(ex.Message, Rstr.MSG_ERR_ZOOM)              ''~7617I~
            rc = False                                                 ''~7409M~
        End Try                                                        ''~7409M~
        Me.Activate()                                                  ''~v018R~
        Return rc                                                      ''~7409M~
    End Function                                                       ''~7409M~
    ''~7409I~
    '*************************************************************     ''~v032I~
    Private Sub zoomImage(Pzoom As Integer)                            ''~7408I~
        '                                                                  ''~7408I~
        Dim scaleNext, scaleBoundary As Double                          ''~7408R~
        Dim hh, ww As Integer                                           ''~7408I~
        ''~7408I~
        ''~7408I~
        If Pzoom > 0 Then                                            ''~7408I~''~7513R~
            scaleNext = scaleNew + scaleRate                           ''~7408I~
        ElseIf Pzoom < 0 Then                                          ''~7513R~
            scaleNext = scaleNew - scaleRate                           ''~7408I~
        Else                                                           ''~7513I~
            scaleNext = scaleNew                                       ''~7513I~
        End If                                                         ''~7408I~
        hh = imageHorg * scaleNext                                         ''~7408I~
        ww = imageWorg * scaleNext                                         ''~7408I~
        '* stop at boundary                                                    ''~7408I~
        scaleBoundary = chkZoomBoundary(Pzoom, scaleNext)                 ''~7408I~
        If (scaleBoundary <> 0 AndAlso scaleBoundary <> scaleNew) Then                                           ''~7408I~''~7409R~''~7411R~
            scaleNext = scaleBoundary                                  ''~7408M~
            hh = imageHorg * scaleNext                                 ''~7408I~
            ww = imageWorg * scaleNext                                 ''~7408I~
        End If                                                         ''~7408I~
        If hh < SCALELIMIT_LOW Then                                           ''~7408I~
            Exit Sub                                                   ''~7408I~
        End If                                                          ''~7408I~
        If drawImage(ww, hh) Then                                ''~7408I~
            scaleNew = scaleNext                                       ''~7408I~
            imageH = hh                                                ''~7408I~
            imageW = ww                                                ''~7408I~
        End If                                                         ''~7408I~
    End Sub                                                            ''~7408I~


    Private Function chkZoomBoundary(Pzoom As Integer, Pscale As Double) As Double ''~7408I~
        ' stop at fit to just rectangle                                    ''~7408I~
        Dim scaleNew As Double = 0                                       ''~7409M~
        Dim swH, swW As Boolean                                         ''~7409M~
        Dim hh, ww As Integer                                          ''~7408I~''~7409R~
        Dim hhPanel As Integer = panelPictureBox.Height                          ''~7409R~''~v100R~
        Dim wwPanel As Integer = panelPictureBox.Width                           ''~7409I~''~v100R~
        hh = imageHorg * Pscale                                        ''~7408I~
        ww = imageWorg * Pscale                                        ''~7408I~
        ''~7408I~
        swH = False : swW = False                                            ''~7409I~
        If (Pzoom > 0) Then     'enlarge                               ''~7408I~
            If imageH < hhPanel AndAlso hh >= hhPanel Then    'stop at hhPanel                    ''~7408I~''~7409R~''~7411R~
                swH = True                                              ''~7409I~
            End If                                                      ''~7409I~
            If imageW < wwPanel AndAlso ww >= wwPanel Then  'stop at wwPanel''~7409I~''~7411R~
                swW = True                                              ''~7409I~
            End If                                                      ''~7409I~
            If (swH) Then                                                   ''~7409I~
                If (swW) Then                                               ''~7409I~
                    Dim scaleH = adjustZoomH()                       ''~7409I~
                    Dim scaleW = adjustZoomW()                       ''~7409I~
                    scaleNew = Math.Min(scaleH, scaleW)                   ''~7409I~
                Else                                                   ''~7409I~
                    scaleNew = adjustZoomH()                         ''~7409I~
                End If                                                  ''~7409I~
            Else                                                       ''~7409I~
                If (swW) Then                                               ''~7409I~
                    scaleNew = adjustZoomW()                         ''~7409I~
                End If                                                  ''~7409I~
            End If                                                      ''~7409I~
        Else                   'shrink                                 ''~7408I~
            If imageH > hhPanel AndAlso hh <= hhPanel Then                        ''~7408I~''~7411R~
                swH = True                                              ''~7409I~
            End If                                                     ''~7409I~
            If imageW > wwPanel AndAlso ww <= wwPanel Then                 ''~7409I~''~7411R~
                swW = True                                               ''~7409I~
            End If                                                     ''~7409I~
            If (swH) Then                                                   ''~7409I~
                If (swW) Then                                               ''~7409I~
                    Dim scaleH = adjustZoomH()                       ''~7409I~
                    Dim scaleW = adjustZoomW()                       ''~7409I~
                    scaleNew = Math.Min(scaleH, scaleW)                   ''~7409I~
                Else                                                   ''~7409I~
                    scaleNew = adjustZoomH()                         ''~7409I~
                End If                                                  ''~7409I~
            Else                                                       ''~7409I~
                If (swW) Then                                               ''~7409I~
                    scaleNew = adjustZoomW()                         ''~7409I~
                End If                                                  ''~7409I~
            End If                                                      ''~7409I~
        End If                                                         ''~7408I~
        Return scaleNew                                               ''~7408I~
    End Function                                                       ''~7408I~
    ''~7409I~
    Private Function adjustZoomH() As Double                           ''~7409I~
        '** rate when HH=panelH                                            ''~7409I~
        Dim hhPanel As Integer = panelPictureBox.Height                          ''~7409I~''~v100R~
        Dim wwPanel As Integer = panelPictureBox.Width                           ''~7409I~''~v100R~
        Dim imageHW As Double = imageHorg / imageWorg                      ''~7409I~
        Dim hh, ww As Integer                                           ''~7409I~
        ''~7409I~
        hh = hhPanel                                                     ''~7409I~
        ww = hh / imageHW                                                  ''~7409I~
        If ww > wwPanel Then   'Hscallvar exist                               ''~7409I~
            hh -= scrollbarH                                             ''~7409I~
            ww = hh / imageHW                                              ''~7409I~
        End If                                                          ''~7409I~
        Return ww / imageWorg                                            ''~7409I~
    End Function                                                       ''~7409I~
    ''~7409I~
    Private Function adjustZoomW() As Double                           ''~7409I~
        '** rate when WW=panelW                                            ''~7409I~
        Dim hhPanel As Integer = panelPictureBox.Height                          ''~7409I~''~v100R~
        Dim wwPanel As Integer = panelPictureBox.Width                           ''~7409I~''~v100R~
        Dim imageHW As Double = imageHorg / imageWorg                      ''~7409I~
        Dim hh, ww As Integer                                           ''~7409I~
        ''~7409I~
        ww = wwPanel                                                     ''~7409I~
        hh = ww * imageHW                                                  ''~7409I~
        If hh > hhPanel Then   'Vscallvar exist                               ''~7409I~
            ww -= scrollbarW                                             ''~7409I~
            hh = ww * imageHW                                              ''~7409I~
        End If                                                         ''~7409I~
        Return ww / imageWorg                                            ''~7409I~
    End Function                                                       ''~7409I~
    ''~7409I~
#Else                                                                  ''~v100R~
    '*************************************************************     ''~v100R~
    '*from handler                                                     ''~v152I~
    '*************************************************************     ''~v152I~
    Private Sub drawZoom(Pzoom As Integer)                             ''~v100R~
        '**Pzoom 1:zoomin(enlarge) , -1,zoomout                        ''~v100R~
        Dim zoomBMP As Bitmap                                          ''~v100R~
        If swWordBMP Then                                              ''~v100R~
            zoomBMP = wordBMP                                          ''~v100R~
        Else                                                           ''~v100R~
            If ctrDegree <> 0 Then                                              ''~v152I~
                zoomBMP = rotateAnyBMP                                    ''~v152I~
            Else                                                         ''~v152I~
                zoomBMP = orgBMP                                           ''~v100R~
            End If                                                       ''~v152I~
        End If                                                         ''~v100R~
        Try                                                            ''~v100R~
            Dim scaleNext As Double                                    ''~v100R~
            Dim scaleOld As Double = scaleNew                            ''~v142I~
            Dim hh, ww As Integer                                      ''~v100R~
            If Pzoom <> 0 Then                                         ''~v100R~
#If False Then                                                         ''~v100R~
                scaleNext = scaleNew + SCALE_RATE * Pzoom              ''~v100R~
#Else                                                                  ''~v100R~
                scaleNext = scaleNew * (1.0 + SCALE_RATE * Pzoom) 'small rate when zoomout(-1)''~v100R~
#End If                                                                ''~v100R~
            Else                                                       ''~v100R~
                scaleNext = scaleNew                                   ''~v100R~
            End If                                                     ''~v100R~
            ''~v@@@I~                                                  ''~v100R~
            scaleNext = adjustScale(zoomBMP, Pzoom, scaleNext, scaleNew) ''~v100R~
            '*          hh = CType(orgBMP.Height * scaleNext, Integer)                             ''~v100R~''~v152R~
            '*          ww = CType(orgBMP.Width * scaleNext, Integer)                              ''~v100R~''~v152R~
            hh = CType(zoomBMP.Height * scaleNext, Integer)            ''~v152I~
            ww = CType(zoomBMP.Width * scaleNext, Integer)             ''~v152I~
            If scaleNext < SCALE_LIMIT_LOW Then                        ''~v100R~
                Exit Sub                                               ''~v100R~
            End If                                                     ''~v100R~
            bitmapZoom = New Bitmap(zoomBMP, ww, hh)                   ''~v100R~
            scaleNew = scaleNext                                       ''~v100R~
            setPictureBoxImage(bitmapZoom)                             ''~v100R~
            If swRectBMP Then                                               ''~v142I~
                drawClipBoxWordBMP(bitmapZoom, scaleNext / scaleOld)      ''~v142I~
            End If                                                     ''~v142I~
        Catch ex As Exception                                          ''~v100R~
            MessageBox.Show("Zoom I/Out:" & ex.Message)                ''~v100R~
        End Try                                                        ''~v100R~
    End Sub                                                            ''~v100R~
    '*************************************************************     ''~v100R~
    Private Function drawZoom(Pbitmap As Bitmap, Pzoom As Double) As Boolean ''~v100R~
        If Pzoom = SCALE_INITIAL Then                                  ''~v100R~
            setPictureBoxImage(Pbitmap)                                ''~v100R~
            Return False     'Dont dispose                             ''~v100R~
        End If                                                         ''~v100R~
        Try                                                            ''~v100R~
            Dim hh, ww As Integer ''~v@@@I~                            ''~v100R~
            '*          hh = CType(orgBMP.Height * Pzoom, Integer)                                 ''~v100R~''~v152R~
            '*          ww = CType(orgBMP.Width * Pzoom, Integer)                                  ''~v100R~''~v152R~
            Dim bmp As Bitmap                                          ''~v152I~
            If ctrDegree <> 0 Then                                     ''~v152I~
                bmp = rotateAnyBMP                                     ''~v152I~
            Else                                                       ''~v152I~
                bmp = orgBMP                                           ''~v152I~
            End If                                                     ''~v152I~
            hh = CType(bmp.Height * Pzoom, Integer)                    ''~v152I~
            ww = CType(bmp.Width * Pzoom, Integer)                     ''~v152I~
            bitmapZoom = New Bitmap(Pbitmap, ww, hh)                   ''~v100R~
            setPictureBoxImage(bitmapZoom)                             ''~v100R~
        Catch ex As Exception                                          ''~v100R~
            MessageBox.Show("Zoom by Rate:" & ex.Message)              ''~v100R~
            Return False     'Dont dispose                             ''~v100R~
        End Try                                                        ''~v100R~
        Return True 'Dispose parm bitmap                               ''~v100R~
    End Function                                                       ''~v100R~
    '*************************************************************     ''~v100R~
    Private Function adjustScale(Pbmp As Bitmap, Pzoom As Integer, PscaleNext As Double, PscaleOld As Double) As Double ''~v100R~
        Dim hhNew, wwNew, hhOld, wwOld As Double                      ''~v100R~
        Dim hhPanel As Double = PanelPictureBox.Height - scrollbarH   ''~v100R~
        Dim wwPanel As Double = PanelPictureBox.Width - scrollbarW    ''~v100R~
        Dim scaleNext As Double = PscaleNext                           ''~v100R~
        '********                                                      ''~v100R~
        hhNew = orgBMP.Height * PscaleNext                             ''~v100R~
        wwNew = orgBMP.Width * PscaleNext                              ''~v100R~
        hhOld = orgBMP.Height * PscaleOld                              ''~v100R~
        wwOld = orgBMP.Width * PscaleOld                               ''~v100R~
        If Pzoom < 0 Then  'Zoom out(-)                                ''~v100R~
            If hhNew < hhPanel AndAlso wwNew < wwPanel Then            ''~v100R~
                If hhOld >= hhPanel OrElse wwOld >= wwPanel Then 'wholly included at this zoomout''~v100R~
                    Dim rateH As Double = hhPanel / hhOld       'zoomout to panel''~v100R~
                    Dim rateW As Double = wwPanel / wwOld              ''~v100R~
                    scaleNext = Math.Min(rateH, rateW) * PscaleOld     ''~v100R~
                End If                                                 ''~v100R~
            End If                                                     ''~v100R~
        End If                                                         ''~v100R~
        Return scaleNext                                               ''~v100R~
    End Function                                                       ''~v100R~
    '*************************************************************     ''~v100R~
    '*initial scale to fit all in panel                                ''~v100R~
    Private Function adjustScale(Pbmp As Bitmap, PscaleOld As Double) As Double ''~v100R~
        Dim hhOld, wwOld As Double                      ''~v@@@I~     ''~v100R~
        '       Dim hhPanel As Double = PanelPictureBox.Height - scrollbarH    ''~v100R~
        '       Dim wwPanel As Double = PanelPictureBox.Width - scrollbarW     ''~v100R~
        Dim hhPanel As Double = PanelPictureBox.Height - 1   'initially fill panel without scrollbar''~v100I~
        Dim wwPanel As Double = PanelPictureBox.Width - 1                ''~v100I~
        Dim scaleNext As Double = PscaleOld                            ''~v100R~
        Dim rateH, rateW As Double                                     ''~v100R~
        '********                                                      ''~v100R~
        hhOld = Pbmp.Height * PscaleOld                                ''~v100R~
        wwOld = Pbmp.Width * PscaleOld                                 ''~v100R~
        If hhOld > hhPanel OrElse wwOld > wwPanel Then 'overflow panel size''~v100R~
            rateH = hhPanel / hhOld       'shrink rate                 ''~v100R~
            rateW = wwPanel / wwOld                                    ''~v100R~
            scaleNext = Math.Min(rateH, rateW) * PscaleOld             ''~v100R~
        ElseIf hhOld < hhPanel OrElse wwOld < wwPanel Then 'wholely in panel''~v100R~
            rateH = hhPanel / hhOld       'shrink rate                 ''~v100R~
            rateW = wwPanel / wwOld                                    ''~v100R~
            scaleNext = Math.Min(rateH, rateW) * PscaleOld             ''~v100R~
        End If                                                         ''~v100R~
        Return scaleNext                                               ''~v100R~
    End Function                                                       ''~v100R~
#End If                                                                ''~v100R~
    '*************************************************************     ''~v100R~
#If False Then                                                              ''~v100R~
    Private Sub rotateImage(Pclockwise As Integer)                     ''~7409I~
        '** rotate image                                                   ''~7409I~
        Dim rt As System.Drawing.RotateFlipType                        ''~7409I~
        If Pclockwise = 0 Then 'restore rotation                              ''~7513I~
            Select Case rotation Mod 4                                 ''~7513I~
                Case 0                                                     ''~7513I~
                    Exit Sub                                               ''~7513I~
                Case 1                                                     ''~7513I~
                    rt = RotateFlipType.Rotate90FlipNone                   ''~7513I~
                Case 2                                                     ''~7513I~
                    rt = RotateFlipType.Rotate180FlipNone                  ''~7513I~
                Case 3                                                     ''~7513I~
                    rt = RotateFlipType.Rotate270FlipNone                  ''~7513I~
            End Select                                                 ''~7513I~
        Else                                                           ''~7513I~
            rotation += Pclockwise                                           ''~7409I~''~7513R~
            If (Pclockwise = 1) Then                                              ''~7409I~''~7513R~
                rt = RotateFlipType.Rotate90FlipNone                         ''~7409R~''~7513R~
            Else                                                           ''~7409I~''~7513R~
                rt = RotateFlipType.Rotate270FlipNone                        ''~7409R~''~7513R~
            End If                                                         ''~7409I~''~7513R~
        End If                                                         ''~7513I~
        bitmapZoom.RotateFlip(rt)                                      ''~7409I~
        PictureBox1.Image = bitmapZoom                                 ''~7409I~
    End Sub                                                            ''~7409I~
#Else                                                                  ''~v100R~
    '*************************************************************     ''~v100R~
    Private Sub rotateImage(Pdest As Integer)                          ''~v100R~
        ' Pdest: 1:ClockWise, 3:CounterClockWise                       ''~v100R~
        '       swRotated = True    'allow New Extracting                         ''~v100I~''~v106R~
        Try                                                            ''~v100R~
            '** rotate image                                           ''~v100R~
            Dim rt As System.Drawing.RotateFlipType                    ''~v100R~
            Dim bmp As Bitmap = orgBMP                                 ''~v100R~
            If Pdest = 1 Then                                           ''~v100R~
                rt = RotateFlipType.Rotate90FlipNone                   ''~v100R~
            Else                                                       ''~v100R~
                rt = RotateFlipType.Rotate270FlipNone                  ''~v100R~
            End If                                                     ''~v100R~
            Dim ww, hh As Integer                                      ''~v100R~
            ww = bmp.Width                                             ''~v100R~
            hh = bmp.Height                                            ''~v100R~
            bitmapZoom = New Bitmap(bmp, ww, hh)                       ''~v100R~
            bitmapZoom.RotateFlip(rt)                                  ''~v100R~
            saveOrgBMP(bitmapZoom)                                     ''~v100R~
            bmp = bitmapZoom      'dispose when replaced bitmapZoom at drawZoom''~v100I~
            If drawZoom(bmp, scaleNew) Then                             ''~v100R~
                bmp.Dispose()      'old bitmapZoom                     ''~v100R~
            End If                                                     ''~v100R~
        Catch ex As Exception                                          ''~v100R~
            MessageBox.Show("Rotate :" & ex.Message)                   ''~v100R~
        End Try                                                        ''~v100R~
    End Sub                                                            ''~v100R~
#End If                                                                ''~v100R~
    '*************************************************************         ''~7410I~
#If FalseThen Then                                                          ''~v100R~
    Private Sub Image2Text(Pfnm As String, PenglishDoc As Boolean)                             ''~7410I~''~7619R~
#Else                                                                  ''~v100R~
    Private Sub Image2Text(Pfnm As String)                             ''~v100R~
        langTag = getSelectedLangTag()                                 ''~v100R~
#End If                                                                ''~v100R~
        Dim swDoI2K As Boolean = False                                   ''~v017R~
'*      Dim swNewForm3 As Boolean = False                              ''~v106I~''+v210R~
        swNewForm3 = False                                             ''+v210I~
        If Form1.formText Is Nothing OrElse Form1.formText.IsDisposed Then ''~7411R~''~7521R~
            '           Form1.formText = New Form3()   ' open after extraction succeeded      ''~7410I~''~7411R~''~7521R~''~v106R~
            swNewForm3 = True                                            ''~v106I~
        Else                                                           ''~7508I~
            If Not swRectBMP Then 'full extract                                ''~v106I~
                '           If Not Form1.formText.chkDiscard(Nothing) Then                   ''~7508I~''~7521R~''~v017R~
                If Not Form1.formText.chkDiscard2(swDoI2K) Then            ''~v017R~
                    Exit Sub                                               ''~7508I~
                End If                                                     ''~7508I~
            End If                                                       ''~v106I~
        End If                                                          ''~7411I~
#If False Then                                                           ''~v106R~
        If Not swRectBMP Then 'full extract                                   ''~v106I~
            If Not swDoI2K Then                                                     ''~v017R~
#If False Then                                                              ''~v100R~
        If dupError(Pfnm, PenglishDoc) Then                                              ''~7618I~''~7619R~
#Else                                                                  ''~v100R~
                If dupError(Pfnm, langTag) Then                                 ''~v100R~
#End If                                                                ''~v100R~
                    Exit Sub                                                   ''~7618I~
                End If                                                         ''~7618I~
            End If                                                             ''~v017R~
        End If                                                           ''~v106I~
#End If                                                                ''~v106I~
#If False Then                                                              ''~v100R~
        Dim rc as Boolean=Form1.formText.setImage(Pfnm, PenglishDoc)                                          ''~7410R~''~7411R~''~7521R~''~7619R~
#Else                                                                  ''~v100R~
        Dim swRect As Boolean = swRectBMP 'it is reset by drawWord ->setPictureBoxImage''~v106I~
        Dim rc As Boolean = extractText()                                ''~v100R~
        '*      swRectBMP = False    'drawWord is not called when extracted null ''~v106I~''~v142R~
        If xText.Length = 0 Then                                      ''~v106I~
            rc = False                                                   ''~v106I~
        End If                                                         ''~v106I~
        If rc Then                                                     ''~v100I~
            If swNewForm3 Then                                     ''~v106I~
                Form1.formText = New Form3()   ' open after extraction succeeded''~v106I~
                Form1.formText.setImage(Pfnm, xText, swEnglishDoc) ''~v106I~
            Else                                                   ''~v106I~
                If swRect Then                                             ''~v106R~
                    showPartialText(xText)                                     ''~v106R~
                Else                                                       ''~v106I~
                    Form1.formText.setImage(Pfnm, xText, swEnglishDoc)           ''~v100I~
                    Form1.showTop(Form1.formText)                      ''~v113I~
                End If                                                     ''~v106I~
            End If                                                     ''~v106I~
        End If                                                         ''~v100I~
#End If                                                                ''~v100R~
        If rc Then                                                          ''~7619I~
            Form1.formText.Show()                                          ''~7410R~''~7411R~''~7521R~''~7619R~
        End If                                                         ''~7619I~
    End Sub                                                            ''~7410I~
    '*************************************************************     ''+v210I~
    '*from FormMS                                                      ''+v210I~
    '*************************************************************     ''+v210I~
    Public Function Image2TextMultiSelect(Pfnm As String, PlangTag As String) As String''+v210I~
        imageFilename = Pfnm                                           ''+v210I~
        langTag = PlangTag                                             ''+v210I~
        Dim rc As Boolean = extractTextMultiSelect()                   ''+v210I~
        If Not rc Then                                                 ''+v210I~
            xText = ""                                                 ''+v210I~
        End If                                                         ''+v210I~
        Return xText                                                   ''+v210I~
    End Function                                                       ''+v210I~
    '*************************************************************     ''~v032I~
    '* from Form15:send partial text                                   ''~v106I~
    Private Sub Image2TextPartial(PclipText As String)                 ''~v106I~
        '       Dim swNew As Boolean = False                                     ''~v106I~''~v113R~
        '       If Form1.formText Is Nothing OrElse Form1.formText.IsDisposed Then ''~v106I~''~v113R~
        '           Form1.formText = New Form3()                               ''~v106I~''~v113R~
        '           swNew = True                                                 ''~v106I~''~v113R~
        '       End If                                                         ''~v106I~''~v113R~
        Dim swNew As Boolean = Form1.newForm(Form1.formText, Form1.formText) ''~v113R~
        Form1.formText.setImagePartial(swNew, imageFilename, PclipText, swEnglishDoc) ''~v106I~
        '       Form1.formText.Show() 'popup form                              ''~v106I~''~v113R~
        '       If Not swNew Then                                              ''~v113R~
        '           Form1.showTop(Form1.formText) 'popup form                  ''~v113R~
        '       End If                                                         ''~v113R~
        Form1.showForm(Form1.formText, swNew)                           ''~v113R~
    End Sub                                                            ''~v106I~
    '*************************************************************     ''~v106I~
#If False Then                                                           ''~v106R~
#If False Then                                                              ''~v100R~
    Private Function dupError(Pfnm As String, PenglishDoc As Boolean) As Boolean               ''~7618I~''~7619R~
#Else                                                                  ''~v100R~
    Private Function dupError(Pfnm As String, PenglishDoc As String) As Boolean ''~v100R~
#End If                                                                ''~v100R~
#If False Then                                                              ''~v100R~
        If PenglishDoc = extractingEnglishDoc Then       'allow English and Japanese for same doc         ''~7618I~''~7619R~
#Else                                                                  ''~v100R~
        If PenglishDoc.CompareTo(extractingEnglishDoc) = 0 Then       'allow English and Japanese for same doc''~v100R~
#End If                                                                ''~v100R~
            Dim fnmtxt As String = Form3.getTitleFilename()            ''~7618R~
            Dim fnmimg As String                                       ''~7619R~
            '           If PenglishDoc Then                                             ''~7619I~''~v032R~
            '               fnmimg = Form1.changeExt(Pfnm, Form1.FILTER_DEFAULT_ENGLISHTEXT) ''~7619I~''~v032R~
            '           Else                                                       ''~7619I~''~v032R~
            fnmimg = Form1.changeExt(Pfnm, Form1.FILTER_DEFAULT_KANJITEXT) ''~7619I~
            '           End If                                                     ''~7619I~''~v032R~
            If (Not IsNothing(fnmtxt)) AndAlso (fnmimg.CompareTo(fnmtxt) = 0) Then ''~7618R~
                '               MessageBox.Show(Pfnm, Rstr.MSG_ERR_ALREADY_EXTRACTED)   ''~7618I~''~v100R~
                If Not swRotated Then                                         ''~v100I~
                    showStatus(Pfnm & " : " & Rstr.MSG_ERR_ALREADY_EXTRACTED) ''~v100I~
                    Return True                                            ''~7618I~
                End If                                                   ''~v100I~
            End If                                                     ''~7618I~
        End If                                                         ''~7618I~
        extractingEnglishDoc = PenglishDoc                             ''~7619R~
        swRotated = False                                                ''~v100I~
        Return False                                                   ''~7618I~
    End Function                                                       ''~7618I~
#End If                                                                ''~v106I~
    '*************************************************************     ''~v106I~
    Private Sub showHelp()                                             ''~7429I~
        Dim txt As String                                              ''~7429I~
        If FormOptions.swLangEN Then                                   ''~7613I~
            txt = My.Resources.help_form2E                             ''~7613I~
        Else                                                           ''~7613I~
            txt = My.Resources.help_form2                                  ''~7429I~''~7613R~
        End If                                                         ''~7613I~
        MessageBox.Show(txt, titlePrefix)                                           ''~7429I~''~7613R~
    End Sub                                                            ''~7429I~
    '*************************************************************     ''~v106I~
    Private Sub disposePictureBox()                                         ''~7513I~
        If Not IsNothing(bitmapZoom) Then                                   ''~7513I~
            bitmapZoom.Dispose()                                       ''~7513R~
        End If                                                         ''~7513I~
    End Sub                                                            ''~7513I~
    Public Sub setLocale(Prefresh As Boolean)                                            ''~7613I~''~7619R~
        Dim title As String = Me.Text                                  ''~7619I~
        FormOptions.setLang(Me, GetType(Form2)) 'reset control language                       ''~7613I~''~7619R~
        updateTitle(title)                                             ''~7619I~
    End Sub                                                            ''~7613I~
    Private Sub updateTitle(Pold As String)                            ''~7619I~
        Dim pos As Integer                                             ''~7619I~
        pos = Pold.IndexOf(Form1.TITLE_SEP)                            ''~7619R~
        If pos >= 0 Then                                               ''~7619I~
            Me.Text = Rstr.FORM2_TITLE & Pold.Substring(pos)           ''~7619I~
        End If                                                         ''~7619I~
    End Sub                                                            ''~7619I~
    '*************************************************************     ''~v032I~
    Private Function extractText() As Boolean                          ''~v100R~
        Dim swOK As Boolean = False                                      ''~v100R~
        Try                                                            ''~v100R~
            xText = ""                                                  ''~v100R~
            iOCR.setRect(swRectBMP, CType(PictureBox1.Image, Bitmap), scaleNew, clipRect) ''~v106I~
            '*          swOK = iOCR.extractText(imageFilename, orgBMP, langTag, xText) 'text with CRLF''~v100R~''~va08R~
            Dim bmp As Bitmap                                          ''~va08I~
            If ctrDegree <> 0 Then                                     ''~va08I~
                bmp = rotateAnyBMP                                     ''~va08I~
            Else                                                       ''~va08I~
                bmp = orgBMP                                           ''~va08I~
            End If                                                     ''~va08I~
            swOK = iOCR.extractText(imageFilename, bmp, langTag, xText)     ''~v@@@R~''~va08I~
            extractedBMP = bmp                                         ''~va08I~
            '           setText(xText)                                             ''~v100R~
            If swOK Then                                               ''~v100R~
                If xText.Length = 0 Then                               ''~v106I~
                    '                   showStatus(Rstr.getStr("WARN_EXTRACTED_NULLSTR"))  ''~v106R~''~v110R~
                    showStatus(My.Resources.WARN_EXTRACTED_NULLSTR()) 'by setting of CurrentCulture ''~v106I~''~v110R~
                Else                                                   ''~v106I~
                    showWords()                                            ''~v100R~
                End If                                                 ''~v106I~
                '               swSaved = False                                        ''~v100R~
            Else                                                       ''~v100I~
                showStatus(iOCR.statusMsg)                             ''~v100I~
            End If                                                     ''~v100R~
            '           swSetText = swOK                                           ''~v100R~
        Catch ex As Exception                                          ''~v100R~
            MessageBox.Show("extractText exception:" & ex.Message)     ''~v100R~
        End Try                                                        ''~v100R~
        Return swOK                                                    ''~v100R~
    End Function                                                       ''~v100R~
    '*************************************************************     ''+v210I~
    Private Function extractTextMultiSelect() As Boolean               ''+v210I~
        Dim swOK As Boolean = False                                    ''+v210I~
        Dim clipRectMS As Rectangle = New Rectangle(0, 0, 0, 0)        ''+v210I~
        Dim scaleNewMS As Double = 1.0                                 ''+v210I~
        Dim NotUsedBMP As Bitmap = Nothing                             ''+v210I~
        Try                                                            ''+v210I~
            xText = ""                                                 ''+v210I~
            If CreateImage(imageFilename, orgBMP) Then     '* set seRectBMP=false,ctrDegree=0.0''+v210I~
                '*          iOCR.setRect(swRectBMP, CType(PictureBox1.Image, Bitmap), scaleNew, clipRect)''+v210I~
                iOCR.setRect(swRectBMP, NotUsedBMP, scaleNewMS, clipRectMS)''+v210I~
                Dim bmp As Bitmap                                      ''+v210I~
                '*          If ctrDegree <> 0 Then                     ''+v210I~
                '*              bmp = rotateAnyBMP                     ''+v210I~
                '*          Else                                       ''+v210I~
                '*              bmp = orgBMP                           ''+v210I~
                '*          End If                                     ''+v210I~
                bmp = orgBMP                                           ''+v210I~
                swOK = iOCR.extractText(imageFilename, bmp, langTag, xText)''+v210I~
                '*          extractedBMP = bmp                         ''+v210I~
                '*          If swOK Then                               ''+v210I~
                '*              If xText.Length = 0 Then               ''+v210I~
                '*                  showStatus(My.Resources.WARN_EXTRACTED_NULLSTR()) 'by setting of CurrentCulture''+v210I~
                '*              Else                                   ''+v210I~
                '*                  showWords()                        ''+v210I~
                '*              End If                                 ''+v210I~
                '*          Else                                       ''+v210I~
                '*              showStatus(iOCR.statusMsg)             ''+v210I~
                '*          End If                                     ''+v210I~
                orgBMP.Dispose()                                       ''+v210I~
            End If                                                     ''+v210I~
        Catch ex As Exception                                          ''+v210I~
            MessageBox.Show("extractText for " & imageFilename & " exception:" & ex.Message)''+v210I~
        End Try                                                        ''+v210I~
        Return swOK                                                    ''+v210I~
    End Function                                                       ''+v210I~
    '**************************************************                ''~v100R~
    Private Function getSelectedLangTag() As String                    ''~v100R~
        Dim tag As String = iOCR.getSelectedLangTag(cbLang, idxLang)    ''~v100R~
        swEnglishDoc = Not tag.StartsWith(LANG_TAG_JP)                    ''~v100I~
        Return tag                                                     ''~v100I~
    End Function                                                       ''~v100R~
    '*************************************************************     ''~v100R~
    Private Sub showWords()                        ''~v@@@R~           ''~v100R~
        '** avoid exceotion:Indexed Pixel at Graphics.FromImage for mono color image''~v100R~
        Dim showRect As Boolean = swRectBMP   'recognize in the clipRect''~v106I~
        Try                                                            ''~v100R~
            '********************                                      ''~v100R~
            Dim g As Graphics                                          ''~v100R~
            Dim bmpDraw As Bitmap                                      ''~v100R~
            Try   ' chk indexd pixcel format                           ''~v100R~
                '*              g = Graphics.FromImage(orgBMP)                         ''~v100R~''~va08R~
                g = Graphics.FromImage(extractedBMP)                    ''~va08I~
                '*              bmpDraw = CType(orgBMP.Clone(), Bitmap)     'Not Indexed Pixel format,draw to clone''~v100R~''~va08R~
                bmpDraw = CType(extractedBMP.Clone(), Bitmap)     'Not Indexed Pixel format,draw to clone''~va08I~
            Catch ex As Exception                                      ''~v100R~
                '*              bmpDraw = Index2NonIndex(orgBMP)                       ''~v100R~''~va08R~
                bmpDraw = Index2NonIndex(extractedBMP)                 ''~va08I~
            End Try                                                    ''~v100R~
            If Not iOCR.markWords(bmpDraw) Then                        ''~v100R~
                Exit Sub                                               ''~v100R~
            End If                                                     ''~v100R~
            saveWordBMP(bmpDraw)                                       ''~v100R~
            If drawZoom(bmpDraw, scaleNew) Then                        ''~v100R~
                bmpDraw.Dispose() 'clone was cleaated by zoom env      ''~v100R~
            End If                                                     ''~v100R~
            If showRect Then                                           ''~v106I~
                drawClipBox(CType(PictureBox1.Image, Bitmap), clipRect) ''~v106R~
            End If                                                     ''~v106I~
        Catch ex As Exception                                          ''~v100R~
            MessageBox.Show("ShowWords :" & ex.Message)                ''~v100R~
        End Try                                                        ''~v100R~
    End Sub                                                            ''~v100R~
    '*************************************************************     ''~v106I~
    Private Sub drawClipBox(Pbmp As Bitmap, Prect As Rectangle)        ''~v106I~
        iIC.drawRect(Pbmp, Prect)                                      ''~v106I~
    End Sub                                                            ''~v106I~
    '*************************************************************     ''~v142I~
    Private Sub drawClipBoxWordBMP(Pbmp As Bitmap, Pscale As Double)   ''~v142I~
        Dim rect As Rectangle = clipRect                                 ''~v142I~
        rect.X = CType(rect.X * Pscale, Integer)                                                 ''~v142I~
        rect.Y = CType(rect.Y * Pscale, Integer)                                                 ''~v142I~
        rect.Width = CType(rect.Width * Pscale, Integer)                                             ''~v142I~
        rect.Height = CType(rect.Height * Pscale, Integer)                                            ''~v142I~
        drawClipBox(Pbmp, rect)
        clipRect = rect ''~v142I~
    End Sub                                                            ''~v142I~
    '*************************************************************     ''~v@@@I~''~v100I~
    Private Function Index2NonIndex(Psrc As Bitmap) As Bitmap          ''~v@@@I~''~v100I~
        Dim r As New Rectangle(0, 0, Psrc.Width, Psrc.Height)             ''~v@@@I~''~v100I~
        Dim bmpNonIndexed = Psrc.Clone(r, Imaging.PixelFormat.Format32bppArgb) ''~v@@@I~''~v100I~
        Dim fmt As Imaging.PixelFormat = bmpNonIndexed.PixelFormat     ''~v@@@I~''~v100I~
        Return bmpNonIndexed                                           ''~v@@@I~''~v100I~
    End Function                                                       ''~v@@@I~''~v100I~
    '*************************************************************     ''~va08I~
    Private Sub rotateAny(Pdegree As Integer)                          ''~va08I~
        If PictureBox1.Image Is Nothing Then                           ''~va08I~
            Exit Sub                                                   ''~va08I~
        End If                                                         ''~va08I~
        If ctrDegree = 0 Then                                          ''~v151R~
            ctrDegreeMsg = 0                                           ''~v151R~
        End If                                                         ''~v151R~
        ctrDegreeMsg -= Pdegree                                        ''~v151R~
        ctrDegree = (ctrDegree + Pdegree) Mod 360                      ''~va08I~
        If ctrDegree < 0 Then                                          ''~va08I~
            ctrDegree += 360                                           ''~va08I~
        End If                                                         ''~va08I~
        Try                                                            ''~va08I~
            Dim hh As Integer = orgBMP.Height                          ''~va08I~
            Dim ww As Integer = orgBMP.Width                           ''~va08I~
            Dim rad As Single = CSng((ctrDegree / 180) * Math.PI)      ''~va08I~
            ''~va08I~
            Dim msin As Double = Math.Sin(-rad)                        ''~va08I~
            Dim mcos As Double = Math.Cos(-rad)                        ''~va08I~
            Dim p1x As Double = hh * msin  '*bottom left               ''~va08I~
            Dim p1y As Double = hh * mcos                              ''~va08I~
            Dim p2x As Double = ww * mcos  '*top right                 ''~va08I~
            Dim p2y As Double = -ww * msin                             ''~va08I~
            Dim p3x As Double = p1x + ww * mcos '*bottom right ->hh*msin+ww*mcos''~va08I~
            Dim p3y As Double = p1y - ww * msin '*             ->hh*mcos+ww*Msin''~va08I~
            Dim wwnew As Double                                        ''~va08I~
            Dim hhnew As Double                                        ''~va08I~
            Dim posx As Double                                         ''~va08I~
            Dim posy As Double                                         ''~va08I~
            If ctrDegree >= 0 AndAlso ctrDegree <= 90 Then  '*p1:bottom Left is on 3rd orthant''~va08I~
                wwnew = p2x - p1x                                     '' ~va08R~''~va08I~
                hhnew = p3y                                           '' ~va08R~''~va08I~
                posx = -p1x                                        '' ~va08I~''~va08I~
                posy = 0                                           '' ~va08I~''~va08I~
            ElseIf ctrDegree > 270 AndAlso ctrDegree <= 360 Then '*p1:bottom left is on 4th orthant''~va08I~
                wwnew = p3x           '4th orthant                     ''~va08I~
                hhnew = p1y - p2y                                      ''~va08I~
                posx = 0                                               ''~va08I~
                posy = -p2y                                            ''~va08I~
            ElseIf ctrDegree > 90 AndAlso ctrDegree <= 180 Then  '*p1:bottom left is on 2nd orthant''~va08I~
                wwnew = -p3x                                           ''~va08I~
                hhnew = p2y - p1y                                      ''~va08I~
                posx = -p3x                                            ''~va08I~
                posy = -p1y                                            ''~va08I~
            ElseIf ctrDegree > 180 AndAlso ctrDegree <= 270 Then  '*p1:bottom left is on 2nd orthant''~va08I~
                wwnew = p1x - p2x                                      ''~va08I~
                hhnew = -p3y                                           ''~va08I~
                posx = -p2x                                            ''~va08I~
                posy = -p3y                                            ''~va08I~
            End If                                                     ''~va08I~
            Dim ratew As Double = (wwnew / ww)                         ''~va08I~
            Dim rateh As Double = (hhnew / hh)                         ''~va08I~
#If False Then                                                         ''~va08I~
            Dim includingrate As Double = 1.0 / Math.Max(rateh, ratew) ''~va08I~
            Dim bmp As Bitmap = New Bitmap(ww, hh)  '*including box    ''~va08I~
            Dim g As Graphics = Graphics.FromImage(bmp)                ''~va08I~
            Dim br As Brush = Brushes.Blue                             ''~va08I~
            g.FillRectangle(br, New Rectangle(0, 0, ww, hh))           ''~va08I~
            g.ResetTransform()                                         ''~va08I~
            g.ScaleTransform(CSng(includingrate), CSng(includingrate)) ''~va08I~
            g.TranslateTransform(CSng(posx), CSng(posy))               ''~va08I~
            g.RotateTransform(CSng(ctrDegree))                         ''~va08I~
            Dim rect As Rectangle = New Rectangle(0, 0, CType(ww, Integer), CType(hh, Integer))''~va08I~
            g.DrawImage(orgBMP, rect)                                  ''~va08I~
            g.Dispose()                                                ''~va08I~
            drawZoom(bmp, scaleNew)                                    ''~va08I~
#Else                                                                  ''~va08I~
#If False Then                                                         ''~va08I~
            Dim wwnewi As Integer = CType(ww / includingrate, Integer) ''~va08I~
            Dim hhnewi As Integer = CType(hh / includingrate, Integer) ''~va08I~
#Else                                                                  ''~va08I~
            Dim wwnewi As Integer = CType(wwnew, Integer)              ''~va08I~
            Dim hhnewi As Integer = CType(hhnew, Integer)              ''~va08I~
#End If                                                                ''~va08I~
            Dim bmp As Bitmap = New Bitmap(wwnewi, hhnewi)  '*including box''~va08I~
            Dim g As Graphics = Graphics.FromImage(bmp)                ''~va08I~
            Dim br As Brush = Brushes.Blue                             ''~va08I~
            g.FillRectangle(br, New Rectangle(0, 0, wwnewi, hhnewi))   ''~va08I~
            g.ResetTransform()                                         ''~va08I~
            '*           g.ScaleTransform(CSng(1.0 / includingrate), CSng(1.0 / includingrate))''~va08I~
            g.TranslateTransform(CSng(posx), CSng(posy))               ''~va08I~
            g.RotateTransform(CSng(ctrDegree))                         ''~va08I~
            Dim rect As Rectangle = New Rectangle(0, 0, CType(ww, Integer), CType(hh, Integer)) ''~va08I~
            g.DrawImage(orgBMP, rect)                                  ''~va08I~
            ''~va08I~
            g.Dispose()                                                ''~va08I~
            drawZoomRotateAny(bmp, scaleNew)                           ''~va08I~
#End If                                                                ''~va08I~
            showStatus("Rotation=" & ctrDegreeMsg & "°")              ''~v151I~
        Catch ex As Exception                                          ''~va08I~
            MessageBox.Show("RotateAny :" & ex.Message)                ''~va08I~
        End Try                                                        ''~va08I~
    End Sub    'rotate                                                 ''~va08I~
    '*************************************************************     ''~v100R~
    Private Sub saveOrgBMP(Pbmp As Bitmap)                             ''~v100R~
        Dim oldbmp = orgBMP                                            ''~v100R~
        orgBMP = CType(Pbmp.Clone(), Bitmap)                                          ''~v100R~
        If oldbmp IsNot Nothing Then                                   ''~v100R~
            oldbmp.Dispose()                                           ''~v100R~
        End If                                                         ''~v100R~
        swWordBMP = False    'zoom use orgBMP                          ''~v100R~
    End Sub                                                            ''~v100R~
#If False Then                                                              ''~v112R~
    Private Sub PanelPictureBox_Paint(sender As Object, e As PaintEventArgs) Handles PanelPictureBox.Paint

    End Sub
#End If                                                                ''~v112I~
    '*************************************************************     ''~v100R~
    Private Sub saveWordBMP(Pbmp As Bitmap)                            ''~v100R~
        Dim oldbmp = wordBMP                                           ''~v100R~
        wordBMP = CType(Pbmp.Clone(), Bitmap)                                         ''~v100R~
        If oldbmp IsNot Nothing Then                                   ''~v100R~
            oldbmp.Dispose()                                           ''~v100R~
        End If                                                         ''~v100R~
        swWordBMP = True    'zoom use wordBMP                          ''~v100R~
    End Sub                                                            ''~v100R~
    '*************************************************************     ''~v106I~
    Private Sub saveRectBMP(Pbmp As Bitmap)                            ''~v106I~
        If swSaveRectImage Then    'called true setPictureBoxImage from setPictureBoxImageRect''~v106I~
            Return                                                     ''~v106I~
        End If                                                         ''~v106I~
        Dim oldbmp = bmpForRect                                        ''~v106I~
        bmpForRect = DirectCast(Pbmp.Clone(), Bitmap)                  ''~v106I~
        '*      Trace.W("saveRectBMP  new bmpForRect Hashcode:" & bmpForRect.GetHashCode()) ''~v106I~''~va06R~
        If oldbmp IsNot Nothing Then                                   ''~v106I~
            '*          Trace.W("saveWordBMP dispose Hashcode:" & oldbmp.GetHashCode()) ''~v106I~''~va06R~
            oldbmp.Dispose()                                           ''~v106I~
        End If                                                         ''~v106I~
    End Sub                                                            ''~v106I~
    '*************************************************************     ''~va08I~
    Private Sub saveRotateAnyBMP(Pbmp As Bitmap)                       ''~va08I~
        Dim oldbmp = rotateAnyBMP                                      ''~va08I~
        rotateAnyBMP = CType(Pbmp.Clone(), Bitmap)                     ''~va08I~
'*        Trace.W("saveRotateAnyBMP  new bmp Hashcode:" & rotateAnyBMP.GetHashCode()) ''~va08I~''~v152R~
        If oldbmp IsNot Nothing Then                                   ''~va08I~
'*            Trace.W("saveRotateAnyBMP dispose Hashcode:" & oldbmp.GetHashCode()) ''~va08I~''~v152R~
            oldbmp.Dispose()                                           ''~va08I~
        End If                                                         ''~va08I~
        swWordBMP = False    'zoom use orgBMP                          ''~v152M~
    End Sub                                                            ''~va08I~
    '*************************************************************     ''~va08I~
    Private Function drawZoomRotateAny(Pbitmap As Bitmap, Pzoom As Double) As Boolean ''~va08I~
        Try                                                            ''~va08I~
            Dim bitmapZoom As Bitmap                                   ''~va08I~
            Dim hh, ww As Integer                                      ''~va08I~
            hh = CType(Pbitmap.Height * Pzoom, Integer)                ''~va08I~
            ww = CType(Pbitmap.Width * Pzoom, Integer)                 ''~va08I~
            bitmapZoom = New Bitmap(Pbitmap, ww, hh)                   ''~va08I~
            saveRotateAnyBMP(Pbitmap)                                  ''~va08I~
            setPictureBoxImage(bitmapZoom)                             ''~va08I~
        Catch ex As Exception                                          ''~va08I~
            MessageBox.Show("Zoom by Rate:" & ex.Message)              ''~va08I~
            Return False     'Dont dispose                             ''~va08I~
        End Try                                                        ''~va08I~
        Return True 'Dispose parm bitmap                               ''~va08I~
    End Function                                                       ''~va08I~
    '**************************************************                ''~v100I~
    Private Sub setupComboBoxLang()                                    ''~v100I~
        idxLang = iOCR.setupComboBoxLang(cbLang, idxLang)                 ''~v100R~
    End Sub                                                            ''~v100I~
    '*************************************************************     ''~v@@@I~''~v100I~
    Private Sub setPictureBoxImage(Pbmp As Bitmap)                     ''~v@@@I~''~v100I~
        Dim oldbmp = PictureBox1.Image                                   ''~v@@@I~''~v100I~
        PictureBox1.Image = Pbmp                                       ''~v@@@I~''~v100I~
        '*      swRectBMP = False 'PictureBox image is not rect drawn          ''~v106I~''~v142R~
        '       If Not swWordBMP Then   'not zoom use wordBMP          ''~v106I~
        If Not swSaveRectImage Then                                    ''~v106R~
            saveRectBMP(Pbmp)                                          ''~v106I~
        End If                                                         ''~v106I~
        If oldbmp IsNot Nothing Then                                       ''~v@@@I~''~v100I~
            Try                                                        ''~v@@@I~''~v100I~
                oldbmp.Dispose()                                       ''~v@@@R~''~v100I~
            Catch ex As Exception                                      ''~v@@@I~''~v100I~
                MessageBox.Show("setPictureBoxImage dispose :" & ex.Message) ''~v@@@I~''~v100I~
            End Try                                                    ''~v@@@I~''~v100I~
            ''~v@@@I~                                                  ''~v100I~
        End If                                                         ''~v@@@I~''~v100I~
    End Sub                                                            ''~v@@@I~''~v100I~
    '*************************************************************     ''~v106I~
    Private Sub setPictureBoxImageRect(Pbmp As Bitmap, PrestoreOrg As Boolean) ''~v106I~
        swSaveRectImage = True                                         ''~v106I~
        If PrestoreOrg Then                                            ''~v106I~
            drawZoom(Pbmp, scaleNew)  'set clone to PictureBox.Image   ''~v106I~
            swRectBMP = False  'next box is not drawn,mousedown case   ''~v142I~
        Else                                                           ''~v106I~
            setPictureBoxImage(Pbmp)                                   ''~v106I~
            swRectBMP = True 'PictureBox image is rect drawn           ''~va01I~
        End If                                                         ''~v106I~
        swSaveRectImage = False                                        ''~v106I~
        '       swRectBMP = True 'PictureBox image is rect drawn               ''~v106I~''~va01R~
        '*      swWordBMP = False   'clip image is from orgBMP                 ''~v106R~''~v142R~
    End Sub                                                            ''~v106I~
    '*************************************************************     ''~v100I~
    Private Sub showStatus(Pmsg As String)                             ''~v100I~
        ToolStripStatusLabel1.Text = Pmsg                                 ''~v100I~''~v112R~
    End Sub                                                            ''~v100I~
    '*************************************************************     ''~v106I~
    '* from Form15                                                     ''~v106I~
    Public Sub receivePartialText(Ptext As String)                     ''~v106I~
        Image2TextPartial(Ptext)                                       ''~v106I~
    End Sub                                                            ''~v106I~
    '*************************************************************     ''~v106I~
    Private Sub PBmouseDown(e As MouseEventArgs)                       ''~v106I~
        If bmpForRect Is Nothing Then                                  ''~v106I~
            Exit Sub              'not yet open image file             ''~v106I~
        End If                                                         ''~v106I~
        Dim bmprect As Bitmap = bmpForRect 'rectangle free bitmap      ''~v106I~
        '*      Trace.W("PBmouseDown bmpForRect=" & bmprect.ToString() & ",swWordBMP=" & swWordBMP) ''~v106I~''~va06R~
        iIC.mouseDown(e, bmprect)                                      ''~v106I~
        setPictureBoxImageRect(bmprect, True) 'clear old box           ''~v106R~
    End Sub                                                            ''~v106I~
    '*************************************************************     ''~v106I~
    Private Sub PBmouseUp(e As MouseEventArgs)                         ''~v106I~
        Dim bmprect As Bitmap = Nothing 'output from mouseUp           ''~v106I~
        Dim rc As Boolean = iIC.mouseUp(e, bmprect, clipRect) 'get clipRect and clip drawn bmp''~v106I~
        If rc Then  'rectangle drawn                                   ''~v106I~
            setPictureBoxImageRect(bmprect, False)                     ''~v106R~
        End If                                                         ''~v106I~
    End Sub                                                            ''~v106I~
    '*************************************************************     ''~v106I~
    Private Sub PBmouseMove(e As MouseEventArgs)                       ''~v106I~
        Dim bmprect As Bitmap = Nothing 'output from mouseMove         ''~v106I~
        Dim rc As Boolean = iIC.mouseMove(e, bmprect, clipRect)        ''~v106I~
        If rc Then  'rectangle drawn                                   ''~v106I~
            setPictureBoxImageRect(bmprect, False)                     ''~v106R~
        End If                                                         ''~v106I~
    End Sub                                                            ''~v106I~
    '*************************************************************     ''~v106I~
    Private Sub showPartialText(Ptext As String)                       ''~v106I~
        '       Dim swNew As Boolean = False                                   ''~v113R~
        '       If formClip Is Nothing OrElse formClip.IsDisposed Then         ''~v106I~''~v113R~
        '           formClip = New Form15(Me)                                  ''~v106I~''~v113R~
        '           swNew = True                                               ''~v113R~
        '       End If                                                         ''~v106I~''~v113R~
        Dim swNew As Boolean = newClipForm()                           ''~v113R~
        formClip.setText(Ptext)                                        ''~v106I~
        '       If Not swNew Then                                              ''~v113R~
        '           Form1.showTop(formClip)                                    ''~v113R~
        '       End If                                                         ''~v113R~
        Form1.showForm(formClip, swNew)                                 ''~v113R~
    End Sub                                                            ''~v106I~
    '*************************************************************     ''~v112R~
    Private Sub loadMRUList()                                          ''~v112R~
        Dim item As ToolStripMenuItem = ToolStripMenuItemFile          ''~v112R~
        Form1.MainForm.loadMRUListForm2(item)                          ''~v112R~
    End Sub                                                            ''~v112R~
    '*************************************************************     ''~v112I~
    'from Form1 when menuitem selected                                 ''~v112I~
    Public Sub updateMRUList()                                         ''~v112I~
        loadMRUList()           'MRUList(1) is update,propagate to form2''~v112I~
    End Sub                                                            ''~v112I~
    '*************************************************************     ''~v113I~
    Private Function newClipForm() As Boolean                          ''~v113I~
        Dim swNew As Boolean = Form1.newForm(formClip, formClip)        ''~v113I~
        formClip.setParent(Me) 'newForm support only New(void)         ''~v113I~
        Return swNew                                                   ''~v113I~
    End Function                                                       ''~v113I~
    '*************************************************************     ''~va04R~
    Private Sub saveImage()                                            ''~va04R~
        Dim fnm As String = imageFilename                              ''~va04R~
        If fnm Is Nothing OrElse fnm.Length = 0 Then                   ''~va04R~
            Exit Sub                                                   ''~va04R~
        End If                                                         ''~va04R~
        Dim base As String = Nothing, ext As String = Nothing                                        ''~va04R~
        getFileNameExt(fnm, base, ext)                                 ''~va04R~
        Dim dlg As SaveFileDialog = SaveFileDialogImage                ''~va04R~
        dlg.Filter = imageSaveFileFilter                               ''~va04R~
        dlg.FileName = base & "-clip"                                  ''~va04R~
        '       dlg.AddExtension = True   'add extension if missing    ''~va04R~
        dlg.DefaultExt = ext                                           ''~va04R~
        imageSaveFilterIndex = getSaveFilterIndex(imageSaveFilterIndex, imageSaveFileFilter, ext) ''~va06I~
        dlg.FilterIndex = imageSaveFilterIndex                         ''~va04R~
        If dlg.ShowDialog() = DialogResult.OK Then                     ''~va04R~
            fnm = dlg.FileName                                         ''~va04R~
            imageSaveFilterIndex = dlg.FilterIndex                     ''~va04R~
            '           insertMRUList(1, fnm)      '1:imagefile        ''~va04R~
            getFileNameExt(fnm, base, ext)                             ''~va04R~
            Dim fnmsaved As String = SaveImage(base, ext)                ''~va04R~
            If fnmsaved.Length <> 0 Then                                      ''~va04I~
                Form1.MainForm.isrtMRUListSaveImage(fnmsaved)           ''~va04I~
                If swRectBMP Then                                           ''~va04I~
                    showStatus(My.Resources.STR_MSG_INFO_IMAGESAVE_PARTIAL & fnmsaved) ''~va04R~
                Else                                                   ''~va04I~
                    showStatus(My.Resources.STR_MSG_INFO_IMAGESAVE_FULL & fnmsaved) ''~va04R~
                End If                                                 ''~va04I~
            End If                                                     ''~va04I~
        End If                                                         ''~va04R~
    End Sub                                                            ''~va04R~
    Private Function SaveImage(Pfnm As String, Pfmt As String) As String ''~va04R~
        '*      Dim fnm As String = iOCR.saveImage(Pfnm, Pfmt, swRectBMP, orgBMP, scaleNew, clipRect) ''~va04R~''~v150R~
        Dim bmp As Bitmap                                               ''~v150I~
        If ctrDegree <> 0 Then                                                ''~v150I~
            bmp = rotateAnyBMP                                           ''~v150I~
        Else                                                           ''~v150I~
            bmp = orgBMP                                                 ''~v150I~
        End If                                                         ''~v150I~
        Dim fnm As String = iOCR.saveImage(Pfnm, Pfmt, swRectBMP, bmp, scaleNew, clipRect) ''~v150R~
        Return fnm                                                     ''~va04I~
    End Function                                                       ''~va04R~
    Private Function getFileNameExt(Pfnm As String, ByRef Pppath As String, ByRef Ppext As String) As Boolean ''~va04R~
        If Pfnm Is Nothing OrElse Pfnm.Length = 0 Then                 ''~va04R~
            Return False                                               ''~va04R~
        End If                                                         ''~va04R~
        Dim ext As String = System.IO.Path.GetExtension(Pfnm)          ''~va04R~
        Dim other As String = Pfnm                                     ''~va04R~
        If ext Is Nothing Or ext.Length = 0 Then                       ''~va04R~
            ext = ""                                                   ''~va04R~
        Else                                                           ''~va04R~
            other = Pfnm.Substring(0, Pfnm.Length - ext.Length)        ''~va04R~
            ext = ext.Substring(1, ext.Length - 1)                     ''~va04R~
        End If                                                         ''~va04R~
        Pppath = other                                                 ''~va04R~
        Ppext = ext                                                    ''~va04R~
        Return True                                                    ''~va04R~
    End Function                                                       ''~va04R~
    Private Function getSaveFilterIndex(Poldidx as Integer,PstrFilter as String,Pext as String) as Integer''~va06I~
    '* Bitmap|*.bmp|Jpeg|*.jpg|Png|*.png|Tiff|*.tif|Icon|*.ico|All Files|*.*"''~va06I~
    	Dim idx as Integer=Poldidx                                     ''~va06I~
        Dim fmt As Imaging.ImageFormat = iOCR.str2Fmt(Pext)            ''~va06I~
        Dim ext As String = iOCR.getImageFormat(fmt)                   ''~va06I~
        Dim pos as Integer=PstrFilter.indexOf(ext)                     ''~va06I~
        if pos>0                                                       ''~va06I~
        	Dim idx2 as Integer=0                                      ''~va06I~
            For ii As Integer = 0 To pos                               ''~va06I~
                If PstrFilter.Chars(ii) = "|"c Then                    ''~va06I~
                    idx2 += 1                                          ''~va06I~
                End If                                                 ''~va06I~
            Next                                                       ''~va06I~
            idx =CType((idx2+1)/2,Integer)                             ''~va06I~
        end if                                                         ''~va06I~
        return idx                                                     ''~va06I~
    End Function                                                       ''~va06I~
End Class