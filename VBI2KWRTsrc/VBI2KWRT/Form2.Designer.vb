<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class Form2
    Inherits System.Windows.Forms.Form

    'フォームがコンポーネントの一覧をクリーンアップするために dispose をオーバーライドします。
    <System.Diagnostics.DebuggerNonUserCode()>
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Windows フォーム デザイナーで必要です。
    Private components As System.ComponentModel.IContainer

    'メモ: 以下のプロシージャは Windows フォーム デザイナーで必要です。
    'Windows フォーム デザイナーを使用して変更できます。  
    'コード エディターを使って変更しないでください。
    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Form2))
        Me.MenuStrip1 = New System.Windows.Forms.MenuStrip()
        Me.ToolStripMenuItemFile = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemZoomIn = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemZoomOut = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemRotateLeft = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemRotateRight = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripComboBoxLang = New System.Windows.Forms.ToolStripComboBox()
        Me.ToolStripMenuItemExtract = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemSaveImage = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemHelp = New System.Windows.Forms.ToolStripMenuItem()
        Me.StatusStrip1 = New System.Windows.Forms.StatusStrip()
        Me.ToolStripStatusLabel1 = New System.Windows.Forms.ToolStripStatusLabel()
        Me.PanelPictureBox = New System.Windows.Forms.Panel()
        Me.PictureBox1 = New System.Windows.Forms.PictureBox()
        Me.SaveFileDialogImage = New System.Windows.Forms.SaveFileDialog()
        Me.ToolStripMenuItemDegree1 = New System.Windows.Forms.ToolStripMenuItem()
        Me.MenuStrip1.SuspendLayout()
        Me.StatusStrip1.SuspendLayout()
        Me.PanelPictureBox.SuspendLayout()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'MenuStrip1
        '
        Me.MenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripMenuItemFile, Me.ToolStripMenuItemZoomIn, Me.ToolStripMenuItemZoomOut, Me.ToolStripMenuItemDegree1, Me.ToolStripMenuItemRotateLeft, Me.ToolStripMenuItemRotateRight, Me.ToolStripComboBoxLang, Me.ToolStripMenuItemExtract, Me.ToolStripMenuItemSaveImage, Me.ToolStripMenuItemHelp})
        resources.ApplyResources(Me.MenuStrip1, "MenuStrip1")
        Me.MenuStrip1.Name = "MenuStrip1"
        '
        'ToolStripMenuItemFile
        '
        Me.ToolStripMenuItemFile.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripMenuItemFile, "ToolStripMenuItemFile")
        Me.ToolStripMenuItemFile.Name = "ToolStripMenuItemFile"
        '
        'ToolStripMenuItemZoomIn
        '
        Me.ToolStripMenuItemZoomIn.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripMenuItemZoomIn, "ToolStripMenuItemZoomIn")
        Me.ToolStripMenuItemZoomIn.Name = "ToolStripMenuItemZoomIn"
        '
        'ToolStripMenuItemZoomOut
        '
        Me.ToolStripMenuItemZoomOut.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripMenuItemZoomOut, "ToolStripMenuItemZoomOut")
        Me.ToolStripMenuItemZoomOut.Name = "ToolStripMenuItemZoomOut"
        '
        'ToolStripMenuItemRotateLeft
        '
        Me.ToolStripMenuItemRotateLeft.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripMenuItemRotateLeft, "ToolStripMenuItemRotateLeft")
        Me.ToolStripMenuItemRotateLeft.Name = "ToolStripMenuItemRotateLeft"
        '
        'ToolStripMenuItemRotateRight
        '
        Me.ToolStripMenuItemRotateRight.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripMenuItemRotateRight, "ToolStripMenuItemRotateRight")
        Me.ToolStripMenuItemRotateRight.Name = "ToolStripMenuItemRotateRight"
        '
        'ToolStripComboBoxLang
        '
        Me.ToolStripComboBoxLang.Name = "ToolStripComboBoxLang"
        resources.ApplyResources(Me.ToolStripComboBoxLang, "ToolStripComboBoxLang")
        '
        'ToolStripMenuItemExtract
        '
        Me.ToolStripMenuItemExtract.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripMenuItemExtract, "ToolStripMenuItemExtract")
        Me.ToolStripMenuItemExtract.Name = "ToolStripMenuItemExtract"
        '
        'ToolStripMenuItemSaveImage
        '
        Me.ToolStripMenuItemSaveImage.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripMenuItemSaveImage, "ToolStripMenuItemSaveImage")
        Me.ToolStripMenuItemSaveImage.Name = "ToolStripMenuItemSaveImage"
        '
        'ToolStripMenuItemHelp
        '
        Me.ToolStripMenuItemHelp.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripMenuItemHelp, "ToolStripMenuItemHelp")
        Me.ToolStripMenuItemHelp.Name = "ToolStripMenuItemHelp"
        '
        'StatusStrip1
        '
        Me.StatusStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripStatusLabel1})
        resources.ApplyResources(Me.StatusStrip1, "StatusStrip1")
        Me.StatusStrip1.Name = "StatusStrip1"
        '
        'ToolStripStatusLabel1
        '
        Me.ToolStripStatusLabel1.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text
        Me.ToolStripStatusLabel1.Name = "ToolStripStatusLabel1"
        resources.ApplyResources(Me.ToolStripStatusLabel1, "ToolStripStatusLabel1")
        '
        'PanelPictureBox
        '
        resources.ApplyResources(Me.PanelPictureBox, "PanelPictureBox")
        Me.PanelPictureBox.BackColor = System.Drawing.SystemColors.ActiveCaption
        Me.PanelPictureBox.Controls.Add(Me.PictureBox1)
        Me.PanelPictureBox.Name = "PanelPictureBox"
        '
        'PictureBox1
        '
        resources.ApplyResources(Me.PictureBox1, "PictureBox1")
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.TabStop = False
        '
        'ToolStripMenuItemDegree1
        '
        Me.ToolStripMenuItemDegree1.BackColor = System.Drawing.SystemColors.ControlDarkDark
        Me.ToolStripMenuItemDegree1.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text
        Me.ToolStripMenuItemDegree1.Name = "ToolStripMenuItemDegree1"
        resources.ApplyResources(Me.ToolStripMenuItemDegree1, "ToolStripMenuItemDegree1")
        '
        'Form2
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.PanelPictureBox)
        Me.Controls.Add(Me.StatusStrip1)
        Me.Controls.Add(Me.MenuStrip1)
        Me.MainMenuStrip = Me.MenuStrip1
        Me.Name = "Form2"
        Me.MenuStrip1.ResumeLayout(False)
        Me.MenuStrip1.PerformLayout()
        Me.StatusStrip1.ResumeLayout(False)
        Me.StatusStrip1.PerformLayout()
        Me.PanelPictureBox.ResumeLayout(False)
        Me.PanelPictureBox.PerformLayout()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents MenuStrip1 As MenuStrip
    Friend WithEvents StatusStrip1 As StatusStrip
    Friend WithEvents PanelPictureBox As Panel
    Friend WithEvents PictureBox1 As PictureBox
    Friend WithEvents ToolStripMenuItemFile As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemZoomIn As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemZoomOut As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemRotateLeft As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemRotateRight As ToolStripMenuItem
    Friend WithEvents ToolStripComboBoxLang As ToolStripComboBox
    Friend WithEvents ToolStripMenuItemExtract As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemHelp As ToolStripMenuItem
    Friend WithEvents ToolStripStatusLabel1 As ToolStripStatusLabel
    Friend WithEvents ToolStripMenuItemSaveImage As ToolStripMenuItem
    Friend WithEvents SaveFileDialogImage As SaveFileDialog
    Friend WithEvents ToolStripMenuItemDegree1 As ToolStripMenuItem
End Class
