<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class FormMS
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormMS))
        Me.TextBoxOutputTitle = New System.Windows.Forms.TextBox()
        Me.TextBoxOutputName = New System.Windows.Forms.TextBox()
        Me.TextBoxInputTitle = New System.Windows.Forms.TextBox()
        Me.TextBoxInputFiles = New System.Windows.Forms.TextBox()
        Me.MenuStrip1 = New System.Windows.Forms.MenuStrip()
        Me.ToolStripComboBoxLang = New System.Windows.Forms.ToolStripComboBox()
        Me.ToolStripMenuItemExtract = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuHelp = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemHelp = New System.Windows.Forms.ToolStripMenuItem()
        Me.MenuStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'TextBoxOutputTitle
        '
        resources.ApplyResources(Me.TextBoxOutputTitle, "TextBoxOutputTitle")
        Me.TextBoxOutputTitle.Name = "TextBoxOutputTitle"
        Me.TextBoxOutputTitle.ReadOnly = True
        '
        'TextBoxOutputName
        '
        Me.TextBoxOutputName.BackColor = System.Drawing.Color.FromArgb(CType(CType(192, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(192, Byte), Integer))
        resources.ApplyResources(Me.TextBoxOutputName, "TextBoxOutputName")
        Me.TextBoxOutputName.Name = "TextBoxOutputName"
        '
        'TextBoxInputTitle
        '
        resources.ApplyResources(Me.TextBoxInputTitle, "TextBoxInputTitle")
        Me.TextBoxInputTitle.Name = "TextBoxInputTitle"
        Me.TextBoxInputTitle.ReadOnly = True
        '
        'TextBoxInputFiles
        '
        Me.TextBoxInputFiles.BackColor = System.Drawing.SystemColors.GradientInactiveCaption
        resources.ApplyResources(Me.TextBoxInputFiles, "TextBoxInputFiles")
        Me.TextBoxInputFiles.Name = "TextBoxInputFiles"
        '
        'MenuStrip1
        '
        resources.ApplyResources(Me.MenuStrip1, "MenuStrip1")
        Me.MenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripComboBoxLang, Me.ToolStripMenuItemExtract, Me.ToolStripMenuHelp, Me.ToolStripMenuItemHelp})
        Me.MenuStrip1.Name = "MenuStrip1"
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
        'ToolStripMenuHelp
        '
        resources.ApplyResources(Me.ToolStripMenuHelp, "ToolStripMenuHelp")
        Me.ToolStripMenuHelp.Name = "ToolStripMenuHelp"
        '
        'ToolStripMenuItemHelp
        '
        Me.ToolStripMenuItemHelp.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripMenuItemHelp, "ToolStripMenuItemHelp")
        Me.ToolStripMenuItemHelp.Name = "ToolStripMenuItemHelp"
        '
        'FormMS
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.TextBoxInputFiles)
        Me.Controls.Add(Me.TextBoxInputTitle)
        Me.Controls.Add(Me.TextBoxOutputName)
        Me.Controls.Add(Me.TextBoxOutputTitle)
        Me.Controls.Add(Me.MenuStrip1)
        Me.Name = "FormMS"
        Me.MenuStrip1.ResumeLayout(False)
        Me.MenuStrip1.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents TextBoxOutputTitle As TextBox
    Friend WithEvents TextBoxOutputName As TextBox
    Friend WithEvents TextBoxInputTitle As TextBox
    Friend WithEvents TextBoxInputFiles As TextBox
    Friend WithEvents MenuStrip1 As MenuStrip
    Friend WithEvents ToolStripMenuHelp As ToolStripMenuItem
    Friend WithEvents ToolStripComboBoxLang As ToolStripComboBox
    Friend WithEvents ToolStripMenuItemExtract As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItemHelp As ToolStripMenuItem
End Class
