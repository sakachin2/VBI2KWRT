<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Form15
    Inherits System.Windows.Forms.Form

    'フォームがコンポーネントの一覧をクリーンアップするために dispose をオーバーライドします。
    <System.Diagnostics.DebuggerNonUserCode()> _
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
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Form15))
        Me.ToolStrip1 = New System.Windows.Forms.ToolStrip()
        Me.ToolStripButtonOK = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripSeparator2 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripButtonHelp = New System.Windows.Forms.ToolStripButton()
        Me.PanelTextBox = New System.Windows.Forms.Panel()
        Me.TextBox = New System.Windows.Forms.TextBox()
        Me.ToolStrip1.SuspendLayout()
        Me.PanelTextBox.SuspendLayout()
        Me.SuspendLayout()
        '
        'ToolStrip1
        '
        Me.ToolStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripButtonOK, Me.ToolStripSeparator1, Me.ToolStripSeparator2, Me.ToolStripButtonHelp})
        Me.ToolStrip1.Location = New System.Drawing.Point(0, 0)
        Me.ToolStrip1.Name = "ToolStrip1"
        Me.ToolStrip1.Size = New System.Drawing.Size(438, 39)
        Me.ToolStrip1.TabIndex = 0
        Me.ToolStrip1.Text = "ToolStrip1"
        '
        'ToolStripButtonOK
        '
        Me.ToolStripButtonOK.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text
        Me.ToolStripButtonOK.Image = CType(resources.GetObject("ToolStripButtonOK.Image"), System.Drawing.Image)
        Me.ToolStripButtonOK.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None
        Me.ToolStripButtonOK.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ToolStripButtonOK.Name = "ToolStripButtonOK"
        Me.ToolStripButtonOK.Size = New System.Drawing.Size(39, 36)
        Me.ToolStripButtonOK.Text = "  OK  "
        '
        'ToolStripSeparator1
        '
        Me.ToolStripSeparator1.Name = "ToolStripSeparator1"
        Me.ToolStripSeparator1.Size = New System.Drawing.Size(6, 39)
        '
        'ToolStripSeparator2
        '
        Me.ToolStripSeparator2.Name = "ToolStripSeparator2"
        Me.ToolStripSeparator2.Size = New System.Drawing.Size(6, 39)
        '
        'ToolStripButtonHelp
        '
        Me.ToolStripButtonHelp.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButtonHelp.Image = CType(resources.GetObject("ToolStripButtonHelp.Image"), System.Drawing.Image)
        Me.ToolStripButtonHelp.ImageScaling = System.Windows.Forms.ToolStripItemImageScaling.None
        Me.ToolStripButtonHelp.ImageTransparentColor = System.Drawing.Color.Magenta
        Me.ToolStripButtonHelp.Name = "ToolStripButtonHelp"
        Me.ToolStripButtonHelp.Size = New System.Drawing.Size(36, 36)
        Me.ToolStripButtonHelp.Text = "ToolStripButton1"
        '
        'PanelTextBox
        '
        Me.PanelTextBox.AutoScroll = True
        Me.PanelTextBox.Controls.Add(Me.TextBox)
        Me.PanelTextBox.Dock = System.Windows.Forms.DockStyle.Fill
        Me.PanelTextBox.Location = New System.Drawing.Point(0, 39)
        Me.PanelTextBox.Name = "PanelTextBox"
        Me.PanelTextBox.Size = New System.Drawing.Size(438, 222)
        Me.PanelTextBox.TabIndex = 1
        '
        'TextBox
        '
        Me.TextBox.BackColor = System.Drawing.Color.FromArgb(CType(CType(192, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        Me.TextBox.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TextBox.Location = New System.Drawing.Point(0, 0)
        Me.TextBox.Multiline = True
        Me.TextBox.Name = "TextBox"
        Me.TextBox.ScrollBars = System.Windows.Forms.ScrollBars.Both
        Me.TextBox.Size = New System.Drawing.Size(438, 222)
        Me.TextBox.TabIndex = 0
        '
        'Form15
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 12.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = Global.WindowsApplication1.My.MySettings.Default.AS_Form15ClientSize
        Me.Controls.Add(Me.PanelTextBox)
        Me.Controls.Add(Me.ToolStrip1)
        Me.DataBindings.Add(New System.Windows.Forms.Binding("ClientSize", Global.WindowsApplication1.My.MySettings.Default, "AS_Form15ClientSize", True, System.Windows.Forms.DataSourceUpdateMode.OnPropertyChanged))
        Me.MinimizeBox = False
        Me.MinimumSize = New System.Drawing.Size(300, 200)
        Me.Name = "Form15"
        Me.Text = "ClipOutText"
        Me.ToolStrip1.ResumeLayout(False)
        Me.ToolStrip1.PerformLayout()
        Me.PanelTextBox.ResumeLayout(False)
        Me.PanelTextBox.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents ToolStrip1 As ToolStrip
    Friend WithEvents ToolStripButtonOK As ToolStripButton
    Friend WithEvents PanelTextBox As Panel
    Friend WithEvents TextBox As TextBox
    Friend WithEvents ToolStripButtonHelp As ToolStripButton
    Friend WithEvents ToolStripSeparator1 As ToolStripSeparator
    Friend WithEvents ToolStripSeparator2 As ToolStripSeparator
End Class
