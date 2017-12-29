<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Form14
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Form14))
        Me.SaveFileDialog1 = New System.Windows.Forms.SaveFileDialog()
        Me.OpenFileDialog1 = New System.Windows.Forms.OpenFileDialog()
        Me.PanelButtons = New System.Windows.Forms.Panel()
        Me.ButtonHelp = New System.Windows.Forms.Button()
        Me.ButtonCancel = New System.Windows.Forms.Button()
        Me.ButtonDefault = New System.Windows.Forms.Button()
        Me.ButtonOK = New System.Windows.Forms.Button()
        Me.MenuStrip1 = New System.Windows.Forms.MenuStrip()
        Me.OpenToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SaveToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SaveAsToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CutToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CopyToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.PasteToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemSwitchTarget = New System.Windows.Forms.ToolStripMenuItem()
        Me.PanelDGV = New System.Windows.Forms.Panel()
        Me.DataGridViewSymbol = New System.Windows.Forms.DataGridView()
        Me.ColumnEnable = New System.Windows.Forms.DataGridViewCheckBoxColumn()
        Me.ColumnKey = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.ColumnSend = New System.Windows.Forms.DataGridViewButtonColumn()
        Me.ColumnSymbol = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.ColumnComment = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.CommentE = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.ColumnDel = New System.Windows.Forms.DataGridViewCheckBoxColumn()
        Me.StatusStrip1 = New System.Windows.Forms.StatusStrip()
        Me.ToolStripStatusLabel1 = New System.Windows.Forms.ToolStripStatusLabel()
        Me.PanelMenu = New System.Windows.Forms.Panel()
        Me.PanelStatuSButton = New System.Windows.Forms.Panel()
        Me.PanelMiddle = New System.Windows.Forms.Panel()
        Me.PanelButtons.SuspendLayout()
        Me.MenuStrip1.SuspendLayout()
        Me.PanelDGV.SuspendLayout()
        CType(Me.DataGridViewSymbol, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.StatusStrip1.SuspendLayout()
        Me.PanelMenu.SuspendLayout()
        Me.PanelStatuSButton.SuspendLayout()
        Me.PanelMiddle.SuspendLayout()
        Me.SuspendLayout()
        '
        'OpenFileDialog1
        '
        Me.OpenFileDialog1.FileName = "OpenFileDialog1"
        '
        'PanelButtons
        '
        Me.PanelButtons.Controls.Add(Me.ButtonHelp)
        Me.PanelButtons.Controls.Add(Me.ButtonCancel)
        Me.PanelButtons.Controls.Add(Me.ButtonDefault)
        Me.PanelButtons.Controls.Add(Me.ButtonOK)
        resources.ApplyResources(Me.PanelButtons, "PanelButtons")
        Me.PanelButtons.Name = "PanelButtons"
        '
        'ButtonHelp
        '
        resources.ApplyResources(Me.ButtonHelp, "ButtonHelp")
        Me.ButtonHelp.Name = "ButtonHelp"
        Me.ButtonHelp.UseVisualStyleBackColor = True
        '
        'ButtonCancel
        '
        resources.ApplyResources(Me.ButtonCancel, "ButtonCancel")
        Me.ButtonCancel.Name = "ButtonCancel"
        Me.ButtonCancel.UseVisualStyleBackColor = True
        '
        'ButtonDefault
        '
        resources.ApplyResources(Me.ButtonDefault, "ButtonDefault")
        Me.ButtonDefault.Name = "ButtonDefault"
        Me.ButtonDefault.UseVisualStyleBackColor = True
        '
        'ButtonOK
        '
        resources.ApplyResources(Me.ButtonOK, "ButtonOK")
        Me.ButtonOK.Name = "ButtonOK"
        Me.ButtonOK.UseVisualStyleBackColor = True
        '
        'MenuStrip1
        '
        resources.ApplyResources(Me.MenuStrip1, "MenuStrip1")
        Me.MenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.OpenToolStripMenuItem, Me.SaveToolStripMenuItem, Me.SaveAsToolStripMenuItem, Me.CutToolStripMenuItem, Me.CopyToolStripMenuItem, Me.PasteToolStripMenuItem, Me.ToolStripMenuItemSwitchTarget})
        Me.MenuStrip1.Name = "MenuStrip1"
        '
        'OpenToolStripMenuItem
        '
        Me.OpenToolStripMenuItem.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.OpenToolStripMenuItem, "OpenToolStripMenuItem")
        Me.OpenToolStripMenuItem.Name = "OpenToolStripMenuItem"
        '
        'SaveToolStripMenuItem
        '
        Me.SaveToolStripMenuItem.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.SaveToolStripMenuItem, "SaveToolStripMenuItem")
        Me.SaveToolStripMenuItem.Name = "SaveToolStripMenuItem"
        '
        'SaveAsToolStripMenuItem
        '
        Me.SaveAsToolStripMenuItem.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.SaveAsToolStripMenuItem, "SaveAsToolStripMenuItem")
        Me.SaveAsToolStripMenuItem.Name = "SaveAsToolStripMenuItem"
        '
        'CutToolStripMenuItem
        '
        Me.CutToolStripMenuItem.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.CutToolStripMenuItem, "CutToolStripMenuItem")
        Me.CutToolStripMenuItem.Name = "CutToolStripMenuItem"
        '
        'CopyToolStripMenuItem
        '
        Me.CopyToolStripMenuItem.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.CopyToolStripMenuItem, "CopyToolStripMenuItem")
        Me.CopyToolStripMenuItem.Name = "CopyToolStripMenuItem"
        '
        'PasteToolStripMenuItem
        '
        Me.PasteToolStripMenuItem.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.PasteToolStripMenuItem, "PasteToolStripMenuItem")
        Me.PasteToolStripMenuItem.Name = "PasteToolStripMenuItem"
        '
        'ToolStripMenuItemSwitchTarget
        '
        Me.ToolStripMenuItemSwitchTarget.Name = "ToolStripMenuItemSwitchTarget"
        resources.ApplyResources(Me.ToolStripMenuItemSwitchTarget, "ToolStripMenuItemSwitchTarget")
        '
        'PanelDGV
        '
        Me.PanelDGV.Controls.Add(Me.DataGridViewSymbol)
        resources.ApplyResources(Me.PanelDGV, "PanelDGV")
        Me.PanelDGV.Name = "PanelDGV"
        '
        'DataGridViewSymbol
        '
        Me.DataGridViewSymbol.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.DataGridViewSymbol.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.ColumnEnable, Me.ColumnKey, Me.ColumnSend, Me.ColumnSymbol, Me.ColumnComment, Me.CommentE, Me.ColumnDel})
        resources.ApplyResources(Me.DataGridViewSymbol, "DataGridViewSymbol")
        Me.DataGridViewSymbol.Name = "DataGridViewSymbol"
        Me.DataGridViewSymbol.RowTemplate.Height = 21
        '
        'ColumnEnable
        '
        resources.ApplyResources(Me.ColumnEnable, "ColumnEnable")
        Me.ColumnEnable.Name = "ColumnEnable"
        Me.ColumnEnable.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
        '
        'ColumnKey
        '
        resources.ApplyResources(Me.ColumnKey, "ColumnKey")
        Me.ColumnKey.Name = "ColumnKey"
        Me.ColumnKey.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'ColumnSend
        '
        resources.ApplyResources(Me.ColumnSend, "ColumnSend")
        Me.ColumnSend.Name = "ColumnSend"
        Me.ColumnSend.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
        Me.ColumnSend.Text = "←"
        Me.ColumnSend.UseColumnTextForButtonValue = True
        '
        'ColumnSymbol
        '
        resources.ApplyResources(Me.ColumnSymbol, "ColumnSymbol")
        Me.ColumnSymbol.Name = "ColumnSymbol"
        Me.ColumnSymbol.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'ColumnComment
        '
        resources.ApplyResources(Me.ColumnComment, "ColumnComment")
        Me.ColumnComment.Name = "ColumnComment"
        Me.ColumnComment.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'CommentE
        '
        resources.ApplyResources(Me.CommentE, "CommentE")
        Me.CommentE.Name = "CommentE"
        Me.CommentE.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'ColumnDel
        '
        resources.ApplyResources(Me.ColumnDel, "ColumnDel")
        Me.ColumnDel.Name = "ColumnDel"
        Me.ColumnDel.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
        '
        'StatusStrip1
        '
        resources.ApplyResources(Me.StatusStrip1, "StatusStrip1")
        Me.StatusStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripStatusLabel1})
        Me.StatusStrip1.Name = "StatusStrip1"
        '
        'ToolStripStatusLabel1
        '
        Me.ToolStripStatusLabel1.Name = "ToolStripStatusLabel1"
        resources.ApplyResources(Me.ToolStripStatusLabel1, "ToolStripStatusLabel1")
        '
        'PanelMenu
        '
        Me.PanelMenu.Controls.Add(Me.MenuStrip1)
        resources.ApplyResources(Me.PanelMenu, "PanelMenu")
        Me.PanelMenu.Name = "PanelMenu"
        '
        'PanelStatuSButton
        '
        Me.PanelStatuSButton.Controls.Add(Me.StatusStrip1)
        Me.PanelStatuSButton.Controls.Add(Me.PanelButtons)
        resources.ApplyResources(Me.PanelStatuSButton, "PanelStatuSButton")
        Me.PanelStatuSButton.Name = "PanelStatuSButton"
        '
        'PanelMiddle
        '
        Me.PanelMiddle.Controls.Add(Me.PanelDGV)
        resources.ApplyResources(Me.PanelMiddle, "PanelMiddle")
        Me.PanelMiddle.Name = "PanelMiddle"
        '
        'Form14
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = Global.WindowsApplication1.My.MySettings.Default.CFG_Form14ClientSize
        Me.Controls.Add(Me.PanelMiddle)
        Me.Controls.Add(Me.PanelStatuSButton)
        Me.Controls.Add(Me.PanelMenu)
        Me.DataBindings.Add(New System.Windows.Forms.Binding("ClientSize", Global.WindowsApplication1.My.MySettings.Default, "CFG_Form14ClientSize", True, System.Windows.Forms.DataSourceUpdateMode.OnPropertyChanged))
        Me.MainMenuStrip = Me.MenuStrip1
        Me.MinimizeBox = False
        Me.Name = "Form14"
        Me.PanelButtons.ResumeLayout(False)
        Me.MenuStrip1.ResumeLayout(False)
        Me.MenuStrip1.PerformLayout()
        Me.PanelDGV.ResumeLayout(False)
        CType(Me.DataGridViewSymbol, System.ComponentModel.ISupportInitialize).EndInit()
        Me.StatusStrip1.ResumeLayout(False)
        Me.StatusStrip1.PerformLayout()
        Me.PanelMenu.ResumeLayout(False)
        Me.PanelMenu.PerformLayout()
        Me.PanelStatuSButton.ResumeLayout(False)
        Me.PanelStatuSButton.PerformLayout()
        Me.PanelMiddle.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents SaveFileDialog1 As System.Windows.Forms.SaveFileDialog
    Friend WithEvents OpenFileDialog1 As System.Windows.Forms.OpenFileDialog
    Friend WithEvents PanelButtons As System.Windows.Forms.Panel
    Friend WithEvents ButtonHelp As System.Windows.Forms.Button
    Friend WithEvents ButtonCancel As System.Windows.Forms.Button
    Friend WithEvents ButtonDefault As System.Windows.Forms.Button
    Friend WithEvents ButtonOK As System.Windows.Forms.Button
    Friend WithEvents MenuStrip1 As System.Windows.Forms.MenuStrip
    Friend WithEvents OpenToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents SaveToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents SaveAsToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents CutToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents CopyToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents PasteToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents PanelDGV As System.Windows.Forms.Panel
    Friend WithEvents DataGridViewSymbol As System.Windows.Forms.DataGridView
    Friend WithEvents StatusStrip1 As System.Windows.Forms.StatusStrip
    Friend WithEvents ToolStripStatusLabel1 As System.Windows.Forms.ToolStripStatusLabel
    Friend WithEvents PanelMenu As System.Windows.Forms.Panel
    Friend WithEvents PanelStatuSButton As System.Windows.Forms.Panel
    Friend WithEvents PanelMiddle As System.Windows.Forms.Panel
    Friend WithEvents ColumnEnable As System.Windows.Forms.DataGridViewCheckBoxColumn
    Friend WithEvents ColumnKey As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents ColumnSend As System.Windows.Forms.DataGridViewButtonColumn
    Friend WithEvents ColumnSymbol As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents ColumnComment As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents CommentE As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents ColumnDel As System.Windows.Forms.DataGridViewCheckBoxColumn
    Friend WithEvents ToolStripMenuItemSwitchTarget As ToolStripMenuItem
End Class
