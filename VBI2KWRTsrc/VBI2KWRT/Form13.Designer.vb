<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Form13
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Form13))
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.MenuStrip1 = New System.Windows.Forms.MenuStrip()
        Me.OpenToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SaveToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SaveAsToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CutToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CopyToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.PasteToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItemSwitchTarget = New System.Windows.Forms.ToolStripMenuItem()
        Me.PanelBottons = New System.Windows.Forms.Panel()
        Me.ButtonCancel = New System.Windows.Forms.Button()
        Me.ButtonHelp = New System.Windows.Forms.Button()
        Me.ButtonOK = New System.Windows.Forms.Button()
        Me.PanelDataGrid = New System.Windows.Forms.Panel()
        Me.DataGridViewWords = New System.Windows.Forms.DataGridView()
        Me.ColumnEnable = New System.Windows.Forms.DataGridViewCheckBoxColumn()
        Me.ColumnKanji = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.ColumnSend = New System.Windows.Forms.DataGridViewButtonColumn()
        Me.ColumnKana = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.ColumnDel = New System.Windows.Forms.DataGridViewCheckBoxColumn()
        Me.OpenFileDialog1 = New System.Windows.Forms.OpenFileDialog()
        Me.SaveFileDialog1 = New System.Windows.Forms.SaveFileDialog()
        Me.StatusStrip1 = New System.Windows.Forms.StatusStrip()
        Me.ToolStripStatusLabel1 = New System.Windows.Forms.ToolStripStatusLabel()
        Me.Panel1.SuspendLayout()
        Me.MenuStrip1.SuspendLayout()
        Me.PanelBottons.SuspendLayout()
        Me.PanelDataGrid.SuspendLayout()
        CType(Me.DataGridViewWords, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.StatusStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'Panel1
        '
        resources.ApplyResources(Me.Panel1, "Panel1")
        Me.Panel1.Controls.Add(Me.MenuStrip1)
        Me.Panel1.Name = "Panel1"
        '
        'MenuStrip1
        '
        resources.ApplyResources(Me.MenuStrip1, "MenuStrip1")
        Me.MenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.OpenToolStripMenuItem, Me.SaveToolStripMenuItem, Me.SaveAsToolStripMenuItem, Me.CutToolStripMenuItem, Me.CopyToolStripMenuItem, Me.PasteToolStripMenuItem, Me.ToolStripMenuItemSwitchTarget})
        Me.MenuStrip1.Name = "MenuStrip1"
        '
        'OpenToolStripMenuItem
        '
        resources.ApplyResources(Me.OpenToolStripMenuItem, "OpenToolStripMenuItem")
        Me.OpenToolStripMenuItem.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.OpenToolStripMenuItem.Name = "OpenToolStripMenuItem"
        '
        'SaveToolStripMenuItem
        '
        resources.ApplyResources(Me.SaveToolStripMenuItem, "SaveToolStripMenuItem")
        Me.SaveToolStripMenuItem.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.SaveToolStripMenuItem.Name = "SaveToolStripMenuItem"
        '
        'SaveAsToolStripMenuItem
        '
        resources.ApplyResources(Me.SaveAsToolStripMenuItem, "SaveAsToolStripMenuItem")
        Me.SaveAsToolStripMenuItem.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.SaveAsToolStripMenuItem.Name = "SaveAsToolStripMenuItem"
        '
        'CutToolStripMenuItem
        '
        resources.ApplyResources(Me.CutToolStripMenuItem, "CutToolStripMenuItem")
        Me.CutToolStripMenuItem.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.CutToolStripMenuItem.Name = "CutToolStripMenuItem"
        '
        'CopyToolStripMenuItem
        '
        resources.ApplyResources(Me.CopyToolStripMenuItem, "CopyToolStripMenuItem")
        Me.CopyToolStripMenuItem.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.CopyToolStripMenuItem.Name = "CopyToolStripMenuItem"
        '
        'PasteToolStripMenuItem
        '
        resources.ApplyResources(Me.PasteToolStripMenuItem, "PasteToolStripMenuItem")
        Me.PasteToolStripMenuItem.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.PasteToolStripMenuItem.Name = "PasteToolStripMenuItem"
        '
        'ToolStripMenuItemSwitchTarget
        '
        resources.ApplyResources(Me.ToolStripMenuItemSwitchTarget, "ToolStripMenuItemSwitchTarget")
        Me.ToolStripMenuItemSwitchTarget.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text
        Me.ToolStripMenuItemSwitchTarget.Name = "ToolStripMenuItemSwitchTarget"
        '
        'PanelBottons
        '
        resources.ApplyResources(Me.PanelBottons, "PanelBottons")
        Me.PanelBottons.Controls.Add(Me.ButtonCancel)
        Me.PanelBottons.Controls.Add(Me.ButtonHelp)
        Me.PanelBottons.Controls.Add(Me.ButtonOK)
        Me.PanelBottons.Name = "PanelBottons"
        '
        'ButtonCancel
        '
        resources.ApplyResources(Me.ButtonCancel, "ButtonCancel")
        Me.ButtonCancel.Name = "ButtonCancel"
        Me.ButtonCancel.UseVisualStyleBackColor = True
        '
        'ButtonHelp
        '
        resources.ApplyResources(Me.ButtonHelp, "ButtonHelp")
        Me.ButtonHelp.Name = "ButtonHelp"
        Me.ButtonHelp.UseVisualStyleBackColor = True
        '
        'ButtonOK
        '
        resources.ApplyResources(Me.ButtonOK, "ButtonOK")
        Me.ButtonOK.Name = "ButtonOK"
        Me.ButtonOK.UseVisualStyleBackColor = True
        '
        'PanelDataGrid
        '
        resources.ApplyResources(Me.PanelDataGrid, "PanelDataGrid")
        Me.PanelDataGrid.Controls.Add(Me.DataGridViewWords)
        Me.PanelDataGrid.Name = "PanelDataGrid"
        '
        'DataGridViewWords
        '
        resources.ApplyResources(Me.DataGridViewWords, "DataGridViewWords")
        Me.DataGridViewWords.ClipboardCopyMode = System.Windows.Forms.DataGridViewClipboardCopyMode.EnableWithoutHeaderText
        Me.DataGridViewWords.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.DataGridViewWords.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.ColumnEnable, Me.ColumnKanji, Me.ColumnSend, Me.ColumnKana, Me.ColumnDel})
        Me.DataGridViewWords.Name = "DataGridViewWords"
        Me.DataGridViewWords.RowTemplate.Height = 21
        '
        'ColumnEnable
        '
        resources.ApplyResources(Me.ColumnEnable, "ColumnEnable")
        Me.ColumnEnable.Name = "ColumnEnable"
        '
        'ColumnKanji
        '
        resources.ApplyResources(Me.ColumnKanji, "ColumnKanji")
        Me.ColumnKanji.Name = "ColumnKanji"
        Me.ColumnKanji.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
        Me.ColumnKanji.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'ColumnSend
        '
        resources.ApplyResources(Me.ColumnSend, "ColumnSend")
        Me.ColumnSend.Name = "ColumnSend"
        Me.ColumnSend.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
        Me.ColumnSend.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.Automatic
        Me.ColumnSend.Text = "←"
        Me.ColumnSend.UseColumnTextForButtonValue = True
        '
        'ColumnKana
        '
        resources.ApplyResources(Me.ColumnKana, "ColumnKana")
        Me.ColumnKana.Name = "ColumnKana"
        Me.ColumnKana.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
        Me.ColumnKana.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'ColumnDel
        '
        resources.ApplyResources(Me.ColumnDel, "ColumnDel")
        Me.ColumnDel.Name = "ColumnDel"
        '
        'OpenFileDialog1
        '
        Me.OpenFileDialog1.FileName = "OpenFileDialog1"
        resources.ApplyResources(Me.OpenFileDialog1, "OpenFileDialog1")
        '
        'SaveFileDialog1
        '
        resources.ApplyResources(Me.SaveFileDialog1, "SaveFileDialog1")
        '
        'StatusStrip1
        '
        resources.ApplyResources(Me.StatusStrip1, "StatusStrip1")
        Me.StatusStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripStatusLabel1})
        Me.StatusStrip1.Name = "StatusStrip1"
        '
        'ToolStripStatusLabel1
        '
        resources.ApplyResources(Me.ToolStripStatusLabel1, "ToolStripStatusLabel1")
        Me.ToolStripStatusLabel1.Name = "ToolStripStatusLabel1"
        '
        'Form13
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = Global.WindowsApplication1.My.MySettings.Default.CFG_Form13ClientSize
        Me.Controls.Add(Me.StatusStrip1)
        Me.Controls.Add(Me.PanelDataGrid)
        Me.Controls.Add(Me.PanelBottons)
        Me.Controls.Add(Me.Panel1)
        Me.DataBindings.Add(New System.Windows.Forms.Binding("ClientSize", Global.WindowsApplication1.My.MySettings.Default, "CFG_Form13ClientSize", True, System.Windows.Forms.DataSourceUpdateMode.OnPropertyChanged))
        Me.MainMenuStrip = Me.MenuStrip1
        Me.MinimizeBox = False
        Me.Name = "Form13"
        Me.Panel1.ResumeLayout(False)
        Me.Panel1.PerformLayout()
        Me.MenuStrip1.ResumeLayout(False)
        Me.MenuStrip1.PerformLayout()
        Me.PanelBottons.ResumeLayout(False)
        Me.PanelDataGrid.ResumeLayout(False)
        CType(Me.DataGridViewWords, System.ComponentModel.ISupportInitialize).EndInit()
        Me.StatusStrip1.ResumeLayout(False)
        Me.StatusStrip1.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents PanelBottons As System.Windows.Forms.Panel
    Friend WithEvents PanelDataGrid As System.Windows.Forms.Panel
    Friend WithEvents DataGridViewWords As System.Windows.Forms.DataGridView
    Friend WithEvents ButtonOK As System.Windows.Forms.Button
    Friend WithEvents ButtonHelp As System.Windows.Forms.Button
    Friend WithEvents MenuStrip1 As System.Windows.Forms.MenuStrip
    Friend WithEvents OpenToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents SaveToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents SaveAsToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents OpenFileDialog1 As System.Windows.Forms.OpenFileDialog
    Friend WithEvents SaveFileDialog1 As System.Windows.Forms.SaveFileDialog
    Friend WithEvents CutToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents CopyToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents PasteToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ButtonCancel As System.Windows.Forms.Button
    Friend WithEvents ColumnEnable As System.Windows.Forms.DataGridViewCheckBoxColumn
    Friend WithEvents ColumnKanji As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents ColumnSend As System.Windows.Forms.DataGridViewButtonColumn
    Friend WithEvents ColumnKana As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents ColumnDel As System.Windows.Forms.DataGridViewCheckBoxColumn
    Friend WithEvents StatusStrip1 As System.Windows.Forms.StatusStrip
    Friend WithEvents ToolStripStatusLabel1 As System.Windows.Forms.ToolStripStatusLabel
    Friend WithEvents ToolStripMenuItemSwitchTarget As ToolStripMenuItem
End Class
