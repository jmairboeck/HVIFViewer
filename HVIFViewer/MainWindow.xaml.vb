Imports System.Windows.Input
Imports Microsoft.Win32

Class MainWindow
    Public Sub New()
        InitializeComponent()
    End Sub

    Private Sub Open_Executed(sender As Object, e As ExecutedRoutedEventArgs)
        Dim fileDialog As New OpenFileDialog()
        If fileDialog.ShowDialog(Me) Then
            Dim viewModel = DirectCast(DataContext, MainWindowViewModel)
            viewModel.FileName = fileDialog.FileName
        End If
    End Sub
End Class
