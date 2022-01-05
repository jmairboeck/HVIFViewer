Imports System.ComponentModel

Public Class MainWindowViewModel
    Implements INotifyPropertyChanged

    Private _fileName As String
    Public Property FileName() As String
        Get
            Return _fileName
        End Get
        Set(value As String)
            _fileName = value
            RaiseEvent PropertyChanged(Me, New PropertyChangedEventArgs(NameOf(FileName)))
        End Set
    End Property

    Public Event PropertyChanged As PropertyChangedEventHandler Implements INotifyPropertyChanged.PropertyChanged
End Class
