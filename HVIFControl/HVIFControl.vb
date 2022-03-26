Imports System
Imports System.Collections.Generic
Imports System.IO
Imports System.Windows
Imports System.Windows.Media
Imports System.Windows.Shapes

Public Class HVIFControl
    Inherits FrameworkElement

    Public Shared ReadOnly FileNameProperty As DependencyProperty = DependencyProperty.Register(NameOf(FileName), GetType(String), GetType(HVIFControl), New FrameworkPropertyMetadata(Nothing, FrameworkPropertyMetadataOptions.AffectsRender Or FrameworkPropertyMetadataOptions.AffectsMeasure, AddressOf OnFileNameChanged))

    Public Property FileName() As String
        Get
            Return DirectCast(GetValue(FileNameProperty), String)
        End Get
        Set(value As String)
            SetValue(FileNameProperty, value)
        End Set
    End Property

    Public ReadOnly Property Styles() As List(Of Brush) = New List(Of Brush)

    Public ReadOnly Property Paths() As PathFigureCollection = New PathFigureCollection

    Public ReadOnly Property Shapes() As List(Of Shapes.Path) = New List(Of Shapes.Path)

    Private Shared Sub OnFileNameChanged(d As DependencyObject, e As DependencyPropertyChangedEventArgs)
        Dim control = DirectCast(d, HVIFControl)
        Dim value = DirectCast(e.NewValue, String)
        control.LoadFile(value)
    End Sub

    Private Sub LoadFile(value As String)
        For Each shape In Shapes
            RemoveVisualChild(shape)
        Next
        Styles.Clear()
        Paths.Clear()
        Shapes.Clear()
        Dim buffer = File.ReadAllBytes(value)
        Dim magic = BitConverter.ToInt32(buffer, 0)
        If magic <> &H6669636E Then
            Throw New Exception()
        End If
        Dim styleCount = buffer(4)
        Dim offset = 5
        Dim readCoordinate =
            Function()
                Dim result As Double
                If buffer(offset) >= 128 Then
                    result = (((buffer(offset) - 128) << 8) Or buffer(offset + 1)) / 102 - 128
                    offset += 2
                Else
                    result = CDbl(buffer(offset)) - 32
                    offset += 1
                End If
                Return result
            End Function
        Dim readFloat24 =
            Function()
                Dim shortValue = (CInt(buffer(offset)) << 16) Or (CInt(buffer(offset + 1)) << 8) Or buffer(offset + 2)
                offset += 3
                Return If(shortValue = 0, 0.0, BitConverter.ToSingle(BitConverter.GetBytes(((shortValue And &H800000) << 8) Or ((((shortValue And &H7E0000) >> 17) + 95) << 23) Or (shortValue And &H1FFFF) << 6), 0))
            End Function

        For i = 0 To styleCount - 1
            Dim styleType = CType(buffer(offset), StyleType)
            offset += 1
            Dim brush As Brush
            Select Case styleType
                Case StyleType.SolidColor
                    brush = New SolidColorBrush(Color.FromArgb(buffer(offset + 3), buffer(offset), buffer(offset + 1), buffer(offset + 2)))
                    offset += 4
                Case StyleType.Gradient
                    Dim gradientType = CType(buffer(offset), GradientType)
                    Dim gradientFlags = CType(buffer(offset + 1), GradientFlag)
                    Dim gradientStopCount = buffer(offset + 2)
                    offset += 3
                    Dim gradientBrush As GradientBrush = Nothing
                    Select Case gradientType
                        Case GradientType.Linear
                            gradientBrush = New LinearGradientBrush()
                        Case GradientType.Circular
                            gradientBrush = New RadialGradientBrush()
                        ' TODO: handle these types correctly
                        Case GradientType.Diamond
                            gradientBrush = New LinearGradientBrush()
                        Case GradientType.Conic
                            gradientBrush = New LinearGradientBrush()
                        Case GradientType.Xy
                            gradientBrush = New LinearGradientBrush()
                        Case GradientType.SqrtXy
                            gradientBrush = New LinearGradientBrush()
                    End Select
                    If gradientFlags.HasFlag(GradientFlag.Transform) Then
                        gradientBrush.Transform = New MatrixTransform(readFloat24(), readFloat24(), readFloat24(), readFloat24(), readFloat24(), readFloat24())
                    End If
                    Dim greenOffset = If(gradientFlags.HasFlag(GradientFlag.Grays), 1, 2)
                    Dim blueOffset = If(gradientFlags.HasFlag(GradientFlag.Grays), 1, 3)
                    For j = 0 To gradientStopCount - 1
                        Dim stopOffset = buffer(offset)
                        Dim color As Color
                        If gradientFlags.HasFlag(GradientFlag.NoAlpha) Then
                            color = Color.FromRgb(buffer(offset + 1), buffer(offset + greenOffset), buffer(offset + blueOffset))
                        Else
                            color = Color.FromArgb(buffer(offset + blueOffset + 1), buffer(offset + 1), buffer(offset + greenOffset), buffer(offset + blueOffset))
                            offset += 1
                        End If
                        offset += blueOffset + 1
                        gradientBrush.GradientStops.Add(New GradientStop(color, stopOffset / 255))
                    Next
                    brush = gradientBrush
                Case StyleType.SolidColorNoAlpha
                    brush = New SolidColorBrush(Color.FromRgb(buffer(offset), buffer(offset + 1), buffer(offset + 2)))
                    offset += 3
                Case StyleType.SolidGray
                    brush = New SolidColorBrush(Color.FromArgb(buffer(offset + 1), buffer(offset), buffer(offset), buffer(offset)))
                    offset += 2
                Case StyleType.SolidGrayNoAlpha
                    brush = New SolidColorBrush(Color.FromRgb(buffer(offset), buffer(offset), buffer(offset)))
                    offset += 1
                Case Else
                    offset += BitConverter.ToUInt16(buffer, offset) + 2
                    Continue For
            End Select
            Styles.Add(brush)
        Next
        Dim pathCount = buffer(offset)
        offset += 1
        For i = 0 To pathCount - 1
            Dim pathFlags = CType(buffer(offset), PathFlag)
            Dim pointCount = buffer(offset + 1)
            offset += 2
            Dim pathCommands As PathCommand() = Nothing
            If pathFlags.HasFlag(PathFlag.UsesCommands) Then
                ReDim pathCommands(pointCount - 1)
                Dim commandPosition = 0
                For j = 0 To pointCount - 1
                    pathCommands(j) = CType((buffer(offset) >> commandPosition) And 3, PathCommand)
                    commandPosition += 2
                    If commandPosition = 8 Then
                        commandPosition = 0
                        offset += 1
                    End If
                Next
                If commandPosition <> 0 Then
                    offset += 1
                End If
            End If
            Dim path As New PathFigure With {
                .IsClosed = pathFlags.HasFlag(PathFlag.Closed)
            }
            Dim lastPoint, lastPointOut, firstPointIn As Point
            For j = 0 To pointCount - 1
                Dim first = j = 0
                Dim addLineSegment = Sub() path.Segments.Add(New BezierSegment With {
                        .Point1 = lastPointOut,
                        .Point2 = lastPoint,
                        .Point3 = lastPoint
                    })
                Dim readLine =
                    Sub()
                        lastPoint = New Point(readCoordinate(), readCoordinate())
                        If first Then
                            path.StartPoint = lastPoint
                            firstPointIn = lastPoint
                        Else
                            addLineSegment()
                        End If
                        lastPointOut = lastPoint
                    End Sub
                Dim readCurve =
                    Sub()
                        lastPoint = New Point(readCoordinate(), readCoordinate())
                        Dim pointIn As New Point(readCoordinate(), readCoordinate())
                        If first Then
                            path.StartPoint = lastPoint
                            firstPointIn = pointIn
                        Else
                            path.Segments.Add(New BezierSegment With {
                                .Point1 = lastPointOut,
                                .Point2 = pointIn,
                                .Point3 = lastPoint
                            })
                        End If
                        lastPointOut = New Point(readCoordinate(), readCoordinate())
                    End Sub

                If pathFlags.HasFlag(PathFlag.NoCurves) Then
                    readLine()
                ElseIf pathFlags.HasFlag(PathFlag.UsesCommands) Then
                    Select Case pathCommands(j)
                        Case PathCommand.HLine
                            lastPoint = New Point(readCoordinate(), lastPoint.Y)
                            addLineSegment()
                            lastPointOut = lastPoint
                        Case PathCommand.VLine
                            lastPoint = New Point(lastPoint.X, readCoordinate())
                            addLineSegment()
                            lastPointOut = lastPoint
                        Case PathCommand.Line
                            readLine()
                        Case PathCommand.Curve
                            readCurve()
                    End Select
                Else
                    readCurve()
                End If
            Next
            If pathFlags.HasFlag(PathFlag.Closed) Then
                path.Segments.Add(New BezierSegment With {
                    .Point1 = lastPointOut,
                    .Point2 = firstPointIn,
                    .Point3 = path.StartPoint
                })
            End If
            Paths.Add(path)
        Next
        Dim shapeCount = buffer(offset)
        offset += 1
        For i = 0 To shapeCount - 1
            Dim shapeType = CType(buffer(offset), ShapeType)
            offset += 1
            Select Case shapeType
                Case ShapeType.PathSource
                    Dim styleIndex = buffer(offset)
                    Dim shapePathCount = buffer(offset + 1)
                    offset += 2
                    Dim pathGeometry As New PathGeometry
                    Dim path As New Shapes.Path With {
                        .Fill = Styles(styleIndex),
                        .Data = pathGeometry
                    }
                    For j = 0 To shapePathCount - 1
                        Dim pathIndex = buffer(offset)
                        offset += 1
                        pathGeometry.Figures.Add(Paths(pathIndex))
                    Next
                    Dim shapeFlags = CType(buffer(offset), ShapeFlag)
                    offset += 1
                    path.SnapsToDevicePixels = shapeFlags.HasFlag(ShapeFlag.Hinting)
                    If shapeFlags.HasFlag(ShapeFlag.Transform) Then
                        pathGeometry.Transform = New MatrixTransform(readFloat24(), readFloat24(), readFloat24(), readFloat24(), readFloat24(), readFloat24())
                    ElseIf shapeFlags.HasFlag(ShapeFlag.Translation) Then
                        pathGeometry.Transform = New TranslateTransform(readCoordinate(), readCoordinate())
                    End If
                    If shapeFlags.HasFlag(ShapeFlag.LodScale) Then
                        path.Tag = (buffer(offset) / 63.75, buffer(offset + 1) / 63.75)
                        offset += 2
                    End If
                    If shapeFlags.HasFlag(ShapeFlag.HasTransformers) Then
                        Dim transformerCount = buffer(offset)
                        offset += 1
                        For j = 0 To transformerCount - 1
                            Dim transformerType = CType(buffer(offset), TransformerType)
                            Select Case transformerType
                                Case TransformerType.Affine
                                    offset += 1
                                    Dim matrix As New Matrix(readFloat24(), readFloat24(), readFloat24(), readFloat24(), readFloat24(), readFloat24())
                                    matrix *= pathGeometry.Transform.Value
                                    pathGeometry.Transform = New MatrixTransform(matrix)
                                Case TransformerType.Contour
                                    ' TODO: this is supported only once (and probably not correct)
                                    path.StrokeThickness = buffer(offset + 1) - 128
                                    path.StrokeLineJoin = ConvertLineJoin(buffer(offset + 2))
                                    path.StrokeMiterLimit = buffer(offset + 3)
                                    If path.Fill IsNot Nothing Then
                                        path.Stroke = path.Fill
                                        path.Fill = Nothing
                                    End If
                                    offset += 4
                                Case TransformerType.Perspective
                                    ' TODO: handle this
                                    offset += 1
                                Case TransformerType.Stroke
                                    ' TODO: this is supported only once
                                    Dim lineOptions = buffer(offset + 2)
                                    Dim lineCap = ConvertLineCap(lineOptions >> 4)
                                    path.StrokeThickness = buffer(offset + 1) - 128
                                    path.StrokeLineJoin = ConvertLineJoin(lineOptions And 15)
                                    path.StrokeStartLineCap = lineCap
                                    path.StrokeEndLineCap = lineCap
                                    path.StrokeMiterLimit = buffer(offset + 3)
                                    If path.Fill IsNot Nothing Then
                                        path.Stroke = path.Fill
                                        path.Fill = Nothing
                                    End If
                                    offset += 4
                                Case Else
                                    offset += BitConverter.ToUInt16(buffer, offset + 1) + 3
                                    Continue For
                            End Select
                        Next
                    End If

                    Shapes.Add(path)
                    AddVisualChild(path)
                Case Else
                    offset += BitConverter.ToUInt16(buffer, offset) + 2
                    Continue For
            End Select
        Next
    End Sub

    Private Shared Function ConvertLineCap(lineCap As Byte) As PenLineCap
        Select Case CType(lineCap, Agg.LineCap)
            Case Agg.LineCap.ButtCap
                Return PenLineCap.Flat
            Case Agg.LineCap.SquareCap
                Return PenLineCap.Square
            Case Agg.LineCap.RoundCap
                Return PenLineCap.Round
        End Select
        Return PenLineCap.Flat
    End Function

    Private Shared Function ConvertLineJoin(lineJoin As Integer) As PenLineJoin
        Select Case CType(lineJoin, Agg.LineJoin)
            Case Agg.LineJoin.MiterJoin
            Case Agg.LineJoin.MiterJoinRevert
            Case Agg.LineJoin.MiterJoinRound
                Return PenLineJoin.Miter
            Case Agg.LineJoin.RoundJoin
                Return PenLineJoin.Round
            Case Agg.LineJoin.BevelJoin
                Return PenLineJoin.Bevel
        End Select
        Return PenLineJoin.Miter
    End Function

    Protected Overrides ReadOnly Property VisualChildrenCount As Integer
        Get
            Return Shapes.Count
        End Get
    End Property

    Protected Overrides Function GetVisualChild(index As Integer) As Visual
        Return Shapes(index)
    End Function

    Protected Overrides Function MeasureOverride(availableSize As Size) As Size
        Dim scaleFactor = Math.Min(availableSize.Width, availableSize.Height) / 64
        For Each shape In Shapes
            shape.Measure(availableSize)
            shape.LayoutTransform = New ScaleTransform(scaleFactor, scaleFactor)
            If shape.Tag IsNot Nothing Then
                Dim visibilityScale = DirectCast(shape.Tag, (Min As Double, Max As Double))
                shape.Visibility = If(scaleFactor < visibilityScale.Min OrElse (scaleFactor > visibilityScale.Max AndAlso visibilityScale.Max < 4), Visibility.Hidden, Visibility.Visible)
            End If
        Next
        Return availableSize
    End Function

    Protected Overrides Function ArrangeOverride(finalSize As Size) As Size
        For Each shape In Shapes
            shape.Arrange(New Rect(Math.Max((finalSize.Width - finalSize.Height) / 2, 0), Math.Max((finalSize.Height - finalSize.Width) / 2, 0), finalSize.Width, finalSize.Height))
        Next
        Return finalSize
    End Function
End Class
