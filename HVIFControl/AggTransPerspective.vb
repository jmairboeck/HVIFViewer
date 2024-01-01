Imports System.Windows
Imports System.Windows.Media

Namespace Agg
    Public Class TransPerspective
        Public sx, shy, w0, shx, sy, w1, tx, ty, w2 As Double

        Public Sub New(sx As Double, shy As Double, w0 As Double, shx As Double, sy As Double, w1 As Double, tx As Double, ty As Double, w2 As Double)
            Me.sx = sx
            Me.shy = shy
            Me.w0 = w0
            Me.shx = shx
            Me.sy = sy
            Me.w1 = w1
            Me.tx = tx
            Me.ty = ty
            Me.w2 = w2
        End Sub

        Public Sub New(a As Matrix)
            sx = a.M11
            shy = a.M12
            shx = a.M21
            sy = a.M22
            tx = a.OffsetX
            ty = a.OffsetY
            w2 = 1
        End Sub

        Public Sub Transform(ByRef point As Point)
            Dim x = point.X
            Dim y = point.Y
            Dim m = 1 / (x * w0 + y * w1 + w2)
            point.X = m * (x * sx + y * shx + tx)
            point.Y = m * (x * shy + y * sy + ty)
        End Sub

        Public Shared Operator *(a As TransPerspective, b As TransPerspective) As TransPerspective
            Dim result As New TransPerspective(
                sx:=a.sx * b.sx + a.shx * b.shy + a.tx * b.w0,
                shx:=a.sx * b.shx + a.shx * b.sy + a.tx * b.w1,
                tx:=a.sx * b.tx + a.shx * b.ty + a.tx * b.w2,
                shy:=a.shy * b.sx + a.sy * b.shy + a.ty * b.w0,
                sy:=a.shy * b.shx + a.sy * b.sy + a.ty * b.w1,
                ty:=a.shy * b.tx + a.sy * b.ty + a.ty * b.w2,
                w0:=a.w0 * b.sx + a.w1 * b.shy + a.w2 * b.w0,
                w1:=a.w0 * b.shx + a.w1 * b.sy + a.w2 * b.w1,
                w2:=a.w0 * b.tx + a.w1 * b.ty + a.w2 * b.w2
            )
            Return result
        End Operator

        Public Shared Operator *(a As Matrix, b As TransPerspective) As TransPerspective
            Dim result As New TransPerspective(
                sx:=a.M11 * b.sx + a.M21 * b.shy + a.OffsetX * b.w0,
                shx:=a.M11 * b.shx + a.M21 * b.sy + a.OffsetX * b.w1,
                tx:=a.M11 * b.tx + a.M21 * b.ty + a.OffsetX * b.w2,
                shy:=a.M12 * b.sx + a.M22 * b.shy + a.OffsetY * b.w0,
                sy:=a.M12 * b.shx + a.M22 * b.sy + a.OffsetY * b.w1,
                ty:=a.M12 * b.tx + a.M22 * b.ty + a.OffsetY * b.w2,
                w0:=b.w0,
                w1:=b.w1,
                w2:=b.w2
            )
            Return result
        End Operator

#Region "Custom additions"
        Public Shared Narrowing Operator CType(a As TransPerspective) As Matrix
            Return New Matrix(a.sx, a.shy, a.shx, a.sy, a.tx, a.ty)
        End Operator

        Public ReadOnly Property IsAffine As Boolean
            Get
                Return IsEqualEps(w0, 0) AndAlso IsEqualEps(w1, 0) AndAlso IsEqualEps(w2, 1)
            End Get
        End Property
#End Region
    End Class
End Namespace