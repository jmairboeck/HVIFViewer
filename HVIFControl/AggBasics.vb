Imports System

Namespace Agg
    Public Module Basics
        ''' <remarks>
        ''' This is declared upstream in agg_trans_affine.h
        ''' </remarks>
        Public Const AffineEpsilon As Double = 0.00000000000001

        ''' <param name="epsilon">The parameter isn't optional upstream</param>
        Function IsEqualEps(v1 As Double, v2 As Double, Optional epsilon As Double = AffineEpsilon) As Boolean
            Return Math.Abs(v1 - v2) <= epsilon
        End Function
    End Module
End Namespace
