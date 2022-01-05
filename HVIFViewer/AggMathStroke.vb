Imports Microsoft.VisualBasic
Namespace Agg
    Public Enum LineCap
        ButtCap
        SquareCap
        RoundCap
    End Enum

    Public Enum LineJoin
        MiterJoin = 0
        MiterJoinRevert = 1
        RoundJoin = 2
        BevelJoin = 3
        MiterJoinRound = 4
    End Enum
End Namespace