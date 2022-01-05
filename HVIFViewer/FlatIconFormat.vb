Imports System

Public Enum StyleType As Byte
    SolidColor = 1
    Gradient = 2
    SolidColorNoAlpha = 3
    SolidGray = 4
    SolidGrayNoAlpha = 5
End Enum

Public Enum GradientType As Byte
    Linear = 0
    Circular
    Diamond
    Conic
    Xy
    SqrtXy
End Enum

Public Enum ShapeType
    PathSource = 10
End Enum

Public Enum TransformerType
    Affine = 20
    Contour = 21
    Perspective = 22
    Stroke = 23
End Enum

<Flags>
Public Enum GradientFlag
    Transform = 1 << 1
    NoAlpha = 1 << 2
    _16BitColors = 1 << 3
    Grays = 1 << 4
End Enum

<Flags>
Public Enum PathFlag
    Closed = 1 << 1
    UsesCommands = 1 << 2
    NoCurves = 1 << 3
End Enum

Public Enum PathCommand
    HLine = 0
    VLine = 1
    Line = 2
    Curve = 3
End Enum

<Flags>
Public Enum ShapeFlag
    Transform = 1 << 1
    Hinting = 1 << 2
    LodScale = 1 << 3
    HasTransformers = 1 << 4
    Translation = 1 << 5
End Enum