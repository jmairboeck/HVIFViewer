# HVIFViewer
Haiku Vector Icon Format Viewer using WPF in VB.NET

This is a WPF based renderer for the [Haiku Vector Icon Format (HVIF)](https://en.wikipedia.org/w/index.php?title=Haiku_Vector_Icon_Format&oldid=1150295285).

## Missing and incomplete features
 * most gradients (except linear and radial)
 * multiple "Contour" or "Stroke" transformers (does that even make sense?)
 * hinting?
 
## Building and testing
This can be built using any modern Visual Studio with VB.NET enabled. Requires .NET 4.0 or higher and `System.ValueTuple`.

For testing, the HVIF files from [this repository](https://github.com/darealshinji/haiku-icons) can be used, or HVIF files exported from Haiku Icon-O-Matic.

## License
MIT (inspired by source code from the Haiku project)

## Credits
The parser code is based on Haiku's [libicon](https://git.haiku-os.org/haiku/tree/src/libs/icon), [libagg headers](https://git.haiku-os.org/haiku/tree/headers/libs/agg) and [this article](http://blog.leahhanson.us/post/recursecenter2016/haiku_icons.html) by Leah Hanson for the initial format description.
