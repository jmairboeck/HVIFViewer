# HVIFViewer
Haiku Vector Icon Format Viewer using WPF in VB.NET

This is a WPF based renderer for the [Haiku Vector Icon Format (HVIF)](https://en.wikipedia.org/wiki/Haiku_Vector_Icon_Format).

## Missing and incomplete features
 * most gradients (except linear and radial)
 * transformers
 * hinting?
 
## Building and testing
This can be built using any modern Visual Studio with VB.NET enabled. Requires .NET 4.0 or higher and `System.ValueTuple`.

For testing, the HVIF files from [this repository](https://github.com/darealshinji/haiku-icons) can be used, or HVIF files exported from Haiku Icon-O-Matic.

## License
MIT (inspired by source code from the Haiku project)

## Credits
The parser code is based on Haiku's [libicon](https://git.haiku-os.org/haiku/tree/src/libs/icon) and [this article](http://blog.leahhanson.us/post/recursecenter2016/haiku_icons.html) by Leah Hanson for the initial format description.
