
This is an implementation of the Wasi API in C#.

Currently it targets .NET Core 2.2.  Originally
I was targeting .NET Standard 2.0, but I wanted to
use things like Stream.Read(Span<byte>) for performance.

