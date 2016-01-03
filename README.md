# What is it?
Regexp2 is a more feature-rich RegExp engine for Go.  It does not have the same guarantees as the framework's re2-based engine, but it can backtrack and use most of the Perl5 regex features. 

# Basis of the engine?
The engine is ported from the .NET framework's System.Text.RegularExpressions.Regex engine.  It was open sourced in 2015 under the MIT license and is a nice foundation for this kind of project.  There are some fundamental differences between .NET strings and Go strings that require a bit of borrowing from the Go framework regex engine as well.  

# Lifecycle
This thing is still in the foundational stages -- don't get too excited.