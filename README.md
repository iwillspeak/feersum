# Feersum

[![Build Status](https://dev.azure.com/iwillspeak/GitHub/_apis/build/status/iwillspeak.feersum?branchName=main)](https://dev.azure.com/iwillspeak/GitHub/_build/latest?definitionId=4&branchName=main)

> Wots in that thare box yoor holdin?

A programming lanugage experiment in F#. Progress [livestreamed on Twitch][twitch], [catchup on YouTube][yt]

# Planned Features

This is a rough list of interesting features I'd like to implement in this project,
and is by no means a guarantee or a strict roadmap.

 * [ ] Proper support for different target frameworks
 * [ ] Implement [all special forms](docs/special-forms.md)
 * [ ] Support [all value types](docs/values.md)
 * [ ] Quoted expressions.
 * [ ] Macros support.
 * [ ] Standard library & builtins from a separate assembly.
 * [ ] Interop with other .NET assemblies.
 * [ ] Multi-statement support in the REPL.
 * [ ] Diagnostics with position
    * [ ] Expose position information in parser.
    * [ ] Handle multiple parser errors.
    * [ ] Turn errors in `bind` into diagnotics too.
 * [ ] Debugging support - needs position information in bound tree.
 * [ ] MSBUILD SDK support so `.scmproj` can be defined.
 * [x] Perform on-demand compilation & emit to in-memory assembly for REPL.
 
  [twitch]: https://twitch.tv/iwillspeak
  [yt]: https://www.youtube.com/playlist?list=PLCum1jXOlhoRCBewbQD8ELE7B_7EWnWaO
