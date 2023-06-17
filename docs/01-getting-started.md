# Getting Started with Feersum Scheme

The Feersum scheme compiler is available as a .NET tool. Install it globally
with `dotnet tool install -g Feersum`. Once installed it should be available
on the command line as `feersum-scheme`. You can alais this to `scheme` if you
are brave enough to use Feersum as your _default_ scheme implementation.

Once you have the tool installed you can access a Scheme Read, Eval, Print, Loop
(REPL) by running `feersum-scheme`. You can then type in scheme expressions to
be evaluated and see their representation printed back to you:

```
Feersum Scheme Compiler - 0.2.6
§> "Hello, Schemer!"
}= "Hello, Schemer!"
§> (+ 1 9)  
}= 10
§> 
```

> note, current limitations prevent REPL expressions from accessing the results
> of previous evaluations. Each expression must be a complete Scheme form on a
> single line.
>
> Check out <https://github.com/iwillspeak/feersum/issues/35> to track progress
> on interactive scripting.

## SDK Projects

Feersum scheme is also available as a .NET SDK. See the [Examples](/examples/) for
more information.
