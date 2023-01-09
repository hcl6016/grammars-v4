// Template generated code from trgen <version>

using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

<if(has_name_space)>namespace <name_space>
{<endif>
    public class ErrorListener\<S> : IAntlrErrorListener\<S>
    {
        public bool had_error;
        bool _quiet;
        TextWriter _out;

        public ErrorListener(bool quiet, TextWriter @out)
        {
            _quiet = quiet;
            _out = @out;
        }

        public virtual void SyntaxError(IRecognizer recognizer, S offendingSymbol, int line, int col, string msg, RecognitionException e)
        {
            had_error = true;
            if (!_quiet)
            {
                _out.WriteLine("line " + line + ":" + col + " " + msg);
            }
        }
    }
<if(has_name_space)>}<endif>
