// Template generated code from trgen <version>

import antlr4 from 'antlr4';
<tool_grammar_tuples: {x | import <x.GrammarAutomName> from './<x.GeneratedFileName>';
} >
import strops from 'typescript-string-operations';
import fs from 'fs-extra';
import pkg from 'timer-node';
import * as readline from 'node:readline';

const { Timer, Time, TimerOptions } = pkg;

function getChar() {
	let buffer = Buffer.alloc(1);
	var xx = 0;
	try {
		xx = fs.readSync(0, buffer, 0, 1);
	} catch (err) {
	}
    if (xx === 0) {
        return '';
    }
    return buffer.toString('utf8');
}

var num_errors = 0;

class MyErrorListener extends antlr4.error.ErrorListener {
    syntaxError(recognizer, offendingSymbol, line, column, msg, err) {
        num_errors++;
        console.error(`${offendingSymbol} line ${line}, col ${column}: ${msg}`);
    }
}

var shunt_output = false;
var show_profile = false;
var show_tree = false;
var show_tokens = false;
var show_trace = false;
var error_code = 0;
var encoding = 'utf8';
var string_instance = 0;
var prefix = '';
var quiet = false;
var inputs = [];
var is_fns = [];

function splitLines(t) { return t.split(/\r\n|\r|\n/); }

function main() {
    for (let i = 2; i \< process.argv.length; ++i)
    {
        switch (process.argv[i]) {
            case '-tokens':
                show_tokens = true;
                break;
            case '-tree':
                show_tree = true;
                break;
            case '-prefix':
                prefix = process.argv[++i] + ' ';
                break;
            case '-input':
                inputs.push(process.argv[++i]);
                is_fns.push(false);
                break;
            case '-shunt':
                shunt_output = true;
                break;
            case '-encoding':
                encoding = process.argv[++i];
                break;
			case '-x':
				var sb = new strops.StringBuilder();
				var ch;
				while ((ch = getChar()) != '') {
					sb.Append(ch);
				}
				var input = sb.ToString();
				var sp = splitLines(input);
				for (var ii of sp) {
					if ( ii == '' ) continue;
					inputs.push(ii);
					is_fns.push(true);
				}
                break;
            case '-trace':
                show_trace = true;
                break;
            default:
                inputs.push(process.argv[i]);
                is_fns.push(true);
                break;
        }
    }
    if (inputs.length == 0) {
        ParseStdin();
    }
    else {
        const timer = new Timer({ label: 'test-timer' });
        timer.start();
        for (var f = 0; f \< inputs.length; ++f)
        {
            if (is_fns[f])
                ParseFilename(inputs[f], f);
            else
                ParseString(inputs[f], f);
        }
        timer.stop();
        var t = timer.time().m * 60 + timer.time().s + timer.time().ms / 1000;
        console.error('Total Time: ' + t);
    }
    process.exitCode = error_code;
}

function ParseStdin() {
    var sb = new strops.StringBuilder();
    var ch;
    while ((ch = getChar()) != '') {
        sb.Append(ch);
    }
    var input = sb.ToString();
    var str = antlr4.CharStreams.fromString(input);
    DoParse(str, "stdin", 0);
}

function ParseString(input, row_number) {
    var str = antlr4.CharStreams.fromString(input);
    DoParse(str, "string" + string_instance++, row_number);
}

function ParseFilename(input, row_number) {
    var str = antlr4.CharStreams.fromPathSync(input, encoding);
    DoParse(str, input, row_number);
}

function DoParse(str, input_name, row_number) {
    const lexer = new <lexer_name>(str);
    lexer.strictMode = false;
    const tokens = new antlr4.CommonTokenStream(lexer);
    const parser = new <parser_name>(tokens);
    lexer.removeErrorListeners();
    parser.removeErrorListeners();
    parser.addErrorListener(new MyErrorListener());
    lexer.addErrorListener(new MyErrorListener());
    if (show_tokens) {
        for (var i = 0; ; ++i) {
            var ro_token = lexer.nextToken();
            var token = ro_token;
            token.TokenIndex = i;
            console.log(token.toString());
            if (token.type === antlr4.Token.EOF)
                break;
        }
        lexer.reset();
    }
    if (show_trace) {
        parser._interp.trace_atn_sim = true;
        antlr4.context.PredictionContext.trace_atn_sim = true;
    }
    const timer = new Timer({ label: 'test-timer2' });
    timer.start();
    const tree = parser.<start_symbol>();
    timer.stop();
    var result = "";
    if (num_errors > 0) {
        result = 'fail';
        error_code = 1;
    }
    else {
        result = 'success';
    }
    var t = timer.time().m * 60 + timer.time().s + timer.time().ms / 1000;
    if (show_tree) {
        if (shunt_output) {
            fs.writeFileSync(input_name + ".tree", tree.toStringTree(parser.ruleNames));
        } else {
            console.log(tree.toStringTree(parser.ruleNames));
        }
    }
    console.error(prefix + 'JavaScript ' + row_number + ' ' + input_name + ' ' + result + ' ' + t);
}


main()
