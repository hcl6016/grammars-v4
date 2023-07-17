import {Parser, Lexer, Token, TokenStream, ParserRuleContext} from "antlr4";
import CPP14Parser from './CPP14Parser';
import ParametersAndQualifiersContext from './CPP14Parser';

export default abstract class CPP14ParserBase extends Parser {

    constructor(input: TokenStream) {
        super(input);
    }

    protected IsPureSpecifierAllowed() : boolean {
        try {
            var x = this._ctx; // memberDeclarator
            var c = (x.children[0] as ParserRuleContext).children[0] as ParserRuleContext;
            var c2 = c.children[0] as ParserRuleContext;
            var p = c2.children[1] as ParserRuleContext;
	    if (p == undefined)
		return false;
//	    console.log("P " + p);
//	    var q = p.constructor === ParametersAndQualifiersContext;
	    var q = p instanceof ParametersAndQualifiersContext;
//	    console.log("Q " + q);
	    return true;
        } catch (e) {
        }
        return false;
    }
}
