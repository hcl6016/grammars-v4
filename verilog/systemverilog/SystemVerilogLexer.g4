/*
MIT License

Copyright (c) 2022 Mustafa Said Ağca

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

// $antlr-format alignTrailingComments true, columnLimit 150, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine true, allowShortBlocksOnASingleLine true, minEmptyLines 0, alignSemicolons ownLine
// $antlr-format alignColons trailing, singleLineOverrulesHangingColon true, alignLexerCommands true, alignLabels true, alignTrailers true

lexer grammar SystemVerilogLexer;
channels {
    COMMENTS,
    DIRECTIVES
}

ACCEPT_ON           : 'accept_on';
ALIAS               : 'alias';
ALWAYS              : 'always';
ALWAYS_COMB         : 'always_comb';
ALWAYS_FF           : 'always_ff';
ALWAYS_LATCH        : 'always_latch';
AND                 : 'and';
ASSERT              : 'assert';
ASSIGN              : 'assign';
ASSUME              : 'assume';
AUTOMATIC           : 'automatic';
BEFORE              : 'before';
BEGIN               : 'begin';
BIND                : 'bind';
BINS                : 'bins';
BINSOF              : 'binsof';
BIT                 : 'bit';
BREAK               : 'break';
BUF                 : 'buf';
BUFIFONE            : 'bufif1';
BUFIFZERO           : 'bufif0';
BYTE                : 'byte';
CASE                : 'case';
CASEX               : 'casex';
CASEZ               : 'casez';
CELL                : 'cell';
CHANDLE             : 'chandle';
CHECKER             : 'checker';
CLASS               : 'class';
CLOCKING            : 'clocking';
CMOS                : 'cmos';
CONFIG              : 'config';
CONST               : 'const';
CONSTRAINT          : 'constraint';
CONTEXT             : 'context';
CONTINUE            : 'continue';
COVER               : 'cover';
COVERGROUP          : 'covergroup';
COVERPOINT          : 'coverpoint';
CROSS               : 'cross';
DEASSIGN            : 'deassign';
DEFAULT             : 'default';
DEFPARAM            : 'defparam';
DESIGN              : 'design';
DISABLE             : 'disable';
DIST                : 'dist';
DLERROR             : '$error';
DLFATAL             : '$fatal';
DLFULLSKEW          : '$fullskew';
DLHOLD              : '$hold';
DLINFO              : '$info';
DLNOCHANGE          : '$nochange';
DLPERIOD            : '$period';
DLRECOVERY          : '$recovery';
DLRECREM            : '$recrem';
DLREMOVAL           : '$removal';
DLROOT              : '$root';
DLSETUP             : '$setup';
DLSETUPHOLD         : '$setuphold';
DLSKEW              : '$skew';
DLTIMESKEW          : '$timeskew';
DLUNIT              : '$unit';
DLWARNING           : '$warning';
DLWIDTH             : '$width';
DO                  : 'do';
DQDPIDQ             : '"DPI"';
DQDPIMICDQ          : '"DPI-C"';
EDGE                : 'edge';
ELSE                : 'else';
END                 : 'end';
ENDCASE             : 'endcase';
ENDCHECKER          : 'endchecker';
ENDCLASS            : 'endclass';
ENDCLOCKING         : 'endclocking';
ENDCONFIG           : 'endconfig';
ENDFUNCTION         : 'endfunction';
ENDGENERATE         : 'endgenerate';
ENDGROUP            : 'endgroup';
ENDINTERFACE        : 'endinterface';
ENDMODULE           : 'endmodule';
ENDPACKAGE          : 'endpackage';
ENDPRIMITIVE        : 'endprimitive';
ENDPROGRAM          : 'endprogram';
ENDPROPERTY         : 'endproperty';
ENDSEQUENCE         : 'endsequence';
ENDSPECIFY          : 'endspecify';
ENDTABLE            : 'endtable';
ENDTASK             : 'endtask';
ENUM                : 'enum';
EVENT               : 'event';
EVENTUALLY          : 'eventually';
EXPECT              : 'expect';
EXPORT              : 'export';
EXTENDS             : 'extends';
EXTERN              : 'extern';
FINAL               : 'final';
FIRST_MATCH         : 'first_match';
FOR                 : 'for';
FORCE               : 'force';
FOREACH             : 'foreach';
FOREVER             : 'forever';
FORK                : 'fork';
FORKJOIN            : 'forkjoin';
FUNCTION            : 'function';
GENERATE            : 'generate';
GENVAR              : 'genvar';
GLOBAL              : 'global';
HIGHZONE            : 'highz1';
HIGHZZERO           : 'highz0';
IF                  : 'if';
IFF                 : 'iff';
IFNONE              : 'ifnone';
IGNORE_BINS         : 'ignore_bins';
ILLEGAL_BINS        : 'illegal_bins';
IMPLEMENTS          : 'implements';
IMPLIES             : 'implies';
IMPORT              : 'import';
INCLUDE             : 'include' -> pushMode(LIBRARY_MODE);
INITIAL             : 'initial';
INOUT               : 'inout';
INPUT               : 'input';
INSIDE              : 'inside';
INSTANCE            : 'instance';
INT                 : 'int';
INTEGER             : 'integer';
INTERCONNECT        : 'interconnect';
INTERFACE           : 'interface';
INTERSECT           : 'intersect';
JOIN                : 'join';
JOIN_ANY            : 'join_any';
JOIN_NONE           : 'join_none';
LARGE               : 'large';
LET                 : 'let';
LIBLIST             : 'liblist';
LIBRARY             : 'library' -> pushMode(LIBRARY_MODE);
LOCAL               : 'local';
LOCALPARAM          : 'localparam';
LOGIC               : 'logic';
LONGINT             : 'longint';
MACROMODULE         : 'macromodule';
MATCHES             : 'matches';
MEDIUM              : 'medium';
MIINCDIR            : '-incdir';
MODPORT             : 'modport';
MODULE              : 'module';
NAND                : 'nand';
NEGEDGE             : 'negedge';
NETTYPE             : 'nettype';
NEW                 : 'new';
NEXTTIME            : 'nexttime';
NMOS                : 'nmos';
NOR                 : 'nor';
NOSHOWCANCELLED     : 'noshowcancelled';
NOT                 : 'not';
NOTIFONE            : 'notif1';
NOTIFZERO           : 'notif0';
NULL                : 'null';
ONESTEP             : '1step';
OPTION              : 'option';
OR                  : 'or';
OUTPUT              : 'output';
PACKAGE             : 'package';
PACKED              : 'packed';
PARAMETER           : 'parameter';
PATHPULSEDL         : 'PATHPULSE$';
PMOS                : 'pmos';
POSEDGE             : 'posedge';
PRIMITIVE           : 'primitive';
PRIORITY            : 'priority';
PROGRAM             : 'program';
PROPERTY            : 'property';
PROTECTED           : 'protected';
PULLDOWN            : 'pulldown';
PULLONE             : 'pull1';
PULLUP              : 'pullup';
PULLZERO            : 'pull0';
PULSESTYLE_ONDETECT : 'pulsestyle_ondetect';
PULSESTYLE_ONEVENT  : 'pulsestyle_onevent';
PURE                : 'pure';
RAND                : 'rand';
RANDC               : 'randc';
RANDCASE            : 'randcase';
RANDOMIZE           : 'randomize';
RANDSEQUENCE        : 'randsequence';
RCMOS               : 'rcmos';
REAL                : 'real';
REALTIME            : 'realtime';
REF                 : 'ref';
REG                 : 'reg';
REJECT_ON           : 'reject_on';
RELEASE             : 'release';
REPEAT              : 'repeat';
RESTRICT            : 'restrict';
RETURN              : 'return';
RNMOS               : 'rnmos';
RPMOS               : 'rpmos';
RTRAN               : 'rtran';
RTRANIFONE          : 'rtranif1';
RTRANIFZERO         : 'rtranif0';
S_ALWAYS            : 's_always';
S_EVENTUALLY        : 's_eventually';
S_NEXTTIME          : 's_nexttime';
S_UNTIL             : 's_until';
S_UNTIL_WITH        : 's_until_with';
SAMPLE              : 'sample';
SCALARED            : 'scalared';
SEQUENCE            : 'sequence';
SHORTINT            : 'shortint';
SHORTREAL           : 'shortreal';
SHOWCANCELLED       : 'showcancelled';
SIGNED              : 'signed';
SMALL               : 'small';
SOFT                : 'soft';
SOLVE               : 'solve';
SPECIFY             : 'specify';
SPECPARAM           : 'specparam';
STATIC              : 'static';
STD                 : 'std';
STRING              : 'string';
STRONG              : 'strong';
STRONGONE           : 'strong1';
STRONGZERO          : 'strong0';
STRUCT              : 'struct';
SUPER               : 'super';
SUPPLYONE           : 'supply1';
SUPPLYZERO          : 'supply0';
SYNC_ACCEPT_ON      : 'sync_accept_on';
SYNC_REJECT_ON      : 'sync_reject_on';
TABLE               : 'table' -> pushMode(TABLE_MODE);
TAGGED              : 'tagged';
TASK                : 'task';
THIS                : 'this';
THROUGHOUT          : 'throughout';
TIME                : 'time';
TIMEPRECISION       : 'timeprecision';
TIMEUNIT            : 'timeunit';
TRAN                : 'tran';
TRANIFONE           : 'tranif1';
TRANIFZERO          : 'tranif0';
TRI                 : 'tri';
TRIAND              : 'triand';
TRIONE              : 'tri1';
TRIOR               : 'trior';
TRIREG              : 'trireg';
TRIZERO             : 'tri0';
TYPE                : 'type';
TYPE_OPTION         : 'type_option';
TYPEDEF             : 'typedef';
UNION               : 'union';
UNIQUE              : 'unique';
UNIQUEZERO          : 'unique0';
UNSIGNED            : 'unsigned';
UNTIL               : 'until';
UNTIL_WITH          : 'until_with';
UNTYPED             : 'untyped';
USE                 : 'use';
UWIRE               : 'uwire';
VAR                 : 'var';
VECTORED            : 'vectored';
VIRTUAL             : 'virtual';
VOID                : 'void';
WAIT                : 'wait';
WAIT_ORDER          : 'wait_order';
WAND                : 'wand';
WEAK                : 'weak';
WEAKONE             : 'weak1';
WEAKZERO            : 'weak0';
WHILE               : 'while';
WILDCARD            : 'wildcard';
WIRE                : 'wire';
WITH                : 'with';
WITHIN              : 'within';
WOR                 : 'wor';
XNOR                : 'xnor';
XOR                 : 'xor';

AM       : '&';
AMAM     : '&&';
AMAMAM   : '&&&';
AMEQ     : '&=';
AP       : '\'';
AS       : '*';
ASAS     : '**';
ASEQ     : '*=';
ASGT     : '*>';
AT       : '@';
ATAT     : '@@';
CA       : '^';
CAEQ     : '^=';
CATI     : '^~';
CL       : ':';
CLCL     : '::';
CLEQ     : ':=';
CLSL     : ':/';
CO       : ',';
DL       : '$';
DQ       : '"';
DT       : '.';
DTAS     : '.*';
EM       : '!';
EMEQ     : '!=';
EMEQEQ   : '!==';
EMEQQM   : '!=?';
EQ       : '=';
EQEQ     : '==';
EQEQEQ   : '===';
EQEQQM   : '==?';
EQGT     : '=>';
GA       : '`' -> channel(DIRECTIVES), pushMode(DIRECTIVE_MODE);
GT       : '>';
GTEQ     : '>=';
GTGT     : '>>';
GTGTEQ   : '>>=';
GTGTGT   : '>>>';
GTGTGTEQ : '>>>=';
HA       : '#';
HAEQHA   : '#=#';
HAHA     : '##';
HAMIHA   : '#-#';
LB       : '[';
LC       : '{';
LP       : '(';
LT       : '<';
LTEQ     : '<=';
LTLT     : '<<';
LTLTEQ   : '<<=';
LTLTLT   : '<<<';
LTLTLTEQ : '<<<=';
LTMIGT   : '<->';
MI       : '-';
MICL     : '-:';
MIEQ     : '-=';
MIGT     : '->';
MIGTGT   : '->>';
MIMI     : '--';
MO       : '%';
MOEQ     : '%=';
PL       : '+';
PLCL     : '+:';
PLEQ     : '+=';
PLPL     : '++';
QM       : '?';
RB       : ']';
RC       : '}';
RP       : ')';
SC       : ';';
SL       : '/';
SLEQ     : '/=';
TI       : '~';
TIAM     : '~&';
TICA     : '~^';
TIVL     : '~|';
VL       : '|';
VLEQ     : '|=';
VLEQGT   : '|=>';
VLMIGT   : '|->';
VLVL     : '||';

BINARY_BASE             : '\'' [sS]? [bB]       -> pushMode(BINARY_NUMBER_MODE);
BLOCK_COMMENT           : '/*' ASCII_ANY*? '*/' -> channel(COMMENTS);
DECIMAL_BASE            : '\'' [sS]? [dD]       -> pushMode(DECIMAL_NUMBER_MODE);
ESCAPED_IDENTIFIER      : '\\' ASCII_PRINTABLE_NO_SPACE* [ \t\r\n];
EXPONENTIAL_NUMBER      : UNSIGNED_NUMBER ( '.' UNSIGNED_NUMBER)? [eE] [+\-]? UNSIGNED_NUMBER;
FIXED_POINT_NUMBER      : UNSIGNED_NUMBER '.' UNSIGNED_NUMBER;
HEX_BASE                : '\'' [sS]? [hH]        -> pushMode(HEX_NUMBER_MODE);
LINE_COMMENT            : '//' ASCII_NO_NEWLINE* -> channel(COMMENTS);
OCTAL_BASE              : '\'' [sS]? [oO]        -> pushMode(OCTAL_NUMBER_MODE);
SIMPLE_IDENTIFIER       : [a-zA-Z_] [a-zA-Z0-9_$]*;
STRING_LITERAL          : '"' ( ASCII_NO_NEWLINE_QUOTE_BACKSLASH | ESC_NEWLINE | ESC_SPECIAL_CHAR)* '"';
SYSTEM_TF_IDENTIFIER    : '$' [a-zA-Z0-9_$] [a-zA-Z0-9_$]*;
TIME_LITERAL            : UNSIGNED_NUMBER ( '.' UNSIGNED_NUMBER)? TIME_UNIT;
UNBASED_UNSIZED_LITERAL : '\'0' | '\'1' | '\'' [xXzZ];
UNSIGNED_NUMBER         : [0-9] [0-9_]*;
WHITE_SPACE             : [ \t\r\n]+ -> channel(HIDDEN);
ZERO_OR_ONE_X_OR_Z      : [01] [xXzZ];

mode BINARY_NUMBER_MODE;
BINARY_VALUE  : [01xXzZ?] [01xXzZ?_]* -> popMode;
WHITE_SPACE_0 : WHITE_SPACE           -> channel(HIDDEN), type(WHITE_SPACE);

mode DECIMAL_NUMBER_MODE;
UNSIGNED_NUMBER_0 : UNSIGNED_NUMBER -> type(UNSIGNED_NUMBER), popMode;
WHITE_SPACE_1     : WHITE_SPACE     -> channel(HIDDEN), type(WHITE_SPACE);
X_OR_Z_UNDERSCORE : [xXzZ?] '_'*    -> popMode;

mode HEX_NUMBER_MODE;
HEX_VALUE     : [0-9a-fA-FxXzZ?] [0-9a-fA-FxXzZ?_]* -> popMode;
WHITE_SPACE_2 : WHITE_SPACE                         -> channel(HIDDEN), type(WHITE_SPACE);

mode LIBRARY_MODE;
BLOCK_COMMENT_0      : BLOCK_COMMENT      -> channel(COMMENTS), type(BLOCK_COMMENT);
CO_0                 : CO                 -> type(CO);
ESCAPED_IDENTIFIER_0 : ESCAPED_IDENTIFIER -> type(ESCAPED_IDENTIFIER);
GA_0                 : GA                 -> channel(DIRECTIVES), type(GA), pushMode(DIRECTIVE_MODE);
LINE_COMMENT_0       : LINE_COMMENT       -> channel(COMMENTS), type(LINE_COMMENT);
MIINCDIR_0           : MIINCDIR           -> type(MIINCDIR);
SC_0                 : SC                 -> type(SC), popMode;
SIMPLE_IDENTIFIER_0  : SIMPLE_IDENTIFIER  -> type(SIMPLE_IDENTIFIER);
WHITE_SPACE_3        : WHITE_SPACE        -> channel(HIDDEN), type(WHITE_SPACE);
FILE_PATH_SPEC       : ( [a-zA-Z0-9_./] | ESC_ASCII_PRINTABLE)+ | STRING_LITERAL;

mode OCTAL_NUMBER_MODE;
OCTAL_VALUE   : [0-7xXzZ?] [0-7xXzZ?_]* -> popMode;
WHITE_SPACE_4 : WHITE_SPACE             -> channel(HIDDEN), type(WHITE_SPACE);

mode TABLE_MODE;
BLOCK_COMMENT_1        : BLOCK_COMMENT -> channel(COMMENTS), type(BLOCK_COMMENT);
CL_0                   : CL            -> type(CL);
EDGE_SYMBOL            : [rRfFpPnN*];
ENDTABLE_0             : ENDTABLE -> type(ENDTABLE), popMode;
GA_1                   : GA       -> channel(DIRECTIVES), type(GA), pushMode(DIRECTIVE_MODE);
LEVEL_ONLY_SYMBOL      : [?bB];
LINE_COMMENT_1         : LINE_COMMENT -> channel(COMMENTS), type(LINE_COMMENT);
LP_0                   : LP           -> type(LP);
MI_0                   : MI           -> type(MI);
OUTPUT_OR_LEVEL_SYMBOL : [01xX];
RP_0                   : RP          -> type(RP);
SC_1                   : SC          -> type(SC);
WHITE_SPACE_5          : WHITE_SPACE -> channel(HIDDEN), type(WHITE_SPACE);

mode DIRECTIVE_MODE;
BEGIN_KEYWORDS_DIRECTIVE:
    'begin_keywords' -> channel(DIRECTIVES), mode(BEGIN_KEYWORDS_DIRECTIVE_MODE)
;
CELLDEFINE_DIRECTIVE: 'celldefine' -> channel(DIRECTIVES), popMode;
DEFAULT_NETTYPE_DIRECTIVE:
    'default_nettype' -> channel(DIRECTIVES), mode(DEFAULT_NETTYPE_DIRECTIVE_MODE)
;
DEFINE_DIRECTIVE              : 'define'              -> channel(DIRECTIVES), mode(DEFINE_DIRECTIVE_MODE);
ELSE_DIRECTIVE                : 'else'                -> channel(DIRECTIVES), popMode, mode(ELSE_DIRECTIVE_MODE);
ELSIF_DIRECTIVE               : 'elsif'               -> channel(DIRECTIVES), popMode, mode(ELSIF_DIRECTIVE_MODE);
END_KEYWORDS_DIRECTIVE        : 'end_keywords'        -> channel(DIRECTIVES), popMode;
ENDCELLDEFINE_DIRECTIVE       : 'endcelldefine'       -> channel(DIRECTIVES), popMode;
ENDIF_DIRECTIVE               : 'endif'               -> channel(DIRECTIVES), popMode, popMode, popMode;
FILE_DIRECTIVE                : '__FILE__'            -> channel(DIRECTIVES), popMode;
IFDEF_DIRECTIVE               : 'ifdef'               -> channel(DIRECTIVES), mode(IFDEF_DIRECTIVE_MODE);
IFNDEF_DIRECTIVE              : 'ifndef'              -> channel(DIRECTIVES), mode(IFDEF_DIRECTIVE_MODE);
INCLUDE_DIRECTIVE             : 'include'             -> channel(DIRECTIVES), mode(INCLUDE_DIRECTIVE_MODE);
LINE_DIRECTIVE                : 'line'                -> channel(DIRECTIVES), mode(LINE_DIRECTIVE_MODE);
LINE_DIRECTIVE_               : '__LINE__'            -> channel(DIRECTIVES), popMode;
NOUNCONNECTED_DRIVE_DIRECTIVE : 'nounconnected_drive' -> channel(DIRECTIVES), popMode;
PRAGMA_DIRECTIVE              : 'pragma'              -> channel(DIRECTIVES), mode(PRAGMA_DIRECTIVE_MODE);
RESETALL_DIRECTIVE            : 'resetall'            -> channel(DIRECTIVES), popMode;
TIMESCALE_DIRECTIVE           : 'timescale'           -> channel(DIRECTIVES), mode(TIMESCALE_DIRECTIVE_MODE);
UNCONNECTED_DRIVE_DIRECTIVE:
    'unconnected_drive' -> channel(DIRECTIVES), mode(UNCONNECTED_DRIVE_DIRECTIVE_MODE)
;
UNDEF_DIRECTIVE       : 'undef'                                -> channel(DIRECTIVES), mode(UNDEF_DIRECTIVE_MODE);
UNDEFINEALL_DIRECTIVE : 'undefineall'                          -> channel(DIRECTIVES), popMode;
MACRO_USAGE           : IDENTIFIER ( WHITE_SPACE? MACRO_ARGS)? -> channel(DIRECTIVES), popMode;

mode BEGIN_KEYWORDS_DIRECTIVE_MODE;
BLOCK_COMMENT_2 : BLOCK_COMMENT -> channel(COMMENTS), type(BLOCK_COMMENT);
DQ_0            : DQ            -> channel(DIRECTIVES), type(DQ);
NEWLINE_0       : NEWLINE       -> channel(HIDDEN), type(WHITE_SPACE), popMode;
SPACE_TAB_0     : SPACE_TAB     -> channel(HIDDEN), type(WHITE_SPACE);
VERSION_SPECIFIER:
    (
        '1800-2017'
        | '1800-2012'
        | '1800-2009'
        | '1800-2005'
        | '1364-2005'
        | '1364-2001'
        | '1364-2001-noconfig'
        | '1364-1995'
    ) -> channel(DIRECTIVES)
;

mode DEFAULT_NETTYPE_DIRECTIVE_MODE;
BLOCK_COMMENT_3: BLOCK_COMMENT -> channel(COMMENTS), type(BLOCK_COMMENT);
DEFAULT_NETTYPE_VALUE:
    (
        'wire'
        | 'tri'
        | 'tri0'
        | 'tri1'
        | 'wand'
        | 'triand'
        | 'wor'
        | 'trior'
        | 'trireg'
        | 'uwire'
        | 'none'
    ) -> channel(DIRECTIVES), popMode
;
NEWLINE_1   : NEWLINE   -> channel(HIDDEN), type(WHITE_SPACE), popMode;
SPACE_TAB_1 : SPACE_TAB -> channel(HIDDEN), type(WHITE_SPACE);

mode DEFINE_DIRECTIVE_MODE;
MACRO_NAME   : IDENTIFIER MACRO_ARGS? -> channel(DIRECTIVES), mode(MACRO_TEXT_MODE);
NEWLINE_12   : NEWLINE                -> channel(HIDDEN), type(WHITE_SPACE), popMode;
SPACE_TAB_11 : SPACE_TAB              -> channel(HIDDEN), type(WHITE_SPACE);

mode ELSE_DIRECTIVE_MODE;
NEWLINE_8   : NEWLINE   -> channel(HIDDEN), type(WHITE_SPACE), mode(SOURCE_TEXT_MODE);
SPACE_TAB_7 : SPACE_TAB -> channel(HIDDEN), type(WHITE_SPACE);

mode ELSIF_DIRECTIVE_MODE;
IDENTIFIER_0 : IDENTIFIER -> channel(DIRECTIVES), type(MACRO_IDENTIFIER);
NEWLINE_9    : NEWLINE    -> channel(HIDDEN), type(WHITE_SPACE), mode(SOURCE_TEXT_MODE);
SPACE_TAB_8  : SPACE_TAB  -> channel(HIDDEN), type(WHITE_SPACE);

mode ENDIF_DIRECTIVE_MODE;
NEWLINE_13 : NEWLINE -> channel(HIDDEN), type(WHITE_SPACE), popMode, popMode, popMode ;
SPACE_TAB_12 : SPACE_TAB -> channel(HIDDEN), type(WHITE_SPACE) ;

mode FILENAME_MODE;
DQ_1: DQ -> channel(DIRECTIVES), type(DQ), popMode;
FILENAME:
    (ASCII_PRINTABLE_NO_QUOTE_ANGLE_BRACKETS_BACKSLASH | ESC_ASCII_PRINTABLE)+ -> channel(DIRECTIVES)
;
GT_0: GT -> channel(DIRECTIVES), type(GT), popMode;

mode IFDEF_DIRECTIVE_MODE;
IDENTIFIER_1 : IDENTIFIER -> channel(DIRECTIVES), type(MACRO_IDENTIFIER);
NEWLINE_10   : NEWLINE    -> channel(HIDDEN), type(WHITE_SPACE), pushMode(SOURCE_TEXT_MODE);
SPACE_TAB_9  : SPACE_TAB  -> channel(HIDDEN), type(WHITE_SPACE);

mode INCLUDE_DIRECTIVE_MODE;
DQ_2          : DQ          -> channel(DIRECTIVES), type(DQ), pushMode(FILENAME_MODE);
GA_2          : GA          -> channel(DIRECTIVES), type(GA);
LT_0          : LT          -> channel(DIRECTIVES), type(LT), pushMode(FILENAME_MODE);
MACRO_USAGE_0 : MACRO_USAGE -> channel(DIRECTIVES), type(MACRO_USAGE), popMode;
NEWLINE_2     : NEWLINE     -> channel(HIDDEN), type(WHITE_SPACE), popMode;
SPACE_TAB_2   : SPACE_TAB   -> channel(HIDDEN), type(WHITE_SPACE);

mode LINE_DIRECTIVE_MODE;
DQ_3              : DQ              -> channel(DIRECTIVES), type(DQ), pushMode(FILENAME_MODE);
NEWLINE_3         : NEWLINE         -> channel(HIDDEN), type(WHITE_SPACE), popMode;
SPACE_TAB_3       : SPACE_TAB       -> channel(HIDDEN), type(WHITE_SPACE);
UNSIGNED_NUMBER_1 : UNSIGNED_NUMBER -> channel(DIRECTIVES), type(UNSIGNED_NUMBER);

mode MACRO_TEXT_MODE;
BLOCK_COMMENT_5   : BLOCK_COMMENT                                        -> channel(COMMENTS), type(BLOCK_COMMENT);
GA_3              : GA                                                   -> channel(DIRECTIVES), type(MACRO_TEXT);
MACRO_DELIMITER   : '``'                                                 -> channel(DIRECTIVES);
MACRO_ESC_NEWLINE : ESC_NEWLINE                                          -> channel(DIRECTIVES);
MACRO_ESC_QUOTE   : '`\\`"'                                              -> channel(DIRECTIVES);
MACRO_ESC_SEQ     : ESC_ASCII_NO_NEWLINE                                 -> channel(DIRECTIVES), type(MACRO_TEXT);
MACRO_QUOTE       : '`"'                                                 -> channel(DIRECTIVES);
MACRO_TEXT        : ASCII_NO_NEWLINE_QUOTE_SLASH_BACKSLASH_GRAVE_ACCENT+ -> channel(DIRECTIVES);
NEWLINE_4         : NEWLINE                                              -> channel(HIDDEN), type(WHITE_SPACE), popMode;
SL_2              : SL                                                   -> more;
STRING_LITERAL_0  : STRING_LITERAL                                       -> channel(DIRECTIVES), type(STRING_LITERAL);

mode PRAGMA_DIRECTIVE_MODE;
BLOCK_COMMENT_6     : BLOCK_COMMENT     -> channel(COMMENTS), type(BLOCK_COMMENT);
CO_1                : CO                -> channel(DIRECTIVES), type(CO);
EQ_0                : EQ                -> channel(DIRECTIVES), type(EQ);
LP_1                : LP                -> channel(DIRECTIVES), type(LP);
NEWLINE_5           : NEWLINE           -> channel(HIDDEN), type(WHITE_SPACE), popMode;
RP_1                : RP                -> channel(DIRECTIVES), type(RP);
SIMPLE_IDENTIFIER_1 : SIMPLE_IDENTIFIER -> channel(DIRECTIVES), type(SIMPLE_IDENTIFIER);
SPACE_TAB_4         : SPACE_TAB         -> channel(HIDDEN), type(WHITE_SPACE);
STRING_LITERAL_1    : STRING_LITERAL    -> channel(DIRECTIVES), type(STRING_LITERAL);
UNSIGNED_NUMBER_2   : UNSIGNED_NUMBER   -> channel(DIRECTIVES), type(UNSIGNED_NUMBER);

mode SOURCE_TEXT_MODE;
BLOCK_COMMENT_7 : BLOCK_COMMENT                -> channel(COMMENTS), type(BLOCK_COMMENT);
GA_4            : GA                           -> channel(DIRECTIVES), type(GA), pushMode(DIRECTIVE_MODE);
LINE_COMMENT_2  : LINE_COMMENT                 -> channel(COMMENTS), type(LINE_COMMENT);
SL_0            : SL                           -> more;
SOURCE_TEXT     : ASCII_NO_SLASH_GRAVE_ACCENT+ -> channel(DIRECTIVES);

mode TIMESCALE_DIRECTIVE_MODE;
BLOCK_COMMENT_8 : BLOCK_COMMENT         -> channel(COMMENTS), type(BLOCK_COMMENT);
NEWLINE_6       : NEWLINE               -> channel(HIDDEN), type(WHITE_SPACE), popMode;
SL_1            : SL                    -> channel(DIRECTIVES), type(SL);
SPACE_TAB_5     : SPACE_TAB             -> channel(HIDDEN), type(WHITE_SPACE);
TIME_UNIT       : [munpf]? 's'          -> channel(DIRECTIVES);
TIME_VALUE      : ( '1' | '10' | '100') -> channel(DIRECTIVES);

mode UNCONNECTED_DRIVE_DIRECTIVE_MODE;
BLOCK_COMMENT_9         : BLOCK_COMMENT        -> channel(COMMENTS), type(BLOCK_COMMENT);
NEWLINE_7               : NEWLINE              -> channel(HIDDEN), type(WHITE_SPACE), popMode;
SPACE_TAB_6             : SPACE_TAB            -> channel(HIDDEN), type(WHITE_SPACE);
UNCONNECTED_DRIVE_VALUE : ( 'pull0' | 'pull1') -> channel(DIRECTIVES), popMode;

mode UNDEF_DIRECTIVE_MODE;
MACRO_IDENTIFIER : IDENTIFIER -> channel(DIRECTIVES);
NEWLINE_11       : NEWLINE    -> channel(HIDDEN), type(WHITE_SPACE), popMode;
SPACE_TAB_10     : SPACE_TAB  -> channel(HIDDEN), type(WHITE_SPACE);

fragment ASCII_ANY        : [\u0000-\u007f];
fragment ASCII_NO_NEWLINE : [\u0000-\u0009\u000b-\u000c\u000e-\u007f];
fragment ASCII_NO_NEWLINE_QUOTE_BACKSLASH:
    [\u0000-\u0009\u000b-\u000c\u000e-\u0021\u0023-\u005b\u005d-\u007f]
;
fragment ASCII_NO_NEWLINE_QUOTE_SLASH_BACKSLASH_GRAVE_ACCENT:
    [\u0000-\u0009\u000b-\u000c\u000e-\u0021\u0023-\u002e\u0030-\u005b\u005d-\u005f\u0061-\u007f]
;
fragment ASCII_NO_PARENTHESES        : [\u0000-\u0027\u002a-\u007f];
fragment ASCII_NO_SLASH_GRAVE_ACCENT : [\u0000-\u002e\u0030-\u005f\u0061-\u007f];
fragment ASCII_PRINTABLE             : [\u0020-\u007e];
fragment ASCII_PRINTABLE_NO_QUOTE_ANGLE_BRACKETS_BACKSLASH:
    [\u0020-\u0021\u0023-\u003b\u003d\u003f-\u005b\u005d-\u007e]
;
fragment ASCII_PRINTABLE_NO_SPACE : [\u0021-\u007e];
fragment CHAR_HEX                 : [0-9a-fA-F] [0-9a-fA-F]?;
fragment CHAR_OCTAL               : [0-7] [0-7]? [0-7]?;
fragment ESC_ASCII_NO_NEWLINE     : '\\' ASCII_NO_NEWLINE;
fragment ESC_ASCII_PRINTABLE      : '\\' ASCII_PRINTABLE;
fragment ESC_NEWLINE              : '\\' NEWLINE;
fragment ESC_SPECIAL_CHAR         : '\\' ( [nt\\"vfa] | CHAR_HEX | CHAR_OCTAL);
fragment IDENTIFIER               : ESCAPED_IDENTIFIER | SIMPLE_IDENTIFIER;
fragment MACRO_ARGS               : '(' ( MACRO_ARGS | ASCII_NO_PARENTHESES)* ')';
fragment NEWLINE                  : '\r'? '\n';
fragment SPACE_TAB                : [ \t]+;