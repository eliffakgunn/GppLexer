/*
 *********************************************
 *  341 Programming Languages                *
 *  Fall 2020                                *
 *  Author: Elif Akgun                       *
 *********************************************
 */


%{
int flag = 0;   //flag == 1 when quotation mark is opened
                //flag == 0 when quotation mark is closed
int temp = 1;   //if temp == 0 then loop goes on
                //if temp == 1 then loop breaks
FILE *fp_out; 
%}

%%
"and"       { fprintf (fp_out, "KW_AND\n"); temp=0; }
"or"        { fprintf (fp_out, "KW_OR\n"); temp=0; }
"not"       { fprintf (fp_out, "KW_NOT\n"); temp=0; }   
"equal"     { fprintf (fp_out, "KW_EQUAL\n"); temp=0; }
"less"      { fprintf (fp_out, "KW_LESS\n"); temp=0; }
"nil"       { fprintf (fp_out, "KW_NIL\n"); temp=0; }
"list"      { fprintf (fp_out, "KW_LIST\n"); temp=0; }
"append"    { fprintf (fp_out, "KW_APPEND\n"); temp=0; }
"concat"    { fprintf (fp_out, "KW_CONCAT\n"); temp=0; }
"set"       { fprintf (fp_out, "KW_SET\n"); temp=0; }
"deffun"    { fprintf (fp_out, "KW_DEFFUN\n"); temp=0; }
"for"       { fprintf (fp_out, "KW_FOR\n"); temp=0; }
"if"        { fprintf (fp_out, "KW_IF\n"); temp=0; }
"exit"      { fprintf (fp_out, "KW_EXIT\n"); temp=0; } 
"load"      { fprintf (fp_out, "KW_LOAD\n"); temp=0; }
"disp"      { fprintf (fp_out, "KW_DISP\n"); temp=0; }
"true"      { fprintf (fp_out, "KW_TRUE\n"); temp=0; }
"false"     { fprintf (fp_out, "KW_FALSE\n"); temp=0; }
"+"         { fprintf (fp_out, "OP_PLUS\n"); temp=0; }
"-"         { fprintf (fp_out, "OP_MINUS\n"); temp=0; }
"/"         { fprintf (fp_out, "OP_DIV\n"); temp=0; }
"*"         { fprintf (fp_out, "OP_MULT\n"); temp=0; }
"("         { fprintf (fp_out, "OP_OP\n"); temp=0; }
")"         { fprintf (fp_out, "OP_CP\n"); temp=0; }
"**"        { fprintf (fp_out, "OP_DBLMULT\n"); temp=0; }
"\""        {
                if(flag == 0){
                    fprintf (fp_out, "OP_OC\n");
                    flag = 1;
                }
                else{
                    fprintf (fp_out, "OP_CC\n");
                    flag = 0;                   
                }
                temp=0;
            }
","         { fprintf (fp_out, "OP_COMMA\n"); temp=0; }

";;".*                  { fprintf (fp_out, "COMMENT\n"); temp=0; }
[0]|[1-9][0-9]*         { fprintf (fp_out, "VALUE\n"); temp=0; }      //regular expression
[a-zA-Z_][a-zA-Z0-9_]*  { fprintf (fp_out, "IDENTIFIER\n"); temp=0; } //regular expression

[\t ]       { } //ignore white spaces
[\n]        { 
                if(temp == 0) 
                    temp = 1;
                else
                    return 0;
            } 
.           { fprintf (fp_out, "SYNTAX_ERROR %s cannot be tokenized\n", yytext); temp = 0;}   
%%

int yywrap(){} 

int main(int argc, char *argv[]){ 
    FILE *fp_in; 

    fp_out = fopen ("parsed_cpp.txt", "w");

    if(argc == 1)
        yylex();
    else if(argc == 2){
        fp_in = fopen(argv[1], "r"); 

        if(fp_in == NULL){
            printf("File could not opened.\n");
            return -1;
        }

        yyin=fp_in;
        yylex();
    }
    else
        printf("You entered wrong command line.\n");
    
    return 0; 
} 
