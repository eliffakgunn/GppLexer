# GppLexer

A lexer that accepts any set of valid G++ expressions and reject incorrect expressions. It tokenizes the valid statements of the G++ language and identify incorrect statements and produce an error message regarding the incorrect statement. G++ programming language is Lisp like programming language and it is described in Gppsyntax.pdf file.  

It implemented two different ways: using Flex and in Lisp.  

You can run this program with following commands:  

## Part 1 - Flex Lexer

There is a makefile in this part.  
  
make  
./gpp_lexer.out or ./gpp_lexer.out <file_name>  

## Part 2 - Clisp Lexer

clisp gpp_lexer.lisp or clisp gpp_lexer.lisp <file_name>
  

*In both part, if you enter file name as parameter, it reads file. Otherwise program enters into REPL mode.*




