package br.ufma.ecp;

import static br.ufma.ecp.token.TokenType.*;

import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

import br.ufma.ecp.token.Token;
import br.ufma.ecp.token.TokenType;

public class Scanner {

    private byte[] input;
    private int current;
    private int start;
    private int line = 1;

    private static final Map<String, TokenType> keywords;
 

    static {
        keywords = new HashMap<>();
        keywords.put("while", WHILE);
        keywords.put("int", INT);
        keywords.put("class", CLASS);
        keywords.put("constructor", CONSTRUCTOR);
        keywords.put("function", FUNCTION);
        keywords.put("method", METHOD);
        keywords.put("field", FIELD);
        keywords.put("static", STATIC);
        keywords.put("var", VAR);
        keywords.put("char", CHAR);
        keywords.put("boolean", BOOLEAN);
        keywords.put("void", VOID);
        keywords.put("true", TRUE);
        keywords.put("false", FALSE);
        keywords.put("null", NULL);
        keywords.put("this", THIS);
        keywords.put("let", LET);
        keywords.put("do", DO);
        keywords.put("if", IF);
        keywords.put("else", ELSE);
        keywords.put("return", RETURN);
  }

    
    public Scanner (byte[] input) {
        this.input = input;
        current = 0;
        start = 0;
    }

    private void skipBlockComments() {
        boolean endComment = false;
        advance();
        while (!endComment) {
            advance();
            char ch = peek();
            if (ch == '\n')
                line++;
            if ( ch == 0) {
                System.exit(1);
            }
         
            if (ch == '*') {
               for (ch = peek(); ch == '*';  advance(), ch = peek()) ;
                if (ch == '/') {
                    endComment = true;
                    advance();
                }
            }

        }
    }

    private void skipLineComments() {
        for (char ch = peek(); ch != '\n' && ch != 0; advance(), ch = peek())
            if (ch == '\n')
                line++;
    }

    private void skipWhitespace() {
        char ch = peek();
        while (ch == ' ' || ch == '\r' || ch == '\t' || ch == '\n') {

            if (ch == '\n')
                line++;

            advance();
            ch = peek();
        }
    }

    private char peekNext () {
        int next = current + 1;
        if ( next  < input.length) {
            return (char)input[next];
        } else {
            return 0;
        }
   }

    public Token nextToken () {

        skipWhitespace();

        start = current;
        char ch = peek();

        if (Character.isDigit(ch)) {
            return number();
        }

        if (isAlpha(ch)) {
            return identifier();
        }

        switch (ch) {

            case '*':
                advance();
                return new Token (ASTERISK,"*", line); 
            case '.':
                advance();
                return new Token (DOT,".", line); 
            case '&':
                advance();
                return new Token (AND,"&", line); 
            case '|':
                advance();
                return new Token (OR,"|", line); 
            case '~':
                advance();
                return new Token (NOT,"~", line); 
            case '>':
                advance();
                return new Token (GT,">", line); 
            case '<':
                advance();
                return new Token (LT,"<", line); 
            case '=':
                advance();
                return new Token (EQ,"=", line); 
            case '(':
                advance();
                return new Token (LPAREN,"(", line); 
            case ')':
                advance();
                return new Token (RPAREN,")", line); 
            case '{':
                advance();
                return new Token (LBRACE,"{", line); 
            case '}':
                advance();
                return new Token (RBRACE,"}", line); 
            case '[':
                advance();
                return new Token (LBRACKET,"[", line); 
            case ']':
                advance();
                return new Token (RBRACKET,"]", line); 
            case ';':
                advance();
                return new Token (SEMICOLON,";", line); 
            case ',':
                advance();
                return new Token (COMMA,",", line);
            case '+':
                advance();
                return new Token(PLUS, "+", line);
            case '-':
                advance();
                return new Token (MINUS,"-", line);
            case '/':
                if (peekNext() == '/') {
                    skipLineComments();
                    return nextToken();
                } else if (peekNext() == '*') {
                    skipBlockComments();
                    return nextToken();
                }
                else {
                    advance();
                    return new Token (SLASH,"/", line);
                }
            case '"':
                return string();
            case 0:
                return new Token (EOF,"EOF", line);
            default:
                advance();
                return new Token(ILLEGAL, Character.toString(ch), line);
        }
    }

    private Token identifier() {
        while (isAlphaNumeric(peek())) advance();

        String id = new String(input, start, current-start, StandardCharsets.UTF_8)  ;
        TokenType type = keywords.get(id);
        if (type == null) type = IDENT;
        return new Token(type, id, line);
    }

    private Token number() {
        while (Character.isDigit(peek())) {
            advance();
        }
        
            String num = new String(input, start, current-start, StandardCharsets.UTF_8)  ;
            return new Token(NUMBER, num, line);
    }

    private Token string () {
        advance();
        start = current;
        while (peek() != '"' && peek() != 0) {
            advance();
        }
        String s = new String(input, start, current-start, StandardCharsets.UTF_8);
        Token token = new Token (STRING,s, line);
        advance();
        return token;
 }

    private void advance()  {
        char ch = peek();
        if (ch != 0) {
            current++;
        }
    }

    private boolean isAlpha(char c) {
        return (c >= 'a' && c <= 'z') ||
               (c >= 'A' && c <= 'Z') ||
                c == '_';
      }
    
      private boolean isAlphaNumeric(char c) {
        return isAlpha(c) || Character.isDigit((c));
      }
    

      private char peek () {
        if (current < input.length)
           return (char)input[current];
       return 0;
    }
}