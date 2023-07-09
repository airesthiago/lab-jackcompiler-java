package br.ufma.ecp;

import static br.ufma.ecp.token.TokenType.*;

import br.ufma.ecp.token.Token;
import br.ufma.ecp.token.TokenType;
import br.ufma.ecp.SymbolTable.Kind;
import br.ufma.ecp.SymbolTable.Symbol;
import br.ufma.ecp.VMWriter.Command;
import br.ufma.ecp.VMWriter.Segment;

public class Parser {
    private static class ParseError extends RuntimeException {
    }

    private Scanner scan;
    private Token currentToken;
    private Token peekToken;

    private StringBuilder xmlOutput = new StringBuilder();

    private SymbolTable symbolTable;
    private VMWriter vmWriter;
    private String className;
    private int ifLabelNum;
    private int whileLabelNum;

    public Parser(byte[] input) {
        scan = new Scanner(input);
        symbolTable = new SymbolTable();
        vmWriter = new VMWriter();
        nextToken();
        ifLabelNum = 0;
        whileLabelNum = 0;
    }

    private void nextToken() {
        currentToken = peekToken;
        peekToken = scan.nextToken();
    }

    void parse() {
        parseClass();
    }

    public String XMLOutput() {
        return xmlOutput.toString();
    }

    public String VMOutput() {
        return vmWriter.vmOutput();
    }

    private void printNonTerminal(String nterminal) {
        xmlOutput.append(String.format("<%s>\r\n", nterminal));
    }

    boolean peekTokenIs(TokenType type) {
        return peekToken.type == type;
    }

    boolean currentTokenIs(TokenType type) {
        return currentToken.type == type;
    }

    private void expectPeek(TokenType... types) {
        for (TokenType type : types) {
            if (peekToken.type == type) {
                expectPeek(type);
                return;
            }
        }
        throw error(peekToken, "Expected a statement");
    }

    private void expectPeek(TokenType type) {
        if (peekToken.type == type) {
            nextToken();
            xmlOutput.append(String.format("%s\r\n", currentToken.toString()));
        } else {
            throw error(peekToken, "Expected " + type.name());
        }
    }

    private static void report(int line, String where, String message) {
        System.err.println("[line " + line + "] Error" + where + ": " + message);
    }

    private ParseError error(Token token, String message) {
        if (token.type == TokenType.EOF) {
            report(token.line, " at end", message);
        } else {
            report(token.line, " at '" + token.lexeme + "'", message);
        }
        return new ParseError();
    }

    static public boolean isOperator(String op) {
        return "+-*/<>=~&|".contains(op);
    }

    private Segment kind2Segment(Kind kind) {
        if (kind == Kind.STATIC)
            return Segment.STATIC;
        if (kind == Kind.FIELD)
            return Segment.THIS;
        if (kind == Kind.VAR)
            return Segment.LOCAL;
        if (kind == Kind.ARG)
            return Segment.ARG;
        return null;
    }

    private Command typeOperator(TokenType type) {
        if (type == PLUS)
            return Command.ADD;
        if (type == MINUS)
            return Command.SUB;
        if (type == LT)
            return Command.LT;
        if (type == GT)
            return Command.GT;
        if (type == EQ)
            return Command.EQ;
        if (type == AND)
            return Command.AND;
        if (type == OR)
            return Command.OR;
        return null;
    }

    void parseClass() {
        printNonTerminal("class");
        expectPeek(CLASS);
        expectPeek(IDENT);

        className = currentToken.lexeme;

        expectPeek(LBRACE);
        while (peekTokenIs(STATIC) || peekTokenIs(FIELD)) {
            parseClassVarDec();
        }

        while (peekTokenIs(FUNCTION) || peekTokenIs(CONSTRUCTOR) || peekTokenIs(METHOD)) {
            parseSubroutineDec();
        }
        expectPeek(RBRACE);
        printNonTerminal("/class");
    }

    void parseClassVarDec() {
        printNonTerminal("classVarDec");
        expectPeek(FIELD, STATIC);

        SymbolTable.Kind kind = Kind.STATIC;
        if (currentTokenIs(FIELD))
            kind = Kind.FIELD;

        expectPeek(INT, CHAR, BOOLEAN, IDENT);
        String type = currentToken.value();

        expectPeek(IDENT);
        String name = currentToken.value();
        symbolTable.define(name, type, kind);
        while (peekTokenIs(COMMA)) {
            expectPeek(COMMA);
            expectPeek(IDENT);

            name = currentToken.value();
            symbolTable.define(name, type, kind);
        }
        expectPeek(SEMICOLON);
        printNonTerminal("/classVarDec");
    }

    void parseSubroutineDec() {
        printNonTerminal("subroutineDec");

        ifLabelNum = 0;
        whileLabelNum = 0;

        symbolTable.startSubroutine();

        expectPeek(CONSTRUCTOR, FUNCTION, METHOD);
        var subroutineType = currentToken.type;

        if (subroutineType == METHOD) {
            symbolTable.define("this", className, Kind.ARG);
        }

        expectPeek(VOID, INT, CHAR, BOOLEAN, IDENT);
        expectPeek(IDENT);

        var functionName = className + "." + currentToken.lexeme;

        expectPeek(LPAREN);
        parseParameterList();
        expectPeek(RPAREN);
        parseSubroutineBody(functionName, subroutineType);

        printNonTerminal("/subroutineDec");
    }

    void parseParameterList() {
        printNonTerminal("parameterList");

        SymbolTable.Kind kind = Kind.ARG;
        if (!peekTokenIs(RPAREN)) {
            expectPeek(INT, CHAR, BOOLEAN, IDENT);
            String type = currentToken.value();

            expectPeek(IDENT);
            String name = currentToken.value();

            symbolTable.define(name, type, kind);
            while (peekTokenIs(COMMA)) {
                expectPeek(COMMA);
                expectPeek(INT, CHAR, BOOLEAN, IDENT);
                type = currentToken.value();

                expectPeek(IDENT);
                name = currentToken.value();
            }
        }
        printNonTerminal("/parameterList");
    }

    void parseSubroutineBody(String functionName, TokenType subroutineType) {
        printNonTerminal("subroutineBody");
        expectPeek(LBRACE);

        while (peekTokenIs(VAR)) {
            parseVarDec();
        }
        var nlocals = symbolTable.varCount(Kind.VAR);

        vmWriter.writeFunction(functionName, nlocals);

        if (subroutineType == CONSTRUCTOR) {
            vmWriter.writePush(Segment.CONST, symbolTable.varCount(Kind.FIELD));
            vmWriter.writeCall("Memory.alloc", 1);
            vmWriter.writePop(Segment.POINTER, 0);
        }

        if (subroutineType == METHOD) {
            vmWriter.writePush(Segment.ARG, 0);
            vmWriter.writePop(Segment.POINTER, 0);
        }

        parseStatements();
        expectPeek(RBRACE);
        printNonTerminal("/subroutineBody");
    }

    void parseVarDec() {
        printNonTerminal("varDec");
        expectPeek(VAR);

        SymbolTable.Kind kind = Kind.VAR;

        expectPeek(INT, CHAR, BOOLEAN, IDENT);
        String type = currentToken.lexeme;

        expectPeek(IDENT);
        String name = currentToken.lexeme;

        //symbolTable.define(name, type, kind);
        while (peekTokenIs(COMMA)) {
            expectPeek(COMMA);
            expectPeek(IDENT);
            name = currentToken.lexeme;
            symbolTable.define(name, type, kind);
        }
        expectPeek(SEMICOLON);
        printNonTerminal("/varDec");
    }

    void parseStatements() {
        printNonTerminal("statements");
        while (peekTokenIs(WHILE) ||
                peekTokenIs(IF) ||
                peekTokenIs(LET) ||
                peekTokenIs(DO) ||
                peekTokenIs(RETURN)) {
            parseStatement();
        }
        printNonTerminal("/statements");
    }

    void parseStatement() {
        switch (peekToken.type) {
            case LET:
                parseLet();
                break;
            case WHILE:
                parseWhile();
                break;
            case IF:
                parseIf();
                break;
            case DO:
                parseDo();
                break;
            case RETURN:
                parseReturn();
                break;
            default:
                throw error(peekToken, "Expected a statement");
        }
    }

    void parseLet() {
        var isArray = false;
        printNonTerminal("letStatement");
        expectPeek(LET);
        expectPeek(IDENT);

        var symbol = symbolTable.resolve(currentToken.value());
        if (peekTokenIs(LBRACKET)) {
            expectPeek(LBRACKET);
            parseExpression();

            vmWriter.writePush(kind2Segment(symbol.kind()), symbol.index());
            vmWriter.writeArithmetic(Command.ADD);
            
            expectPeek(RBRACKET);

            isArray = true;
        }

        expectPeek(EQ);
        parseExpression();

        if (isArray) {
            vmWriter.writePop(Segment.TEMP, 0);
            vmWriter.writePop(Segment.POINTER, 1);
            vmWriter.writePush(Segment.TEMP, 0);
            vmWriter.writePop(Segment.THAT, 0);
        }

        expectPeek(SEMICOLON);
        printNonTerminal("/letStatement");
    }

    void parseWhile() {
        printNonTerminal("whileStatement");

        var labelTrue = "WHILE_EXP" + whileLabelNum;
        var labelFalse = "WHILE_END" + whileLabelNum;
        whileLabelNum++;

        vmWriter.writeLabel(labelTrue);

        expectPeek(WHILE);
        expectPeek(LPAREN);
        parseExpression();

        vmWriter.writeArithmetic(Command.NOT);
        vmWriter.writeIf(labelFalse);

        expectPeek(RPAREN);
        expectPeek(LBRACE);
        parseStatements();

        vmWriter.writeGoto(labelTrue);
        vmWriter.writeLabel(labelFalse);

        expectPeek(RBRACE);
        printNonTerminal("/whileStatement");
    }

    void parseIf() {
        printNonTerminal("ifStatement");

        var labelTrue = "IF_TRUE" + ifLabelNum;
        var labelFalse = "IF_FALSE" + ifLabelNum;
        var labelEnd = "IF_END" + ifLabelNum;

        ifLabelNum++;

        expectPeek(IF);
        expectPeek(LPAREN);
        parseExpression();
        expectPeek(RPAREN);

        vmWriter.writeIf(labelTrue);
        vmWriter.writeGoto(labelFalse);
        vmWriter.writeLabel(labelTrue);

        expectPeek(LBRACE);
        parseStatements();
        expectPeek(RBRACE);

        if (peekTokenIs(ELSE)) {
            vmWriter.writeGoto(labelEnd);
        }

        vmWriter.writeLabel(labelFalse);
        if (peekTokenIs(ELSE)) {
            expectPeek(ELSE);
            expectPeek(LBRACE);
            parseStatements();
            expectPeek(RBRACE);
            vmWriter.writeLabel(labelEnd);
        }
        printNonTerminal("/ifStatement");
    }

    void parseDo() {
        printNonTerminal("doStatement");

        expectPeek(DO);
        expectPeek(IDENT);
        parseSubroutineCall();

        expectPeek(SEMICOLON);
        vmWriter.writePop(Segment.TEMP, 0);
        printNonTerminal("/doStatement");
    }

    void parseReturn() {
        printNonTerminal("returnStatement");
        expectPeek(RETURN);

        if (!peekTokenIs(SEMICOLON)) {
            parseExpression();
        } else {
            vmWriter.writePush(Segment.CONST, 0);
        }
        expectPeek(SEMICOLON);
        vmWriter.writeReturn();

        printNonTerminal("/returnStatement");
    }

    void parseExpression() {
        printNonTerminal("expression");
        parseTerm();

        while (isOperator(peekToken.value())) { //while (isOperator(peekToken.type))
            var ope = peekToken.type;
            expectPeek(peekToken.type);
            parseTerm();
            compileOperators(ope);
        }
        printNonTerminal("/expression");
    }

    void parseTerm() {
        printNonTerminal("term");
        switch (peekToken.type) {
            case INT:
                expectPeek(INT);
                vmWriter.writePush(Segment.CONST, Integer.parseInt(currentToken.value())); //parseInt(currentToken.lexeme));
                break;
            case NUMBER:
                expectPeek(NUMBER);
                vmWriter.writePush(Segment.CONST, Integer.parseInt(currentToken.value())); //parseInt(currentToken.lexeme));
                break;
            case STRING:
                expectPeek(STRING);
                var strValue = currentToken.value(); //currentToken.lexeme;
                vmWriter.writePush(Segment.CONST, strValue.length());
                vmWriter.writeCall("String.new", 1);
                for (int i = 0; i < strValue.length(); i++) {
                    vmWriter.writePush(Segment.CONST, strValue.charAt(i));
                    vmWriter.writeCall("String.appendChar", 2);
                }
                break;
            case FALSE:
            case NULL:
            case TRUE:
                expectPeek(FALSE, NULL, TRUE);
                vmWriter.writePush(Segment.CONST, 0);
                if (currentToken.type == TRUE)
                    vmWriter.writeArithmetic(Command.NOT);
                break;
            case THIS:
                expectPeek(THIS);
                vmWriter.writePush(Segment.POINTER, 0);
                break;
            case LPAREN:
                expectPeek(LPAREN);
                parseExpression();
                expectPeek(RPAREN);
                break;
            case IDENT:
                expectPeek(IDENT);
                Symbol sym = symbolTable.resolve(currentToken.value());
                if (peekTokenIs(LPAREN) || peekTokenIs(DOT)) {
                    parseSubroutineCall();
                } else {
                    if (peekTokenIs(LBRACKET)) {
                        expectPeek(LBRACKET);
                        parseExpression();
                        vmWriter.writePush(kind2Segment(sym.kind()), sym.index());
                        vmWriter.writeArithmetic(Command.ADD);
                        expectPeek(RBRACKET);
                        vmWriter.writePop(Segment.POINTER, 1);
                        vmWriter.writePush(Segment.THAT, 0);
                    }
                }
                break;
            case MINUS:
            case NOT:
                expectPeek(MINUS, NOT);
                var op = currentToken.type;
                parseTerm();
                if (op == MINUS)
                    vmWriter.writeArithmetic(Command.NEG);
                else
                    vmWriter.writeArithmetic(Command.NOT);
                break;
            default:
                throw error(peekToken, "term expected");
        }
        printNonTerminal("/term");
    }

    void parseSubroutineCall() {
        var nArgs = 0;

        var ident = currentToken.value();
        var symbol = symbolTable.resolve(ident);
        var functionName = ident + ".";

        if (peekTokenIs(LPAREN)) {
            expectPeek(LPAREN);
            vmWriter.writePush(Segment.POINTER, 0);
            nArgs = parseExpressionList() + 1;
            expectPeek(RPAREN);
            functionName = className + "." + ident;
        } else {
            expectPeek(DOT);
            expectPeek(IDENT);

            if (symbol != null) {
                functionName = symbol.type() + "." + currentToken.value();
                vmWriter.writePush(kind2Segment(symbol.kind()), symbol.index());
                nArgs = 1;
            } else {
                functionName += currentToken.value();
            }
            
            expectPeek(LPAREN);
            nArgs += parseExpressionList();
            expectPeek(RPAREN);
        }
        vmWriter.writeCall(functionName, nArgs);
    }

    int parseExpressionList() {
        printNonTerminal("expressionList");
        var nArgs = 0;

        if (!peekTokenIs(RPAREN)) {
            parseExpression();
            nArgs = 1;
        }
        while (peekTokenIs(COMMA)) {
            expectPeek(COMMA);
            parseExpression();
            nArgs++;
        }
        printNonTerminal("/expressionList");
        return nArgs;
    }

    void compileOperators(TokenType type) {

        if (type == ASTERISK) {
            vmWriter.writeCall("Math.multiply", 2);
        } else if (type == SLASH) {
            vmWriter.writeCall("Math.divide", 2);
        } else {
            vmWriter.writeArithmetic(typeOperator(type));
        }
    }
}