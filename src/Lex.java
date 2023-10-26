import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

enum TokenType {
    IDENTIFIER, NUMBER, PLUS, MINUS, TIMES, SLASH, ODD, EQL, NEQ, LSS, LEQ, GTR, GEQ, LPAREN, RPAREN, COMMA, SEMICOLON,
    PERIOD, BECOMES, BEGIN, END, IF, THEN, WHILE, DO, CALL, CONST, VAR, PROCEDURE, WRITE, READ;
}

class Token {
    public TokenType type;
    public String value;

    public Token(TokenType type, String value) {
        this.type = type;
        this.value = value;
    }
}

public class Lex {
    private BufferedReader reader;
    private char currentChar;
    private StringBuilder currentTokenValue;

    public Lex(String filename) throws IOException {
        reader = new BufferedReader(new FileReader(filename));
        currentChar = (char) reader.read();
        currentTokenValue = new StringBuilder();
    }

    public Token getNextToken() throws IOException {
        while (currentChar != -1) {
            if (Character.isWhitespace(currentChar)) {
                consumeWhitespace();
            } else if (Character.isLetter(currentChar)) {
                return readIdentifierOrKeyword();
            } else if (Character.isDigit(currentChar)) {
                return readNumber();
            } else {
                // Handle operators, punctuation, and other symbols
                switch (currentChar) {
                    case '+':
                        currentChar = (char) reader.read();
                        return new Token(TokenType.PLUS, "+");
                    // Add cases for other operators and symbols
                    // ...
                    default:
                        // Handle unknown characters or report an error
                        currentChar = (char) reader.read();
                }
            }
        }
        return null; // End of input
    }

    private void consumeWhitespace() throws IOException {
        while (Character.isWhitespace(currentChar)) {
            currentChar = (char) reader.read();
        }
    }

    private Token readIdentifierOrKeyword() throws IOException {
        currentTokenValue.setLength(0);
        while (Character.isLetterOrDigit(currentChar)) {
            currentTokenValue.append(currentChar);
            currentChar = (char) reader.read();
        }

        String tokenValue = currentTokenValue.toString();
        switch (tokenValue) {
            case "BEGIN":
                return new Token(TokenType.BEGIN, tokenValue);
            case "END":
                return new Token(TokenType.END, tokenValue);
            case "IF":
                return new Token(TokenType.IF, tokenValue);
            case "THEN":
                return new Token(TokenType.THEN, tokenValue);
            // Add cases for other keywords
            // ...
            default:
                return new Token(TokenType.IDENTIFIER, tokenValue);
        }
    }

    private Token readNumber() throws IOException {
        currentTokenValue.setLength(0);
        while (Character.isDigit(currentChar)) {
            currentTokenValue.append(currentChar);
            currentChar = (char) reader.read();
        }
        return new Token(TokenType.NUMBER, currentTokenValue.toString());
    }

    public void close() throws IOException {
        reader.close();
    }

    public static void main(String[] args) throws IOException {
        Lex lexer = new Lex("../PL0-Compiler/src/fakult.pl0");
        Token token;
        while ((token = lexer.getNextToken()) != null) {
            System.out.println("Token: " + token.type + ", Value: " + token.value);
        }
        lexer.close();
    }
}
