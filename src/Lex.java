import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

enum TokenType {
    IDENTIFIER, NUMBER, PLUS, MINUS, 
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
                // Handle operators
                switch (currentChar) {
                    case '+':
                        currentChar = (char) reader.read();
                        return new Token(TokenType.PLUS, "+");
                    // Add cases
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
        Lex lexer = new Lex("../src/fakult.pl0");
        Token token;
        while ((token = lexer.getNextToken()) != null) {
            System.out.println("Token: " + token.type + ", Value: " + token.value);
        }
        lexer.close();
    }
}
