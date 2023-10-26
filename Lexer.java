import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.Buffer;

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

public class Lexer {
    private InputStream in;
    private BufferedReader reader;
    private char currentChar;
    private StringBuilder currentTokenValue;

    public Lexer(InputStream inputStream) throws Exception {
        in = inputStream;
        reader = new BufferedReader(new InputStreamReader(in));
        currentChar = (char) reader.read();
        currentTokenValue = new StringBuilder();
    }

    // Zeichenklassen
    // 0: Sonderzeichen 1: Ziffern 2: Buchstaben
    // 3: : 4: = 5: < 6: > 7: sonstige
    static int[] zeichenklassen = {
            /* 0 1 2 3 4 5 6 7 8 9 A B C D E F */
            /* 0 */ 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
            /* 10 */ 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
            /* 20 */ 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            /* 30 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 0, 5, 4, 6, 0,
            /* 40 */ 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            /* 50 */ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0,
            /* 60 */ 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
            /* 70 */ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0
    };

    public Token getNextToken() throws Exception {
        int zeichenklasse;
        int nextState;
        while (currentChar != -1) {
            zeichenklasse = zeichenklassen[currentChar];
            nextState = 
        }
    
    }

    public static void main(String args[]) throws Exception {
        String path = args[0];
        InputStream inputStream = Lexer.class.getResourceAsStream(path);
        if (inputStream == null) {
            System.out.println("File not found");
            return;
        }
    }
}