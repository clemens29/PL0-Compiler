import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;

enum TokenType {
    NIL, SYM, NUM, IDENT;
}

public class Lexer {
    private InputStream in;
    private int currentState;
    private State nextState;
    private BufferedReader reader;
    private char currentChar;
    private String currentTokenValue;
    private Token t;
    private String currentToken;
    private boolean end;

    public Lexer(InputStream inputStream) throws Exception {
        in = inputStream;
        reader = new BufferedReader(new InputStreamReader(in));
        currentTokenValue = new String();
        currentToken = new String();
    }

    class Token {
        private TokenType type;
        private String value;
        private int line, col;

        public Token(TokenType type, String value) {
            this.type = type;
            this.value = value;
        }
    }

    abstract class State {
        public int state;

        public State(int state) {
            this.state = state;
        }

        public int getState() {
            return this.state;
        }

        public abstract void action();
    }

    void fl() {
        try {
            currentChar = (char) reader.read();
        } catch (Exception e) {
            System.out.println("Fehler: " + e);
        }
    }

    void fs() {
        currentTokenValue += currentChar;
    }

    void fb() {
        switch (currentState) {
            case 3: // :
            case 4: // <
            case 5: // >
            case 6: // =
            case 7: // <=
            case 8: // >=
            case 0: // sonstige
                t = new Token(TokenType.SYM, currentTokenValue.toString());
                break;
            case 1: // Zahl
                t = new Token(TokenType.NUM, currentTokenValue.toString());
                break;
            case 2: // Buchstabe
                t = new Token(TokenType.IDENT, currentTokenValue.toString());
                break;
        }
        end = true;
    }

    class l extends State {
        public l(int state) {
            super(state);
        }

        // lesen
        public void action() {
            fl();
        }
    }

    class sl extends State {
        public sl(int state) {
            super(state);
        }

        // speichern und lesen
        public void action() {
            fs();
            fl();
        }
    }

    class sgl extends State {
        public sgl(int state) {
            super(state);
        }

        // speichern, groß und lesen
        public void action() {
            currentChar = Character.toUpperCase(currentChar);
            fs();
            fl();
        }
    }

    class slb extends State {
        public slb(int state) {
            super(state);
        }

        // speichern, lesen und beenden
        public void action() {
            fs();
            fl();
            fb();
        }
    }

    class b extends State {
        public b(int state) {
            super(state);
        }

        public void action() {
            fb();
        }
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

    // Automatentabelle
    State[][] automatentabelle = {
            /* 0 1 2 3 4 5 6 7 */
            /* z0 */ { new slb(0), new sl(1), new sgl(2), new sl(3), new slb(0), new sl(4), new sl(5), new l(0) },
            /* z1 */ { new b(0), new sl(1), new b(0), new b(0), new b(0), new b(0), new b(0), new b(0), new b(0) },
            /* z2 */ { new b(0), new sl(2), new sgl(2), new b(0), new b(0), new b(0), new b(0), new b(0), new b(0) },
            /* z3 */ { new b(0), new b(0), new b(0), new b(0), new sl(6), new b(0), new b(0), new b(0), new b(0) },
            /* z4 */ { new b(0), new b(0), new b(0), new b(0), new sl(7), new b(0), new b(0), new b(0), new b(0) },
            /* z5 */ { new b(0), new b(0), new b(0), new b(0), new sl(8), new b(0), new b(0), new b(0), new b(0) },
            /* z6 */ { new b(0), new b(0), new b(0), new b(0), new b(0), new b(0), new b(0), new b(0), new b(0) },
            /* z7 */ { new b(0), new b(0), new b(0), new b(0), new b(0), new b(0), new b(0), new b(0), new b(0) },
            /* z8 */ { new b(0), new b(0), new b(0), new b(0), new b(0), new b(0), new b(0), new b(0), new b(0) }
    };

    public Token getNextToken() throws Exception {
        int zeichenklasse;
        currentTokenValue = "";
        currentToken = "";
        end = false;
        while (!end) {
            try {
                zeichenklasse = zeichenklassen[currentChar];
            } catch (Exception e) {
                t = new Token(TokenType.NIL, currentTokenValue);
                break;
            }
            nextState = automatentabelle[currentState][zeichenklasse];
            nextState.action();
            currentState = nextState.getState();
        }
        return t;
    };

    public static void main(String args[]) throws Exception {
        String path = args[0];
        InputStream inputStream = Lexer.class.getResourceAsStream(path);
        if (inputStream == null) {
            System.out.println("File not found");
            return;
        }
        Lexer lexer = new Lexer(inputStream);
        Token token;
        while ((token = lexer.getNextToken()).type != TokenType.NIL) {
            System.out.println(token.type + " " + token.value);
        }
    }
}