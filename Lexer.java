import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;

enum TokenType {
    NIL, SYM, NUM, IDENT;
}

enum Token_Value {
    NIL(0), BEGIN(128), CALL(129), CONST(130), DO(131), END(132),
    IF(133), ODD(134), PROCEDURE(135), THEN(136), VAR(137), WHILE(138),
    EQ(139), NEQ(140), LE(141), LT(142), GE(143), GT(144);

    public int value;

    Token_Value(int value) {
        this.value = value;
    }

    public int getValue() {
        return this.value;
    }
}

public class Lexer {
    public InputStream in;
    public int currentState;
    public State nextState;
    public BufferedReader reader;
    public char currentChar;
    public String currentTokenValue;
    public Token t = new Token();
    public String currentToken;
    public boolean end;
    public String[] keywords = { "BEGIN", "CALL", "CONST", "DO", "END", "IF", "ODD", "PROCEDURE", "THEN",
            "VAR", "WHILE" };

    public Lexer() {
    }

    public Lexer(String file) throws Exception {
        in = Lexer.class.getResourceAsStream(file);
        if (in == null) {
            throw new IllegalArgumentException("File not found: " + file);
        }
        reader = new BufferedReader(new InputStreamReader(in));
        currentTokenValue = new String();
        currentToken = new String();
    }

    class Token {
        public TokenType type;
        public String value;
        public int line, col;

        public Token() {
            this.type = TokenType.NIL;
        }

        public Token(TokenType type, String value) {
            this.type = type;
            this.value = value;
            this.col = t.col;
            this.line = t.line;
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

    Token schlwort() {
        Token t = null;
        for (int i = 0; i < keywords.length; i++) {
            if (keywords[i].equals(currentTokenValue)) {
                t = new Token(TokenType.SYM, Integer.toString(Token_Value.valueOf(keywords[i]).getValue()));
                break;
            } else {
                t = new Token(TokenType.IDENT, currentTokenValue);
            }
        }
        return t;
    }

    void fl() {
        try {
            currentChar = (char) reader.read();
            if (currentChar == '\n') {
                t.line++;
                t.col = 0;
            } else
                t.col++;
        } catch (Exception e) {
            System.out.println("Fehler: " + e);
            System.exit(-1);
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
            case 0: // sonstige
                t = new Token(TokenType.SYM, currentTokenValue);
                break;
            case 1: // Zahl
                t = new Token(TokenType.NUM, currentTokenValue);
                break;
            case 2: // Buchstabe
                t = schlwort();
                break;
            case 6: // :=
                t = new Token(TokenType.SYM, "" + Token_Value.EQ.value);
                break;
            case 7: // <=
                t = new Token(TokenType.SYM, "" + Token_Value.LE.value);
                break;
            case 8: // >=
                t = new Token(TokenType.SYM, "" + Token_Value.GE.value);
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
        if (t.type == TokenType.SYM && t.value == ".")
            return t;
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
        if (path == null) {
            System.out.println("File not found");
            return;
        }
        Lexer lexer = new Lexer(path);
        Token token;
        while ((token = lexer.getNextToken()).type != TokenType.NIL) {
            System.out.println(token.type + " " + token.value);
        }
    }
}