import java.io.InputStream;

public class Lexer {

    private InputStream in;
    private int state;
    private static int X;
    private static int line, col;

    public Lexer(InputStream inputStream) {
        state = 0;
        in = inputStream;
        line = col = 0;
    }

    public void printFile() throws Exception {
        int c;
        while ((c = in.read()) != -1) {
            System.out.print((char) c);
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

    // Zustände des Automaten
    public abstract class State {
        int nextState;

        public State(int nextState) {
            this.nextState = nextState;
        }

        abstract void action() throws Exception;
    }

    // lesen
    public class l extends State {
        public l(int nextState) {
            super(nextState);
        }

        void action() throws Exception {
            X = in.read();
            if (X == '\n') {
                line++;
                col = 0;
            } else {
                col++;
            }
        }
    }

    // beenden
    public class b extends State {
        public b(int nextState) {
            super(nextState);
        }

        void action() {

        }
    }

    // schreiben
    public class sgl extends State {
        public sgl(int nextState) {
            super(nextState);
        }

        void action() {

        }
    }

    // schreiben, lesen
    public class sl extends State {
        public sl(int nextState) {
            super(nextState);
        }

        void action() {

        }
    }

    // schreiben, lesen, beenden
    public class slb extends State {
        public slb(int nextState) {
            super(nextState);
        }

        void action() {
        }
    }

    // Automatentabelle
    State[] automatentabelle1 = {
            /* 0 1 2 3 4 5 6 7 */
            /* z0 */ new slb(0), new slb(1), new sgl(2), new sl(3), new slb(0), new sl(4), new sl(5), new b(0),
            /* z1 */ new b(0), new sl(1), new b(0), new b(0), new b(0), new b(0), new b(0), new b(0), new b(0),
            /* z2 */ new b(0), new sl(2), new sgl(2), new b(0), new b(0), new b(0), new b(0), new b(0), new b(0),
            /* z3 */ new b(0), new b(0), new b(0), new b(0), new sl(6), new b(0), new b(0), new b(0), new b(0),
            /* z4 */ new b(0), new b(0), new b(0), new b(0), new sl(7), new b(0), new b(0), new b(0), new b(0),
            /* z5 */ new b(0), new b(0), new b(0), new b(0), new sl(8), new b(0), new b(0), new b(0), new b(0),
            /* z6 */ new b(0), new b(0), new b(0), new b(0), new b(0), new b(0), new b(0), new b(0), new b(0),
            /* z7 */ new b(0), new b(0), new b(0), new b(0), new b(0), new b(0), new b(0), new b(0), new b(0),
            /* z8 */ new b(0), new b(0), new b(0), new b(0), new b(0), new b(0), new b(0), new b(0), new b(0)
    };

    public static void main(String args[]) throws Exception {
        String path = args[0];
        InputStream inputStream = Lexer.class.getResourceAsStream(path);
        if (inputStream == null) {
            System.out.println("File not found");
            return;
        }
        Lexer lexer = new Lexer(inputStream);
        lexer.printFile();
    }
}