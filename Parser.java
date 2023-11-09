import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;


public class Parser extends Lexer{

    Lexer lexer = new Lexer();

    public class Edge {
        int next;
        int alt;
        public Edge(int n, int a) {
            next = n;
            alt = a;
        }
        public Edge() {}
    }

    public class EdgeNil extends Edge {
        public EdgeNil(int n, int a) {
            super(n,a);
        }
    }

    public class EdgeSymbol extends Edge {
        int symbol;
        public EdgeSymbol(int s, int n, int a) {
            super(n,a);
            symbol = s;
        }
    }

    public class EdgeToken extends Edge {
        Token token;
        public EdgeToken(Token t, int n, int a) {
            super(n,a);
            token = t;
        }
    }   

    public class EdgeGraph extends Edge {
        Edge graph;
        public EdgeGraph(Edge[] g, int n, int a) {
            super(n,a);
            graph = g;
        }
    }

    public class EdgeEnd extends Edge {
        public EdgeEnd(int n, int a) {
            super(n,a);
        }
    }

    Edge[] gProgramm;
    Edge[] gBlock;
    Edge[] gExpr;
    Edge[] gTerm;
    Edge[] gStatement;
    Edge[] gFact;
    Edge[] gCondition;

    public Parser() {
        gProgramm = new Edge[] {
            new EdgeGraph(gBlock, 1, 0),
            new EdgeSymbol((int)'.', 2, 0),
            new EdgeEnd(0, 0)
        };

        gBlock = new Edge[] {

        };

        gExpr = new Edge[] {
            new EdgeSymbol((int)'-', 2, 1),
            new EdgeGraph(gTerm, 3, 0),
            new EdgeNil(4,5),
            new EdgeSymbol((int)'+', 6, 5),
            new EdgeSymbol((int)'-', 6, 7),
            new EdgeGraph(gTerm, 3, 0),
            new EdgeNil(8, 4),
            new EdgeEnd(0, 0)
        };

        gTerm = new Edge[] {
            new EdgeGraph(gFact, 1, 0),
            new EdgeNil(2,3),
            new EdgeSymbol((int)'*', 4, 0),
            new EdgeSymbol((int)'/', 4, 0),
            new EdgeGraph(gFact, 1, 0),
            new EdgeNil(7, 4),
            new EdgeEnd(0, 0)
        };

        gStatement = new Edge[] {

        };

        gFact = new Edge[] {
            new EdgeToken(lexer.new Token(TokenType.IDENT, null), 5, 1),
            new EdgeToken(lexer.new Token(TokenType.NUM, null), 5, 2),
            new EdgeSymbol((int)'(', 3, 0),
            new EdgeGraph(gExpr, 4, 0),
            new EdgeSymbol((int)'(', 5, 0),
            new EdgeEnd(0, 0)
        };

        gCondition = new Edge[] {

        };
        
    }


    

    public static void main(String args[]) {
        
    }
}