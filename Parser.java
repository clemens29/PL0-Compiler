import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;


public class Parser extends Lexer{

    public class Edge {
        int next;
        int alt;
        public Edge(int n, int a) {
            next = n;
            alt = a;
        }
        public Edge() {}
    }

    public class EdgeNil extends Edge {}

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
        public EdgeGraph(Edge g, int n, int a) {
            super(n,a);
            graph = g;
        }
    }

    public Parser() {

    }
    


    

   
}