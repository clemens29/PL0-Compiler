public class Parser extends Lexer {

    Lexer lexer = new Lexer();

    public class Edge {
        int next;
        int alt;

        public Edge(int n, int a) {
            next = n;
            alt = a;
        }

        public Edge() {
        }
    }

    public class EdgeNil extends Edge {
        public EdgeNil(int n, int a) {
            super(n, a);
        }
    }

    public class EdgeSymbol extends Edge {
        int symbol;

        public EdgeSymbol(int s, int n, int a) {
            super(n, a);
            symbol = s;
        }
    }

    public class EdgeToken extends Edge {
        Token token;

        public EdgeToken(Token t, int n, int a) {
            super(n, a);
            token = t;
        }
    }

    public class EdgeGraph extends Edge {
        Edge[] graph;

        public EdgeGraph(Edge[] g, int n, int a) {
            super(n, a);
            graph = g;
        }
    }

    public class EdgeEnd extends Edge {
        public EdgeEnd(int n, int a) {
            super(n, a);
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
                new EdgeSymbol((int) '.', 2, 0),
                new EdgeEnd(0, 0)
        };

        gBlock = new Edge[] {
                new EdgeSymbol(Token_Value.CONST.value, 1, 6),
                new EdgeToken(lexer.new Token(TokenType.IDENT, null), 2, 0),
                new EdgeSymbol((int) '=', 3, 0),
                new EdgeToken(lexer.new Token(TokenType.NUM, null), 4, 5),
                new EdgeSymbol((int) ',', 1, 0),
                new EdgeSymbol((int) ';', 7, 0),
                new EdgeNil(7, 0),
                new EdgeSymbol(Token_Value.VAR.value, 8, 11),
                new EdgeToken(lexer.new Token(TokenType.IDENT, null), 9, 0),
                new EdgeSymbol((int) ',', 8, 10),
                new EdgeSymbol((int) ';', 12, 0),
                new EdgeNil(12, 0),
                new EdgeSymbol(Token_Value.PROCEDURE.value, 13, 17),
                new EdgeToken(lexer.new Token(TokenType.IDENT, null), 14, 0),
                new EdgeSymbol((int) ';', 15, 0),
                new EdgeGraph(gBlock, 16, 0),
                new EdgeSymbol((int) ';', 12, 0),
                new EdgeNil(18, 0),
                new EdgeGraph(gStatement, 19, 0),
                new EdgeEnd(0, 0)
        };

        gExpr = new Edge[] {
                new EdgeSymbol((int) '-', 2, 0),
                new EdgeGraph(gTerm, 3, 0),
                new EdgeGraph(gTerm, 3, 0),
                new EdgeNil(4, 8),
                new EdgeSymbol((int) '+', 6, 5),
                new EdgeSymbol((int) '-', 7, 0),
                new EdgeGraph(gTerm, 3, 0),
                new EdgeGraph(gTerm, 3, 0),
                new EdgeNil(9, 0),
                new EdgeEnd(0, 0)
        };

        gTerm = new Edge[] {
                new EdgeGraph(gFact, 1, 0),
                new EdgeNil(2, 6),
                new EdgeSymbol((int) '*', 4, 3),
                new EdgeSymbol((int) '/', 5, 0),
                new EdgeGraph(gFact, 1, 0),
                new EdgeGraph(gFact, 1, 0),
                new EdgeNil(7, 0),
                new EdgeEnd(0, 0)
        };

        gStatement = new Edge[] {
                new EdgeToken(lexer.new Token(TokenType.IDENT, null), 8, 1),
                new EdgeSymbol(Token_Value.IF.value, 9, 2),
                new EdgeSymbol(Token_Value.WHILE.value, 10, 3),
                new EdgeSymbol(Token_Value.BEGIN.value, 11, 4),
                new EdgeSymbol(Token_Value.CALL.value, 13, 5),
                new EdgeSymbol((int) '?', 14, 6),
                new EdgeSymbol((int) '!', 15, 7),
                new EdgeNil(22, 0),
                new EdgeSymbol(Token_Value.EQ.value, 16, 0),
                new EdgeGraph(gCondition, 17, 0),
                new EdgeGraph(gCondition, 18, 0),
                new EdgeGraph(gStatement, 19, 12),
                new EdgeSymbol((int) ';', 11, 0),
                new EdgeToken(lexer.new Token(TokenType.IDENT, null), 22, 0),
                new EdgeToken(lexer.new Token(TokenType.IDENT, null), 22, 0),
                new EdgeGraph(gExpr, 22, 0),
                new EdgeGraph(gExpr, 22, 0),
                new EdgeSymbol(Token_Value.THEN.value, 20, 0),
                new EdgeSymbol(Token_Value.DO.value, 21, 0),
                new EdgeSymbol(Token_Value.END.value, 22, 0),
                new EdgeGraph(gStatement, 22, 0),
                new EdgeGraph(gStatement, 22, 0),
                new EdgeEnd(0, 0)
        };

        gFact = new Edge[] {
                new EdgeToken(lexer.new Token(TokenType.IDENT, null), 5, 1),
                new EdgeToken(lexer.new Token(TokenType.NUM, null), 5, 2),
                new EdgeSymbol((int) '(', 3, 0),
                new EdgeGraph(gExpr, 4, 0),
                new EdgeSymbol((int) ')', 5, 0),
                new EdgeEnd(0, 0)
        };

        gCondition = new Edge[] {
                new EdgeSymbol(Token_Value.ODD.value, 2, 1),
                new EdgeGraph(gExpr, 3, 0),
                new EdgeGraph(gExpr, 5, 0),
                new EdgeSymbol(Token_Value.EQ.value, 9, 4),
                new EdgeSymbol(Token_Value.NEQ.value, 9, 4),
                new EdgeSymbol(Token_Value.LT.value, 9, 4),
                new EdgeSymbol(Token_Value.GT.value, 9, 4),
                new EdgeSymbol(Token_Value.LE.value, 9, 4),
                new EdgeSymbol(Token_Value.GE.value, 9, 4),
                new EdgeGraph(gExpr, 10, 0),
                new EdgeEnd(0, 0)
        };
    }

    public static void main(String args[]) {

    }
}