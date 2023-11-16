public class Parser extends Lexer {
    public class Edge {
        int next;
        int alt;

        public Edge(int n, int a) {
            next = n;
            alt = a;
        }

        public Edge() {
        }

        public boolean action() {
            return true;
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

    Token t;
    Lexer lexer;
    Edge[] gProgramm = new Edge[3];
    Edge[] gBlock = new Edge[20];
    Edge[] gExpr = new Edge[10];
    Edge[] gTerm = new Edge[8];
    Edge[] gStatement = new Edge[23]; 
    Edge[] gFact = new Edge[6];
    Edge[] gCondition = new Edge[11];

    public Parser(String file) throws Exception {
        t = new Token();
        lexer = new Lexer(file);

        // Für gProgramm
        gProgramm[0] = new EdgeGraph(gBlock, 1, 0) {public boolean action() {System.out.println("Start Programm"); return true;}};
        gProgramm[1] = new EdgeSymbol((int) '.', 2, 0);
        gProgramm[2] = new EdgeEnd(0, 0) {public boolean action() {System.out.println("End Programm");; return true;}};

        // Für gBlock
        gBlock[0] = new EdgeSymbol(Token_Value.CONST.value, 1, 6) {public boolean action() {System.out.println("Start Block"); return true;}};
        gBlock[1] = new EdgeToken(lexer.new Token(TokenType.IDENT, null), 2, 0);
        gBlock[2] = new EdgeSymbol((int) '=', 3, 0);
        gBlock[3] = new EdgeToken(lexer.new Token(TokenType.NUM, null), 4, 5);
        gBlock[4] = new EdgeSymbol((int) ',', 1, 0);
        gBlock[5] = new EdgeSymbol((int) ';', 7, 0);
        gBlock[6] = new EdgeNil(7, 0);
        gBlock[7] = new EdgeSymbol(Token_Value.VAR.value, 8, 11);
        gBlock[8] = new EdgeToken(lexer.new Token(TokenType.IDENT, null), 9, 0);
        gBlock[9] = new EdgeSymbol((int) ',', 8, 10);
        gBlock[10] = new EdgeSymbol((int) ';', 12, 0);
        gBlock[11] = new EdgeNil(12, 0);
        gBlock[12] = new EdgeSymbol(Token_Value.PROCEDURE.value, 13, 17);
        gBlock[13] = new EdgeToken(lexer.new Token(TokenType.IDENT, null), 14, 0);
        gBlock[14] = new EdgeSymbol((int) ';', 15, 0);
        gBlock[15] = new EdgeGraph(gBlock, 16, 0);
        gBlock[16] = new EdgeSymbol((int) ';', 12, 0);
        gBlock[17] = new EdgeNil(18, 0);
        gBlock[18] = new EdgeGraph(gStatement, 19, 0);
        gBlock[19] = new EdgeEnd(0, 0) {public boolean action() {System.out.println("End Block"); return true;}};

        // Für gExpr
        gExpr[0] = new EdgeSymbol((int) '-', 2, 0) {public boolean action() {System.out.println("Start Expression"); return true;}};
        gExpr[1] = new EdgeGraph(gTerm, 3, 0);
        gExpr[2] = new EdgeGraph(gTerm, 3, 0);
        gExpr[3] = new EdgeNil(4, 8);
        gExpr[4] = new EdgeSymbol((int) '+', 6, 5);
        gExpr[5] = new EdgeSymbol((int) '-', 7, 0);
        gExpr[6] = new EdgeGraph(gTerm, 3, 0);
        gExpr[7] = new EdgeGraph(gTerm, 3, 0);
        gExpr[8] = new EdgeNil(9, 0);
        gExpr[9] = new EdgeEnd(0, 0) {public boolean action() {System.out.println("End Expression"); return true;}};

        // Für gTerm
        gTerm[0] = new EdgeGraph(gFact, 1, 0) {public boolean action() {System.out.println("Start Term"); return true;}};
        gTerm[1] = new EdgeNil(2, 6);
        gTerm[2] = new EdgeSymbol((int) '*', 4, 3);
        gTerm[3] = new EdgeSymbol((int) '/', 5, 0);
        gTerm[4] = new EdgeGraph(gFact, 1, 0);
        gTerm[5] = new EdgeGraph(gFact, 1, 0);
        gTerm[6] = new EdgeNil(7, 0);
        gTerm[7] = new EdgeEnd(0, 0) {public boolean action() {System.out.println("End Term"); return true;}};

        // Für gStatement
        gStatement[0] = new EdgeToken(lexer.new Token(TokenType.IDENT, null), 8, 1) {public boolean action() {System.out.println("Start Statement"); return true;}};
        gStatement[1] = new EdgeSymbol(Token_Value.IF.value, 9, 2);
        gStatement[2] = new EdgeSymbol(Token_Value.WHILE.value, 10, 3);
        gStatement[3] = new EdgeSymbol(Token_Value.BEGIN.value, 11, 4);
        gStatement[4] = new EdgeSymbol(Token_Value.CALL.value, 13, 5);
        gStatement[5] = new EdgeSymbol((int) '?', 14, 6);
        gStatement[6] = new EdgeSymbol((int) '!', 15, 7);
        gStatement[7] = new EdgeNil(22, 0);
        gStatement[8] = new EdgeSymbol(Token_Value.EQ.value, 16, 0);
        gStatement[9] = new EdgeGraph(gCondition, 17, 0);
        gStatement[10] = new EdgeGraph(gCondition, 18, 0);
        gStatement[11] = new EdgeGraph(gStatement, 19, 12);
        gStatement[12] = new EdgeSymbol((int) ';', 11, 0);
        gStatement[13] = new EdgeToken(lexer.new Token(TokenType.IDENT, null), 22, 0);
        gStatement[14] = new EdgeToken(lexer.new Token(TokenType.IDENT, null), 22, 0);
        gStatement[15] = new EdgeGraph(gExpr, 22, 0);
        gStatement[16] = new EdgeGraph(gExpr, 22, 0);
        gStatement[17] = new EdgeSymbol(Token_Value.THEN.value, 20, 0);
        gStatement[18] = new EdgeSymbol(Token_Value.DO.value, 21, 0);
        gStatement[19] = new EdgeSymbol(Token_Value.END.value, 22, 0);
        gStatement[20] = new EdgeGraph(gStatement, 22, 0);
        gStatement[21] = new EdgeGraph(gStatement, 22, 0);
        gStatement[22] = new EdgeEnd(0, 0){public boolean action() {System.out.println("End Statement"); return true;}};

        // Für gFact
        gFact[0] = new EdgeToken(lexer.new Token(TokenType.IDENT, null), 5, 1) {public boolean action() {System.out.println("Start Factor");; return true;}};
        gFact[1] = new EdgeToken(lexer.new Token(TokenType.NUM, null), 5, 2);
        gFact[2] = new EdgeSymbol((int) '(', 3, 0);
        gFact[3] = new EdgeGraph(gExpr, 4, 0);
        gFact[4] = new EdgeSymbol((int) ')', 5, 0);
        gFact[5] = new EdgeEnd(0, 0) {public boolean action() {System.out.println("End Factor");; return true;}};

        // Für gCondition
        gCondition[0] = new EdgeSymbol(Token_Value.ODD.value, 2, 1) {public boolean action() {System.out.println("Start Condition");; return true;}};
        gCondition[1] = new EdgeGraph(gExpr, 3, 0);
        gCondition[2] = new EdgeGraph(gExpr, 5, 0);
        gCondition[3] = new EdgeSymbol(Token_Value.EQ.value, 9, 4);
        gCondition[4] = new EdgeSymbol(Token_Value.NEQ.value, 9, 4);
        gCondition[5] = new EdgeSymbol(Token_Value.LT.value, 9, 4);
        gCondition[6] = new EdgeSymbol(Token_Value.GT.value, 9, 4);
        gCondition[7] = new EdgeSymbol(Token_Value.LE.value, 9, 4);
        gCondition[8] = new EdgeSymbol(Token_Value.GE.value, 9, 4);
        gCondition[9] = new EdgeGraph(gExpr, 10, 0);
        gCondition[10] = new EdgeEnd(0, 0) {public boolean action() {System.out.println("End Condition");; return true;}};
    }

    boolean parse(Edge[] graph) throws Exception {
        Edge edge = graph[0];
        boolean success = false;
        if (t.type == TokenType.NIL) {
            t = lexer.getNextToken();
        }

        while (true) {
            if (edge instanceof EdgeNil) success = true;
            if (edge instanceof EdgeSymbol) success = (t.value != "" && ((EdgeSymbol)edge).symbol == (t.value).charAt(0));
            if (edge instanceof EdgeToken) success = (((EdgeToken)edge).token == t);
            if (edge instanceof EdgeGraph) success = parse(((EdgeGraph)edge).graph);
            if (edge instanceof EdgeEnd) return true;

            success = edge.action();

            if (!success) {
                if (edge.alt != 0) edge = graph[edge.alt];
                else return false;
            } else {
                if (edge instanceof EdgeSymbol || edge instanceof EdgeToken) t = lexer.getNextToken();
                edge = graph[edge.next];
            }
        }
    }

    public static void main(String[] args) throws Exception {
        String file = args[0];
        Parser Parser = new Parser(file);

        try {
            boolean success = Parser.parse(Parser.gProgramm);

            if (success) {
                System.out.println("Parsing erfolgreich!");
            } else {
                System.out.println("Fehler beim Parsen.");
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}