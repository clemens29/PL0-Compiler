import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

public class Parser extends Lexer {

    // Namensliste

    public List<Identifier> identifierList = new ArrayList<Identifier>();
    public List<Var> varList = new ArrayList<Var>();
    public List<Const> constList = new ArrayList<Const>();
    public List<Proc> procList = new ArrayList<Proc>();
    public Proc currentProc;
    int currentProcIndex = 0;

    public Const searchConst(long val) {
        for (Const c : constList) {
            if (c.value == val) {
                return c;
            }
        }
        return null;
    }

    public Identifier searchIdentifier(Proc procedure, String identifier) {
        for (Identifier i : identifierList) {
            if (i.name.equals(identifier) && i.indexProc == procedure.index) {
                return i;
            }
        }
        return null;

    }

    public Identifier searchIdentifierGlobal(String identifier) {
        for (Identifier i : identifierList) {
            if (i.name.equals(identifier) && i.indexProc == 0) {
                return i;
            }
        }
        return null;


    }

    public class Identifier {
        int indexProc;
        String name;

        public Identifier(int indexProc, Object o, int address, String n) {
            if (currentProc != null) {
                this.indexProc = currentProc.index;
            } else {
                this.indexProc = 0;
            }
            name = n;
        }
    }

    public class Var{
        int address;

        public Var() {
            this.address = currentProc.address;
            currentProc.address += 4;
            currentProc.list.add(new Identifier(currentProc.index, this, 0, "var"));
        }
    }

    public class Const{
        long value;
        int index;

        public Const(long val) {
            this.value = val;
            this.index = constList.size();
            if (searchConst(val) == null) {
                constList.add(this);
            }
            else {
                this.index = searchConst(val).index;
            }
            currentProc.list.add(new Identifier(currentProc.index, this, 0, "const"));
        }
    }

    public class Proc{
        int index;
        Proc parent;
        LinkedList<Identifier> list;
        int address;
        
        public Proc() {
            list = new LinkedList<Identifier>();
            address = 0;
            if (currentProc != null) {
                currentProc.list.add(new Identifier(currentProc.index, this, 0, "proc"));
                parent = currentProc;
            } else {
                parent = null;
            }
            index = currentProcIndex;
            currentProcIndex++;
        }



    }


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
        gProgramm[0] = new EdgeGraph(gBlock, 1, 0);
        gProgramm[1] = new EdgeSymbol((int) '.', 2, 0);
        gProgramm[2] = new EdgeEnd(0, 0);

        // Für gBlock
        gBlock[0] = new EdgeSymbol(Token_Value.CONST.value, 1, 6);
        gBlock[1] = new EdgeToken(lexer.new Token(TokenType.IDENT, null), 2, 0);
        gBlock[2] = new EdgeSymbol((int) '=', 3, 0);
        gBlock[3] = new EdgeToken(lexer.new Token(TokenType.NUM, null), 4, 0);
        gBlock[4] = new EdgeSymbol((int) ',', 1, 5);
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
        gBlock[19] = new EdgeEnd(0, 0);

        // Für gExpr
        gExpr[0] = new EdgeSymbol((int) '-', 2, 1);
        gExpr[1] = new EdgeGraph(gTerm, 3, 0);
        gExpr[2] = new EdgeGraph(gTerm, 3, 0);
        gExpr[3] = new EdgeNil(4, 0);
        gExpr[4] = new EdgeSymbol((int) '+', 6, 5);
        gExpr[5] = new EdgeSymbol((int) '-', 7, 8);
        gExpr[6] = new EdgeGraph(gTerm, 3, 0);
        gExpr[7] = new EdgeGraph(gTerm, 3, 0);
        gExpr[8] = new EdgeNil(9, 0);
        gExpr[9] = new EdgeEnd(0, 0);

        // Für gTerm
        gTerm[0] = new EdgeGraph(gFact, 1, 0);
        gTerm[1] = new EdgeNil(2, 0);
        gTerm[2] = new EdgeSymbol((int) '*', 4, 3);
        gTerm[3] = new EdgeSymbol((int) '/', 5, 6);
        gTerm[4] = new EdgeGraph(gFact, 1, 0);
        gTerm[5] = new EdgeGraph(gFact, 1, 0);
        gTerm[6] = new EdgeNil(7, 0);
        gTerm[7] = new EdgeEnd(0, 0);

        // Für gStatement
        gStatement[0] = new EdgeToken(lexer.new Token(TokenType.IDENT, null), 8, 1);
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
        gStatement[11] = new EdgeGraph(gStatement, 12, 0);
        gStatement[12] = new EdgeSymbol((int) ';', 11, 19);
        gStatement[13] = new EdgeToken(lexer.new Token(TokenType.IDENT, null), 22, 0);
        gStatement[14] = new EdgeToken(lexer.new Token(TokenType.IDENT, null), 22, 0);
        gStatement[15] = new EdgeGraph(gExpr, 22, 0);
        gStatement[16] = new EdgeGraph(gExpr, 22, 0);
        gStatement[17] = new EdgeSymbol(Token_Value.THEN.value, 20, 0);
        gStatement[18] = new EdgeSymbol(Token_Value.DO.value, 21, 0);
        gStatement[19] = new EdgeSymbol(Token_Value.END.value, 22, 0);
        gStatement[20] = new EdgeGraph(gStatement, 22, 0);
        gStatement[21] = new EdgeGraph(gStatement, 22, 0);
        gStatement[22] = new EdgeEnd(0, 0);

        // Für gFact
        gFact[0] = new EdgeToken(lexer.new Token(TokenType.IDENT, null), 5, 1);
        gFact[1] = new EdgeToken(lexer.new Token(TokenType.NUM, null), 5, 2);
        gFact[2] = new EdgeSymbol((int) '(', 3, 0);
        gFact[3] = new EdgeGraph(gExpr, 4, 0);
        gFact[4] = new EdgeSymbol((int) ')', 5, 0);
        gFact[5] = new EdgeEnd(0, 0);

        // Für gCondition
        gCondition[0] = new EdgeSymbol(Token_Value.ODD.value, 2, 1);
        gCondition[1] = new EdgeGraph(gExpr, 3, 0);
        gCondition[2] = new EdgeGraph(gExpr, 10, 0);
        gCondition[3] = new EdgeSymbol((int) '=', 9, 4);
        gCondition[4] = new EdgeSymbol((int) '#', 9, 5);
        gCondition[5] = new EdgeSymbol((int) '<', 9, 6);
        gCondition[6] = new EdgeSymbol((int) '>', 9, 7);
        gCondition[7] = new EdgeSymbol(Token_Value.LE.value, 9, 8);
        gCondition[8] = new EdgeSymbol(Token_Value.GE.value, 9, 0);
        gCondition[9] = new EdgeGraph(gExpr, 10, 0);
        gCondition[10] = new EdgeEnd(0, 0);
    }

    boolean parse(Edge[] graph) throws Exception {
        return parse(graph, true);
    }

    boolean parse(Edge[] graph, boolean topLevel) throws Exception {
        Edge edge = graph[0];
        boolean success = false;

        if (t.type == TokenType.NIL) {
            t = lexer.getNextToken();
        }

        while (true) {
            if (edge instanceof EdgeNil) {
                success = true;
            } else if (edge instanceof EdgeSymbol) {
                success = (t.type == TokenType.SYM)
                        && (((EdgeSymbol) edge).symbol == (t.value.length() == 1 ? (int) t.value.charAt(0)
                                : Integer.parseInt(t.value)));
            } else if (edge instanceof EdgeToken) {
                success = (((EdgeToken) edge).token.type == t.type);
            } else if (edge instanceof EdgeGraph) {
                success = parse(((EdgeGraph) edge).graph, false);
            } else if (edge instanceof EdgeEnd) {
                return true;
            }

            success = success && edge.action();

            if (!success) {
                if (edge.alt != 0) {
                    edge = graph[edge.alt];
                } else {
                    if (topLevel) {
                        System.out.println("Fehler in Zeile " + t.line + ", Spalte " + t.col + ": " + t.value);
                    }
                    return false;
                }
            } else {
                if (edge instanceof EdgeSymbol || edge instanceof EdgeToken) {
                    t = lexer.getNextToken();
                }
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