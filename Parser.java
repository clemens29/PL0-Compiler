import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;

public class Parser extends Lexer{

    public Parser(InputStream inputStream) throws Exception {
        super(inputStream);
    }

    public static void main(String[] args) throws Exception{
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