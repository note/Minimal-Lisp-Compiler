package lisp;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

public class FileTokenizer implements ITokenizer{
	BufferedReader reader;
	Tokenizer tokenizer = new Tokenizer();
	
	public void loadFile(String filename) throws FileNotFoundException{
		reader = new BufferedReader(new FileReader(filename));
	}
	
	public Token nextToken() throws IOException{
		Token token = tokenizer.nextToken();
		String strLine;
		while(token.getCode() == Token.EOF){
			strLine = reader.readLine();
			if(strLine == null)
				return new Token(Token.EOF, "");
			tokenizer.loadInput(strLine);
			token = tokenizer.nextToken();
		}
		return token;
	}
}
