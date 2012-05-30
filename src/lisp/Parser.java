package lisp;

import java.io.IOException;
import java.util.ArrayList;



public class Parser {
	private java.util.List<LispForm> read(ITokenizer tokenizer, SymbolTable st, int openedParenthesis) throws SyntaxException, IOException{
		ArrayList<LispForm> res = new ArrayList<LispForm>();
		
		Token token = tokenizer.nextToken();
		LispForm newForm;
		while(token.getCode() != Token.EOF){
			if(token.getCode() == Token.UNEXPECTED)
				throw new SyntaxException("Unexpected token");
			
			if(token.getCode() == Token.COMMENT){
				token = tokenizer.nextToken();
				continue;
			}
				
			if(token.getCode() == Token.OPENING_PARENTHESIS){
				List newList = new List();
				java.util.List<LispForm> children = read(tokenizer, st, openedParenthesis + 1);
				for(LispForm child : children)
					newList.addChild(child);
				
				newForm = newList;
			}else if(token.getCode() == Token.CLOSING_PARENTHESIS){
				if(openedParenthesis < 1)
					throw new SyntaxException("Unmatched close parenthesis");
				
				return res;
			}else if(token.getCode() == Token.INT){
				newForm = new Int(token.getValue());
			}else
				newForm = new Symbol(token.getValue());
			
			
			res.add(newForm);
			token = tokenizer.nextToken();
		}
		
		if(token.getCode() == Token.EOF && openedParenthesis != 0)
			throw new SyntaxException("Unexpected end of input");
		
		return res;
	}
	
	public java.util.List<LispForm> parse(ITokenizer tokenizer) throws SyntaxException, IOException{
		return read(tokenizer, new SymbolTable(), 0);
	}
	
	private java.util.List<LispForm> addTopLevelToMain(java.util.List<LispForm> tree){
		java.util.List<LispForm> res = new ArrayList<LispForm>();
		
		List progn = List.createForm("progn");
		List mainDefun = List.createDefun("Main", List.createEmptyList(), progn);
		
		for(LispForm it : tree)
			if(it instanceof List && !((List) it).isDefun())
				progn.addChild(it);
			else
				res.add(it);
		
		if(progn.getParameters().size() > 0)
			res.add(mainDefun);
		return res;
	}
	
	public void compile(ITokenizer tokenizer){
		try {
			java.util.List<LispForm> tree = parse(tokenizer);
			tree = addTopLevelToMain(tree);
			
			SymbolTable st = new SymbolTable();
			for(LispForm it : tree)
				it.compile(st);
			
		} catch (SyntaxException e) {
			e.getMessage();
			e.printStackTrace();
		} catch (IOException e) {
			e.getMessage();
			e.printStackTrace();
		}
	}
	
	public static void main(String [] args){
		Parser p = new Parser();
		Tokenizer tokenizer = new Tokenizer();
		//tokenizer.loadInput("(print 77) (print 34)");
		//tokenizer.loadInput("(print (+ 3 (* 4 5)))");
		//tokenizer.loadInput("(print (let ((a 10)) (setq a 12)))");
		//tokenizer.loadInput("(print (let ((x 2) (y 6)) (progn (setq x 4 y 8) (+ x y))))");
		//tokenizer.loadInput("(print (if (< 89 5) 10 13))");
		tokenizer.loadInput("(print (quote (quote (x))))");
		p.compile(tokenizer);
	}
}
