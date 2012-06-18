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
				throw new SyntaxException("Unexpected token '" + token.getValue() + "'");
			
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
			}else if(token.getCode() == Token.QUOTE || token.getCode() == Token.BACKQUOTE || token.getCode() == Token.COMMA || token.getCode() == Token.COMMA_AT || token.getCode() == Token.HASH){
				List newList = new List();
				if(token.getCode() == Token.QUOTE){
					newList.addChild(new Symbol("quote"));
				}else if(token.getCode() == Token.BACKQUOTE){
					newList.addChild(new Symbol("backquote"));
				}else if(token.getCode() == Token.COMMA){
					newList.addChild(new Symbol("comma"));
				}else if(token.getCode() == Token.COMMA_AT){
					newList.addChild(new Symbol("comma_at"));
				}else if(token.getCode() == Token.HASH){
					newList.addChild(new Symbol("hash"));
				}
				java.util.List<LispForm> children = read(tokenizer, st, openedParenthesis);
				if(children.size() == 0)
					throw new SyntaxException("Trailing comma");
				
				newList.addChild(children.get(0));
				newForm = newList;
				res.add(newForm);
				
				for(int i=1; i<children.size(); ++i)
					res.add(children.get(i));
				
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
	
	public static void main(String [] args){
		Parser p = new Parser();
		Tokenizer tokenizer = new Tokenizer();
		//tokenizer.loadInput("(print 77) (print 34)");
		//tokenizer.loadInput("(print (+ 3 (* 4 5)))");
		//tokenizer.loadInput("(print (let ((a 10)) (setq a 12)))");
		//tokenizer.loadInput("(print (let ((x 2) (y 6)) (progn (setq x 4 y 8) (+ x y))))");
		//tokenizer.loadInput("(print (if (< 89 5) 10 13))");
		//tokenizer.loadInput("(print (quote (quote (x))))");
		//tokenizer.loadInput("(print (let ((ff (lambda (x) (* 2 x)))) (funcall ff 5)))");
		tokenizer.loadInput("(print (let ((ff (lambda (x) (* 2 x)))) (funcall ff 5)))");
	}
}
