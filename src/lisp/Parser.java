package lisp;

import java.util.ArrayList;

public class Parser {
	private java.util.List<ILispForm> read(Tokenizer tokenizer, int openedParenthesis) throws SyntaxException{
		ArrayList<ILispForm> res = new ArrayList<ILispForm>();
		
		Token token = tokenizer.nextToken();
		ILispForm newForm;
		boolean firstToken = true;
		while(token.getCode() != Token.EOF){
			if(token.getCode() == Token.UNEXPECTED)
				throw new SyntaxException("Unexpected token");
			
			if(token.getCode() == Token.COMMENT)
				continue;
			
			if(token.getCode() == Token.OPENING_PARENTHESIS){
				newForm = new List();
				((List) newForm).setChildren(read(tokenizer, openedParenthesis + 1));
			}else if(token.getCode() == Token.CLOSING_PARENTHESIS){
				if(openedParenthesis < 1)
					throw new SyntaxException("Unmatched close parenthesis");
				
				return res;
			}else{
				 if(firstToken){
					 if(token.getCode() != Token.SYMBOL)
						 throw new SyntaxException("Symbol expected");
					 
					 newForm = new Function(token.getValue());
				 }else{
					 if(token.getCode() == Token.SYMBOL)
						 newForm = new Variable(token.getValue());
					 else
						 newForm = new Int(token.getValue()); 
				 }
			}
			
			res.add(newForm);
			firstToken = false;
			token = tokenizer.nextToken();
		}
		
		if(token.getCode() == Token.EOF && openedParenthesis != 0)
			throw new SyntaxException("Unexpected end of input");
		
		return res;
	}
	
	public java.util.List<ILispForm> parse(Tokenizer tokenizer) throws SyntaxException{
		return read(tokenizer, 0);
	}
}
