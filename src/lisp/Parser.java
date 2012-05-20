package lisp;

import java.util.ArrayList;
import java.util.Iterator;



public class Parser {
	private List createEmptyList(){
		return new List();
	}
	
	private java.util.List<ILispForm> read(Tokenizer tokenizer, SymbolTable st, int openedParenthesis) throws SyntaxException{
		ArrayList<ILispForm> res = new ArrayList<ILispForm>();
		ArrayList<ILispForm> params = new ArrayList<ILispForm>();
		
		Token token = tokenizer.nextToken();
		ILispForm newForm;
		Symbol currentFunction = null;
		boolean firstToken = true;
		while(token.getCode() != Token.EOF){
			if(token.getCode() == Token.UNEXPECTED)
				throw new SyntaxException("Unexpected token");
			
			if(token.getCode() == Token.COMMENT)
				continue;
			
			if(token.getCode() == Token.OPENING_PARENTHESIS){
				newForm = new List();
				((List) newForm).setChildren(read(tokenizer, st, openedParenthesis + 1));
				params.add(newForm);
			}else if(token.getCode() == Token.CLOSING_PARENTHESIS){
				if(openedParenthesis < 1)
					throw new SyntaxException("Unmatched close parenthesis");
				
				if(currentFunction != null)
					currentFunction.setParameters(params);
				currentFunction = null;
				return res;
			}else{
				 if(firstToken){
					 if(token.getCode() != Token.SYMBOL)
						 throw new SyntaxException("Symbol expected");
					 
					 currentFunction = Symbol.createSymbol(token.getValue(), st);
					 newForm = currentFunction;
					 params.clear();
				 }else{
					 if(token.getCode() == Token.SYMBOL)
						 newForm = new Variable(token.getValue());
					 else
						 newForm = new Int(token.getValue());
					 params.add(newForm);
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
		return read(tokenizer, new SymbolTable(), 0);
	}
	
	private java.util.List<ILispForm> addTopLevelToMain(java.util.List<ILispForm> tree){
		java.util.List<ILispForm> res = new ArrayList<ILispForm>();
		SymbolTable st = new SymbolTable();
		
		List mainDefun = List.createForm("defun", st);
		mainDefun.addChildToForm(Symbol.createSymbol("Main", st));
		mainDefun.addChildToForm(createEmptyList());
		List progn = List.createForm("progn", st);
		mainDefun.addChildToForm(progn);
		
		for(ILispForm it : tree){
			if(it instanceof List && !((List) it).isDefun())
				progn.addChildToForm(it);
			else
				res.add(it);
		}
		
		res.add(mainDefun);
		return res;
	}
	
	public void compile(Tokenizer tokenizer){
		try {
			java.util.List<ILispForm> tree = parse(tokenizer);
			tree = addTopLevelToMain(tree);
			
			SymbolTable st = new SymbolTable();
			Iterator<ILispForm> it = tree.iterator();
			while(it.hasNext())
				it.next().compile(st);
			
		} catch (SyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public static void main(String [] args){
		Parser p = new Parser();
		Tokenizer tokenizer = new Tokenizer();
		//tokenizer.loadInput("(print 77) (print 34)");
		tokenizer.loadInput("(let ((x 34)) (print (plus 10 x)))");
		p.compile(tokenizer);
	}
}
