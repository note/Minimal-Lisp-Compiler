package lisp;


public class Symbol extends LispForm{
	public String name;
	
	Symbol(String name){
		this.name = name;
	}
	
	public String getName(){
		return name;
	}
	
	protected String getMethodDescriptor(int paramsNum){
		String methodDesc = "(";
		for(int i=0; i<paramsNum; ++i)
			methodDesc += "I";
		methodDesc += ")I";
		return methodDesc;
	}

	@Override
	public void compile(SymbolTable symbolTable) throws SyntaxException {
		// TODO Auto-generated method stub
		
	}
	
	protected java.util.List<LispForm> getParameters() throws SyntaxException{
		if(!(parent instanceof List))
			throw new SyntaxException("Function invocation not inside parenthesis");
		
		return ((List) parent).getParameters();
	}
}
