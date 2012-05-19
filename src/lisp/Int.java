package lisp;



public class Int implements ILispForm{
	int value;
	
	public Int(String value){
		this.value = Integer.parseInt(value);
	}

	@Override
	public void compile(SymbolTable symbolTable) {
		// TODO Auto-generated method stub
		
	}
	
}
