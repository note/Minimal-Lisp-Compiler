package lisp;



public class Int extends LispForm{
	int value;
	
	public Int(String value){
		this.value = Integer.parseInt(value);
	}
	
	public int getValue(){
		return value;
	}

	@Override
	public void compile(SymbolTable symbolTable) {
		// TODO Auto-generated method stub
		
	}
	
}
