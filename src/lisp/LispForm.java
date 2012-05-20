package lisp;



public abstract class LispForm {
	LispForm parent = null;
	
	void setParent(LispForm parent){
		this.parent = parent;
	}
	
	public abstract void compile(SymbolTable symbolTable) throws SyntaxException;
}
