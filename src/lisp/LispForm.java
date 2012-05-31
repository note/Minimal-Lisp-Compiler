package lisp;



public abstract class LispForm {
	LispForm parent = null;
	
	void setParent(LispForm parent){
		this.parent = parent;
	}
	
	public abstract void compile(SymbolTable symbolTable) throws SyntaxException;
	
	/**
	 * Generates bytecode that translates compile time structure to runtime structure.
	 *  
	 * @param symbolTable
	 * @throws SyntaxException
	 */
	public abstract void generateYourself(SymbolTable symbolTable) throws SyntaxException;
	
	public Function getLambda(SymbolTable symbolTable){
		return null;
	}
}
