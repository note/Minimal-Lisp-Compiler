package lisp;



public abstract class LispForm {
	protected LispForm parent = null;
	
	public void setParent(LispForm parent){
		this.parent = parent;
	}
	
	public LispForm getParent(){
		return parent;
	}
	
	public abstract void compile(SymbolTable symbolTable) throws SyntaxException;
	
	/**
	 * Only List and Comma needs more interesting implementation
	 * 
	 * @param symbolTable
	 * @throws SyntaxException
	 */
	public void compileIfComma(SymbolTable symbolTable) throws SyntaxException{
		this.generateYourself(symbolTable);
	}
	
	public LispForm expandMacros(SymbolTable symbolTable) throws SyntaxException{
		return this;
	}
	
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
