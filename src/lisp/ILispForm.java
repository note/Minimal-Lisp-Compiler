package lisp;



public interface ILispForm {
	void compile(SymbolTable symbolTable) throws SyntaxException;
	
}
