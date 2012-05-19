package lisp.specialoperators;

import lisp.Factory;
import lisp.ILispForm;
import lisp.SpecialOperator;
import lisp.SymbolTable;
import lisp.SyntaxException;

import org.objectweb.asm.Opcodes;

public class Progn extends SpecialOperator{

	public Progn() {
		super("progn");
	}
	
	public void compile(SymbolTable symbolTable) throws SyntaxException{
		if(parameters.size() > 0){
			for(int i=0; i<parameters.size(); ++i){
				parameters.get(i).compile(symbolTable);
				if(i+1 < parameters.size())
					Factory.getMethodVisitor().visitInsn(Opcodes.POP);
			}
		}/*else
			return NIL;*/
	}

}
