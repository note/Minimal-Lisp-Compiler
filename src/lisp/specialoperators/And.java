package lisp.specialoperators;

import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

import lisp.Factory;
import lisp.Generator;
import lisp.LispForm;
import lisp.SpecialOperator;
import lisp.SymbolTable;
import lisp.SyntaxException;

public class And extends SpecialOperator {
	public And(){
		super("and");
	}
	
	@Override
	public void compile(SymbolTable symbolTable) throws SyntaxException {
		MethodVisitor mv = Factory.getMethodVisitor();
		java.util.List<LispForm> parameters = getParameters();
		
		if(parameters.size() == 0){
			Generator.generateT();
		}else{
			Label nil = new Label();
			Label popAndNil = new Label();
			Label end = new Label();
			for(int i=0; i<parameters.size()-1; ++i){
				parameters.get(i).compile(symbolTable);
				
				mv.visitMethodInsn(Opcodes.INVOKESTATIC,"lisp/RT/Runtime", "isNil","(Llisp/LispForm;)Z");
				mv.visitJumpInsn(Opcodes.IFNE, nil);	
			}
			
			// the last parameter is treaded specially
			parameters.get(parameters.size()-1).compile(symbolTable);
			// the value of last parameter might be returned
			mv.visitInsn(Opcodes.DUP);
			
			mv.visitMethodInsn(Opcodes.INVOKESTATIC,"lisp/RT/Runtime", "isNil","(Llisp/LispForm;)Z");
			mv.visitJumpInsn(Opcodes.IFNE, popAndNil);
			mv.visitJumpInsn(Opcodes.GOTO, end);
			
			mv.visitLabel(popAndNil);
			mv.visitInsn(Opcodes.POP);
			mv.visitLabel(nil);
			Generator.generateEmptyList();
			mv.visitLabel(end);
		}
	}
}
