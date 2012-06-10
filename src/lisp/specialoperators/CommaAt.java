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

public class CommaAt extends SpecialOperator {
	public CommaAt(){
		super("comma_at");
	}
	
	@Override
	public void compileIfComma(SymbolTable symbolTable) throws SyntaxException {
		java.util.List<LispForm> parameters = getParameters();
		MethodVisitor mv = Factory.getMethodVisitor();
		
		if(parameters.size() != 1)
			throw new SyntaxException("Special operator ,@ expects 1 argument (got " + parameters.size() + " arguments");
		
		//stack: ParentList
		
		parameters.get(0).compile(symbolTable);
		
		//stack: ParentList, Result
		
		mv.visitInsn(Opcodes.DUP);
		
		Label isList = new Label();
		mv.visitTypeInsn(Opcodes.INSTANCEOF, "lisp/List");
		
		//stack: ParentList, Result, BooleanIsList
		
		mv.visitJumpInsn(Opcodes.IFNE, isList);
		
		////////// BEGIN result is not a list:
		mv.visitInsn(Opcodes.DUP2);
		
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "lisp/List", "addChild", "(Llisp/LispForm;)V");
		Label end = new Label();
		
		//stack: ParentList, Result
		mv.visitJumpInsn(Opcodes.GOTO, end);
		////////// END result is not a list:
		
		////////// BEGIN result is a list:
		mv.visitLabel(isList);
		
		Label loop = new Label();
		mv.visitLabel(loop);
		
		mv.visitInsn(Opcodes.DUP);
		
		//stack: ParentList, Result, Result
		
		mv.visitTypeInsn(Opcodes.CHECKCAST, "lisp/List");
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "lisp/List", "isEmpty", "()Z");
		
		//stack: ParentList, Result, isEmptyBoolean
		
		mv.visitJumpInsn(Opcodes.IFNE, end);
		
		////////// BEGIN list is not empty:
		mv.visitInsn(Opcodes.DUP2);
		
		//stack: ParentList, Result, ParentList, Result
		mv.visitTypeInsn(Opcodes.CHECKCAST, "lisp/List");
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "lisp/List", "removeFirst", "()Llisp/LispForm;");
		
		//stack: ParentList, Result, ParentList, FirstItemOfResult
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "lisp/List", "addChild", "(Llisp/LispForm;)V");
		
		//stack: ParentList, Result
		mv.visitJumpInsn(Opcodes.GOTO, loop);
		////////// END list is not empty:
		////////// END result is a list:
		
		mv.visitLabel(end);
		mv.visitInsn(Opcodes.POP);
		mv.visitInsn(Opcodes.POP);
		//stack is empty
	}
}
