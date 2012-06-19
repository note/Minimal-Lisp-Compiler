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

public class Apply extends Funcall {
	public Apply(){
		super("apply");
	}
	
	@Override
	public void compile(SymbolTable symbolTable) throws SyntaxException {
		java.util.List<LispForm> parameters = getParameters();
		MethodVisitor mv = Factory.getMethodVisitor();
//		Label end = new Label();
//		Label noRestEnd = new Label();
//		Label hasRest = new Label();
//		Label rest = new Label();
//		Label restEnd = new Label();
//		
		if(parameters.size() != 2)
			throw new SyntaxException("Special operator apply expects 2 arguments (got " + parameters.size() + " arguments");
		
		checkParameters(symbolTable, "apply");
		parameters.get(1).compile(symbolTable);
		//stack: String(functionName), int(expectedNumberOfParameters), LispForm
		
		Generator.generateCheckIfList(symbolTable, "Second argument of apply is expected to be a list");
		Generator.generateCastToList();
		//mv.visitInsn(Opcodes.DUP);
		//mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "lisp/List", "size", "()I");
		//stack: String(functionName), int(expectedNumberOfParameters), List(arguments)	
		
		mv.visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/RT/Runtime", "apply", "(Ljava/lang/String;ILlisp/List;)Llisp/LispForm;");
		
//		mv.visitLdcInsn(parameters.get(0));
//		mv.visitMethodInsn(Opcodes.INVOKESTATIC, "lisp/RT/Runtime", "hasRest", "(Ljava/lang/String;)Z");
//		mv.visitJumpInsn(Opcodes.IFNE, hasRest);
//		
//		mv.visitJumpInsn(Opcodes.IF_ICMPEQ, noRestEnd);
//
//		Generator.generateRuntimeException("Invalid number of arguments: " + parameters.size());
//
//		mv.visitLabel(noRestEnd);
//		mv.visitLdcInsn(parameters.get(0));
//		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "lisp/List", "treatAsListOfParameters", "(Ljava/lang/String;)V");
//		mv.visitJumpInsn(Opcodes.GOTO, end);
//		
//		mv.visitLabel(rest);
//		
//		mv.visitJumpInsn(Opcodes.IF_ICMPGT, restEnd);
//		
//		Generator.generateRuntimeException("Invalid number of arguments: " + parameters.size());
//		
//		mv.visitLabel(restEnd);
//		mv.visitLdcInsn(parameters.get(0));
//		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "lisp/List", "treatAsListOfParametersWithRest", "(ILjava/lang/String;)V");
//		
//		mv.visitLabel(end);
	}
}
