package lisp;

import java.util.ArrayList;
import java.util.Iterator;

import lisp.RT.Runtime;
import lisp.specialoperators.Comma;
import lisp.specialoperators.CommaAt;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class List extends LispForm {
	private ArrayList<LispForm> children = new ArrayList<LispForm>();
	private int firstElement = 0;

	public List() {

	}

	public List(List list, int firstElement) {
		this.children = list.children;
		this.firstElement = firstElement;
	}

	public LispForm getHead() {
		if (firstElement < children.size())
			return children.get(firstElement);
		else
			return List.createEmptyList();
	}

	/**
	 * Certainly this implementation of tails makes sense only if lists are
	 * immutable.
	 * 
	 * @param list
	 * @return
	 */
	public List getTail() {
		if (firstElement < children.size() - 1)
			return new List(this, firstElement + 1);
		else
			return List.createEmptyList();
	}

	public void addAtFront(LispForm child) {
		children.add(0, child);
	}
	
	private LispForm handleLambda(LispForm form, SymbolTable st) throws SyntaxException{
		LispForm res = form.getLambda(st);
		if(res != null){
			form.compile(st);
			Factory.getMethodVisitor().visitInsn(Opcodes.POP); // in this case: ((lambda (x) (+ x x))) we do not need function object on stack
		}
		
		return res;
	}
	
	private Symbol handleSymbol(Symbol symbol, SymbolTable st){
		if (st.isSpecialOperator(symbol.getName()))
			return st.getNewSpecialOperatorInstance(symbol.getName());
		else
			return new Function(symbol.getName());
	}

	/*
	 * First element of list might be a special operator of function.
	 */
	private LispForm resolveFirstElement(LispForm form, SymbolTable st) throws SyntaxException {
		// the special case: ((lambda (x) (body))) - the first element of list is not a symbol
		LispForm res = handleLambda(form, st);
		
		if(res == null){ // if not lambda
			if(!(form instanceof Symbol))
				throw new SyntaxException("First element of the list is expected to be a symbol.");
			
			res = handleSymbol((Symbol) form, st);
		}
		
		res.setParent(this);
		return res;
	}

	public void compile(SymbolTable st) throws SyntaxException {
		if (children.size() > 0)
			resolveFirstElement(children.get(0), st).compile(st);
		// else //todo
	}
	
	public LispForm expandMacros(SymbolTable st) throws SyntaxException{
		if(children.size() > 0){
			if(children.get(0) instanceof Symbol){
				Symbol first = (Symbol) children.get(0);
				if(first.getName().equals("quote"))
					return this;
				
				java.util.List<LispForm> expandedChildren = new java.util.ArrayList<LispForm>();
				for(LispForm child : children)
					expandedChildren.add(child.expandMacros(st));
				
				if(Runtime.isMacro(first.getName())){
					List list = new List();
					list.setChildren(expandedChildren);
					Macro m = new Macro(first.getName());
					m.setParent(list);
					return m.expandMacros(st);
				}else{
					List res = new List();
					res.setChildren(expandedChildren);
					return res;
				}
			}
		}
		return this;
	}

	public void setChildren(java.util.List<LispForm> children) {
		for(LispForm child : children){
			this.children.add(child);
			child.setParent(this);
		}		
	}

	public java.util.List<LispForm> getChildren() {
		return children;
	}

	public java.util.List<LispForm> getParameters() {
		ArrayList<LispForm> params = new ArrayList<LispForm>();
		if (children.size() > 1)
			for (int i = 1; i < children.size(); ++i)
				if (children.get(i) instanceof Symbol)
					params.add(new Variable(((Symbol) children.get(i))
							.getName()));
				else
					params.add(children.get(i));

		return params;
	}

	public void addChild(LispForm form) {
		children.add(form);
		form.setParent(this);
	}

	public int size() {
		return children.size();
	}

	public boolean isDefun() {
		return children.size() > 0 && children.get(0) instanceof Symbol
				&& ((Symbol) children.get(0)).getName().equals("defun");
	}
	
	public boolean isCommaAt(){
		return children.size () > 0 && children.get(0) instanceof Symbol
				&& ((Symbol) children.get(0)).getName().equals("comma_at");
	}
	
	public boolean isMacro() {
		return children.size() > 0 && children.get(0) instanceof Symbol && ((Symbol) children.get(0)).getName().equals("defmacro");
	}

	public static List createForm(String name) {
		List res = new List();
		res.addChild(new Symbol(name));
		return res;
	}

	public static List createDefun(String functionName, LispForm arguments,
			LispForm body) {
		List defunForm = createForm("defun");
		defunForm.addChild(new Symbol(functionName));
		defunForm.addChild(arguments);
		defunForm.addChild(body);
		return defunForm;
	}

	public static List createEmptyList() {
		return new List();
	}

	public String toString() {
		if (children.size() == 0)
			return "NIL";
		else {
			StringBuffer buff = new StringBuffer("(");
			for (int i = 0; i < children.size() - 1; ++i) {
				buff.append(children.get(i).toString());
				buff.append(" ");
			}
			buff.append(children.get(children.size() - 1).toString());
			buff.append(")");
			return buff.toString();
		}
	}

	public boolean isEmpty() {
		return getChildren().size() == 0;
	}

	@Override
	public void generateYourself(SymbolTable symbolTable)
			throws SyntaxException {
		MethodVisitor mv = Factory.getMethodVisitor();

		mv.visitTypeInsn(Opcodes.NEW, "lisp/List");
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "lisp/List", "<init>", "()V");
		for (LispForm child : children) {
			mv.visitInsn(Opcodes.DUP);
			child.generateYourself(symbolTable);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "lisp/List", "addChild",
					"(Llisp/LispForm;)V");
		}
	}
	
	@Override
	public void compileIfComma(SymbolTable symbolTable)
			throws SyntaxException {
		if (children.size() > 0){
			if(children.get(0) instanceof Symbol && (((Symbol) children.get(0)).getName() == "comma" || ((Symbol) children.get(0)).getName() == "comma_at")){
				LispForm comma;
				if(((Symbol) children.get(0)).getName() == "comma")
					comma = new Comma();
				else
					comma = new CommaAt();
				comma.setParent(children.get(0).getParent());
				comma.compileIfComma(symbolTable);
			}else{
				MethodVisitor mv = Factory.getMethodVisitor();

				mv.visitTypeInsn(Opcodes.NEW, "lisp/List");
				mv.visitInsn(Opcodes.DUP);
				mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "lisp/List", "<init>", "()V");
				for (LispForm child : children) {
					mv.visitInsn(Opcodes.DUP);
					child.compileIfComma(symbolTable);
					if(!(child instanceof List && ((List) child).isCommaAt())) // CommaAt is treated specially because it might return multiple-value result
						mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "lisp/List", "addChild", "(Llisp/LispForm;)V");
				}
			}
		}
	}
	
	@Override
	public Function getLambda(SymbolTable st) {
		if(children.size() > 0)
			if(children.get(0) instanceof Symbol)
				return handleSymbol((Symbol) children.get(0), st).getLambda(st);
		return null;
	}
	
	public boolean hasRest() throws SyntaxException{
		for(int i=0; i<children.size(); ++i){
			if(!(children.get(i) instanceof Symbol))
				throw new SyntaxException("Elements of argument list should be symbols");
			if(((Symbol) children.get(i)).getName().equals("&rest"))
				if(i != children.size() - 2)
					throw new SyntaxException("&rest is expected to be followed by exactly one argument name");
				else
					return true;
		}
		return false;
	}
	
	public void removeRest() throws SyntaxException{
		java.util.List tmp = new ArrayList(children);
		for(int i=0; i<tmp.size(); ++i)
			if((tmp.get(i) instanceof Symbol) && ((Symbol) tmp.get(i)).getName().equals("&rest"))
				children.remove(i);
	}
	
	/**
	 * This method should be used only during runtime. It assumes that user has ensured that list is non-empty (eg. by calling isEmpty())
	 * @return
	 */
	public LispForm removeFirst(){
		return children.remove(0);
	}
}
