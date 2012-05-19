package lisp;

import java.util.ArrayList;
import java.util.Iterator;



public class List implements ILispForm{
	private ArrayList<ILispForm> children = new ArrayList<ILispForm>();

	@Override
	public void compile(SymbolTable symbolTable) throws SyntaxException{
		if(children.size() > 0)
			children.get(0).compile(symbolTable);
	}
	
	public void setChildren(java.util.List<ILispForm> children){
		Iterator<ILispForm> it = children.iterator();
		while(it.hasNext())
			this.children.add(it.next());
	}
	
	public java.util.List<ILispForm> getChildren(){
		return children;
	}
	
	public void addChild(ILispForm form){
		children.add(form);
	}
	
	public int size(){
		return children.size();
	}
	
	public boolean isDefun(){
		return children.size() > 0 && children.get(0) instanceof SpecialOperator && ((SpecialOperator) children.get(0)).getName().equals("defun");
	}

}
