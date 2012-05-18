package lisp;

import java.util.ArrayList;
import java.util.Iterator;

public class List implements ILispForm{
	private ArrayList<ILispForm> children = new ArrayList<ILispForm>();

	@Override
	public void compile(SymbolTable symbolTable) {
		// TODO Auto-generated method stub
		
	}
	
	public void setChildren(java.util.List<ILispForm> children){
		Iterator<ILispForm> it = children.iterator();
		while(it.hasNext())
			this.children.add(it.next());
	}

}
