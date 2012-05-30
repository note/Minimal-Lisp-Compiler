package lisp;

import java.util.ArrayList;
import java.util.Iterator;



public class List extends LispForm{
	private ArrayList<LispForm> children = new ArrayList<LispForm>();
	private int firstElement = 0;
	
	public List(){
		
	}
	
	public List(List list, int firstElement){
		this.children = list.children;
		this.firstElement = firstElement;
	}
	
	public LispForm getHead(){
		if(firstElement < children.size())
			return children.get(firstElement);
		else
			return List.createEmptyList();
	}
	
	/**
	 * Certainly this implementation of tails makes sense only if lists are immutable.
	 * 
	 * @param list
	 * @return
	 */
	public List getTail(){
		if(firstElement < children.size() - 1)
			return new List(this, firstElement + 1);
		else
			return List.createEmptyList();
	}
	
	public void addAtFront(LispForm child){
		children.add(0, child);
	}

	/*
	 * First element of list might be a special operator of function.
	 */
	private LispForm resolveFirstElement(String name, SymbolTable st){
		LispForm res;
		if(st.isSpecialOperator(name))
			res = st.getNewSpecialOperatorInstance(name);
		else
			res = new Function(name);
		res.setParent(this);
		return res;
	}
	
	public void compile(SymbolTable st) throws SyntaxException{
		if(children.size() > 0){
			if(!(children.get(0) instanceof Symbol))
				throw new SyntaxException("First element of the list is expected to be a symbol.");
			resolveFirstElement(((Symbol) children.get(0)).getName(), st).compile(st);
		}
		//else //todo
	}
	
	public void setChildren(java.util.List<LispForm> children){
		Iterator<LispForm> it = children.iterator();
		while(it.hasNext())
			this.children.add(it.next());
	}
	
	public java.util.List<LispForm> getChildren(){
		return children;
	}
	
	public java.util.List<LispForm> getParameters(){
		ArrayList<LispForm> params = new ArrayList<LispForm>();
		if(children.size() > 1)
			for(int i=1; i<children.size(); ++i)
				if(children.get(i) instanceof Symbol)
					params.add(new Variable(((Symbol) children.get(i)).getName()));
				else
					params.add(children.get(i));
		
		return params;
	}
	
	public void addChild(LispForm form){
		children.add(form);
		form.setParent(this);
	}
	
	public int size(){
		return children.size();
	}
	
	public boolean isDefun(){
		return children.size() > 0 && children.get(0) instanceof Symbol && ((Symbol) children.get(0)).getName().equals("defun");
	}
	
	public static List createForm(String name){
		List res = new List();
		res.addChild(new Symbol(name));
		return res;
	}
	
	public static List createDefun(String functionName, LispForm arguments, LispForm body){
		List defunForm = createForm("defun");
		defunForm.addChild(new Symbol(functionName));
		defunForm.addChild(arguments);
		defunForm.addChild(body);
		return defunForm;
	}
	
	public static List createEmptyList(){
		return new List();
	}
	
	public String toString(){
		if(children.size() == 0)
			return "NIL";
		else{
			StringBuffer buff = new StringBuffer("(");
			for(int i=0; i<children.size()-1; ++i){
				buff.append(children.get(i).toString());
				buff.append(" ");
			}
			buff.append(children.get(children.size()-1).toString());
			buff.append(")");
			return buff.toString();
		}
	}
	
	public boolean isEmpty(){
		return getChildren().size() == 0;
	}
}
