package lisp;

import java.util.HashMap;

import lisp.specialoperators.Car;
import lisp.specialoperators.Cdr;
import lisp.specialoperators.Defun;
import lisp.specialoperators.Let;
import lisp.specialoperators.ListOperator;
import lisp.specialoperators.Progn;

class VariablesMap{
	private HashMap<String, Integer> variablesMap = new HashMap<String, Integer>();
	private VariablesMap previous = null;
	
	public VariablesMap(){		
	}
	
	public VariablesMap(VariablesMap previous, HashMap<String, Integer> initMap){
		this.previous = previous;
		variablesMap = initMap;
	}
	
	public int getAddress(String varName) throws SyntaxException{
		Integer addr = variablesMap.get(varName);
		if(addr != null)
			return addr;
		if(previous != null)
			return previous.getAddress(varName);
		throw new SyntaxException("The variable " + varName + " is unbound.");
	}
}

public class SymbolTable {
	private static final HashMap<String, Class<? extends SpecialOperator>> specialOperatorsMap = new HashMap<String, Class<? extends SpecialOperator>>();
	private VariablesMap variablesMap;
	
	static{
		specialOperatorsMap.put("defun", Defun.class);
		specialOperatorsMap.put("progn", Progn.class);
		specialOperatorsMap.put("let", Let.class);
		specialOperatorsMap.put("list", ListOperator.class);
		specialOperatorsMap.put("car", Car.class);
		specialOperatorsMap.put("cdr", Cdr.class);
	}
	
	public boolean isSpecialOperator(String name){
		return specialOperatorsMap.containsKey(name);
	}
	
	public SpecialOperator getNewSpecialOperatorInstance(String name){
		Class<? extends SpecialOperator> clazz = specialOperatorsMap.get(name);
		if(clazz != null)
			try {
				return clazz.newInstance();
			} catch (InstantiationException e) {
				return null;
			} catch (IllegalAccessException e) {
				return null;
			}
		return null;
	}
	
	public SymbolTable(){
		variablesMap = new VariablesMap();
	}
	
	public SymbolTable(SymbolTable previous, HashMap<String, Integer> initMap){
		variablesMap = new VariablesMap(previous.variablesMap, initMap);
	}
	
	public int getAddress(String name) throws SyntaxException{
		return variablesMap.getAddress(name);
	}
}
