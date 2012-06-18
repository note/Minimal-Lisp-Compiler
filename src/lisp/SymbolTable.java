package lisp;

import java.util.ArrayList;
import java.util.HashMap;

import lisp.specialoperators.And;
import lisp.specialoperators.Backquote;
import lisp.specialoperators.Car;
import lisp.specialoperators.Cdr;
import lisp.specialoperators.Comma;
import lisp.specialoperators.Cons;
import lisp.specialoperators.Defmacro;
import lisp.specialoperators.Defun;
import lisp.specialoperators.Eval;
import lisp.specialoperators.Funcall;
import lisp.specialoperators.Hash;
import lisp.specialoperators.IfOperator;
import lisp.specialoperators.Lambda;
import lisp.specialoperators.Let;
import lisp.specialoperators.ListOperator;
import lisp.specialoperators.Progn;
import lisp.specialoperators.Quote;
import lisp.specialoperators.Setq;

public class SymbolTable {
	class VariablesMap{
		private HashMap<String, Integer> variablesMap = new HashMap<String, Integer>();
		private VariablesMap previous = null;
		
		public VariablesMap(){		
		}
		
		public VariablesMap(VariablesMap previous, HashMap<String, Integer> initMap) throws SyntaxException{
			for(String definedConstant : SymbolTable.definedConstants)
				if(initMap.containsKey(definedConstant))
					throw new SyntaxException(definedConstant + " names a defined constant, and cannot be used as a local variable.");
			
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
	
	private static final HashMap<String, Class<? extends SpecialOperator>> specialOperatorsMap = new HashMap<String, Class<? extends SpecialOperator>>();
	private static final ArrayList<String> definedConstants = new ArrayList<String>();
	private static HashMap<String, String> aliases = new HashMap<String, String>();
	private VariablesMap variablesMap;
	
	static{
		aliases.put("+", "_plus");
		aliases.put("-", "_minus");
		aliases.put("*", "_mul");
		aliases.put("<", "_lower");
		aliases.put(">", "_greater");
		
		definedConstants.add("NIL");
		definedConstants.add("T");
		
		specialOperatorsMap.put("defun", Defun.class);
		specialOperatorsMap.put("progn", Progn.class);
		specialOperatorsMap.put("let", Let.class);
		specialOperatorsMap.put("list", ListOperator.class);
		specialOperatorsMap.put("car", Car.class);
		specialOperatorsMap.put("cdr", Cdr.class);
		specialOperatorsMap.put("cons", Cons.class);
		specialOperatorsMap.put("setq", Setq.class);
		specialOperatorsMap.put("if", IfOperator.class);
		specialOperatorsMap.put("quote", Quote.class);
		specialOperatorsMap.put("lambda", Lambda.class);
		specialOperatorsMap.put("funcall", Funcall.class);
		specialOperatorsMap.put("defmacro", Defmacro.class);
		specialOperatorsMap.put("backquote", Backquote.class);
		specialOperatorsMap.put("comma", Comma.class);
		specialOperatorsMap.put("eval", Eval.class);
		specialOperatorsMap.put("hash", Hash.class);
		specialOperatorsMap.put("and", And.class);
	}
	
	/**
	 * 
	 * @param name - name of function or alias
	 * @return - name of function (if parameter name was alias it will return function name)
	 * @example for "+" will return "_plus"
	 */
	public static String getFunctionName(String name){
		if(aliases.containsKey(name))
			return aliases.get(name);
		return name;
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
	
	public SymbolTable(SymbolTable previous, HashMap<String, Integer> initMap) throws SyntaxException{
		variablesMap = new VariablesMap(previous.variablesMap, initMap);
	}
	
	public int getVariableAddress(String name) throws SyntaxException{
		return variablesMap.getAddress(name);
	}
}
