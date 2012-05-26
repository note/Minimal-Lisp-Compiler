package lisp;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Tokenizer implements ITokenizer{
	private static class CodeAndPattern{
		int code;
		Pattern pattern;
		
		public CodeAndPattern(int code, String pattern){
			this.code = code;
			this.pattern = Pattern.compile(pattern);
		}
	}
	
	private final static String unsignedInteger = "[1-9]*[0-9]+";
	private final static String integer = "[\\+-]?" + unsignedInteger;
	private final static String begin = "^[\\s]*";
	private final static String end = "(\\)|[\\s]|\\Z)";
	private final static String exp = "[de][0-9]+";
	
	private static ArrayList<CodeAndPattern> patterns = new ArrayList<CodeAndPattern>();
	
	static{
		patterns.add(new CodeAndPattern(Token.OPENING_PARENTHESIS, begin + "(\\()"));
		patterns.add(new CodeAndPattern(Token.CLOSING_PARENTHESIS, begin + "(\\))"));
		patterns.add(new CodeAndPattern(Token.INT, begin + "(" + integer + ")" + end));	
		patterns.add(new CodeAndPattern(Token.FLOAT, begin + "(" + integer + ")e([0-9]+)" + end));
		patterns.add(new CodeAndPattern(Token.FLOAT, begin + "([\\+-]?[0-9]*)\\.[0-9]+(e[0-9]+)?" + end));
		patterns.add(new CodeAndPattern(Token.FLOAT, begin + "(" + integer + ")" + end));
		patterns.add(new CodeAndPattern(Token.DOUBLE, begin + "(" + integer + ")d[0-9]+" + end));
		patterns.add(new CodeAndPattern(Token.DOUBLE, begin + "([\\+-]?[0-9]*\\.[0-9]+d[0-9]+)" + end));
		patterns.add(new CodeAndPattern(Token.DOUBLE, begin + "([\\+-]?[0-9]+\\.[0-9]*d[0-9]+)" + end));
		patterns.add(new CodeAndPattern(Token.STRING, begin + "\"(.*)\"" + end));
		patterns.add(new CodeAndPattern(Token.SYMBOL, begin + "([^\\s\\(\\)'\"`,:;\\\\|]+)" + end));
		patterns.add(new CodeAndPattern(Token.COMMENT, begin + ";([^\n]*)"));
		patterns.add(new CodeAndPattern(Token.UNEXPECTED, begin + "(.+)" + end));
	}
	
	String input = null;
	
	public void loadInput(String text){
		input = text;
		
		Pattern windowsEndOfLinePattern = Pattern.compile("\r\n");
		Matcher matcher = windowsEndOfLinePattern.matcher(text);
		matcher.replaceAll("\n");
	}
	
	private Token getToken(CodeAndPattern cap){
		Token token = null;
		if(input != null){
			Matcher matcher = cap.pattern.matcher(input);
			if(matcher.find()){
				token = new Token(cap.code, input.substring(matcher.start(1), matcher.end(1)));
				input = input.substring(matcher.end(1));
			}
		}
		return token;
	}
	
	public Token nextToken(){
		Token token;
		
		Iterator<CodeAndPattern> it = patterns.iterator();
		while(it.hasNext()){
			token = getToken(it.next());
			if(token != null)
				return token;
		}
		return new Token(Token.EOF, "");
	}
}
