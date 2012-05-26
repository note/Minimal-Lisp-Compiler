package lisp;

import java.io.IOException;

public interface ITokenizer {
	public Token nextToken() throws IOException;
}
