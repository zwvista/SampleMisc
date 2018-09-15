import java.util.*;


/**
 * 60 minutes
 */
public final class Splitter extends Object {

	private boolean optionTrimResults = false;
	
	private boolean optionRemoveEmptyString = false;
	
	private boolean optionSplitByLen = false;
	
	private String separator;
	
	private int lenOfSubString;
	
	private Splitter(String str) {
		separator = str;
	}
	
	private Splitter(int len) {
		lenOfSubString = len;
		optionSplitByLen = true;
	}
	
	/**
	 * Divides a string into substring by recognizing a separator which is expressed as a single character
	 * @param ch the separator
	 */
	public static Splitter on(char ch) {
		return new Splitter(Character.toString(ch));
	}
	
	/**
	 * Divides a string into substring by recognizing a separator which is expressed as a literal string
	 * @param str the separator
	 */
	public static Splitter on(String str) {
		return new Splitter(str);
	}
	
	/**
	 * Divides a string into substring by using a fixed substring length
	 * @param len the substring length
	 */
	public static Splitter on(int len) {
		return new Splitter(len);
	}
	
	/**
	 * Trim the whitespace from the result strings
	 * @return the Splitter object
	 */
	public Splitter trimResults() {
		optionTrimResults = true;
		return this;
	}
	
	/**
	 * Remove empty strings from the results
	 * @return the Splitter object
	 */
	public Splitter removeEmptyString() {
		optionRemoveEmptyString = true;
		return this;
	}
	
	/**
	 * Invoke the split action
	 * @param text the string to be split
	 * @return the list of strings computed by dividing the string
	 */
	public List<String> split(String text) {
		String[] strs;
		if(optionSplitByLen) {
			int n = (text.length() + lenOfSubString - 1) / lenOfSubString;
			strs = new String[n];
			for(int i = 0, b = 0, e = lenOfSubString; i < n; i++, b += lenOfSubString, e += lenOfSubString) {
				strs[i] = text.substring(b, Math.min(e, text.length()));
			}
			
		} else {
			strs = text.split(separator);
		}
		
		List<String> result = new ArrayList<String>();
		for(String s : strs) {
			if (optionTrimResults) {
				s = s.trim();
			}
			if (!optionRemoveEmptyString || !s.isEmpty()) {
				result.add(s);
			}
		}
		
		return result;
	}
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		List<String> result = Splitter.on(5).split("1234567");
		for(String s : result) {
			System.out.println(s);
		}
		result = Splitter.on(',').removeEmptyString().trimResults().split("foo,,bar, quux");
		for(String s : result) {
			System.out.println(s);
		}
	}
}
