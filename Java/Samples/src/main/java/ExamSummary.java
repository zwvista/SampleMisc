import java.util.*;

/**
 * 80 minutes
 */
public class ExamSummary {
	private HashMap<String, ExamAllScores> data = new HashMap<String, ExamAllScores>();
	private ArrayList<ExamTopScoreIds> topScoredIds = new ArrayList<ExamTopScoreIds>();
	
	public enum Subject { MATH, SCIENCE, ENGLISH }
	
	private static final int SUBJECT_CNT = 3;
	
	static class ExamAllScores {
		int[] scores = new int[SUBJECT_CNT];
		double avg;
		
		ExamAllScores() {
			for(int i = 0; i < SUBJECT_CNT; i++) {
				scores[i] = 0;
			}
		}
		
		void computeAvg(){
			double sum = 0.0;
			for(int s : scores) {
				sum += s;
			}
			avg = sum / SUBJECT_CNT;
		}
	}
	
	static class ExamTopScoreIds {
		ArrayList<String> ids = new ArrayList<String>();
		int topScore = 0;
		
		void register(String studentId , int score) {
			if(score > topScore) {
				topScore = score;
				ids = new ArrayList<String>();
				ids.add(studentId);
			} else if (score == topScore) {
				ids.add(studentId);
			}
		}
	}
	
	public ExamSummary() {
		for(int i = 0; i < SUBJECT_CNT; i++) {
			topScoredIds.add(new ExamTopScoreIds());
		}
	}

	/**
	* Register Student ID, Subject and Score
	*/
	public void register(String studentId , Subject subject , int score) {
		if(!data.containsKey(studentId)) {
			data.put(studentId, new ExamAllScores());
		}
		
		ExamAllScores eas = data.get(studentId);
		eas.scores[subject.ordinal()] = score;
		eas.computeAvg();
		
		topScoredIds.get(subject.ordinal()).register(studentId, score);
	}
	
	/**
	* Return the score of the specified studentId and subject
	* @return score
	*/
	public int getScore(String studentId, Subject subject) {
		return data.get(studentId).scores[subject.ordinal()];
	}
	
	/**
	* Return the studentIds who scored the best in the specified subject
	* @return list_of_studentId
	*/
	public List<String> getTopScoreIdsBySubject(Subject subject) {
		return topScoredIds.get(subject.ordinal()).ids;
	}
	
	/**
	* Return the studentIds sorted by the average score
	* @return list_of_studentId
	*/
	public List<String> getIdsByAverage() {
		ArrayList<String> result = new ArrayList<String>(data.keySet());
		Collections.sort(result, new java.util.Comparator<String>() {
			public int compare(String s, String t) {
				return Double.compare(data.get(t).avg, data.get(s).avg);
			}
		});
		return result;
	}
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		ExamSummary es = new ExamSummary();
		es. register ("00kc001",Subject.MATH , 85);
		es. register ("00kc001",Subject.SCIENCE , 75);
		es. register ("00kc001",Subject.ENGLISH , 65);
		es. register ("00kc002",Subject.MATH , 60);
		es. register ("00kc002",Subject.SCIENCE , 80);
		es. register ("00kc002",Subject.ENGLISH , 70);
		es. register ("00kc003",Subject.MATH , 90);
		es. register ("00kc003",Subject.SCIENCE , 55);
		es. register ("00kc003",Subject.ENGLISH , 65);
		int score = es.getScore("00kc002",Subject.MATH);
		System.out.println(score);
		List<String> topScoreIds = es.getTopScoreIdsBySubject(Subject.ENGLISH);
		for(String s : topScoreIds) {
			System.out.println(s);
		}
		List<String> ids = es.getIdsByAverage();
		for(String s : ids) {
			System.out.println(s);
		}
	
	}

}
