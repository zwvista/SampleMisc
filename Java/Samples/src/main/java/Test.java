import java.util.Arrays;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Test {

    public static void main(String[] args) {
        String s = "123-4567-89";
        Pattern r = Pattern.compile("\\d{3}-(\\d{4})-\\d{2}");
        Matcher m = r.matcher(s);
        if (m.find())
            for (int i = 0; i <= m.groupCount(); i++)
                System.out.printf("group %d : %s\n", i, m.group(i));

        // https://stackoverflow.com/questions/19737653/what-is-the-equivalent-of-regex-replace-with-function-evaluation-in-java-7
        r = Pattern.compile("\\d+");
        m = r.matcher(s);
        StringBuffer sb = new StringBuffer();
        while (m.find())
            m.appendReplacement(sb, new StringBuffer(m.group(0)).reverse().toString());
        m.appendTail(sb);
        System.out.println(sb.toString());

        r = Pattern.compile("%(begin|next|end)%");
        s = "%begin%hello%next%world%end%";
        System.out.println(Arrays.asList(r.split(s)));
    }
}

/*
group 0 : 123-4567-89
group 1 : 4567
321-7654-98
[, hello, world]
 */