import java.util.List;

import org.springframework.web.client.RestTemplate;

import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.TypeRef;
import com.jayway.jsonpath.spi.json.GsonJsonProvider;
import com.jayway.jsonpath.spi.mapper.GsonMappingProvider;

import lombok.ToString;

public class LollyRestApiTest {
	@ToString
	public static class VLANGUAGE {
		public int ID;
		public String LANGNAME;
		public String USTEXTBOOKID;
		public String USDICTID;
	}
	public static void main(String[] args) {
		String uri = "https://zwvista.000webhostapp.com/lolly/apisqlite.php/VLANGUAGES?transform=1";
		String json = new RestTemplate().getForObject(uri, String.class);
		Configuration conf = Configuration.builder()
				.jsonProvider(new GsonJsonProvider())
				.mappingProvider(new GsonMappingProvider())
				.build();
		List<VLANGUAGE> s = JsonPath.parse(json, conf).read("$.VLANGUAGES", new TypeRef<List<VLANGUAGE>>() {});
		s.forEach(System.out::println);
	}

}
