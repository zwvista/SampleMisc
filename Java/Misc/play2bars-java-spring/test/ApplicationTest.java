import static org.fest.assertions.api.Assertions.assertThat;
import static play.mvc.Http.Status.BAD_REQUEST;
import static play.mvc.Http.Status.OK;
import static play.mvc.Http.Status.SEE_OTHER;
import static play.test.Helpers.GET;
import static play.test.Helpers.contentAsString;
import static play.test.Helpers.fakeApplication;
import static play.test.Helpers.fakeRequest;
import static play.test.Helpers.route;
import static play.test.Helpers.running;
import static play.test.Helpers.testServer;

import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import models.Bar;
import play.data.Form;
import play.libs.ws.WS;
import play.mvc.Http.RequestBuilder;
import play.mvc.Result;
import play.twirl.api.Content;

// todo: not using the right spring context when using fakeApplication()
public class ApplicationTest {

    @Test
    public void indexTemplate() {
        running(fakeApplication(), new Runnable() {
            public void run() {
                Form<Bar> form = Form.form(Bar.class);
                Content html = views.html.index.render(form);
                assertThat(html.contentType()).isEqualTo("text/html");
                assertThat(contentAsString(html)).contains("Welcome");
            }
        });
    }

    @Test
    public void callIndex() {
        running(fakeApplication(), new Runnable() {
            public void run() {
		        Result result = route(controllers.routes.Application.index());
		        assertThat(result.status()).isEqualTo(OK);
		        assertThat(result.contentType()).isEqualTo("text/html");
		        assertThat(result.charset()).isEqualTo("utf-8");
		        assertThat(contentAsString(result)).contains("Welcome");
            }
        });
    }

    @Test
    public void callAddBar() {
        running(fakeApplication(), new Runnable() {
            public void run() {
                Map<String, String> formParams = new HashMap<String, String>();
                formParams.put("name", "foo");
                
                RequestBuilder requestBuilder = fakeRequest(controllers.routes.Application.addBar())
                		.bodyForm(formParams);
                
                Result result = route(requestBuilder);
                assertThat(result.status()).isEqualTo(SEE_OTHER);
            }
        });
    }
	
	@Test
    public void callAddBarEmpty() {
        running(fakeApplication(), new Runnable() {
            public void run() {
                Result result = route(controllers.routes.Application.addBar());
                assertThat(result.status()).isEqualTo(BAD_REQUEST);
				assertThat(contentAsString(result)).contains("The name is required");
            }
        });
    }

    @Test
    public void callListBars() {
        running(fakeApplication(), new Runnable() {
            public void run() {
                Map<String, String> formParams = new HashMap<String, String>();
                formParams.put("name", "foo");

                RequestBuilder requestBuilder = fakeRequest(controllers.routes.Application.addBar())
                		.bodyForm(formParams);

                route(requestBuilder);
                
                Result result = route(controllers.routes.Application.listBars());
                assertThat(result.status()).isEqualTo(OK);
                assertThat(result.contentType()).isEqualTo("application/json");
                assertThat(contentAsString(result)).startsWith("[");
                assertThat(contentAsString(result)).contains("foo");
            }
        });
    }

    @Test
    public void barsRoute() {
        running(fakeApplication(), new Runnable() {
            public void run() {
                Result result = route(fakeRequest(GET, "/bars"));
                assertThat(result).isNotNull();
            }
        });
    }

    @Test
    public void realBarsRequest() {
        running(testServer(3333), new Runnable() {
            public void run() {
                assertThat(WS.url("http://localhost:3333/bars").get().get(1000).getStatus()).isEqualTo(OK);
            }
        });
    }

}
