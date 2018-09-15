import static org.fest.assertions.api.Assertions.assertThat;

import org.junit.Test;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.AbstractTransactionalJUnit4SpringContextTests;

import configs.AppConfig;
import models.Bar;

@ContextConfiguration(classes={AppConfig.class, TestDataConfig.class})
public class BarTest extends AbstractTransactionalJUnit4SpringContextTests {
 
    @Test
    public void setBarName() {
        Bar bar = new Bar();
        bar.name = "foo";
        assertThat(bar.name).isEqualTo("foo");
    }

}
