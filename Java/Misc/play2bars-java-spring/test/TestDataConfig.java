import javax.sql.DataSource;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import configs.DataConfig;

@Configuration
@EnableTransactionManagement
public class TestDataConfig extends DataConfig {
    
    @Bean
    @Override
    public DataSource dataSource() {
        final DriverManagerDataSource dataSource = new DriverManagerDataSource();
        dataSource.setDriverClassName("org.h2.Driver");
        dataSource.setUrl("jdbc:h2:./testdb"); // todo: why doesn't in-memory work here?
        return dataSource;
    }
    
}
