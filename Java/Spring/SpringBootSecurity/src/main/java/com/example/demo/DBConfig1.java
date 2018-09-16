package com.example.demo;

import org.apache.ibatis.session.SqlSessionFactory;
import org.apache.ibatis.type.JdbcType;
import org.mybatis.spring.SqlSessionFactoryBean;
import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.autoconfigure.jdbc.DataSourceBuilder;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;

import javax.sql.DataSource;

@Configuration
@MapperScan(value="com.example.demo.mapper1", sqlSessionFactoryRef="sqlSessionFactory1")
public class DBConfig1 {
    @Primary
    @Bean
    @ConfigurationProperties(prefix = "sp.ds1")
    public DataSource dataSource1() {
        return DataSourceBuilder.create().build();
    }
    @Primary
    @Bean
    public DataSourceTransactionManager transactionManager1() {
        return new DataSourceTransactionManager(dataSource1());
    }
    @Primary
    @Bean
    public SqlSessionFactory sqlSessionFactory1() throws Exception {
        SqlSessionFactoryBean sessionFactory = new SqlSessionFactoryBean();
        sessionFactory.setDataSource(dataSource1());
        SqlSessionFactory sqlSessionFactory = sessionFactory.getObject();
        sqlSessionFactory.getConfiguration().setMapUnderscoreToCamelCase(true);
        sqlSessionFactory.getConfiguration().setJdbcTypeForNull(JdbcType.NULL);
        return sqlSessionFactory;
    }
}
