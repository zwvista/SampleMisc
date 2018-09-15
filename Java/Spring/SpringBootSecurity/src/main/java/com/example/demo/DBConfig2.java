package com.example.demo;

import org.apache.ibatis.session.SqlSessionFactory;
import org.mybatis.spring.SqlSessionFactoryBean;
import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.autoconfigure.jdbc.DataSourceBuilder;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;

import javax.sql.DataSource;

@Configuration
@MapperScan(value="com.example.demo.mapper2", sqlSessionFactoryRef="sqlSessionFactory2")
public class DBConfig2 {
    @Bean
    @ConfigurationProperties(prefix = "sp.ds2")
    public DataSource dataSource2() {
        return DataSourceBuilder.create().build();
    }
    @Bean
    public DataSourceTransactionManager transactionManager2() {
        return new DataSourceTransactionManager(dataSource2());
    }
    @Bean
    public SqlSessionFactory sqlSessionFactory2() throws Exception {
        SqlSessionFactoryBean sessionFactory = new SqlSessionFactoryBean();
        sessionFactory.setDataSource(dataSource2());
        return sessionFactory.getObject();
    }
}
