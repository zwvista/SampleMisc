package com.example.demo;

//import Fruit1Mapper;
//import com.example.demo.mapper2.Fruit2Mapper;
//import org.apache.ibatis.session.SqlSessionFactory;
//import org.mybatis.spring.SqlSessionFactoryBean;
//import org.mybatis.spring.mapper1.MapperFactoryBean;
//import org.springframework.beans.factory.annotation.Qualifier;
//import org.springframework.boot.autoconfigure.jdbc.DataSourceBuilder;
//import org.springframework.boot.context.properties.ConfigurationProperties;
//import org.springframework.context.annotation.Bean;
//import org.springframework.context.annotation.Configuration;
//import org.springframework.context.annotation.Primary;
//
//import javax.sql.DataSource;
//
//@Configuration
//public class MultipleDBConfig {
//
//    public static final String DATASOURCE_NAME_1 = "postgresDb1";
//    public static final String DATASOURCE_NAME_2 = "postgresDb2";
//
//    public static final String SQL_SESSION_FACTORY_NAME_1 = "sqlSessionFactory1";
//    public static final String SQL_SESSION_FACTORY_NAME_2 = "sqlSessionFactory2";
//
//    public static final String MAPPERS_PACKAGE_NAME_1 = "com.example.demo.mapper1";
//    public static final String MAPPERS_PACKAGE_NAME_2 = "com.example.demo.mapper2";
//
//
//    @Bean(name = DATASOURCE_NAME_1)
//    @Primary
//    @ConfigurationProperties(prefix = "sp.ds1")
//    public DataSource dataSource1() {
//        System.out.println("db1 datasource");
//        return DataSourceBuilder.create().build();
//    }
//
//    @Bean(name = DATASOURCE_NAME_2)
//    @ConfigurationProperties(prefix = "sp.ds2")
//    public DataSource dataSource2() {
//        System.out.println("db2 datasource");
//        return  DataSourceBuilder.create().build();
//    }
//
//    @Bean(name = SQL_SESSION_FACTORY_NAME_1)
//    @Primary
//    public SqlSessionFactory sqlSessionFactory1(@Qualifier(DATASOURCE_NAME_1) DataSource dataSource1) throws Exception {
//        System.out.println("sqlSessionFactory1");
//        SqlSessionFactoryBean sqlSessionFactoryBean = new SqlSessionFactoryBean();
//        sqlSessionFactoryBean.setTypeHandlersPackage(MAPPERS_PACKAGE_NAME_1);
//        sqlSessionFactoryBean.setDataSource(dataSource1);
//        SqlSessionFactory sqlSessionFactory = sqlSessionFactoryBean.getObject();
////        sqlSessionFactory.getConfiguration().addMapper(Fruit1Mapper.class);
////        sqlSessionFactory.getConfiguration().setMapUnderscoreToCamelCase(true);
////        sqlSessionFactory.getConfiguration().setJdbcTypeForNull(JdbcType.NULL);
//        return sqlSessionFactory;
//    }
//
//    @Bean(name = SQL_SESSION_FACTORY_NAME_2)
//    public SqlSessionFactory sqlSessionFactory2(@Qualifier(DATASOURCE_NAME_2) DataSource dataSource2) throws Exception {
//        System.out.println("sqlSessionFactory2");
//        SqlSessionFactoryBean diSqlSessionFactoryBean = new SqlSessionFactoryBean();
//        diSqlSessionFactoryBean.setTypeHandlersPackage(MAPPERS_PACKAGE_NAME_2);
//        diSqlSessionFactoryBean.setDataSource(dataSource2);
//        SqlSessionFactory sqlSessionFactory = diSqlSessionFactoryBean.getObject();
////        sqlSessionFactory.getConfiguration().addMapper(Fruit2Mapper.class);
////        sqlSessionFactory.getConfiguration().setMapUnderscoreToCamelCase(true);
////        sqlSessionFactory.getConfiguration().setJdbcTypeForNull(JdbcType.NULL);
//        return sqlSessionFactory;
//    }
//
////    @Bean
////    @Primary
////    public MapperScannerConfigurer mapperScannerConfigurer1() {
////        System.out.println("mapperScannerConfigurer1");
////        MapperScannerConfigurer configurer = new MapperScannerConfigurer();
////        configurer.setBasePackage(MAPPERS_PACKAGE_NAME_1);
////        configurer.setSqlSessionFactoryBeanName(SQL_SESSION_FACTORY_NAME_1);
////        return configurer;
////    }
////
////    @Bean
////    public MapperScannerConfigurer mapperScannerConfigurer2() {
////        System.out.println("mapperScannerConfigurer2");
////        MapperScannerConfigurer configurer = new MapperScannerConfigurer();
////        configurer.setBasePackage(MAPPERS_PACKAGE_NAME_2);
////        configurer.setSqlSessionFactoryBeanName(SQL_SESSION_FACTORY_NAME_2);
////        return configurer;
////    }
//
//
//    @Bean
//    public MapperFactoryBean<Fruit1Mapper> fruitMapper(@Qualifier(SQL_SESSION_FACTORY_NAME_1) final SqlSessionFactory sqlSessionFactory)
//        throws Exception {
//        MapperFactoryBean<Fruit1Mapper> factoryBean = new MapperFactoryBean<>(Fruit1Mapper.class);
//        factoryBean.setSqlSessionFactory(sqlSessionFactory);
//        return factoryBean;
//    }
//
//    @Bean
//    public MapperFactoryBean<Fruit2Mapper> Fruit2Mapper(@Qualifier(SQL_SESSION_FACTORY_NAME_2) final SqlSessionFactory sqlSessionFactory)
//        throws Exception {
//        MapperFactoryBean<Fruit2Mapper> factoryBean = new MapperFactoryBean<>(Fruit2Mapper.class);
//        factoryBean.setSqlSessionFactory(sqlSessionFactory);
//        return factoryBean;
//    }
//
//
//}
