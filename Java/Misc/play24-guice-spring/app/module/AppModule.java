package module;

import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.guice.module.SpringModule;

import configs.AppConfig;

public class AppModule extends SpringModule {
	public AppModule() {
		super((DefaultListableBeanFactory) new AnnotationConfigApplicationContext(AppConfig.class).getAutowireCapableBeanFactory());
	}

}
