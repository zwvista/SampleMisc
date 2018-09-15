package javafx;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;

public class WebViewDemo2 extends Application {

	public static void main(String[] args) {
		launch(args);
	}
    	
    @Override
    public void start(Stage stage) throws Exception {
        java.net.URL url = getClass().getResource("WebViewDemo2.fxml");
        Parent root = FXMLLoader.load(url);
    
        Scene scene = new Scene(root, 300, 275);
    
        stage.setTitle("FXML Welcome");
        stage.setScene(scene);
        stage.show();
    }

	
}
