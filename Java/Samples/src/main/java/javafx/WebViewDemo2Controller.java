package javafx;
import java.net.URL;
import java.util.ResourceBundle;

import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.web.WebView;

public class WebViewDemo2Controller implements Initializable {

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		
	}
	
    @FXML
    private WebView webView;

    @FXML
    void onLoadHtml(ActionEvent event) {
    	webView.getEngine().loadContent("<html><h1>Hello</h1><h2>Hello</h2></html>");
    }

    @FXML
    void onLoadPage(ActionEvent event) {
    	webView.getEngine().load("http://www.french-linguistics.co.uk/freng.exe?word=fleur");
    }


}
