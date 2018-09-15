/*
Code revised from Desktop Java Live:
http://www.sourcebeat.com/downloads/
*/

import java.awt.event.ActionEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import com.jgoodies.binding.PresentationModel;
import com.jgoodies.binding.adapter.BasicComponentFactory;
import com.jgoodies.binding.beans.Model;
import com.jgoodies.binding.value.ValueModel;
import com.jgoodies.forms.builder.DefaultFormBuilder;
import com.jgoodies.forms.layout.FormLayout;

public class PresentationModelPropertyChangeExample extends JPanel {
    private PersonBean personBean1;
    private PersonBean personBean2;
    private PresentationModel presentationModel;

    public PresentationModelPropertyChangeExample() {
        DefaultFormBuilder defaultFormBuilder = new DefaultFormBuilder(new FormLayout("p, 2dlu, p:g"));
        defaultFormBuilder.setDefaultDialogBorder();

        this.personBean1 = new PersonBean("Scott", "Delap");
        this.personBean2 = new PersonBean("Matt", "Raible");

        this.presentationModel = new PresentationModel(this.personBean1);
        this.presentationModel.addPropertyChangeListener(new BeanChannelChangeListener());

        ValueModel firstNameAdapter = presentationModel.getModel("firstName");
        ValueModel lastNameAdapter = presentationModel.getModel("lastName");

        JTextField firstNameTextField = BasicComponentFactory.createTextField(firstNameAdapter);
        JTextField lastNameTextField = BasicComponentFactory.createTextField(lastNameAdapter);

        defaultFormBuilder.append("First Name: ", firstNameTextField);
        defaultFormBuilder.append("Last Name: ", lastNameTextField);
        defaultFormBuilder.append(new JButton(new ChangeBeanAction()), 3);

        add(defaultFormBuilder.getPanel());
    }

    private class ChangeBeanAction extends AbstractAction {
        public ChangeBeanAction() {
            super("Change PersonBean");
        }

        public void actionPerformed(ActionEvent event) {
            if (presentationModel.getBean() == personBean1) {
                presentationModel.setBean(personBean2);
            } else {
                presentationModel.setBean(personBean1);
            }
        }
    }

    private class BeanChannelChangeListener implements PropertyChangeListener {
        public void propertyChange(PropertyChangeEvent evt) {
            JOptionPane.showMessageDialog(null, "Property " + evt.getPropertyName() + " Old fp.Value = " + evt.getOldValue() + ", New fp.Value = " + evt.getNewValue());
        }
    }

    public class PersonBean extends Model {
        private String firstName;
        private String lastName;

        public static final String FIRST_NAME_PROPERTY = "firstName";
        public static final String LAST_NAME_PROPERTY = "lastName";

        public PersonBean(String firstName, String lastName) {
            this.firstName = firstName;
            this.lastName = lastName;
        }

        public String getFirstName() {
            return firstName;
        }

        public void setFirstName(String firstName) {
            String oldValue = this.firstName;
            this.firstName = firstName;
            firePropertyChange(FIRST_NAME_PROPERTY, oldValue, this.firstName);
        }

        public String getLastName() {
            return lastName;
        }

        public void setLastName(String lastName) {
            String oldValue = this.lastName;
            this.lastName = lastName;
            firePropertyChange(LAST_NAME_PROPERTY, oldValue, this.lastName);
        }

        public String toString() {
            return getFirstName() + " " + getLastName();
        }
    }

    public static void main(String[] a){
      JFrame f = new JFrame("Presentation PropertyChange Example");
      f.setDefaultCloseOperation(2);
      f.add(new PresentationModelPropertyChangeExample());
      f.pack();
      f.setVisible(true);
    }
}
