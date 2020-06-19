import java.util.*;
import javax.swing.*;

public class MainFrame extends javax.swing.JFrame
{
  private static final long serialVersionUID = 1;

  public static void main(String[] args)
  {
	SwingUtilities.invokeLater(new Runnable()
	{
	  public void run()
	  {
        MainFrame inst = new MainFrame();
        inst.setLocationRelativeTo(null); 
        inst.setVisible(true);
	  }
    });
  }

  public MainFrame()
  {
    super();
    Initialize();
  }
  
  private void Initialize()
  {
    try
	{
      ResourceBundle.Control control = ResourceBundle.Control.getControl(ResourceBundle.Control.FORMAT_XML);
      ResourceBundle rb = ResourceBundle.getBundle("LabelsBundle", "en_US", control);
      
      JPanel contentPane = (JPanel)getContentPane();
	  contentPane.setLayout(new BoxLayout(contentPane, BoxLayout.Y_AXIS));

	  setTitle("Title");
	  setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);

	  pack();
	  setSize(300, 200);
	}
	catch (Exception e)
	{
      e.printStackTrace();
	}
  }
}