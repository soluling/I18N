import java.util.*;
import java.text.*;
import javax.swing.*;

public class MainFrame extends javax.swing.JFrame
{
  private static final ResourceBundle res = ResourceBundle.getBundle("Messages");

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

  private JLabel Add(String key)
  {
    JLabel label = new JLabel();

    getContentPane().add(label);
    label.setText(res.getString(key));
    return label;
  }

  private JLabel AddMessage(String key, Object... args)
  {
    JLabel label = new JLabel();

    getContentPane().add(label);
    label.setText(MessageFormat.format(res.getString(key), args)); 
    return label;
  }

  private void Initialize()
  {
	try
	{
      JPanel contentPane = (JPanel)getContentPane();
	  contentPane.setLayout(new BoxLayout(contentPane, BoxLayout.Y_AXIS));

	  setTitle(res.getString("Title"));
	  setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);

  	  Add("single1");
  	  Add("single2");
  	  Add("double1");
  	  Add("double2");

  	  Add("single'1");
  	  Add("single\'1");

  	  Add("plain");
	  Add("Plain");
	  Add("One Two");
	  Add("one=two");
	  Add("multiline");
	  Add("colon");
	  Add("colon ");
	  Add("unicode");
	  Add("NT");
	  
	  for (int i = 1; i <= 6; i++) 
        AddMessage("message" + i, 'A', 'B', 'C');

	  pack();
	  setSize(250, 500);
	}
	catch (Exception e)
	{
      e.printStackTrace();
	}
  }
}