package Image;

import java.util.*;
import javax.swing.*;
import newtool.ImageResource;

public class MainFrame extends javax.swing.JFrame
{
  private static final long serialVersionUID = 1;
  private static final ResourceBundle res = ResourceBundle.getBundle("Image.messages");
  private final ImageResource imageRes = new ImageResource(this);

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
  
  private JLabel AddText(String key)
  {
	JLabel label = new JLabel();
	
    label.setText(res.getString(key));
    getContentPane().add(label);
    return label;
  }
  
  private JLabel AddImage(String name)
  {
    JLabel label = new JLabel();
	
    label.setIcon(imageRes.getImage(name));
    getContentPane().add(label);
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
	  
	  AddText("Flag");
	  AddImage("flag");

	  pack();
	  setSize(250, 150);
	}
	catch (Exception e)
	{
      e.printStackTrace();
	}
  }
}