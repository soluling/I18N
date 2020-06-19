package Driving;

import java.util.*;
import javax.swing.*;
import newtool.*;

public class MainFrame extends javax.swing.JFrame
{
  private static final long serialVersionUID = 1;
  private static final ResourceBundle res = ResourceBundle.getBundle("Driving.messages");
  private final ImageResource imageRes = new ImageResource(this);
  private JTextField distanceField;
  private JTextField speedField;
  private JLabel messageLabel;

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
  
  private double getDistance()
  {
    return Double.parseDouble(distanceField.getText().toString()); 
  }
  
  private double getSpeed()
  {
    return Double.parseDouble(speedField.getText().toString()); 
  }

  private JPanel AddPanel(JPanel parent, int boxLayout)
  {
		JPanel panel = new JPanel();
	
		parent.add(panel);
	    panel.setLayout(new BoxLayout(panel, boxLayout));
	    return panel;
  }
  
  private JLabel AddText(JPanel panel, String key)
  {
		JLabel label = new JLabel();
		
		if (key != "")
      label.setText(res.getString(key));
	
    panel.add(label);
    return label;
  }
  
  private JLabel AddImage(JPanel panel, String name)
  {
    JLabel label = new JLabel();
	
    label.setIcon(imageRes.getImage(name));
    panel.add(label);
    return label;
  }

  private void Initialize()
  {
		try
		{
		  Plural.SetLanguage(res.getString("Locale"));
		  
		  setTitle(res.getString("Title"));
		  setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
	
	    JPanel contentPane = (JPanel)getContentPane();
		  contentPane.setLayout(new BoxLayout(contentPane, BoxLayout.Y_AXIS));
	
		  // Upper
		  JPanel upperPanel = AddPanel(contentPane, BoxLayout.X_AXIS);
		  JPanel distancePanel = AddPanel(upperPanel, BoxLayout.Y_AXIS);
		  JPanel speedPanel = AddPanel(upperPanel, BoxLayout.Y_AXIS);
		  JPanel calculatePanel = AddPanel(upperPanel, BoxLayout.Y_AXIS);
		  
	    AddText(distancePanel, "Distance");
		  distanceField = new JTextField();
		  distancePanel.add(distanceField);
	
	    AddText(speedPanel, "Speed");
		  speedField = new JTextField();
		  speedPanel.add(speedField);
	
		  JButton calculateButton = new JButton();
		  calculateButton.setText(res.getString("Calculate"));
		  calculatePanel.add(calculateButton);
		  
		  // Lower
		  JPanel lowerPanel = AddPanel(contentPane, BoxLayout.X_AXIS);
	
	    AddImage(lowerPanel, "flag");
	    messageLabel = AddText(lowerPanel, "");
	    AddImage(lowerPanel, "car");
	
		  calculateButton.addActionListener((e) -> 
	    {
	      // Calculate the driving time
        double time = getDistance()/getSpeed();
        int hours = (int)time;
        int minutes = (int)(60*(time - hours));
      	    
        // Convert hours and minutes into plural enabled strings
        String hoursStr = Plural.format(res.getString("HoursPlural"), hours, hours);
        String minutesStr = Plural.format(res.getString("MinutesPlural"), minutes, minutes);
      	    
        // Show the time view and update its text
        messageLabel.setText(String.format(res.getString("DrivingTimeIs"), hoursStr, minutesStr));
	    });  
	
/*	  
		  calculateButton.addActionListener(new ActionListener() 
		  {
	        public void actionPerformed(ActionEvent e)
	        {
	  	      // Calculate the driving time
	          double time = getDistance()/getSpeed();
	          int hours = (int)time;
	          int minutes = (int)(60*(time - hours));
	        	    
	          // Convert hours and minutes into plural enabled strings
	          String hoursStr = Plural.format(res.getString("HoursPlural"), hours, hours);
	          String minutesStr = Plural.format(res.getString("MinutesPlural"), minutes, minutes);
	        	    
	          // Show the time view and update its text
	          messageLabel.setText(String.format(res.getString("DrivingTimeIs"), hoursStr, minutesStr));
	        }
	      });
*/
	
		  pack();
		  setSize(400, 120);
		}
		catch (Exception e)
		{
      e.printStackTrace();
		}
  }
}