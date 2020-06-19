import java.util.*;
import java.io.*;

public class XMLResourceBundle extends ResourceBundle 
{
  private Properties props;

  XMLResourceBundle(InputStream stream) throws IOException 
  {
    props = new Properties();
    props.loadFromXML(stream);
  }
   
  protected Object handleGetObject(String key) 
  {
    return props.getProperty(key);
  }
   
  public Enumeration<String> getKeys() 
  {
    Set<String> handleKeys = props.stringPropertyNames();
    return Collections.enumeration(handleKeys);
  }
}