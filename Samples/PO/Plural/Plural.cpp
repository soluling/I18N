int main(void)
{  
  printf(gettext("Sample"));  
  printf(gettext("Hello %s"), "John");  
  printf(ngettext("%d file", "%d files", 2));  
  return 0;
}