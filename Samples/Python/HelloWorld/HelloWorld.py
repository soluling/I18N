import gettext
from gettext import gettext as _

gettext.bindtextdomain('HelloWorld', 'locale')
gettext.textdomain('HelloWorld')

# This is a comment
print(_("Hello World"))

print(_("This is a sample"))

# name: Name of the user
print(_("Hello {name}!").format(name='John'))

name = "John"
company = "Soluling"

# name: Name of the user, company: Name of the company
print(_("Hello {name} and {company}!").format(name=name, company=company))