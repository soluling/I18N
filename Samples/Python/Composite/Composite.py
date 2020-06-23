import gettext
from gettext import gettext as _

gettext.bindtextdomain('Composite', 'locale')
gettext.textdomain('Composite')

name = "John"
age = 43

# DO NOT write code like this! 
# The problem is that you will have three fragments of a sentance. They are hard to translate and impossible to change order
s = _("My name is ") + name + _(" and I am ") + str(age) + _(" years old")
print(s)

# DO NOT write code like this! 
# f-string is basically just a syntax sugar that makes compisite string more readable but f-string cannot be localized because
# they don't provide a single string that can be stored as a resource item. Do not use!
s = _(f"My name is {name} and I am {age} years old")
print(s)

# This is the preferred way to localize composite strings.
# {}: Name of the person
s = _("My name is {}").format(name)
print(s)

# DO NOT write code like this!
# It is hard for translators to chnage the order of the placheolder
# 1st {}: Name of the person
# 2nd {}: Age of the person
s = _("My name is {} and I am {} years old").format(name, age)
print(s)

# Either add indexes to the placehoders or use named paramters.  
# This allows to reorder parameters in the localized patterns.
# 0: Name of the person
# 1: Age of the person
s = _("My name is {0} and I am {1} years old").format(name, age)
print(s)

# name: Name of the person
# age: Age of the person
s = _("My name is {name} and I am {age} years old").format(name=name, age=age)
print(s)

# You can also use the old format syntax that uses %
# %s: Name of the person
s = _("My name is %s") % name
print(s)

# However if you have more than do not use the old syntax: the parameter order cannot be changed
# %s: Name of the person
# %d: Age of the person
s = _("My name is %s and I am %d years old") % (name, age)
print(s)

# Instead use the named parameters
# name: Name of the person
# age: Age of the person
s = _("My name is %(name)s and I am %(age)d years old") % {'name': name, 'age': age}
print(s)
