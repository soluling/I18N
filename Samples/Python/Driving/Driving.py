import gettext
import math

gettext.bindtextdomain('Driving', 'locale')
gettext.textdomain('Driving')
_ = gettext.gettext

print(_("Driving Time"))
print(_("Driving distance:") + " ", end="")
distance = int(input())
print(_("Speed:") + " ", end="")
speed = int(input())

if speed > 0:
  time = distance/speed;
  hours = math.trunc(time);
  minutes = round(60*(time - hours));
  drivingTime = ""

  if hours > 0:
    # hours: Hours part of the driving time such a 1
    drivingTime = drivingTime + gettext.ngettext(" {hours} hour", " {hours} hours", hours).format(hours=hours)

  if minutes > 0:
    # minutes: Minutes part of the driving time such as 25
    drivingTime = drivingTime + gettext.ngettext(" {minutes} minute", " {minutes} minutes", minutes).format(minutes=minutes)

  # drivingTime: Driving time string in format of "x hours y minutes" or "x hours" or "y minutes"
  print(_("Driving time is{drivingTime}").format(drivingTime=drivingTime))
