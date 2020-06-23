"""
Minimal URL Configuration

The `urlpatterns` list routes URLs to views. For more information please see:
    https://docs.djangoproject.com/en/2.1/topics/http/urls/

Examples:
Function views
    1. Add an import:  from my_app import views
    2. Add a URL to urlpatterns:  path('', views.home, name='home')
Class-based views
    1. Add an import:  from other_app.views import Home
    2. Add a URL to urlpatterns:  path('', Home.as_view(), name='home')
Including another URLconf
    1. Import the include() function: from django.urls import include, path
    2. Add a URL to urlpatterns:  path('blog/', include('blog.urls'))
"""

import minimal
from django.urls import include, path
# Add this to enabled localized patterns
from django.conf import settings
from django.conf.urls.i18n import i18n_patterns
from django.conf.urls.static import static

urlpatterns = [
  path('', minimal.index, name='home')
]
'''
urlpatterns += i18n_patterns( # < here
  path('', include('Minimal.urls'))
) + static(settings.MEDIA_URL, document_root=settings.MEDIA_ROOT)
'''