import { enableProdMode } from '@angular/core';
import { platformBrowserDynamic } from '@angular/platform-browser-dynamic';
import { loadTranslations } from '@angular/localize';
import { getTranslations } from '@soluling/angular';

import { environment } from './environments/environment';

if (environment.production) {
  enableProdMode();
}

getTranslations('assets/i18n').then(translations => 
{
  if (translations)
    loadTranslations(translations);

  import('./app/app.module').then(module => 
  {
    platformBrowserDynamic()
      .bootstrapModule(module.AppModule)
      .catch(err => console.error(err));
  });      
});