import { enableProdMode } from '@angular/core';
import { platformBrowserDynamic } from '@angular/platform-browser-dynamic';
import { loadTranslations } from '@angular/localize';

import { getTranslations } from 'angular';

import { environment } from './environments/environment';

import '@angular/localize/init';

if (environment.production) 
{
  enableProdMode();
}

// Get translations from a resource file in assets/i18n directory
getTranslations('assets/i18n').then(translations => 
{
  // If translations were found load them
  if (translations)
    loadTranslations(translations);

  // Load the application module and bootstrap it
  import('./app/app.module').then(module => 
  {
    platformBrowserDynamic()
      .bootstrapModule(module.AppModule)
      .catch(err => console.error(err));
  });      
});
