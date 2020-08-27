import { BrowserModule } from '@angular/platform-browser';
import { NgModule, LOCALE_ID } from '@angular/core';
import { registerLocaleData } from '@angular/common';

import { LocaleService } from '@soluling/angular';

import { AppComponent } from './app.component';

// Register the locales we want to support so they will be compiled to the application bundle
import de from '@angular/common/locales/de';
import fi from '@angular/common/locales/fi';
import ja from '@angular/common/locales/ja';

registerLocaleData(de, 'de');
registerLocaleData(fi, 'fi');
registerLocaleData(ja, 'ja');

@NgModule({
  declarations: [
    AppComponent
  ],
  imports: [
    BrowserModule
  ],
  providers: [
    // This service tells us the active locale
    LocaleService, 

    // Use the locale service to get an active value to LOCALE_ID
    { provide: LOCALE_ID, deps: [LocaleService], useFactory: (service: LocaleService) => service.localeId },
  ],
  bootstrap: [AppComponent]
})
export class AppModule { }