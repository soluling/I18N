import { BrowserModule } from '@angular/platform-browser';
import { FormsModule } from '@angular/forms';
import { NgModule, TRANSLATIONS, LOCALE_ID, TRANSLATIONS_FORMAT } from '@angular/core';

declare const require; // Use the require method provided by webpack
import { I18n } from '@ngx-translate/i18n-polyfill';

import { AppComponent } from './app.component';

@NgModule({
  declarations: [
    AppComponent
  ],
  imports: [
    BrowserModule,
    FormsModule
  ],
  providers: [
    {provide: TRANSLATIONS_FORMAT, useValue: "xlf"},
    {
      provide: TRANSLATIONS,
      useFactory: (locale) => 
      {
        locale = locale || 'en';
        return require(`raw-loader!../locale/source.${locale}.xlf`);
      },
      deps: [LOCALE_ID]
    },
    I18n
  ],
  bootstrap: [AppComponent]
})
export class AppModule { }
