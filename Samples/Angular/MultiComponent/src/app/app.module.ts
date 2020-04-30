import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';

import { AppComponent } from './app.component';
import { EmbeddedComponent } from './embedded.component';
import { ExternalComponent } from './external.component';

@NgModule({
  declarations: [
    AppComponent,
    EmbeddedComponent,
    ExternalComponent
  ],
  imports: [
    BrowserModule
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
