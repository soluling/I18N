import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';

import { AppComponent } from './app.component';
import { NumberComponent }  from './number.component';
import { OrdinalPipe } from './ordinal.pipe';


@NgModule({
  declarations: [
    AppComponent,
    NumberComponent, 
    OrdinalPipe
  ],
  imports: [
    BrowserModule
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
