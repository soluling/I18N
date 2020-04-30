import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';

import { AppComponent } from './app.component';
import { NoId1Component } from './noid1.component';
import { NoId2Component } from './noid2.component';
import { Id1Component } from './id1.component';
import { Id2Component } from './id2.component';
import { Meaning1Component } from './meaning1.component';
import { Meaning2Component } from './meaning2.component';

@NgModule({
  declarations: [
    AppComponent,
    NoId1Component,
    NoId2Component,
    Id1Component,
    Id2Component,
    Meaning1Component,
    Meaning2Component
  ],
  imports: [
    BrowserModule
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
