import { Component } from '@angular/core';

@Component({
  selector: 'app-root',
  template: `
  <h1 i18n="header|">Sample</h1>
  <app-id1></app-id1>
  <app-id2></app-id2>
  <app-meaning1></app-meaning1>
  <app-meaning2></app-meaning2>
  <app-noid1></app-noid1>
  <app-noid2></app-noid2>
  `
})
export class AppComponent 
{
  title = 'app works!';
}
