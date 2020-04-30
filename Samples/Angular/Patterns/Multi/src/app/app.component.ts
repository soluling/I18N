import { Component } from '@angular/core';

@Component({
  selector: 'app-root',
  template: `
  <h1 i18n="header|">Sample</h1>
  <p i18n="skis|">I have {{ski}} skis.</p>
  <p i18n="plural|">I have {ski, plural, one {{{ski}} ski} other {{{ski}} skis}} and {car, plural, one {{{car}} car} other {{{car}} cars}}.</p>
  `
})
export class AppComponent 
{
  ski = 2;
  car = 1
}
