import { Component } from '@angular/core';

@Component({
  selector: 'gender-name',
  inputs: ['name', 'gender'],
  template: `
  <p i18n="value|name: Name of the person">{gender, select, male {{{name}} drives his car.} female {{{name}} drives her car.} }</p>
  <p i18n="value2|name: Name of the person">{gender, select, male {{{name}} drives his car.} female {{{name}} drives her car.} other {{{name}} drives the car.} }</p>
  `
})
export class NameComponent
{
  name: string;
  gender: string;
}