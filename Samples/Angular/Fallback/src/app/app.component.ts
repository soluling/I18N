import { Component } from '@angular/core';

@Component({
  selector: 'app-root',
  template:  `
  <p i18n="one|">One</p>
  <p i18n="two|">Two</p>
  <p i18n="three|">Three</p>
  <p i18n="four|">Four</p>
  <p i18n="name|">Hello {{name}}</p>
  <p i18n="plural|0: Number of cars">{cars, plural, one {I have {{cars}} car.} other {I have {{cars}} cars.}}</p>
  `
})
export class AppComponent 
{
  name: string = "John";
  cars = 2;
}