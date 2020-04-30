import { Component } from '@angular/core';

@Component({
  selector: 'app-root',
  template: `
  <h1 i18n="header|">Angular Sample</h1>
  <p i18n="hello|name: Name">Hello {{name}}!</p>
  <p i18n="hello2|name: Name\r\nname2: Another name">Hello {{name}} and {{name2}}!</p>
  <p i18n="sample|">This is an Angular localization sample.</p>

  <h2 i18n="formatting|">Formatting</h2>  
  <p i18n="number|value: Value as string">Value is {{value|number}}</p>
  <p i18n="percent|share: Share percent as string">Share is {{share|percent}}</p>
  <p i18n="date|now: Date as string">Today is {{now|date:'shortDate'}}</p>
  <p i18n="time|now: Time as string">Time is {{now|date:'shortTime'}}</p>

  <h2 i18n="plurals|">Plurals and Genders</h2>  
  <p i18n="plural|cars: Number of cars">{cars, plural, one {I have {{cars}} car.} other {I have {{cars}} cars.}}</p>
  <p i18n="plural2|cars: Number of cars">Cars: {cars, plural, one {I have {{cars}} car.} other {I have {{cars}} cars.}}</p>
  <p i18n="gender|name: Name of the person">{gender, select, male {{{name}} drives his car.} female {{{name}} drives her car.}}</p>
  `
})
export class AppComponent 
{
  name: string = "John";
  name2: string = "Jill";
  gender: string = "male";
  cars = 2;
  now = new Date();
  value = 1.5;
  share = 0.32;
}
