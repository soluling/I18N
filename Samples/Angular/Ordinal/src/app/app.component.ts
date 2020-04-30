import { Component } from '@angular/core';

@Component({
  selector: 'app-root',
  template: `
  <h1 i18n="header|">Sample</h1>
  <div *ngFor="let num of numbers">
    <ordinal-number [num]="num"></ordinal-number>
  </div>
  `
})
export class AppComponent 
{
  numbers: number[] = [1, 2, 3, 4, 5, 10, 11, 21, 101];
}
