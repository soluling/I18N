import { Component } from '@angular/core';

@Component({
  selector: 'app-root',
  template: `
  <h1 i18n="header|">Plural Sample</h1>
  <div *ngFor="let num of numbers">
    <plural-number [num]="num"></plural-number>
  </div>
  `
})
export class AppComponent  
{
  numbers: number[] = [0, 1, 2, 3, 4, 5, 11, 21, 101, 111];
}
