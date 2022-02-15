import { Component } from '@angular/core';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent 
{
  name1 = 'John';
  name2 = 'Jill';
  now = new Date();

  sourceText = $localize `Source code text`;
  sourceTextContext = $localize `:@@sourceTextContext:Source code context text`;
  sourceTextPattern = $localize `Hello ${this.name1} and ${this.name2}!`;
}
