import { Component } from '@angular/core';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent 
{
  name = 'Bill';
  name2 = 'Jill';
  source1 = $localize`This is a sample.`;
  source2 = $localize`My name is ${this.name}`;
  source3 = $localize`${this.name} and ${this.name2} will come`;
  now = new Date();
}
