import { Component } from '@angular/core';

@Component({
  selector: 'app-root',
  template: `
  <h1 i18n="header|">Gender Sample</h1>
  <div *ngFor="let name of names; let i = index">
    <gender-name [name]="name" [gender]="genders[i]"></gender-name>
  </div>
  `
})
export class AppComponent 
{
  names: string[] = ["John", "Jill"];
  genders: string[] = ["male", "female"];
}
