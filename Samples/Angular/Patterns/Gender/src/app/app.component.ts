import { Component } from '@angular/core';

@Component({
    selector: 'app-root',
    template: `
  <h1 i18n="header|">Gender Sample</h1>
  @for (name of names; track name; let i = $index) {
    <div>
      <gender-name [name]="name" [gender]="genders[i]"></gender-name>
    </div>
  }
  `,
    standalone: false
})
export class AppComponent 
{
  names: string[] = ["John", "Jill"];
  genders: string[] = ["male", "female"];
}
