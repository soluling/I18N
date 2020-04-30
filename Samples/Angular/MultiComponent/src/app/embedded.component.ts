import { Component } from '@angular/core';

@Component({
  selector: 'app-embedded',
  template: `
  <p i18n="hello|name: Name of the user">Hello {{name}}!</p>
  <p i18n="sample|">This is a sample</p>
  `
})
export class EmbeddedComponent
{
  name = "John";
}
