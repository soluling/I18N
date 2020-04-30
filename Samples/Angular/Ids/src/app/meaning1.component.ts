import { Component } from '@angular/core';

@Component({
  selector: 'app-meaning1',
  template: `
  <h2 i18n="header|">Sample</h2>
  <p i18n="sample|Some comment">This is a sample</p>
  <p i18n="sample2|">This is a sample</p>
  <p i18n="sample3|">This is a sample</p>
  <p i18n="sample3|">This is a duplicate sample</p>
  `
})
export class Meaning1Component 
{
}
