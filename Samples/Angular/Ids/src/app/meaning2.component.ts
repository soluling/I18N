import { Component, ChangeDetectionStrategy } from '@angular/core';

@Component({
    selector: 'app-meaning2',
    template: `
  <h2 i18n="header|">Sample</h2>
  <p i18n="sample|Another comment">This is another sample</p>
  <p i18n="sample2|">This is another sample</p>
  <p i18n="sample3|">This is another sample</p>
  `,
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false
})
export class Meaning2Component 
{
}
