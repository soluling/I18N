import { Component, ChangeDetectionStrategy } from '@angular/core';

@Component({
    selector: 'app-noid2',
    template: `
  <h2 i18n>Sample</h2>
  <p i18n>This is another sample</p>
  <p i18n>This is another sample</p>
  <p i18n>This is another sample</p>
  `,
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false
})
export class NoId2Component 
{
}
