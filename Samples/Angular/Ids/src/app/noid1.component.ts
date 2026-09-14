import { Component, ChangeDetectionStrategy } from '@angular/core';

@Component({
    selector: 'app-noid1',
    template: `
  <h2 i18n>Sample</h2>
  <p i18n>This is a sample</p>
  <p i18n>This is a sample</p>
  <p i18n>This is a sample</p>
  `,
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false
})
export class NoId1Component 
{
}
