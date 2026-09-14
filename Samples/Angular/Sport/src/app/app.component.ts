import { Component, ChangeDetectionStrategy } from '@angular/core';

@Component({
  selector: 'app-root',
  template: `
  <h1 class="mat-h1" i18n="header|">Sports</h1>
  <app-sports></app-sports>
  `,
  changeDetection: ChangeDetectionStrategy.Eager,
  standalone: false
})

export class AppComponent 
{
}
