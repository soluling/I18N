import { Component, ChangeDetectionStrategy } from '@angular/core';

@Component({
    selector: 'plural-number',
    inputs: ['num'],
    template: `
  <p i18n="value|num: Number of files">{num, plural, one {{{num}} file} other {{{num}} files} }</p>
  `,
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false
})
export class NumberComponent
{
  num: number;
}