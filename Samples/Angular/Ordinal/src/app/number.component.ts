import { Component } from '@angular/core';

@Component({
  selector: 'ordinal-number',
  inputs: ['num'],
  template: `
  <p i18n="shortSingular|num: Singular ordinal number in a short form such as '1st'">This is the {{num|ordinal}} attempt.</p>
  <p i18n="longSingular|num: Singular ordinal number in a long form such as 'first'" *ngIf="num <= 10">This is the {{num|ordinal:'long'}} attempt.</p>

  <p i18n="shortPlural|num: Plural ordinal number in a short form such as '1st'">These are the {{num|ordinal:'other'}} attempts.</p>
  <p i18n="longPlural|num: Plural ordinal number in a long form such as 'first'" *ngIf="num <= 10">These are the {{num|ordinal:'long;other'}} attempts.</p>
  `
})
export class NumberComponent
{
  num: number;
}