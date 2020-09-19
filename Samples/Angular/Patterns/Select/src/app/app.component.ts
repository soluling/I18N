import { Component } from '@angular/core';

// Stores information about one sport
class Sport 
{
  name: string;        // Name of the sport
  select: string;      // Code used in the select markup
  bestPlayer: string;  // Name of the best player in that sport

  constructor(
    name: string,
    select: string = '',
    bestPlayer: string = '') 
  {
    this.name = name;
    this.select = select;
    this.bestPlayer = bestPlayer;
  }
}

@Component({
  selector: 'app-root',
  template: `
  <h1 i18n="header|">Select Sample</h1>

  <label i18n="select|Sport combo box is following">Select the sport:</label>
  <select [(ngModel)]="selected" (ngModelChange)="onChange($event)" >
    <option *ngFor="let sport of sports; let i = index" [value]="i">{{sport.name}}</option>
  </select>

  <h2 i18n="selectHeader|">Sports</h2>
  <p i18n="selectDescription|">The following text is a result of select states:</p>
  <p i18n="selectText|bestPlayer: Name of the best player">{sport, select, soccer {{{bestPlayer}} is the best soccer player.} hockey {{{bestPlayer}} is the best ice hockey player.} basketball {{{bestPlayer}} is the best basketball player.} }</p>

  <h2 i18n="fallbackHeader|">Sports with a fallback</h2>
  <p i18n="fallbackDescription|">The following text is a result the same select states plus a fallback state:</p>
  <p i18n="fallbackText|bestPlayer: Name of the best player">{sport, select, soccer {{{bestPlayer}} is the best soccer player.} hockey {{{bestPlayer}} is the best ice hockey player.} basketball {{{bestPlayer}} is the best basketball player.} other {Somebody is the best player.} }</p>

  <!-- This is not used but because the current Angular string extractor does not extract string from .ts file we need to add all the string used in.ts files into a hidden elements. -->
  <p hidden>
    <ng-container i18n>Soccer</ng-container>
    <ng-container i18n>Ice Hockey</ng-container>
    <ng-container i18n>Basketball</ng-container>
    <ng-container i18n>Other</ng-container>
  </p>
    `
})
export class AppComponent 
{
  sports: Sport[] = Array();
  selected: number;

  constructor()
  {
    // Populate the sport array
    this.sports.push(new Sport($localize`Soccer`, 'soccer', 'Pelé'));
    this.sports.push(new Sport($localize`Ice Hockey`, 'hockey', 'Wayne Gretzky'));
    this.sports.push(new Sport($localize`Basketball`, 'basketball', 'Michael Jordan'));
    this.sports.push(new Sport($localize`Other`));

    // Select the first sport
    this.onChange(0);
  }

  get sport(): string
  {
    return this.sports[this.selected].select;
  }

  get bestPlayer(): string
  {
    return this.sports[this.selected].bestPlayer;
  }

  onChange(index) 
  {
    // Selection changed. This triggers Angular to rerender the view.
    this.selected = index;
  }
}
