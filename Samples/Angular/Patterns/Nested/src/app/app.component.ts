import { Component } from '@angular/core';

// This samples uses nested plural/select markup. The sample show how flexible the markup is but you should 
// avoid nested or at least deeply nested markup because they tend to come very hard to read and also very difficult to translate
// without editor's support. Soluling's multi pattern editor does not support nested patters but you have to use chained patterns. 
// See Multi sample to see how use chained patterns.
@Component({
  selector: 'app-root',
  template: `
  <h1 i18n="header|">Sample</h1>

  <p i18n="nested|name: Name of the person, dogs: Amount of dogs">{gender, select,
    male {{dogs, plural, =0 {{{name}} will not bring his dog} one {{{name}} will bring his {{dogs}} dog} other {{{name}} will bring his {{dogs}} dogs}} }
    female {{dogs, plural, =0 {{{name}} will not bring her dog} one {{{name}} will bring her {{dogs}} dog} other {{{name}} will bring her {{dogs}} dogs}} }
    other {{dogs, plural, =0 {Nobody will bring a dog} one {Somebody will bring {{dogs}} dog} other {Somebody will bring {{dogs}} dogs}} }}</p>

  <p i18n="nested2|name: Name of the person, dogs: Amount of dogs">Dogs: {gender, select,
    male {{dogs, plural, =0 {{{name}} will not bring his dog} one {{{name}} will bring his {{dogs}} dog} other {{{name}} will bring his {{dogs}} dogs}} }
    female {{dogs, plural, =0 {{{name}} will not bring her dog} one {{{name}} will bring her {{dogs}} dog} other {{{name}} will bring her {{dogs}} dogs}} }
    other {{dogs, plural, =0 {Nobody will bring a dog} one {Somebody will bring {{dogs}} dog} other {Somebody will bring {{dogs}} dogs}} }}</p>
    
  <p i18n="nested3|">{dogs, plural,
    =0 {{gender, select, male {{{name}} will not bring his dog} female {{{name}} will not bring her dog} other {Nobody will bring a dog}} }
    one {{gender, select, male {{{name}} will not bring his {{dogs}} dog} female {{{name}} will bring her {{dogs}} dog} other {Somebody will bring {{dogs}} dog}} }
    other {{gender, select, male {{{name}} will bring his {{dogs}} dogs} female {{{name}} will bring her {{dogs}} dogs} other {Somebody will bring {{dogs}} dogs}} }}</p>
    `
})
export class AppComponent 
{
  dogs = 0;
  gender = "female";
  name = "";

  constructor()
  {
    if (this.gender == "male")
      this.name = "John";
    else
      this.name = "Jill";
  }
}
