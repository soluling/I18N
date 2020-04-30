import { Component } from '@angular/core';

@Component({
  selector: 'app-root',
  template: `
  <h1 i18n>Sample</h1>
  <p i18n>Plain text.</p>
  <p i18n>This is <b>bold</b> text.</p>
  <p i18n>This is <b>bold</b> and <b>new</b> text.</p>
  <p i18n>This is <b>bold</b> and <i>new</i> text.</p>
  <p i18n="defaultLink|">This is <a href="https://www.w3schools.com/html/">link</a> sample.</p>
  <p i18n="blankLink|">This is <a href="https://www.w3schools.com/html/" target="_blank">link</a> sample.</p>
  <p i18n>This is <a href="https://www.w3schools.com/html/" target="_blank">link</a> and <b>bold</b> sample.</p>
  `
})
export class AppComponent 
{
}
