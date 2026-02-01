import { Component } from '@angular/core';

@Component({
    selector: 'app-external',
    templateUrl: './external.component.html',
    standalone: false
})
export class ExternalComponent 
{
  name = "John";
  name2 = "Jill";
}
