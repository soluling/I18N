import { Component, ChangeDetectionStrategy } from '@angular/core';

@Component({
    selector: 'app-external',
    templateUrl: './external.component.html',
    changeDetection: ChangeDetectionStrategy.Eager,
    standalone: false
})
export class ExternalComponent 
{
  name = "John";
  name2 = "Jill";
}
