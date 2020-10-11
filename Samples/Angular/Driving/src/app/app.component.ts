import { Component, Inject, OnInit } from '@angular/core';
import { Title } from '@angular/platform-browser';
import { FormsModule, ReactiveFormsModule, FormBuilder, FormControl, FormGroup, Validators, AbstractControl } from '@angular/forms';
import { MatDialog, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';

class Driving
{
  distance: number;
  speed: number;
}

@Component({
  selector: 'app-root',
  template: `
  <h1 mat-dialog-title i18n="header|">Driving Time</h1>

  <mat-form-field>
    <input matInput [(ngModel)]="distance" placeholder="Driving distance">
  </mat-form-field>

  <mat-form-field>
    <input matInput [(ngModel)]="speed" placeholder="Driving speed">
  </mat-form-field>
  
  <button mat-raised-button (click)="calculate()">Calculate</button>

  <br/>
  <p mat-text *ngIf="calculated">Driving time is {hours, plural, =0 { } one {{{hours}} hour } other {{{hours}} hours }} {minutes, plural, one {{{minutes}} minute} other {{{minutes}} minutes}}</p>
  `
})
export class AppComponent
{
  distance: number = 100;
  speed: number = 55;
  result: string;
  hours: number;
  minutes: number;
  calculated: boolean;
  
  constructor(
    private titleService: Title, 
    public dialog: MatDialog) 
  {
    titleService.setTitle("Driving Time Calculator");
  }

  calculate()
  {
    var time = this.distance/this.speed;
    this.hours = Math.floor(time);
    this.minutes = Math.round(60*(time - this.hours));
    this.calculated = true;
  }
}
