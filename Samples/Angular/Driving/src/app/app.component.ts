import { Component, Inject, OnInit } from '@angular/core';
import { Title } from '@angular/platform-browser';
import {MatDialog, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';

@Component({
    selector: 'app-root',
    template: `
  @if (isDialogMode) {
    <h1 mat-dialog-title i18n="header|">Driving Time</h1>
    <mat-form-field>
      <input matInput i18n [(ngModel)]="distance" placeholder="Driving distance">
    </mat-form-field>
    <mat-form-field>
      <input matInput i18n [(ngModel)]="speed" placeholder="Driving speed">
    </mat-form-field>
    <button mat-raised-button i18n (click)="calculate()">Calculate</button>
    <br/>
    @if (calculated) {
      <p mat-text i18n>Driving time is {hours, plural, =0 { } one {{{hours}} hour } other {{{hours}} hours }} {minutes, plural, one {{{minutes}} minute} other {{{minutes}} minutes}}</p>
    }
  } @else {
    <div class="main-container">
      <p i18n>Opening Driving Time Calculator dialog...</p>
    </div>
  }
  
  `,
    standalone: false
})
export class AppComponent implements OnInit
{
  distance: number = 100;
  speed: number = 55;
  result: string;
  hours: number;
  minutes: number;
  calculated: boolean;
  isDialogMode = false;
  
  constructor(
    private titleService: Title, 
    private dialog: MatDialog,
    public dialogRef: MatDialogRef<AppComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any
  )
  {
    titleService.setTitle("Driving Time Calculator");
    this.isDialogMode = !!this.dialogRef;
  }

  ngOnInit() {
    // Automatically open the dialog when the app starts
    if (!this.isDialogMode) {
      this.dialog.open(AppComponent, {
        width: '420px',
        disableClose: false,
        autoFocus: true
      });
    }
  }  

  calculate()
  {
    var time = this.distance/this.speed;
    this.hours = Math.floor(time);
    this.minutes = Math.round(60*(time - this.hours));
    this.calculated = true;
  }
}
