import { Component, ViewChild, AfterViewInit } from '@angular/core';
import { MatLegacyDialogRef as MatDialogRef } from '@angular/material/legacy-dialog';

@Component({
  selector: 'select-dialog',
  template:`
  <p class="mat-subheading-2">{{title}}</p>
  <button mat-button (click)="dialogRef.close(0)">{{select1Caption}}</button>
  <button mat-button (click)="dialogRef.close(1)">{{select2Caption}}</button>
  <button mat-button (click)="dialogRef.close(2)">{{select3Caption}}</button>
  `
})
export class SelectDialog
{
  public title: string;
  public select1Caption: string;
  public select2Caption: string;
  public select3Caption: string;

  constructor(public dialogRef: MatDialogRef<SelectDialog>) 
  {
  }  
}
