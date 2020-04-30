import { Component } from '@angular/core';
import { MatDialog, MatDialogConfig } from "@angular/material";
import { CourseDialogComponent } from "./course-dialog.component";

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent 
{
  constructor(private dialog: MatDialog) {}

  openDialog() 
  {
    const dialogConfig = new MatDialogConfig();

    var data = { description: "Sample" }

    dialogConfig.disableClose = true;
    dialogConfig.autoFocus = true;
    dialogConfig.data = data;

    this.dialog.open(CourseDialogComponent, dialogConfig);
  }  
}
