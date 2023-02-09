import { Injectable } from '@angular/core';
import { MatLegacyDialogRef as MatDialogRef, MatLegacyDialog as MatDialog } from '@angular/material/legacy-dialog';
import { Observable } from 'rxjs';
import { SelectDialog } from './select-dialog.component';

@Injectable()
export class DialogsService 
{
  constructor(private dialog: MatDialog) 
  { 
  }

  public select(title: string, select1Caption: string, select2Caption: string, select3Caption: string): Observable<boolean> 
  {
    let dialogRef: MatDialogRef<SelectDialog>;

    dialogRef = this.dialog.open(SelectDialog);

    dialogRef.componentInstance.title = title;
    dialogRef.componentInstance.select1Caption = select1Caption;
    dialogRef.componentInstance.select2Caption = select2Caption;
    dialogRef.componentInstance.select3Caption = select3Caption;

    return dialogRef.afterClosed();
  }
}