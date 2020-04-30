import { BrowserModule } from '@angular/platform-browser';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { NgModule } from '@angular/core';
import { HttpClientModule } from '@angular/common/http';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { CdkTableModule } from '@angular/cdk/table';

import { MatButtonModule } from '@angular/material/button';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatDialogModule } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatGridListModule } from '@angular/material/grid-list';
import { MatIconModule } from '@angular/material/icon';
import { MatInputModule } from '@angular/material/input';
import { MatListModule } from '@angular/material/list';
import { MatRadioModule } from '@angular/material/radio';
import { MatSelectModule } from '@angular/material/select';
import { MatTableModule } from '@angular/material/table';

import { AppComponent } from './app.component';
import { SportsComponent } from './sports/sports.component';
import { SportsService } from './sports/sports.service'
import { SportDialogComponent } from './sports/sport-dialog.component'
import { DialogsService } from './dialogs/dialogs.service';
import { SelectDialog }   from './dialogs/select-dialog.component';

@NgModule({
  declarations: 
  [
    AppComponent,
    SportsComponent,
    SportDialogComponent,
    SelectDialog
  ],
  imports: 
  [
    BrowserModule,
    BrowserAnimationsModule,
    HttpClientModule,
    FormsModule,
    ReactiveFormsModule,
    CdkTableModule,
    MatButtonModule,
    MatCheckboxModule,
    MatDialogModule,
    MatFormFieldModule,
    MatGridListModule,
    MatIconModule,
    MatInputModule,
    MatListModule,
    MatRadioModule,
    MatSelectModule,
    MatTableModule
  ],
  exports: [
    SelectDialog,
  ],
  entryComponents: 
  [
    SportDialogComponent,
    SelectDialog
  ],
  providers: 
  [
    SportsService,
    DialogsService
  ],
  bootstrap: [AppComponent]
})
export class AppModule { }