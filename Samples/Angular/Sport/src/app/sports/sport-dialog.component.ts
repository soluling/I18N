import { Component, Inject, OnInit } from '@angular/core';
import { UntypedFormBuilder, UntypedFormGroup, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { Sport, SportKind, Olympic } from './sport'
 
@Component({
  selector: 'app-sport-dialog',
  templateUrl: './sport-dialog.component.html',
  styleUrls: ['./sport-dialog.component.css']
})
export class SportDialogComponent implements OnInit
{
  form: UntypedFormGroup;
  isNew: boolean;

  constructor(
    public dialogRef: MatDialogRef<SportDialogComponent>,
    @Inject(MAT_DIALOG_DATA) public sport: Sport,
    private formBuilder: UntypedFormBuilder) 
  {
  }

  ngOnInit()
  {
    this.form = this.formBuilder.group(
    {
      name: [this.sport.languages[0].name, Validators.required],
      olympic: [this.sport.olympic, Validators.required],
      kind: [this.sport.kind, Validators.required],
      fieldPlayers: [this.sport.fieldPlayers/*, Validators.required*/],
      goalie: [this.sport.goalie/*, Validators.required*/],
      origin: [this.sport.languages[0].origin, Validators.required],
      description: [this.sport.languages[0].description, Validators.required]
    });
  }

  onSubmit(): void 
  {
    if (this.form.invalid)
      return;
      
    this.sport.languages[0].name = this.form.value.name;
    this.sport.languages[0].origin = this.form.value.origin;
    this.sport.languages[0].description = this.form.value.description;

    this.sport.olympic = this.form.value.olympic;
    this.sport.kind = this.form.value.kind;

    if (this.sport.kind == SportKind.Team)
    {
      this.sport.fieldPlayers = this.form.value.fieldPlayers;
      this.sport.goalie = this.form.value.goalie;
    }
  
    this.dialogRef.close(this.sport);
  }

  onTeamCheck(): void
  {
  }

  onCancelClick(): void 
  {
    this.dialogRef.close();
  }

  isFieldValid(field: string) 
  {
    return !this.form.get(field).valid && this.form.get(field).touched;
  }
}