import { Component } from '@angular/core';

import { Subject, EMPTY } from 'rxjs';
import { tap, map, catchError } from 'rxjs/operators';

import { SportsService } from '../sports.service';
import { DisplaySport } from '../sport';

@Component({
  selector: 'app-sports',
  templateUrl: './sports.component.html',
  styleUrls: ['./sports.component.css']
})
export class SportsComponent
{
  private errorMessageSubject = new Subject<string>();
  errorMessage$ = this.errorMessageSubject.asObservable();

  sports$ = this.sportService.sports$
    .pipe(
      tap(sports => console.log("raw sports", sports)),
      map(sports =>
        sports.map(sport => ({
          id: sport.id,
          name: sport.languages[0].name,
          origin: sport.languages[0].origin,
          description: sport.languages[0].description,
          fieldPlayers: sport.fieldPlayers,
          goalie: sport.goalie,
          olympic: sport.olympic,
          kind: sport.kind
        }) as DisplaySport)
      ),
      tap(sports => console.log("display sports", sports)),
      catchError(err => 
        {
          console.log('Inner error', err);
          this.errorMessageSubject.next(err);
          return EMPTY;
        })
      );

  constructor(private sportService: SportsService) 
  {
  }
}
