import { Injectable, Inject, LOCALE_ID } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';

import { Observable, throwError } from 'rxjs';
import { map, tap, catchError } from 'rxjs/operators';

import { Sport } from './sport'

//export const URL = "http://localhost:53783/sports";
export const URL = "https://soluling.com/sportapi/sports";

@Injectable({
  providedIn: 'root'
})
export class SportsService
{
  constructor(private http: HttpClient, @Inject(LOCALE_ID) private locale: string) 
  {
  }

  private jsonToSport(json: any): Sport
  {
    return Sport.deserialize(json);
  }

  get getOptions(): any
  {
    return { headers: new HttpHeaders({'Accept-Language': this.locale}) };
  }

  get payloadOptions(): any
  {
    return { headers: new HttpHeaders({'Content-Type': 'application/json'}) };
  }

  sports$ = this.http.get<Sport[]>(URL)
    .pipe(
      tap(data => console.log('sports', data)),
      catchError(this.handleError)
    );

  private handleError(err: any) 
  {
    let errorMessage: string;
    console.error(err);

    if (err.error instanceof ErrorEvent) 
    {
      // A client-side or network error occurred. Handle it accordingly.
      errorMessage = `An error occurred: ${err.error.message}`;
    } else 
    {
      // The backend returned an unsuccessful response code.
      // The response body may contain clues as to what went wrong,
      errorMessage = `Backend returned code ${err.status}: ${err.statusText}`;
    }

    return throwError(errorMessage);
  }
  
  getSports(): Observable<Sport[]>
  {
    // Get HTTP payload and it to array of Sport objects
    return this.http.get<any[]>(URL, this.getOptions)
      .pipe(map((sports) =>
      {
        let result: Sport[] = [];

        for (let index in sports) 
          result.push(this.jsonToSport(sports[index]));

        return result;
      }));
  }

  getSport(id: number): Observable<Sport>
  {
    // Get HTTP payload and it to a Sport object
    return this.http.get(`${URL}/${id}`, this.getOptions).pipe(map((sport: any) => this.jsonToSport(sport)));
  }

  addSport(sport: Sport): Observable<Sport>
  {
    // Convert sport object to JSON playload and post it
    return this.http.post(URL, sport, this.payloadOptions).pipe(map((sport: any) => this.jsonToSport(sport)));
  }

  editSport(sport: Sport): Observable<Sport>
  {
    // Convert sport object to JSON playload and put it
    return this.http.put(`${URL}/${sport.id}`, sport, this.payloadOptions).pipe(map((sport: any) => this.jsonToSport(sport)));
  }

  deleteSport(id: number): Observable<Object>
  {
    // Delete a sport
    return this.http.delete(`${URL}/${id}`);
  }

  deleteSportLanguage(id: number, language: string): Observable<Object>
  {
    // Delete a sport language
    return this.http.delete(`${URL}/${id}/${language}`);
  }
}