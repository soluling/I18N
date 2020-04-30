import { Injectable, Inject, LOCALE_ID } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { Sport } from './sport'

//export const URL = "http://localhost:53783/sports";
export const URL = "https://soluling.com/sportapi/sports";

@Injectable()
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