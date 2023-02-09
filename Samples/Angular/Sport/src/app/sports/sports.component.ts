import { Component, Inject, LOCALE_ID } from '@angular/core';
import { DataSource } from '@angular/cdk/collections';

import { Observable ,  BehaviorSubject } from 'rxjs';

import { MatLegacyDialog as MatDialog } from '@angular/material/legacy-dialog';

import { Sport, Olympic } from './sport'
import { SportsService, URL } from './sports.service'
import { SportDialogComponent } from './sport-dialog.component'
import { DialogsService } from '../dialogs/dialogs.service';

const dialogWidth = '800px';

@Component({
  selector: 'app-sports',
  templateUrl: './sports.component.html',
  styleUrls: ['./sports.component.css']
})
export class SportsComponent
{
  private url: string;
  private sports: Sport[];
  displayedColumns = ['id', 'name', 'olympic', 'fieldPlayers', 'goalie', 'origin', 'description', 'edit'];
  dataSource = new SportDataSource(this.sportsService);

  constructor(
    private sportsService: SportsService, 
    private dialog: MatDialog, 
    private dialogsService: DialogsService, 
    @Inject(LOCALE_ID) private locale: string)
  {
    this.url = URL;
  }

  private doAdd(sport: Sport)
  {
    this.sportsService.addSport(sport).subscribe(sport => this.dataSource.refresh());
  }

  addSport()
  {
    let sport = new Sport();
    sport.addLanguage(this.locale);

    let dialogRef = this.dialog.open(SportDialogComponent, 
      {
        width: dialogWidth,
        data: sport
      });

    dialogRef.componentInstance.isNew = true;

    dialogRef.afterClosed().subscribe((sport: Sport) => 
    {
      if (sport != null)
      {
        this.sportsService.addSport(sport).subscribe(sport => this.dataSource.refresh());
      }
    });
  }

  addWaterpolo() 
  {
    let sport = new Sport();

    sport.olympic = Olympic.Summer;
    sport.fieldPlayers = 6;
    sport.goalie = true;
    sport.addLanguage("en", "Water polo", "England", "Water polo is a competitive team sport played in the water between two teams");
    sport.addLanguage("fi", "Vesipallo", "Englanti", "Vesipallo on vesialtaassa pelattava pallopeli");

    this.doAdd(sport);
  }

  addVolleyball() 
  {
    let sport = new Sport();

    sport.olympic = Olympic.Summer;
    sport.fieldPlayers = 6;
    sport.goalie = false;
    sport.addLanguage("en", "Volleyball", "United States", "Volleyball is a team sport in which two teams of six players are separated by a net");
    sport.addLanguage("fi", "Lentopallo", "Yhdysvallat", "Lentopallo on yleensä sisätiloissa pelattava joukkuepeli, jossa kaksi verkon erottamaa kuusihenkistä joukkuetta lyövät ilmatäytteistä palloa puolelta toiselle");

    this.doAdd(sport);
  }

  edit(value: number) 
  {
    console.log("Editing #" + value);

    this.sportsService.getSport(value).subscribe(sport => 
    {
      console.log("Kind: " + sport.kind);
      console.log('Editing ' + JSON.stringify(sport));

      let dialogRef = this.dialog.open(
        SportDialogComponent, 
        {
          width: dialogWidth,
          data: sport
        });

      dialogRef.componentInstance.isNew = false;

      dialogRef.afterClosed().subscribe((sport: Sport) => 
      {
        if (sport != null)
        {
          this.sportsService.editSport(sport).subscribe(sport => 
          {
            console.log(sport);
            this.dataSource.refresh();
          });
        }
      });
    });
  }

  delete(value: number) 
  {
    console.log("Deleting #" + value);

    this.dialogsService
      .select("Select What to Delete", "Delete whole sport", "Delete current language only", "Cancel")
      .subscribe(res => 
      {
        let result = JSON.stringify(res);
        console.log(result);

        if (result == "0")
        {
          console.log(`Delete language`);

          this.sportsService.deleteSport(value).subscribe(result => 
            {
              console.log(result);
              this.dataSource.refresh();
            });
        }
        else if (result == "1")
        {
          console.log(`Delete language part: ${this.locale}`);

          this.sportsService.deleteSportLanguage(value, this.locale).subscribe(result => 
            {
              console.log(result);
              this.dataSource.refresh();
            });
        }
      });
  }
}

export class SportDataSource extends DataSource<Sport> 
{
  private sports = new BehaviorSubject<Sport[]>([]);
  
  constructor(private sportsSerivce: SportsService)
  {
    super();
  }

  private getSports(): void
  {
    this.sportsSerivce.getSports().subscribe(sports => this.sports.next(sports))
  }

  connect(): Observable<Sport[]> 
  {
    this.getSports();
    return this.sports;
  }

  disconnect() 
  {
  }

  refresh()
  {
    this.getSports();
  }
}
