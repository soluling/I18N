// A class that contains the language depend properties of a sport
export class SportLanguage
{
  id: number;
  language: string;
  name: string;
  origin: string;
  description: string;

  constructor()
  {
    this.name = '';
    this.origin = '';
    this.description = '';
  }

  // Converts JavaScript object to a strongly typed SportLanguage object
  public static deserialize(jsonObj: any): SportLanguage
  {
    return Object.assign(new SportLanguage(), jsonObj);
  }
}

// We need to use string enum to get JSON serialization to work
export enum Olympic 
{ 
  No = "No",
  Winter = "Winter",
  Summer = "Summer"
}

export enum SportKind 
{ 
  Individual = "Individual",
  Team = "Team"
}

// A sport class
export class Sport
{
  id: number;
  olympic: Olympic;
  fieldPlayers: number;
  goalie: boolean;

  private _kind: SportKind;
  
  get kind(): SportKind
  {
    return this._kind;
  }

  set kind(value: SportKind)
  {
    this._kind = value;
    
    if (value == SportKind.Individual)
    {
      this.fieldPlayers = null;
      this.goalie = null;
    }
  }

  get individual(): boolean
  {
    return (this.fieldPlayers == undefined) || (this.goalie == undefined);
  }

  get olympicAsString(): string
  {
    if (this.olympic == Olympic.Summer)
      return "Summer";
    else if (this.olympic == Olympic.Winter)
      return "Winter";
    else  
      return "";
  }

  get goalieAsString(): string
  {
    if (this.goalie == undefined)
      return "";
    else if (this.goalie)
      return "Yes";
    else  
      return "No";
  }

  // There can be several language variants of the language
  languages: SportLanguage[] = [];

  // Add a language variant to the sport
  addLanguage(
    language: string, 
    name: string = "", 
    origin: string = "", 
    description: string = "")
  {
    let sportLanguage = new SportLanguage();

    sportLanguage.language = language;
    sportLanguage.name = name;
    sportLanguage.origin = origin;
    sportLanguage.description = description;

    if (this.languages == undefined)
      this.languages = new Array<SportLanguage>();

    this.languages.push(sportLanguage);
  }

  // Converts JavaScript object to a strongly typed Sport object
  public static deserialize(jsonObj: any): Sport
  {
    const sport = Object.assign(new Sport(), jsonObj);

    sport.languages = [];

    for (let language of jsonObj.languages)
      sport.languages.push(SportLanguage.deserialize(language));

    return sport;
  }
}