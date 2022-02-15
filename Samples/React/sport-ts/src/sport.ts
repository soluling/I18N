export interface SportLanguage {
  id: number,
  language: string,
  name: string,
  origin: string,
  description: string
}

export interface Sport {
  id: number,
  olympic: string,
  fieldPlayers: number,
  goalie: boolean
  languages: SportLanguage[]
}