import { Pipe, PipeTransform, Injector, LOCALE_ID } from '@angular/core';
import { PluralForm, Gender } from './language';
import { Ordinal, OrdinalStringForm } from './ordinal';

@Pipe({name: 'ordinal'})
export class OrdinalPipe implements PipeTransform 
{
  constructor(private injector: Injector) 
  {
  }

  transform(value: number, args: string[]): any 
  {
    let id = this.injector.get(LOCALE_ID);
    Ordinal.SetLanguage(id);

    var form: OrdinalStringForm = OrdinalStringForm.Short;
    var plural: PluralForm = PluralForm.One;
    var gender: Gender = Gender.Neutral;

    if (args != null)
    {
      var str: string = args.toString();
      var params: string[] = str.split(";");

      for (var param of params)
      {
        param = param.trim();

        // Ordinal string form
        if (param == "long")
          form = OrdinalStringForm.Long;
        else if (param == "short")
          form = OrdinalStringForm.Short;

        // Plural
        else if (param == "zero")
          plural = PluralForm.Zero;
        else if (param == "one")
          plural = PluralForm.One;
        else if (param == "two")
          plural = PluralForm.Two;
        else if (param == "few")
          plural = PluralForm.Few;
        else if (param == "many")
          plural = PluralForm.Many;
        else if (param == "other")
          plural = PluralForm.Other;

        // Gender
        else if (param == "male")
          gender = Gender.Male;
        else if (param == "female")
          gender = Gender.Female;
        else if (param == "neutral")
          gender = Gender.Neutral;

        else
          throw new SyntaxError(`Invalid parameter: "${param}"`);
      }
    }

    return Ordinal.Format(form, value, plural, gender);
  }
}