import { Pipe, PipeTransform } from '@angular/core';
import { getLoadedLocale } from './translationloader';

// Date, number, percent, and currency pipes that format with the browser's Intl API in the
// language of the loaded translations (getLoadedLocale / LocaleService.localeId).
//
// Angular's own DatePipe, DecimalPipe, PercentPipe, and CurrencyPipe need Angular's locale data,
// which is code in the application: registerLocaleData for each language. A language added over the
// air, after the release (see getOtaTranslations), has none, and Angular fails as soon as it formats
// with it. The browser has the formats of practically every language, so these pipes format a
// language added later correctly too, such as "lördag 28 november 2026" in Swedish.
//
// The parameters follow Angular's pipes, so a template changes only the pipe name - date ->
// localDate, number -> localNumber, percent -> localPercent, currency -> localCurrency - and each
// also takes Intl options for anything else. The last parameter, locale, overrides the language.

// Angular's predefined date formats as Intl options
const DATE_FORMATS: Record<string, Intl.DateTimeFormatOptions> =
{
  short: { dateStyle: 'short', timeStyle: 'short' },
  medium: { dateStyle: 'medium', timeStyle: 'medium' },
  long: { dateStyle: 'long', timeStyle: 'long' },
  full: { dateStyle: 'full', timeStyle: 'full' },
  shortDate: { dateStyle: 'short' },
  mediumDate: { dateStyle: 'medium' },
  longDate: { dateStyle: 'long' },
  fullDate: { dateStyle: 'full' },
  shortTime: { timeStyle: 'short' },
  mediumTime: { timeStyle: 'medium' },
  longTime: { timeStyle: 'long' },
  fullTime: { timeStyle: 'full' },
};

export type LocalDateFormat =
  'short' | 'medium' | 'long' | 'full' |
  'shortDate' | 'mediumDate' | 'longDate' | 'fullDate' |
  'shortTime' | 'mediumTime' | 'longTime' | 'fullTime';

// The formatters made so far, by locale and options: making one is slow compared with formatting
const formatters = new Map<string, object>();

function getFormatter<T extends object>(
  kind: string,
  locale: string,
  options: object,
  create: () => T): T
{
  const key = `${kind}|${locale}|${JSON.stringify(options)}`;
  let formatter = formatters.get(key) as T;

  if (!formatter)
  {
    formatter = create();
    formatters.set(key, formatter);
  }

  return formatter;
}

function isEmpty(value: unknown): boolean
{
  return (value === null) || (value === undefined) || (value === '');
}

function toNumber(value: number | string): number
{
  const result = (typeof value === 'string') ? Number(value) : value;

  if (isNaN(result))
    throw new Error(`${value} is not a number`);

  return result;
}

// Angular's digitsInfo, {minIntegerDigits}.{minFractionDigits}-{maxFractionDigits} such as 1.0-2,
// as Intl options. Every part is optional.
export function parseDigitsInfo(digitsInfo: string): Intl.NumberFormatOptions
{
  const match = /^(\d+)?\.(\d+)?(?:-(\d+))?$/.exec(digitsInfo.trim());

  if (!match)
    throw new Error(`${digitsInfo} is not a valid digit info, such as 1.0-2`);

  const options: Intl.NumberFormatOptions = {};

  if (match[1] !== undefined)
    options.minimumIntegerDigits = Number(match[1]);

  if (match[2] !== undefined)
    options.minimumFractionDigits = Number(match[2]);

  if (match[3] !== undefined)
    options.maximumFractionDigits = Number(match[3]);

  if ((options.minimumFractionDigits !== undefined) && (options.maximumFractionDigits !== undefined) &&
    (options.minimumFractionDigits > options.maximumFractionDigits))
  {
    throw new Error(`${digitsInfo}: the minimum fraction digits are more than the maximum`);
  }

  return options;
}

function toNumberOptions(format: string | Intl.NumberFormatOptions | undefined): Intl.NumberFormatOptions
{
  if (format === undefined)
    return {};

  return (typeof format === 'string') ? parseDigitsInfo(format) : format;
}

// {{ today | localDate }}, {{ today | localDate:'short' }},
// {{ today | localDate:{ weekday: 'long', day: 'numeric', month: 'long' } }}
@Pipe({ name: 'localDate', standalone: true })
export class LocalDatePipe implements PipeTransform
{
  transform(
    value: Date | number | string | null | undefined,
    format: LocalDateFormat | Intl.DateTimeFormatOptions = 'mediumDate',
    locale?: string): string | null
  {
    if (isEmpty(value))
      return null;

    const date = (value instanceof Date) ? value : new Date(value as number | string);

    if (isNaN(date.getTime()))
      throw new Error(`${value} is not a valid date`);

    let options: Intl.DateTimeFormatOptions;

    if (typeof format === 'string')
    {
      options = DATE_FORMATS[format];

      if (!options)
        throw new Error(`${format} is not a date format; use one of ${Object.keys(DATE_FORMATS).join(', ')}, or Intl options`);
    }
    else
      options = format;

    const id = locale || getLoadedLocale();
    return getFormatter('date', id, options, () => new Intl.DateTimeFormat(id, options)).format(date);
  }
}

// {{ total | localNumber }}, {{ total | localNumber:'1.0-2' }},
// {{ total | localNumber:{ notation: 'compact' } }}
@Pipe({ name: 'localNumber', standalone: true })
export class LocalNumberPipe implements PipeTransform
{
  transform(
    value: number | string | null | undefined,
    format: string | Intl.NumberFormatOptions = '1.0-3',
    locale?: string): string | null
  {
    if (isEmpty(value))
      return null;

    const options = toNumberOptions(format);
    const id = locale || getLoadedLocale();
    return getFormatter('number', id, options, () => new Intl.NumberFormat(id, options)).format(toNumber(value as number | string));
  }
}

// {{ 0.256 | localPercent }} -> 26 %, {{ 0.256 | localPercent:'1.1-1' }} -> 25,6 %
@Pipe({ name: 'localPercent', standalone: true })
export class LocalPercentPipe implements PipeTransform
{
  transform(
    value: number | string | null | undefined,
    format: string | Intl.NumberFormatOptions = '1.0-0',
    locale?: string): string | null
  {
    if (isEmpty(value))
      return null;

    const options: Intl.NumberFormatOptions = { ...toNumberOptions(format), style: 'percent' };
    const id = locale || getLoadedLocale();
    return getFormatter('percent', id, options, () => new Intl.NumberFormat(id, options)).format(toNumber(value as number | string));
  }
}

// {{ price | localCurrency:'EUR' }} -> 12,50 €, {{ price | localCurrency:'EUR':'code' }},
// {{ price | localCurrency:'JPY':'symbol':'1.0-0' }}. The digits default to the currency's own.
@Pipe({ name: 'localCurrency', standalone: true })
export class LocalCurrencyPipe implements PipeTransform
{
  transform(
    value: number | string | null | undefined,
    currencyCode: string = 'USD',
    display: 'symbol' | 'narrowSymbol' | 'code' | 'name' = 'symbol',
    format?: string | Intl.NumberFormatOptions,
    locale?: string): string | null
  {
    if (isEmpty(value))
      return null;

    const options: Intl.NumberFormatOptions = { ...toNumberOptions(format), style: 'currency', currency: currencyCode, currencyDisplay: display };
    const id = locale || getLoadedLocale();
    return getFormatter('currency', id, options, () => new Intl.NumberFormat(id, options)).format(toNumber(value as number | string));
  }
}

// Relative time: the time from now in the largest unit that fits, for a date
const RELATIVE_UNITS: [Intl.RelativeTimeFormatUnit, number][] =
[
  ['year', 365.25 * 24 * 60 * 60],
  ['month', 30.4375 * 24 * 60 * 60],
  ['week', 7 * 24 * 60 * 60],
  ['day', 24 * 60 * 60],
  ['hour', 60 * 60],
  ['minute', 60],
  ['second', 1],
];

// {{ -3 | localRelativeTime:'day' }} -> 3 days ago, {{ 1 | localRelativeTime:'week' }} -> next week,
// {{ comment.created | localRelativeTime }} -> 5 minutes ago, yesterday, in 2 weeks: for a date, the
// unit that fits the time from now. The default numeric: 'auto' gives words such as yesterday and
// tomorrow; pass { numeric: 'always' } for "1 day ago". Like Angular's pure pipes, the text is made
// when the value changes, not as time goes on.
@Pipe({ name: 'localRelativeTime', standalone: true })
export class LocalRelativeTimePipe implements PipeTransform
{
  transform(
    value: number | Date | string | null | undefined,
    unit?: Intl.RelativeTimeFormatUnit,
    options: Intl.RelativeTimeFormatOptions = { numeric: 'auto' },
    locale?: string): string | null
  {
    if (isEmpty(value))
      return null;

    let amount: number;

    if ((typeof value === 'number') && unit)
      amount = value;
    else
    {
      const date = (value instanceof Date) ? value : new Date(value as number | string);

      if (isNaN(date.getTime()))
        throw new Error(`${value} is not a valid date; give a number with a unit, such as -3 | localRelativeTime:'day'`);

      const seconds = (date.getTime() - Date.now()) / 1000;
      const units = unit ? RELATIVE_UNITS.filter(([name]) => name === unit) : RELATIVE_UNITS;
      const [name, length] = units.find(([, length]) => Math.abs(seconds) >= length) ?? RELATIVE_UNITS[RELATIVE_UNITS.length - 1];

      unit = name;
      amount = Math.round(seconds / length);
    }

    const id = locale || getLoadedLocale();
    return getFormatter('relative', id, options, () => new Intl.RelativeTimeFormat(id, options)).format(amount, unit);
  }
}

// Intl.ListFormat and the current Intl.DisplayNames are in every current browser, but not in the
// TypeScript library this package is compiled with - so their options are declared here, and their
// constructors reached through IntlApi
export interface LocalListOptions
{
  type?: 'conjunction' | 'disjunction' | 'unit';
  style?: 'long' | 'short' | 'narrow';
}

export type LocalDisplayNameType = 'language' | 'region' | 'script' | 'currency' | 'calendar' | 'dateTimeField';

export interface LocalDisplayNameOptions
{
  style?: 'long' | 'short' | 'narrow';
  languageDisplay?: 'dialect' | 'standard';
  fallback?: 'code' | 'none';
}

interface IntlApi
{
  ListFormat: new (locale: string, options: LocalListOptions) => { format(list: Iterable<string>): string };
  DisplayNames: new (locales: string[], options: LocalDisplayNameOptions & { type: LocalDisplayNameType }) => { of(code: string): string | undefined };
}

const intlApi = Intl as unknown as IntlApi;

// {{ ['Anna', 'Ben', 'Carl'] | localList }} -> Anna, Ben, and Carl (Anna, Ben ja Carl in Finnish),
// {{ sizes | localList:{ type: 'disjunction' } }} -> S, M, or L,
// {{ ['5 kg', '3 g'] | localList:{ type: 'unit', style: 'narrow' } }}
@Pipe({ name: 'localList', standalone: true })
export class LocalListPipe implements PipeTransform
{
  transform(
    value: Iterable<string> | null | undefined,
    options: LocalListOptions = { type: 'conjunction', style: 'long' },
    locale?: string): string | null
  {
    if (isEmpty(value))
      return null;

    const id = locale || getLoadedLocale();
    return getFormatter('list', id, options, () => new intlApi.ListFormat(id, options)).format(value as Iterable<string>);
  }
}

// {{ 'fi' | localDisplayName }} -> Finnish (in the loaded language), {{ 'SE' | localDisplayName:'region' }},
// {{ 'EUR' | localDisplayName:'currency' }}, {{ 'Hant' | localDisplayName:'script' }}.
// The locale 'self' gives a language's name in that language itself, for a language list:
// {{ 'sv' | localDisplayName:'language':'self' }} -> svenska. An unknown code shows as it is.
@Pipe({ name: 'localDisplayName', standalone: true })
export class LocalDisplayNamePipe implements PipeTransform
{
  transform(
    value: string | null | undefined,
    type: LocalDisplayNameType = 'language',
    locale?: string,
    options: LocalDisplayNameOptions = {}): string | null
  {
    if (isEmpty(value))
      return null;

    const code = value as string;
    const id = (locale === 'self') ? code : (locale || getLoadedLocale());
    const allOptions = { fallback: 'code' as const, ...options, type };

    try
    {
      return getFormatter('names', id, allOptions, () => new intlApi.DisplayNames([id], allOptions)).of(code) ?? code;
    }
    catch
    {
      // Not a valid code, or not a valid locale for 'self': show the code
      return code;
    }
  }
}
