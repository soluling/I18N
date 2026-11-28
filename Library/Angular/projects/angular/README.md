# @soluling/angular

Runtime translation loading, over-the-air (OTA) translation updates, and locale-aware pipes for Angular.

Angular's standard localization builds one application per language. This library lets one build load the translations of the user's language at runtime, from the application's own files or over the air from [Soluling Cloud](https://www.soluling.com/Documentation/OTA), so a translation fix or a new language reaches your users without a new build. It works on top of Angular's own localization (`$localize` and `i18n` attributes), not instead of it.

- [Get started](#get-started)
- [Resource files](#resource-files)
- [Loading translations](#loading-translations): `getTranslations`, `getTranslationsEx`, `TranslateOptions`
- [Over-the-air updates](#over-the-air-updates): `getOtaTranslations`, `getOtaLanguages`, `OtaOptions`
- [The active language](#the-active-language): `LocaleService`, `getLoadedLocale`, `LOCALE_ID`, cookies
- [Pipes](#pipes): dates, numbers, currencies, relative times, lists, and names
- [Other functions and types](#other-functions-and-types)
- [Versions](#versions)

Learn more from the [Angular localization guide](https://www.soluling.com/Guides/Angular). The [Runtime](https://github.com/soluling/I18N/tree/master/Samples/Angular/Runtime) and [Ota](https://github.com/soluling/I18N/tree/master/Samples/Angular/Ota) samples use this library.

## Get started

Install the library:

```bash
npm install @soluling/angular
```

Load the translations before the application starts - `$localize` translates each text once, when the application starts - and import the application only after that. In `main.ts`:

```typescript
import { loadTranslations } from '@angular/localize';
import { getTranslationsEx } from '@soluling/angular';

getTranslationsEx('assets/i18n', { format: 'arb' }).then(translations =>
{
  if (translations)
    loadTranslations(translations);

  // Import the application only now, so that its texts use the loaded translations
  import('./app/bootstrap').then(module => module.bootstrap());
});
```

`@angular/localize/init` must be loaded first, for example as a polyfill in `angular.json`. If no translations are found, the application uses its original texts.

Then make `LOCALE_ID` the language of the loaded translations, so that Angular's own pipes format in it - see [The active language](#the-active-language):

```typescript
import { LOCALE_ID } from '@angular/core';
import { LocaleService } from '@soluling/angular';

bootstrapApplication(AppComponent,
{
  providers:
  [
    LocaleService,
    { provide: LOCALE_ID, deps: [LocaleService], useFactory: (service: LocaleService) => service.localeId },
  ]
});
```

## Resource files

The library loads Angular's own runtime formats. Extract the original texts with Angular's extract tool, localize them, for example with [Soluling](https://www.soluling.com/), and save each language as `<id>.<format>`, where `id` is the language code, such as `fi.arb` or `de-AT.json`.

| Format | Extract | Content |
|---|---|---|
| `json` (default) | `ng extract-i18n --format=json` | `{ "locale": "fi", "translations": { "id": "text" } }` |
| `arb` | `ng extract-i18n --format=arb` | `{ "@@locale": "fi", "id": "text", "@id": { "description": "..." } }` |

ARB keeps the descriptions (`i18n="description@@id"`), so translators see them; JSON has only the ids and texts. Make sure your web server serves `.arb` files, for example as `application/json`.

Copy the localized files to a folder that the application serves, such as `assets/i18n` or a `locale` folder mapped in `angular.json`:

```json
"assets": [
  { "glob": "*.arb", "input": "src/locale", "output": "locale", "ignore": ["messages.arb"] }
]
```

## Loading translations

### getTranslationsEx

```typescript
getTranslationsEx(baseUrl: string, options?: TranslateOptions): Promise<Translations>
```

Loads `<baseUrl>/<id>.<format>` of the user's language and resolves to the translations, or `null` if none is found. The languages are tried in this order:

1. The language in the cookie (`cookieName`), or the `locale` option, or the browser's languages (`navigator.languages`)
2. Each followed by its parent: `de-AT`, then `de`
3. The fallback language (`fallbackId`)

The first file found is used. Its language becomes the [active language](#the-active-language).

### TranslateOptions

| Option | Default | |
|---|---|---|
| `format` | `'json'` | `'json'` or `'arb'` |
| `cookieName` | none | The cookie that keeps the language the user selected. See [setCookie](#other-functions-and-types). |
| `locale` | the browser's languages | The languages to try, separated by `;`, such as `'fi;de'` |
| `fallbackId` | `'en'` | The language tried last |
| `tryAllLanguages` | `true` | Try all the browser's languages, not only the first |
| `ingoreCountry` | `false` | Try only the language parts of the browser's languages, such as `fi` for `fi-FI` |
| `setLocaleId` | `true` | Make the loaded language the active language |

### getTranslations

```typescript
getTranslations(baseUrl: string, ingoreCountry?, tryAllLanguages?, setLocaleId?, fallbackId?, locale?, cookieName?, format?): Promise<Translations>
```

The same as `getTranslationsEx`, with the options as parameters, for existing code.

## Over-the-air updates

Requires version 14.2 or later.

### getOtaTranslations

```typescript
getOtaTranslations(projectGuid: string, options?: OtaOptions): Promise<Translations>
```

Loads the translations of the user's language from the files published in Soluling Cloud. Turn on *Publish artifacts* on the *Integrations* tab of the project's *Manage* page, and build the project; the GUID is on its *Overview* tab. The project must be made from the runtime file, such as `messages.arb`, so that the published files are files the application can load.

```typescript
getOtaTranslations('1b6e2f0a-4c2d-4e9a-9f1a-example-guid', { format: 'arb', fallbackUrl: 'locale', cookieName: 'language' })
  .then(translations => ...);
```

- **First visit:** reads the manifest of the published files (the CDN's first, the API's if the CDN has none), and downloads the file of the user's language. If that fails or takes longer than `timeout`, the application's own files in `fallbackUrl` are used, as `getTranslationsEx` does.
- **After that:** the translations saved in the browser (`localStorage`) are used at once, and newer ones are checked in the background once a `checkInterval`. A newer file is used on the next load. A file is downloaded only when its hash has changed.
- **Missing texts:** a published file replaces the application's own file of that language. A text that the published file doesn't have shows in the original language. Message ids come from the original texts, so every version of the application can use the same published files.

The languages are tried in the same order as with `getTranslationsEx`.

### OtaOptions

All the [TranslateOptions](#translateoptions), and:

| Option | Default | |
|---|---|---|
| `fallbackUrl` | `'assets/i18n'` | The application's own files, used until the first download, or if it fails. `null` for none. |
| `timeout` | `3000` | Milliseconds to wait for the first download |
| `checkInterval` | a day | Milliseconds between the background checks for newer translations. `0` checks on every load. |
| `cache` | `true` | Save the translations and the languages in the browser |
| `fileFilter` | the files of `format` | Which published files are the application's, when the project has other files too: `(fileName: string) => boolean` |
| `cdnUrl` | `SOLULING_CDN_URL` | The published files on Soluling's CDN, where the manifest is read first |
| `apiUrl` | `SOLULING_API_URL` | Soluling's API, for the manifest when the CDN doesn't have it yet |

### getOtaLanguages

```typescript
getOtaLanguages(projectGuid: string, options?: OtaOptions): Promise<string[]>
```

Requires version 14.4 or later. Resolves to the languages published in Soluling Cloud, such as `['de', 'fi', 'ja']`: the languages of the application's files (`format`, `fileFilter`), without the original language. Add them to the languages of the application itself, so that a language published later is in the application's language list too:

```typescript
const APP_LANGUAGES = ['en', 'fi', 'de', 'ja'];

getOtaLanguages(guid, options).then(published =>
  languages.set([...APP_LANGUAGES, ...published.filter(id => !APP_LANGUAGES.includes(id))]));
```

```html
@for (id of languages(); track id) {
  <option [value]="id">{{ id | localDisplayName:'language':'self' }}</option>
}
```

It reads the same manifest as `getOtaTranslations`, only once per page load, and saves the list in the browser: after the first visit the list is there at once, and it is read again in the background once a `checkInterval`. Pass the same options as to `getOtaTranslations`. If nothing is published, or the read fails, the list is empty; the promise never rejects.

## The active language

The active language is the language of the loaded translations, such as `fi` or `zh-Hant`, or `en-US` when none were loaded. It is what the [pipes](#pipes) of this library format in.

- `LocaleService.localeId` - an injectable service; provide `LocaleService` in your providers
- `getLoadedLocale()` - the same without dependency injection (version 14.5 or later)

### LOCALE_ID

Angular's own pipes (`date`, `number`, ...) and components such as Angular Material's use `LOCALE_ID` and Angular's locale data, which is code in the application: register it with `registerLocaleData` for each language. If `LOCALE_ID` is a language without locale data, the application fails as soon as Angular formats with it.

A language added over the air, after the release, has no locale data. So use the loaded language as `LOCALE_ID` only when its locale data is registered, and otherwise the original language. Angular uses the data of the language part, such as `fi` for `fi-FI`:

```typescript
import de from '@angular/common/locales/de';
import fi from '@angular/common/locales/fi';
import ja from '@angular/common/locales/ja';

const LOCALE_DATA = { de, fi, ja };

for (const [id, data] of Object.entries(LOCALE_DATA))
  registerLocaleData(data, id);

function getLocaleId(service: LocaleService): string
{
  const id = service.localeId;
  return ((id in LOCALE_DATA) || (id.split('-')[0] in LOCALE_DATA)) ? id : 'en-US';
}

// providers
LocaleService,
{ provide: LOCALE_ID, deps: [LocaleService], useFactory: getLocaleId },
```

Use the [pipes](#pipes) of this library for your own formatting: they need no locale data, so a language added later gets its own formats too. For the language the user sees, such as the selected item of a language list, use `LocaleService.localeId`, not `LOCALE_ID`.

### Changing the language

`$localize` translates each text when the application starts, so a new language needs a new start. Keep the selected language in a cookie, pass the same `cookieName` to `getTranslationsEx` or `getOtaTranslations`, and load the page again:

```typescript
setCookie('language', 'fi');
location.reload();
```

## Pipes

Requires version 14.5 or later.

The pipes format with the browser's `Intl` API in the [active language](#the-active-language). Unlike Angular's pipes, they need no locale data: the browser has the formats of practically every language, so a language added over the air formats correctly too, such as *lördag 28 november 2026* in Swedish. They are standalone pipes; import them in a standalone component's or a module's `imports`:

```typescript
import { LocalDatePipe, LocalNumberPipe } from '@soluling/angular';

@Component({
  imports: [LocalDatePipe, LocalNumberPipe],
  ...
})
```

The first four replace Angular's pipes with the same parameters, so a template changes only the pipe name. Every pipe takes `Intl` options for anything else, and a locale as its last parameter to override the active language. An empty value (`null`, `undefined`, `''`) gives `null`; a value that isn't a valid date or number throws an error.

### localDate

```
value | localDate[:format[:locale]]
```

| Parameter | Default | |
|---|---|---|
| `value` | | A `Date`, a timestamp, or an ISO date string |
| `format` | `'mediumDate'` | One of Angular's format names, or `Intl.DateTimeFormatOptions` |
| `locale` | the active language | |

The format names are `short`, `medium`, `long`, `full` (date and time), `shortDate`, `mediumDate`, `longDate`, `fullDate`, and `shortTime`, `mediumTime`, `longTime`, `fullTime`.

```html
{{ today | localDate }}                                   <!-- 28.11.2026 in Finnish -->
{{ today | localDate:'fullDate' }}                        <!-- lauantai 28. marraskuuta 2026 -->
{{ today | localDate:'short' }}                           <!-- 28.11.2026 klo 14.05 -->
{{ today | localDate:{ weekday: 'long', day: 'numeric', month: 'long' } }}
```

### localNumber, localPercent

```
value | localNumber[:format[:locale]]
value | localPercent[:format[:locale]]
```

| Parameter | Default | |
|---|---|---|
| `value` | | A number, or a string with a number |
| `format` | `'1.0-3'` (number), `'1.0-0'` (percent) | Angular's digits info, or `Intl.NumberFormatOptions` |
| `locale` | the active language | |

The digits info is `{minIntegerDigits}.{minFractionDigits}-{maxFractionDigits}`, each part optional, such as `'1.2-2'`.

```html
{{ 1234567.891 | localNumber }}                    <!-- 1 234 567,891 in Finnish -->
{{ 12.5 | localNumber:'1.2-2' }}                   <!-- 12,50 -->
{{ 1500000 | localNumber:{ notation: 'compact' } }} <!-- 1,5 milj. -->
{{ 0.256 | localPercent }}                         <!-- 26 % -->
{{ 0.256 | localPercent:'1.1-1' }}                 <!-- 25,6 % -->
```

### localCurrency

```
value | localCurrency[:currencyCode[:display[:format[:locale]]]]
```

| Parameter | Default | |
|---|---|---|
| `value` | | A number, or a string with a number |
| `currencyCode` | `'USD'` | An ISO 4217 code, such as `'EUR'` |
| `display` | `'symbol'` | `'symbol'`, `'narrowSymbol'`, `'code'`, or `'name'` |
| `format` | the currency's own digits | Angular's digits info, or `Intl.NumberFormatOptions` |
| `locale` | the active language | |

```html
{{ 12.5 | localCurrency:'EUR' }}          <!-- 12,50 € in Finnish -->
{{ 12.5 | localCurrency:'EUR':'code' }}   <!-- 12,50 EUR -->
{{ 1234 | localCurrency:'JPY' }}          <!-- ¥1,234 in English -->
```

### localRelativeTime

```
value | localRelativeTime[:unit[:options[:locale]]]
```

| Parameter | Default | |
|---|---|---|
| `value` | | A number with a `unit`, or a `Date`, timestamp, or ISO date string |
| `unit` | the unit that fits, for a date | `'second'`, `'minute'`, `'hour'`, `'day'`, `'week'`, `'month'`, `'quarter'`, `'year'` |
| `options` | `{ numeric: 'auto' }` | `Intl.RelativeTimeFormatOptions` |
| `locale` | the active language | |

For a date, the pipe uses the time from now in the largest unit that fits. `numeric: 'auto'` gives words such as *yesterday* and *next week*; `{ numeric: 'always' }` gives *1 day ago*. Like Angular's pure pipes, the text is made when the value changes, not as time goes on.

```html
{{ -3 | localRelativeTime:'day' }}         <!-- 3 days ago -->
{{ 1 | localRelativeTime:'week' }}         <!-- next week -->
{{ comment.created | localRelativeTime }}  <!-- 5 minutes ago, yesterday, in 2 weeks -->
```

### localList

```
value | localList[:options[:locale]]
```

| Parameter | Default | |
|---|---|---|
| `value` | | An array or another iterable of strings |
| `options` | `{ type: 'conjunction', style: 'long' }` | `type`: `'conjunction'`, `'disjunction'`, or `'unit'`; `style`: `'long'`, `'short'`, or `'narrow'` |
| `locale` | the active language | |

```html
{{ ['Anna', 'Ben', 'Carl'] | localList }}                <!-- Anna, Ben ja Carl in Finnish -->
{{ ['S', 'M', 'L'] | localList:{ type: 'disjunction' } }} <!-- S, M, or L in English -->
```

### localDisplayName

```
value | localDisplayName[:type[:locale[:options]]]
```

| Parameter | Default | |
|---|---|---|
| `value` | | A code, such as `'fi'`, `'SE'`, `'EUR'`, or `'Hant'` |
| `type` | `'language'` | `'language'`, `'region'`, `'currency'`, `'script'`, `'calendar'`, or `'dateTimeField'` |
| `locale` | the active language | `'self'` gives a language's name in the language itself, for a language list |
| `options` | `{ fallback: 'code' }` | `style`, `languageDisplay`, `fallback` |

An unknown code shows as it is.

```html
{{ 'fi' | localDisplayName }}                     <!-- Finnish in English, Finnisch in German -->
{{ 'sv' | localDisplayName:'language':'self' }}   <!-- svenska -->
{{ 'SE' | localDisplayName:'region' }}            <!-- Ruotsi in Finnish -->
{{ 'EUR' | localDisplayName:'currency' }}         <!-- euro in Finnish -->
```

## Other functions and types

| Name | |
|---|---|
| `setCookie(name, value)` | Saves the language the user selected in a cookie |
| `getCookie(name)` | Reads a cookie, or `''` |
| `getTranslationsByUrl(url, id, setLocaleId?, format?)` | Loads one resource file from a URL |
| `getTranslationsById(baseUrl, id, fallbacks, setLocaleId, format?)` | Loads `<baseUrl>/<id>.<format>`, then its parents and the fallback languages |
| `parseDigitsInfo(digitsInfo)` | Angular's digits info, such as `'1.0-2'`, as `Intl.NumberFormatOptions` |
| `Translations` | The loaded translations: `Record<MessageId, TargetMessage>`, for `loadTranslations` |
| `ResourceFormat` | `'json'` or `'arb'` |
| `LocalDateFormat` | The format names of `localDate` |
| `SOLULING_API_URL`, `SOLULING_CDN_URL` | The default addresses of Soluling's API and CDN |

## Versions

| Version | |
|---|---|
| 14.5 | The pipes; `getLoadedLocale` |
| 14.4 | `getOtaLanguages`; the manifest is read once per page load |
| 14.2 | Over-the-air updates: `getOtaTranslations` |

The library supports Angular 14 and later.
