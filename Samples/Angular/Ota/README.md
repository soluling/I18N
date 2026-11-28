# OTA

An Angular application that gets its translations over the air (OTA) from Soluling Cloud with `getOtaTranslations` of [@soluling/angular](https://www.npmjs.com/package/@soluling/angular). When you change a translation in Soluling Cloud and publish the files, the next load of the application shows it, without a new build or deployment. Learn more from [Update the translations over the air](https://www.soluling.com/Guides/Angular#ota) and [Over-the-air updates](https://www.soluling.com/Documentation/OTA).

| File                    | Description                                                                                  |
| ----------------------- | -------------------------------------------------------------------------------------------- |
| `src/ota.config.ts`     | The GUID of your Soluling Cloud project. Empty: only the application's own files are used.  |
| `src/main.ts`           | Gets the translations before the application starts: over the air, or from the application's own files. |
| `src/locale/messages.arb` | The extracted original strings with their descriptions (`npm run extract`).               |
| `src/locale/messages.ntp` | The Soluling project. *Home \| Build All* writes `fi.arb`, `de.arb`, and `ja.arb` next to `messages.arb`. |
| `src/locale/fi.arb`, ... | The application's own translations, used until the first download, or if it fails. `angular.json` copies them to the `locale` folder of the application. |

The localized files are named by the language only, such as `fi.arb`, because the library loads the application's own files from `locale/<language>.arb`. The files published in Soluling Cloud can have any name: the application finds them by language.

## Run

```bash
npm install
npm start
```

Without a project GUID, the application works as the Runtime sample: it loads the translations of the browser's language from its own files. Select a language to change it; the language is kept in a cookie.

## Get the translations over the air

1. Sign in to [Soluling Cloud](https://www.soluling.com), add a project on the dashboard, and upload `src/locale/messages.arb`. Choose *Angular resource file* as the source type, and Finnish, German, and Japanese as the target languages. The project must be made from the ARB file, so that the published files are ARB files that the application can load. Translators see the descriptions as comments.
2. Translate the project in Translate.
3. On the project's *Manage* page, turn on *Publish artifacts* on the *Integrations* tab, and build the localized files (*Build localized files*). Copy the project's GUID from the *Overview* tab.
4. Set `SOLULING_PROJECT_GUID` in `src/ota.config.ts`, and run the application. To keep the GUID out of git, copy `src/ota.config.ts` to `src/ota.config.local.ts` (git ignores it), set the GUID there, and run `npm run start:local`.
5. Change a translation in Soluling Cloud, build, and publish again. Load the page again: it shows the new translation.

The first visit downloads the translations. After that, the application starts with the translations saved in the browser and checks for newer ones in the background, so a newer file is used on the next load. The sample checks on every load (`checkInterval: 0`); leave that out in your application, and it checks once a day.

A language that isn't published yet, and a text that a published file doesn't have, use the application's own files and the original texts.

## Add a language over the air

Add the language to the project in Soluling Cloud, translate, build, and publish. A user whose browser prefers that language gets it on the next load, without a new build. The date is formatted with the `localDate` pipe of @soluling/angular (14.5.0 or later), which uses the browser's `Intl` API. The browser has the formats of practically every language, so a new language gets its own date format too, such as *lördag 28 november 2026* in Swedish. Angular's own pipes need Angular's locale data, which is code in the bundle and can't come over the air: `src/app/bootstrap.ts` registers the locale data of the release's languages, and uses the original language as `LOCALE_ID` for a language without it, so the application doesn't fail. The language list on the page has the languages of the application itself and the languages published in Soluling Cloud (`getOtaLanguages`, @soluling/angular 14.4.0 or later), each named in its own language, so a new language is in the list too.
