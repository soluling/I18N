# Driving Time Calculator

A small Vue 3 application that shows how to internationalize and localize a Vue application using [Vue I18n](https://vue-i18n.intlify.dev) and Soluling. Learn more from the [Vue localization guide](https://www.soluling.com/Guides/Vue).

| Project  | Description                                                                                         |
| -------- | --------------------------------------------------------------------------------------------------- |
| original | The original English application that has not been internationalized.                               |
| vue-i18n | The internationalized application, localized into Finnish, German, and Japanese, with a language selector. |

To run a project, go to its directory and run:

```bash
npm install
npm start
```

The strings of the `vue-i18n` project are in `src/locales/en.json`. `src/locales/en.ntp` is the Soluling project. Open it in Soluling and choose *Home | Build All* to create the localized files: `fi.json`, `de.json`, and `ja.json`.
