# Driving Time Calculator

A small React application that shows how to internationalize and localize a React application using [React Intl](https://formatjs.github.io/docs/react-intl/) and Soluling. Learn more from the [React localization guide](https://www.soluling.com/Guides/React).

| Project    | Description                                                                                         |
| ---------- | --------------------------------------------------------------------------------------------------- |
| original   | The original English application that has not been internationalized.                               |
| react-intl | The internationalized application, localized into Finnish, German, and Japanese, with a language selector. |

To run a project, go to its directory and run:

```bash
npm install
npm start
```

To extract the strings of the `react-intl` project into `src/translations/en.json`, run:

```bash
npm run extract
```

`src/translations/en.ntp` is the Soluling project. Open it in Soluling and choose *Home | Build All* to create the localized files: `fi.json`, `de.json`, and `ja.json`.
