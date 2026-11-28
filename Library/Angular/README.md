# @soluling/angular

Runtime translation loading, over-the-air (OTA) translation updates, and locale-aware pipes for Angular.

**The documentation is in [projects/angular/README.md](projects/angular/README.md)** - the same file that the [npm package page](https://www.npmjs.com/package/@soluling/angular) shows.

```bash
npm install @soluling/angular
```

## This folder

This is the Angular workspace of the library:

| Path | |
|---|---|
| `projects/angular` | The library: `src/lib/translationloader.ts` (loading and over-the-air updates) and `src/lib/intlpipes.ts` (the pipes) |
| `projects/angulartest` | A test application |

To build the library, and to build and publish it to npm:

```bash
npx ng build angular --configuration production
npm run buildlib
```

The version is in `projects/angular/package.json`.
