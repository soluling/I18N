# Soluling.AspNet

Soluling's library for ASP.NET Core applications localized with [Soluling](https://www.soluling.com).

## Over-the-air translation updates

When you translate in Soluling Cloud and publish the project's files, the application gets new translations without a new deployment. Add it after `AddLocalization`, with the project's GUID from its Manage page:

```csharp
builder.Services.AddLocalization(options => options.ResourcesPath = "Resources");
builder.Services.AddSolulingOta("1b6e2f0a-4c2d-4e9a-9f1a-example-guid");
```

`IStringLocalizer`, `IViewLocalizer`, `IHtmlLocalizer`, and the validation messages then use the published translations first, and the application's own resources for the rest. A background service checks for changed files every 30 minutes; the next request uses them.

To get a fix within seconds, let Soluling's webhook tell the application when a build is published:

```csharp
builder.Services.AddSolulingOta("1b6e2f0a-4c2d-4e9a-9f1a-example-guid",
  options => options.WebhookSecret = builder.Configuration["Soluling:WebhookSecret"]);
...
app.MapSolulingOta();  // POST /soluling/ota
```

See the [ASP.NET Core guide](https://www.soluling.com/Guides/AspNetCore#ota).

## Request localization with the deployed languages

`UseRequestLocalizationWithAvailableLanguages` and `ConfigureWithAvailableLanguages` set the supported cultures to the languages whose satellite assemblies are deployed.
