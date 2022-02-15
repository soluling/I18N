import { Injectable } from '@angular/core';
import { ɵMessageId, ɵTargetMessage } from '@angular/localize';

// A type that contains the translations
export type Translations = Record<ɵMessageId, ɵTargetMessage>;

// Stores the locale id of the loaded resource
let resourceLocale: string = 'en-US';

// Convert JSON resource file data to Translations
function parseTranslations(fileData: string): Translations 
{
  // Converts JSON file into JavaScript object and cast that to the type of the strongly typed translation
  let translations: Translations = JSON.parse(fileData).translations;
  return translations;
}

// Get a resource file URL
function getLanguageUrl(baseUrl: string, id: string): string
{
  return `${baseUrl}/${id}.json`;
}

export function setCookie(name: string, value: string) 
{
  // Add language to the cookie
  // language=fi
  document.cookie = `${name}=${value}`;
}

export function getCookie(name: string) 
{
  //console.log('getCookie');

  // cookie: language=fi; other=xxx
  // return: fi
  let tag = name + "=";
  let decodedCookie = decodeURIComponent(document.cookie);
  let values = decodedCookie.split(';');

  for (let value of values) 
  {
    value = value.trim();

    if (value.indexOf(tag) == 0) 
      return value.substring(tag.length, value.length);
  }

  return "";
}

// Load resources from the specific URL
export function getTranslationsByUrl(
  url: string, 
  id: string, 
  setLocaleId: boolean = true): Promise<Translations> 
{
  return fetch(url)
    .then(response => 
    {
      if (!response.ok)
        return null;

      return response.text();
    })
    .then((data: string) => 
    {
      if (data && setLocaleId)
        resourceLocale = id;

      return parseTranslations(data);
    })
    .catch(() => null);
}

// Load resources matching the give language id. If not found, use the fallback ids.
export function getTranslationsById(baseUrl: string, id: string, fallbacks: string[], setLocaleId: boolean): Promise<Translations> 
{
  //console.log('getTranslationsById: ' + id);

  return getTranslationsByUrl(getLanguageUrl(baseUrl, id), id, setLocaleId).then(translations => 
    {
      // If translations were found return it
      if (translations)
        return translations;

      // No translations were found. Try the parent language, if any.
      let parts = id.split('-');

      if (parts.length > 1)
      {
        // Try the parent id. For example de-DE -> de
        let parentId = parts[0];

        for (let i = 1; i < parts.length - 1; i++)
          parentId = parentId + '-' + parts[i];
  
        return getTranslationsById(baseUrl, parentId, fallbacks, setLocaleId);
      }
      else
      {
        // No more parents. Try the first language in the fallback list, if any.
        if (fallbacks.length > 0)
        {
          let id = fallbacks.shift();
          return getTranslationsById(baseUrl, id, fallbacks, setLocaleId);
        }
        else
        {
          // No more fallback languages. Return null (e.g., no translations found)
          return null;
        }
      }
    });
}

export interface TranslateOptions 
{
  ingoreCountry?: boolean;    // Ignore the country part in the locale if. Default false
  tryAllLanguages?: boolean;  // Try all languages in Accept-Language. Default true
  setLocaleId?: boolean;      // Set LOCALE_ID to match the loaded resource. Default true
  fallbackId?: string;        // Specifies the fallback language/locale id. Default en
  locale?: string;            // Specifies the locale to be loaded. Separate multiple locales with a semicolon (;). If null or empty use the browser language. Default null
  cookieName?: string;        // Specifies the cookie name that stores the language. If null cookies are ignored. Default null
}

// Load resources matching the browser language(s)
export function getTranslationsEx(
  baseUrl: string, 
  options: TranslateOptions = null): Promise<Translations> 
{
  let ingoreCountry = false;
  let tryAllLanguages = true;
  let setLocaleId = true;
  let fallbackId = "en";
  let locale: string = null;
  let cookieName = null;

  if (options)
  {
    if (options.ingoreCountry != null)
      ingoreCountry = options.ingoreCountry;

    if (options.tryAllLanguages != null)
      tryAllLanguages = options.tryAllLanguages;

    if (options.setLocaleId != null)
      setLocaleId = options.setLocaleId;

    if (options.fallbackId)
      fallbackId = options.fallbackId;
      
    if (options.locale)
      locale = options.locale;

    if (options.cookieName)
      cookieName = options.cookieName;
  }

  if (cookieName)
    locale = getCookie(cookieName);

  // Get a copy of the browser languages
  let languages = [];

  if (locale)
    languages = locale.split(';');
  else if (!ingoreCountry)
    languages = [...navigator.languages];
  else
  {
    for (let language of navigator.languages)
    {
      let parts = language.split('-');
      let id = parts[0];

      if (languages.indexOf(id) == -1)
        languages.push(id);
    }
  }

  //console.log(languages);

  // Take the first id from the language list
  let id = languages.shift();

  // If only first should be tried clear the language list
  if (!tryAllLanguages)
    languages = [];

  // If a fallback has been specified and the language list does not contain it, add the fallback to the list
  if (fallbackId && (fallbackId !== id) && (languages.indexOf(fallbackId) == -1))
    languages.push(fallbackId);

  // Get the translations
  return getTranslationsById(baseUrl, id, languages, setLocaleId);
}

// Load resources matching the browser language(s)
export function getTranslations(
  baseUrl: string, 
  ingoreCountry: boolean = false, 
  tryAllLanguages: boolean = true, 
  setLocaleId: boolean = true, 
  fallbackId: string = 'en',
  locale: string = null,
  cookieName: string = null): Promise<Translations> 
{
  let options: TranslateOptions = 
  {
    ingoreCountry,
    tryAllLanguages,
    setLocaleId,
    fallbackId,
    locale,
    cookieName
  };

  return getTranslationsEx(baseUrl, options);
}

// Service that provides you the active resource locale
@Injectable()
export class LocaleService
{
  get localeId(): string
  {
    return resourceLocale;
  }
}