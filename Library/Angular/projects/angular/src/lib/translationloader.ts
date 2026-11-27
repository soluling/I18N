import { Injectable } from '@angular/core';
import { MessageId, TargetMessage } from '@angular/localize';

// A type that contains the translations
export type Translations = Record<MessageId, TargetMessage>;

// Stores the locale id of the loaded resource
let resourceLocale: string = 'en-US';

// The format of the resource files
// - json: Angular JSON, { "locale": "fi", "translations": { "id": "text" } }
// - arb: Application Resource Bundle, { "@@locale": "fi", "id": "text", "@id": { "description": "..." } }.
//   Unlike Angular JSON, the extracted file contains the descriptions, so translators see them.
export type ResourceFormat = 'json' | 'arb';

// Convert resource file data to Translations
function parseTranslations(fileData: string, format: ResourceFormat = 'json'): Translations
{
  if (!fileData)
    return null;

  let data = JSON.parse(fileData);

  if (format === 'arb')
  {
    // Every key that does not start with @ is a message. @@locale is the locale and @id the
    // metadata (description, locations) of message id.
    let translations: Translations = {};

    for (let key of Object.keys(data))
    {
      if (!key.startsWith('@') && (typeof data[key] === 'string'))
        translations[key] = data[key];
    }

    return translations;
  }

  // Converts JSON file into JavaScript object and cast that to the type of the strongly typed translation
  let translations: Translations = data.translations;
  return translations;
}

// Get a resource file URL
function getLanguageUrl(baseUrl: string, id: string, format: ResourceFormat = 'json'): string
{
  return `${baseUrl}/${id}.${format}`;
}

export function setCookie(name: string, value: string) 
{
  // Add language to the cookie
  // language=fi
  document.cookie = `${name}=${value}`;
}

export function getCookie(name: string) 
{
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
  setLocaleId: boolean = true,
  format: ResourceFormat = 'json'): Promise<Translations>
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

      return parseTranslations(data, format);
    })
    .catch(() => null);
}

// Load resources matching the give language id. If not found, use the fallback ids.
export function getTranslationsById(
  baseUrl: string,
  id: string,
  fallbacks: string[],
  setLocaleId: boolean,
  format: ResourceFormat = 'json'): Promise<Translations>
{
  return getTranslationsByUrl(getLanguageUrl(baseUrl, id, format), id, setLocaleId, format).then(translations =>
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
  
        return getTranslationsById(baseUrl, parentId, fallbacks, setLocaleId, format);
      }
      else
      {
        // No more parents. Try the first language in the fallback list, if any.
        if (fallbacks.length > 0)
        {
          let id = fallbacks.shift();
          return getTranslationsById(baseUrl, id, fallbacks, setLocaleId, format);
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
  format?: ResourceFormat;    // Specifies the format of the resource files: json (<id>.json) or arb (<id>.arb). Default json
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
  let format: ResourceFormat = 'json';

  if (options)
  {
    if (options.format)
      format = options.format;

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

  // Take the first id from the language list
  let id = languages.shift();

  // If only first should be tried clear the language list
  if (!tryAllLanguages)
    languages = [];

  // If a fallback has been specified and the language list does not contain it, add the fallback to the list
  if (fallbackId && (fallbackId !== id) && (languages.indexOf(fallbackId) == -1))
    languages.push(fallbackId);

  // Get the translations
  return getTranslationsById(baseUrl, id, languages, setLocaleId, format);
}

// Load resources matching the browser language(s)
export function getTranslations(
  baseUrl: string, 
  ingoreCountry: boolean = false, 
  tryAllLanguages: boolean = true, 
  setLocaleId: boolean = true, 
  fallbackId: string = 'en',
  locale: string = null,
  cookieName: string = null,
  format: ResourceFormat = 'json'): Promise<Translations>
{
  let options: TranslateOptions =
  {
    ingoreCountry,
    tryAllLanguages,
    setLocaleId,
    fallbackId,
    locale,
    cookieName,
    format
  };

  return getTranslationsEx(baseUrl, options);
}

// Over-the-air (OTA) translation updates from Soluling Cloud.
//
// The translations come from the files published in Soluling Cloud (Publish artifacts on the
// project's Manage page), so a translation fix or a new language reaches the users without a new
// build of the application. The application starts as fast as before: after the first visit it uses
// the translations saved in the browser, and checks for newer ones in the background - a newer file
// is used on the next load. On the first visit it downloads them, and if that fails or takes too
// long, it uses the files of the application (fallbackUrl), as getTranslationsEx does.
//
// Message ids are made from the original texts, so a published file with texts the application
// doesn't have is harmless, and a text it lacks shows in the original language. Every version of
// the application can use the same published files.

// The default address of Soluling's API
export const SOLULING_API_URL = 'https://www.soluling.com/api';

// The default address of the published files on Soluling's CDN, where the manifest is read first
export const SOLULING_CDN_URL = 'https://www.soluling.com/artifacts';

export interface OtaOptions extends TranslateOptions
{
  apiUrl?: string;                            // Soluling's API, for the manifest when the CDN doesn't have it. Default https://www.soluling.com/api
  cdnUrl?: string;                            // The published files on Soluling's CDN, where the manifest is read first. Default https://www.soluling.com/artifacts
  fallbackUrl?: string;                       // The application's own files, as in getTranslationsEx, when nothing is downloaded yet. Null for none. Default assets/i18n
  timeout?: number;                           // Milliseconds to wait for the first download before using fallbackUrl. Default 3000
  fileFilter?: (fileName: string) => boolean; // Which published files are the application's, when the project has others too. Default: the files of the format (.json or .arb)
  cache?: boolean;                            // Save the translations in the browser (localStorage). Default true
  checkInterval?: number;                     // Milliseconds between the background checks for newer translations. Default a day; 0 checks on every load
}

// One published file in the project's manifest (GET artifacts/{projectGuid}/manifest)
interface ManifestItem
{
  fileName: string;
  language: string;
  hash: string;
  size?: number;
  url: string;
}

// The translations of one language saved in the browser
interface CachedTranslations
{
  hash: string;
  data: string;
  format: ResourceFormat;
}

// The languages to try, in order: the chosen ones (cookie, locale option, or the browser's), each
// followed by its parents (de-DE -> de), and then the fallback language - the same order as
// getTranslationsEx
function getCandidateIds(options: TranslateOptions): string[]
{
  let languages: string[] = [];
  let locale = options?.locale;

  if (options?.cookieName)
    locale = getCookie(options.cookieName) || locale;

  if (locale)
    languages = locale.split(';').filter(id => !!id);
  else if (options?.ingoreCountry)
  {
    for (let language of navigator.languages)
    {
      let id = language.split('-')[0];

      if (languages.indexOf(id) == -1)
        languages.push(id);
    }
  }
  else
    languages = [...navigator.languages];

  if (options?.tryAllLanguages === false)
    languages = languages.slice(0, 1);

  let fallbackId = (options?.fallbackId != null) ? options.fallbackId : 'en';

  if (fallbackId && (languages.indexOf(fallbackId) == -1))
    languages.push(fallbackId);

  let result: string[] = [];

  for (let language of languages)
  {
    let parts = language.split('-');

    for (let i = parts.length; i > 0; i--)
    {
      let id = parts.slice(0, i).join('-');

      if (!result.some(existing => existing.toLowerCase() === id.toLowerCase()))
        result.push(id);
    }
  }

  return result;
}

// Translations change a few times a year: a check on every page load would only cost
const DEFAULT_CHECK_INTERVAL = 24 * 60 * 60 * 1000;

function getCheckedKey(projectGuid: string): string
{
  return `soluling-ota:${projectGuid}:checked`;
}

// Whether the last successful check was less than interval ago
function checkedRecently(projectGuid: string, interval: number): boolean
{
  try
  {
    let last = Number(localStorage.getItem(getCheckedKey(projectGuid)));
    return (interval > 0) && (last > 0) && (Date.now() - last < interval);
  }
  catch
  {
    return false;
  }
}

function saveChecked(projectGuid: string)
{
  try
  {
    localStorage.setItem(getCheckedKey(projectGuid), String(Date.now()));
  }
  catch
  {
    // Not saved: checked again on the next load
  }
}

function getCacheKey(projectGuid: string, id: string): string
{
  return `soluling-ota:${projectGuid}:${id.toLowerCase()}`;
}

function readCache(projectGuid: string, id: string): CachedTranslations
{
  try
  {
    let value = localStorage.getItem(getCacheKey(projectGuid, id));
    return value ? JSON.parse(value) : null;
  }
  catch
  {
    return null;
  }
}

function writeCache(projectGuid: string, id: string, value: CachedTranslations)
{
  try
  {
    localStorage.setItem(getCacheKey(projectGuid, id), JSON.stringify(value));
  }
  catch
  {
    // Storage full or blocked - the translations are only not saved
  }
}

// fetch with a timeout; null on failure
function fetchText(url: string, timeout: number): Promise<string>
{
  let controller = (typeof AbortController !== 'undefined') ? new AbortController() : null;
  let timer = (controller && (timeout > 0)) ? setTimeout(() => controller.abort(), timeout) : null;

  return fetch(url, controller ? { signal: controller.signal } : undefined)
    .then(response => response.ok ? response.text() : null)
    .catch(() => null)
    .finally(() =>
    {
      if (timer)
        clearTimeout(timer);
    });
}

// Downloads the best published language: the first candidate with a published file, unless the
// browser already has that file (same hash). Resolves to the language and its file data, or null.
function downloadTranslations(
  projectGuid: string,
  candidates: string[],
  options: OtaOptions,
  timeout: number): Promise<{ id: string, data: string }>
{
  let apiUrl = (options.apiUrl || SOLULING_API_URL).replace(/\/+$/, '');
  let cdnUrl = ((options.cdnUrl !== undefined) ? options.cdnUrl : SOLULING_CDN_URL)?.replace(/\/+$/, '');
  let format: ResourceFormat = options.format || 'json';
  let filter = options.fileFilter || ((fileName: string) => fileName.toLowerCase().endsWith('.' + format));
  let cache = options.cache !== false;

  // The static manifest on the CDN first - it costs nothing on Soluling's servers however many
  // visitors the application has - and the API's when the CDN doesn't have one yet
  let manifest = cdnUrl ? fetchText(`${cdnUrl}/${projectGuid}/manifest.json`, timeout) : Promise.resolve<string>(null);

  return manifest.then(text => text || fetchText(`${apiUrl}/artifacts/${projectGuid}/manifest`, timeout)).then(text =>
  {
    let items: ManifestItem[];

    try
    {
      items = text ? JSON.parse(text) : null;
    }
    catch
    {
      items = null;
    }

    if (!Array.isArray(items))
      return null;

    for (let id of candidates)
    {
      let item = items.find(i => i.language && (i.language.toLowerCase() === id.toLowerCase()) && filter(i.fileName));

      if (!item)
        continue;

      let cached = cache ? readCache(projectGuid, id) : null;

      if (cached && (cached.hash === item.hash) && (cached.format === format))
      {
        saveChecked(projectGuid);
        return { id, data: cached.data };
      }

      return fetchText(item.url, timeout).then(data =>
      {
        // A failed download is checked again on the next load
        if (!data)
          return null;

        if (cache)
          writeCache(projectGuid, id, { hash: item.hash, data, format });

        saveChecked(projectGuid);
        return { id, data };
      });
    }

    // Nothing of the user's languages published: checked all the same
    saveChecked(projectGuid);
    return null;
  });
}

// Load the translations of the project published in Soluling Cloud - see the comment above
export function getOtaTranslations(
  projectGuid: string,
  options: OtaOptions = null): Promise<Translations>
{
  options = options || {};

  let candidates = getCandidateIds(options);
  let format: ResourceFormat = options.format || 'json';
  let setLocaleId = options.setLocaleId !== false;
  let timeout = (options.timeout != null) ? options.timeout : 3000;
  let fallbackUrl = (options.fallbackUrl !== undefined) ? options.fallbackUrl : 'assets/i18n';

  function use(id: string, data: string): Translations
  {
    let translations = parseTranslations(data, format);

    if (translations && setLocaleId)
      resourceLocale = id;

    return translations;
  }

  // Saved before: use it now, and check for a newer one for the next load
  if (options.cache !== false)
  {
    for (let id of candidates)
    {
      let cached = readCache(projectGuid, id);

      if (cached && (cached.format === format))
      {
        // A check for newer ones for the next load - once a checkInterval
        let interval = (options.checkInterval != null) ? options.checkInterval : DEFAULT_CHECK_INTERVAL;

        if (!checkedRecently(projectGuid, interval))
          downloadTranslations(projectGuid, candidates, options, 0).catch(() => null);

        return Promise.resolve(use(id, cached.data));
      }
    }
  }

  // Nothing saved: download it, or use the application's own files
  return downloadTranslations(projectGuid, candidates, options, timeout)
    .catch(() => null)
    .then(result =>
    {
      if (result)
        return use(result.id, result.data);

      if (fallbackUrl)
        return getTranslationsEx(fallbackUrl, options);

      return null;
    });
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