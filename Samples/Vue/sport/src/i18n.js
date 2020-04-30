import Vue from 'vue'
import VueI18n from 'vue-i18n'

Vue.use(VueI18n)

// Set the fallback language. In most cases it is English
const fallbackLocale = "en";

// Initially the active language is undefined
let activeLocale;

// Try to load a resource for the specific language
function tryLoadLocaleMessage(messages, id) 
{
  if (messages[id])
    return;
  
  try
  {
    // Try to resolve it. If it does not exist an exception is thrown
    require.resolve(`./locales/${id}.json`);

    // Get the file
    messages[id] = require(`./locales/${id}.json`);

    // Update the active locale
    if (!activeLocale)
      activeLocale = id;

    console.log(`${id} found`);
  }
  catch
  {
    console.log(`${id} not found`);
  }
}

// Local resources. Load only those resources matching browser's language and the fallback language
function loadLocaleMessages() 
{
  const messages = {}

  // Full locale match (e.g. fi-FI)
  tryLoadLocaleMessage(messages, navigator.language);

  // Language match (e.g. fi)
  if (navigator.language.indexOf('-') >= 0)
    tryLoadLocaleMessage(messages, navigator.language.split('-')[0]);

  // Fallback language (e.g. en)
  tryLoadLocaleMessage(messages, fallbackLocale);

  // If no languages were loaded then use fallback language as the active language 
  // although that was not loaded so the ultimate fallback will be the key values
  if (!activeLocale)
    activeLocale = fallbackLocale;

  return messages
}

let i18n = new VueI18n({
  fallbackLocale: fallbackLocale,
  messages: loadLocaleMessages()
});

// Turn the active language on
i18n.locale = activeLocale;

export default i18n;