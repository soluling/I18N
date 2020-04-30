function getLanguage()
{
  console.log('getLanguage');
  return navigator.language || navigator.userLanguage;
}