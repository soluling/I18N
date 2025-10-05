using System.Text;
using Newtonsoft.Json;

namespace Soluling.Sport
{
  /// <summary>
  /// Class that call    Sport API
  /// </summary>
  public class SportService
  {
    // By default this library uses the API on Soluling's server
    // If you want to use a local API comment the folloiwng line and uncomment the line after that. Check the port.
    private const string URL = "https://soluling.com/SportApi/sports";
    //private const string URL = "http://localhost:53783/sports";

    public async Task<Sport[]> GetAllAsync(string language)
    {
      using var client = new HttpClient();
      client.DefaultRequestHeaders.Add("Accept-Language", language);

      var response = await client.GetAsync(URL);

      if (response.StatusCode != System.Net.HttpStatusCode.OK)
        return null;

      var payload = response.Content.ReadAsStringAsync().Result;

      if (payload == null)
        return null;

      return JsonConvert.DeserializeObject<Sport[]>(payload);
    }

    public async Task<Sport> GetAsync(int id, string language)
    {
      using var client = new HttpClient();
      client.DefaultRequestHeaders.Add("Accept-Language", language);

      var response = client.GetAsync(URL + $"/{id}").Result;

      if (response.StatusCode != System.Net.HttpStatusCode.OK)
        return null;

      var payload = await response.Content.ReadAsStringAsync();

      if (payload == null)
        return null;

      return JsonConvert.DeserializeObject<Sport>(payload);
    }

    public async Task<Sport> AddAsync(Sport value)
    {
      using var client = new HttpClient();
      using var request = new HttpRequestMessage(HttpMethod.Post, URL);
      var payload = JsonConvert.SerializeObject(value);

      using var content = new StringContent(payload, Encoding.UTF8, "application/json");
      request.Content = content;

      using var response = await client.SendAsync(request, HttpCompletionOption.ResponseHeadersRead);

      if (response.StatusCode != System.Net.HttpStatusCode.OK)
        return null;

      payload = await response.Content.ReadAsStringAsync();

      if (payload == null)
        return null;

      return JsonConvert.DeserializeObject<Sport>(payload);
    }

    public async Task<Sport> EditAsync(Sport value)
    {
      using var client = new HttpClient();
      using var request = new HttpRequestMessage(HttpMethod.Patch, URL);
      var payload = JsonConvert.SerializeObject(value);

      using var content = new StringContent(payload, Encoding.UTF8, "application/json");
      request.Content = content;

      using var response = await client.SendAsync(request, HttpCompletionOption.ResponseHeadersRead);

      if (response.StatusCode != System.Net.HttpStatusCode.OK)
        return null;

      payload = await response.Content.ReadAsStringAsync();

      if (payload == null)
        return null;

      return JsonConvert.DeserializeObject<Sport>(payload);
    }

    public async Task<Sport[]> SetDefaultAsync()
    {
      using var client = new HttpClient();
      using var response = await client.PostAsync(URL + "/initialize", null);

      if (response.StatusCode != System.Net.HttpStatusCode.OK)
        return null;

      var payload = await response.Content.ReadAsStringAsync();

      if (payload == null)
        return null;

      return JsonConvert.DeserializeObject<Sport[]>(payload);
    }

    public async Task<bool> DeleteAsync(int id)
    {
      using var client = new HttpClient();
      var response = await client.DeleteAsync(URL + $"/{id}");

      return response.StatusCode == System.Net.HttpStatusCode.OK;
    }
  }
}
