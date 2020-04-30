using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Microsoft.EntityFrameworkCore;
using Soluling.MachineTranslation;
using Soluling.Sport;
using SportAPI.Models;

namespace SportAPI.Controllers
{
  [Produces("application/json")]
  [Route("sports")]
  public class SportsController : Controller
  {
    private readonly SportContext context;
    private static MachineTranslator machineTranslator;

    static SportsController()
    {
      var key = Startup.Configuration["MicrosoftTranslator:Key"];
      var endpoint = Startup.Configuration["MicrosoftTranslator:Endpoint"];

      if (!string.IsNullOrEmpty(key) && !string.IsNullOrEmpty(endpoint))
        machineTranslator = new MicrosoftTranslator(key, endpoint);
      else
        machineTranslator = null;
    }

    public SportsController(SportContext context)
    {
      this.context = context;
    }

    public string Language 
    { 
      get 
      { 
        // Language is passed either using Accept-Language header, culture/ui-culture URL parameter, or cookie.
        // ASP.NET processes it and then sets thread's current culture.
        return Thread.CurrentThread.CurrentCulture.TwoLetterISOLanguageName;
      }
    }

    private void ProcessSport(Sport sport)
    {
      // If the language does not exist and machine translator exit, use it to translate the sport to that language.
      var targetSportLanguage = sport.Languages.Find(m => m.Language == Language);

      if ((targetSportLanguage == null) && (machineTranslator != null) && (sport.Languages.Count > 0))
      {
        // Find the English sport to be used as the original language.
        var originalSportLanguage = sport.Languages.Find(m => m.Language == "en");

        // If not found take the first sport.
        if (originalSportLanguage == null)
          originalSportLanguage = sport.Languages[0];

        // Create a new language specific sport using the machine translated properties
        targetSportLanguage = new SportLanguage
        {
          Language = Language,
          MachineTranslated = true,
          Name = machineTranslator.Translate(originalSportLanguage.Name, originalSportLanguage.Language, Language),
          Origin = machineTranslator.Translate(originalSportLanguage.Origin, originalSportLanguage.Language, Language),
          Description = machineTranslator.Translate(originalSportLanguage.Description, originalSportLanguage.Language, Language)
        };

        // If the machine translation succeeded add the sport language to the list and sav it to the DB
        if (!string.IsNullOrEmpty(targetSportLanguage.Name) && !string.IsNullOrEmpty(targetSportLanguage.Origin) && !string.IsNullOrEmpty(targetSportLanguage.Description))
        {
          sport.AddLanguage(targetSportLanguage);
          context.SaveChanges();
        }
      }

      // Move the active language to the first in the list
      sport.MoveDefaultLanguateToTop(Language);
    }

    // GET: sports
    [HttpGet]
    public IEnumerable<Sport> GetSports()
    {
      var sports = context.Sport.Include(s => s.Languages).ToArray();

      foreach (var sport in sports)
        ProcessSport(sport);

      return sports;
    }

    // GET: sports/<id>
    [HttpGet("{id}")]
    public IActionResult GetSport(int id)
    {
      if (!ModelState.IsValid)
        return BadRequest(ModelState);

      var sport = context.Sport.Include(s => s.Languages).SingleOrDefault(s => s.Id == id);

      if (sport == null)
        return NotFound();

      ProcessSport(sport);
      return Ok(sport);
    }

    // GET: sports/<id>/<language>
    [HttpGet("{id}/{language}")]
    public async Task<IActionResult> GetSportLanguage(int id, string language)
    {
      if (!ModelState.IsValid)
        return BadRequest(ModelState);

      var sport = await context.Sport.Include(s => s.Languages).SingleOrDefaultAsync(s => s.Id == id);

      if (sport == null)
        return NotFound();

      var sportLanguage = sport.Languages.FirstOrDefault(l => l.Language == language);

      if (sportLanguage == null)
        return NotFound();

      return Ok(sportLanguage);
    }

    // POST: sports
    [HttpPost]
    public async Task<IActionResult> PostSport([FromBody]Sport sport)
    {
      if (!ModelState.IsValid)
        return BadRequest(ModelState);

      context.Sport.Add(sport);
      await context.SaveChangesAsync();

      return CreatedAtAction("PostSport", new { id = sport.Id }, sport);
    }

    // POST: sports/<id>
    [HttpPost("{id}")]
    public async Task<IActionResult> PostSportLanguage(int id, [FromBody]SportLanguage sportLanguage)
    {
      if (!ModelState.IsValid)
        return BadRequest(ModelState);

      var sport = await context.Sport.Include(s => s.Languages).SingleOrDefaultAsync(s => s.Id == id);

      if (sport == null)
        return NotFound();

      sport.AddLanguage(sportLanguage);
      context.SportLanguage.Add(sportLanguage);
      await context.SaveChangesAsync();

      return CreatedAtAction("PostSportLanguage", new { id = sportLanguage.Id }, sportLanguage);
    }

    // POST: sports/initialize
    [HttpPost("initialize")]
    public async Task<IEnumerable<Sport>> PostDefault()
    {
      // Remove items from DB
      context.SportLanguage.RemoveRange(context.SportLanguage);
      context.Sport.RemoveRange(context.Sport);
      await context.SaveChangesAsync();

      // Seed the DB
      SeedData.Initialize(context);

      return GetSports();
    }

    // PUT: sports/<id>
    [HttpPut("{id}")]
    public async Task<IActionResult> PutSport(int id, [FromBody]Sport sport)
    {
      if (!ModelState.IsValid)
        return BadRequest(ModelState);

      if (id != sport.Id)
        return BadRequest();

      context.Entry(sport).State = EntityState.Modified;
      context.Entry(sport.Languages[0]).State = EntityState.Modified;

      try
      {
        await context.SaveChangesAsync();
      }
      catch (DbUpdateConcurrencyException)
      {
        if (!SportExists(id))
          return NotFound();
        else
          throw;
      }

      return CreatedAtAction("PutSport", new { id = sport.Id }, sport);
    }

    private bool SportExists(int id)
    {
      return context.Sport.Any(s => s.Id == id);
    }

    // DELETE: api/sports/<id>
    [HttpDelete("{id}")]
    public async Task<IActionResult> DeleteSport(int id)
    {
      var sport = await context.Sport.Include(s => s.Languages).SingleOrDefaultAsync(s => s.Id == id);

      context.Sport.Remove(sport);
      await context.SaveChangesAsync();

      return Ok();
    }

    // DELETE: sports/<id>/<language>
    [HttpDelete("{id}/{language}")]
    public async Task<IActionResult> DeleteSportLanguage(int id, string language)
    {
      var sportLanguage = await context.SportLanguage.SingleOrDefaultAsync(s => (s.SportId == id) && (s.Language == language));

      if ((sportLanguage == null) && (language.IndexOf('-') >= 0))
      {
        var neutralLanguage = language.Split('-')[0];

        sportLanguage = await context.SportLanguage.SingleOrDefaultAsync(s => (s.SportId == id) && (s.Language == neutralLanguage));
      }

      if (sportLanguage == null)
        return NotFound();

      context.SportLanguage.Remove(sportLanguage);
      await context.SaveChangesAsync();

      return Ok();
    }
  }
}