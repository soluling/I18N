using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Microsoft.EntityFrameworkCore;
using Soluling.Sport;
using SportAPI.Models;

namespace SportAPI.Controllers
{
  [Produces("application/json")]
  [Route("sports")]
  public class SportsController : Controller
  {
    private readonly SportContext context;

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

    // GET: sports
    [HttpGet]
    public IEnumerable<Sport> GetSports()
    {
      var sports = context.Sport.Include(s => s.Languages).ToArray();

      foreach (var sport in sports)
        sport.MoveDefaultLanguateToTop(Language);

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

      sport.MoveDefaultLanguateToTop(Language);
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