using Microsoft.EntityFrameworkCore;
using Soluling.Sport;

namespace SportAPI.Models
{
  public class SportContext : DbContext
  {
    public SportContext (DbContextOptions<SportContext> options): base(options)
    {
    }

    public DbSet<Sport> Sport { get; set; }
    public DbSet<SportLanguage> SportLanguage { get; set; }
  }
}
