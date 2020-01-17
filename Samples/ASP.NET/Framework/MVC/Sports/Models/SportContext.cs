using System.Data.Entity;

namespace Sports.Models
{
  public class SportContext: DbContext
  {
    public DbSet<Sport> Sports { get; set; }
  }
}
