namespace Sports.Models
{
  public class Sport
  {
    public int Id { get; set; }
    public string Language { get; set; }
    public string Name { get; set; }
    public uint FieldPlayers { get; set; }
    public int Goalie { get; set; }
    public string Origin { get; set; }
    public string Description { get; set; }
  }
}
