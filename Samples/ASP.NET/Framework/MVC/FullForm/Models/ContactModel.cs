using System.ComponentModel.DataAnnotations;

namespace FullForm.Models
{
  public class ContactModel
  {
    [Required(ErrorMessage = "Required")]
    public string Name { get; set; }

    [Required(ErrorMessage = "Required")]
    [RegularExpression(@".*@.*", ErrorMessage = "Must be a valid email")]
    public string Email { get; set; }

    [Required(ErrorMessage = "Required")]
    public string Comments { get; set; }

    public bool HasValue
    {
      get { return (Name != null) || (Email != null) || (Comments != null); }
    }
  }
}