using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace RazorPagesMovie.Models
{
  public class Movie
  {
    public int ID { get; set; }

    [Required(ErrorMessage = "The Title field is required.")]  //loc No title value entered
    [StringLength(60, MinimumLength = 3)]
    [Display(Name = "Title")]  //loc Display name of Title field
    public string Title { get; set; }

    [Required(ErrorMessage = "The Release Date field is required.")]  //loc No release date value entered
    [DataType(DataType.Date)]
    [Display(Name = "Release Date")]  //loc Display name of Release Date field
    public DateTime ReleaseDate { get; set; }

    [Required(ErrorMessage = "The Genre field is required.")]
    [StringLength(30)]
    [Display(Name = "Genre")]  //loc Display name of Genre field
    public string Genre { get; set; }

    [Required(ErrorMessage = "The Price field is required.")]
    [Range(1, 100)]
    [DataType(DataType.Currency)]
    [Column(TypeName = "decimal(18, 2)")]
    [Display(Name = "Price")]  //loc Display name of Price field
    public decimal Price { get; set; }

    [Required(ErrorMessage = "The Rating field is required.")]  //loc No rating value entered.
    [RegularExpression(
      @"^[A-Z]+[a-zA-Z0-9""'\s-]*$", 
      ErrorMessage = "Rating code must contain only alphanumeric text.")]  //loc Invalid rating value entered
    [StringLength(5)]
    [Display(Name = "Rating")]  //loc Display name of Rating field
    public string Rating { get; set; }
  }
}