using System.Reflection;
using Microsoft.Extensions.Localization;

namespace RazorPagesMovie.Models
{
  /// <summary>
  /// Dummy class to group shared resources
  /// </summary>
  public class SharedResource
  {
  }

  public class SharedLocalizer
  {
    private readonly IStringLocalizer _localizer;

    public SharedLocalizer(IStringLocalizerFactory factory)
    {
      var type = typeof(SharedResource);
      var assemblyName = new AssemblyName(type.GetTypeInfo().Assembly.FullName);
      _localizer = factory.Create("SharedResource", assemblyName.Name);
    }

    public string this[string name] { get { return _localizer[name]; } }
    public string this[string name, params object[] arguments] { get { return _localizer[name, arguments]; } }
  }
}
