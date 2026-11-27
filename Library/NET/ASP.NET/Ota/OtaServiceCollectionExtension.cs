using System;
using System.Linq;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Localization;
using Microsoft.Extensions.Options;

namespace Soluling.AspNet
{
  /// <summary>
  /// Over-the-air (OTA) translation updates from Soluling Cloud for ASP.NET Core.
  /// </summary>
  public static class OtaServiceCollectionExtension
  {
    /// <summary>
    /// Uses the translations published in Soluling Cloud (Publish artifacts on the project's Manage
    /// page) in the application's localizers, so a translation fix or a new language reaches the
    /// users without a new deployment. A background service checks for changed files every few
    /// minutes; a change is used by the next request. The strings the published files don't have
    /// come from the application's own resources (satellite assemblies) as before, and a published
    /// text whose placeholders differ from the original's is not used.
    ///
    /// Call it after AddLocalization (it calls AddLocalization itself when not called before):
    /// <code>
    /// builder.Services.AddLocalization(options => options.ResourcesPath = "Resources");
    /// builder.Services.AddSolulingOta("1b6e2f0a-4c2d-4e9a-9f1a-example-guid");
    /// </code>
    /// </summary>
    /// <param name="services">The application's services.</param>
    /// <param name="projectGuid">The GUID of the project in Soluling Cloud, shown on its Manage page.</param>
    /// <param name="configure">Changes the other options, or null.</param>
    /// <returns>The services.</returns>
    public static IServiceCollection AddSolulingOta(this IServiceCollection services, string projectGuid, Action<OtaOptions> configure = null)
    {
      services.Configure<OtaOptions>(options =>
      {
        options.ProjectGuid = projectGuid;
        configure?.Invoke(options);
      });

      var registered = services.LastOrDefault(d => d.ServiceType == typeof(IStringLocalizerFactory));

      if (registered == null)
      {
        services.AddLocalization();
        registered = services.Last(d => d.ServiceType == typeof(IStringLocalizerFactory));
      }

      // The factory registered before becomes the inner one of the OTA factory
      services.Remove(registered);

      services.AddSingleton(provider => new OtaStore(provider.GetRequiredService<IOptions<OtaOptions>>().Value));
      services.AddSingleton<IStringLocalizerFactory>(provider => new OtaStringLocalizerFactory(CreateInner(provider, registered), provider.GetRequiredService<OtaStore>()));
      // One instance: the hosted service's timer and the webhook endpoint (MapSolulingOta) share it
      services.AddSingleton<OtaUpdateService>();
      services.AddHostedService(provider => provider.GetRequiredService<OtaUpdateService>());

      return services;
    }

    private static IStringLocalizerFactory CreateInner(IServiceProvider provider, ServiceDescriptor descriptor)
    {
      if (descriptor.ImplementationInstance != null)
        return (IStringLocalizerFactory)descriptor.ImplementationInstance;

      if (descriptor.ImplementationFactory != null)
        return (IStringLocalizerFactory)descriptor.ImplementationFactory(provider);

      return (IStringLocalizerFactory)ActivatorUtilities.CreateInstance(provider, descriptor.ImplementationType);
    }
  }
}
