using System;
using System.IO;
using System.Net.Http;

namespace Soluling.AspNet
{
  /// <summary>
  /// Options of over-the-air (OTA) translation updates from Soluling Cloud - see
  /// <see cref="OtaServiceCollectionExtension.AddSolulingOta"/>.
  /// </summary>
  public class OtaOptions
  {
    /// <summary>The default address of Soluling's API.</summary>
    public const string DEFAULT_API_URL = "https://www.soluling.com/api";

    /// <summary>The GUID of the project in Soluling Cloud, shown on its Manage page.</summary>
    public string ProjectGuid { get; set; }

    /// <summary>The default address of the published files on Soluling's CDN.</summary>
    public const string DEFAULT_CDN_URL = "https://www.soluling.com/artifacts";

    /// <summary>
    /// The address of Soluling's API. The manifest is read from there when the CDN doesn't have it
    /// yet - a project not built since its manifest started to be published there.
    /// </summary>
    public string ApiUrl { get; set; } = DEFAULT_API_URL;

    /// <summary>
    /// The address of the published files on Soluling's CDN, where the manifest is read first:
    /// {CdnUrl}/{ProjectGuid}/manifest.json. It's a static file, so a check costs nothing on
    /// Soluling's servers.
    /// </summary>
    public string CdnUrl { get; set; } = DEFAULT_CDN_URL;

    /// <summary>
    /// The secret of the project's webhook that calls the application when a build is published -
    /// see <see cref="OtaEndpointExtension.MapSolulingOta"/>. With it, the translations are updated at
    /// once, and the application checks by itself only hourly.
    /// </summary>
    public string WebhookSecret { get; set; }

    /// <summary>
    /// How often to check for changed translations. Default 30 minutes, or an hour with a
    /// <see cref="WebhookSecret"/> - the webhook brings the changes then, and checking is only for a
    /// call that didn't arrive.
    /// </summary>
    public TimeSpan? UpdateInterval { get; set; }

    internal TimeSpan GetUpdateInterval() =>
      UpdateInterval ?? (string.IsNullOrEmpty(WebhookSecret) ? TimeSpan.FromMinutes(30) : TimeSpan.FromHours(1));

    /// <summary>
    /// The folder where the downloaded translations are kept, so they are used after a restart too,
    /// before the first check. Default: Soluling\Ota\{ProjectGuid} in the local application data
    /// folder, or in the temporary folder when there is none. Null or empty keeps them in memory only.
    /// </summary>
    public string CacheDir { get; set; }

    /// <summary>The HTTP client to use. Default: one of its own. For tests and proxies.</summary>
    public HttpClient HttpClient { get; set; }

    internal string GetCacheDir()
    {
      if (CacheDir != null)
        return CacheDir;

      var root = Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData);

      if (string.IsNullOrEmpty(root))
        root = Path.GetTempPath();

      return Path.Combine(root, "Soluling", "Ota", ProjectGuid ?? "");
    }
  }
}
