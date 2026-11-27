using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Options;

namespace Soluling.AspNet
{
  /// <summary>
  /// Checks Soluling Cloud for changed translations at start-up and then every
  /// <see cref="OtaOptions.UpdateInterval"/>: reads the project's manifest, downloads the changed
  /// .resx files and satellite assemblies, and gives them to the <see cref="OtaStore"/>. Requests
  /// never wait for it - they use the translations the store has.
  /// </summary>
  public class OtaUpdateService : BackgroundService
  {
    private static readonly HttpClient SharedClient = new() { Timeout = TimeSpan.FromSeconds(30) };

    private readonly OtaOptions options;
    private readonly OtaStore store;
    private readonly ILogger<OtaUpdateService> logger;

    /// <summary>Creates the service.</summary>
    public OtaUpdateService(IOptions<OtaOptions> options, OtaStore store, ILogger<OtaUpdateService> logger)
    {
      this.options = options.Value;
      this.store = store;
      this.logger = logger;
    }

    /// <inheritdoc/>
    protected override async Task ExecuteAsync(CancellationToken stoppingToken)
    {
      while (!stoppingToken.IsCancellationRequested)
      {
        try
        {
          await UpdateAsync(stoppingToken);
        }
        catch (OperationCanceledException) when (stoppingToken.IsCancellationRequested)
        {
          break;
        }
        catch (Exception ex)
        {
          logger.LogWarning(ex, "Soluling OTA: could not check for new translations");
        }

        try
        {
          await Task.Delay(options.GetUpdateInterval(), stoppingToken);
        }
        catch (OperationCanceledException)
        {
          break;
        }
      }
    }

    // One published file in the manifest (GET artifacts/{projectGuid}/manifest)
    private class ManifestItem
    {
      public string FileName { get; set; }
      public string Language { get; set; }
      public string Hash { get; set; }
      public string Url { get; set; }
    }

    // One check at a time: the timer and a webhook call can come together
    private readonly SemaphoreSlim updating = new(1, 1);

    /// <summary>
    /// Checks once: downloads the changed files and updates the store. Returns whether the
    /// translations changed. Called by the timer, and by the webhook endpoint
    /// (<see cref="OtaEndpointExtension.MapSolulingOta"/>).
    /// </summary>
    public async Task<bool> UpdateAsync(CancellationToken cancellationToken = default)
    {
      if (string.IsNullOrEmpty(options.ProjectGuid))
        throw new InvalidOperationException("OtaOptions.ProjectGuid is not set");

      await updating.WaitAsync(cancellationToken);

      try
      {
        return await DoUpdateAsync(cancellationToken);
      }
      finally
      {
        updating.Release();
      }
    }

    // The manifest: the static file on the CDN, or the API's when the CDN doesn't have one yet. Null
    // when neither answered.
    private async Task<string> GetManifestAsync(HttpClient client, CancellationToken cancellationToken)
    {
      if (!string.IsNullOrEmpty(options.CdnUrl))
      {
        using var response = await client.GetAsync($"{options.CdnUrl.TrimEnd('/')}/{options.ProjectGuid}/manifest.json", cancellationToken);

        if (response.IsSuccessStatusCode)
          return await response.Content.ReadAsStringAsync(cancellationToken);
      }

      var apiUrl = (options.ApiUrl ?? OtaOptions.DEFAULT_API_URL).TrimEnd('/');
      using var apiResponse = await client.GetAsync($"{apiUrl}/artifacts/{options.ProjectGuid}/manifest", cancellationToken);

      if (apiResponse.IsSuccessStatusCode)
        return await apiResponse.Content.ReadAsStringAsync(cancellationToken);

      logger.LogWarning("Soluling OTA: the manifest of project {Project} answered {Status}", options.ProjectGuid, (int)apiResponse.StatusCode);
      return null;
    }

    private async Task<bool> DoUpdateAsync(CancellationToken cancellationToken)
    {
      var client = options.HttpClient ?? SharedClient;
      var json = await GetManifestAsync(client, cancellationToken);

      if (json == null)
        return false;
      var items = JsonSerializer.Deserialize<List<ManifestItem>>(json, new JsonSerializerOptions { PropertyNameCaseInsensitive = true }) ?? new List<ManifestItem>();

      // The application's files: .resx files and satellite assemblies of a language
      items = items
        .Where(item => !string.IsNullOrEmpty(item.Language) && !string.IsNullOrEmpty(item.Url) &&
          (item.FileName.EndsWith(".resx", StringComparison.OrdinalIgnoreCase) || item.FileName.EndsWith(".resources.dll", StringComparison.OrdinalIgnoreCase)))
        .ToList();

      var current = store.Files;
      var files = new Dictionary<string, OtaFile>(StringComparer.OrdinalIgnoreCase);
      var changed = items.Count != current.Count;
      var cacheDir = string.IsNullOrEmpty(store.CacheDir) ? Path.Combine(Path.GetTempPath(), "Soluling", "Ota", options.ProjectGuid) : store.CacheDir;

      Directory.CreateDirectory(cacheDir);

      foreach (var item in items)
      {
        if (current.TryGetValue(item.FileName, out var existing) && (existing.Hash == item.Hash) && File.Exists(existing.LocalPath))
        {
          files[item.FileName] = existing;
          continue;
        }

        // Named by the hash: a new version never overwrites a file a reader may have open
        var localPath = Path.Combine(cacheDir, item.Hash + Path.GetExtension(item.FileName));
        var bytes = await client.GetByteArrayAsync(item.Url, cancellationToken);
        await File.WriteAllBytesAsync(localPath, bytes, cancellationToken);

        files[item.FileName] = new OtaFile { FileName = item.FileName, Language = item.Language, Hash = item.Hash, LocalPath = localPath };
        changed = true;
      }

      if (!changed)
        return false;

      store.Replace(files);
      logger.LogInformation("Soluling OTA: {Count} files of project {Project} in use", files.Count, options.ProjectGuid);

      // The files no longer published
      foreach (var old in current.Values.Where(old => !files.Values.Any(file => file.LocalPath == old.LocalPath)))
      {
        try
        {
          File.Delete(old.LocalPath);
        }
        catch
        {
          // Removed at a later update
        }
      }

      return true;
    }
  }
}
