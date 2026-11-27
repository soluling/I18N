using System;
using System.IO;
using System.Security.Cryptography;
using System.Text;
using System.Text.Json;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Routing;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Options;

namespace Soluling.AspNet
{
  /// <summary>
  /// The webhook endpoint of over-the-air (OTA) translation updates: Soluling calls it when a build
  /// is published, so the application updates its translations at once instead of waiting for its
  /// next check.
  /// </summary>
  public static class OtaEndpointExtension
  {
    /// <summary>The largest webhook call read - Soluling's calls are a few hundred bytes.</summary>
    private const int MAX_BODY = 64 * 1024;

    /// <summary>
    /// Maps the endpoint Soluling's webhook calls when a build is published. In Soluling, add a
    /// webhook on the project's Manage page (Integrations) with the URL of this endpoint, such as
    /// https://example.com/soluling/ota, and the Build finished event. Give its secret to the
    /// application:
    /// <code>
    /// builder.Services.AddSolulingOta(projectGuid, options => options.WebhookSecret = builder.Configuration["Soluling:WebhookSecret"]);
    /// ...
    /// app.MapSolulingOta();
    /// </code>
    /// The endpoint answers only calls signed with the secret (the X-Soluling-Signature header), and
    /// is not there (404) without a secret. A call of another project is ignored.
    /// </summary>
    /// <param name="endpoints">The application's endpoints.</param>
    /// <param name="pattern">The path of the endpoint.</param>
    /// <returns>The endpoint's builder, for more conventions such as allowing anonymous calls.</returns>
    public static IEndpointConventionBuilder MapSolulingOta(this IEndpointRouteBuilder endpoints, string pattern = "/soluling/ota")
    {
      return endpoints.MapPost(pattern, async (HttpContext context, OtaUpdateService service, IOptions<OtaOptions> options, ILogger<OtaUpdateService> logger) =>
      {
        var settings = options.Value;

        if (string.IsNullOrEmpty(settings.WebhookSecret))
          return Results.NotFound();

        if (context.Request.ContentLength > MAX_BODY)
          return Results.StatusCode(StatusCodes.Status413PayloadTooLarge);

        string body;

        using (var reader = new StreamReader(context.Request.Body, Encoding.UTF8))
        {
          var buffer = new char[MAX_BODY + 1];
          var length = await reader.ReadBlockAsync(buffer, 0, buffer.Length);

          if (length > MAX_BODY)
            return Results.StatusCode(StatusCodes.Status413PayloadTooLarge);

          body = new string(buffer, 0, length);
        }

        if (!IsSigned(settings.WebhookSecret, body, context.Request.Headers["X-Soluling-Signature"].ToString()))
          return Results.Unauthorized();

        // A webhook of another project with the same secret: nothing to update here
        if (!IsProject(body, settings.ProjectGuid))
          return Results.Ok();

        var eventName = context.Request.Headers["X-Soluling-Event"].ToString();

        if ((eventName == "build.completed") || (eventName == "test"))
        {
          // Answered at once - Soluling waits only briefly - and updated in the background
          _ = Task.Run(async () =>
          {
            try
            {
              if (await service.UpdateAsync())
                logger.LogInformation("Soluling OTA: translations updated by a webhook call");
            }
            catch (Exception ex)
            {
              logger.LogWarning(ex, "Soluling OTA: could not update after a webhook call");
            }
          });
        }

        return Results.Ok();
      });
    }

    /// <summary>
    /// Whether the signature is Soluling's for the body: sha256= and the hex HMAC-SHA256 of the
    /// body with the webhook's secret, compared in constant time.
    /// </summary>
    public static bool IsSigned(string secret, string body, string signature)
    {
      if (string.IsNullOrEmpty(secret) || string.IsNullOrEmpty(signature))
        return false;

      var hash = HMACSHA256.HashData(Encoding.UTF8.GetBytes(secret), Encoding.UTF8.GetBytes(body ?? ""));
      var expected = "sha256=" + Convert.ToHexString(hash).ToLowerInvariant();

      return CryptographicOperations.FixedTimeEquals(Encoding.UTF8.GetBytes(expected), Encoding.UTF8.GetBytes(signature.Trim()));
    }

    // Whether the call's project (project.guid in the body) is the application's. A body without
    // one, such as an older format, counts as the application's.
    private static bool IsProject(string body, string projectGuid)
    {
      try
      {
        using var document = JsonDocument.Parse(body);

        if (document.RootElement.TryGetProperty("project", out var project) &&
          project.TryGetProperty("guid", out var guid) &&
          (guid.ValueKind == JsonValueKind.String))
        {
          return string.Equals(guid.GetString(), projectGuid, StringComparison.OrdinalIgnoreCase);
        }
      }
      catch (JsonException)
      {
      }

      return true;
    }
  }
}
