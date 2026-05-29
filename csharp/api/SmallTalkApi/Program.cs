using Microsoft.EntityFrameworkCore;

var builder = WebApplication.CreateBuilder(args);

builder.Services.AddDbContext<SmallTalkDb>(opt => 
        opt.UseInMemoryDatabase("SmallTalk"));
builder.Services.AddDatabaseDeveloperPageExceptionFilter();

builder.Services.AddEndpointsApiExplorer();
builder.Services.AddOpenApiDocument(config =>
        {
        config.DocumentName = "SmallTalkApi";
        config.Title        = "SmallTalk API";
        config.Version      = "0";
        });

var app = builder.Build();

if (app.Environment.IsDevelopment())
{
    app.UseOpenApi();
    app.UseSwaggerUi(config =>
            {
            config.DocumentTitle = "SmallTalk API";
            config.Path          = "/swagger";
            config.DocumentPath  = "/swagger/{documentName}/swagger.json";
            config.DocExpansion  = "list";
            });
}

app.MapGet("/", () => "Hello!");

app.MapGet("/log", async (SmallTalkDb db) =>
        await db.Messages.ToListAsync());

app.MapPost("/chat", async (Message message, SmallTalkDb db) =>
        {
        db.Messages.Add(message);
        // return Results.Created($"/chat/{message.Id}", message);
        Message response = new (message.Id + 1, "Nice day, isn't it?");
        db.Messages.Add(response);
        await db.SaveChangesAsync();
        return response;
        });

app.Run();
