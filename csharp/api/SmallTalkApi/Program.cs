using Microsoft.EntityFrameworkCore;

var builder = WebApplication.CreateBuilder(args);

// DATABASE
builder.Services.AddDbContextPool<SmallTalkDb>(opt => 
        opt.UseSqlite(
                builder.Configuration.GetConnectionString("DefaultConnection")));

builder.Services.AddEndpointsApiExplorer();
builder.Services.AddOpenApiDocument(config =>
        {
        config.DocumentName = "SmallTalkApi";
        config.Title        = "SmallTalk API";
        config.Version      = "0";
        });

var app = builder.Build();

// SWAGGER
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

int myId = 0;

app.MapGet("/", () => "Hello!");

app.MapGet("/log", async (SmallTalkDb db) =>
        await db.Messages.ToListAsync());

app.MapPost("/chat", async (Message message, SmallTalkDb db) =>
        {
        db.Messages.Add(message);
        int partnerId = message.SenderId; 
        // TODO really partner should first state a registered ID or request
        // one; at minimum should assert partnerId != myId

        // return Results.Created($"/chat/{message.Id}", message);
        Message response = new (myId, partnerId, DateTimeOffset.Now, 
                                "Nice day, isn't it?");
        db.Messages.Add(response);
        await db.SaveChangesAsync();
        return response;
        });

app.Run();
