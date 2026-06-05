using Microsoft.EntityFrameworkCore;

List<string> chitchat = new List<string> { 
    "How about those clouds!",
    "Do you think the Bills will go all the way this time?",
    "What's new with you?",
    "Aren't baby groundhogs cute?",
    "I hear what you're saying.",
    "That's just what I was thinking.",
    "Could be!",
    "That's so interesting!",
    "Well with this weather, anything could happen.",
    "Well, you know what they say...",
    "You got that right!",
    "I can't complain!",
};

const int computerId = 0;

Random randomizer = new();

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

string allowedOrigins = "SmallTalkUI";

builder.Services.AddCors(options =>
        { 
        options.AddPolicy(name: allowedOrigins,
                policy => { 
                policy.WithOrigins("http://localhost:5173")
                      .AllowAnyHeader()
                      .AllowAnyMethod(); 
                      });
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

app.UseCors(allowedOrigins);

app.MapGet("/", () => "Hello!");

app.MapGet("/log", async (SmallTalkDb db) =>
        await db.Messages.ToListAsync());

app.MapPost("/history", async (User user, SmallTalkDb db) =>
        await db.Messages
                .Where(msg => msg.RecipientId == user.Id
                              || msg.SenderId == user.Id)
                .ToListAsync());

app.MapPost("/chat", async (Message message, SmallTalkDb db) =>
        {
        db.Messages.Add(message);
        int partnerId = message.SenderId; 
        // TODO really partner should first state a registered ID or request
        // one; at minimum should assert partnerId != computerId

        string text = chitchat[randomizer.Next(chitchat.Count)];
        Message response = new (computerId, partnerId, DateTimeOffset.Now, text);
        db.Messages.Add(response);
        await db.SaveChangesAsync();
        return response;
        });


app.Run();
