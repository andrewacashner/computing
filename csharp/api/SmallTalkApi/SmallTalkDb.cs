using Microsoft.EntityFrameworkCore;

class SmallTalkDb : DbContext
{
    public SmallTalkDb(DbContextOptions<SmallTalkDb> options)
        : base(options) { }

    public DbSet<Message> Messages => Set<Message>();
}
