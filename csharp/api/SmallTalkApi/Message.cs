using System.ComponentModel.DataAnnotations;

public class Message
{
    [Key]
    public int              Id          { get; set; }
    public int              SenderId    { get; set; }
    public int              RecipientId { get; set; }
    public DateTimeOffset   Time        { get; set; }
    public string           Text        { get; set; }

    public Message(int senderId, int recipientId, DateTimeOffset time, string text)
    {
        SenderId    = senderId;
        RecipientId = recipientId;
        Time        = time.ToUniversalTime();
        Text        = text;
    }
}
