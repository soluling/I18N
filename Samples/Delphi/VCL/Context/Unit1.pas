unit Unit1;

{
  Context sample

  Shows how the context that Soluling extracts helps a translator - and the AI machine translation
  (Claude) - to translate short, ambiguous strings correctly. English uses the same word for very
  different meanings:

    Open   - a command (open a file, reopen a ticket) or a status (the ticket is not solved)
    Close  - a command (close a ticket) or a distance (the store is near)
    Free   - a price (costs nothing) or an amount (unused disk space)
    Order  - a command (place an order), a noun (a purchase order) or a sort direction
    Post   - a command (publish a reply) or a noun (a forum post)
    Book   - a command (reserve a table) or a noun (a book you read)
    Light  - a color theme or a weight

  Most languages need a different word for each meaning. Without context a translator (human or
  machine) has to guess. This sample gives the context in three ways:

  1. Resource strings with a //loc comment. Soluling reads the comment from the source code and
     shows it to the translator, and gives it to the AI machine translation. The comment can also
     contain a length limit (MaxChars=N) and descriptions of the placeholders (0: ..., 1: ...).
  2. Resource strings without a comment, for comparison. The translator only sees the string id,
     for example Unit1.SNearNoComment.
  3. Form (.dfm) strings. They have no comment, but the component name and property
     (OpenButton.Caption, SortLabel.Caption) are the id, and Soluling shows the form visually.

  The sample also has patterns (plural, select, gender and a multi-plural) whose texts contain the
  same ambiguous words.

  Add a glossary to the Soluling project or account with terms such as "ticket" and "queue" to see
  that the glossary translation is used too.
}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.UITypes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Menus;

type
  TTicketStatus = (tsOpen, tsPending, tsClosed);

  TForm1 = class(TForm)
    // The menu captions are form strings: the id (FileMenu, OpenMenu, ...) is their context
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    OpenMenu: TMenuItem;
    SaveMenu: TMenuItem;
    CloseMenu: TMenuItem;
    N1: TMenuItem;
    ExitMenu: TMenuItem;
    EditMenu: TMenuItem;
    ClearMenu: TMenuItem;
    FindMenu: TMenuItem;
    ViewMenu: TMenuItem;
    ThemeMenu: TMenuItem;
    LightMenu: TMenuItem;
    DarkMenu: TMenuItem;

    // Support ticket
    TicketGroupBox: TGroupBox;
    StatusCaptionLabel: TLabel;
    StatusLabel: TLabel;
    StatusTextLabel: TLabel;
    OpenCloseButton: TButton;
    PendingButton: TButton;
    ReplyLabel: TLabel;
    ReplyEdit: TEdit;
    PostButton: TButton;
    PostsLabel: TLabel;
    AssignedLabel: TLabel;
    DeleteButton: TButton;

    // Ticket queue
    QueueGroupBox: TGroupBox;
    CountCaptionLabel: TLabel;
    CountEdit: TEdit;
    CountUpDown: TUpDown;
    QueueCaptionLabel: TLabel;
    QueueEdit: TEdit;
    QueueUpDown: TUpDown;
    OpenTicketsLabel: TLabel;
    SummaryLabel: TLabel;

    // Shop
    ShopGroupBox: TGroupBox;
    PriceCaptionLabel: TLabel;
    PriceLabel: TLabel;
    DistanceCaptionLabel: TLabel;
    DistanceLabel: TLabel;
    DistanceNoCommentLabel: TLabel;
    BookButton: TButton;
    OrderButton: TButton;
    SortLabel: TLabel;
    SortComboBox: TComboBox;
    DiskLabel: TLabel;

    ThemeLabel: TLabel;
    LanguageButton: TButton;

    procedure FormCreate(Sender: TObject);
    procedure OpenCloseButtonClick(Sender: TObject);
    procedure PendingButtonClick(Sender: TObject);
    procedure PostButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure CountEditChange(Sender: TObject);
    procedure BookButtonClick(Sender: TObject);
    procedure OrderButtonClick(Sender: TObject);
    procedure ThemeMenuClick(Sender: TObject);
    procedure ExitMenuClick(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);

  private
    FStatus: TTicketStatus;
    FPosts: Integer;
    FLightTheme: Boolean;

    procedure UpdateTexts;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  NtPattern,
  NtLanguageDlg;

resourcestring
  // 1. Short ambiguous strings WITH a comment.
  //    The comment after //loc tells the meaning. MaxChars=N limits the length of the translation,
  //    for example because the text is on a button that doesn't grow.

  // "Open" as a status (adjective) and as a command (verb)
  SStatusOpen = 'Open'; //loc Status of a support ticket (adjective): the problem is not solved yet. Not a command.
  SStatusPending = 'Pending'; //loc Status of a support ticket (adjective): waiting for an answer from the customer.
  SStatusClosed = 'Closed'; //loc Status of a support ticket (adjective): the problem has been solved.
  SOpenCommand = '&Open'; //loc MaxChars=12 Button (verb): opens a closed support ticket again. & marks the hotkey.
  SCloseCommand = '&Close'; //loc MaxChars=12 Button (verb): marks the support ticket solved. & marks the hotkey.

  // "Close" as a distance
  SNear = 'Close'; //loc Distance of the store (adjective): near, a short walk away. Not the Close command.

  // "Free" as a price and as an amount
  SFreePrice = 'Free'; //loc Price of a product (adjective): it costs nothing. Not "available" or "unused".
  SFreeSpace = '%s free'; //loc 0: Amount of disk space, for example "12 GB". Free means unused here, not "costs nothing".

  // "Post", "Book" and "Order" as commands and nouns
  SPosted = 'Your post was published.'; //loc Message after publishing. Post is a noun here: a reply in a discussion.
  SBooked = 'Table booked for %s.'; //loc 0: Time of the reservation, for example "18:30". Booked = reserved.
  SOrderPlaced = 'Order %s was placed.'; //loc 0: Number of the purchase order, for example "A-1024". Order is a noun: a purchase order.

  // "Light" as a color theme
  SLightTheme = 'Light'; //loc Name of the light color theme of the user interface. Not a weight.
  SDarkTheme = 'Dark'; //loc Name of the dark color theme of the user interface.
  STheme = 'Theme: %s'; //loc 0: Name of the color theme: Light or Dark

  // Placeholder descriptions: "0:" describes the first parameter, "1:" the second
  SDeleteTicket = 'Delete ticket %s from the %s queue?'; //loc 0: Number of the support ticket, for example "#1234", 1: Name of the queue, for example "Billing". A ticket is a support request, not a travel ticket.

  // 2. The same kind of string WITHOUT a comment. The translator - and the machine translation -
  //    only sees the id (SNearNoComment) and has to guess between "near" and "shut".
  SNearNoComment = 'Close';

  // 3. Patterns. Each part of a pattern is translated separately, and the comment applies to all
  //    the parts.

  // Plural: one part for each plural form. Many languages need more forms than English (one, other).
  SOpenTickets = '{plural, zero {No open tickets} one {%d open ticket} other {%d open tickets}}'; //loc %d: Number of support tickets that are not solved yet. Open is an adjective here.

  // Plural with two numbers: the first plural part is for the first number, the second for the second
  SSummary = '{plural, one {%d ticket} other {%d tickets}} in {plural, one {%d queue} other {%d queues}}'; //loc 0: Number of support tickets, 1: Number of queues where the tickets are waiting

  // Plural of posts: "post" is a noun here
  SPostCount = '{plural, zero {No posts yet} one {%d post} other {%d posts}}'; //loc %d: Number of replies published in the support ticket. Post is a noun: a reply.

  // Select: one part for each status value
  SStatusText = '{select, open {The ticket is open.} pending {The ticket is waiting for the customer.} closed {The ticket is closed.}}'; //loc The status of a support ticket as a sentence. Open and closed are adjectives.

  // Gender: one part for each gender of the person
  SAssigned = '{gender, male {%s assigned the ticket to himself.} female {%s assigned the ticket to herself.}}'; //loc 0: First name of the support agent

const
  // Not localized: an internal value. A constant is never extracted, unlike a resource string.
  AGENT_C = 'Anna';
  ORDER_NUMBER_C = 'A-1024';
  BOOKING_TIME_C = '18:30';
  QUEUE_NAME_C = 'Billing';
  TICKET_NUMBER_C = '#1234';
  FREE_SPACE_C = '12 GB';

procedure TForm1.FormCreate(Sender: TObject);
begin
  FStatus := tsOpen;
  FPosts := 0;
  FLightTheme := True;
  SortComboBox.ItemIndex := 0;
  UpdateTexts;
end;

// Sets every runtime text. Called again after the language is changed, because the form strings
// are translated by the language switch but runtime texts are not.
procedure TForm1.UpdateTexts;
const
  STATUS_VALUES: array[TTicketStatus] of String = ('open', 'pending', 'closed');  // Select values, not localized
begin
  // Ticket
  case FStatus of
    tsOpen: StatusLabel.Caption := SStatusOpen;
    tsPending: StatusLabel.Caption := SStatusPending;
    tsClosed: StatusLabel.Caption := SStatusClosed;
  end;

  StatusTextLabel.Caption := TMultiPattern.Format(SStatusText, STATUS_VALUES[FStatus], []);

  if FStatus = tsClosed then
    OpenCloseButton.Caption := SOpenCommand
  else
    OpenCloseButton.Caption := SCloseCommand;

  PostsLabel.Caption := TMultiPattern.Format(SPostCount, FPosts, [FPosts]);
  AssignedLabel.Caption := TMultiPattern.Format(SAssigned, geFemale, [AGENT_C]);

  // Queue
  OpenTicketsLabel.Caption := TMultiPattern.Format(SOpenTickets, CountUpDown.Position, [CountUpDown.Position]);
  SummaryLabel.Caption := TMultiPattern.Format(SSummary, [CountUpDown.Position, QueueUpDown.Position]);

  // Shop
  PriceLabel.Caption := SFreePrice;
  DistanceLabel.Caption := SNear;
  DistanceNoCommentLabel.Caption := SNearNoComment;
  DiskLabel.Caption := Format(SFreeSpace, [FREE_SPACE_C]);

  // Theme
  LightMenu.Checked := FLightTheme;
  DarkMenu.Checked := not FLightTheme;

  if FLightTheme then
    ThemeLabel.Caption := Format(STheme, [SLightTheme])
  else
    ThemeLabel.Caption := Format(STheme, [SDarkTheme]);
end;

procedure TForm1.OpenCloseButtonClick(Sender: TObject);
begin
  if FStatus = tsClosed then
    FStatus := tsOpen
  else
    FStatus := tsClosed;

  UpdateTexts;
end;

procedure TForm1.PendingButtonClick(Sender: TObject);
begin
  FStatus := tsPending;
  UpdateTexts;
end;

procedure TForm1.PostButtonClick(Sender: TObject);
begin
  Inc(FPosts);
  ReplyEdit.Clear;
  UpdateTexts;
  ShowMessage(SPosted);
end;

procedure TForm1.DeleteButtonClick(Sender: TObject);
begin
  MessageDlg(Format(SDeleteTicket, [TICKET_NUMBER_C, QUEUE_NAME_C]), mtConfirmation, [mbYes, mbNo], 0);
end;

procedure TForm1.CountEditChange(Sender: TObject);
begin
  if Assigned(CountUpDown) and Assigned(QueueUpDown) then
    UpdateTexts;
end;

procedure TForm1.BookButtonClick(Sender: TObject);
begin
  ShowMessage(Format(SBooked, [BOOKING_TIME_C]));
end;

procedure TForm1.OrderButtonClick(Sender: TObject);
begin
  ShowMessage(Format(SOrderPlaced, [ORDER_NUMBER_C]));
end;

procedure TForm1.ThemeMenuClick(Sender: TObject);
begin
  FLightTheme := Sender = LightMenu;
  UpdateTexts;
end;

procedure TForm1.ExitMenuClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.LanguageButtonClick(Sender: TObject);
begin
  if TNtLanguageDialog.Select then
    UpdateTexts;
end;

end.
