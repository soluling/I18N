# Context sample

A Delphi VCL application for testing how context helps translation, especially AI machine
translation (Claude) in Soluling. English uses the same short word for different meanings, and
most languages don't:

| String | Meanings in the sample |
|---|---|
| Open | Ticket status (adjective), a button that reopens a ticket (verb), File > Open (verb) |
| Close | Button that closes a ticket (verb), File > Close (verb), distance to a store (adjective) |
| Free | Price (costs nothing), disk space (unused) |
| Order | Button that places an order (verb), purchase order (noun), sort order (label) |
| Post | Button that publishes a reply (verb), forum posts (noun) |
| Book | Button that reserves a table (verb) |
| Light | Color theme |

## Where the context comes from

1. **`//loc` comments on resource strings** (`Unit1.pas`). Soluling reads the comment and shows
   it to the translator, and the AI machine translation gets it with the text.
   - `MaxChars=12` limits the length of the translation (`SOpenCommand`, `SCloseCommand`).
   - `0: ..., 1: ...` describes the placeholders (`SDeleteTicket`, `SSummary`).
2. **No comment** for comparison: `SNearNoComment` is the same "Close" as `SNear`, but the
   translator only sees the id. It is shown in gray next to `SNear` in the Shop group.
3. **Form strings** (`Unit1.dfm`) have no comments. The component and property names
   (`OrderButton.Caption`, `SortLabel.Caption`, `LightMenu.Caption`) are the id, and Soluling
   shows the form visually.

Patterns use the same ambiguous words:

- Plural: `SOpenTickets`, `SPostCount` (with a zero form)
- Plural with two numbers: `SSummary`
- Select: `SStatusText` (open, pending, closed)
- Gender: `SAssigned`

## Testing

1. Build the project (Delphi 12 or later). The Soluling library is expected in
   `D:\NT\Deploy\Library\Delphi`; change the search path in the project options if it is
   elsewhere.
2. Create a Soluling project from `Win32\Debug\Context.exe` (or `Context.dproj`) and add a target
   language, for example Finnish or German.
3. Optional: add glossary terms such as *ticket* and *queue* to the account's glossary.
4. Upload the project to Soluling Cloud and translate it with the Claude engine, either in
   Translate (the suggestions popup of an empty cell) or with the automatic translation.
5. Compare:
   - `SStatusOpen` (status) vs. `SOpenCommand` (command) vs. `Form1.FileMenu.OpenMenu.Caption`
   - `SNear` (with comment) vs. `SNearNoComment` (without)
   - `SFreePrice` vs. `SFreeSpace`
   - `SOrderPlaced`, `OrderButton.Caption` and `SortLabel.Caption`
   - the `MaxChars=12` rows: the translation must fit the limit
   - the hotkey `&` and the placeholders `%s` and `%d` are kept
   - glossary terms are translated as the glossary says
6. Build the localized application and switch the language with the **Language...** button.
