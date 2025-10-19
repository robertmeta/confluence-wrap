# confluence.el - Emacs Interface for Confluence CLI

An Emacs package providing a full-featured interface to the Confluence CLI tool, with enhanced accessibility support for Emacspeak users.

## Features

- **Search Confluence** pages with keyword queries
- **View pages** with clean, readable formatting
- **Quick page info** without opening full view
- **Open in browser** for web-based editing
- **List spaces** to browse available Confluence spaces
- **Enhanced Emacspeak support** with custom line reading and auditory icons
- **Evil mode integration** (starts in Emacs state)

## Requirements

- Emacs 27.1 or later
- [Confluence CLI](https://www.npmjs.com/package/confluence-cli) installed and configured
- Optional: Emacspeak for enhanced audio feedback

## Installation

### Install and Configure Confluence CLI

```bash
# Install confluence-cli
npm install -g confluence-cli

# Initialize configuration
confluence init
```

Follow the prompts to configure your Confluence instance URL and credentials.

### Install the Emacs package

#### Manual Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/robertmeta/confluence-wrap.git
   ```

2. Add to your Emacs configuration:
   ```elisp
   (use-package confluence
     :ensure nil
     :load-path "~/path/to/confluence-wrap"
     :commands (confluence-search)
     :init
     (setq confluence-search-limit 25)
     :bind (("s-C" . confluence-search)))
   ```

## Usage

### Commands

- `M-x confluence-search` - Search for Confluence pages (or `s-C` if bound)
- `M-x confluence-view-page` - View a page by ID
- `M-x confluence-list-spaces` - List all Confluence spaces

### Keybindings in Search Results (confluence-mode)

| Key   | Command                          |
|-------|----------------------------------|
| `RET` | View page details                |
| `s`   | New search                       |
| `i`   | Show page info                   |
| `o`   | Open page in browser             |
| `S`   | List all spaces                  |
| `g`   | Refresh search results           |
| `n/p` | Next/previous line               |
| `q`   | Quit window                      |

### Keybindings in Page View (confluence-view-mode)

| Key | Command              |
|-----|----------------------|
| `q` | Quit window          |
| `i` | Show page info       |
| `o` | Open in browser      |
| `s` | New search           |

### Display Format

Search results are displayed with:

```
Confluence Search: kubernetes
Found 25 results

Commands: [RET] view  [i] info  [o] open in browser  [s] new search  [S] spaces  [q] quit

Front End NonPaper Unit Test Results (ID: 797049287)
  Job: dental/dental-adjudication/main-containers-build-and-push/main, Build: 5859 RUN v3.2.4...

SNOW-->DST Portal Test Plan (ID: 1035731370)
  Overview The purpose of the test plan is to validate CMS' Enterprise ServiceNow...
```

Page view shows:
- Page metadata (title, ID, type, status, space)
- Full page content in readable markdown/text format

## Emacspeak Support

This package includes comprehensive Emacspeak integration:

### Custom Line Reading

When navigating search results with Emacspeak:
- **Visual**: Multi-line entry with title, ID, and preview
- **Spoken**: "Front End NonPaper Unit Test Results, page 797049287, Job: dental/dental-adjudication..."

The spoken format:
- Starts with page title (most important)
- Includes page ID for reference
- Provides preview excerpt (up to 100 chars)
- Cleans up highlight markers (`@@@hl@@@`)
- Skips visual formatting

### Auditory Icons

- **Search results**: "open-object" sound
- **View page**: "open-object" sound

### Voice Personalities

Future enhancement: Different personalities for different page types or spaces.

## Configuration

### Customization Variables

```elisp
;; Number of search results to return
(setq confluence-search-limit 25)

;; Path to confluence command (if not in PATH)
(setq confluence-command "/usr/local/bin/confluence")

;; Default space to search (nil = all spaces)
(setq confluence-default-space nil)
```

### Example Configuration

```elisp
(use-package confluence
  :ensure nil
  :load-path "~/projects/robertmeta/confluence-wrap"
  :commands (confluence-search confluence-list-spaces)
  :init
  (setq confluence-search-limit 50)
  (setq confluence-default-space "SEAS")
  :bind (("s-C" . confluence-search)))
```

## Workflow Examples

### Search and view a page

1. `M-x confluence-search` - Enter search query
2. Navigate results with `n`/`p`
3. Press `RET` to view full page content
4. Press `o` to open in browser for editing
5. Press `q` to return to search results

### Quick page lookup

1. `M-x confluence-search` - Search for page title
2. Press `i` on a result to see page info (without opening)
3. Press `o` to jump directly to browser
4. Press `s` to start a new search

### Browse spaces

1. `M-x confluence-list-spaces` - See all available spaces
2. Note the space key you want
3. `M-x confluence-search` - Search (optionally set `confluence-default-space`)

## Development

### Project Structure

```
confluence-wrap/
├── README.md           # This file
├── confluence.el       # Main package file
└── .gitignore         # Git ignore patterns
```

### Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

### Known Limitations

- Page creation not yet implemented (can use `confluence create` via shell)
- Page editing opens external editor (not inline Emacs editing)
- Search limited to text queries (no CQL support yet)
- No attachment management

### Roadmap

- [ ] Pure Emacs forms for page creation
- [ ] Inline editing with markdown-mode
- [ ] Recent pages view
- [ ] Favorite pages
- [ ] Attachment viewing/downloading
- [ ] Page tree navigation
- [ ] Watch/unwatch pages
- [ ] Org-mode export integration

## License

MIT License - see LICENSE file for details.

## Acknowledgments

- Built for the Confluence CLI tool
- Emacspeak integration inspired by T.V. Raman's Emacspeak project
- Created as an accessibility-first interface to Confluence

## See Also

- [confluence-cli](https://www.npmjs.com/package/confluence-cli) - The underlying CLI tool
- [Emacspeak](https://github.com/tvraman/emacspeak) - The complete audio desktop
- [reminders-wrap](https://github.com/robertmeta/reminders-wrap) - Emacs wrapper for macOS Reminders
- [jira-wrap](https://github.com/robertmeta/jira-wrap) - Emacs wrapper for Jira CLI
