### SETTINGS
config.load_autoconfig(False)
config.set('content.cookies.accept', 'all', 'chrome-devtools://*')
config.set('content.cookies.accept', 'all', 'devtools://*')
config.set('content.headers.accept_language', '', 'https://matchmaker.krunker.io/*')
c.aliases = {'q': 'quit', 'w': 'session-save', 'wq': 'quit --save'}
c.downloads.location.directory = '~/Pobrane'

## User agents
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}) AppleWebKit/{webkit_version} (KHTML, like Gecko) {upstream_browser_key}/{upstream_browser_version} Safari/{webkit_version}', 'https://web.whatsapp.com/')
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}; rv:90.0) Gecko/20100101 Firefox/90.0', 'https://accounts.google.com/*')
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99 Safari/537.36', 'https://*.slack.com/*')

## JavaScript.
config.set('content.javascript.enabled', True, 'chrome-devtools://*')
config.set('content.javascript.enabled', True, 'devtools://*')
config.set('content.javascript.enabled', True, 'chrome://*/*')
config.set('content.javascript.enabled', True, 'qute://*/*')

## Start page
c.url.default_page = 'file:///hdd/git/orgmode-startpage/index.html'
c.url.start_pages = 'file:///hdd/git/orgmode-startpage/index.html'

## Search
c.url.searchengines = {'DEFAULT': 'https://duckduckgo.com/?q={}', 'btw': 'https://wiki.archlinux.org/?search={}', 'gg': 'https://www.google.com/search?q={}', 'rd': 'https://www.reddit.com/r/{}', 'wiki': 'https://en.wikipedia.org/wiki/{}', 'yt': 'https://www.youtube.com/results?search_query={}', 'yi': 'https://yandex.com/images/search?text={}', 'gr': 'https://www.goodreads.com/search?q={}', 'sv': 'https://stardewvalleywiki.com/mediawiki/index.php?title=Special%3ASearch&search=%s{}', 'lb': 'https://letterboxd.com/search/{}', 'ytm': 'https://music.youtube.com/search?q={}',}

### APPEARANCE
config.set('content.images', True, 'chrome-devtools://*')
config.set('content.images', True, 'devtools://*')
c.tabs.show = 'switching'

## Dark mode
# config.set("colors.webpage.darkmode.enabled", True)
# c.colors.webpage.darkmode.enabled
# c.qt.args = ['blink-settings=forceDarkModeEnabled=true,forceDarkModeImagePolicy=2,forceDarkModePagePolicy=1,forceDarkModeInversionAlgorithm=4']

## Theme
import themes.spacegray
config.load_autoconfig()
themes.spacegray.blood(c, {
    'spacing': {
        'vertical': 6,
        'horizontal': 8
    }
})

## Fonts
c.fonts.default_family = '"JetBrainsMono Nerd Font"'
c.fonts.default_size = '10pt'
c.fonts.completion.entry = 'default_size "JetBrainsMono Nerd Font"'
c.fonts.debug_console = 'default_size "JetBrainsMono Nerd Font"'
c.fonts.prompts = 'default_size "Cantarell"'
c.fonts.statusbar = 'default_size "JetBrainsMono Nerd Font"'


### KEYBINDINGS
config.bind('t', 'set-cmd-text -s :open -t')
config.bind('xb', 'config-cycle statusbar.show always never')
config.bind('xt', 'config-cycle tabs.show always never')
config.bind('xx', 'config-cycle statusbar.show always never;; config-cycle tabs.show always never')

## https://github.com/alphapapa/solarized-everything-css
config.bind(',gr', 'config-cycle content.user_stylesheets ~/.config/qutebrowser/solarized-everything-css/css/gruvbox/gruvbox-all-sites.css ""')
