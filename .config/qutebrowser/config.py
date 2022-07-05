c.fonts.default_size = "20pt"
c.zoom.default = "200%"
c.editor.command = ["alacritty", "-e", "nvim", "{file}"]
config.load_autoconfig()
c.url.searchengines = {
    "DEFAULT": "https://duckduckgo.com/?q={}",
    "we": "https://en.wikipedia.org/w/index.php?title=Special%3ASearch&search={}",
    "wd": "https://de.wikipedia.org/w/index.php?title=Spezial%3ASuche&search={}",
    "maps": "https://www.google.com/maps/?q={}",
}
