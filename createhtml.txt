static_help = function(pkg, links = tools::findHTMLlinks()) {
  wd <- getwd()
  helpdir <- system.file('html', package = "EchoviewR")
  setwd(helpdir)   
  message("Generated help files will be placed in ", helpdir)
  pkgRdDB = tools:::fetchRdDB(file.path(find.package(pkg), 
                                        'help', pkg))
  force(links); topics = names(pkgRdDB)
  for (p in topics) {
    tools::Rd2HTML(pkgRdDB[[p]], 
                   paste(p, 'html', sep = '.'),
                   package = pkg, 
                   Links = links, 
                   no_links = is.null(links))
  }
  setwd(wd) # Get back to the current working directory
}

static_help("EchoviewR")
