### get the environment variables
envrootname='TW_ANALYTICS_SUMMIT_ROOT'
rootdir=Sys.getenv(envrootname)
if (rootdir == "" & !file.exists(rootdir)) {
  stop("You must set a root directory in an .Renviron file")  
}

### set global variables
# force read from source files? set to FALSE for speed
FORCEREAD = FALSE

### get the directories
Rdir=file.path(rootdir,"R")

### done
print("TW Analytics Summit project loaded")
