JsOsaDAS1.001.00bplist00�Vscript_�function writeTextToFile(text, file, overwriteExistingContent) {
  try { 
    // Convert the file to a string
    var fileString = file.toString()
 
    // Open the file for writing
    var openedFile = app.openForAccess(Path(fileString), { writePermission: true })
 
    // Clear the file if content should be overwritten
    if (overwriteExistingContent) {
      app.setEof(openedFile, { to: 0 })
    }
 
    // Write the new content to the file
    app.write(text, { to: openedFile, startingAt: app.getEof(openedFile) })
 
    // Close the file
    app.closeAccess(openedFile)
 
    // Return a boolean indicating that writing was successful
    return(true)
  }
  catch(error) {
    try {
      // Close the file
      app.closeAccess(file)
    }
    catch(error) {
      // Report the error is closing failed
      console.log(`Couldn't close file: ${error}`)
    }
 
    // Return a boolean indicating that writing was successful
    return(false)
  }
}

app = Application.currentApplication()
app.includeStandardAdditions = true

itunes = Application('iTunes')

pl_name = "Pool"
pl = itunes.playlists[pl_name]
tracks = pl.tracks()

track_infos = tracks.map(function(track) {
	track_info = [
		track.databaseID(),
		track.albumArtist().replace(/[,"]/g, ""),
	  track.size(),
	  track.album().replace(/[,"]/g, ""),
		track.rating() / 20,
		track.trackNumber()
	]
	return(track_info)
})

track_infos_s = track_infos.join("\n").toString()
writeTextToFile(track_infos_s, "/Users/devin/Music/big.csv", true)
                              � jscr  ��ޭ