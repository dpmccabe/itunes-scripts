import iTunesLibrary

let library = try? iTunesLibrary.ITLibrary(apiVersion: "1.0")
let playlists = library?.allPlaylists

let date_formatter = DateFormatter()
date_formatter.dateFormat = "yyyy-MM-dd'T'HH:mm:ss'Z'"

var tracks = [[
    "id",
    "no",
    "duration",
    "plays",
    "rating",
    "genre",
    "grouping",
    "last_played",
    ].joined(separator: ";")]

for playlist in playlists! {
    if playlist.name == "★+" {
        for track in playlist.items.enumerated() {
            tracks.append([
                String(NSString(format: "%016llX", track.element.persistentID.intValue)),
                String(track.element.trackNumber),
                String(track.element.totalTime),
                String(track.element.playCount),
                String(track.element.rating),
                track.element.genre,
                track.element.grouping?.replacingOccurrences(of: ";", with: "") ?? "",
                date_formatter.string(from: track.element.lastPlayedDate!),
                ].joined(separator: ";"))
        }
    }
}

do {
    try tracks.joined(separator: "\n").write(
        toFile: "/Users/devin/Music/starred.csv",
        atomically: true,
        encoding: String.Encoding.utf8
    )
} catch {
    print("Unexpected error: \(error).")
}
