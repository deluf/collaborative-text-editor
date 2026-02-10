package main

import (
	"log"
	"net/http"
)

func main() {
	// Serve CSS/JS files
	http.Handle("/static/", http.StripPrefix("/static/", http.FileServer(http.Dir("static"))))

	// Homepage
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/" {
			http.NotFound(w, r)
			return
		}
		http.ServeFile(w, r, "templates/index.html")
	})

	// Note page
	http.HandleFunc("/note", func(w http.ResponseWriter, r *http.Request) {
		http.ServeFile(w, r, "templates/note.html")
	})

	log.Println("Server starting on http://localhost:8080")
	log.Fatal(http.ListenAndServe(":8080", nil))
}

