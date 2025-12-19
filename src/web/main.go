package main

import (
	"context"
	"fmt"
	"html/template"
	"log"
	"net/http"

	"github.com/redis/go-redis/v9"
)

// Global context required by go-redis
var ctx = context.Background()

// PageData holds data to be passed to the HTML template
type PageData struct {
	Title       string
	VisitCount  string
	LastVisitor string
}

func main() {
	// 1. Connect to Redis
	rdb := redis.NewClient(&redis.Options{
		Addr:     "localhost:6379", // Default Redis address
		Password: "",               // No password set
		DB:       0,                // Use default DB
	})

	// Test the connection
	_, err := rdb.Ping(ctx).Result()
	if err != nil {
		log.Fatalf("Could not connect to Redis: %v", err)
	}
	fmt.Println("Connected to Redis successfully")

	// 1. Create a file server for the "static" directory
    fs := http.FileServer(http.Dir("static"))

    // 2. Tell Go to strip "/static/" from the URL before looking for the file
    //    URL: /static/css/styles.css  ->  File System: static/css/styles.css
    http.Handle("/static/", http.StripPrefix("/static/", fs))
	
	// 2. Define Request Handler
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		// Redis Operation: Increment a counter
		// Incr is atomic; it handles concurrency safely
		newCount, err := rdb.Incr(ctx, "visit_count").Result()
		if err != nil {
			http.Error(w, "Redis Error", http.StatusInternalServerError)
			return
		}

		// Redis Operation: Store the user-agent as the "last visitor"
		err = rdb.Set(ctx, "last_visitor", r.UserAgent(), 0).Err()
		if err != nil {
			log.Printf("Error setting key: %v", err)
		}

		// Redis Operation: Retrieve the last visitor
		lastVisitor, err := rdb.Get(ctx, "last_visitor").Result()
		if err != nil {
			lastVisitor = "Unknown"
		}

		// 3. Prepare Data for Template
		data := PageData{
			Title:       "Go + Redis Demo",
			VisitCount:  fmt.Sprintf("%d", newCount),
			LastVisitor: lastVisitor,
		}

		// 4. Parse and Execute Template
		// In a production app, you would parse templates once (globally) at startup, not per request.
		tmpl, err := template.ParseFiles("templates/index.html")
		if err != nil {
			http.Error(w, "Template Error", http.StatusInternalServerError)
			log.Printf("Template parsing error: %v", err)
			return
		}

		err = tmpl.Execute(w, data)
		if err != nil {
			log.Printf("Template execution error: %v", err)
		}
	})

	// 5. Start Server
	fmt.Println("Server starting on http://localhost:8080")
	if err := http.ListenAndServe(":8080", nil); err != nil {
		log.Fatal(err)
	}
}

