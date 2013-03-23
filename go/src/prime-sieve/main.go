package main

import (
	"fmt"
)

func main() {
	fmt.Printf("Ahoy y'all\n")
	int_channel := integers()
	fmt.Printf("Here's an int: %v\n", <-int_channel)
	fmt.Printf("Here's another: %v\n", <-int_channel)
}

func integers() <-chan int {
	ch := make(chan int)
	go func() {
		i := 2
		for {
			ch <- i
			i += 1
		}
	}()
	return ch
}
