package main

import (
	"fmt"
)

func main() {
	fmt.Printf("Ahoy y'all\n")
	int_channel := integers()
	fmt.Printf("Here's an int: %v\n", <-int_channel)
	fmt.Printf("Here's another: %v\n", <-int_channel)

	odds_only := exclude_factors(int_channel, 2)
	fmt.Printf("here's an odd number: %v\n", <-odds_only)
	fmt.Printf("here's another: %v\n", <-odds_only)
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

func exclude_factors(numbers <-chan int, i int) <-chan int {
	ch := make(chan int)
	go func() {
		for {
			candidate := <-numbers
			if (candidate % i) != 0 {
				ch <- candidate
			}
		}
	}()
	return ch
}
