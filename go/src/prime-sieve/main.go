package main

import (
	"fmt"
)

func main() {
	primes := primes()
	computed := 0
	printed := 0
	for printed < 5 {
		p := <-primes
		computed++

		if computed%1000 == 0 {
			fmt.Printf("%v ", p)
			printed++
		}
	}
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

func primes() <-chan int {
	ch := make(chan int)

	go func() {
		the_sieve := integers()
		for {
			a_prime := <-the_sieve
			ch <- a_prime
			the_sieve = exclude_factors(the_sieve, a_prime)
		}
	}()

	return ch
}
