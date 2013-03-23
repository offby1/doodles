package main

import (
	"fmt"
)

func main() {
	primes := primes()
	for {
		p := <-primes
		fmt.Printf("%v ", p)
		if p > 1000 {
			break
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
