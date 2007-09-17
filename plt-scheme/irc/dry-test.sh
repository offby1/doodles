#!/bin/bash -x

(

    echo -e ":localhost. 001 rudybot :Welcome to the Debian dancer-ircd Network rudybot \\r"
    
    for c in squank wank hippy hoppy flippy floppy silly sally
    do
        echo -e ":localhost 366 rudybot #$c :what's up, homes? \\r"
    done

    sleep 20
    
    echo -e ":a!b@c PRIVMSG #scheme-bots :rudybot: quote\\r"

    while true
    do
        sleep 120
    done

    ) | ./run-bot.ss -c '#scheme-bots' -n rudybot
