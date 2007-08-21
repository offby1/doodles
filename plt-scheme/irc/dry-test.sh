#!/bin/sh

(

    echo  ":localhost. 001 rudybot :Welcome to the Debian dancer-ircd Network rudybot \\r"
    echo  ":localhost 366 rudybot #scheme-bots :what's up, homes? \\r"

    echo ":a!b@c PRIVMSG #scheme-bots :hey buddy\\r"
    echo ":a!b@c PRIVMSG #scheme-bots :rudybot: hey buddy\\r"
    echo ":a!b@c PRIVMSG #scheme-bots :rudybot: seen a?\\r"

    sleep 10
    
    echo ":a!b@c PRIVMSG #scheme-bots :rudybot: news\\r"
    echo ":a!b@c PRIVMSG #scheme-bots :rudybot: quote\\r"
    echo ":a!b@c PRIVMSG rudybot :\001VERSION\001\\r"
    echo ":a!b@c PRIVMSG rudybot :what ho, my good man\\r"

    while true
    do
        sleep 120
    done

    ) | ./run-bot.ss -c '#scheme-bots' -n rudybot -q 1
