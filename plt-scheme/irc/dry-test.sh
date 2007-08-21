#!/bin/sh

(

    echo  ":localhost. 001 rudybot :Welcome to the Debian dancer-ircd Network rudybot \\r"
    echo  ":localhost 366 rudybot #scheme-bots :what's up, homes? \\r"

    echo ":a!b@c PRIVMSG #scheme-bots :hey buddy"
#     while true
#     do
#         sleep 120
#     done

    ) | ./run-bot.ss -c '#scheme-bots'
