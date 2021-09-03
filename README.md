# telegram-vk-bot
This repo contains a Bot service written in Haskell as requested by MetaLamp (https://rizzoma.com/topic/c27faf1cfa188c1120f59af4c35e6099/0_b_9n8n_8jl2l/).


## Intructions to set up and run

`Stack` was used for managing the packages, it is a fantastic tool that just works. You can find instructions on how to install [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

After installing `Stack`, clone this repo and `cd` into the project. In order to build the project, run
```
# download the compiler if needed
stack setup 

# build the project
stack build 
```
Rename (or copy) file `bot.conf.template` to `bot.conf`. You can also create a file `local_bot.conf`, if it is present, the configuration parameters will be determined in accordance with it, otherwise, by `bot.conf`.
Before starting the application, you must specify the bot implementation you will use. To do this, you need to write the name of the messenger in the configuration file - `telegram` or `VK` (default - `telegram`). 
You also need to specify authorization data in the config file (token for telegrams, and token and group_id for VK).

Instructions for creating and registering a bot are here -
https://core.telegram.org/bots#6-botfather and
https://vk.com/dev/manuals (разделы `сообщества` и `ключи доступа`)
That's it! And then when you want to run the service:
```
stack exec telegram-vk-bot
```


You can run the tests with:
```
stack test
```

## Project structure

- `app/Main.hs` - parses the config file and handles and transfers control to the appropriate bot implementation.
- `bot.template.conf` - configuration file template.
- `package.yaml` - the file where we add all the dependencies and project metadata.
- `stack.yaml` - `Stack` configuration.
- `src/` - contains all the code used by this service.
    - `Bot.hs` -  bot logic.
	- `JsonDrop.hs` - functions used during deserialization and serialization.
	- `Dictionary.hs` - dictionary maintenance.
    - `Session`	- description of the type `Session` used in all implementations of the bot
	- `Bot/` - possible bot implementations.
    - `Serveces/` - сonfig and logger services.
      - `Impl/` - сonfig and logger services implementations.
- `test/` - our specs, for testing some internal functions. 





 
