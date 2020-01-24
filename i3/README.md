# i3 traveller

plays nice with ```.xinitrc```:

```bash
$HOME/dotfiles/i3/gen_config.py computer:<computer_id> location:$2 && exec i3
```

and then:

```bash
startx <location_id>
```
