import subprocess as sp
from dataclasses import dataclass, field


FOLDERS = [
    "/home/vimmoos/venvs",
    "/home/vimmoos/downloads",
]
UTILS = [
    "terminator",
    "r",
    "emacs",
    "firefox",
    "zsh",
    # "mupdf",
]
CONFIGS = {
    "i3conf": "/home/vimmoos/.i3/config",
    "xmod": "/home/vimmoos/.Xmodmap",
    "terminator": "/home/vimmoos/.config/terminator/config",
    "zsh": "/home/vimmoos/.zshrc",
    "p10k": "/home/vimmoos/.p10k.zsh",
    "emacs_config": "/home/vimmoos/emacs_config",
    "init.el": "/home/vimmoos/init.el",
}


@dataclass
class Colorcodes:
    """Utility class for coloring the output terminal without any
    additional library"""

    bold: str = field(default_factory=lambda: "")
    reset: str = field(default_factory=lambda: "")
    blue: str = field(default_factory=lambda: "")
    green: str = field(default_factory=lambda: "")
    orange: str = field(default_factory=lambda: "")
    red: str = field(default_factory=lambda: "")

    colors: dict = field(
        default_factory=lambda: {
            "bold": "bold",
            "reset": "sgr0",
            "blue": "setaf 4",
            "green": "setaf 2",
            "orange": "setaf 3",
            "red": "setaf 1",
        }
    )
    functions: dict = field(
        default_factory=lambda: {
            "warning": "orange",
            "info": "blue",
            "complete": "green",
            "error": "red",
        }
    )

    def __post_init__(self):
        try:
            for k, v in self.colors.items():
                self.__setattr__(
                    k, sp.check_output(f"tput {v}".split()).decode("latin1")
                )
        except sp.CalledProcessError as e:
            print(f"ERROR in generating colors using tput\nERROR:{e}")
        for k, v in self.functions.items():
            self.__setattr__(k, self.gen_function(getattr(self, v)))

    def gen_function(self, color):
        def inner(out):
            print(f"{self.bold}{color}{out}{self.reset}")

        return inner


def run(string):
    sp.Popen(string.split()).communicate()


c = Colorcodes()

c.info("===========Starting Post installation===========")
c.info("Creating default folders")
try:
    for v in FOLDERS:
        run(f"mkdir {v}")
except Exception as e:
    c.error(f"ERROR:\n{e}")

c.complete("DONE creating folders")

c.info("START installing utils")
try:
    run(f"sudo pacman -Syyu --noconfirm {' '.join(UTILS)}")
except Exception as e:
    c.error(f"ERROR:\n{e}")
c.complete("DONE installing utils")

c.info("Set up zsh")
try:
    run("sudo chsh -s /bin/zsh vimmoos")
    run(
        'sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"'
    )
    run("yay -S --noconfirm zsh-theme-powerlevel10k-git")
except Exception as e:
    c.error(f"ERROR:\n{e}")

c.complete("DONE with zsh")

c.info("Copying all default configs")
try:
    for k, v in CONFIGS.items():
        run(f"cp -r /home/vimmoos/init_sys/{k} {v}")
except Exception as e:
    c.error(f"ERROR:\n{e}")
c.complete("DONE with configs")
