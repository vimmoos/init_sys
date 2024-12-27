import subprocess as sp
from dataclasses import dataclass, field
import time

USER = "vimmoos"
HOME = f"/home/{USER}"

FOLDERS = [
    # f"{HOME}/venvs",
    f"{HOME}/poetry",
    f"{HOME}/downloads",
    f"{HOME}/.config/terminator",
    f"{HOME}/.config/systemd/user",
]
RM_FOLDERS = [
    f"{HOME}/Desktop",
    f"{HOME}/Documents",
    f"{HOME}/Downloads",
    f"{HOME}/Music",
    f"{HOME}/Pictures",
    f"{HOME}/Public",
    f"{HOME}/Templates",
    f"{HOME}/Videos",
]
UTILS = [
    "terminator",
    "r",
    "emacs",
    "firefox",
    "zsh",
    "python-pipx",
    "ttf-fira-code",
    # "mupdf",
]
CONFIGS = {
    "i3conf": f"{HOME}/.i3/config",
    "xmod": f"{HOME}/.Xmodmap",
    "terminator": f"{HOME}/.config/terminator/config",
    "zsh": f"{HOME}/.zshrc",
    "p10k": f"{HOME}/.p10k.zsh",
    # "emacs_config": f"{HOME}/emacs_config",
    "init.el": f"{HOME}/init.el",
    "emacs": f"{HOME}/.emacs",
    "emacs.service": f"{HOME}/.config/systemd/user/emacs.service",
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


def run(string, *args):
    sp.Popen(string.split() + list(args)).communicate()


c = Colorcodes()

c.info("===========Starting Post installation===========")
c.info("Creating default folders")
try:
    for v in FOLDERS:
        run(f"mkdir -p {v}")
    for v in RM_FOLDERS:
        run(f"rm -rf {v}")
except Exception as e:
    c.error(f"ERROR:\n{e}")

c.complete("DONE creating folders")

c.info("Update OS")
# try:
#     run("sudo pacman-key --refresh-keys")
# except Exception as e:
#     c.error(f"ERROR:\n{e}")

try:
    run("sudo pacman -Syy archlinux-keyring")
except Exception as e:
    c.error(f"ERROR:\n{e}")

try:
    run("sudo pacman -Syyu")
except Exception as e:
    c.error(f"ERROR:\n{e}")

c.complete("DONE Updated OS ")

time.sleep(1)

c.info("START installing utils")
try:
    run(f"sudo pacman -Syyu --noconfirm {' '.join(UTILS)}")
except Exception as e:
    c.error(f"ERROR:\n{e}")
c.complete("DONE installing utils")

c.info("Set up zsh")
try:
    run("sudo chsh -s /bin/zsh vimmoos")
    cmd = sp.check_output(
        "curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh".split()
    ).decode("latin1")
    run("sh -c", f"'{cmd}'")
    run(
        f"git clone --depth=1 https://github.com/romkatv/powerlevel10k.git {HOME}/.oh-my-zsh/custom/themes/powerlevel10k"
    )
except Exception as e:
    c.error(f"ERROR:\n{e}")

c.complete("DONE with zsh")

c.info("Create hard-link to configs")
try:
    for k, v in CONFIGS.items():
        run(f"ln {HOME}/init_sys/{k} {v}")
except Exception as e:
    c.error(f"ERROR:\n{e}")
c.complete("DONE with configs")

c.info("Setting up emacs daemon")

run("systemctl --user daemon-reload")
run("systemctl --user enable emacs.service")
run("systemctl --user start emacs.service")
c.complete("DONE with emacs daemon")

c.info("Installing poetry")
run("pipx install poetry")
c.complete("DONE with poetry")

c.info("Generate ssh keys")
run("ssh-keygen")
c.complete("DONE ssh keys")


c.info("Run emacs icons")
run("emacs --eval '(all-the-icons-isntall-fonts t)'")
c.complete("DONE Emacs icons")
