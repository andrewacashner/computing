# chimp (tutorial, 2021/05/23)
# vim: set foldmethod=indent :
import os, sys
import pygame as pg
from pygame.compat import geterror

# Constants
## Colors
WHITE = (250, 250, 250)
GRAY  = (10, 10, 10)
RED   = (250, 10, 10)

## File paths
main_dir = os.path.split(os.path.abspath(__file__))[0]
data_dir = os.path.join(main_dir, "data")

## How many times can you punch the chimp?
MAX_HITS = 7

# Load media
def load_image(name, colorkey=None):
    fullname = os.path.join(data_dir, name)

    try:
        image = pg.image.load(fullname)
    except pg.error:
        print("Cannot load image: %s" % name)
        raise SystemExit(str(geterror()))

    image = image.convert()
    if colorkey is not None:
        # colorkey = -1 means use top left pixel of image for colorkey
        if colorkey == -1:
            colorkey = image.get_at((0, 0))
        image.set_colorkey(colorkey, pg.RLEACCEL)

    return image, image.get_rect()

def load_sound(name):
    class NoneSound:
        def play(self): 
            pass

    if not pg.mixer or not pg.mixer.get_init():
        return NoneSound()

    fullname = os.path.join(data_dir, name)

    try:
        sound = pg.mixer.Sound(fullname)
    except pg.error:
        print("Cannot load sound: %s" % fullname)
        raise SystemExit(str(geterror()))

    return sound

# Game Object Classes
class Fist(pg.sprite.Sprite):
    """Clenched fist that moves on the screen, following the mouse"""
    def __init__(self):
        pg.sprite.Sprite.__init__(self) # call Sprite initializer
        self.image, self.rect = load_image("fist.bmp", -1)
        self.punching = 0

    def update(self):
        """move the fist based on the mouse position"""
        pos = pg.mouse.get_pos()
        self.rect.midtop = pos
        if self.punching:
            self.rect.move_ip(5, 10)

    def punch(self, target):
        """returns true if the fist collides with the target"""
        if not self.punching:
            self.punching = 1
            hitbox = self.rect.inflate(-5, 5)
            return hitbox.colliderect(target.rect)

    def unpunch(self):
        """called to pull the fist back"""
        self.punching = 0

class Chimp(pg.sprite.Sprite):
    """Monkey critter that moves across the screen. It spins when it is
    punched, and keeps track of how many times it has been hit."""
    def __init__(self):
        pg.sprite.Sprite.__init__(self) # call Sprite initializer

        self.image, self.rect = load_image("chimp.bmp", -1)

        screen = pg.display.get_surface()
        self.area = screen.get_rect()
        self.rect.topleft = 10, 10
        self.move = 9
        self.dizzy = 0
        self.hits = 0

    def update(self):
        """walk or spin, depending on the monkey's state"""
        if self.dizzy:
            self._spin()
        else:
            self._walk()

    def _walk(self):
        """move the monkey across the screen, and turn at the ends"""
        newpos = self.rect.move((self.move, 0))
        if not self.area.contains(newpos):
            if self.rect.left < self.area.left or \
                    self.rect.right > self.area.right:
                self.move = -self.move
                newpos = self.rect.move((self.move, 0))
                self.image = pg.transform.flip(self.image, 1, 0)
            self.rect = newpos

    def _spin(self):
        """spin the monkey image"""
        center = self.rect.center
        self.dizzy += 12
        if self.dizzy >= 360:
            self.dizzy = 0
            self.image = self.original
        else:
            rotate = pg.transform.rotate
            self.image = rotate(self.original, self.dizzy)
        self.rect = self.image.get_rect(center=center)

    def punched(self):
        """add to hit count; this will cause the monkey to start spinning"""
        self.hits += 1
        if not self.dizzy:
            self.dizzy = 1
            self.original = self.image

class Banner(pg.Surface):
    """Big banner of text centered on the window."""
    def __init__(self, screen, message, color):
        pg.Surface.__init__(self, screen.get_size())
        self.convert()
        self.fill(WHITE)

        if pg.font:
            font = pg.font.Font(None, 36)
            text = font.render(message, 1, color) # 1 means antialias
            center = self.get_width() / 2
            textpos = text.get_rect(centerx = center)
            self.blit(text, textpos)

def make_window(caption, x, y):
    """Basic small window with caption, hide mouse"""
    screen = pg.display.set_mode((x, y))
    pg.display.set_caption(caption)
    pg.mouse.set_visible(0)
    return screen

def check_setup():
    """Check for working fonts and sound. Return relevant error message if
    either doesn't work, or False if all is good"""
    msg = False
    if not pg.font: 
        msg = "Warning, fonts disabled"
    if not pg.mixer: 
        msg = "Warning, sound disabled"
    return msg

def main():
    # Initialize everything
    pg.init()
    
    try:
        setup = check_setup()
    except setup:
        print(setup)
        return

    # Setup the window
    screen      = make_window("Monkey Fever", 468, 60)

    # Create the backgrounds
    background  = Banner(screen, "Pummel the Chimp, and Win $$$", GRAY)
    endscreen   = Banner(screen, "$$$ YOU BEAT THE CHIMP!!! $$$", RED)

    # Prepare game objects
    whiff_sound = load_sound("whiff.wav")
    punch_sound = load_sound("punch.wav")

    chimp       = Chimp()
    fist        = Fist()
    allsprites  = pg.sprite.RenderPlain((fist, chimp))

    clock       = pg.time.Clock()

    # Put background and sprites on screen
    screen.blit(background, (0, 0))
    pg.display.flip()

    # Main loop
    going = True
    while going and chimp.hits < MAX_HITS:
        clock.tick(60) # no faster than 60 fps

        ## Handle all input events
        for event in pg.event.get():
            if event.type == pg.QUIT:
                going = False
            elif event.type == pg.KEYDOWN and event.key == pg.K_ESCAPE:
                going = False
            elif event.type == pg.MOUSEBUTTONDOWN:
                if fist.punch(chimp):
                    punch_sound.play() # punch
                    chimp.punched()
                else:
                    whiff_sound.play() # miss
            elif event.type == pg.MOUSEBUTTONUP:
                fist.unpunch()
                
        ## Update the sprites
        allsprites.update()

        ## Draw the entire scene
        screen.blit(background, (0, 0))
        allsprites.draw(screen)
        pg.display.flip()
    
    # Game over
    ## Celebrate victory if achieved
    if chimp.hits >= MAX_HITS:
        screen.blit(endscreen, (0, 0))
        pg.display.flip()
        pg.time.wait(3000)

    pg.display.quit()
    pg.quit()
    sys.exit()

# this calls the main function when this script is executed
if __name__ == "__main__":
    main()

