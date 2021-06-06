import sys, pygame
pygame.init()

size = width, height = 1080, 720 
speed = [1, 1]
black = 0, 0, 0

screen = pygame.display.set_mode(size)

ball = pygame.image.load("intro_ball.gif")
ballrect = ball.get_rect()

def bounce_horizontal(speed):
    speed[0] = -speed[0]
    return speed

def bounce_vertical(speed):
    speed[1] = -speed[1]
    return speed

while 1:
    ballrect = ballrect.move(speed)
    
    for event in pygame.event.get():
        if event.type == pygame.KEYDOWN:
            if event.key in [pygame.K_ESCAPE, pygame.K_q]:
                pygame.display.quit()
                pygame.quit()
                sys.exit()
            if event.key in [pygame.K_LEFT, pygame.K_RIGHT]:
                speed = bounce_horizontal(speed)
            if event.key in [pygame.K_DOWN, pygame.K_UP]:
                speed = bounce_vertical(speed)
#            if event.key == pygame.K_PLUS:
#                ballrect = pygame.Rect.inflate(10, 10)
#                ballrect = pygame.Rect.update()

    if ballrect.left < 0 or ballrect.right > width:
        speed = bounce_horizontal(speed)
    if ballrect.top < 0 or ballrect.bottom > height:
        speed = bounce_vertical(speed)

    screen.fill(black)
    screen.blit(ball, ballrect)
    pygame.display.flip()



