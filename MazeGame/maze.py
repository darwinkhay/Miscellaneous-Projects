# dkk8es

'''
A Maze Game:
- User Input: arrow keys
- Graphics/Images: the player and enemies will have images
- Start Screen
- Small Enough Window

Optional Features:
- Enemies: there will be enemy characters that move around in the maze and will kill you
- Collectibles: there will be coins to pick up that spawn at random locations in each maze that give you points
- Timer: each time you spawn in a maze, there is a timer that starts from 30 seconds and counts down, and the player
    has to reach the end point before the timer ends, otherwise they lose.
- Multiple levels: there will be multiple levels to this maze game.




'''
import pygame
import gamebox
import random
camera = gamebox.Camera(800,600)

game_on = False
starting_menu_text =[
    gamebox.from_text(camera.left + 400, 50, 'RAZE.', 50, 'dark blue', bold = True, italic= True),
    gamebox.from_text(camera.left + 400, 100, 'This game is a series of stages that are sort of like mazes,', 25, 'black'),
    gamebox.from_text(camera.left + 400, 150, 'so there are multiple levels.', 25, 'black'),
    gamebox.from_text(camera.left + 400, 200, 'You must reach each END POINT of each maze to progress to the next one.', 25, 'black'),
    gamebox.from_text(camera.left + 400, 250, 'AVOID ENEMIES; otherwise, you lose points.', 25, 'black'),
    gamebox.from_text(camera.left + 400, 300, 'Collect coins to gain points. There will be a 2 minute countdown the first time you spawn.', 25, 'black'),
    gamebox.from_text(camera.left + 400, 350, 'You have 2 minutes to finish ALL mazes to win; if you die at 0 POINTS, you LOSE.', 25, 'black'),
    gamebox.from_text(camera.left + 400, 550, 'Darwin Khay dkk8es', 25, 'black'),
    gamebox.from_text(camera.left + 400, 400, 'Press SPACE to go to the game', 25, 'red'),
    gamebox.from_text(camera.left + 400, 450, 'Use the ARROW keys to move. AVOID HITTING THE BLACK WALLS: ', 25, 'red'),
    gamebox.from_text(camera.left + 400, 500, 'if you get stuck and must press SPACE to go back to the spawn point', 25, 'red'),
]
time = 120

background = [
    gamebox.from_color(400,400, 'lightskyblue', 800, 600),
    gamebox.from_color(400,400, '#ffb347', 800, 600),
    gamebox.from_color(400,400, 'royalblue', 800, 800),
    gamebox.from_color(400, 400, '#ffccff', 800, 800),
    gamebox.from_color(400, 400, '#101046', 800, 800)

]
background[0].scale_by(5)
background[1].scale_by(1.3)
background[2].scale_by(1.2)
background[3].scale_by(5)
level_endpoints =[
    gamebox.from_color(800,75, 'red', 60, 50), # level 1 endpoint
    gamebox.from_color(800,408, 'red', 20, 67), # level 2 endpoint
    gamebox.from_color(800, 75, 'red', 40,50), # level 3 endpoint
    gamebox.from_color(800, 300, 'red', 40, 75), # level 4 endpoint
    gamebox.from_color(800, 350, 'red', 20, 100) # level 5 endpoint
]
level_1_walls =[
    gamebox.from_color(0,0, 'black', 400, 1200),
    gamebox.from_color(200,0, 'black',1200,100),
    gamebox.from_color(800,550, 'black', 700, 900)
]



level_2_walls =[
    gamebox.from_color(0,0, 'black', 400, 750),
    gamebox.from_color(800,0, 'black', 400, 750),
    gamebox.from_color(400, 0, 'black', 400, 400),
    gamebox.from_color(400,300,'black', 250, 50),
    gamebox.from_color(400,500,'black', 350, 50),
    gamebox.from_color(400, 600, 'black', 800, 75),
    gamebox.from_color(0, 600, 'black', 300, 320),
    gamebox.from_color(800, 600, 'black', 300, 320)
]

level_3_walls =[
    gamebox.from_color(0,600, 'black', 400, 1000),
    gamebox.from_color(800, 600, 'black', 400, 1000),
    gamebox.from_color(0,0,'black', 700, 100),
    gamebox.from_color(800,0,'black',700, 100),
    gamebox.from_color(200, 600, 'black', 75, 900),
    gamebox.from_color(600, 600, 'black', 75, 900),
    gamebox.from_color(250, 600, 'black', 100, 500),
    gamebox.from_color(550, 600, 'black', 100, 500),
    gamebox.from_color(330,600,'black',70,300),
    gamebox.from_color(470, 600, 'black',70, 300),
    gamebox.from_color(400,600,'black',70,200),
    gamebox.from_color(320,80,'black',70,20),
    gamebox.from_color(485,80,'black',70,20),
    gamebox.from_color(335,170,'black',30,175),
    gamebox.from_color(465, 170,'black',30,175),
    gamebox.from_color(360, 130, 'black',20,320),
    gamebox.from_color(440, 130, 'black', 20, 320),
    gamebox.from_color(320,25,'black',70,100),
    gamebox.from_color(485, 25,'black',70,100)


]
level_4_walls = [
    gamebox.from_color(50,0,'black', 300, 525),
    gamebox.from_color(0, 600, 'black', 200, 525),
    gamebox.from_color(200, 0, 'black', 50, 775),
    gamebox.from_color(120,600,'black', 100,300),
    gamebox.from_color(265,600, 'black', 70, 300),
    gamebox.from_color(260, 0, 'black', 130, 525),
    gamebox.from_color(358, 600, 'black', 115,525),
    gamebox.from_color(450,600, 'black', 70,300),
    gamebox.from_color(600,600, 'black', 70, 300),
    gamebox.from_color(525, 600, 'black', 80, 300),
    gamebox.from_color(520, 0, 'black', 50, 775),
    gamebox.from_color(750,600,'black',229 ,525),
    gamebox.from_color(600,0, 'black', 420,525)
]
level_4_blue_walls =[
    gamebox.from_color(200,600, 'turquoise', 50, 400),
    gamebox.from_color(358,0, 'turquoise', 65, 525)
]

level_5_walls = [
    gamebox.from_color(0,0,'black', 400, 600),
    gamebox.from_color(0,600, 'black', 1800, 400),
    gamebox.from_color(800, 0, 'black', 1000, 600),
    gamebox.from_color(400, 0, 'black', 1000, 400)
]
# player sprite SOURCE: https://publicdomainvectors.org/en/free-clipart/Game-character/46006.html
player_sprite = [
    gamebox.from_image(320, 550, 'pixel_character-removebg-preview.png'), # level 1 player location
    gamebox.from_image(0, 420, 'pixel_character-removebg-preview.png'), # level 2 player location
    gamebox.from_image(10, 75, 'pixel_character-removebg-preview.png'), # level 3 player location
    gamebox.from_image(10, 300, 'pixel_character-removebg-preview.png'), # level 4 player location
    gamebox.from_image(20, 350, 'pixel_character-removebg-preview.png') # level 5 player location
]
for each in player_sprite:
    each.scale_by(0.7)

# enemy sprite SOURCE: https://publicdomainvectors.org/en/free-clipart/Cherry-Bomb-enemy-vector-image/22153.html
enemy_sprite1 = [
    gamebox.from_image(320,200, 'enemy_sprite-removebg-preview.png'), # first enemy sprite in level 1
    gamebox.from_image(320,300, 'enemy_sprite-removebg-preview.png'), # second enemy sprite in level 1
    gamebox.from_image(320,400, 'enemy_sprite-removebg-preview.png'), # third enemy sprite in level 1
]
for each in enemy_sprite1:
    each.scale_by(0.07)
for each in enemy_sprite1:
    enemy_sprite1[0].xspeed = 5
    enemy_sprite1[1].xspeed = -8
    enemy_sprite1[2].xspeed = 3
enemy_sprite2 =[
    gamebox.from_image(400,420, 'enemy_sprite-removebg-preview.png'),
    gamebox.from_image(400,225, 'enemy_sprite-removebg-preview.png')
]
for each in enemy_sprite2:
    each.scale_by(0.07)
for each in enemy_sprite2:
    enemy_sprite2[0].xspeed = 4
    enemy_sprite2[1].xspeed = -4
enemy_sprite3 =[
    gamebox.from_image(400, 10, 'enemy_sprite-removebg-preview.png')
]
for each in enemy_sprite3:
    each.scale_by(0.07)
for each in enemy_sprite3:
    each.yspeed = 10
enemy_sprite4 =[
    gamebox.from_image(200,580,'enemy_sprite-removebg-preview.png'),
    gamebox.from_image(358, 20, 'enemy_sprite-removebg-preview.png')
]
for each in enemy_sprite4:
    each.scale_by(0.07)
enemy_sprite4[0].yspeed = -4
enemy_sprite4[1].yspeed = 4

# enemy_sprite5 SOURCE: https://pixabay.com/vectors/ghost-fear-scare-halloween-horror-3748591/
enemy_sprite5 = [
    gamebox.from_image(250, 250, 'ghost-3748591_640.png')
]
for each in enemy_sprite5:
    each.scale_by(0.05)
# coin sprite SOURCE: https://freesvg.org/spinning-coin-vector-clip-art
coins1 =[
    gamebox.from_image(random.randrange(220,420),350, 'shiny-coin3-removebg-preview.png') # first coin in level 1

]
for each in coins1:
    each.scale_by(0.05)
coins2 = [
    gamebox.from_image(random.randrange(300,500), 240, 'shiny-coin3-removebg-preview.png'), # first coin in level 2
    gamebox.from_image(random.randrange(300,500), 430, 'shiny-coin3-removebg-preview.png') # second coin in level 2
]
for each in coins2:
    each.scale_by(0.05)
coins3 =[
    gamebox.from_image(400, 475, 'shiny-coin3-removebg-preview.png') # first coin in level 3
]
for each in coins3:
    each.scale_by(0.05)
coins4 = [
    gamebox.from_image(200,420, 'shiny-coin3-removebg-preview.png'), # first coin in level 4
    gamebox.from_image(358, 200, 'shiny-coin3-removebg-preview.png') # second coin in level 4
]
for each in coins4:
    each.scale_by(0.05)
points = 0
level = 0
def tick(keys):
    '''
    This function provides the mechanics of the maze game.
    :param keys:
    :return:
    '''
    global game_on
    global time
    global level
    global points
    time -= 1/60
    if game_on is False:
        time = 120
        camera.clear('dark grey')
        for each in starting_menu_text:
            camera.draw(each)
    if pygame.K_SPACE in keys:
        game_on = True
        level = 1
    if game_on is True:
        camera.clear('turquoise')
        camera.draw(background[0])
        if level == 1:
            for each in level_1_walls:
                camera.draw(each)
            camera.draw(gamebox.from_text(camera.left + 80, 20, 'Time Remaining: ' + str(int(time)), 20, 'white'))
            camera.draw(gamebox.from_text(camera.left + 80, 30, 'Points: ' + str(int(points)), 20 ,'white'))
            camera.draw(level_endpoints[0])
            camera.draw(player_sprite[0])
            for each in enemy_sprite1:
                camera.draw(each)
                each.move_speed()
            for each in level_1_walls:
                if enemy_sprite1[0].touches(each):
                    enemy_sprite1[0].xspeed = -enemy_sprite1[0].xspeed
                if enemy_sprite1[1].touches(each):
                    enemy_sprite1[1].xspeed = -enemy_sprite1[1].xspeed
                if enemy_sprite1[2].touches(each):
                    enemy_sprite1[2].xspeed = -enemy_sprite1[2].xspeed
            for each in enemy_sprite1:
                if player_sprite[0].touches(each, padding = -14):
                    camera.draw(gamebox.from_text(400, 300, 'You hit an ENEMY, press SPACE to reset', 30, 'white'))
                    gamebox.pause()
                    keys.clear()
                    gamebox.timer_loop(60, restart)
            for each in coins1:
                camera.draw(each)
            if player_sprite[0].touches(coins1[0]):
                points += 10
                coins1[0].x = 8000
                coins1[0].y = 8000
            if pygame.K_UP in keys:
                player_sprite[0].y -= 3
            if pygame.K_DOWN in keys:
                player_sprite[0].y += 3
            if pygame.K_RIGHT in keys:
                player_sprite[0].x += 3
            if pygame.K_LEFT in keys:
                player_sprite[0].x -= 3
            for each in level_1_walls:
                if player_sprite[0].touches(each, padding = -12):
                    camera.draw(gamebox.from_text(400, 300, 'You hit a WALL, press SPACE to reset', 30, 'white'))
                    gamebox. pause()
                    keys.clear()
                    gamebox.timer_loop(60, restart)
            if player_sprite[0].touches(level_endpoints[0]):
                level = 2
                camera.clear('turquoise')
        if level == 2:
            camera.draw(background[1])
            for each in level_2_walls:
                camera.draw(each)
            camera.draw(gamebox.from_text(camera.left + 80, 20, 'Time Remaining: ' + str(int(time)), 20, 'white'))
            camera.draw(gamebox.from_text(camera.left + 80, 30, 'Points: ' + str(int(points)), 20, 'white'))
            camera.draw(level_endpoints[1])
            camera.draw(player_sprite[1])
            for each in enemy_sprite2:
                camera.draw(each)
                each.move_speed()
            for each in level_2_walls:
                if enemy_sprite2[1].touches(each):
                    enemy_sprite2[1].xspeed = -enemy_sprite2[1].xspeed
            if enemy_sprite2[0].x == camera.right:
                enemy_sprite2[0].xspeed = -enemy_sprite2[0].xspeed
            if enemy_sprite2[0].x == camera.left:
                enemy_sprite2[0].xspeed = -enemy_sprite2[0].xspeed
            for each in enemy_sprite2:
                if player_sprite[1].touches(each, padding = -14):
                    camera.draw(gamebox.from_text(400, 300, 'You hit an ENEMY, press SPACE to reset', 30, 'white'))
                    gamebox.pause()
                    keys.clear()
                    gamebox.timer_loop(60, restart)
            for each in coins2:
                camera.draw(each)
                if player_sprite[1].touches(each):
                    points += 10
                    each.x = 8000
                    each.y = 8000
            if pygame.K_UP in keys:
                player_sprite[1].y -= 3
            if pygame.K_DOWN in keys:
                player_sprite[1].y += 3
            if pygame.K_RIGHT in keys:
                player_sprite[1].x += 3
            if pygame.K_LEFT in keys:
                player_sprite[1].x -= 3
            for each in level_2_walls:
                if player_sprite[1].touches(each, padding = -12):
                    camera.draw(gamebox.from_text(400, 300, 'You hit a WALL, press SPACE to reset', 30, 'white'))
                    gamebox.pause()
                    keys.clear()
                    gamebox.timer_loop(60, restart)
            if player_sprite[1].touches(level_endpoints[1]):
                level = 3
                camera.clear('turquoise')
        if level == 3:
            camera.draw(background[2])
            for each in level_3_walls:
                camera.draw(each)
            camera.draw(gamebox.from_text(camera.left + 80, 20, 'Time Remaining: ' + str(int(time)), 20, 'white'))
            camera.draw(gamebox.from_text(camera.left + 80, 30, 'Points: ' + str(int(points)), 20, 'white'))
            camera.draw(level_endpoints[2])
            camera.draw(player_sprite[2])
            camera.draw(gamebox.from_text(400, 560, 'Is it worth getting this coin? :)', 20, 'white'))
            for each in enemy_sprite3:
                camera.draw(each)
                each.move_speed()
            for each in enemy_sprite3:
                if each.y == 0:
                    each.yspeed = -each.yspeed
            for each in level_3_walls:
                if enemy_sprite3[0].touches(each):
                    enemy_sprite3[0].yspeed = -enemy_sprite3[0].yspeed
            for each in enemy_sprite3:
                if player_sprite[2].touches(each, padding = -14):
                    camera.draw(gamebox.from_text(400, 300, 'You hit an ENEMY, press SPACE to reset', 30, 'white'))
                    gamebox.pause()
                    keys.clear()
                    gamebox.timer_loop(60, restart)
            for each in coins3:
                camera.draw(each)
                if player_sprite[2].touches(each):
                    points += 10
                    each.x = 8000
                    each.y = 8000
            if coins3[0].x == 8000:
                camera.draw(gamebox.from_text(400, 580, 'I guess it was worth', 20, 'white'))
            if pygame.K_UP in keys:
                player_sprite[2].y -= 3
            if pygame.K_DOWN in keys:
                player_sprite[2].y += 3
            if pygame.K_RIGHT in keys:
                player_sprite[2].x += 3
            if pygame.K_LEFT in keys:
                player_sprite[2].x -= 3
            for each in level_3_walls:
                if player_sprite[2].touches(each, padding = -12):
                    camera.draw(gamebox.from_text(400, 300, 'You hit a WALL, press SPACE to reset', 30, 'white'))
                    gamebox.pause()
                    keys.clear()
                    gamebox.timer_loop(60, restart)
            if player_sprite[2].touches(level_endpoints[2]):
                level = 4
                camera.clear('turquoise')
        if level == 4:
            for each in level_4_blue_walls:
                camera.draw(each)
            camera.draw(background[3])
            for each in level_4_walls:
                camera.draw(each)
            camera.draw(gamebox.from_text(camera.left + 80, 20, 'Time Remaining: ' + str(int(time)), 20, 'white'))
            camera.draw(gamebox.from_text(camera.left + 80, 30, 'Points: ' + str(int(points)), 20, 'white'))
            camera.draw(level_endpoints[3])
            camera.draw(player_sprite[3])
            camera.draw(gamebox.from_text(600, 580, 'You\'re almost there...', 40 , 'white'))
            for each in enemy_sprite4:
                camera.draw(each)
                each.move_speed()
            for each in enemy_sprite4:
                if each.y == 0 or each.y == 600:
                    each.yspeed = -each.yspeed
            for each in level_4_walls:
                if enemy_sprite4[0].touches(each):
                    enemy_sprite4[0].yspeed = -enemy_sprite4[0].yspeed
                if enemy_sprite4[1].touches(each):
                    enemy_sprite4[1].yspeed = -enemy_sprite4[1].yspeed
            for each in enemy_sprite4:
                if player_sprite[3].touches(each, padding = -14):
                    camera.draw(gamebox.from_text(400, 300, 'You hit an ENEMY, press SPACE to reset', 30, 'white'))
                    gamebox.pause()
                    keys.clear()
                    gamebox.timer_loop(60, restart)
            for each in coins4:
                camera.draw(each)
                if player_sprite[3].touches(each):
                    points += 10
                    each.x = 8000
                    each.y = 8000
            if pygame.K_UP in keys:
                player_sprite[3].y -= 3
            if pygame.K_DOWN in keys:
                player_sprite[3].y += 3
            if pygame.K_RIGHT in keys:
                player_sprite[3].x += 3
            if pygame.K_LEFT in keys:
                player_sprite[3].x -= 3
            for each in level_4_walls:
                if player_sprite[3].touches(each, padding = -12):
                    camera.draw(gamebox.from_text(400, 300, 'You hit a WALL, press SPACE to reset', 30, 'white'))
                    gamebox.pause()
                    keys.clear()
                    gamebox.timer_loop(60, restart)
            if player_sprite[3].touches(level_endpoints[3]):
                level = 5
                camera.clear('turquoise')
        if level == 5:
            camera.draw(background[4])
            for each in level_5_walls:
                camera.draw(each)
            camera.draw(level_endpoints[4])
            camera.draw(gamebox.from_text(camera.left + 80, 20, 'Time Remaining: ' + str(int(time)), 20, 'white'))
            camera.draw(gamebox.from_text(camera.left + 80, 30, 'Points: ' + str(int(points)), 20, 'white'))
            camera.draw(player_sprite[4])
            camera.draw(enemy_sprite5[0])
            if player_sprite[4].x >= 30:
                camera.draw(gamebox.from_text(100, 430, 'What\'s this?..', 30, 'white'))
            if pygame.K_RIGHT in keys:
                player_sprite[4].x += 1
            if player_sprite[4].x > 300 and enemy_sprite5[0].y < 350:
                enemy_sprite5[0].yspeed = 10
                enemy_sprite5[0].move_speed()
            if enemy_sprite5[0].y >= 350:
                enemy_sprite5[0].yspeed = 0
                enemy_sprite5[0].move_speed()
                camera.draw(gamebox.from_text(250, 450, 'huh...?', 30, 'white'))
            if player_sprite[4].x > 465:
                camera.draw(gamebox.from_text(500, 475, 'You should run. DO. NOT. STOP. MOVING.', 30, 'white'))
            if player_sprite[4].x > 550:
                enemy_sprite5[0].x = 325
                camera.draw(gamebox.from_text(300, 150, 'run.', 30, 'white'))
            if player_sprite[4].x > 645:
                enemy_sprite5[0].x = 350
                camera.draw(gamebox.from_text(500, 175, 'RUN.', 40, 'white'))
            if player_sprite[4].x > 740:
                camera.draw(gamebox.from_text(400, 300, 'R U N', 250, 'red'))
                enemy_sprite5[0].xspeed += 12
                enemy_sprite5[0].move_speed()
            if enemy_sprite5[0].touches(player_sprite[4]):
                points += 100
                # pop up image SOURCE: https://pixabay.com/vectors/ghost-fear-scare-halloween-horror-3748591/
                camera.clear('black')
                camera.draw(gamebox.from_image(400,300, 'ghost-3748591_640.png'))
                camera.draw(gamebox.from_text(400, 300, 'You LOSE. Press SPACE', 90, 'red'))
                gamebox.pause()
                gamebox.timer_loop(60, restart)
            elif player_sprite[4].touches(level_endpoints[4]):
                camera.clear('white')
                gamebox.pause()
                camera.draw(gamebox.from_text(400, 200, 'CONGRATS', 90, 'pink', bold = True))
                camera.draw(gamebox.from_text(400, 300, 'You beat the game!!!', 90, 'light blue', bold = True))
        if time <= 0:
            camera.draw(gamebox.from_text(400, 300, 'TIMES UP! Press SPACE', 30, 'white'))
            points = 0
            gamebox.pause()
            keys.clear()
            gamebox.timer_loop(60, restart)
    camera.display()

def restart(keys):
    '''
    This function is executed whenever the player hits a wall or hits an enemy -- it's basically a "reset".
    :param keys:
    :return:
    '''
    global time
    global level
    global points
    if pygame.K_SPACE in keys:
        gamebox.unpause()
        points = points - 10
        if points < 0:
            camera.clear('black')
            camera.draw(gamebox.from_text(camera.left + 400, 200, 'You Lose!', 50, 'white'))
            camera.draw(gamebox.from_text(camera.left + 400, 300, 'Quit and reopen the game to play again', 50, 'white'))
        elif level == 1:
            player_sprite[0].x = 320
            player_sprite[0].y = 550
            gamebox.timer_loop(60, tick)
        elif level == 2:
            player_sprite[1].x = 0
            player_sprite[1].y = 420
            enemy_sprite2[0].x = 400
            enemy_sprite2[0].y = 420
            enemy_sprite2[1].x = 400
            enemy_sprite2[1].y = 225
            enemy_sprite2[0].xspeed = -enemy_sprite2[0].xspeed
            enemy_sprite2[1].xspeed = -enemy_sprite2[1].xspeed
            gamebox.timer_loop(60, tick)
        elif level == 3:
            player_sprite[2].x = 10
            player_sprite[2].y = 75
            gamebox.timer_loop(60, tick)
        elif level == 4:
            player_sprite[3].x = 10
            player_sprite[3].y = 300
            gamebox.timer_loop(60, tick)
        elif level == 5:
            camera.clear('black')
            camera.draw(gamebox.from_text(400, 300, 'You failed to win, quit the game and reopen to try again.', 40, 'red'))
            gamebox.stop_loop()

    camera.display()
gamebox.timer_loop(60, tick)
