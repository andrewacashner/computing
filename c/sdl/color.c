#include <SDL2/SDL.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

int main(void)
{
  SDL_Window *window;
  SDL_Renderer *renderer;
  SDL_Event event;
  bool quit = false;
  int i;
  int rgb_rainbow[6][3] = { {255, 0, 0},
			    {255, 127, 0},
			    {255, 255, 0},
			    {0, 255, 0},
			    {0, 0, 255},
			    {75, 0, 130} };
      
  if (SDL_Init(SDL_INIT_VIDEO) < 0) {
    SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
		 "Couldn't initialize SDL: %s", SDL_GetError());
    return(3);
  }
  if (SDL_CreateWindowAndRenderer(640,640,
				  SDL_WINDOW_RESIZABLE, &window, &renderer)) {
    SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
		 "Couldn't create window and renderer: %s", SDL_GetError());
    return(3);
  }

  while (quit == false) {
    for (i = 0; i < 6; ++i ) {
      SDL_PollEvent(&event);
      if (event.type == SDL_QUIT) {
	quit = true;
	break;
      }
      SDL_SetRenderDrawColor(renderer,
			     rgb_rainbow[i][0],
			     rgb_rainbow[i][1],
			     rgb_rainbow[i][2], 255);
      SDL_RenderClear(renderer);
      SDL_RenderPresent(renderer);
      SDL_Delay(500);
    }
  }

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);

  SDL_Quit();
      
  return(0);
}
