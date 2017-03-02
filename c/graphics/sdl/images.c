/* images.c -- Andrew Cashner, 2016/12/22
 * Try basics of displaying multiple images with SDL2
 */

#include <SDL2/SDL.h>
#include <stdio.h>
#include <stdlib.h>

int main(void)
{
  SDL_Window *window;
  SDL_Renderer *renderer;
  SDL_Surface *surface[2];
  SDL_Texture *texture[2];
  char *img[] = { "img/cat.bmp",
		  "img/aardvark.bmp"
  };
  int i;
  /*   SDL_Event event; */

  if (SDL_Init(SDL_INIT_VIDEO) < 0) {
    SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
		 "Couldn't initialize SDL: %s", SDL_GetError());
    return(3);
  }
  if (SDL_CreateWindowAndRenderer(640, 580, SDL_WINDOW_RESIZABLE,
				  &window, &renderer)) {
    SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
		 "Couldn't create window and renderer: %s", SDL_GetError());
    return(3);
  }

  for (i = 0; i < 2; ++i) {
  
    surface[i] = SDL_LoadBMP(img[i]);
    if (!surface[i]) {
      SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
		   "Couldn't create surface from image: %s", SDL_GetError());
      return(3);
    }
    
    texture[i] = SDL_CreateTextureFromSurface(renderer, surface[i]);
    if (!texture[i]) {
      SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
		   "Couldn't create texture from surface: %s", SDL_GetError());
      return(3);
    }
    SDL_FreeSurface(surface[i]);

    SDL_SetRenderDrawColor(renderer, 0x00, 0x00, 0x00, 0x00);
    SDL_RenderClear(renderer);
    SDL_RenderCopy(renderer, texture[i], NULL, NULL);
    SDL_RenderPresent(renderer);
    SDL_Delay(1000);

    SDL_DestroyTexture(texture[i]);
    
  }

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);

  SDL_Quit();

  return(0);
}
