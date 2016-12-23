/* images.c -- Andrew Cashner, 2016/12/22
 * Try basics of displaying multiple images with SDL2
 */

#include <SDL2/SDL.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

int main(void)
{
  SDL_Window *window;
  SDL_Renderer *renderer;
  SDL_Surface *surface;
  SDL_Texture *texture;
  char *img[] = { 
    "img/aardvark.bmp",
    "img/bat.bmp",
    "img/cat.bmp"
  };
  char *picture_file;
  SDL_Event event;
  bool quit = false;
  bool show_picture = false;

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

  while (quit == false) {
    while (SDL_PollEvent(&event)) {
      switch (event.type) {
      case SDL_QUIT:
	quit = true;
	break;
      case SDL_WINDOWEVENT:
	switch (event.window.event) {
	case SDL_WINDOWEVENT_CLOSE:
	  quit = true;
	  break;
	default:
	  ; /* Do nothing */
	}
      case SDL_KEYDOWN:
	switch (event.key.keysym.sym) {
	case SDLK_ESCAPE:
	  quit = true;
	  break;
	case SDLK_a:
	  picture_file = img['a' - 'a'];
	  show_picture = true;
	  break;
	case SDLK_b:
	  picture_file = img['b' - 'a'];
	  show_picture = true;
	  break;
	case SDLK_c:
	  picture_file = img['c' - 'a'];
	  show_picture = true;
	  break;
	default:
	  ; /* Do nothing */
	}
      }
      if (quit == true) {
	break;
      }

      SDL_SetRenderDrawColor(renderer, 0x00, 0x00, 0x00, 0x00);
      SDL_RenderClear(renderer);

      if (show_picture == true) {
	surface = SDL_LoadBMP(picture_file);
	if (!surface) {
	  SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
		       "Couldn't create surface from image: %s", SDL_GetError());
	  return(3);
	}
    
	texture = SDL_CreateTextureFromSurface(renderer, surface);
	if (!texture) {
	  SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
		       "Couldn't create texture from surface: %s", SDL_GetError());
	  return(3);
	}
	SDL_FreeSurface(surface);
	
	SDL_RenderCopy(renderer, texture, NULL, NULL);
	SDL_RenderPresent(renderer);
	SDL_Delay(1000);
	SDL_DestroyTexture(texture);
	show_picture = false;
      } 
    }
  }

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);

  SDL_Quit();

  return(0);
}
