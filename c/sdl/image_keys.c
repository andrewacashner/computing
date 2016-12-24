/* images.c -- Andrew Cashner, 2016/12/22
 * Try basics of displaying multiple images with SDL2
 */

#include <SDL2/SDL.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define MAX_STRING 80

int main(void)
{
  SDL_Window *window;
  SDL_Renderer *renderer;
  SDL_Surface *surface;
  SDL_Texture *texture;
  SDL_Event event;
  
  char image_filename[] = "img/a.bmp";
  int image_filename_directory_length = 4;

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
	if (event.key.keysym.sym == SDLK_ESCAPE) {
	  quit = true;
	  break;
	} else if (event.key.keysym.sym >= SDLK_a &&
		   event.key.keysym.sym <= SDLK_d) {
	  image_filename[image_filename_directory_length]
	    = (char)(event.key.keysym.sym - SDLK_a + 'a');
	  show_picture = true;
	  break;
	}
      default:
	; /* Do nothing */
      }

      if (quit == true) {
	break;
      }

      SDL_SetRenderDrawColor(renderer, 0x00, 0x00, 0x00, 0x00);
      SDL_RenderClear(renderer);

      if (show_picture == true) {
	surface = SDL_LoadBMP(image_filename);
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
	
      } else {
	SDL_RenderCopy(renderer, NULL, NULL, NULL);
	SDL_RenderPresent(renderer);
      }
    }
  }

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);

  SDL_Quit();

  return(0);
}
