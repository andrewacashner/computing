/* images.c -- Andrew Cashner, 2016/12/22
 * Try basics of displaying multiple images with SDL2
 */

#include <SDL2/SDL.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define MAX_STRING 80

/* Lookup tables */
const char *error_msg[] = {
  "Couldn't initialize SDL",
  "Couldn't create window and renderer",
  "Couldn't create surface from image",
  "Couldn't create texture from surface",
  "Unknown error\n"
};
const enum {
  INITIALIZE_ERROR,
  WINDOW_RENDERER_ERROR,
  SURFACE_ERROR,
  TEXTURE_ERROR,
  UNKNOWN_ERROR
} error_types;

/* Function prototypes */
void exit_SDL_error(int error_code);
void display_picture(char *image_filename, SDL_Renderer *renderer,
		     SDL_Surface *surface, SDL_Texture *texture);


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
    exit_SDL_error(INITIALIZE_ERROR);
  }
  if (SDL_CreateWindowAndRenderer(640, 580, SDL_WINDOW_RESIZABLE,
				  &window, &renderer)) {
    exit_SDL_error(WINDOW_RENDERER_ERROR);
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
	display_picture(image_filename, renderer, surface, texture);
	show_picture = false;
      } else {
	display_picture(NULL, renderer, surface, texture);
      }
    }
  }

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);

  SDL_Quit();

  return(0);
}


void exit_SDL_error(int error_code)
{
  if (error_code >= UNKNOWN_ERROR) {
    fprintf(stderr, "%s", error_msg[UNKNOWN_ERROR]);
  } else {
    SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
		 "%s: %s", error_msg[error_code], SDL_GetError());
  }
  exit(EXIT_FAILURE);
}
  
void display_picture(char *image_filename, SDL_Renderer *renderer,
		     SDL_Surface *surface, SDL_Texture *texture)
{
  if (image_filename == NULL) {
        SDL_RenderCopy(renderer, NULL, NULL, NULL);
	SDL_RenderPresent(renderer);
  } else {
    surface = SDL_LoadBMP(image_filename);
    if (!surface) {
      exit_SDL_error(SURFACE_ERROR);
    }
    
    texture = SDL_CreateTextureFromSurface(renderer, surface);
    if (!texture) {
      exit_SDL_error(TEXTURE_ERROR);
    }
    SDL_FreeSurface(surface);
    
    SDL_RenderCopy(renderer, texture, NULL, NULL);
    SDL_RenderPresent(renderer);
    SDL_Delay(1000);
    SDL_DestroyTexture(texture);
  }
  return;
}

  
