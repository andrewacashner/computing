#include <SDL2/SDL.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

int main(void)
{
  SDL_Window *window;
  SDL_Renderer *renderer;
  SDL_Surface *cat_surface, *aardvark_surface;
  SDL_Texture *texture;
  SDL_Event event;
  bool quit = false;
  bool cats = false;
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

  cat_surface = SDL_LoadBMP("img/cat.bmp");
  if (!cat_surface) {
    SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
		 "Couldn't create surface from image: %s", SDL_GetError());
    return(3);
  }
  aardvark_surface = SDL_LoadBMP("img/aardvark.bmp");
  if (!aardvark_surface) {
       SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
		    "Couldn't create surface from image: %s", SDL_GetError());
       
  texture = SDL_CreateTextureFromSurface(renderer, cat_surface);
  if (!texture) {
    SDL_LogError(SDL_LOG_CATEGORY_APPLICATION,
		 "Couldn't create texture from surface: %s", SDL_GetError());
  }
  SDL_FreeSurface(cat_surface);

  while (quit == false) {
    for (i = 0; i < 6; ++i ) {
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
	  case SDLK_c:
	    cats = true;
	    break;
	  default:
	    ; /* Do nothing */
	  }
	default:
	  ; /* Do nothing */
	}
      }
      if (quit == true) {
	break;
      }
      SDL_SetRenderDrawColor(renderer,
			     rgb_rainbow[i][0],
			     rgb_rainbow[i][1],
			     rgb_rainbow[i][2], 255);
      SDL_RenderClear(renderer);
      
      switch (show_picture) {
      case CAT:
	SDL_RenderCopy(renderer, texture, NULL, NULL);
	SDL_RenderPresent(renderer);
	SDL_Delay(1000);
	show_picture = COLOR;
	break;
      case AARDVARK:
	   SDL_BlitSurface(aardvark_surface, /* ... ? */
			   /* START HERE */
      } else {
	SDL_RenderPresent(renderer);
	SDL_Delay(500);
      }

    }
  }

  SDL_DestroyTexture(texture);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);

  SDL_Quit();
      
  return(0);
}
