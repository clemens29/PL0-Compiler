// Arnold Beck

/****************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "lex.h"

static FILE  *pIF;			/* Quellfile 				*/
static tMorph Morph,MorphInit;		/* Morphem   				*/
static signed char X;			/* Aktuelles Eingabezeichen 		*/
static int    Z;			/* Aktueller Zustand des Automaten 	*/
static char   vBuf[128+1],*pBuf;	/* Ausgabepuffer */
static int    Ende;			/* Flag 				*/

/*---- Initialisierung der lexiaklischen Analyse ----*/
int initLex(char* fname)
{
  pIF=fopen(fname,"r+t");
  if (pIF) X=fgetc(pIF);
  Morph.MC=mcEmpty;
  return (int)pIF;
}

/* Zeichenklassenvektor */

...

/* Automatentabelle */

...

/* Ausgabefunktionen des Automaten */
static void fl  (void);
static void fb  (void);
static void fgl (void);
static void fsl (void);
static void fslb(void);

typedef void (*FX)(void);

static FX vfx[]={fl,fb,fgl,fsl,fslb};

/*---- Lexikalische Analyse ----*/
tMorph* Lex(void)
{
  int Zn;
  Morph=MorphInit;
  pBuf=vBuf;
  Ende=0;
  do
  {
  /*
    /* Berechnung des Folgezustands */
    Zn=   vSMatrix[Z][vZKl[X&0x7f]]&0xF;
    /* Ausfuehrung der Aktion (Ausgabefunktion */
    vfx[((vSMatrix[Z][vZKl[X&0x7f]]))>>4]();
    /* Automat schaltet */
    Z=Zn;
  */
  }while (Ende==0);
  return &Morph;
}
