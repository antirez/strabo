/* Strabo.c - Copyright (C) 2008-2009 Salvatore Sanfilippo
 * All Rights Reserved
 * Feedbacks: mail to antirez at gmail dot com */

/* CHANGELOG:
 * 12 Feb 2009: UTM transformation now uses interpolation.
 */

#define STRABOVERSION "1.2 (15 Feb 2009)"
#define STRABOCOPYRIGHT "Copyright (C) 2008-2009 Salvatore Sanfilippo and Ester Cavalcanti"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <ctype.h>

double round(double x);

char *font[] = {
    " ", "",
    "a", "02345679d",
    "b", "015679d",
    "c", "012367",
    "d", "045679d",
    "e", "0123679",
    "f", "01239",
    "g", "0123567d",
    "h", "01459d",
    "i", "bf",
    "j", "acf",
    "k", "019ce",
    "l", "0167",
    "m", "0145ac",
    "n", "0145ae",
    "o", "01234567",
    "p", "012349d",
    "q", "01234567e",
    "r", "012349de",
    "s", "1235679d",
    "t", "23bf",
    "u", "014567",
    "v", "018c",
    "w", "01458e",
    "x", "ac8e",
    "y", "acf",
    "z", "23c876",
    "0", "01234567",
    "1", "45c",
    "2", "0234679d",
    "3", "234567d",
    "4", "1459d",
    "5", "1235679d",
    "6", "01235679d",
    "7", "2345",
    "8", "012345679d",
    "9", "12345679d",
    "?", "1234df",
    ",", "8",
    "'", "1",
    "-", "9d",
    "/", "8c",
    NULL,
};

#define MISSINGPOINT -32768

#ifdef PI
#undef PI
#endif
#define PI 3.14159265358979323846

#define WGS84_A 6378137
#define WGS84_B 6356752.3142

#define TIFF_BYTE 1
#define TIFF_ASCII 2
#define TIFF_SHORT 3
#define TIFF_LONG 4
#define TIFF_RATIONAL 5

#define TIFF_FIELDSDATA_LEN 42

double pixelstep = 0.00083333333333333333;

int curvestep = 100; /* default curves step in meters */
int hmapflat = 1; /* default hmap flattering factor */
int prescaling = 1; /* default hgt prescaling is to don't prescale (1:1) */
int utmconv = 0;
int fixerrors = 0;
int drawscalebar = 0;
int heightgain3d = 5;
double heightgain = 1;
int outputvrml = 0;
int cropx, cropy, cropw, croph = 0;
char *ofile = "output.tif"; /* default output file name */
char *markpat = NULL;   /* Mark cities pattern. NULL = no mark */

struct tiff {
    int width;
    int length;
    int nextptr;
    char *fieldsdata;
    int datalen;
};

struct pixel {
    unsigned char r, g, b;
};

struct raster {
    int width;
    int height;
    struct pixel *p;
};

struct hgt {
    int width;
    int height;
    int norig;
    int eorig;
    short *h;
    double minlat, minlon, xstep, ystep;
};

struct palette {
    struct pixel *p;
    int colors;
};

/* ----------------------------- Prototypes  -------------------------------- */
short getheight(struct hgt *hgt, int x, int y);
void setheight(struct hgt *hgt, int x, int y, short height);
void addheight(float *fhgt, struct hgt *hgt, int x, int y, float height);
struct hgt *mkhgt(int width, int height, int norig, int eorig);
void freehgt(struct hgt *hgt);

/* --------------------------------- Utils  --------------------------------- */

/* Glob-style pattern matching. */
int stringmatchlen(const char *pattern, int patternLen,
        const char *string, int stringLen, int nocase)
{
    while(patternLen) {
        switch(pattern[0]) {
        case '*':
            while (pattern[1] == '*') {
                pattern++;
                patternLen--;
            }
            if (patternLen == 1)
                return 1; /* match */
            while(stringLen) {
                if (stringmatchlen(pattern+1, patternLen-1,
                            string, stringLen, nocase))
                    return 1; /* match */
                string++;
                stringLen--;
            }
            return 0; /* no match */
            break;
        case '?':
            if (stringLen == 0)
                return 0; /* no match */
            string++;
            stringLen--;
            break;
        case '[':
        {
            int not, match;

            pattern++;
            patternLen--;
            not = pattern[0] == '^';
            if (not) {
                pattern++;
                patternLen--;
            }
            match = 0;
            while(1) {
                if (pattern[0] == '\\') {
                    pattern++;
                    patternLen--;
                    if (pattern[0] == string[0])
                        match = 1;
                } else if (pattern[0] == ']') {
                    break;
                } else if (patternLen == 0) {
                    pattern--;
                    patternLen++;
                    break;
                } else if (pattern[1] == '-' && patternLen >= 3) {
                    int start = pattern[0];
                    int end = pattern[2];
                    int c = string[0];
                    if (start > end) {
                        int t = start;
                        start = end;
                        end = t;
                    }
                    if (nocase) {
                        start = tolower(start);
                        end = tolower(end);
                        c = tolower(c);
                    }
                    pattern += 2;
                    patternLen -= 2;
                    if (c >= start && c <= end)
                        match = 1;
                } else {
                    if (!nocase) {
                        if (pattern[0] == string[0])
                            match = 1;
                    } else {
                        if (tolower((int)pattern[0]) == tolower((int)string[0]))
                            match = 1;
                    }
                }
                pattern++;
                patternLen--;
            }
            if (not)
                match = !match;
            if (!match)
                return 0; /* no match */
            string++;
            stringLen--;
            break;
        }
        case '\\':
            if (patternLen >= 2) {
                pattern++;
                patternLen--;
            }
            /* fall through */
        default:
            if (!nocase) {
                if (pattern[0] != string[0])
                    return 0; /* no match */
            } else {
                if (tolower((int)pattern[0]) != tolower((int)string[0]))
                    return 0; /* no match */
            }
            string++;
            stringLen--;
            break;
        }
        pattern++;
        patternLen--;
        if (stringLen == 0) {
            while(*pattern == '*') {
                pattern++;
                patternLen--;
            }
            break;
        }
    }
    if (patternLen == 0 && stringLen == 0)
        return 1;
    return 0;
}

/* Like vi_match_len but more handly if used against nul-term strings. */
int stringmatch(const char *pattern, const char *string, int nocase)
{
    int patternLen = strlen(pattern);
    int stringLen = strlen(string);
    return stringmatchlen(pattern, patternLen, string, stringLen, nocase);
}

/* ---------------------------------- Tiff ---------------------------------- */

struct tiff *mkTiff(int width, int length) {
    struct tiff *t = malloc(sizeof(*t));

    if (!t) return NULL;
    t->width = width;
    t->length = length;
    t->nextptr = 112;
    t->fieldsdata = malloc(TIFF_FIELDSDATA_LEN);
    memset(t->fieldsdata,0,TIFF_FIELDSDATA_LEN);
    if (!t->fieldsdata) {
        free(t);
        return NULL;
    }
    t->datalen = 0;
    return t;
}

void tiffWriteWord(FILE *fp, unsigned short w) {
    fwrite(&w, 1, 2, fp);
}

void tiffWriteLong(FILE *fp, unsigned long l) {
    fwrite(&l, 1, 4, fp);
}

void tiffWriteField(FILE *fp, struct tiff *t, unsigned short tag,
                    unsigned short type, unsigned short count, void *ptr)
{
    int datalen;

    tiffWriteWord(fp,tag);
    tiffWriteWord(fp,type);
    tiffWriteLong(fp,count);
    datalen = count;
    if (type == TIFF_SHORT) datalen *= 2;
    if (type == TIFF_LONG) datalen *= 4;
    if (type == TIFF_RATIONAL) datalen *= 8;
    if (datalen <= 4) {
        fwrite(ptr, 1, datalen, fp);
        while(datalen < 4) {
            fwrite("\0", 1, 1, fp);
            datalen++;
        }
    } else {
        tiffWriteLong(fp, t->nextptr);
        memcpy(t->fieldsdata+t->datalen, ptr, datalen);
        while(datalen % 2) datalen++;
        t->datalen += datalen;
        t->nextptr += datalen;
    }
}

void tiffWriteLongField(FILE *fp, struct tiff *t, unsigned short tag,
                        unsigned long l)
{
    tiffWriteField(fp, t, tag, TIFF_LONG, 1, &l);
}

void tiffWriteWordField(FILE *fp, struct tiff *t, unsigned short tag,
                        unsigned short w)
{
    tiffWriteField(fp, t, tag, TIFF_SHORT, 1, &w);
}

void tiffWriteRationalField(FILE *fp, struct tiff *t, unsigned short tag,
                        unsigned long numerator, unsigned long denominator)
{
    unsigned long x[2];
    x[0] = numerator;
    x[1] = denominator;
    tiffWriteField(fp, t, tag, TIFF_RATIONAL, 1, x);
}

void writeTiff(FILE *fp, struct tiff *t, unsigned char *imgdata) {
    /* TIFF Header */
    fwrite("II", 1, 2, fp);         /* Endianess */
    tiffWriteWord(fp, 42);              /* TIFF signature */
    tiffWriteLong(fp, 8);               /* IFD pointer */

    /* IFD */
    tiffWriteWord(fp, 12);              /* Number of fields */
    tiffWriteWordField(fp, t, 256, t->width);   /* 256 width */
    tiffWriteWordField(fp, t, 257, t->length);  /* 256 length */
    tiffWriteWordField(fp, t, 258, 8); /* 258 Bits Per Sample */
    tiffWriteWordField(fp, t, 259, 1);          /* 259 compression: 1 = nocompr */
    tiffWriteWordField(fp, t, 262, 2);          /* 262 Photometric interpr. 2 */
    tiffWriteWordField(fp, t, 273, 200);        /* 273 Strip Offsets */
    tiffWriteWordField(fp, t, 277, 3);          /* 277 Samples Per Pixel */
    tiffWriteLongField(fp, t, 278, t->length);  /* 278 Rows Per Strip */
    tiffWriteLongField(fp, t, 279, t->length*t->width*3);  /* 279 Strip Bytes */
    tiffWriteRationalField(fp, t, 282, 300, 1);  /* 282 X Res: 300 */
    tiffWriteRationalField(fp, t, 283, 300, 1);  /* 283 Y Res: 300 */
    tiffWriteWordField(fp, t, 296, 2);           /* 296 Res Unit: Inch = 2 */
    tiffWriteLong(fp, 0);                       /* Next IFP: 0 = none */
    fwrite(t->fieldsdata,1,TIFF_FIELDSDATA_LEN,fp); /* Write fields data */
    fwrite(imgdata, 1, t->length*t->width*3, fp);
}

/* ---------------------------------- UTM ----------------------------------- */

float degree2decimal(float d, float m, float s) {
    return d+(m/60)+(s/3600);
}

float degree2radiant(float d) {
    return (d*PI)/180;
}

void decimal2degree(double x, int *d, int *m, int *s) {
    *d = floor(x);
    x -= *d;
    x *= 3600;
    *s = (int)x % 60;
    *m = (int)x / 60;
}

void latlon2utm(double lat, double lon, double *yptr, double *xptr) {
    double a,b,k0,e,e1s,es,n,rho,nu,p,sin1,A1,B1,C1,D1,E1,S,radlat;
    double K1,K2,K3,K4,K5;
    double x, y;
    int zone, zonecm;

    if (lon > 0) {
        zone = ((int)(lon/6))+31;
    } else {
        zone = ((int)((lon+180)/6))+1;
    }
    zonecm = 6*zone-183;
    zonecm = 12;

    radlat = degree2radiant(lat);
    a = WGS84_A;
    b = WGS84_B;
    k0 = 0.9996;
    e = sqrt(1-((b/a)*(b/a)));
    e1s = (e*e)/(1-e*e);
    es = e*e;
    n = (a-b)/(a+b);
    rho = (a*(1-es))/(pow((1-es*(sin(radlat)*sin(radlat))),1.5));
    nu = a/pow((1-es*(sin(radlat)*sin(radlat))),0.5);
    p = lon-zonecm;
    p = (p*3600)/10000;
    sin1 = 0.0000048481368;

    /* Calculate the Meridional Arc */
    A1 = a*(1-n+(5.0/4.0)*((n*n)-(n*n*n))+(81.0/64.0)*((n*n*n*n)-(n*n*n*n*n)));
    B1 = (3.0*a*n/2.0)*(1-n-(7*n*n/8)*(1-n)+55*(n*n*n*n)/64);
    C1 = ((15*a*(n*n))/16.0)*(1-n+(3.0/4.0)*((n*n)-(n*n*n)));
    D1 = ((35*a*(n*n*n))/48.0)*(1-n+(11.0/16.0)*((n*n)-(n*n*n)));
    E1 = (315*a*(n*n*n*n)/51.0)*(1-n);
    S = A1*radlat-B1*sin(2*radlat)+C1*sin(4*radlat)-D1*sin(6*radlat)+E1*sin(8*radlat);

    /* Convert radlatutide and Longitude to UTM */
    K1 = S*k0;
    K2 = (k0*(sin1*sin1)*nu*sin(radlat)*cos(radlat))/2;
    K2 *= 100000000;
    K3 = (k0*(sin1*sin1*sin1*sin1)*nu*sin(radlat)*(cos(radlat)*cos(radlat)*cos(radlat)))/24;
    K3 *= 5-(tan(radlat)*tan(radlat))+9*e1s*(cos(radlat)*cos(radlat))+4*(e1s*e1s)*(cos(radlat)*cos(radlat)*cos(radlat)*cos(radlat));
    K3 *= 10000000000000000LL;
    K4 = k0*sin1*nu*cos(radlat);
    K4 *= 10000;
    K5 = ((k0*(sin1*sin1*sin1)*nu*(cos(radlat)*cos(radlat)*cos(radlat)))/6);
    K5 *= 1-(tan(radlat)*tan(radlat))+e1s*(cos(radlat)*cos(radlat));
    K5 *= 1000000000000LL;

    y = K1+K2*(p*p)+K3*(p*p*p*p);
    x = K4*p+K5*(p*p*p);
    x += 500000;

    *xptr = x;
    *yptr = y;
}

void fixutm(struct hgt *hgt) {
    int x, y;

    for (y = 1; y < hgt->height-1; y++) {
        for (x = 1; x < hgt->width-1; x++) {
            int height = getheight(hgt,x,y);
            if (height == MISSINGPOINT) {
                int a,b,c,d,e,f;
                int fixed = 0;

                /* Complex */
                a = getheight(hgt,x,y-1);
                b = getheight(hgt,x,y+1);
                c = getheight(hgt,x-1,y-1);
                d = getheight(hgt,x+1,y-1);
                e = getheight(hgt,x-1,y+1);
                f = getheight(hgt,x+1,y+1);
                if (a != MISSINGPOINT && b != MISSINGPOINT &&
                    c != MISSINGPOINT && d != MISSINGPOINT &&
                    e != MISSINGPOINT && f != MISSINGPOINT) {
                    setheight(hgt,x,y,(a+b+c+d+e+f)/6);
                    fixed = 1;
                }

                /* Vertical */
                a = getheight(hgt,x,y-1);
                b = getheight(hgt,x,y+1);
                if (!fixed && a != MISSINGPOINT && b != MISSINGPOINT) {
                    setheight(hgt,x,y,(a+b)/2);
                    fixed = 1;
                }

                /* Horizontal */
                a = getheight(hgt,x-1,y);
                b = getheight(hgt,x+1,y);
                if (!fixed && a != MISSINGPOINT && b != MISSINGPOINT) {
                    setheight(hgt,x,y,(a+b)/2);
                    fixed = 1;
                }
            }
        }
    }
}

struct hgt *utm(struct hgt *hgt) {
    double lon, lat, minlon = 0, minlat = 0, maxlon = 0, maxlat = 0;
    double utmx, utmy;
    float x, y;
    int utmxres, utmyres, j;
    double xstep, ystep;
    struct hgt *utm;
    float *value, *weight;

    printf("* Calculating the target HGT canvas size\n");
    lon = hgt->eorig/3600;
    lat = hgt->norig/3600;
    for (y = 0; y < hgt->height; y++) {
        for (x = 0; x < hgt->width; x++) {
            latlon2utm(lat-(y*pixelstep),lon+(x*pixelstep),&utmy,&utmx);
            if (x == 0 && y == 0) {
                minlat = maxlat = utmy;
                minlon = maxlon = utmx;
            } else {
                if (utmy < minlat) minlat = utmy;
                if (utmx < minlon) minlon = utmx;
                if (utmy > maxlat) maxlat = utmy;
                if (utmx > maxlon) maxlon = utmx;
            }
        }
    }
    printf(". width in meters: %f\n",maxlon-minlon);
    printf(". height in meters: %f\n",maxlat-minlat);
    utmxres = hgt->width;
    utmyres = (utmxres * (maxlat-minlat))/(maxlon-minlon);
    printf(". After the UTM transformation the new size will be %dx%d pixels\n", utmxres, utmyres);
    xstep = (double)(maxlon-minlon)/(utmxres);
    ystep = (double)(maxlat-minlat)/(utmyres);
    printf(". In the UTM one pixel in x/y is equivalent to %f/%f meters\n", xstep, ystep);

    utm = mkhgt(utmxres,utmyres,hgt->norig,hgt->eorig);
    value = malloc(sizeof(float)*utmxres*utmyres);
    weight = malloc(sizeof(float)*utmxres*utmyres);
    for (j = 0; j < utmxres*utmyres; j++) {
        value[j] = weight[j] = 0;
    }

    printf("* Doing the actual UTM convertion with interpolation\n");
    for (y = 0; y < hgt->height; y += 0.5) {
        for (x = 0; x < hgt->width; x += 0.5) {
            float h00,h10,h01,h11,height;
            float p00,p10,p01,p11;
            int intx, inty;
            float decx, decy, targetx, targety;

            latlon2utm(lat-(y*pixelstep),lon+(x*pixelstep),&utmy,&utmx);
            utmx -= minlon;
            utmy -= minlat;

            intx = floor(x);
            inty = floor(y);
            decx = x - intx;
            decy = y - inty;

            h00 = getheight(hgt,intx,inty);
            h10 = (intx+1 == hgt->width) ? 0 : getheight(hgt,intx+1,inty);
            h01 = (inty+1 == hgt->height) ? 0 : getheight(hgt,intx,inty+1);
            h11 = (intx+1 == hgt->width || inty+1 == hgt->height) ? 0 : getheight(hgt,intx+1,inty+1);

            height = h00*((1-decx)*(1-decy)) +
                     h10*((decx)*(1-decy)) +
                     h01*((1-decx)*(decy)) +
                     h11*(decx*decy);

            targetx = utmx/xstep;
            targety = (utmyres-1)-(utmy/ystep);
            intx = floor(targetx);
            inty = floor(targety);
            decx = targetx - intx;
            decy = targety - inty;

            p00 = (1-decx)*(1-decy)*100;
            p10 = decx*(1-decy)*100;
            p01 = (1-decx)*decy*100;
            p11 = decx*decy*100;

            addheight(value,utm,intx,inty,(height*p00)/100);
            addheight(weight,utm,intx,inty,p00);
            addheight(value,utm,intx+1,inty,(height*p10)/100);
            addheight(weight,utm,intx+1,inty,p10);
            addheight(value,utm,intx,inty+1,(height*p01)/100);
            addheight(weight,utm,intx,inty+1,p01);
            addheight(value,utm,intx+1,inty+1,(height*p11)/100);
            addheight(weight,utm,intx+1,inty+1,p11);
        }
    }

    for (y = 0; y < utm->height; y++) {
        for (x = 0; x < utm->width; x++) {
            float hw, hval;

            hval = value[utm->width*(int)y+(int)x];
            hw = weight[utm->width*(int)y+(int)x];
            if (hw) setheight(utm,x,y,round((hval*100)/hw));
        }
    }

    freehgt(hgt);
    free(weight);
    free(value);
    utm->minlat = minlat;
    utm->minlon = minlon;
    utm->xstep = xstep;
    utm->ystep = ystep;
    return utm;
}

/* ---------------------------------- HGT ----------------------------------- */

struct hgt *mkhgt(int width, int height, int norig, int eorig) {
    struct hgt *hgt = malloc(sizeof(*hgt));
    int j;

    hgt->width = width;
    hgt->height = height;
    hgt->norig = norig;
    hgt->eorig = eorig;
    hgt->h = malloc(sizeof(short)*width*height);
    for (j = 0; j < width*height; j++)
        hgt->h[j] = MISSINGPOINT;
    return hgt;
}

void freehgt(struct hgt *hgt) {
    free(hgt->h);
    free(hgt);
}

void hgtmapcoord(struct hgt *hgt, int ng, int nf, int ns, int eg, int ef, int es, int *x, int *y) {
    if (!utmconv) {
        int nseconds = (hgt->norig)-(ng*3600+nf*60+ns);
        int eseconds = (eg*3600+ef*60+es)-(hgt->eorig);

        *y = nseconds/(3*prescaling);
        *x = eseconds/(3*prescaling);
    } else {
        double lat, lon, utmy, utmx;

        lat = degree2decimal(ng,nf,ns);
        lon = degree2decimal(eg,ef,es);
        latlon2utm(lat,lon,&utmy,&utmx);
        utmx -= hgt->minlon;
        utmy -= hgt->minlat;
        *x = utmx/hgt->xstep;
        *y = ((hgt->height)-1)-(utmy/hgt->ystep);
    }
}

short getheight(struct hgt *hgt, int x, int y) {
    return hgt->h[hgt->width*y+x];
}

void setheight(struct hgt *hgt, int x, int y, short height) {
    hgt->h[hgt->width*y+x] = height;
}

void addheight(float *fhgt, struct hgt *hgt, int x, int y, float height) {
    if (x < 0 || y < 0 || x >= hgt->width || y >= hgt->height) return;
    fhgt[hgt->width*y+x] += height;
}

short maxheight(struct hgt *hgt) {
    int x, y, max, height;

    max = hgt->h[0];
    for (y = 0; y < hgt->height; y++) {
        for (x = 0; x < hgt->width; x++) {
            height = getheight(hgt,x,y);
            if (height > max) max = height;
        }
    }
    return max;
}

struct hgt *readhgt(char *filename) {
    FILE *fp;
    struct hgt *hgt;
    short h;
    char *p = (char*) &h;
    int x, y, X, Y;
    int width, height, norig = 0, eorig = 0;
    char **filenamev;
    short *squarebuf = malloc(1201*1201*sizeof(short));
    int filenamec = 0;

    filenamev = malloc(sizeof(char*)*4096);
    if (!strchr(filename,'-')) {
        char *p = malloc(1024);
        p[0] = '\0';
        strcat(p,filename);
        strcat(p,"-");
        strcat(p,filename);
        filename = p;
    }
    /* NlaElon.hgt-NlaElon.hgt */
    if (strchr(filename,'-') && strlen(filename) == 23) {
        int lat1, lon1, lat2, lon2, lat, lon;

        lat1 = (filename[2]-'0')+(filename[1]-'0')*10;
        lat2 = (filename[14]-'0')+(filename[13]-'0')*10;
        lon1 = (filename[6]-'0')+(filename[5]-'0')*10+(filename[4]-'0')*100;
        lon2 = (filename[18]-'0')+(filename[17]-'0')*10+(filename[16]-'0')*100;
        width = (lon2-lon1)+1;
        height = (lat1-lat2)+1;
        norig = (lat1+1)*3600;
        eorig = lon1*3600;
        printf("=> Loading an area of %dx%d HGT squares\n", width, height);
        for (lat = lat1; lat >= lat2; lat--) {
            for (lon = lon1; lon <= lon2; lon++) {
                filenamev[filenamec] = malloc(64);
                sprintf(filenamev[filenamec],"%c%02d%c%03d.hgt",
                    filename[0],lat,filename[3],lon);
                // printf("File %s\n",filenamev[filenamec]);
                filenamec++;
            }
        }
    } else {
        fprintf(stderr,"Wrong file name\n");
        exit(1);
    }

    hgt = mkhgt((1200/prescaling)*width+1,
                (1200/prescaling)*height+1,norig,eorig);

    for (Y = 0; Y < height; Y++) {
        for (X = 0; X < width; X++) {
            int xoff, yoff;
            short *square = squarebuf;

            xoff = X*(1200/prescaling);
            yoff = Y*(1200/prescaling);

            printf("* Opening %s (%d)\n", filenamev[Y*width+X], Y*width+X);
            fp = fopen(filenamev[Y*width+X],"rb");
            if (fp == NULL) {
                fprintf(stderr,
                    "Warning! Can't open %s, using a black square instead.\n",
                    filenamev[Y*width+X]);
            }
            /* Read this HGT square */
            for (y = 0; y < 1201; y++) {
                for (x = 0; x < 1201; x++) {
                    if (fp) {
                        fread(p+1,1,1,fp);
                        fread(p,1,1,fp);
                    } else {
                        h = 0;
                    }
                    square[y*1201+x] = h;
                }
            }
            /* Insert it in the right position, possibily prescaling */
            for (y = 0; y < 1201; y+=prescaling) {
                for (x = 0; x < 1201; x+=prescaling) {
                    h = square[y*1201+x];
                    h = (short)((double)h*heightgain);
                    setheight(hgt,(x/prescaling)+xoff,(y/prescaling)+yoff,h);
                }
            }
            if (fp) fclose(fp);
        }
    }
    free(squarebuf);
    pixelstep *= prescaling;
    return hgt;
}

struct hgt *crophgt(struct hgt *hgt, int cx, int cy, int cw, int ch) {
    struct hgt *new = mkhgt(cw,ch,0,0);
    int x, y;

    for (y = 0; y < ch; y++) {
        for (x = 0; x < cw; x++) {
            if (x+cx < hgt->width && y+cy <= hgt->height)
                setheight(new,x,y,getheight(hgt,x+cx,y+cy));
        }
    }
    freehgt(hgt);
    return new;
}

/* --------------------------------- RASTER --------------------------------- */

struct raster *mkraster(int width, int height) {
    struct raster *ra = malloc(sizeof(*ra));

    ra->width = width;
    ra->height = height;
    ra->p = malloc(sizeof(struct pixel)*width*height);
    memset(ra->p,0,sizeof(struct pixel)*width*height);
    return ra;
}

void freeraster(struct raster *ra) {
    free(ra->p);
    free(ra);
}

void setpixel(struct raster *ra, int x, int y, int r, int g, int b) {
    struct pixel *p;

    if (x < 0 || x >= ra->width || y < 0 || y >= ra->height) return;
    p = ra->p+((ra->width*y)+x);
    p->r = r;
    p->g = g;
    p->b = b;
}

void drawmark(struct raster *ra, int x, int y) {
    int j;

    for (j = 0; j < 5; j++) {
        setpixel(ra,x-j,y,255,255,255);
        setpixel(ra,x+j,y,255,255,255);
        setpixel(ra,x,y-j,255,255,255);
        setpixel(ra,x,y+j,255,255,255);
    }
}

struct palette *mkpalette(int colors) {
    struct palette *pal = malloc(sizeof(*pal));

    pal->colors = colors;
    pal->p = malloc(sizeof(struct pixel)*colors);
    memset(pal->p,0,sizeof(struct pixel)*colors);
    return pal;
}

void freepalette(struct palette *pa) {
    free(pa->p);
    free(pa);
}

struct pixel getpixel(struct raster *ra, int x, int y) {
    struct pixel p;

    if (x < 0 || x >= ra->width || y < 0 || y >= ra->height) {
        p.r = p.g = p.b = 0;
        return p;
    }
    p = *(ra->p+((ra->width*y)+x));
    return p;
}

struct pixel morphcolor(int r1, int g1, int b1, int r2, int g2, int b2, int parts, int index) {
    float dr, dg, db;
    struct pixel p;

    dr = ((float)(r2-r1))/parts;
    dg = ((float)(g2-g1))/parts;
    db = ((float)(b2-b1))/parts;
    p.r = r1+(dr*index);
    p.g = g1+(dg*index);
    p.b = b1+(db*index);
    return p;
}

void palettegradient(struct palette *pa, int r1, int g1, int b1, int r2, int g2, int b2, int firstindex, int lastindex) {
    int i, size;

    size = lastindex-firstindex+1;
    for (i = firstindex; i <= lastindex; i++) {
        struct pixel color;

        color = morphcolor(r1,g1,b1,r2,g2,b2,size,i-firstindex);
        pa->p[i] = color;
    }
}

struct palette *mkspectrumpalette(void) {
    struct palette *pa = mkpalette(201);
    palettegradient(pa,255,0,0,255,255,0,0,40);
    palettegradient(pa,255,255,0,0,255,0,40,80);
    palettegradient(pa,0,255,0,0,255,255,80,120);
    palettegradient(pa,0,255,255,0,0,255,120,160);
    palettegradient(pa,0,0,255,255,0,255,160,200);
    return pa;
}

struct palette *mktexturepalette(void) {
    struct palette *pa = mkpalette(201);

    palettegradient(pa,253,255,192,187,111,63,0,30);
    palettegradient(pa,187,111,63,13,7,5,30,120);
    palettegradient(pa,13,7,5,0,0,0,120,200);

/*
    palettegradient(pa,171,221,171,253,255,192,0,10);
    palettegradient(pa,253,255,192,187,111,63,10,30);
    palettegradient(pa,187,111,63,13,7,5,30,120);
    palettegradient(pa,13,7,5,0,0,0,120,200);
*/

/*  antirez palette
    palettegradient(pa,152,183,93,171,202,134,0,3);
    palettegradient(pa,171,202,134,255,246,131,3,6);
    palettegradient(pa,255,246,131,227,196,88,6,30);
    palettegradient(pa,227,196,88,109,25,0,30,100);
    palettegradient(pa,109,25,0,0,0,0,100,200);
*/

/*
    palettegradient(pa,199,218,199,251,245,197,0,2);
    palettegradient(pa,251,245,197,242,227,161,2,6);
    palettegradient(pa,242,227,161,240,196,145,6,12);
    palettegradient(pa,240,196,145,212,155,87,12,20);
    palettegradient(pa,212,155,87,160,99,51,20,26);
    palettegradient(pa,160,99,51,94,48,19,26,36);
    palettegradient(pa,94,48,19,70,39,14,36,46);
    palettegradient(pa,70,39,14,46,25,11,46,60);
    palettegradient(pa,46,25,11,36,19,9,60,120);
    palettegradient(pa,36,19,9,0,0,0,120,200);
    */

    return pa;
}

void drawline(struct raster *ra, int x1, int y1, int x2, int y2, int r, int g, int b) {
    double len, dx, dy;
    double j;

    len = sqrt(abs(x2-x1)*abs(x2-x1)+abs(y2-y1)*abs(y2-y1));
    dx = ((double)(x2-x1))/(len);
    dy = ((double)(y2-y1))/(len);

    for (j = 0; j <= len; j++) {
        int px,py;

        px = round(x1+dx*j);
        py = round(y1+dy*j);
        setpixel(ra,px,py,r,g,b);
    }
}

void drawcharseg(struct raster *ra, int x, int y, char *s, int size, int r, int g, int b) {
    int segments[] = {  0,4,0,2, 0,2,0,0, 0,0,1,0, 1,0,2,0,
                        2,0,2,2, 2,2,2,4, 2,4,1,4, 1,4,0,4,
                        0,4,1,2, 0,2,1,2, 0,0,1,2, 1,0,1,2,
                        2,0,1,2, 2,2,1,2, 2,4,1,2, 1,4,1,2 };
    char *sym = "0123456789abcdef";
    while(*s) {
        char *p;
        int off, x1, x2, y1, y2;

        p = strchr(sym,*s);
        if (!p) continue;
        off = p-sym;
        x1 = segments[off*4]*size;
        y1 = segments[off*4+1]*size;
        x2 = segments[off*4+2]*size;
        y2 = segments[off*4+3]*size;
        drawline(ra,x+x1,y+y1,x+x2,y+y2,r,g,b);
        s++;
    }
}

char *getcharseg(int c) {
    int i = 0;
    while(font[i] != NULL) {
        if (font[i][0] == c) return font[i+1];
        i+= 2;
    }
    return NULL;
}

void drawchar(struct raster *ra, int x, int y, int c, int size, int r, int g, int b) {
    char *seg;

    seg = getcharseg(c);
    if (!seg) seg = getcharseg('?');
    drawcharseg(ra, x, y, seg, size, r, g, b);
}

void drawtext(struct raster *ra, int x, int y, char *s, int size, int r, int g, int b) {
    while(*s) {
        drawchar(ra, x, y, tolower(*s), size, r, g, b);
        s++;
        x += ((size==1?2:1)*size)+2*size;
    }
}

void drawctext(struct raster *ra, int x, int y, char *s, int size, int r, int g, int b) {
    x -= (strlen(s)*(2*size)+strlen(s)*((size==1?2:1)*size))/2;
    drawtext(ra, x, y, s, size, r, g, b);
}

void drawdot(struct raster *ra, int x, int y, int r, int g, int b) {
    setpixel(ra,x,y,r,g,b);
    setpixel(ra,x-1,y,r,g,b);
    setpixel(ra,x+1,y,r,g,b);
    setpixel(ra,x,y-1,r,g,b);
    setpixel(ra,x,y+1,r,g,b);
}

void marklocationdegree(struct raster *ra, struct hgt *hgt, int ng, int nf, int ns, int eg, int ef, int es, char *name, int size) {
    int x, y;

    hgtmapcoord(hgt,ng,nf,ns,eg,ef,es,&x,&y);
    drawdot(ra,x,y,255,0,0);
    drawctext(ra,x,y+5,name,size,100,100,100);
}

void marklocation(struct raster *ra, struct hgt *hgt, double lat, double lon, char *name, int size) {
    int ng, nf, ns, eg, ef, es;

    decimal2degree(lat, &ng, &nf, &ns);
    decimal2degree(lon, &eg, &ef, &es);
    marklocationdegree(ra, hgt, ng, nf, ns, eg, ef, es, name, size);
}

/* The following is just a work in progress... for now */
void markcities(struct raster *ra, struct hgt *hgt, char *pattern, int fontsize) {
    FILE *fp;
    char buf[1024];

    pattern = pattern;
    fp = fopen("cities_it.txt","r");
    if (!fp) perror("Can't open cities_it.txt");
    while(fgets(buf, 1024, fp) != NULL) {
        char *name, *slat, *slon, *p;
        double lat, lon;

        buf[1023] = '\0';
        if (buf[strlen(buf)-1] == '\r') buf[strlen(buf)-1] = '\0';
        if (buf[strlen(buf)-1] == '\n') buf[strlen(buf)-1] = '\0';

        p = strchr(buf,','); if (!p) continue;
        /* it,abadia a isola,Abadia a Isola,16,43.3833333,11.2 */
        /*   ^                                                 */
        name = p+1;
        p = strchr(name, ','); if (!p) continue;
        *p = '\0';
        /* it,abadia a isola0Abadia a Isola,16,43.3833333,11.2 */
        /*                  ^                                  */
        p = strchr(p+1, ','); if (!p) continue;
        /* it,abadia a isola0Abadia a Isola,16,43.3833333,11.2 */
        /*                                 ^                   */
        p = strchr(p+1, ','); if (!p) continue;
        /* it,abadia a isola0Abadia a Isola,16,43.3833333,11.2 */
        /*                                    ^                */
        slat = p+1;
        p = strchr(p+1, ','); if (!p) continue;
        *p = '\0';
        /* it,abadia a isola0Abadia a Isola,16,43.3833333011.2 */
        /*                                               ^      */
        slon = p+1;

        lat = atof(slat);
        lon = atof(slon);
        marklocation(ra,hgt,lat,lon,name,fontsize);
//        printf("Mark %s at %f,%f\n", name, lat, lon);
    }
    fclose(fp);
}

void scalebar(struct raster *ra, struct hgt *hgt) {
    int lens[] = {1000,5000,10000,25000,50000,100000,250000,-1};
    int len = lens[0], j = 0, x, y;
    int plen; /* length in pixels */
    char buf[128];

    while(lens[j] != -1) {
        len = lens[j];
        if (len/hgt->xstep > 80) break;
        j++;
    }
    plen = len/hgt->xstep;
    x = ra->width-(ra->width/30);
    y = ra->height-(ra->height/30);
    drawline(ra,x,y,x-(plen-1),y,100,100,100);
    drawline(ra,x,y-2,x,y+2,100,100,100);
    drawline(ra,x-(plen-1),y-2,x-(plen-1),y+2,100,100,100);
    sprintf(buf, "%d meters", len);
    drawtext(ra,x-(plen-1)+2,y+2,buf,2,100,100,100);
}

struct raster *cropra(struct raster *ra, int cx, int cy, int cw, int ch) {
    struct raster *new = mkraster(cw,ch);
    int x, y;

    for (y = 0; y < ch; y++) {
        for (x = 0; x < cw; x++) {
            struct pixel p;

            p = getpixel(ra,x+cx,y+cy);
            setpixel(new,x,y,p.r,p.g,p.b);
        }
    }
    freeraster(ra);
    return new;
}

/* ----------------------------------- PPM ---------------------------------- */

void emitppm(FILE *fp, struct raster *ra) {
    int y,x;

    fprintf(fp,"P3\n%d %d\n255\n",ra->width,ra->height);
    for (y = 0; y < ra->height; y++) {
        for (x = 0; x < ra->width; x++) {
            struct pixel *p;

            p = ra->p+((ra->width*y)+x);
            fprintf(fp, "%d %d %d ", p->r,p->g,p->b);
        }
        fprintf(fp, "\n");
    }
}

/* -------------------------------- STRABO ---------------------------------- */

short interpolateheight(int h1, int h2, int parts, int index) {
    float dh;

    dh = ((float)(h2-h1))/parts;
    return h1+(dh*index);
}

void automarks(struct hgt *hgt, struct raster *ra) {
    int x,y,n = hgt->norig/3600,e = hgt->eorig/3600;
    int xpixel, ypixel;
    for(y = n; y > n-20; y--) {
        for(x = e; x < e+20; x++) {
            hgtmapcoord(hgt,y,0,0,x,0,0,&xpixel,&ypixel);
            drawmark(ra,xpixel,ypixel);
        }
    }
}

void fasce(struct hgt *hgt, struct raster *ra, int q, int texture) {
    int height, x, y;
    struct palette *spectrum;

    if (texture) {
        spectrum = mktexturepalette();
    } else {
        spectrum = mkspectrumpalette();
    }

    for(y = 0; y < hgt->height; y++) {
        for (x = 0; x < hgt->width; x++) {
            struct pixel color;

            height = getheight(hgt,x,y);
            if (height == MISSINGPOINT) {
                color.r = color.g = color.b = 255;
            } else {
                if (height < 0) height = 0;
                if (height > 5000) height = 5000;
                if (height <= 0 && !texture) {
                    color.r = color.g = color.b = 0;
                } else {
                    color = spectrum->p[(height/(25*q))*q];
                }
            }
            setpixel(ra,x,y,color.r,color.g,color.b);
        }
    }
#if 0
    for(y = 0; y < 199; y++) {
        for (x = 0; x < 199; x++) {
            struct pixel color = spectrum->p[x];
            setpixel(ra,x*2,y,color.r,color.g,color.b);
            setpixel(ra,(x*2)+1,y,color.r,color.g,color.b);
        }
    }
#endif
    freepalette(spectrum);
}

void curve(struct hgt *hgt, struct raster *ra, int meters, int usecolors) {
    int x, y;
    struct palette *spectrum = mkspectrumpalette();

    for (y = 1; y < hgt->height; y++) {
        for (x = 1; x < hgt->width; x++) {
            int this = getheight(hgt,x,y);
            int prev = getheight(hgt,x-1,y);
            int top = getheight(hgt,x,y-1);
            int k;

            for (k = meters; k < 5000; k += meters) {
                struct pixel c;

                if ((k >= prev && k <= this) ||
                (k <= prev && k >= this) ||
                (k <= top && k >= this) ||
                (k >= top && k <= this)) {
                    if (usecolors) {
                        int height = this;
                        if (this < 0) this = 0;
                        if (this > 5000) this = 5000;
                        c = spectrum->p[height/25];
                        setpixel(ra,x,y,c.r,c.g,c.b);
                    } else {
                        setpixel(ra,x,y,255,255,255);
                    }
                    break;
                }
            }
        }
    }
    freepalette(spectrum);
}

void mare(struct hgt *hgt, struct raster *ra, int r, int g, int b) {
    int x, y;

    for (y = 1; y < hgt->height; y++) {
        for (x = 1; x < hgt->width; x++) {
            int this = getheight(hgt,x,y);
            int prev = getheight(hgt,x-1,y);
            int top = getheight(hgt,x,y-1);

            if (this == 0 && prev == 0 && top == 0) {
                if (x != (hgt->width-1) && y != (hgt->height-1)) {
                    int next = getheight(hgt,x+1,y);
                    int bottom = getheight(hgt,x,y+1);
                    if (next != 0 || bottom != 0) continue;
                }
                setpixel(ra,x,y,r,g,b);
            }
        }
    }
}

void info(struct hgt *hgt) {
    int x, y;
    int minheight, maxheight, missing;

    minheight = maxheight = hgt->h[0];
    missing = 0;

    for (y = 0; y < hgt->height; y++) {
        for (x = 0; x < hgt->width; x++) {
            int this = getheight(hgt,x,y);

            if (this == MISSINGPOINT) {
                missing++;
                continue;
            }
            if (this > maxheight) maxheight = this;
            if (this < minheight) minheight = this;
        }
    }
    printf("! Max height: %d\n", maxheight);
    printf("! Min height: %d\n", minheight);
    printf("! Bad points: %d (%.4f%%)\n", missing, ((float)missing/(hgt->width*hgt->height))*100);
}

void costa(struct hgt *hgt, struct raster *ra) {
    int x, y;
    struct raster *m = mkraster(hgt->width,hgt->height);

    mare(hgt,m,255,255,255);
    for (y = 2; y < hgt->height; y++) {
        for (x = 2; x < hgt->width; x++) {
            int this = (getpixel(m,x,y)).r;
            int prev = (getpixel(m,x-1,y)).r;
            int top = (getpixel(m,x,y-1)).r;
            if (this != prev || this != top) {
                setpixel(ra,x,y,255,255,255);
            }
        }
    }
    freeraster(m);
}

void missing(struct hgt *hgt, struct raster *ra) {
    int x, y;

    for (y = 0; y < hgt->height; y++) {
        for (x = 0; x < hgt->width; x++) {
            int height = getheight(hgt,x,y);
            if (height == MISSINGPOINT) {
                setpixel(ra,x,y,255,0,0);
            }
        }
    }
}

void fix(struct hgt *hgt) {
    int x, y;

    for (y = 0; y < hgt->height; y++) {
        for (x = 0; x < hgt->width; x++) {
            int height = getheight(hgt,x,y);
            if (height == MISSINGPOINT) {
                int leftx, rightx, topy, bottomy, h1, h2, lefth, righth, toph, bottomh;

                /* Horizontal */
                leftx = x;
                while(leftx > 0 && getheight(hgt,leftx,y) == MISSINGPOINT)
                    leftx--;
                rightx = x;
                while(rightx < (hgt->width-1) &&
                    getheight(hgt,rightx,y) == MISSINGPOINT)
                    rightx++;
                lefth = getheight(hgt,leftx,y);
                righth = getheight(hgt,rightx,y);
                h1 = MISSINGPOINT;
                if (lefth != MISSINGPOINT && righth != MISSINGPOINT) {
                    h1 = interpolateheight(lefth,righth,rightx-leftx+1,x-leftx);
                }
                /* Vertical */
                topy = y;
                while(topy > 0 && getheight(hgt,x,topy) == MISSINGPOINT)
                    topy--;
                bottomy = y;
                while(bottomy < (hgt->height-1) &&
                    getheight(hgt,x,bottomy) == MISSINGPOINT)
                    bottomy++;
                toph = getheight(hgt,x,topy);
                bottomh = getheight(hgt,x,bottomy);
                h2 = MISSINGPOINT;
                if (toph != MISSINGPOINT && bottomh != MISSINGPOINT) {
                    h2 = interpolateheight(toph,bottomh,bottomy-topy+1,y-topy);
                }
                if (h1 != MISSINGPOINT && h2 != MISSINGPOINT)
                    setheight(hgt,x,y,(h1+h2)/2);
                else if (h1 != MISSINGPOINT)
                    setheight(hgt,x,y,h1);
                else if (h2 != MISSINGPOINT)
                    setheight(hgt,x,y,h2);
            }
        }
    }
}

void dem(struct hgt *hgt, struct raster *ra, int adaptive) {
    int x, y, max;

    max = adaptive ? maxheight(hgt) : 5000;
    if (max == 0) max = 1;
    for (y = 0; y < hgt->height; y++) {
        for (x = 0; x < hgt->width; x++) {
            int height, gray;

            height = getheight(hgt,x,y);
            if (height < 0) height = 0;
            if (height > max) height = max;
            gray = (255*height)/max;
            setpixel(ra,x,y,gray,gray,gray);
        }
    }
}

void genvrml(struct hgt *hgt) {
    int x, y;
    FILE *fp;

    fp = fopen("output.wrl","w");
    fprintf(fp,
"#VRML V2.0 utf8\n"
"Shape {\n"
"    appearance Appearance {\n"
"        material Material {\n"
"            diffuseColor .5 .5 .5\n"
"        }\n"
"    }\n"
"    geometry ElevationGrid {\n"
"        xDimension  %d\n"
"        zDimension  %d\n"
"        xSpacing 0.01\n"
"        zSpacing 0.01\n"
"        solid FALSE\n"
"        creaseAngle 6.28\n"
"        height [\n", hgt->width, hgt->height
    );

    /* Heights */
    for (y = 0; y < hgt->height ; y++) {
        for (x = 0; x < hgt->width; x++) {
            int height;
            float h;

            height = getheight(hgt,x,y);
            if (height == MISSINGPOINT) height = -50;
            h = ((float)height)/(9000*prescaling);
            h *= heightgain3d;
            fprintf(fp, "%f", h);
            if (y != hgt->height-1 && x != hgt->width-1) fprintf(fp,",");
            if (x == hgt->width) fprintf(fp,"\n");
        }
    }

    fprintf(fp,
"        ]\n"
"    }\n"
"}\n"
    );
    fclose(fp);
}

void derivative(struct hgt *hgt, struct raster *ra) {
    int x, y;

    struct hgt *de1 = mkhgt(hgt->width, hgt->height, hgt->norig, hgt->eorig);

    for (y = 1; y < hgt->height; y++) {
        for (x = 1; x < hgt->width; x++) {
            int prev, this, top, delta;

            top = getheight(hgt,x,y-1);
            prev = getheight(hgt,x-1,y);
            this = getheight(hgt,x,y);
            delta = abs(this-prev);
            if (abs(this-top) > delta) delta = abs(this-top);
            if (delta > 100) delta = 100;
            setheight(de1,x,y,delta);
        }
    }
    dem(de1,ra,1);
    freehgt(de1);
}

void convert(char *filename, char **op, int opc) {
    struct hgt *hgt;
    struct raster *ra;
    struct tiff *tiff;
    FILE *fp;
    int j;

    hgt = readhgt(filename);
    if (fixerrors) {
        printf("=> Fixing HGT errors (missing points) using interpolation\n");
        fix(hgt);
    }
    if (utmconv) {
        printf("=> Converting to UTM\n");
        hgt = utm(hgt);
    }
    printf("* Final HGT data is %dx%d pixels\n", hgt->width,hgt->height);
    ra = mkraster(hgt->width,hgt->height);

    for (j = 0; j < opc; j++) {
        if (!strcmp(op[j],"sea")) {
            mare(hgt,ra,255,255,255);
            continue;
        } else if (!strcmp(op[j],"coastline")) {
            printf("=> Generating coastline\n");
            costa(hgt,ra);
            continue;
        } else if (!strcmp(op[j],"dem")) {
            printf("=> Generating DEM\n");
            dem(hgt,ra,0);
            continue;
        } else if (!strcmp(op[j],"adaptivedem")) {
            printf("=> Generating Adaptive DEM\n");
            dem(hgt,ra,1);
            continue;
        } else if (!strcmp(op[j],"curves")) {
            printf("=> Generating curvese\n");
            curve(hgt,ra,curvestep,0);
            costa(hgt,ra);
            continue;
        } else if (!strcmp(op[j],"colorcurves")) {
            printf("=> Generating colorized curvese\n");
            curve(hgt,ra,curvestep,1);
            costa(hgt,ra);
            continue;
        } else if (!strcmp(op[j],"hmap")) {
            printf("=> Generating hmap\n");
            fasce(hgt,ra,hmapflat,0);
            continue;
        } else if (!strcmp(op[j],"texture")) {
            printf("=> Generating texture\n");
            fasce(hgt,ra,hmapflat,1);
            mare(hgt,ra,30,52,172);
            continue;
        } else if (!strcmp(op[j],"derivative")) {
            printf("=> Generating first derivative map\n");
            derivative(hgt,ra);
            continue;
        } else if (!strcmp(op[j],"missing")) {
            printf("=> Generating missing points map\n");
            missing(hgt,ra);
            continue;
        } else if (!strcmp(op[j],"automarks")) {
            printf("=> Generating marks\n");
            automarks(hgt,ra);
            continue;
        } else if (!strcmp(op[j],"vrml")) {
            outputvrml = 1;
            continue;
        } else if (!strcmp(op[j],"info")) {
            info(hgt);
            exit(1);
        }
    }

    if (cropw != 0) {
        hgt = crophgt(hgt,cropx/prescaling,cropy/prescaling,cropw/prescaling,croph/prescaling);
        ra = cropra(ra,cropx/prescaling,cropy/prescaling,cropw/prescaling,croph/prescaling);
    }

    fp = fopen(ofile,"wb");
    if (fp == NULL) {
        perror("Writing output file");
        exit(1);
    }

//    marklocation(ra,hgt,36.686111,15.136111,"portopalo di capopassero",1);
    if (markpat) markcities(ra,hgt,markpat,2);
    if (utmconv && drawscalebar) scalebar(ra,hgt);

    tiff = mkTiff(ra->width, ra->height);
    writeTiff(fp,tiff,(unsigned char*)ra->p);
    fclose(fp);

    if (outputvrml) {
        genvrml(hgt);
        printf("=> Generating VRML\n");
    }

    freeraster(ra);
    free(hgt);
}

int main(int argc, char **argv) {
    char *filename = NULL;
    char *op[64];
    int opc = 0, i, j;
    char *oplist[] = {"sea","coastline","dem","adaptivedem","curves","colorcurves","hmap","texture","derivative","missing","info","automarks","vrml"};

    if (argc < 2) {
        fprintf(stderr, "Usage: strabo <maptype> ... <maptype> [ofile <filename>] [curvestep <meters>] [hmapflat <flattering factor>] file.hgt|<lefttop>.hgt-<rightbottom>.hgt\n"
        "Map types:\n"
        "sea                 create a mask of the map detected as sea\n"
        "coastline           create a mask of the map detected as coast\n"
        "dem                 create a grayscale DEM, max height 5000 meters\n"
        "adaptivedem         create a grayscale DEM, max height autodetected\n"
        "curves              create a level curves image\n"
        "colorcurves         create a colorized level curves image\n"
        "hmap                create a colorized heights map\n"
        "texture             create a texture suitable for a 3D model\n"
        "derivative          create a first derivative map\n"
        "vrml                create a 3D mesh output.wrl\n"
        "info                print information about min/max height\n"
        "\nOptions:\n"
        "curvestep  <meters> set the step for curves, default is 100 meters\n"
        "prescaling <factor> prescale the HGT side length of 'factor'\n"
        "hmapflat   <factor> set the hmap flattering. 2 = 50 m, 4 = 100 m, ...\n"
        "ofile      <file>   set the output file name, default output.tif\n"
        "fix                 apply the error correction algorithm (interpolation)\n"
        "utm                 apply UTM projection\n"
        "automarks           draw marks at .hgt squares vertex\n"
        "mark <pattern>      mark specified comma separated cities in the map\n"
        "scalebar            draw metric bar\n"
        "3dhgain <factor>    3D model height gain factor (default 5)\n"
        "hgain <factor>      HGT heights gain (default 1: no gain)\n"
        "crop <x,y,w,h>      Crop image and 3d model at x,y for WxH pixels\n"
        "version             Print version information and quit\n"
        "\nExample:\n"
        "  To generate a map of Siciliy (Italy) with UTM projection, error corrected, try:\n"
        "    strabo n38e012.hgt-n36e015.hgt utm fix texture prescaling 4\n\n"
        "Strabo is " STRABOCOPYRIGHT "\n"
        );
        exit(1);
    }
    for (i = 1; i < argc; i++) {
        int mapflag = 0;

        if (strstr(argv[i],".hgt") || strstr(argv[i],".HGT")) {
            filename = strdup(argv[i]);
            continue;
        }
        for (j = 0; j < (signed)(sizeof(oplist)/sizeof(char*)); j++) {
            if (!strcmp(argv[i],oplist[j])) {
                op[opc++] = argv[i];
                mapflag = 1;
            }
        }
        if (mapflag) continue;
        if (!strcmp(argv[i],"curvestep")) {
            i++;
            curvestep = atoi(argv[i]);
        } else if (!strcmp(argv[i],"crop")) {
            char *cropstr, *aux;

            i++;
            cropstr = strdup(argv[i]);
            aux = strtok(cropstr,","); cropx = atoi(aux ? aux : "0");
            aux = strtok(NULL,","); cropy = atoi(aux ? aux : "0");
            aux = strtok(NULL,","); cropw = atoi(aux ? aux : "0");
            aux = strtok(NULL,","); croph = atoi(aux ? aux : "0");
        } else if (!strcmp(argv[i],"3dhgain")) {
            i++;
            heightgain3d = atoi(argv[i]);
        } else if (!strcmp(argv[i],"hgain")) {
            i++;
            heightgain = atof(argv[i]);
        } else if (!strcmp(argv[i],"hmapflat")) {
            i++;
            hmapflat = atoi(argv[i]);
        } else if (!strcmp(argv[i],"ofile")) {
            i++;
            ofile = argv[i];
        } else if (!strcmp(argv[i],"mark")) {
            i++;
            markpat = argv[i];
        } else if (!strcmp(argv[i],"scalebar")) {
            drawscalebar = 1;
        } else if (!strcmp(argv[i],"prescaling")) {
            i++;
            prescaling = atoi(argv[i]);
            if (prescaling <= 0 || prescaling > 64 || prescaling % 2) {
                fprintf(stderr, "Wrong prescaling value\n");
                exit(1);
            }
        } else if (!strcmp(argv[i],"utm")) {
            utmconv = 1;
        } else if (!strcmp(argv[i],"fix")) {
            fixerrors = 1;
        } else if (!strcmp(argv[i],"version")) {
            printf("Strabo version " STRABOVERSION "\n"
                    STRABOCOPYRIGHT "\n");
            exit(0);
        } else {
            fprintf(stderr, "Wrong option: '%s'\n", argv[i]);
            exit(1);
        }
    }
    if (filename == NULL) {
        fprintf(stderr, "No hgt file specified!\n");
        exit(1);
    }
    printf("Strabo version " STRABOVERSION "\n"
            STRABOCOPYRIGHT "\n\n");
    convert(filename,op,opc);
    exit(0);
}
