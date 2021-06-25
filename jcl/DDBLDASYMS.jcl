*********************************************************************** 00001**2
*                                                                       00002**2
*                                                                       00003**2
*                                                                       00004**2
*                                                                       00005**2
*                                                                       00006**2
*                                                                       00007**2
*                     THIS MODULE IS OBSOLETE                           00008**2
*                                                                       00009**2
*                 IT HAS BEEN REPLACED BY DDBLDSYMS.                    00010**2
*                                                                       00011**2
*                                                                       00012**2
*                                                                       00013**2
*                                                                       00014**2
*                                                                       00015**2
*                                                                       00016**2
*********************************************************************** 00017**2
* BUILD THE SYMNAMES OUTPUT FILE. SEE MEMBER DDASYMSXIT FOR DETAILS.    00018   
*                                                                       00019   
* THIS CODE ASSUMES THAT THE CURRENT SYSADATA ARCHITECTURE LEVEL IS 3.  00020   
* CONFIRM THIS WHEN WE UPGRADE TO A NEW VERSION OF THE ASSEMBLER, AND   00021   
* CHANGE THE CODE IF NECESSARY!                                         00022   
*                                                                       00023   
 SORT FIELDS=(ADSYM_STMT,A)         *** SORT BY STATEMENT NUMBER        00024   
*                                                                       00025   
* INCLUDE ONLY ORDINARY AND EQU SYMBOLS                                 00026   
*                                                                       00027   
 INCLUDE COND=(ADATA_VERSION,EQ,ADATA_VERASM,AND,                       00028   
               ADATA_LEVEL,EQ,ADATA_L3,AND,                             00029   
               ADATA_RECTYPE,EQ,ADATA_RECSYM,AND,                       00030   
               (ADSYM_TYPE,EQ,ADSYM_TYPE_ORDINARY,OR,                   00031   
                ADSYM_TYPE,EQ,ADSYM_TYPE_EQU))                          00032   
*                                                                       00033   
 MODS E35=(DFSYMXT,5000)           *** BUILD THE SYMNAMES STATEMENT     00034   
*                                                                       00035   
 OPTION VLSCMP                                                          00036   
 OUTFIL FNAMES=SYMSOUT,VTOF,OUTREC=(5,80) *** GENERATE SYNMAMES DATASET 00037   
