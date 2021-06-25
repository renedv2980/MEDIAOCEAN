*          DATA SET ACFIL1C    AT LEVEL 003 AS OF 02/05/13                      
*&&      SET   NOP=N                                                            
*PHASE T6231CA                                                                  
         TITLE 'UNIT RECORD'                                                    
         SPACE 2                                                                
* YNGX 047 16AUG99 USE LABLES TO REPRESENT FIELD NUMBERS                        
         SPACE 2                                                                
FIL1C    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL1C**,RR=RE                                                 
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         MVC   SVPARMS,0(R1)                                                    
         ST    R1,CALLR1                                                        
*                                                                               
         SRL   RF,24                                                            
         CLM   RF,1,=AL1(ROUTSN)                                                
         BNH   *+6                 UNKNOWN ROUTINE                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
         SPACE 1                                                                
ROUTS    DS    0XL4                                                             
         B     OBJECT              OBJECT INVOKER                               
         B     INIT                INITIALIZATION CALL                          
*                                                                               
ROUTSN   EQU   (*-ROUTS)/4                                                      
         SPACE 1                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         SPACE 1                                                                
EXIT     L     R1,CALLR1           RETURN PARAMETERS TO CALLER                  
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
         SPACE 2                                                                
*                                                                               
FLTXL    MVI   SVPARMS,DFLTL       EXIT LOW FOR FILTER                          
         B     EXITOK                                                           
FLTXE    MVI   SVPARMS,DFLTE       EXIT EQUAL FOR FILTER                        
         B     EXITOK                                                           
FLTXH    MVI   SVPARMS,DFLTH       EXIT HIGH FOR FILTER                         
         B     EXITOK                                                           
FLTXX    MVI   SVPARMS,DFLTX       EXIT NOT WANTED FOR FILTER                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     DS    0H                                                               
         OI    GSINDSL1,GSIXKEY    DON'T ASK 'ENTER KEY'                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* TABLE ITERATION ROUTINE  - EXPECTS RF TO HOLD A(TABLE)              *         
*                          - EXPECTS R1 TO HOLD VERB                  *         
***********************************************************************         
         SPACE 1                                                                
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK           ** NEED TO SET HIGH IF NOT OVERRIDING           
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     ITER                ITERATE THIS TABLE                           
*                                                                               
ITER02   ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
         LA    R1,SVPARMS                                                       
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
***********************************************************************         
* OBJECT CONTROLLER - INTERFACES TO ALL USER OBJECTS                  *         
*                                                                     *         
* P1 HOLDS EQUATED VERB                                               *         
***********************************************************************         
         SPACE 1                                                                
OBJECT   L     R1,SVPARMS                                                       
         LA    RF,TABLEOO          KNOWN OBJECTS                                
         B     ITER                                                             
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* KEY OBJECT                                                          *         
* ----------                                                          *         
* SVPARMS1 HOLDS EQUATED OBJECT                                       *         
* SVPARMS2 HOLDS EQUATED VERB                                         *         
* SVPARMS3 A(KEY)                                                     *         
* SVPARMS4 HOLDS SUB-ACTION                                           *         
***********************************************************************         
         SPACE 1                                                                
KEY      LM    R1,R2,SVPARMS2                                                   
         USING UNTRECD,R2                                                       
         LA    RF,KEYTABL                                                       
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR KEY OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
KEYFRST  L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
*                             *** FIRST TIME FOR KEY OBJECT ***                 
*                                 -------------------------                     
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KFKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
KFKVAL   MVC   UNTKEY,BCSPACES                                                  
         MVC   UNTKCPY,CUABIN                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  MVC   UNTKEY,BCSPACES                                                  
         MVC   UNTKCPY,CUABIN                                                   
         MVI   UNTKUNT,X'41'                                                    
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT                                                         *         
* -----------                                                         *         
* SVPARMS1 = EQUATED OBJECT IDENTIFIER                                *         
* SVPARMS2 = EQUATED DATA IDENTIFIER OR 0 IF GLOBAL DATA ACTION       *         
* SVPARMS3 BYTE 0    = EQUATED DATA VERB IF SVPARMS2 IS ZERO          *         
* SVPARMS3 BYTES 1-3 = EQUATED ACTION VERB                            *         
* SVPARMS4 = A(RECORD AT CORRECT LEVEL)                               *         
* SVPARMS5 = A(FIELD TABLE ENTRY) OR ZERO IF SVPARMS2 IS ZERO OR IF   *         
*                                    ACTION IS DOWNLOAD               *         
*                                                                     *         
* FIELD DATA IS EXTRACTED/OUTPUT INTO FVIFLD                          *         
*                                                                     *         
* THE DATA OBJECT PROVIDES FUNCTIONALITY FOR DEALING WITH ALL THE     *         
* DATA ASSOCIATED WITH A RECORD, WHEREVER IT MAY OCCUR.               *         
***********************************************************************         
         SPACE 1                                                                
DATA     ICM   R1,15,SVPARMS2      R1=DATA IDENTIFIER                           
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS4                                                      
         USING UNTRECD,R2                                                       
         XR    R1,R1                                                            
         IC    R1,SVPARMS3         GET GLOBAL VERB                              
         LA    RF,DTATABL          TABLE OF GLOBAL VERBS                        
         B     ITER                ITERATE TABLE                                
*                                                                               
DATA02   LA    RF,KNOWTAB          TABLE OF KNOWN OBJECTS                       
         USING KNOWTABD,RF                                                      
*                                                                               
DATA04   CLC   KNOWID,=AL2(EOT)    REACH END - NOT A KNOWN DATA TYPE            
         BE    EXITH                                                            
         CLM   R1,3,KNOWID         IS THIS A KNOWN TYPE?                        
         BE    DATA06                                                           
         LA    RF,KNOWLQ(RF)                                                    
         B     DATA04                                                           
*                                                                               
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
*                                                                               
         LM    R1,R2,SVPARMS3      R1 HOLDS VERB                                
         USING UNTRECD,R2          R2 HOLDS A(RECORD)                           
         BR    RF                                                               
         SPACE 1                                                                
*                                                                               
DTATABL  DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN DATA OBJECTS                                         *         
***********************************************************************         
         SPACE 1                                                                
*KNOWTAB  DC    AL2(UNT#CPY),AL4(UCPY)       COMPANY CODE                       
KNOWTAB  DC    AL2(UNT#UNIT),AL4(UTCDTA)     UNIT CODE                          
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL1C    CSECT                                                                  
         SPACE 2                                                                
*&&DO                                                                           
***********************************************************************         
* DATA OBJECT FOR COMPANY CODE                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
UCPY     LA    RF,UCPYTB                                                        
         B     ITER                                                             
*                                                                               
UCPYTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCPY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCPY)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISCPY)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETCPY)                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETCPY  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN UNTKCPY FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
DISCPY   CLI   UNTKCPY,C' '                                                     
         BNH   EXITOK                                                           
         GOTO1 VHEXOUT,BOPARM,UNTKCPY,FVIFLD,L'UNTKCPY,0                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN CPYKCPY FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
VALCPY   CLI   CSACT,A#LST            MAKE LIST ACTION INVALID FOR NOW          
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INACT)                                           
         B     EXITL                                                            
*                                                                               
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         XR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 VHEXIN,BOPARM,FVIFLD,UNTKCPY,(RF)                                
         OC    12(4,R1),12(R1)                                                  
         BNZ   EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$INVCP) INVALID COMPANY                           
         B     EXITL                                                            
         SPACE 2                                                                
***********************************************************************         
* SET DEFAULT FOR COMPANY                                             *         
***********************************************************************         
         SPACE 1                                                                
DDFTCPY  GOTO1 VHEXOUT,BOPARM,CUABIN,FVIFLD,L'CUABIN,0                          
         B     EXITOK                                                           
*&&                                                                             
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR THE UNIT CODE                                       *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
UTCDTA   LA    RF,UTCTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
UTCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISUTC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALUTC)                                 
*        DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTUTC)                               
*        DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTUTC)                               
*        DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTUTC)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A UNIT CODE FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
DISUTC   CLI   UNTKUNT,C' '                                                     
         BNH   EXITOK                                                           
         MVC   FVIFLD,UNTKUNT                                                   
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE A UNIT FIELD                                               *         
***********************************************************************         
         SPACE 1                                                                
VALUTC   MVC   UNTKUNT,FVIFLD                                                   
         B     EXITOK                                                           
         SPACE 1                                                                
*&&DO                                                                           
***********************************************************************         
* DISPLAY AN UNIT FILTER FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
DFLTUTC  CLI   FLTIFLD,C' '                                                     
         BNH   EXITOK                                                           
         MVC   FVIFLD,FLTIFLD                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN UNIT FILTER FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
VFLTUTC  MVC   FLTIFLD(L'UNTKUNT),FVIFLD                                        
         MVC   UNTKUNT,FLTIFLD                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON UNIT FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
DOFTUTC  CLC   UNTKUNT,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
*&&                                                                             
***********************************************************************         
* LIST OBJECT                                                         *         
* -----------                                                         *         
* SVPARMS1 = EQUATED OBJECT                                           *         
* SVPARMS2 = EQUATED VERB                                             *         
* SVPARMS3 = CURRENT KEY BUILD AREA                                   *         
* SVPARMS4 = PREVIOUS KEY                                             *         
***********************************************************************         
         DROP  R2                                                               
         SPACE 1                                                                
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING UNTRECD,R2                                                       
LAST     USING UNTRECD,R3                                                       
         LA    RF,LISTABL                                                       
         B     ITER                                                             
*                                                                               
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST PAGE                                            *         
***********************************************************************         
         SPACE 1                                                                
FLST     DS    0H                                                               
         MVC   IOKEY(L'UNTKEY),THIS.UNTRECD                                     
         LHI   R1,XOHIGH+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   EXITL               READ HIGH UNHAPPY                            
         B     NLST02                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
X        USING UNTRECD,IOKEY                                                    
NLST     XR    RF,RF               CONTROLLER REESTABLISHES SEQUENCE            
         IC    RF,X.UNTKUNT                                                     
         LA    RF,1(RF)                                                         
         STC   RF,X.UNTKUNT                                                     
         L     R1,=AL4(XOHID+XOACCDIR+XIO1)                                     
         GOTO1 AIO                                                              
         BNE   EXITL               END OF FILE                                  
*                                                                               
NLST02   CLC   X.UNTKCPY,THIS.UNTKCPY        SAME COMPANY?                      
         BNE   EXITL                         NO                                 
         CLI   X.UNTKUNT,C' '                                                   
         BNH   NLST                                                             
*                                                                               
         MVC   THIS.UNTKEY(ACCKLEN),IOKEY   WE WANT THIS KEY                    
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
         SPACE 2                                                                
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
* ACFILWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
         PRINT ON                                                               
         SPACE 2                                                                
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
OVERWRKD DSECT                                                                  
CALLR1   DS    A                                                                
SVPARMS  DS    0XL24                                                            
SVPARMS1 DS    A                                                                
SVPARMS2 DS    A                                                                
SVPARMS3 DS    A                                                                
SVPARMS4 DS    A                                                                
SVPARMS5 DS    A                                                                
SVPARMS6 DS    A                                                                
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACFIL1C   02/05/13'                                      
         END                                                                    
