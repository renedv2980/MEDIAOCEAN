*          DATA SET CTFIL17    AT LEVEL 010 AS OF 11/16/04                      
*&&      SET   NOP=N                                                            
*PHASE TA1317A                                                                  
         TITLE 'DUMP INFORM EMAIL MAINTENANCE'                                  
FIL17    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,CTFIL17*,R6,RR=RE                                              
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
         B     EXIT                                                             
*                                                                               
EXIT     L     R1,CALLR1           RETURN PARAMETERS                            
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
         SPACE 2                                                                
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL               EXIT WITH FIELD NOT INPUT SET                
EXITNOTN MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL               EXIT WITH FIELD NOT NUMERIC SET              
*                                                                               
FLTXL    MVI   SVPARMS,DFLTL       EXIT LOW FOR FILTER                          
         B     EXITOK                                                           
FLTXE    MVI   SVPARMS,DFLTE       EXIT EQUAL FOR FILTER                        
         B     EXITOK                                                           
FLTXH    MVI   SVPARMS,DFLTH       EXIT HIGH FOR FILTER                         
         B     EXITOK                                                           
FLTXX    MVI   SVPARMS,DFLTX       EXIT NOT WANTED FOR FILTER                   
         B     EXITOK                                                           
*                                                                               
DIE      DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* TABLE ITERATION ROUTINE  - EXPECTS RF TO HOLD A(TABLE)              *         
*                          - EXPECTS R1 TO HOLD VERB                  *         
***********************************************************************         
         SPACE 1                                                                
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK           ** NEED TO SET HIGH IF NOT OVERRIDE             
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     ITER                ITERATE THIS TABLE                           
*                                                                               
ITER02   LR    RE,RF               @@ DEBUG  @@                                 
         ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
         LA    R1,SVPARMS                                                       
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
         SPACE 2                                                                
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
         DC    AL1(ORECH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     GOTO1 VDICTAT,BOPARM,C'LU  ',DCLIST,DSLISTU                            
         GOTO1 (RF),(R1),C'LL  ',,DSLISTL                                       
         OI    GCINDS3,GCINOACT                                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* KEY OBJECT                                                          *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(KEY)                                                           *         
* P4 HOLDS SUB-ACTION                                                 *         
***********************************************************************         
         SPACE 1                                                                
KEY      LM    R0,R3,SVPARMS                                                    
         USING CTDEREC,R2                                                       
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
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KFKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
KFKVAL   XC    CTDEKEY,CTDEKEY     INITIALIZE KEY                               
         MVI   CTDEKTYP,CTDEKTYQ                                                
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  XC    CTDEKEY,CTDEKEY     INITIALIZE KEY                               
         MVI   CTDEKTYP,CTDEKTYQ                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT IDENTIFIER                                  *         
* P2 HOLDS EQUATED DATA IDENTIFIER OR 0 IF GLOBAL DATA ACTION         *         
* P3 BYTE  0   HOLDS EQUATED DATA VERB IF P2 IS ZERO                  *         
* P3 BYTES 1-3 HOLDS EQUATED ACTION VERB                              *         
* P4 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P5 HOLDS A(FIELD TABLE ENTRY) OR ZERO IF P2 IS ZERO                 *         
*                                                                     *         
* FIELD DATA IS EXTRACTED/OUTPUT INTO FVIFLD                          *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
DATA     ICM   R1,15,SVPARMS2      R1=DATA IDENTIFIER                           
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS4                                                      
         USING CTDEKEY,R2                                                       
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
         USING CTDEREC,R2                                                       
         BR    RF                                                               
         SPACE 1                                                                
*                                                                               
DTATABL  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(00001),AL4(SYSDTA)    SYSTEM                                 
         DC    AL2(00002),AL4(PRGDTA)    PROGRAM                                
         DC    AL2(00003),AL4(ONLDTA)    ONLINE/OFFLINE                         
         DC    AL2(01013),AL4(EMLDTA)    EMAIL ID                               
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL17    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* SYSTEM OBJECT                                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
SYSDTA   LA    RF,SYSTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SYSTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSYS)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSYS)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTSYS)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTSYS)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTSYS)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A SYSTEM FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
DISSYS   L     RE,ASYSLST                                                       
         LA    RE,6(RE)                                                         
         USING SYSLSTD,RE          RE=A(SYSTEM LIST)                            
DSYS02   CLI   0(RE),0             TEST E-O-T                                   
         BE    DSYS08                                                           
         CLI   CTDEKSUB,CTDEKSON   ONLINE?                                      
         BNE   DSYS04                                                           
         CLC   CTDESYS,SYSLNUM     SYSTEM NUMBER                                
         BE    DSYS06                                                           
         LA    RE,SYSLLEN(RE)      NO - BUMP TO NEXT TABLE ENTRY                
         B     DSYS02                                                           
*                                                                               
DSYS04   CLC   CTDEOSYS,SYSLNUM    SYSTEM REPORT CHARACTER                      
         BE    DSYS06                                                           
         LA    RE,SYSLLEN(RE)      NO - BUMP TO NEXT TABLE ENTRY                
         B     DSYS02                                                           
*                                                                               
DSYS06   MVC   FVIFLD(L'SYSLNAME),SYSLNAME                                      
         B     EXITOK                                                           
         DROP  RE                                                               
*                                                                               
DSYS08   GOTO1 VHEXOUT,BODMCB,CTDESYS,FVIFLD,1,0                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE A SYSTEM FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALSYS   XR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         L     R4,ASYSLST                                                       
         USING SYSLSTD,R4                                                       
         LA    R4,6(R4)            R4=A(SYSTEM LIST)                            
*                                                                               
VSYS02   CLI   0(R4),0             TEST E-O-T                                   
         BE    EXITNV                                                           
         EX    RF,*+8              MATCH INPUT TO TABLE                         
         BE    VSYS06                                                           
         CLC   SYSLNAME(0),FVIFLD                                               
*                                                                               
VSYS04   LA    R4,SYSLLEN(R4)      NO - BUMP TO NEXT TABLE ENTRY                
         B     VSYS02                                                           
*                                                                               
VSYS06   CLI   CTDEKSUB,CTDEKSON   ONLINE?                                      
         BNE   *+14                                                             
         MVC   CTDESYS,SYSLNUM     SYSTEM NUMBER                                
         B     EXITOK                                                           
*                                                                               
         MVC   CTDEOSYS,SYSLNUM                                                 
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 1                                                                
***********************************************************************         
* DISPLAY A SYSTEM FILTER FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DFLTSYS  L     RE,ASYSLST                                                       
         LA    RE,6(RE)                                                         
         USING SYSLSTD,RE          RE=A(SYSTEM LIST)                            
*                                                                               
DFSYS02  CLI   0(RE),0             TEST E-O-T                                   
         BE    DFSYS04                                                          
         CLC   SYSLNUM,FLTIFLD        SYSTEM NUMBER                             
         BE    *+12                                                             
         AHI   RE,SYSLLEN          NO - BUMP TO NEXT TABLE ENTRY                
         B     DFSYS02                                                          
*                                                                               
         MVC   FVIFLD(L'SYSLNAME),SYSLNAME                                      
         B     EXITOK                                                           
         DROP  RE                                                               
*                                                                               
DFSYS04  GOTO1 VHEXOUT,BODMCB,FLTIFLD,FVIFLD,1,0                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A SYSTEM FILTER FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
VFLTSYS  XR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         L     R4,ASYSLST                                                       
         USING SYSLSTD,R4                                                       
         LA    R4,6(R4)            R4=A(SYSTEM LIST)                            
*                                                                               
VFSYS02  CLI   0(R4),0             TEST E-O-T                                   
         BE    EXITNV                                                           
         EX    RF,*+8              MATCH INPUT TO TABLE                         
         BE    VFSYS06                                                          
         CLC   SYSLNAME(0),FVIFLD                                               
*                                                                               
VFSYS04  LA    R4,SYSLLEN(R4)      NO - BUMP TO NEXT TABLE ENTRY                
         B     VFSYS02                                                          
*                                                                               
VFSYS06  CLI   CTDEKSUB,CTDEKSON   ONLINE?                                      
         BNE   VFSYS08                                                          
         MVC   CTDESYS,SYSLNUM     SYSTEM NUMBER INTO KEY                       
         MVC   FLTIFLD(L'SYSLNUM),SYSLNUM    - AND FILTER                       
         B     EXITOK                                                           
*                                                                               
VFSYS08  MVC   CTDEOSYS,SYSLNUM    SYSTEM CHAR INTO KEY                         
         MVC   FLTIFLD(L'SYSLNUM),SYSLNUM     - AND FILTER                      
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 1                                                                
***********************************************************************         
* DO SYSTEM FILTERING                                                 *         
***********************************************************************         
         SPACE 1                                                                
DOFTSYS  CLI   CTDEKSUB,CTDEKSON   ONLINE?                                      
         BNE   DOSYS02             NO                                           
         CLC   CTDESYS,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
*                                                                               
DOSYS02  CLC   CTDEOSYS,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PROGRAM                                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
PRGDTA   LA    RF,PRGTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
PRGTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPRG)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMPRG)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPRG)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTPRG)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTPRG)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTPRG)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PROGRAM FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
DISPRG   OC    CTDEPRG,CTDEPRG     WAS A PROGRAM ENTERED?                       
         BZ    EXITOK              NO                                           
*                                                                               
         CLI   CTDEKSUB,CTDEKSON   OFFLINE JUST USES 2 CHARS                    
         BE    *+14                                                             
         MVC   FVIFLD(L'CTDERPT),CTDERPT                                        
         B     EXITOK                                                           
*                                                                               
         L     R1,ASYSFAC                                                       
         ICM   R1,15,VSELIST-SYSFACD(R1)                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,CTDESYS                                                  
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     DPRG08                                                           
*                                                                               
         L     R1,SEPGMS                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1          FIND PGMLST ENTRY                            
DPRG02   CLC   PGMNUM,CTDEPRG                                                   
         BNE   DPRG04                                                           
         CLI   PGMCTRY,0           MATCH ON COUNTRY CODE                        
         BE    DPRG06                                                           
         CLC   PGMCTRY,CUCTRY                                                   
         BE    DPRG06                                                           
DPRG04   BXLE  R1,RE,DPRG02                                                     
         B     DPRG08                                                           
*                                                                               
DPRG06   MVC   FVIFLD(L'PGMNAME),PGMNAME                                        
         B     EXITOK                                                           
*                                                                               
DPRG08   GOTO1 VHEXOUT,BODMCB,CTDEPRG,FVIFLD,L'CTDEPRG,0                        
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 1                                                                
***********************************************************************         
* DISPLAY TAG FOR PROGRAM FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DMPRG    CLI   CTDEKSUB,CTDEKSOF   ONLY CHANGE IF OFFLINE                       
         BNE   EXITOK                                                           
         L     RF,SVPARMS6                                                      
         MVC   0(L'LC@RPT,RF),LC@RPT                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE A PROGRAM FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
VALPRG   MVI   CTDEPRG,0           DEFAULT IS SYSTEM LEVEL ONLY                 
         CLI   FVILEN,0            TRYING TO SET TO SYSTEM LEVEL ONLY?          
         BE    EXITOK              YES                                          
*                                                                               
         CLI   CTDEKSUB,CTDEKSON   OFFLINE JUST USES 2 CHARS                    
         BE    VPRG02                                                           
         CLI   FVILEN,2                                                         
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL                                                            
*                                                                               
         MVC   CTDERPT,FVIFLD                                                   
         B     EXITOK                                                           
*                                                                               
VPRG02   TM    FVIIND,FVIHEX                                                    
         BZ    VPRG04                                                           
         CLI   FVILEN,2                                                         
         BNE   VPRG04                                                           
         GOTO1 VHEXIN,BODMCB,FVIFLD,CTDEPRG,2,0                                 
         OC    12(4,R1),12(R1)                                                  
         BZ    EXITNV                                                           
         B     EXITOK                                                           
*                                                                               
VPRG04   L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,CTDESYS                                                  
         BE    VPRG06                                                           
         BXLE  R1,RE,*-10                                                       
         MVC   FVMSGNO,=AL2(FVFESYS)  INVALID SYSTEM                            
         B     EXITL                                                            
*                                                                               
VPRG06   L     R1,SEPGMS                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1          R1=A(PROGRAMS LIST)                          
         XR    R3,R3                                                            
         IC    R3,FVXLEN           R3=INPUT LENGTH-1                            
*                                                                               
VPRG08   CLI   PGMCTRY,0           MATCH ON COUNTRY                             
         BE    *+14                                                             
         CLC   PGMCTRY,CUCTRY                                                   
         BNE   VPRG10                                                           
         EX    R3,*+8                                                           
         BE    VPRG12                                                           
         CLC   PGMNAME(0),FVIFLD   MATCH INPUT TO PROGRAM NAME                  
*                                                                               
VPRG10   BXLE  R1,RE,VPRG08                                                     
         MVI   FVOSYS,GCSYSGEN     CONTROL SYSTEM                               
         MVC   FVMSGNO,=AL2(CE#PGNVS) INVALID PROGRAM                           
         B     EXITL                                                            
*                                                                               
VPRG12   MVC   CTDEPRG,PGMNUM      PROGRAM NUMBER                               
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PROGRAM FILTER FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
DFLTPRG  OC    FLTIFLD(L'CTDEPRG),FLTIFLD PROGRAM ENTERED?                      
         BZ    EXITOK              NO                                           
*                                                                               
         L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,CTDESYS                                                  
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     DFPRG08                                                          
*                                                                               
         L     R1,SEPGMS                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1          FIND PGMLST ENTRY                            
DFPRG02  CLC   PGMNUM,FLTIFLD                                                   
         BNE   DFPRG04                                                          
         CLI   PGMCTRY,0           MATCH ON COUNTRY CODE                        
         BE    DFPRG06                                                          
         CLC   PGMCTRY,CUCTRY                                                   
         BE    DFPRG06                                                          
DFPRG04  BXLE  R1,RE,DFPRG02                                                    
         B     DFPRG08                                                          
         SPACE 1                                                                
DFPRG06  MVC   FVIFLD(L'PGMNAME),PGMNAME                                        
         B     EXITOK                                                           
         DROP  R1                                                               
*                                                                               
DFPRG08  GOTO1 VHEXOUT,BODMCB,FLTIFLD,FVIFLD,1,0                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PROGRAM FILTER FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
VFLTPRG  MVI   CTDEPRG,0           DEFAULT IS SYSTEM LEVEL ONLY                 
         MVI   FLTIFLD,0                                                        
         CLI   FVILEN,0            TRYING TO SET TO SYSTEM LEVEL ONLY?          
         BE    EXITOK              YES                                          
         SPACE 1                                                                
         L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,CTDESYS                                                  
         BE    VFPRG02                                                          
         BXLE  R1,RE,*-10                                                       
         MVC   FVMSGNO,=AL2(FVFESYS)  INVALID SYSTEM                            
         B     EXITL                                                            
         SPACE 1                                                                
VFPRG02  L     R1,SEPGMS                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1          R1=A(PROGRAMS LIST)                          
         XR    R3,R3                                                            
         IC    R3,FVXLEN           R3=INPUT LENGTH-1                            
         SPACE 1                                                                
VFPRG04  CLI   PGMCTRY,0           MATCH ON COUNTRY                             
         BE    *+14                                                             
         CLC   PGMCTRY,CUCTRY                                                   
         BNE   VFPRG06                                                          
         EX    R3,*+8                                                           
         BE    VFPRG08                                                          
         CLC   PGMNAME(0),FVIFLD   MATCH INPUT TO PROGRAM NAME                  
*                                                                               
VFPRG06  BXLE  R1,RE,VFPRG04                                                    
         MVI   FVOSYS,GCSYSGEN     CONTROL SYSTEM                               
         MVC   FVMSGNO,=AL2(CE#PGNVS) INVALID PROGRAM                           
         B     EXITL                                                            
*                                                                               
VFPRG08  MVC   CTDEPRG,PGMNUM      PROGRAM NUMBER                               
         MVC   FLTIFLD(L'PGMNUM),PGMNUM                                         
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 1                                                                
***********************************************************************         
* DO FILTERING FOR PROGRAM FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
DOFTPRG  CLC   CTDEPRG,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* ON/OFF LINE OBJECT                                                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
ONLDTA   LA    RF,ONLTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
ONLTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISONL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALONL)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTONL)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTONL)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTONL)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ON/OFFLINE FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
DISONL   MVC   FVIFLD(L'LC@ONLN),LC@ONLN                                        
         CLI   CTDEKSUB,CTDEKSON                                                
         BE    EXITOK                                                           
         MVC   FVIFLD(L'LC@OFFLN),LC@OFFLN                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE ON/OFFLINE FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
VALONL   MVI   CTDEKSUB,CTDEKSON                                                
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT TO ONLINE                            
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UC@ONLN                                                
         EX    RF,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@ONLN                                                
*                                                                               
         MVI   CTDEKSUB,CTDEKSOF                                                
         EX    RF,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UC@OFFLN                                               
         EX    RF,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@OFFLN                                               
*                                                                               
         B     EXITNV                                                           
         SPACE 1                                                                
***********************************************************************         
* DISPLAY ONLINE/OFFLINE FILTER FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
DFLTONL  MVC   FVIFLD(L'LC@ONLN),LC@ONLN                                        
         MVC   BOBYTE1,FLTIFLD                                                  
         XI    BOBYTE1,X'FF'                                                    
         CLC   CTDEKSUB,BOBYTE1                                                 
         BE    EXITOK                                                           
         MVC   FVIFLD(L'LC@OFFLN),LC@OFFLN                                      
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE ONLINE/OFFLINE FILTER FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
VFLTONL  CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT TO ONLINE                            
*                                                                               
         MVI   CTDEKSUB,CTDEKSON                                                
         MVC   FLTIFLD(1),CTDEKSUB                                              
         XI    FLTIFLD,X'FF'                                                    
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UC@ONLN                                                
         EX    RF,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@ONLN                                                
*                                                                               
         MVI   CTDEKSUB,CTDEKSOF                                                
         MVC   FLTIFLD(1),CTDEKSUB                                              
         XI    FLTIFLD,X'FF'                                                    
*                                                                               
         EX    RF,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UC@OFFLN                                               
         EX    RF,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@OFFLN                                               
*                                                                               
         B     EXITNV                                                           
         SPACE 1                                                                
***********************************************************************         
* DO ON/OFFLINE FILTERING                                             *         
***********************************************************************         
         SPACE 1                                                                
DOFTONL  MVC   BOBYTE1,FLTIFLD                                                  
         XI    BOBYTE1,X'FF'                                                    
         CLC   CTDESYS,BOBYTE1                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR EMAIL ID                                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
EMLDTA   LA    RF,EMLTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
EMLTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISEML)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALEML)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY EMAIL ID                                                    *         
***********************************************************************         
         SPACE 1                                                                
DISEML   MVC   FVIFLD(L'CTDEEML),CTDEEML                                        
         B     EXITOK              NO                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE EMAIL ID                                                   *         
***********************************************************************         
         SPACE 1                                                                
VALEML   MVI   CTDEELM,CTDEELMQ                                                 
         MVI   CTDELN,CTDELNQ                                                   
         MVC   CTDEEML,FVIFLD      SET EMAIL AS ENTERED                         
         MVC   CTDELEN,=AL2(CTDERECL)                                           
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LIST OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS CURRENT KEY BUILD AREA                                     *         
* P4 HOLDS PREVIOUS KEY                                               *         
***********************************************************************         
         SPACE 1                                                                
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING CTDEREC,R2                                                       
         LA    RF,LISTABL                                                       
         USING OBJTABD,RF                                                       
LITER    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK                                                           
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    LITER02             MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     LITER               ITERATE TABLE                                
*                                                                               
LITER02  CLC   OBJIND3,GSSMPAGE    CHECK PAGE OK (0 FOR LIST)                   
         BE    LITER04                                                          
         LA    RF,OBJTABL(RF)                                                   
         B     LITER                                                            
*                                                                               
LITER04  ICM   RF,15,OBJADR        INVOKE OBJECT                                
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
         EJECT                                                                  
*                                                                               
LISTABL  DS    0A                                                               
         DC    AL1(LINIT),AL1(0,0,0),AL4(INITL)                                 
         DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* INITIALISE FOR LIST                                                 *         
***********************************************************************         
         SPACE 1                                                                
INITL    B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
FLST     MVC   IOKEY(L'CTDEKEY),THIS.CTDEKEY                                    
         ICM   R1,15,=AL4(XOHI+XOCONFIL)                                        
         A     R1,SVPARMS5         EQUATED I/O AREA TO USE                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               READ HIGH UNHAPPY                            
         B     NLST04                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     MVC   IOKEY(L'CTJKEY),THIS.CTDEKEY                                     
*                                                                               
NLST02   XR    RF,RF               INCREMENT NAME ID                            
         ICM   RF,3,IOKEY+(CTDESYS-CTDEKEY)                                     
         AHI   RF,1                                                             
         STCM  RF,3,IOKEY+(CTDESYS-CTDEKEY)                                     
         ICM   R1,15,=AL4(XOHI+XOCONFIL)                                        
         A     R1,SVPARMS5         EQUATED I/O AREA TO USE                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               READ HIGH UNHAPPY                            
*                                                                               
NLST04   L     RF,SVPARMS5         CONTROL FILE RECORDS ARE RETURNED IN         
         SRL   RF,4-2              THE I/O AREA                                 
         L     RF,AIO1-L'AIO1(RF)                                               
         CLC   THIS.CTDEKTYP,0(RF)                                              
         BNE   EXITL                                                            
*                                                                               
NLST06   MVC   THIS.CTDEKEY,0(RF)  WE WANT THIS RECORD                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
FFFFFFFF EQU   X'FFFFFFFF'                                                      
         SPACE 1                                                                
         DS    0D                                                               
DCLIST   DCDDL CT#RPT,8,L                                                       
         DCDDL CT#OFFLN,8,L                                                     
         DCDDL CT#ONLN,8,L                                                      
         DCDDL CT#YES,4,L                                                       
         DCDDL GE#NO,4,L                                                        
DCLISTX  DC    X'00'                                                            
*                                                                               
       ++INCLUDE FACTRYTAB                                                      
         EJECT                                                                  
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
AFDREL   DS    A                                                                
AFLTRL   DS    A                                                                
AFVADDR  DS    A                                                                
ESCLEN   DS    XL1                                                              
ESCCHAR  DS    XL2                                                              
MYSCAN   DS    3CL(SCBLKLQ)                                                     
*                                                                               
DSLISTU  DS    0D                                                               
UC@RPT   DS    XL8                                                              
UC@OFFLN DS    XL8                                                              
UC@ONLN  DS    XL8                                                              
UE@YES   DS    XL4                                                              
UE@NO    DS    XL4                                                              
*                                                                               
DSLISTL  DS    0D                                                               
LC@RPT   DS    XL8                                                              
LC@OFFLN DS    XL8                                                              
LC@ONLN  DS    XL8                                                              
LC@YES   DS    XL4                                                              
LC@NO    DS    XL4                                                              
         SPACE 2                                                                
*        CTFILWORK                                                              
         PRINT OFF                                                              
       ++INCLUDE CTFILWORK                                                      
         PRINT ON                                                               
*        FASYSLSTD                                                              
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
*        DDDDEQUS                                                               
         PRINT OFF                                                              
       ++INCLUDE DDDDEQUS                                                       
         PRINT ON                                                               
*        CTMSGEQUS                                                              
         PRINT OFF                                                              
       ++INCLUDE CTMSGEQUS                                                      
         PRINT ON                                                               
*        FASELIST                                                               
         PRINT OFF                                                              
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
*        FAPGMLST                                                               
         PRINT OFF                                                              
       ++INCLUDE FAPGMLST                                                       
         PRINT ON                                                               
*        FASYSFAC                                                               
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
*        DDSCANBLKD                                                             
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
*        DDCOMFACS                                                              
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*        DDFLDHDR                                                               
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
*        CTGENFILE                                                              
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010CTFIL17   11/16/04'                                      
         END                                                                    
