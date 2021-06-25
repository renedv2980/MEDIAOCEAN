*          DATA SET CTFIL16X   AT LEVEL 002 AS OF 08/22/00                      
*&&      SET   NOP=N                                                            
*PHASE TA1316A                                                                  
FIL16    TITLE 'DOWNLOAD COLUMN RECORD OVERLAY'                                 
FIL16    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,CTFIL16*,R7,RR=RE                                              
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         MVC   SVPARMS,0(R1)                                                    
         ST    R1,CALLR1                                                        
         L     R6,ASVEBLK                                                       
         USING MYSAVE,R6                                                        
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
ROUTSN   EQU   (*-ROUTS)/5                                                      
         SPACE 1                                                                
*                                                                               
GSFRR    USING FRRELD,GSFRREL                                                   
PSFRR    USING FRRELD,PSFRREL                                                   
GSFRA    USING FRRELD,GSFRAEL                                                   
PSFRA    USING FRRELD,PSFRAEL                                                   
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
FLTXL    MVI   SVPARMS,0           EXIT LOW FOR FILTER                          
         B     EXITOK                                                           
FLTXE    MVI   SVPARMS,1           EXIT EQUAL FOR FILTER                        
         B     EXITOK                                                           
FLTXH    MVI   SVPARMS,2           EXIT HIGH FOR FILTER                         
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
ITER02   ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
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
         DC    AL1(ORECH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     B     EXITOK                                                           
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
KEY      LM    R0,R2,SVPARMS                                                    
         USING FDLRECD,R2                                                       
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
KFTABL   DC    AL1(KDIS),AL1(0,0,0),AL4(KFKDIS)      DISPLAY                    
         DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(KFDIS),AL1(0,0,0),AL4(KFKFDIS)    DISPLAY FILTER             
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KFKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A KEY OBJECT                              *         
***********************************************************************         
         SPACE 1                                                                
KFKDIS   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
KFKVAL   XC    FDLKEY,FDLKEY       INITIALIZE KEY OF FIELD RECORD               
         MVI   FDLKMIN,FDLKMINQ    SET MINOR SYSTEM                             
         MVI   FDLKTYP,FDLKTYPQ    SET FIELD RECORD                             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A KEY FILTER                              *         
***********************************************************************         
         SPACE 1                                                                
KFKFDIS  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  XC    FDLKEY,FDLKEY       INITIALIZE KEY OF FIELD RECORD               
         MVI   FDLKMIN,FDLKMINQ    SET MINOR SYSTEM                             
         MVI   FDLKTYP,FDLKTYPQ    SET FIELD RECORD                             
         B     EXITOK                                                           
         SPACE 2                                                                
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
         USING FDLRECD,R2                                                       
         XR    R1,R1                                                            
         IC    R1,SVPARMS3         GET GLOBAL VERB                              
         LA    RF,DTATABL          TABLE OF GLOBAL VERBS                        
         B     ITER                ITERATE TABLE                                
*                                                                               
DATA02   LA    RF,KNOWTAB          TABLE OF KNOWN OBJECTS                       
         USING KNOWTABD,RF                                                      
*                                                                               
DATA05   CLC   KNOWID,=AL2(EOT)    REACH END - NOT A KNOWN DATA TYPE            
         BE    EXITH                                                            
         CLM   R1,3,KNOWID         IS THIS A KNOWN TYPE?                        
         BE    DATA06                                                           
         LA    RF,KNOWLQ(RF)                                                    
         B     DATA05                                                           
*                                                                               
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
*                                                                               
         LM    R1,R2,SVPARMS3      R1 HOLDS VERB                                
         USING FDLRECD,R2          R2 HOLDS A(RECORD)                           
         BR    RF                                                               
         SPACE 1                                                                
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(00001),AL4(SYSDTA)    SYSTEM                                 
         DC    AL2(00002),AL4(PRGDTA)    PROGRAM                                
         DC    AL2(00003),AL4(RCODTA)    RECORD                                 
         DC    AL2(00084),AL4(FNTDTA)    FIELD TAG                              
         DC    AL2(00083),AL4(FFEDTA)    FIELD INPUT                            
         DC    AL2(00082),AL4(NAMDTA)    DOWNLOAD NAME                          
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL16    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                          *         
***********************************************************************         
         SPACE 1                                                                
DTAFRST  L     R1,SVPARMS3         VERB                                         
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
*                             *** FIRST TIME FOR DATA OBJECT ***                
*                                 -------------------------                     
DFTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DFDDIS)      DISPLAY                    
         DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)      VALIDATE                   
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFDDIS)     DISPLAY FILTER             
         DC    AL1(DFVAL),AL1(0,0,0),AL4(DFDVAL)     VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DFDDIS   B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR SYSTEM                                              *         
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
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISSYS)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A SYSTEM FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
DISSYS   L     RE,ASYSLST                                                       
         LA    RE,6(RE)                                                         
         USING SYSLSTD,RE          RE=A(SYSTEM LIST)                            
         SPACE 1                                                                
DSYS02   CLI   0(RE),0             TEST E-O-T                                   
         BE    DSYS04                                                           
         CLC   SYSLNUM,FDLKSYS     MATCH ON SYSTEM NUMBER                       
         BE    *+12                                                             
         LA    RE,SYSLLEN(RE)      NO - BUMP TO NEXT TABLE ENTRY                
         B     DSYS02                                                           
         SPACE 1                                                                
         MVC   FVIFLD(L'SYSLNAME),SYSLNAME                                      
         B     EXITOK                                                           
         DROP  RE                                                               
         SPACE 1                                                                
DSYS04   CURED FDLKSYS,(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A SYSTEM FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALSYS   XR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         L     R4,ASYSLST                                                       
         USING SYSLSTD,R4                                                       
         LA    R4,6(R4)            R4=A(SYSTEM LIST)                            
         SPACE 1                                                                
VSYS02   CLI   0(R4),0             TEST E-O-T                                   
         BE    EXITNV                                                           
         CLI   SYSLNUM,1           IGNORE SERVICE SYSTEM                        
         BE    VSYS04                                                           
         EX    RF,*+8              MATCH INPUT TO TABLE                         
         BE    VSYS06                                                           
         CLC   SYSLNAME(0),FVIFLD                                               
         SPACE 1                                                                
VSYS04   LA    R4,SYSLLEN(R4)      NO - BUMP TO NEXT TABLE ENTRY                
         B     VSYS02                                                           
         SPACE 1                                                                
VSYS06   MVC   FDLKSYS,SYSLNUM     SYSTEM NUMBER                                
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A SYSTEM FILTER FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DFLTSYS  L     RE,ASYSLST                                                       
         LA    RE,6(RE)                                                         
         USING SYSLSTD,RE          RE=A(SYSTEM LIST)                            
         SPACE 1                                                                
DFSYS02  CLI   0(RE),0             TEST E-O-T                                   
         BE    DFSYS04                                                          
         CLC   SYSLNUM,FLTIFLD     MATCH ON SYSTEM NUMBER                       
         BE    *+12                                                             
         LA    RE,SYSLLEN(RE)      NO - BUMP TO NEXT TABLE ENTRY                
         B     DFSYS02                                                          
         SPACE 1                                                                
         MVC   FVIFLD(L'SYSLNAME),SYSLNAME                                      
         B     EXITOK                                                           
         DROP  RE                                                               
         SPACE 1                                                                
DFSYS04  XR    RF,RF                                                            
         IC    RF,FLTIFLD                                                       
         CURED (RF),(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                         
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
         SPACE 1                                                                
VFSYS02  CLI   0(R4),0             TEST E-O-T                                   
         BE    EXITNV                                                           
         CLI   SYSLNUM,1           IGNORE SERVICE SYSTEM                        
         BE    VFSYS04                                                          
         EX    RF,*+8              MATCH INPUT TO TABLE                         
         BE    VFSYS06                                                          
         CLC   SYSLNAME(0),FVIFLD                                               
         SPACE 1                                                                
VFSYS04  LA    R4,SYSLLEN(R4)      NO - BUMP TO NEXT TABLE ENTRY                
         B     VFSYS02                                                          
         SPACE 1                                                                
VFSYS06  MVC   FDLKSYS,SYSLNUM     SYSTEM NUMBER INTO KEY                       
         MVC   FLTIFLD(L'SYSLNUM),SYSLNUM    - AND FILTER                       
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* DO SYSTEM FILTERING                                                 *         
***********************************************************************         
         SPACE 1                                                                
DOFTSYS  CLC   FDLKSYS,FLTIFLD                                                  
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
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPRG)                                 
         DC    AL1(DHED),AL1(0,0,0),AL4(HEDPRG)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTPRG)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTPRG)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTPRG)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PROGRAM FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
DISPRG   OC    FDLKPRG,FDLKPRG     WAS A PROGRAM ENTERED?                       
         BZ    EXITOK              NO                                           
         SPACE 1                                                                
         L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FDLKSYS                                                  
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     DPRG08                                                           
         SPACE 1                                                                
         L     R1,SEPGMS                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1          FIND PGMLST ENTRY                            
DPRG02   CLC   PGMNUM,FDLKPRG                                                   
         BNE   DPRG04                                                           
         CLI   PGMCTRY,0           MATCH ON COUNTRY CODE                        
         BE    DPRG06                                                           
         CLC   PGMCTRY,CUCTRY                                                   
         BE    DPRG06                                                           
DPRG04   BXLE  R1,RE,DPRG02                                                     
         B     DPRG08                                                           
         SPACE 1                                                                
DPRG06   MVC   FVIFLD(L'PGMNAME),PGMNAME                                        
         B     EXITOK                                                           
         SPACE 1                                                                
DPRG08   CURED FDLKPRG,(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                      
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PROGRAM FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
VALPRG   MVI   FDLKPRG,0           DEFAULT IS SYSTEM LEVEL ONLY                 
         CLI   FVILEN,0            TRYING TO SET TO SYSTEM LEVEL ONLY?          
         BE    EXITOK              YES                                          
         SPACE 1                                                                
         L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FDLKSYS                                                  
         BE    VPRG02                                                           
         BXLE  R1,RE,*-10                                                       
         MVC   FVMSGNO,=AL2(FVFESYS)  INVALID SYSTEM                            
         B     EXITL                                                            
         SPACE 1                                                                
VPRG02   L     R1,SEPGMS                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1          R1=A(PROGRAMS LIST)                          
         XR    R3,R3                                                            
         IC    R3,FVXLEN           R3=INPUT LENGTH-1                            
         SPACE 1                                                                
VPRG04   CLI   PGMCTRY,0           MATCH ON COUNTRY                             
         BE    *+14                                                             
         CLC   PGMCTRY,CUCTRY                                                   
         BNE   VPRG06                                                           
         EX    R3,*+8                                                           
         BE    VPRG08                                                           
         CLC   PGMNAME(0),FVIFLD   MATCH INPUT TO PROGRAM NAME                  
         SPACE 1                                                                
VPRG06   BXLE  R1,RE,VPRG04                                                     
         MVI   FVOSYS,GCSYSGEN     CONTROL SYSTEM                               
         MVC   FVMSGNO,=AL2(CE#PGNVS) INVALID PROGRAM                           
         B     EXITL                                                            
         SPACE 1                                                                
VPRG08   MVC   FDLKPRG,PGMNUM      PROGRAM NUMBER                               
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* CREATE A PROGRAM HEADLINE FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
HEDPRG   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PROGRAM FILTER FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
DFLTPRG  OC    FLTIFLD(L'FDLKPRG),FLTIFLD PROGRAM ENTERED?                      
         BZ    EXITOK              NO                                           
         SPACE 1                                                                
         L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FDLKSYS                                                  
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     DFPRG08                                                          
         SPACE 1                                                                
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
         SPACE 1                                                                
DFPRG08  XR    RF,RF                                                            
         ICM   RF,1,FLTIFLD                                                     
         CURED (RF),(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                         
         B     EXITOK                                                           
         DROP  R1                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PROGRAM FILTER FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
VFLTPRG  MVI   FDLKPRG,0           DEFAULT IS SYSTEM LEVEL ONLY                 
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
         CLC   SEOVSYS,FDLKSYS                                                  
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
         SPACE 1                                                                
VFPRG06  BXLE  R1,RE,VFPRG04                                                    
         MVI   FVOSYS,GCSYSGEN     CONTROL SYSTEM                               
         MVC   FVMSGNO,=AL2(CE#PGNVS) INVALID PROGRAM                           
         B     EXITL                                                            
         SPACE 1                                                                
VFPRG08  MVC   FDLKPRG,PGMNUM      PROGRAM NUMBER                               
         MVC   FLTIFLD(L'PGMNUM),PGMNUM                                         
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR PROGRAM FIELD                                      *         
***********************************************************************         
DOFTPRG  CLC   FDLKPRG,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD                                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
RCODTA   LA    RF,RCOTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
RCOTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISRCO)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRCO)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTRCO)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTRCO)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTRCO)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
DISRCO   OC    FDLKREC,FDLKREC     GLOBAL FIELD?                                
         BZ    EXITOK                                                           
         ICM   RF,8,FDLKSYS                                                     
         ICM   RF,4,FDLKPRG                                                     
         ICM   RF,2,FDLKREC                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXDIS,(RF)                                   
         BE    EXITOK                                                           
         DC    H'0'                                                             
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALRCO   MVI   FDLKREC,0           IF NO RECORD                                 
         CLI   FVILEN,0            FIELD SET AT LEVELS ABOVE                    
         BE    EXITOK                                                           
         SPACE 1                                                                
         OC    FDLKPRG,FDLKPRG     TRYING TO OVERRIDE RECORD - MUST             
         BZ    EXITNV              INPUT THE PROGRAM                            
         SPACE 1                                                                
         XR    RF,RF                                                            
         ICM   RF,8,FDLKSYS                                                     
         ICM   RF,4,FDLKPRG                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXVAL,(RF)                                   
         BNE   EXITL               NOT FOUND                                    
         L     RF,8(R1)                                                         
         STCM  RF,2,FDLKREC                                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD FILTER FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DFLTRCO  OC    FLTIFLD(L'FDLKREC),FLTIFLD RECORD FIELD?                         
         BZ    EXITOK                                                           
         ICM   RF,8,FDLKSYS                                                     
         ICM   RF,4,FDLKPRG                                                     
         ICM   RF,2,FLTIFLD                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXDIS,(RF)                                   
         BE    EXITOK                                                           
         DC    H'0'                                                             
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD FILTER FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
VFLTRCO  OC    FDLKPRG,FDLKPRG     TRYING TO FILTER RECORD - MUST               
         BZ    EXITNV              INPUT THE PROGRAM                            
         SPACE 1                                                                
         XR    RF,RF                                                            
         ICM   RF,8,FDLKSYS                                                     
         ICM   RF,4,FDLKPRG                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXVAL,(RF)                                   
         BNE   EXITL               NOT FOUND                                    
         L     RF,8(R1)                                                         
         STCM  RF,2,FDLKREC                                                     
         STCM  RF,2,FLTIFLD                                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR RECORD                                             *         
***********************************************************************         
         SPACE 1                                                                
DOFTRCO  CLC   FDLKREC,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DOWNLOAD NAME                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
NAMDTA   LA    RF,NAMTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
NAMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNAM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNAM)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTNAM)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTNAM)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTNAM)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A DOWNLOAD NAME FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DISNAM   MVC   FVIFLD(L'FDLKCODE),FDLKCODE                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A DOWNLOAD NAME FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
VALNAM   MVC   FDLKCODE,FVIFLD                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A DOWNLOAD NAME FILTER FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
DFLTNAM  MVC   FVIFLD(L'FDLKCODE),FLTIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A DOWNLOAD NAME FILTER FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
VFLTNAM  MVC   FLTIFLD(L'FDLKCODE),FDLKCODE                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR DOWNLOAD NAME                                      *         
***********************************************************************         
         SPACE 1                                                                
DOFTNAM  CLC   FDLKCODE,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR FIELD COLUMN NAMES                                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
FNTDTA   LA    RF,FNTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
FNTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFNT)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY COLUMN LIST TAG                                             *         
***********************************************************************         
         SPACE 1                                                                
DISFNT   MVC   FVIFLD(L'TLKRNAM),TLKRNAM                                        
         B     EXITOK              MOVE OUT FIELD FROM TSAR RECORD              
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DIS=VALIDATION                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
FFEDTA   LA    RF,FFETBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
FFETBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFFE)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFFE)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER FIELD VALUES                                         *         
***********************************************************************         
         SPACE 1                                                                
DISFFE   XR    RF,RF                                                            
         ICM   RF,1,TLKPOS                                                      
         BZ    EXITOK                                                           
         CURED (RF),(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER FIELD VALUES                                        *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING FDLELD,BOELEM                                                    
VALFFE   L     R3,AIOREC           A(THIS TSAR RECORD)                          
         USING FDLRECD,R3                                                       
*                                                                               
         XR    RE,RE                                                            
         LA    RF,FDLRECD+FDLFIRST                                              
X        USING FDLELD,RF                                                        
VFFE02   CLI   X.FDLEL,0           EOR?                                         
         BE    VFFE08                                                           
         CLC   TLKRNUM,X.FDLFNUM   MATCH FIELD NUMBER                           
         BE    VFFE06                                                           
*                                                                               
VFFE04   IC    RE,X.FDLLN          ITERATE RECORD                               
         LA    RF,0(RE,RF)                                                      
         B     VFFE02                                                           
*                                                                               
VFFE06   MVI   X.FDLEL,FF          SET ELEMENT CODE DELETABLE                   
         B     VFFE04                                                           
         DROP  X                                                                
*                                                                               
VFFE08   GOTOX ADELEL,BOPARM,(=AL1(FF),FDLRECD),0                               
         CLI   FVILEN,0            CLEARING THIS FIELD?                         
         BE    EXITOK                                                           
*                                                                               
         XC    BOELEM(FDLLNQ),BOELEM                                            
         MVI   FDLEL,FDLELQ        BUILD DUMMY FDLEL                            
         MVI   FDLLN,FDLLNQ                                                     
         MVC   FDLFNUM,TLKRNUM                                                  
*                                                                               
VFFE10   TM    FVIIND,FVINUM       IF ONLY NUMBER INPUT, THEN                   
         BZ    EXITNOTN            DEFAULT IS VARIABLE & CLOSED                 
         ICM   RF,15,BCFULL                                                     
         BZ    EXITNV                                                           
                                                                                
VFFE22   XR    R1,R1               SEE IF # FITS IN THIS DISPLAY TYPE           
         IC    R1,THISMAX                                                       
         CR    RF,R1               IS IT TOO BIG?                               
         BH    EXITNV              YES                                          
         STC   RF,FDLCNUM          SAVE IT                                      
*                                                                               
         LA    R4,FDLRECD+FDLFIRST START OF RECORD                              
X        USING FDLELD,R4                                                        
VFFE24   CLI   X.FDLEL,0                                                        
         BE    VFFE30                                                           
         CLI   X.FDLEL,FDLELQ                                                   
         BNE   VFFE26                                                           
*                                                                               
VFFE26   XR    RF,RF                                                            
         IC    RF,X.FDLLN                                                       
         LA    R4,0(RF,R4)                                                      
         B     VFFE24                                                           
*                                                                               
VFFE28   CLC   X.FDLCNUM,FDLCNUM   ??? THE POSITION TO GO INTO ???              
         BL    VFFE26                                                           
         IC    RF,X.FDLCNUM        BUMP THIS COLUMN NUMBER BY 1                 
         LA    RF,1(RF)                                                         
         STC   RF,X.FDLCNUM        SAVE IT                                      
         CLM   RF,1,THISMAX        ALLOWED?                                     
         BNH   *+8                                                              
         MVI   X.FDLEL,FF          MARK FOR DELETION                            
         B     VFFE26                                                           
*                                                                               
VFFE30   GOTO1 ADELEL,BOPARM,(=AL1(FF),FDLRECD),0                               
         GOTO1 AADDEL,BOPARM,FDLRECD                                            
*                                                                               
VFFE31   XR    RE,RE               MAKE SURE NUMBERS ARE SEQUENTIAL             
         LA    R1,1                                                             
         LA    RF,FDLRECD+FDLFIRST                                              
         MVI   LASTFCR,0                                                        
X        USING FDLELD,RF                                                        
VFFE32   CLI   X.FDLEL,0           EOR?                                         
         BE    VFFE34                                                           
         STCM  R1,1,X.FDLCNUM      SAVE COLUMN NUMBER                           
         LA    R1,1(R1)            BUMP COUNT                                   
         IC    RE,X.FDLLN          ITERATE RECORD                               
         LA    RF,0(RE,RF)                                                      
         B     VFFE32                                                           
         DROP  X                                                                
*                                                                               
VFFE34   OI    LSSCIND1,LSSCIBLD   REBUILD LIST                                 
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* NTRSES OBJECT                                                       *         
* -------------                                                       *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(INITIAL LIST KEY)                                        *         
* P4 HOLDS A(MEMORY COVERED BY FESD)                                  *         
***********************************************************************         
         SPACE 1                                                                
NTRSES   LM    R0,R3,SVPARMS                                                    
         USING FDLRECD,R2                                                       
         USING FESD,R3                                                          
         LA    RF,NSSTABL                                                       
         B     ITER                                                             
*                                                                               
NSSTABL  DC    AL1(SNTRIN),AL1(0,0,0),AL4(NTRIN)                                
         DC    AL1(SXITOUT),AL1(0,0,0),AL4(XITOUT)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER (EXIT FROM)                *         
***********************************************************************         
         SPACE 1                                                                
XITOUT   B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER (ENTER IN)               *         
***********************************************************************         
         SPACE 1                                                                
NTRIN    B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LIST OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS CURRENT KEY BUILD AREA                                     *         
* P4 HOLDS PREVIOUS KEY                                               *         
* P5 HOLDS EQUATE FOR IOREC IF REDEFINED                              *         
***********************************************************************         
         SPACE 1                                                                
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING FDLRECD,R2                                                       
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
*                                                                               
LISTABL  DC    AL1(LINIT),AL1(0,0,0),AL4(ILST)                                  
         DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
*PAGE1                                                                          
         DC    AL1(LINIT),AL1(0,0,1),AL4(ILST1)                                 
         DC    AL1(LGETFRST),AL1(0,0,1),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,1),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,1),AL4(FTFLST1)                            
         DC    AL1(LTSARDIR),AL1(0,0,1),AL4(TSARDIR1)                           
         DC    AL1(LTSARFIL),AL1(0,0,1),AL4(TSARFIL1)                           
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALISE LIST                                                     *         
***********************************************************************         
         SPACE 1                                                                
ILST     B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
FLST     MVC   IOKEY(L'FDLKEY),THIS.FDLRECD                                     
         ICM   R1,15,SVPARMS5                                                   
         A     R1,=A(XOGENDIR+XOHI)                                             
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               READ HIGH DIDN`T WORK                        
         B     NLST02                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     ICM   R1,15,SVPARMS5      CONTROLLER REESTABLISHES SEQUENCE            
         A     R1,=A(XOGENDIR+XOSEQ)                                            
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               END OF FILE                                  
*                                                                               
NLST02   CLC   IOKEY(FDLKSYS-FDLRECD),THIS.FDLRECD                              
         BNE   EXITL                                                            
*                                                                               
         ICM   R1,15,SVPARMS5                                                   
         A     R1,=A(XOGENFIL+XOGET)                                            
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   THIS.FDLKEY(FDLKLEN),IOKEY   WE WANT THIS KEY HERE...            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* INITIALISE LIST 1                                                   *         
***********************************************************************         
         SPACE 1                                                                
THIS     USING FDRRECD,R2                                                       
         USING FDRRECD,R3                                                       
ILST1    MVI   LSSUBLEN,0                                                       
         NI    LSSTAT1,FF-(LSSENTK+LSSTSAR)                                     
         OI    LSSTAT1,LSSBALL+LSSMULIN                                         
         NI    LSSTAT2,FF-(LSSADD)                                              
         MVC   LSCOLLIN,=AL2(25)                                                
         MVC   LSLINROW,=AL2(3)                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST BUILD 1                                         *         
***********************************************************************         
         SPACE 1                                                                
FTFLST1  L     RF,AIOREC                                                        
X        USING FDLRECD,RF                                                       
S        USING FSRRECD,IOKEY                                                    
         XC    S.FSRKEY,S.FSRKEY                                                
         XC    THIS.FDRKEY,THIS.FDRKEY                                          
         MVI   S.FSRKMIN,FSRKMINQ  BUILD FIELD RECORD KEY TO FILTER             
         MVI   S.FSRKTYP,FSRKTYPQ                                               
         MVC   S.FSRKSYS,X.FDLKSYS                                              
         MVC   S.FSRKPRG,X.FDLKPRG                                              
         MVC   S.FSRKREC,X.FDLKREC                                              
         MVI   S.FSRKPAGE,FSRKPLST SET LIST SCREEN                              
         MVC   S.FSRKCTRY,CUCTRY   SET CONNECTED COUNTRY                        
         XI    S.FSRKCTRY,FF                                                    
         L     R1,=A(XOGENDIR+XOREAD+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BE    FTL102              NO SCREEN RECORD FOR THIS COUNTRY            
*                                                                               
         XC    S.FSRKEY,S.FSRKEY                                                
         XC    THIS.FDRKEY,THIS.FDRKEY                                          
         MVI   S.FSRKMIN,FSRKMINQ  BUILD FIELD RECORD KEY TO FILTER             
         MVI   S.FSRKTYP,FSRKTYPQ                                               
         MVC   S.FSRKSYS,X.FDLKSYS                                              
         MVC   S.FSRKPRG,X.FDLKPRG                                              
         MVC   S.FSRKREC,X.FDLKREC                                              
         MVI   S.FSRKPAGE,FSRKPLST SET LIST SCREEN                              
*@@      MVI   S.FSRKCTRY,FF       SET HOST COUNTRY                             
         L     R1,=A(XOGENDIR+XOREAD+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   FTL104              NO SCREEN RECORD FOR THIS LIST               
*                                                                               
FTL102   L     R1,=A(XOGENFIL+XOGET+XIO1)                                       
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO1                                                          
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('SPRELQ',(RF)),0                  
         CLI   12(R1),0                                                         
         BNE   FTL104                 THIS SCREEN IS NOT A SHADOW               
         L     RF,12(R1)                                                        
         MVC   THIS.FDRKPRG,SPRPRG-SPRELD(RF)                                   
         MVC   THIS.FDRKREC,SPRREC-SPRELD(RF)                                   
*                                                                               
FTL104   L     RF,AIOREC                                                        
         MVI   THIS.FDRKMIN,FDRKMINQ  BUILD FIELD RECORD KEY TO FILTER          
         MVI   THIS.FDRKTYP,FDRKTYPQ                                            
         MVC   THIS.FDRKSYS,X.FDLKSYS                                           
         CLI   THIS.FDRKPRG,0         PROGRAM SET FROM SPREL?                   
         BNE   *+10                                                             
         MVC   THIS.FDRKPRG,X.FDLKPRG                                           
         CLI   THIS.FDRKREC,0         RECORD SET FROM SPREL?                    
         BNE   *+10                                                             
         MVC   THIS.FDRKREC,X.FDLKREC                                           
         B     EXITOK                                                           
         DROP  X,S                                                              
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST PAGE 1                                               *         
***********************************************************************         
         SPACE 1                                                                
FLST1    MVC   IOKEY(L'FDRKEY),THIS.FDRRECD                                     
         ICM   R1,15,SVPARMS5                                                   
         A     R1,=AL4(XOGENDIR+XOHI)                                           
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               READ HIGH DIDN`T WORK                        
         B     NLST102                                                          
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST PAGE 1                                                *         
***********************************************************************         
         SPACE 1                                                                
NLST1    ICM   R1,15,SVPARMS5      CONTROLLER REESTABLISHES SEQUENCE            
         A     R1,=AL4(XOGENDIR+XOSEQ)                                          
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               END OF FILE                                  
*                                                                               
NLST102  CLC   IOKEY(FDRKPAS-FDRRECD),THIS.FDRRECD                              
         BNE   EXITL                                                            
*                                                                               
         ICM   R1,15,SVPARMS5                                                   
         A     R1,=AL4(XOGENFIL+XOGET)                                          
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ICM   R1,15,SVPARMS5      GET EQUATE NUMBER FOR IO AREA                
         SRL   R1,4-2                                                           
         L     RF,AIO1-L'AIO1(R1)                                               
         GOTO1 AGETEL,BOPARM,('FDRELQ',(RF)),0                                  
         BNE   NLST1               NOT DISPLAYABLE IN LIST                      
L        USING FDRELD,BOELEM                                                    
         CLI   L.FDRLHLEN,0        LIST HEADING WIDTH IS 0?                     
         BE    NLST1                                                            
         CLI   L.FDRLCLEN,0        LIST DATA WIDTH IS 0?                        
         BE    NLST1                                                            
*                                                                               
         TM    L.FDRINDS1,FDR1DDS                                               
         BZ    *+12                                                             
         TM    CUSTAT,CUSDDS       TEST DDS TERMINAL IF DDS ONLY FIELD          
         BZ    NLST1                                                            
*                                                                               
         MVC   THIS.FDRKEY(FDRKLEN),IOKEY   WE WANT THIS KEY HERE...            
         B     EXITOK                                                           
         DROP  L,R3,THIS                                                        
         SPACE 2                                                                
***********************************************************************         
* GET TSAR RECORD INFORMATION FROM DIRECTORY RECORD                   *         
***********************************************************************         
         SPACE 1                                                                
         USING FDRRECD,R2                                                       
TSARDIR1 L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         LH    RF,=Y(TLENQ)                                                     
         STCM  RF,3,TLRLEN                                                      
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* GET TSAR RECORD INFORMATION FROM FILE RECORD                        *         
***********************************************************************         
         SPACE 1                                                                
         USING FDRRECD,R2                                                       
TSARFIL1 LA    R4,FDRFIRST(R2)                                                  
         USING FDRELD,R4                                                        
         L     R5,AIOREC           COLUMN RECORD STILL IN AIOREC                
         USING FDLRECD,R5                                                       
         L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         XR    R1,R1                                                            
         MVI   TLTYP,0                                                          
*                                                                               
TSAF02   CLI   FDREL,0                                                          
         BE    EXITOK                                                           
         CLI   FDREL,FDRELQ                                                     
         BE    TSAF04                                                           
         IC    R1,FDRLN                                                         
         LA    R4,0(R1,R4)                                                      
         B     TSAF02                                                           
*                                                                               
TSAF04   MVC   TLKRNUM,FDRNUM      SET FIELD NUMBER                             
         MVC   TLKRNAM,BCSPACES                                                 
*                                                                               
         OC    FDRLHED1,FDRLHED1   FIRST HEADLINE?                              
         BZ    TSAF06                                                           
*                                                                               
         MVC   TLKRNAM(L'FDRLHED1),FDRLHED1                                     
         MVI   TLKRNAM,DD#ESCL     SET HEADLINE 1 LEFT ALIGNED                  
*                                                                               
         LA    RF,D_LEN            LENGTH OF DISPLAY FIELD                      
         CLI   FDRLHED2,DD#ESUL2   SECOND HEADLINE IS UNDERLINE?                
         BNL   *+8                 NO                                           
         SRL   RF,1                LENGTH OF DISPLAY FIELD/2                    
         CLM   RF,1,TLKRNAM+3      SHORTER THAN SPACE?                          
         BNL   *+8                                                              
         STCM  RF,1,TLKRNAM+3      MAKE IT FIT                                  
*                                                                               
         ICM   RF,15,=C'SL  '      RESOLVE FIRST HEADLINE                       
         ICM   RF,2,FDRKSYS                                                     
         GOTO1 VDICTAT,BOPARM,(RF),TLKRNAM,0,0                                  
*                                                                               
TSAF06   CLI   FDRLHED2,DD#ESUL2   SECOND HEADLINE IS UNDERLINE?                
         BNL   TSAF08              YES                                          
         OC    FDRLHED2,FDRLHED2   SECOND HEADLINE?                             
         BZ    TSAF08              NO                                           
*                                                                               
         LA    RF,TLKRNAM+D_LEN-1                                               
         CLI   0(RF),C' '          PUT HEADING 2 AFTER HEADING 1                
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'/'          PUT SLASH BETWEEN THEM                       
         LA    RF,2(RF)                                                         
         MVC   0(L'FDRLHED2,RF),FDRLHED2                                        
         MVI   0(RF),DD#ESCL       SET LEFT ALIGN                               
         LA    R0,TLKRNAM+D_LEN-1                                               
         SR    R0,RF                                                            
         CLM   R0,1,3(RF)          WHAT'S WANTED LESS THAN WHAT'S LEFT?         
         BNL   *+8                 YES                                          
         STC   R0,3(RF)                                                         
         ICM   R0,15,=C'SL  '      RESOLVE SECOND HEADLINE                      
         ICM   R0,2,FDRKSYS                                                     
         GOTO1 VDICTAT,BOPARM,(R0),(RF)                                         
*                                                                               
TSAF08   MVI   TLKDIS,TLKNSEL      SET DEFAULT NOT YET SELECTED                 
*                                                                               
         LA    RF,FDLRECD+FDLFIRST                                              
         XR    RE,RE                                                            
         USING FDLELD,RF                                                        
TSAF10   CLI   FDLEL,0             EOR?                                         
         BE    EXITOK                                                           
         CLI   FDLEL,FDLELQ        COLUMN ELEMENT                               
         BNE   TSAF12                                                           
         CLC   TLKRNUM,FDLFNUM     MATCH NUMBER                                 
         BE    TSAF14                                                           
*                                                                               
TSAF12   IC    RE,FDLLN            ITERATE RECORD                               
         LA    RF,0(RE,RF)                                                      
         B     TSAF10                                                           
*                                                                               
TSAF14   MVC   TLKPOS,FDLCNUM      SET POSITION                                 
         MVI   TLKDIS,TLKSEL                                                    
         B     EXITOK                                                           
         DROP  R2,R3,RF                                                         
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
CNTRL    EQU   X'0A'                                                            
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
D_LEN    EQU   18                                                               
         EJECT                                                                  
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
OVERWRKD DSECT                                                                  
CALLR1   DS    A                                                                
SVPARMS  DS    0XL25                                                            
SVPARMS1 DS    A                                                                
SVPARMS2 DS    A                                                                
SVPARMS3 DS    A                                                                
SVPARMS4 DS    A                                                                
SVPARMS5 DS    A                                                                
SVPARMS6 DS    A                                                                
*                                                                               
THISMAX  DS    XL1                                                              
NUMLEN   DS    XL1                                                              
LASTFCR  DS    XL1                                                              
*        CTFILWORK                                                              
         PRINT OFF                                                              
       ++INCLUDE CTFILWORK                                                      
         PRINT ON                                                               
*        DDFLDHDR                                                               
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
*        FAFACTS                                                                
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
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
*                                                                               
TWAD     DSECT                                                                  
         ORG   TWSAVE                                                           
MYSAVE   DS    0X                                                               
*                                                                               
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKDIS   DS    XL1                                                              
TLKSEL   EQU   1                   COLUMN IS SELECTED                           
TLKNSEL  EQU   2                   COLUMN IS NOT SELECTED                       
TLKPOS   DS    XL1                 DISPLAY POSITION                             
TLKRNAM  DS    XL(D_LEN)           LIST COLUMN HEADINGS                         
TLKRNUM  DS    XL2                 FIELD NUMBER                                 
*                                                                               
         ORG   TLUSER                                                           
TLTYP    DS    XL1                                                              
TLOPEN   EQU   X'80'               COLUMN IS OPEN (UNPROTECTED)                 
TLCOPN   EQU   X'40'               COLUMN IS CAN BE OPENED                      
TLENQ    EQU   *-TLSTD                                                          
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002CTFIL16X  08/22/00'                                      
         END                                                                    
