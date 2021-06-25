*          DATA SET CTFIL15    AT LEVEL 012 AS OF 02/18/15                      
*PROCESS USING(WARN(15))                                                        
*&&      SET   NOP=N                                                            
*PHASE TA1315A                                                                  
FIL15    TITLE 'DISPLAY COLUMN RECORD OVERLAY'                                  
FIL15    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,CTFIL15*,R7,RR=RE                                              
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
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
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
         USING FCRRECD,R2                                                       
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
KFKVAL   XC    FCRKEY,FCRKEY       INITIALIZE KEY OF FIELD RECORD               
         MVI   FCRKMIN,FCRKMINQ    SET MINOR SYSTEM                             
         MVI   FCRKTYP,FCRKTYPQ    SET FIELD RECORD                             
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
KFKFVAL  XC    FCRKEY,FCRKEY       INITIALIZE KEY OF FIELD RECORD               
         MVI   FCRKMIN,FCRKMINQ    SET MINOR SYSTEM                             
         MVI   FCRKTYP,FCRKTYPQ    SET FIELD RECORD                             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* RECORD OBJECT                                                       *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(RECORD)                                                        *         
* P4 HOLDS SUB-ACTION VERB                                            *         
***********************************************************************         
         SPACE 1                                                                
RECORD   LM    R0,R3,SVPARMS                                                    
         USING FCRRECD,R2                                                       
         LA    RF,TABLREC                                                       
         B     ITER                ITERATE TABLE                                
*                                                                               
TABLREC  DC    AL1(EOT)                                                         
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
         USING FCRRECD,R2                                                       
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
         USING FCRRECD,R2          R2 HOLDS A(RECORD)                           
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
         DC    AL2(00086),AL4(PAGDTA)    PAGE                                   
         DC    AL2(00085),AL4(SRCDTA)    SUB-RECORD                             
         DC    AL2(00100),AL4(ACTDTA)    ACTION                                 
         DC    AL2(00084),AL4(FNTDTA)    FIELD TAG                              
         DC    AL2(00083),AL4(FFEDTA)    FIELD INPUT                            
         DC    AL2(00097),AL4(CDEDTA)    CODE                                   
         DC    AL2(00150),AL4(CTRDTA)     COUNTRY                               
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL15    CSECT                                                                  
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
         CLC   SYSLNUM,FCRKSYS     MATCH ON SYSTEM NUMBER                       
         BE    *+12                                                             
         LA    RE,SYSLLEN(RE)      NO - BUMP TO NEXT TABLE ENTRY                
         B     DSYS02                                                           
         SPACE 1                                                                
         MVC   FVIFLD(L'SYSLNAME),SYSLNAME                                      
         B     EXITOK                                                           
         DROP  RE                                                               
         SPACE 1                                                                
DSYS04   CURED FCRKSYS,(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                      
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
VSYS06   MVC   FCRKSYS,SYSLNUM     SYSTEM NUMBER                                
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
VFSYS06  MVC   FCRKSYS,SYSLNUM     SYSTEM NUMBER INTO KEY                       
         MVC   FLTIFLD(L'SYSLNUM),SYSLNUM    - AND FILTER                       
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* DO SYSTEM FILTERING                                                 *         
***********************************************************************         
         SPACE 1                                                                
DOFTSYS  CLC   FCRKSYS,FLTIFLD                                                  
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
DISPRG   OC    FCRKPRG,FCRKPRG     WAS A PROGRAM ENTERED?                       
         BZ    EXITOK              NO                                           
         SPACE 1                                                                
         L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FCRKSYS                                                  
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     DPRG08                                                           
         SPACE 1                                                                
         L     R1,SEPGMS                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1          FIND PGMLST ENTRY                            
DPRG02   CLC   PGMNUM,FCRKPRG                                                   
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
DPRG08   CURED FCRKPRG,(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                      
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PROGRAM FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
VALPRG   MVI   FCRKPRG,0           DEFAULT IS SYSTEM LEVEL ONLY                 
         CLI   FVILEN,0            TRYING TO SET TO SYSTEM LEVEL ONLY?          
         BE    EXITOK              YES                                          
*                                                                               
         TM    FVIIND,FVINUM                                                    
         BZ    VPRG01                                                           
         ICM   RF,15,BCFULL                                                     
         BZ    EXITNV                                                           
         CH    RF,=Y(255)                                                       
         BH    EXITNV                                                           
         STC   RF,FCRKPRG                                                       
         B     EXITOK                                                           
*                                                                               
VPRG01   L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FCRKSYS                                                  
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
VPRG08   MVC   FCRKPRG,PGMNUM      PROGRAM NUMBER                               
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
DFLTPRG  OC    FLTIFLD(L'FCRKPRG),FLTIFLD PROGRAM ENTERED?                      
         BZ    EXITOK              NO                                           
         SPACE 1                                                                
         L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FCRKSYS                                                  
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
VFLTPRG  MVI   FCRKPRG,0           DEFAULT IS SYSTEM LEVEL ONLY                 
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
         CLC   SEOVSYS,FCRKSYS                                                  
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
VFPRG08  MVC   FCRKPRG,PGMNUM      PROGRAM NUMBER                               
         MVC   FLTIFLD(L'PGMNUM),PGMNUM                                         
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR PROGRAM FIELD                                      *         
***********************************************************************         
DOFTPRG  CLC   FCRKPRG,FLTIFLD                                                  
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
DISRCO   OC    FCRKREC,FCRKREC     GLOBAL FIELD?                                
         BZ    EXITOK                                                           
         ICM   RF,8,FCRKSYS                                                     
         ICM   RF,4,FCRKPRG                                                     
         ICM   RF,2,FCRKREC                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXDIS,(RF)                                   
         BE    EXITOK                                                           
         DC    H'0'                                                             
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALRCO   MVI   FCRKREC,0           IF NO RECORD                                 
         CLI   FVILEN,0            FIELD SET AT LEVELS ABOVE                    
         BE    EXITOK                                                           
         SPACE 1                                                                
         OC    FCRKPRG,FCRKPRG     TRYING TO OVERRIDE RECORD - MUST             
         BZ    EXITNV              INPUT THE PROGRAM                            
         SPACE 1                                                                
         XR    RF,RF                                                            
         ICM   RF,8,FCRKSYS                                                     
         ICM   RF,4,FCRKPRG                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXVAL,(RF)                                   
         BNE   EXITL               NOT FOUND                                    
         L     RF,8(R1)                                                         
         STCM  RF,2,FCRKREC                                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD FILTER FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DFLTRCO  OC    FLTIFLD(L'FCRKREC),FLTIFLD RECORD FIELD?                         
         BZ    EXITOK                                                           
         ICM   RF,8,FCRKSYS                                                     
         ICM   RF,4,FCRKPRG                                                     
         ICM   RF,2,FLTIFLD                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXDIS,(RF)                                   
         BE    EXITOK                                                           
         DC    H'0'                                                             
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD FILTER FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
VFLTRCO  OC    FCRKPRG,FCRKPRG     TRYING TO FILTER RECORD - MUST               
         BZ    EXITNV              INPUT THE PROGRAM                            
         SPACE 1                                                                
         XR    RF,RF                                                            
         ICM   RF,8,FCRKSYS                                                     
         ICM   RF,4,FCRKPRG                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXVAL,(RF)                                   
         BNE   EXITL               NOT FOUND                                    
         L     RF,8(R1)                                                         
         STCM  RF,2,FCRKREC                                                     
         STCM  RF,2,FLTIFLD                                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR RECORD                                             *         
***********************************************************************         
         SPACE 1                                                                
DOFTRCO  CLC   FCRKREC,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SUB-RECORD                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
SRCDTA   LA    RF,SRCTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SRCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSRC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSRC)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTSRC)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTSRC)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTSRC)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A SUB-RECORD FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
DISSRC   OC    FCRKSREC,FCRKSREC   GLOBAL FIELD?                                
         BZ    EXITOK                                                           
         ICM   RF,8,FCRKSYS                                                     
         ICM   RF,4,FCRKPRG                                                     
         ICM   RF,2,FCRKSREC                                                    
         GOTO1 AGEN,BOPARM,ORTYPE,RTXDIS,(RF)                                   
         BE    EXITOK                                                           
         DC    H'0'                                                             
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A SUB-RECORD FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
VALSRC   MVI   FCRKSREC,0          SUB-RECORD FIELD IS OPTIONAL                 
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         SPACE 1                                                                
         XR    RF,RF                                                            
         ICM   RF,8,FCRKSYS                                                     
         ICM   RF,4,FCRKPRG                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXVAL,(RF)                                   
         BNE   EXITL               NOT FOUND                                    
         L     RF,8(R1)                                                         
         STCM  RF,2,FCRKSREC                                                    
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A SUB-RECORD FILTER FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
DFLTSRC  OC    FLTIFLD(L'FCRKREC),FLTIFLD SUB-RECORD FIELD?                     
         BZ    EXITOK                                                           
         ICM   RF,8,FCRKSYS                                                     
         ICM   RF,4,FCRKPRG                                                     
         ICM   RF,2,FLTIFLD                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXDIS,(RF)                                   
         BE    EXITOK                                                           
         DC    H'0'                                                             
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A SUB-RECORD FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
VFLTSRC  XR    RF,RF                                                            
         ICM   RF,8,FCRKSYS                                                     
         ICM   RF,4,FCRKPRG                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXVAL,(RF)                                   
         BNE   EXITL               NOT FOUND                                    
         L     RF,8(R1)                                                         
         STCM  RF,2,FCRKSREC                                                    
         STCM  RF,2,FLTIFLD                                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR SUB-RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
DOFTSRC  CLC   FCRKSREC,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACTION                                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
ACTDTA   LA    RF,ACTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
ACTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISACT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALACT)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTACT)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTACT)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTACT)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACTION NAME                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISACT   OC    FCRKACT,FCRKACT     ACTION NUMBER                                
         BZ    EXITOK                                                           
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,8,FCRKSYS                                                     
         ICM   RF,4,FCRKPRG                                                     
         ICM   RF,2,FCRKREC                                                     
         ICM   RF,1,FCRKACT                                                     
         GOTO1 AGEN,BOPARM,OACT,AXDIS,(RF)                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACTION NAME                                                *         
***********************************************************************         
         SPACE 1                                                                
VALACT   XR    RF,RF                                                            
         ICM   RF,8,FCRKSYS                                                     
         ICM   RF,4,FCRKPRG                                                     
         ICM   RF,2,FCRKREC                                                     
         GOTO1 AGEN,BOPARM,OACT,AXVAL,(RF)                                      
         BNE   EXITL               NOT FOUND                                    
         L     RF,8(R1)                                                         
         STCM  RF,1,FCRKACT                                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACTION FILTER FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DFLTACT  OC    FLTIFLD(L'FCRKACT),FLTIFLD RECORD FIELD?                         
         BZ    EXITOK                                                           
         XR    RF,RF                                                            
         ICM   RF,8,FCRKSYS                                                     
         ICM   RF,4,FCRKPRG                                                     
         ICM   RF,2,FCRKREC                                                     
         ICM   RF,1,FLTIFLD                                                     
         GOTO1 AGEN,BOPARM,OACT,AXDIS,(RF)                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACTION FIELD FOR FILTERING                                 *         
***********************************************************************         
         SPACE 1                                                                
VFLTACT  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         XR    RF,RF                                                            
         ICM   RF,8,FCRKSYS                                                     
         ICM   RF,4,FCRKPRG                                                     
         ICM   RF,2,FCRKREC                                                     
         GOTO1 AGEN,BOPARM,OACT,AXVAL,(RF)                                      
         BNE   EXITL               NOT FOUND                                    
         L     RF,8(R1)                                                         
         STCM  RF,1,FCRKACT                                                     
         STCM  RF,1,FLTIFLD                                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR ACTION                                             *         
***********************************************************************         
         SPACE 1                                                                
DOFTACT  CLC   FCRKACT,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PAGE                                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
PAGDTA   LA    RF,PAGTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
PAGTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPAG)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPAG)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTPAG)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTPAG)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTPAG)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACTION NAME                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISPAG   XR    RF,RF                                                            
         ICM   RF,1,FCRKPAGE                                                    
         CURED (RF),(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE PAGE NUMBER                                                *         
***********************************************************************         
         SPACE 1                                                                
VALPAG   TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
         L     RF,BCFULL                                                        
         CH    RF,=H'240'                                                       
         BH    EXITNV                                                           
         STC   RF,FCRKPAGE                                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY PAGE NUMBER FILTER FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
DFLTPAG  XR    RF,RF                                                            
         IC    RF,FLTIFLD                                                       
         CURED (RF),(3,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE PAGE NUMBER FIELD FOR FILTERING                            *         
***********************************************************************         
         SPACE 1                                                                
VFLTPAG  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
         L     RF,BCFULL                                                        
         CH    RF,=H'240'                                                       
         BH    EXITNV                                                           
         STC   RF,FLTIFLD                                                       
         STC   RF,FCRKPAGE                                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR PFKEY NUMBER                                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTPAG  CLC   FCRKPAGE,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR INTERNAL CODE                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
CDEDTA   LA    RF,CDETBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
CDETBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCDE)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCDE)                                 
         DC    AL1(DHED),AL1(0,0,0),AL4(HEDCDE)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTCDE)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTCDE)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTCDE)                                
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISCDE)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A CODE FIELD                                                *         
***********************************************************************         
         SPACE 1                                                                
DISCDE   MVC   FVIFLD(L'FCRKTYPE),FCRKTYPE                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A CODE FIELD                                               *         
***********************************************************************         
         SPACE 1                                                                
VALCDE   MVI   FCRKTYPE,0          IF NO CODE, DEFAULT IS ZERO                  
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVC   FCRKTYPE,FVIFLD                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* CREATE A CODE HEADLINE FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
HEDCDE   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A CODE FILTER FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DFLTCDE  OC    FLTIFLD(L'FCRKTYPE),FLTIFLD CODE FIELD?                          
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'FCRKTYPE),FCRKTYPE                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A CODE FILTER FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VFLTCDE  MVI   FCRKTYPE,0          IF NO CODE SET ZERO                          
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVC   FCRKTYPE,FVIFLD                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR CODE                                               *         
***********************************************************************         
         SPACE 1                                                                
DOFTCDE  CLC   FCRKTYPE,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COUNTRY FILTER                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
CTRDTA   LA    RF,CTRTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
CTRTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCTR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCTR)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTCTR)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTCTR)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTCTR)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A COUNTRY FILTER                                            *         
***********************************************************************         
         SPACE 1                                                                
DISCTR   CLI   FCRKCTRY,0          FOR THOSE THERE ALREADY                      
         BE    EXITOK                                                           
         MVC   BOBYTE1,FCRKCTRY    COUNTRY CODE IS 1`S COMP IN KEY              
         XC    BOBYTE1,BCEFFS                                                   
         LA    R1,CTRYTAB                                                       
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING CTRYTABD,R1                                                      
*                                                                               
DCTR02   CLC   CTRYCODE,BOBYTE1    MATCH ON CODE                                
         BNE   *+12                                                             
         TM    CTRYTYPE,CTRYTYCO   MAKE SURE IT IS A COUNTRY                    
         BO    DCTR04                                                           
         BXLE  R1,RE,DCTR02                                                     
         DC    H'0'                COUNTRY CODE IN KEY IS INVALID               
*                                                                               
DCTR04   MVC   FVIFLD(L'CTRYNAM),CTRYNAM   MOVE IN UK NAME FOR CTRY             
         CLI   CUCTRY,CTRYHOMQ                                                  
         BE    EXITOK                                                           
         MVC   FVIFLD(L'CTRYNAMN),CTRYNAMN MOVE IN NATIVE NAME FOR CTRY         
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A COUNTRY FILTER                                           *         
***********************************************************************         
         SPACE 1                                                                
VALCTR   CLI   FVILEN,0            ANY INPUT                                    
         BNE   VCTR01                                                           
         MVC   FCRKCTRY,BCEFFS     SET DEFAULT TO NATIVE COUNTRY                
         B     EXITOK                                                           
*                                                                               
VCTR01   XR    R4,R4                                                            
         IC    R4,FVXLEN                                                        
         LA    R1,CTRYTAB                                                       
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         A     RF,BORELO                                                        
         LA    R1,6(R1)                                                         
         USING CTRYTABD,R1                                                      
*                                                                               
VCTR02   TM    CTRYTYPE,CTRYTYCO   MAKE SURE IT IS A COUNTRY                    
         BZ    VCTR04                                                           
*                                                                               
         CLC   FVIFLD(0),CTRYNAM   FULL UK NAME FOR CTRY                        
         EX    R4,*+8                                                           
         BE    VCTR06                                                           
         CLC   FVIFLD(0),CTRYNAMN  FULL NATIVE NAME FOR CTRY                    
*                                                                               
         CLI   FVILEN,3            LENGTH OK FOR SHORT NAME?                    
         BH    VCTR04                                                           
*                                                                               
         EX    R4,*+8                                                           
         BE    VCTR06                                                           
         CLC   FVIFLD(0),CTRYSHR   SHORT UK NAME FOR CTRY                       
         EX    R4,*+8                                                           
         BE    VCTR06                                                           
         CLC   FVIFLD(0),CTRYSHRN  SHORT NATIVE NAME FOR COUNTRY                
         EX    R4,*+8                                                           
         BE    VCTR06                                                           
*                                                                               
VCTR04   BXLE  R1,RE,VCTR02                                                     
         B     EXITNV              COUNTRY CODE IN KEY IS INVALID               
*                                                                               
VCTR06   MVC   FCRKCTRY,CTRYCODE   MOVE IN COUNTRY CODE                         
         XC    FCRKCTRY,BCEFFS     1`S COMPLIMENT IT                            
         MVC   CTRYCDE,FCRKCTRY                                                 
                                                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A COUNTRY FILTER FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
DFLTCTR  LA    R1,CTRYTAB                                                       
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING CTRYTABD,R1                                                      
DFCTR01  CLC   CTRYCODE,FLTIFLD    MATCH ON CODE                                
         BNE   *+12                                                             
         TM    CTRYTYPE,CTRYTYCO   MAKE SURE IT IS A COUNTRY                    
         BO    DFCTR02                                                          
         BXLE  R1,RE,DFCTR01                                                    
         DC    H'0'                COUNTRY CODE IN KEY IS INVALID               
*                                                                               
DFCTR02  MVC   FVIFLD(L'CTRYNAM),CTRYNAM   MOVE IN UK NAME FOR CTRY             
         CLI   CUCTRY,CTRYHOMQ                                                  
         BE    EXITOK                                                           
         MVC   FVIFLD(L'CTRYNAMN),CTRYNAMN MOVE IN NATIVE NAME FOR CTRY         
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A COUNTRY FILTER FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
VFLTCTR  CLI   FVILEN,0            ANY INPUT                                    
         BNE   *+12                                                             
         MVI   FCRKCTRY,FF         SET DEFAULT TO NATIVE COUNTRY                
         B     EXITOK                                                           
*                                                                               
         XR    R4,R4                                                            
         IC    R4,FVXLEN                                                        
         LA    R1,CTRYTAB                                                       
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING CTRYTABD,R1                                                      
*                                                                               
VFCTR02  TM    CTRYTYPE,CTRYTYCO   MAKE SURE IT IS A COUNTRY                    
         BZ    VFCTR04                                                          
*                                                                               
         CLC   FVIFLD(0),CTRYNAM   FULL UK NAME FOR CTRY                        
         EX    R4,*+8                                                           
         BE    VFCTR06                                                          
         CLC   FVIFLD(0),CTRYNAMN  FULL NATIVE NAME FOR CTRY                    
*                                                                               
         CLI   FVILEN,3            LENGTH OK FOR SHORT NAME?                    
         BH    VFCTR04                                                          
         EX    R4,*+8                                                           
         BE    VFCTR06                                                          
         CLC   FVIFLD(0),CTRYSHR   SHORT UK NAME FOR CTRY                       
         EX    R4,*+8                                                           
         BE    VFCTR06                                                          
         CLC   FVIFLD(0),CTRYSHRN  SHORT NATIVE NAME FOR COUNTRY                
         EX    R4,*+8                                                           
         BE    VFCTR06                                                          
*                                                                               
VFCTR04  BXLE  R1,RE,VFCTR02                                                    
         B     EXITNV              COUNTRY CODE IN KEY IS INVALID               
*                                                                               
VFCTR06  MVC   FCRKCTRY,CTRYCODE   MOVE IN COUNTRY CODE                         
         MVC   FLTIFLD(L'CTRYCODE),CTRYCODE                                     
         XC    FCRKCTRY,BCEFFS     1`S COMPLIMENT CODE IN KEY                   
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* DO COUNTRY FILTERING                                                *         
***********************************************************************         
         SPACE 1                                                                
DOFTCTR  MVC   BOBYTE1,FVIFLD                                                   
         XC    BOBYTE1,BCEFFS                                                   
         CLC   FCRKCTRY,BOBYTE1                                                 
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
*        DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFNT)                               
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
DISFFE   CLI   TLKDIS,TLKNSEL      BEEN ASSIGNED YET?                           
         BE    EXITOK              NO                                           
*                                                                               
         CLI   TLKDIS,TLKFIX       FIXED?                                       
         BNE   *+12                NO                                           
         MVI   FVIFLD,C'F'                                                      
         B     DFFE02                                                           
         CLI   TLKDIS,TLKVAR       VARIABLE?                                    
         BNE   *+12                NO                                           
         MVI   FVIFLD,C'V'                                                      
         B     DFFE02                                                           
         CLI   TLKDIS,TLKVAL       OK TO INCLUDE?                               
         BNE   *+12                NO                                           
         MVI   FVIFLD,C'I'                                                      
         B     DFFE02                                                           
         DC    H'0'               TSAR RECORD FUCKED UP                         
*                                                                               
DFFE02   LA    R3,FVIFLD+1                                                      
         TM    TLTYP,TLNEWL                                                     
         BZ    DFFE04                                                           
         MVI   FVIFLD+1,C'.'                                                    
         LA    R3,FVIFLD+2                                                      
*                                                                               
DFFE04   XR    RF,RF                                                            
         IC    RF,TLKPOS                                                        
         CURED (RF),(3,(R3)),0,DMCB=BOPARM,ALIGN=LEFT                           
         LA    RF,FVIFLD+5                                                      
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         TM    TLTYP,TLOPEN        OPEN AS DEFAULT?                             
         BZ    *+12                NO                                           
         MVI   1(RF),C'&&'         FLAG AS OPEN FIELD.                          
         B     EXITOK                                                           
*                                                                               
         TM    TLTYP,TLCOPN        CAN BE OPENED?                               
         BZ    EXITOK              YES                                          
         MVI   1(RF),C'*'          FLAG AS OPENABLE FIELD                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER FIELD VALUES                                        *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING FCRELD,BOELEM                                                    
VALFFE   L     R3,AIOREC           A(THIS TSAR RECORD)                          
         USING FCRRECD,R3                                                       
*                                                                               
         XR    RE,RE                                                            
         LA    RF,FCRRECD+FCRFIRST                                              
X        USING FCRELD,RF                                                        
VFFE02   CLI   X.FCREL,0           EOR?                                         
         BE    VFFE08                                                           
         CLC   TLKRNUM,X.FCRFNUM   MATCH FIELD NUMBER                           
         BE    VFFE06                                                           
*                                                                               
VFFE04   IC    RE,X.FCRLN          ITERATE RECORD                               
         LA    RF,0(RE,RF)                                                      
         B     VFFE02                                                           
*                                                                               
VFFE06   MVI   X.FCREL,FF          SET ELEMENT CODE DELETABLE                   
         B     VFFE04                                                           
         DROP  X                                                                
*                                                                               
VFFE08   GOTO1 ADELEL,BOPARM,(=AL1(FF),FCRRECD),0                               
         CLI   FVILEN,0            CLEARING THIS FIELD?                         
         BE    VFFE31                                                           
*                                                                               
         XC    BOELEM(FCRLNQ),BOELEM                                            
         MVI   FCREL,FCRELQ        BUILD DUMMY FCREL                            
         MVI   FCRLN,FCRLNQ                                                     
         MVC   FCRFNUM,TLKRNUM                                                  
*                                                                               
         USING DCTABD,R4                                                        
VFFE10   TM    FVIIND,FVINUM       IF ONLY NUMBER INPUT, THEN                   
         BZ    VFFE12              DEFAULT IS VARIABLE & CLOSED                 
         MVI   FCRTYPE,FCRTVAR                                                  
         MVI   THISMAX,VARMAX                                                   
         ICM   RF,15,BCFULL                                                     
         BZ    EXITNV                                                           
         B     VFFE22                                                           
*                                                                               
VFFE12   XR    R4,R4               CLEAR R4                                     
         CLI   FVIFLD,C'F'         FIXED?                                       
         BNE   *+16                NO                                           
         MVI   FCRTYPE,FCRTFIX                                                  
         MVI   THISMAX,FIXMAX                                                   
         B     VFFE14                                                           
*                                                                               
         CLI   FVIFLD,C'V'         VARIABLE?                                    
         BNE   *+16                NO                                           
         MVI   FCRTYPE,FCRTVAR                                                  
         MVI   THISMAX,VARMAX                                                   
         B     VFFE14                                                           
*                                                                               
         CLI   FVIFLD,C'I'         CAN BE DISPLAYED?                            
         BNE   *+16                NO                                           
         MVI   FCRTYPE,FCRTVOK                                                  
         MVI   THISMAX,VARMAX                                                   
         B     VFFE14                                                           
*                                                                               
         B     EXITNV                                                           
*                                                                               
VFFE14   XR    R1,R1                                                            
         IC    R1,FVXLEN           ACTUAL INPUT LENGTH-1                        
         LA    RF,FVIFLD(R1)                                                    
         CLI   0(RF),C'&&'         IS FIELD SUPPOSED TO BE OPEN?                
         BNE   VFFE15              NO                                           
         MVI   FCRCIND,DCTIOPEN+DCTICOPN                                        
         SH    R1,=H'1'                                                         
         BM    EXITNV              R1 CONTAINS LENGTH OF 'NUMBER'               
         BCTR  RF,0                                                             
*                                                                               
VFFE15   CLI   0(RF),C'*'          IS FIELD ABLE TO BE OPENED?                  
         BNE   *+16                NO                                           
         OI    FCRCIND,DCTICOPN                                                 
         SH    R1,=H'1'                                                         
         BM    EXITNV              R1 CONTAINS LENGTH OF 'NUMBER'               
*                                                                               
         STC   R1,NUMLEN           SAVE IT                                      
         LR    R0,R1                                                            
         LA    RF,FVIFLD+1         FIRST BYTE IS FIXED/VARIABLE                 
*                                                                               
         CLI   0(RF),C'.'          NEW LINE START HERE?                         
         BNE   VFFE16                                                           
         OI    FCRCIND,DCTINEWL                                                 
         SH    R1,=H'1'                                                         
         BM    EXITNV              R1 CONTAINS LENGTH OF 'NUMBER'               
*                                                                               
         STC   R1,NUMLEN           SAVE IT                                      
         LR    R0,R1                                                            
         LA    RF,1(RF)            FIRST BYTE IS F/V, SECOND IS .               
*                                                                               
VFFE16   LR    RE,RF               SAVE START OF NUMERIC PORTION                
*                                                                               
VFFE17   CLI   0(RE),C'0'          MAKE SURE THIS IS ALL NUMERIC                
         BL    VFFE18                                                           
         CLI   0(RE),C'9'                                                       
         BH    VFFE18                                                           
         LA    RE,1(RE)                                                         
         BCT   R0,VFFE17                                                        
         B     VFFE20                                                           
*                                                                               
VFFE18   LA    R1,FVIFLD                                                        
         SR    RE,R1                                                            
         STC   RE,FVERRNDX                                                      
         B     EXITNOTN                                                         
*                                                                               
VFFE20   BCTR  R1,0                GET BINARY EQUIVALENT OF NUMBER              
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  BODUB1,0(0,RF)                                                   
         CVB   RF,BODUB1           RF=NUMBER                                    
         LTR   RF,RF                                                            
         BZ    EXITNV              ZERO NOT VALID                               
*                                                                               
VFFE22   XR    R1,R1               SEE IF # FITS IN THIS DISPLAY TYPE           
         IC    R1,THISMAX                                                       
         CR    RF,R1               IS IT TOO BIG?                               
         BH    EXITNV              YES                                          
*                                                                               
         STC   RF,FCRCNUM          SAVE IT                                      
         OC    FCRTYPE,FCRTYPE     MAKE SURE COLUMN TYPE SET                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R4,FCRRECD+FCRFIRST START OF RECORD                              
X        USING FCRELD,R4                                                        
VFFE24   CLI   X.FCREL,0                                                        
         BE    VFFE30                                                           
         CLI   X.FCREL,FCRELQ                                                   
         BNE   VFFE26                                                           
         CLC   X.FCRTYPE,FCRTYPE                                                
         BE    VFFE28                                                           
*                                                                               
VFFE26   XR    RF,RF                                                            
         IC    RF,X.FCRLN                                                       
         LA    R4,0(RF,R4)                                                      
         B     VFFE24                                                           
*                                                                               
VFFE28   CLC   X.FCRCNUM,FCRCNUM   ??? THE POSITION TO GO INTO ???              
         BL    VFFE26                                                           
         IC    RF,X.FCRCNUM        BUMP THIS COLUMN NUMBER BY 1                 
         LA    RF,1(RF)                                                         
         STC   RF,X.FCRCNUM        SAVE IT                                      
         CLM   RF,1,THISMAX        ALLOWED?                                     
         BNH   *+8                                                              
         MVI   X.FCREL,FF          MARK FOR DELETION                            
         B     VFFE26                                                           
*                                                                               
VFFE30   GOTO1 ADELEL,BOPARM,(=AL1(FF),FCRRECD),0                               
         GOTO1 AADDEL,BOPARM,FCRRECD                                            
*                                                                               
VFFE31   XR    RE,RE               MAKE SURE NUMBERS ARE SEQUENTIAL             
         LA    RF,FCRRECD+FCRFIRST                                              
         MVI   LASTFCR,0                                                        
X        USING FCRELD,RF                                                        
VFFE32   CLI   X.FCREL,0           EOR?                                         
         BE    VFFE34                                                           
         CLC   X.FCRTYPE,LASTFCR                                                
         BE    *+8                                                              
         LA    R1,1                                                             
         MVC   LASTFCR,X.FCRTYPE   SAVE THIS TYPE                               
         STCM  R1,1,X.FCRCNUM      SAVE COLUMN NUMBER                           
         LA    R1,1(R1)            BUMP COUNT                                   
*                                                                               
         IC    RE,X.FCRLN          ITERATE RECORD                               
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
         USING FCRRECD,R2                                                       
         USING FESD,R3                                                          
         LA    RF,NSSTABL                                                       
         B     ITER                                                             
*                                                                               
NSSTABL  DC    AL1(SNTRIN),AL1(0,0,0),AL4(NTRIN)                                
         DC    AL1(SXITOUT),AL1(0,0,0),AL4(XITOUT)                              
         DC    AL1(SXITIN),AL1(0,0,0),AL4(XITIN)                                
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
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER (ENTER IN)               *         
***********************************************************************         
         SPACE 1                                                                
*ITIN    NI    NSSAV+(SNINDS1-SSAVD),FF-SNIUSECR                                
XITIN    B     EXITOK                                                           
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
THIS     USING FCRRECD,R2                                                       
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
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LLSTFRST),AL1(0,0,0),AL4(FTFLST)                             
         DC    AL1(LINIT),AL1(0,0,0),AL4(ILST)                                  
*                                                                               
         DC    AL1(LGETFRST),AL1(0,0,1),AL4(BLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,1),AL4(BLST1)                              
         DC    AL1(LINIT),AL1(0,0,1),AL4(ILST1)                                 
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
* FIRST TIME FOR LIST BUILD                                           *         
***********************************************************************         
         SPACE 1                                                                
FTFLST   B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
FLST     MVC   IOKEY(L'FCRKEY),THIS.FCRRECD                                     
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
NLST02   CLC   IOKEY(FCRKSYS-FCRRECD),THIS.FCRRECD                              
         BNE   EXITL                                                            
*                                                                               
         ICM   R1,15,SVPARMS5                                                   
         A     R1,=A(XOGENFIL+XOGET)                                            
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   THIS.FCRKEY(FCRKLEN),IOKEY   WE WANT THIS KEY HERE...            
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
FTFLST1  XC    THIS.FDRKEY,THIS.FDRKEY                                          
         L     R4,AIOREC                                                        
X        USING FCRRECD,R4                                                       
S        USING FSRRECD,IOKEY                                                    
*                                                                               
         OC    ASTEST,ASTEST       TEST VERSION?                                
         BZ    XPX02               NO                                           
*                                                                               
         XC    S.FSRKEY,S.FSRKEY   TEST SCREEN FOR DEFINED COUNTRY?             
         MVI   S.FSRKMIN,FSRKMINQ                                               
         MVI   S.FSRKTYP,FSRKTYPQ                                               
         MVC   S.FSRKSYS,X.FCRKSYS                                              
         MVC   S.FSRKPRG,X.FCRKPRG                                              
         MVC   S.FSRKREC,X.FCRKREC                                              
         MVC   S.FSRKPAGE,X.FCRKPAGE                                            
         CLI   S.FSRKPAGE,0                                                     
         BNE   *+8                                                              
         MVI   S.FSRKPAGE,FSRKPLST SET LIST SCREEN                              
         MVC   S.FSRKCTRY,X.FCRKCTRY                                            
         MVI   S.FSRKSUB,FF                                                     
         MVC   S.FSRKTEST,ASTEST                                                
*                                                                               
         L     R1,=A(XOGENDIR+XOREAD+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BE    FTFL106                                                          
*                                                                               
XPX02    XC    S.FSRKEY,S.FSRKEY   LIVE SCREEN FOR DEFINED COUNTRY?             
         MVI   S.FSRKMIN,FSRKMINQ                                               
         MVI   S.FSRKTYP,FSRKTYPQ                                               
         MVC   S.FSRKSYS,X.FCRKSYS                                              
         MVC   S.FSRKPRG,X.FCRKPRG                                              
         MVC   S.FSRKREC,X.FCRKREC                                              
         MVC   S.FSRKPAGE,X.FCRKPAGE                                            
         CLI   S.FSRKPAGE,0                                                     
         BNE   *+8                                                              
         MVI   S.FSRKPAGE,FSRKPLST SET LIST SCREEN                              
         MVI   S.FSRKCTRY,FF                                                    
         MVI   S.FSRKSUB,FF                                                     
         MVC   S.FSRKTEST,ASTEST                                                
*                                                                               
         L     R1,=A(XOGENDIR+XOREAD+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BE    FTFL106                                                          
*                                                                               
         OC    ASTEST,ASTEST       TEST VERSION?                                
         BZ    XPX04               NO                                           
*                                                                               
         XC    S.FSRKEY,S.FSRKEY   TEST SCREEN FOR HOST PROCESSOR?              
         MVI   S.FSRKMIN,FSRKMINQ                                               
         MVI   S.FSRKTYP,FSRKTYPQ                                               
         MVC   S.FSRKSYS,X.FCRKSYS                                              
         MVC   S.FSRKPRG,X.FCRKPRG                                              
         MVC   S.FSRKREC,X.FCRKREC                                              
         MVC   S.FSRKPAGE,X.FCRKPAGE                                            
         CLI   S.FSRKPAGE,0                                                     
         BNE   *+8                                                              
         MVI   S.FSRKPAGE,FSRKPLST SET LIST SCREEN                              
         MVI   S.FSRKCTRY,FF                                                    
         MVI   S.FSRKSUB,FF                                                     
         MVC   S.FSRKTEST,ASTEST                                                
*                                                                               
         L     R1,=A(XOGENDIR+XOREAD+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BE    FTFL106                                                          
*                                                                               
XPX04    XC    S.FSRKEY,S.FSRKEY   LIVE SCREEN FOR HOST PROCESSOR?              
         MVI   S.FSRKMIN,FSRKMINQ                                               
         MVI   S.FSRKTYP,FSRKTYPQ                                               
         MVC   S.FSRKSYS,X.FCRKSYS                                              
         MVC   S.FSRKPRG,X.FCRKPRG                                              
         MVC   S.FSRKREC,X.FCRKREC                                              
         MVC   S.FSRKPAGE,X.FCRKPAGE                                            
         CLI   S.FSRKPAGE,0                                                     
         BNE   *+8                                                              
         MVI   S.FSRKPAGE,FSRKPLST SET LIST SCREEN                              
         MVI   S.FSRKCTRY,FF                                                    
         MVI   S.FSRKSUB,FF                                                     
         MVC   S.FSRKTEST,ASTEST                                                
*                                                                               
         L     R1,=A(XOGENDIR+XOREAD+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BE    FTFL106                                                          
         B     FTFL108                NO SCREEN RECORD FOR LIST                 
*                                                                               
FTFL106  L     R1,=A(XOGENFIL+XOGET+XIO1)                                       
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,AIO1                                                          
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('SPRELQ',(RF)),0                  
         CLI   12(R1),0                                                         
         BNE   FTFL108                THIS SCREEN IS NOT A SHADOW               
         L     RF,12(R1)                                                        
         MVC   THIS.FDRKPRG,SPRPRG-SPRELD(RF)                                   
         MVC   THIS.FDRKREC,SPRREC-SPRELD(RF)                                   
*                                                                               
FTFL108  MVI   THIS.FDRKMIN,FDRKMINQ  BUILD FIELD RECORD KEY TO FILTER          
         MVI   THIS.FDRKTYP,FDRKTYPQ                                            
         MVC   THIS.FDRKSYS,X.FCRKSYS                                           
         CLI   THIS.FDRKPRG,0         PROGRAM SET FROM SPREL?                   
         BNE   *+10                                                             
         MVC   THIS.FDRKPRG,X.FCRKPRG                                           
         CLI   THIS.FDRKREC,0         RECORD SET FROM SPREL?                    
         BNE   *+10                                                             
         MVC   THIS.FDRKREC,X.FCRKREC                                           
         B     EXITOK                                                           
         DROP  X,S                                                              
         SPACE 2                                                                
***********************************************************************         
* BUILD LIST PAGE 1                                                   *         
***********************************************************************         
         SPACE 1                                                                
K        USING FDRRECD,IOKEY                                                    
BLST1    MVC   K.FDRKEY,THIS.FDRKEY                                             
         XR    RF,RF                                                            
         ICM   RF,3,K.FDRKNUM                                                   
         LA    RF,1(RF)                                                         
         STCM  RF,3,K.FDRKNUM                                                   
*                                                                               
         ICM   R1,15,SVPARMS5                                                   
         A     R1,=A(XOGENDIR+XOHIGH)                                           
         GOTOX ('XIO',AGROUTS)                                                  
         CLC   K.FDRKEY(FDRKNUM-FDRRECD),THIS.FDRRECD                           
         BNE   EXITL               GONE PAST FDRRECS FOR THIS LIST              
*                                                                               
         MVC   THIS.FDRKEY,K.FDRKEY                                             
         MVC   K.FDRKCTRY,CTRYCDE                                               
         MVI   K.FDRKSUB,FF                                                     
         MVC   K.FDRKTEST,ASTEST                                                
         ICM   R1,15,SVPARMS5                                                   
         A     R1,=A(XOGENDIR+XOREAD)                                           
         GOTOX ('XIO',AGROUTS)                                                  
         BE    BLST102             RECORD EXISTS FOR DESIRED COUNTRY            
*                                                                               
         MVC   THIS.FDRKEY,K.FDRKEY                                             
         MVI   K.FDRKCTRY,FF                                                    
         MVI   K.FDRKSUB,FF                                                     
         MVC   K.FDRKTEST,ASTEST                                                
         ICM   R1,15,SVPARMS5                                                   
         A     R1,=A(XOGENDIR+XOREAD)                                           
         GOTOX ('XIO',AGROUTS)                                                  
         BE    BLST102             RECORD EXISTS FOR HOST PROCESSOR             
         B     BLST1                                                            
*                                                                               
BLST102  ICM   R1,15,SVPARMS5                                                   
         A     R1,=A(XOGENFIL+XOGET)                                            
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                GETREC ERROR                                 
*                                                                               
         L     RF,SVPARMS5         GET A(RECORD)                                
         SRL   RF,4-2                                                           
         L     RF,AIO1-L'AIO1(RF)                                               
*                                                                               
         GOTOX VHELLO,BOPARM,(C'G',GCFILNAM),('FDRELQ',(RF)),0                  
         CLI   12(R1),0                                                         
         BNE   BLST1                                                            
*                                                                               
         L     R4,12(R1)                                                        
         USING FDRELD,R4                                                        
         CLI   FDRLHLEN,0          LIST HEADING WIDTH IS 0?                     
         BE    BLST1                                                            
         CLI   FDRLCLEN,0          LIST DATA WIDTH IS 0?                        
         BE    BLST1                                                            
*                                                                               
         TM    FDRINDS1,FDR1DDS                                                 
         BZ    *+12                                                             
         TM    CUSTAT,CUSDDS       TEST DDS TERMINAL IF DDS ONLY FIELD          
         BZ    BLST1                                                            
*                                                                               
         MVC   THIS.FDRKEY(FDRKLEN),IOKEY   WE WANT THIS KEY HERE...            
         B     EXITOK                                                           
         DROP  R3,THIS,R4                                                       
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
         USING FCRRECD,R5                                                       
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
TSAF08   MVI   TLKDIS,TLKNSEL      SET DEFAULT NOT YET SELECTED FOR DIS         
*                                                                               
         LA    RF,FCRRECD+FCRFIRST                                              
         XR    RE,RE                                                            
         USING FCRELD,RF                                                        
TSAF10   CLI   FCREL,0             EOR?                                         
         BE    EXITOK                                                           
         CLI   FCREL,FCRELQ        COLUMN ELEMENT                               
         BNE   TSAF12                                                           
         CLC   TLKRNUM,FCRFNUM     MATCH NUMBER                                 
         BE    TSAF14                                                           
*                                                                               
TSAF12   IC    RE,FCRLN            ITERATE RECORD                               
         LA    RF,0(RE,RF)                                                      
         B     TSAF10                                                           
*                                                                               
TSAF14   MVC   TLKPOS,FCRCNUM      SET POSITION                                 
         CLI   FCRTYPE,FCRTFIX     SET TYPE - FIXED?                            
         BNE   *+8                                                              
         MVI   TLKDIS,TLKFIX                                                    
         CLI   FCRTYPE,FCRTVAR              - VARIABLE                          
         BNE   *+8                                                              
         MVI   TLKDIS,TLKVAR                                                    
         CLI   FCRTYPE,FCRTVOK              - OK, BUT NOT ASSIGNED              
         BNE   *+8                                                              
         MVI   TLKDIS,TLKVAL                                                    
*                                                                               
         TM    FCRCIND,DCTIOPEN    SET OPEN OR CLOSED                           
         BZ    *+8                                                              
         OI    TLTYP,TLOPEN                                                     
         TM    FCRCIND,DCTICOPN    SET CAN BE OPENED                            
         BZ    *+8                                                              
         OI    TLTYP,TLCOPN                                                     
         TM    FCRCIND,DCTINEWL    SET NEW LINE ON THIS FIELD                   
         BZ    *+8                                                              
         OI    TLTYP,TLNEWL                                                     
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
*                                                                               
       ++INCLUDE FACTRYTAB                                                      
*                                                                               
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
CTRYCDE  DS    XL1                                                              
         EJECT                                                                  
*        CTFILWORK                                                              
         PRINT OFF                                                              
       ++INCLUDE CTFILWORK                                                      
         PRINT ON                                                               
*        DDFLDHDR                                                               
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
*        FASYSLSTD                                                              
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
*        DDDDEQUS                                                               
         PRINT OFF                                                              
       ++INCLUDE DDDDEQUS                                                       
         PRINT ON                                                               
*        FACTRYEQUS                                                             
         PRINT OFF                                                              
       ++INCLUDE FACTRYEQUS                                                     
         PRINT ON                                                               
*        FACTRY                                                                 
         PRINT OFF                                                              
       ++INCLUDE FACTRY                                                         
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
TLKFIX   EQU   1                   COLUMN IS FIXED                              
TLKVAR   EQU   2                   COLUMN IS VARIABLE                           
TLKVAL   EQU   3                   COLUMN IS NOT VALID BUT NOT DEFAULT          
TLKNSEL  EQU   4                   COLUMN IS NOT SELECTED                       
TLKPOS   DS    XL1                 DISPLAY POSITION                             
TLKRNAM  DS    XL(D_LEN)           LIST COLUMN HEADINGS                         
TLKRNUM  DS    XL2                 FIELD NUMBER                                 
*                                                                               
         ORG   TLUSER                                                           
TLTYP    DS    XL1                                                              
TLOPEN   EQU   X'80'               COLUMN IS OPEN (UNPROTECTED)                 
TLCOPN   EQU   X'40'               COLUMN IS CAN BE OPENED                      
TLNEWL   EQU   X'20'               COLUMN STARTS NEW LINE                       
TLENQ    EQU   *-TLSTD                                                          
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012CTFIL15   02/18/15'                                      
         END                                                                    
