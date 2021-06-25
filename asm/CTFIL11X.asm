*          DATA SET CTFIL11X   AT LEVEL 008 AS OF 08/22/00                      
*&&      SET   NOP=N                                                            
*PHASE TA1311A                                                                  
FIL11    TITLE 'SCREEN RECORD - ON GENDIR/GENFILE'                              
FIL11    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,CTFIL11*,R6,R7,RR=RE                                           
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         ST    R1,CALLR1                                                        
         MVC   SVPARMS,0(R1)                                                    
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
EXIT     L     R1,CALLR1                                                        
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
*                                                                               
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL               EXIT WITH FIELD NOT INPUT SET                
EXITNOTN MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL               EXIT WITH FIELD NOT NUMERIC SET              
*                                                                               
FLTXL    MVI   SVPARMS,DFLTL       FILTER RETURN LOW                            
         B     EXITOK                                                           
FLTXE    MVI   SVPARMS,DFLTE       FILTER RETURN EQUAL                          
         B     EXITOK                                                           
FLTXH    MVI   SVPARMS,DFLTH       FILTER RETURN HIGH                           
         B     EXITOK                                                           
FLTXX    MVI   SVPARMS,DFLTX       FILTER RETURN NOT WANTED                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* TABLE  ITERATING ROUTINE - R1=EQUATED VERB                          *         
*                          - RF=A(TABLE)                              *         
***********************************************************************         
         SPACE 1                                                                
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK           ** SET HIGH IF NOT WANT OVERRIDE                
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     ITER                ITERATE                                      
*                                                                               
ITER02   ICM   RF,15,OBJADR        INVOKE OBJECT                                
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
         LA    RF,TABLEOO                                                       
         B     ITER                                                             
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OBUILD),AL1(0,0,0),AL4(BUILD)                                
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(OSCRN),AL1(0,0,0),AL4(SCREEN)                                
         DC    AL1(OPFK),AL1(0,0,0),AL4(PFKEY)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     GOTO1 VDICTAT,BOPARM,C'LU  ',DCLIST,DSLIST                             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PFKEY  OBJECT                                                       *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
***********************************************************************         
         SPACE 1                                                                
PFKEY    LM    R0,R3,SVPARMS                                                    
         LA    RF,PFKTABL                                                       
         B     ITER                                                             
*                                                                               
PFKTABL  DC    AL1(PFLST),AL1(0,0,0),AL4(PPFLST)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SET PFKEY VALIDITY                                                  *         
***********************************************************************         
         SPACE 1                                                                
PPFLST   L     RF,SVPARMS3                                                      
         USING FRPELD,RF                                                        
         CLI   FRPPFK#,PFK05       BUILD PFKEY                                  
         BNE   EXITOK                                                           
*                                                                               
         OC    GSRECDA,GSRECDA     ONLY VALID IF WE HAVE THE RECORD             
         BZ    EXITL                                                            
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* SCREEN OBJECT                                                       *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
***********************************************************************         
         SPACE 1                                                                
SCREEN   LM    R0,R3,SVPARMS                                                    
         LA    RF,SCRTABL                                                       
         B     ITER                                                             
*                                                                               
SCRTABL  DC    AL1(SKSET),AL1(0,0,0),AL4(SKSCR)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SET KEY CODE FOR BUILD SCREEN                                       *         
***********************************************************************         
         SPACE 1                                                                
SKSCR    CLI   CSACT,A#BUILD                                                    
         BNE   EXITOK                                                           
         LH    RF,=Y(TWUSER-TWAD)                                               
         A     RF,ATWA                                                          
         USING SAVED,RF                                                         
         MVC   GSSKCODE,KYCFLD                                                  
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
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
         USING FSRRECD,R2                                                       
         LA    RF,KEYTABL                                                       
         B     ITER                                                             
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR KEY OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
KEYFRST  L     R1,SVPARMS+12       EQUATED VERB                                 
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
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
KFKVAL   XC    FSRKEY,FSRKEY       INITIALIZE KEY OF SCREEN RECORD              
         MVI   FSRKMIN,FSRKMINQ    SET MINOR SYSTEM                             
         MVI   FSRKTYP,FSRKTYPQ    SET SCREEN RECORD                            
         MVI   FSRKCTRY,FF                                                      
         MVI   FSRKSUB,FF                                                       
         MVI   EFLAG,0                                                          
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
KFKFVAL  XC    FSRKEY,FSRKEY       INITIALIZE KEY OF SCREEN RECORD              
         MVI   FSRKMIN,FSRKMINQ    SET MINOR SYSTEM                             
         MVI   FSRKTYP,FSRKTYPQ    SET SCREEN RECORD                            
         MVC   FSRKCTRY,CUCTRY                                                  
         XI    FSRKCTRY,FF                                                      
         MVI   EFLAG,0                                                          
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
DATA     ICM   R1,15,SVPARMS+4     RE HOLDS DATA IDENTIFIER                     
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS+12                                                    
         USING FSRRECD,R2                                                       
         XR    R1,R1                                                            
         IC    R1,SVPARMS+8        GET GLOBAL VERB                              
         LA    RF,DTATABL                                                       
         B     ITER                                                             
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
         SPACE 1                                                                
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
         LM    R1,R3,SVPARMS+8     R1 HOLDS VERB                                
         USING FSRRECD,R2          R2 HOLDS A(RECORD)                           
         BR    RF                                                               
         SPACE 1                                                                
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(DLAST),AL1(0,0,0),AL4(DTALAST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(00001),AL4(SYSDTA)       SYSTEM                              
         DC    AL2(00002),AL4(PRGDTA)       PROGRAM                             
         DC    AL2(00003),AL4(RCODTA)       RECORD                              
         DC    AL2(00004),AL4(FLDDTA)       FIELD NUMBER                        
         DC    AL2(00006),AL4(TYPDTA)       TYPE                                
         DC    AL2(00005),AL4(PGNDTA)       PAGE NUMBER                         
         DC    AL2(00095),AL4(PRSDTA)       PROGRAM SHADOW                      
         DC    AL2(00096),AL4(RCSDTA)       RECORD SHADOW                       
         DC    AL2(00097),AL4(CDEDTA)       CODE                                
         DC    AL2(00150),AL4(CTRDTA)       COUNTRY                             
         DC    AL2(00098),AL4(PNMDTA)       PAGE NAME                           
         DC    AL2(00070),AL4(KYCDTA)       KEY CODE FOR SCREEN                 
         DC    AL2(00116),AL4(TSTDTA)       TEST PHASE                          
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL11    CSECT                                                                  
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                          *         
***********************************************************************         
         SPACE 1                                                                
DTAFRST  L     R1,SVPARMS+8                                                     
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
*                             *** FIRST TIME FOR DATA OBJECT ***                
*                                 -------------------------                     
DFTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DFDDIS)      DISPLAY                    
         DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)      VALIDATE                   
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFDFDIS)    DISPLAY FILTER             
         DC    AL1(DFVAL),AL1(0,0,0),AL4(DFDFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DFDDIS   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A DATA FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
DFDFDIS  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA FILTER                            *         
***********************************************************************         
         SPACE 1                                                                
DFDFVAL  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR DATA OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
DTALAST  L     R1,SVPARMS+8                                                     
         LA    RF,DLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
*                             *** LAST TIME FOR DATA OBJECT ***                 
*                                 ------------------------                      
DLTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DLDDIS)      DISPLAY                    
         DC    AL1(DVAL),AL1(0,0,0),AL4(DLDVAL)      VALIDATE                   
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DLDFDIS)    DISPLAY FILTER             
         DC    AL1(DFVAL),AL1(0,0,0),AL4(DLDFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR DISPLAY OF A DATA OBJECT                              *         
***********************************************************************         
         SPACE 1                                                                
DLDDIS   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DLDVAL   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR DISPLAY OF A DATA FILTER                              *         
***********************************************************************         
         SPACE 1                                                                
DLDFDIS  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A DATA FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
DLDFVAL  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR SYSTEM                                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
SYSDTA   LA    RF,SYSTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
SYSTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSYS)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSYS)                                 
         DC    AL1(DHED),AL1(0,0,0),AL4(HEDSYS)                                 
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
         CLC   SYSLNUM,FSRKSYS     MATCH ON SYSTEM NUMBER                       
         BE    *+12                                                             
         LA    RE,SYSLLEN(RE)      NO - BUMP TO NEXT TABLE ENTRY                
         B     DSYS02                                                           
         SPACE 1                                                                
         MVC   FVIFLD(L'SYSLNAME),SYSLNAME                                      
         B     EXITOK                                                           
         DROP  RE                                                               
         SPACE 1                                                                
DSYS04   CURED FSRKSYS,(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A SYSTEM FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALSYS   XR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         L     R3,ASYSLST                                                       
         USING SYSLSTD,R3                                                       
         LA    R3,6(R3)            R3=A(SYSTEM LIST)                            
         SPACE 1                                                                
VSYS02   CLI   0(R3),0             TEST E-O-T                                   
         BE    EXITNV                                                           
         CLI   SYSLNUM,1           IGNORE SERVICE SYSTEM                        
         BE    VSYS04                                                           
         EX    RF,*+8              MATCH INPUT TO TABLE                         
         BE    VSYS06                                                           
         CLC   SYSLNAME(0),FVIFLD                                               
         SPACE 1                                                                
VSYS04   LA    R3,SYSLLEN(R3)      NO - BUMP TO NEXT TABLE ENTRY                
         B     VSYS02                                                           
         SPACE 1                                                                
VSYS06   MVC   FSRKSYS,SYSLNUM     SYSTEM NUMBER                                
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* CREATE A SYSTEM HEADLINE FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
HEDSYS   DS    0H                                                               
         B     EXITOK                                                           
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
         L     R3,ASYSLST                                                       
         USING SYSLSTD,R3                                                       
         LA    R3,6(R3)            R3=A(SYSTEM LIST)                            
         SPACE 1                                                                
VFSYS02  CLI   0(R3),0             TEST E-O-T                                   
         BE    EXITNV                                                           
         CLI   SYSLNUM,1           IGNORE SERVICE SYSTEM                        
         BE    VFSYS04                                                          
         EX    RF,*+8              MATCH INPUT TO TABLE                         
         BE    VFSYS06                                                          
         CLC   SYSLNAME(0),FVIFLD                                               
         SPACE 1                                                                
VFSYS04  LA    R3,SYSLLEN(R3)      NO - BUMP TO NEXT TABLE ENTRY                
         B     VFSYS02                                                          
         SPACE 1                                                                
VFSYS06  MVC   FLTIFLD(L'SYSLNUM),SYSLNUM     SYSTEM NUMBER                     
         MVC   FSRKSYS,SYSLNUM     SYSTEM NUMBER                                
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR SYSTEM                                             *         
***********************************************************************         
         SPACE 1                                                                
DOFTSYS  CLC   FSRKSYS,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR PROGRAM                                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
PRGDTA   LA    RF,PRGTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
PRGTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPRG)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPRG)                                 
         DC    AL1(DHED),AL1(0,0,0),AL4(HEDPRG)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTPRG)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTPRG)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTPRG)                                
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISPRG)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PROGRAM FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
DISPRG   OC    FSRKPRG,FSRKPRG     WAS A PROGRAM ENTERED?                       
         BZ    EXITOK              NO                                           
         SPACE 1                                                                
         L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FSRKSYS                                                  
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     DPRG08                                                           
         SPACE 1                                                                
         L     R1,SEPGMS                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1          FIND PGMLST ENTRY                            
DPRG02   CLC   PGMNUM,FSRKPRG                                                   
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
DPRG08   CURED FSRKPRG,(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                      
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PROGRAM FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
VALPRG   CLI   FVILEN,0                                                         
         BE    EXITNO                                                           
         L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FSRKSYS                                                  
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
VPRG08   MVC   FSRKPRG,PGMNUM      PROGRAM NUMBER                               
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
DFLTPRG  OC    FLTIFLD(L'FSRKPRG),FLTIFLD                                       
         BZ    EXITOK                                                           
         SPACE 1                                                                
         L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FSRKSYS                                                  
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
         IC    RF,FLTIFLD                                                       
         CURED (RF),(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                         
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PROGRAM FILTER FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
VFLTPRG  L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FSRKSYS                                                  
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
VFPRG08  MVC   FLTIFLD(L'PGMNUM),PGMNUM      PROGRAM NUMBER                     
         MVC   FSRKPRG,PGMNUM                                                   
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR PROGRAM FILTER                                     *         
***********************************************************************         
         SPACE 1                                                                
DOFTPRG  CLC   FSRKPRG,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
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
         DC    AL1(DHED),AL1(0,0,0),AL4(HEDRCO)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTRCO)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTRCO)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTRCO)                                
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISRCO)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
DISRCO   OC    FSRKREC,FSRKREC     GLOBAL FIELD?                                
         BZ    EXITOK                                                           
         ICM   RF,8,FSRKSYS                                                     
         ICM   RF,4,FSRKPRG                                                     
         ICM   RF,2,FSRKREC                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXDIS,(RF)                                   
         BE    EXITOK                                                           
         CURED FSRKREC,(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALRCO   MVI   FSRKREC,0           IF NO RECORD                                 
         CLI   FVILEN,0            FIELD SET AT LEVELS ABOVE                    
         BE    EXITOK                                                           
         SPACE 1                                                                
         OC    FSRKPRG,FSRKPRG     TRYING TO OVERRIDE RECORD - MUST             
         BZ    EXITNV              INPUT THE PROGRAM                            
         SPACE 1                                                                
         XR    RF,RF                                                            
         ICM   RF,8,FSRKSYS                                                     
         ICM   RF,4,FSRKPRG                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXVAL,(RF)                                   
         BNE   EXITL               NOT FOUND                                    
         L     RF,8(R1)                                                         
         STCM  RF,2,FSRKREC                                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* CREATE A RECORD HEADLINE FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
HEDRCO   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD FILTER FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DFLTRCO  OC    FLTIFLD(L'FSRKREC),FLTIFLD RECORD FIELD?                         
         BZ    EXITOK                                                           
         ICM   RF,8,FSRKSYS                                                     
         ICM   RF,4,FSRKPRG                                                     
         ICM   RF,2,FLTIFLD                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXDIS,(RF)                                   
         BE    EXITOK                                                           
         CURED FSRKREC,(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD FILTER FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
VFLTRCO  MVI   FSRKREC,0           IF NO RECORD                                 
         CLI   FVILEN,0            FIELD SET AT LEVELS ABOVE                    
         BE    EXITOK                                                           
         SPACE 1                                                                
         OC    FSRKPRG,FSRKPRG     TRYING TO FILTER RECORD - MUST               
         BZ    EXITNV              INPUT THE PROGRAM                            
         SPACE 1                                                                
         XR    RF,RF                                                            
         ICM   RF,8,FSRKSYS                                                     
         ICM   RF,4,FSRKPRG                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXVAL,(RF)                                   
         BNE   EXITL               NOT FOUND                                    
         L     RF,8(R1)                                                         
         STCM  RF,2,FSRKREC                                                     
         STCM  RF,2,FLTIFLD                                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR RECORD                                             *         
***********************************************************************         
         SPACE 1                                                                
DOFTRCO  CLC   FSRKREC,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
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
DISCDE   MVC   FVIFLD(L'FSRKCODE),FSRKCODE                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A CODE FIELD                                               *         
***********************************************************************         
         SPACE 1                                                                
VALCDE   MVI   FSRKCODE,0          IF NO CODE, DEFAULT IS ZERO                  
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVC   FSRKCODE,FVIFLD                                                  
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
DFLTCDE  OC    FLTIFLD(L'FSRKCODE),FLTIFLD CODE FIELD?                          
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'FSRKCODE),FSRKCODE                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A CODE FILTER FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VFLTCDE  MVI   FSRKCODE,0          IF NO CODE SET ZERO                          
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVC   FSRKCODE,FVIFLD                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR CODE                                               *         
***********************************************************************         
         SPACE 1                                                                
DOFTCDE  CLC   FSRKCODE,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR PROGRAM SHADOW                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
PRSDTA   LA    RF,PRSTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
PRSTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPRS)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPRS)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PROGRAM SHADOW FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING SPRELD,BOELEM                                                    
DISPRS   GOTO1 AGETEL,BOPARM,('SPRELQ',FSRRECD),0                               
         BNE   EXITOK                                                           
         SPACE 1                                                                
         L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FSRKSYS                                                  
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     DPRS08                                                           
         SPACE 1                                                                
         L     R1,SEPGMS                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1          FIND PGMLST ENTRY                            
DPRS02   CLC   PGMNUM,SPRPRG                                                    
         BNE   DPRS04                                                           
         CLI   PGMCTRY,0           MATCH ON COUNTRY CODE                        
         BE    DPRS06                                                           
         CLC   PGMCTRY,CUCTRY                                                   
         BE    DPRS06                                                           
DPRS04   BXLE  R1,RE,DPRS02                                                     
         B     DPRS08                                                           
         SPACE 1                                                                
DPRS06   MVC   FVIFLD(L'PGMNAME),PGMNAME                                        
         B     EXITOK                                                           
         SPACE 1                                                                
DPRS08   CURED SPRPRG,(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                       
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PROGRAM SHADOW FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
VALPRS   GOTO1 ADELEL,BOPARM,('SPRELQ',FSRRECD),0                               
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FSRKSYS                                                  
         BE    VPRS02                                                           
         BXLE  R1,RE,*-10                                                       
         MVC   FVMSGNO,=AL2(FVFESYS)  INVALID SYSTEM                            
         B     EXITL                                                            
*                                                                               
VPRS02   L     R1,SEPGMS                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1          R1=A(PROGRAMS LIST)                          
         XR    R3,R3                                                            
         IC    R3,FVXLEN           R3=INPUT LENGTH-1                            
*                                                                               
VPRS04   CLI   PGMCTRY,0           MATCH ON COUNTRY                             
         BE    *+14                                                             
         CLC   PGMCTRY,CUCTRY                                                   
         BNE   VPRS06                                                           
         EX    R3,*+8                                                           
         BE    VPRS08                                                           
         CLC   PGMNAME(0),FVIFLD   MATCH INPUT TO PROGRAM NAME                  
*                                                                               
VPRS06   BXLE  R1,RE,VPRS04                                                     
         MVI   FVOSYS,GCSYSGEN     CONTROL SYSTEM                               
         MVC   FVMSGNO,=AL2(CE#PGNVS) INVALID PROGRAM                           
         B     EXITL                                                            
*                                                                               
         PUSH  USING                                                            
         USING SPRELD,BOELEM                                                    
VPRS08   XC    SPRELD(SPRLNQ),SPRELD                                            
         MVI   SPREL,SPRELQ        SHADOW ELEMENT                               
         MVI   SPRLN,SPRLNQ                                                     
         MVC   SPRPRG,PGMNUM       PROGRAM NUMBER                               
         GOTO1 AADDEL,BOPARM,FSRRECD,0                                          
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR RECORD SHADOW                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
RCSDTA   LA    RF,RCSTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
RCSTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISRCS)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRCS)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING SPRELD,BOELEM                                                    
DISRCS   GOTO1 AGETEL,BOPARM,('SPRELQ',FSRRECD),0                               
         BNE   EXITOK                                                           
*                                                                               
         ICM   RF,8,FSRKSYS                                                     
         ICM   RF,4,SPRPRG                                                      
         ICM   RF,2,SPRREC                                                      
         GOTO1 AGEN,BOPARM,ORTYPE,RTXDIS,(RF)                                   
         BE    EXITOK                                                           
         CURED SPRREC,(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                       
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING SPRELD,BOELEM                                                    
VALRCS   GOTO1 AGETEL,BOPARM,('SPRELQ',FSRRECD),0                               
         BNE   EXITOK                                                           
         CLI   FVILEN,0            FIELD SET AT LEVELS ABOVE                    
         BE    EXITNO                                                           
         SPACE 1                                                                
         XR    RF,RF                                                            
         ICM   RF,8,FSRKSYS                                                     
         ICM   RF,4,SPRPRG                                                      
         GOTO1 AGEN,BOPARM,ORTYPE,RTXVAL,(RF)                                   
         BNE   EXITL               NOT FOUND                                    
         L     RF,8(R1)                                                         
         STCM  RF,2,SPRREC                                                      
         GOTO1 ADELEL,BOPARM,('SPRELQ',FSRRECD),0                               
         GOTO1 AADDEL,BOPARM,FSRRECD,0                                          
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR PAGE NAME                                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
PNMDTA   LA    RF,PNMTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
PNMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPNM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPNM)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PAGE NAME FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING PNMELD,BOELEM                                                    
DISPNM   GOTO1 AGETEL,BOPARM,('PNMELQ',FSRRECD),0                               
         BNE   EXITOK                                                           
*                                                                               
         MVC   BCFULL,PNMNAME                                                   
         XR    RF,RF                                                            
         IC    RF,FSRKSYS                                                       
         GOTOX ('DISDIC',AGROUTS),BOPARM,(RF)                                   
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PAGE NAME FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING PNMELD,BOELEM                                                    
VALPNM   GOTO1 ADELEL,BOPARM,('PNMELQ',FSRRECD),0                               
         CLI   FVILEN,0            PAGE NAME SET?                               
         BE    EXITOK                                                           
         SPACE 1                                                                
         XR    RF,RF                                                            
         IC    RF,FSRKSYS                                                       
         GOTOX ('VALDIC',AGROUTS),BOPARM,(RF)                                   
         BNE   EXITL                                                            
*                                                                               
         XC    BOWORK1(PNMLNQ),BOWORK1                                          
         MVI   PNMEL,PNMELQ                                                     
         MVI   PNMLN,PNMLNQ                                                     
         MVC   PNMNAME,BCFULL                                                   
         GOTO1 AADDEL,BOPARM,FSRRECD,0                                          
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR KEY CODE                                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
KYCDTA   LA    RF,KYCTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
KYCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISKYC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALKYC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A KEY CODE FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
DISKYC   LH    RF,=Y(TWUSER-TWAD)                                               
         A     RF,ATWA                                                          
         USING SAVED,RF                                                         
         MVC   FVIFLD(L'KYCFLD),KYCFLD                                          
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A KEY CODE FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
VALKYC   LH    RF,=Y(TWUSER-TWAD)                                               
         A     RF,ATWA                                                          
         USING SAVED,RF                                                         
         XC    KYCFLD,KYCFLD                                                    
         CLI   FVILEN,0            PAGE NAME SET?                               
         BE    EXITOK                                                           
         MVC   KYCFLD,FVIFLD                                                    
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR TEST PHASE                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
TSTDTA   LA    RF,TSTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
TSTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTST)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTST)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY TEST VERSION                                                *         
***********************************************************************         
         SPACE 1                                                                
DISTST   OC    FSRKTEST,FSRKTEST                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'FSRKTEST),FSRKTEST                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE TEST VERSION                                               *         
***********************************************************************         
         SPACE 1                                                                
VALTST   XC    FSRKTEST,FSRKTEST                                                
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'A'                                                      
         BNE   *+12                                                             
         MVI   FSRKTEST,C'A'                                                    
         B     EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'B'                                                      
         BNE   *+12                                                             
         MVI   FSRKTEST,C'B'                                                    
         B     EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'C'                                                      
         BNE   *+12                                                             
         MVI   FSRKTEST,C'C'                                                    
         B     EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'X'                                                      
         BNE   *+12                                                             
         MVI   FSRKTEST,C'X'                                                    
         B     EXITOK                                                           
*                                                                               
         B     EXITNV                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FIELD NUMBER                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
FLDDTA   LA    RF,FLDTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
FLDTBL   DC    AL1(DNTR),AL1(0,0,0),AL4(NFLDN)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* PASS FIELD NUMBER FOR NTRSES                                        *         
***********************************************************************         
         SPACE 1                                                                
NFLDN    OC    FIELDN,FIELDN                                                    
         BZ    EXITOK                                                           
         XR    RF,RF                                                            
         ICM   RF,3,FIELDN                                                      
         CURED (RF),(5,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR SCREEN TYPE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
TYPDTA   LA    RF,TYPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
TYPTBL   DS    0A                                                               
         DC    AL1(DDIS),AL1(0,0,0),AL4(DISTYP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTYP)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTTYP)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTTYP)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A SCREEN TYPE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISTYP   CLI   EFLAG,0             REDISPLAY AFTER VALIDATE?                    
         BE    DTYP02              NO                                           
         CLI   EFLAG,C'K'          KEY                                          
         BNE   *+10                                                             
         MVC   FVIFLD(L'GE@KEY),GE@KEY                                          
         CLI   EFLAG,C'L'          LIST                                         
         BNE   *+10                                                             
         MVC   FVIFLD(L'GE@LIST),GE@LIST                                        
         CLI   EFLAG,C'D'          DATA                                         
         BNE   *+10                                                             
         MVC   FVIFLD(L'GE@DATA),GE@DATA                                        
         CLI   EFLAG,C'W'          DOWNLOAD                                     
         BNE   *+10                                                             
         MVC   FVIFLD(L'GE@DLOAD),GE@DLOAD                                      
         CLI   EFLAG,C'R'          REPORT                                       
         BNE   *+10                                                             
         MVC   FVIFLD(L'GE@REP),GE@REP                                          
         B     EXITOK                                                           
*                                                                               
DTYP02   MVC   FVIFLD(L'GE@DATA),GE@DATA                                        
*                                                                               
         CLI   FSRKPAGE,0          KEY SCREEN                                   
         BNE   *+10                                                             
         MVC   FVIFLD(L'GE@KEY),GE@KEY                                          
*                                                                               
         CLI   FSRKPAGE,FF         LIST SCREEN                                  
         BNE   *+10                                                             
         MVC   FVIFLD(L'GE@LIST),GE@LIST                                        
*                                                                               
         CLI   FSRKPAGE,X'FE'      DOWNLOAD SCREEN                              
         BNE   *+10                                                             
         MVC   FVIFLD(L'GE@DLOAD),GE@DLOAD                                      
*                                                                               
         CLI   FSRKPAGE,X'FD'      REPORT SCREEN                                
         BNE   *+10                                                             
         MVC   FVIFLD(L'GE@REP),GE@REP                                          
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE A SCREEN TYPE FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALTYP   XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         MVI   EFLAG,C'K'          KEY FIELD?                                   
         MVI   FSRKPAGE,0                                                       
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),GE@KEY                                                 
*                                                                               
         MVI   EFLAG,C'D'          DATA FIELD                                   
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),GE@DATA                                                
*                                                                               
         MVI   EFLAG,C'L'          LIST FIELD                                   
         MVI   FSRKPAGE,FF                                                      
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),GE@LIST                                                
*                                                                               
         MVI   EFLAG,C'W'          DOWNLOAD SCREEN                              
         MVI   FSRKPAGE,X'FE'                                                   
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),GE@DLOAD                                               
*                                                                               
         MVI   EFLAG,C'R'          REPORT SCREEN                                
         MVI   FSRKPAGE,X'FD'                                                   
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),GE@REP                                                 
*                                                                               
         B     EXITNV              NOTHING ELSE ALLOWED                         
         SPACE 2                                                                
***********************************************************************         
* CREATE A SCREEN TYPE HEADLINE FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
HEDTYP   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A SCREEN TYPE FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DFLTTYP  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A SCREEN TYPE FILTER FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
VFLTTYP  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
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
DISCTR   CLI   FSRKCTRY,0          FOR THOSE THERE ALREADY                      
         BE    EXITOK                                                           
         MVC   MYBYTE,FSRKCTRY     COUNTRY CODE IS 1`S COMP IN KEY              
         XC    MYBYTE,BCEFFS                                                    
         LA    R1,CTRYTAB                                                       
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING CTRYTABD,R1                                                      
*                                                                               
DCTR02   CLC   CTRYCODE,MYBYTE     MATCH ON CODE                                
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
         MVC   FSRKCTRY,BCEFFS     SET DEFAULT TO NATIVE COUNTRY                
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
VCTR06   MVC   FSRKCTRY,CTRYCODE   MOVE IN COUNTRY CODE                         
         XC    FSRKCTRY,BCEFFS     1`S COMPLIMENT IT                            
                                                                                
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
         CLC   CTRYCODE,FLTIFLD    MATCH ON CODE                                
         BNE   *+12                                                             
         TM    CTRYTYPE,CTRYTYCO   MAKE SURE IT IS A COUNTRY                    
         BO    DFCTR02                                                          
         BXLE  R1,RE,*-10                                                       
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
         MVI   FSRKCTRY,FF         SET DEFAULT TO NATIVE COUNTRY                
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
VFCTR06  MVC   FSRKCTRY,CTRYCODE   MOVE IN COUNTRY CODE                         
         MVC   FLTIFLD(L'CTRYCODE),CTRYCODE                                     
         XC    FSRKCTRY,BCEFFS     1`S COMPLIMENT CODE IN KEY                   
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* DO COUNTRY FILTERING                                                *         
***********************************************************************         
         SPACE 1                                                                
DOFTCTR  MVC   MYBYTE,FVIFLD                                                    
         XC    MYBYTE,BCEFFS                                                    
         CLC   FSRKCTRY,MYBYTE                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR PAGE NUMBER                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
PGNDTA   LA    RF,PGNTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
PGNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPGN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPGN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PAGE NUMBER FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISPGN   CLI   FSRKPAGE,0         KEY SCREEN                                    
         BE    EXITOK                                                           
         CLI   FSRKPAGE,FSRKPLST  LIST SCREEN                                   
         BE    EXITOK                                                           
         CLI   FSRKPAGE,FSRKPDWN  DOWNLOAD SCREEN                               
         BE    EXITOK                                                           
         CLI   FSRKPAGE,FSRKPREP  REPORT SCREEN                                 
         BE    EXITOK                                                           
*                                                                               
         CURED FSRKPAGE,(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PAGE NUMBER FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALPGN   CLI   EFLAG,C'D'          ONLY DATA SCREEN CAN HAVE NUMBER             
         BE    VPGN02                                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         B     EXITNV                                                           
         SPACE 1                                                                
VPGN02   CLI   FVILEN,0            DATA SCREEN MUST HAVE A NUMBER               
         BE    EXITNO                                                           
         TM    FVIIND,FVINUM       FIELD MUST BE NUMERIC                        
         BZ    EXITNV                                                           
         L     RF,BCFULL                                                        
         CH    RF,=H'250'          MAX NUMBER OF PAGES                          
         BH    EXITNV                                                           
         STC   RF,FSRKPAGE                                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD OBJECT                                                        *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(RECORD)                                                        *         
* P4 HOLDS SUB-ACTION VERB                                            *         
***********************************************************************         
         SPACE 1                                                                
BUILD    LM    R0,R3,SVPARMS                                                    
         USING FSRRECD,R2                                                       
         LA    RF,TABLBLD                                                       
         B     ITER                                                             
*                                                                               
TABLBLD  DC    AL1(BLDSCRN),AL1(0,0,0),AL4(BLDSCR)                              
         DC    AL1(BADDFLD),AL1(0,0,0),AL4(ADDFLD)                              
         DC    AL1(BADDTAG),AL1(0,0,0),AL4(ADDTAG)                              
         DC    AL1(BDELFLD),AL1(0,0,0),AL4(DELFLD)                              
         DC    AL1(BADDLINE),AL1(0,0,0),AL4(ADDLINE)                            
         DC    AL1(BDELLINE),AL1(0,0,0),AL4(DELLINE)                            
         DC    AL1(BSFFORAD),AL1(0,0,0),AL4(SFFORAD)                            
         DC    AL1(BEFCONSC),AL1(0,0,0),AL4(EFCONSC)                            
         DC    AL1(BSLST),AL1(0,0,0),AL4(SLST)                                  
         DC    AL1(BELST),AL1(0,0,0),AL4(ELST)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* BUILD SCREEN                                                        *         
***********************************************************************         
         SPACE 1                                                                
BLDSCR   GOTOX ('FLDVAL',AGROUTS),BASOPTH                                       
*                                                                               
         CLI   BCPFKEY,PFK03       DELETING A FIELD?                            
         BNE   BLDR02                                                           
         GOTO1 AGEN,BOPARM,('GCBOVER',OBUILD),BDELFLD,FSRRECD,0                 
         B     EXIT                                                             
*                                                                               
BLDR02   CLI   BCPFKEY,PFK04       ADDING A LINE?                               
         BNE   BLDR04                                                           
         GOTO1 AGEN,BOPARM,('GCBOVER',OBUILD),BADDLINE,FSRRECD,0                
         B     EXIT                                                             
*                                                                               
BLDR04   CLI   BCPFKEY,PFK05       DELETING A LINE                              
         BNE   BLDR06                                                           
         GOTO1 AGEN,BOPARM,('GCBOVER',OBUILD),BDELLINE,FSRRECD,0                
         B     EXIT                                                             
*                                                                               
BLDR06   CLI   BCPFKEY,PFK06       EDITING A FIELD ON SCREEN                    
         BNE   BLDR08                                                           
         GOTO1 AGEN,BOPARM,('GCBOVER',OBUILD),BEFCONSC,FSRRECD,0                
         B     EXIT                                                             
*                                                                               
BLDR08   CLI   BCPFKEY,PFK07       STARTING LIST                                
         BNE   BLDR10                                                           
         GOTO1 AGEN,BOPARM,('GCBOVER',OBUILD),BSLST,FSRRECD,0                   
         B     EXIT                                                             
*                                                                               
BLDR10   CLI   BCPFKEY,PFK08       ENDING LIST                                  
         BNE   BLDR12                                                           
         GOTO1 AGEN,BOPARM,('GCBOVER',OBUILD),BELST,FSRRECD,0                   
         B     EXIT                                                             
*                                                                               
BLDR12   CLI   BCPFKEY,PFK02       ADDING A FIELD?                              
         BNE   BLDR16                                                           
*                                                                               
         CLI   FVIFLD,C'&&'        ARE WE HANDLING TAGS?                        
         BNE   BLDR14                                                           
         GOTO1 AGEN,BOPARM,('GCBOVER',OBUILD),BADDTAG,FSRRECD,0                 
         B     EXIT                                                             
*                                                                               
BLDR14   TM    FVIIND,FVINUM       NUMBER?                                      
         BZ    EXITOK                                                           
         GOTO1 AGEN,BOPARM,('GCBOVER',OBUILD),BADDFLD,FSRRECD,0                 
         B     EXIT                                                             
*                                                                               
BLDR16   CLI   FVIFLD,C'='         SELECT FIELD FOR ADD?                        
         BNE   BLDR18                                                           
         GOTO1 AGEN,BOPARM,('GCBOVER',OBUILD),BSFFORAD,FSRRECD,0                
         B     EXIT                                                             
*                                                                               
BLDR18   B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* ADD FIELD TO SCREEN                                                 *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING FSRELD,BOELEM       BUILD FIELD ELEMENT                          
         USING FDRRECD,IOKEY                                                    
ADDFLD   L     RF,BCFULL           EXPECTS NUMBER TO BE IN BCFULL               
         STCM  RF,3,FLDNUM                                                      
*                                                                               
         OC    FSRKTEST,FSRKTEST   IS THIS A TEST SCREEN?                       
         BZ    ADDF02                                                           
*                                                                               
         XC    FDRKEY,FDRKEY       READ CONNECTED COUNTRY                       
         MVI   FDRKMIN,FDRKMINQ    AND TEST PHASE                               
         MVI   FDRKTYP,FDRKTYPQ                                                 
         MVC   FDRKSYS,FSRKSYS                                                  
         MVC   FDRKPRG,FSRKPRG                                                  
         MVC   FDRKREC,FSRKREC                                                  
         MVC   FDRKNUM,FLDNUM                                                   
         MVC   FDRKCTRY,FSRKCTRY                                                
         MVC   FDRKSUB,FSRKSUB                                                  
         MVC   FDRKTEST,FSRKTEST                                                
         L     R1,=AL4(XOREAD+XOGENDIR+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    ADDF06                                                           
*                                                                               
ADDF02   XC    FDRKEY,FDRKEY       READ CONNECTED COUNTRY                       
         MVI   FDRKMIN,FDRKMINQ    AND LIVE PHASE                               
         MVI   FDRKTYP,FDRKTYPQ                                                 
         MVC   FDRKSYS,FSRKSYS                                                  
         MVC   FDRKPRG,FSRKPRG                                                  
         MVC   FDRKREC,FSRKREC                                                  
         MVC   FDRKNUM,FLDNUM                                                   
         MVC   FDRKCTRY,FSRKCTRY                                                
         MVC   FDRKSUB,FSRKSUB                                                  
         XC    FDRKTEST,FDRKTEST                                                
         L     R1,=AL4(XOREAD+XOGENDIR+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    ADDF06                                                           
*                                                                               
         OC    FSRKTEST,FSRKTEST   IS THIS A TEST SCREEN?                       
         BZ    ADDF04                                                           
*                                                                               
         XC    FDRKEY,FDRKEY       READ HOST PROCESSOR                          
         MVI   FDRKMIN,FDRKMINQ    AND TEST PHASE                               
         MVI   FDRKTYP,FDRKTYPQ                                                 
         MVC   FDRKSYS,FSRKSYS                                                  
         MVC   FDRKPRG,FSRKPRG                                                  
         MVC   FDRKREC,FSRKREC                                                  
         MVC   FDRKNUM,FLDNUM                                                   
         MVI   FDRKCTRY,FF                                                      
         MVI   FDRKSUB,FF                                                       
         MVC   FDRKTEST,FSRKTEST                                                
         L     R1,=AL4(XOREAD+XOGENDIR+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    ADDF06                                                           
*                                                                               
ADDF04   XC    FDRKEY,FDRKEY       READ HOST PROCESSOR                          
         MVI   FDRKMIN,FDRKMINQ    AND LIVE PHASE                               
         MVI   FDRKTYP,FDRKTYPQ                                                 
         MVC   FDRKSYS,FSRKSYS                                                  
         MVC   FDRKPRG,FSRKPRG                                                  
         MVC   FDRKREC,FSRKREC                                                  
         MVC   FDRKNUM,FLDNUM                                                   
         MVC   FDRKCTRY,FF                                                      
         MVC   FDRKSUB,FF                                                       
         XC    FDRKTEST,FDRKTEST                                                
         L     R1,=AL4(XOREAD+XOGENDIR+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    ADDF06                                                           
*                                                                               
         MVC   FVMSGNO,=AL2(CE#RECNF)                                           
         B     EXITL                                                            
*                                                                               
ADDF06   CLI   FSRKPAGE,FSRKPLST   EDITING A LIST SCREEN?                       
         BNE   ADDF10              NO                                           
*                                                                               
         L     R1,=AL4(XOGET+XOGENFIL+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                DISK ERROR                                   
*                                                                               
         L     RF,AIO1                                                          
         LA    RF,FDRFIRST(RF)                                                  
         XR    RE,RE                                                            
*                                                                               
ADDF08   CLI   0(RF),0                                                          
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(CE#NOFLT)                                           
         B     EXITL                                                            
*                                                                               
         CLI   0(RF),FLTRLQ                                                     
         BE    ADDF10                                                           
         IC    RE,1(RF)                                                         
         LA    RF,0(RE,RF)                                                      
         B     ADDF08                                                           
*                                                                               
ADDF10   XC    BOELEM(FSRLN2Q),BOELEM                                           
         MVI   FSREL,FSRELQ                                                     
         MVI   FSRLN,FSRLN1Q                                                    
         MVC   FSRPOSN,CSCURDSP                                                 
         MVI   FSRIND1,0                                                        
         MVC   FSRNUM,FLDNUM                                                    
         GOTO1 AADDEL,BOPARM,FSRRECD                                            
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* ADD TAG TO SCREEN                                                   *         
***********************************************************************         
         SPACE 1                                                                
ADDTAG   XC    MYBYTE,MYBYTE                                                    
         MVI   MYBYTE1,FHATPR                                                   
         GOTO1 VSCANNER,BOPARM,FVADDR,(4,MYSCAN)                                
         XR    R0,R0                                                            
         ICM   R0,1,4(R1)         NUMBER OF FIELDS INPUT                        
         LA    R3,MYSCAN                                                        
         USING SCANBLKD,R3                                                      
ADDT02   CLC   =C'COLOUR',SC1STFLD                                              
         BE    ADDT04                                                           
         CLC   =C'HIGH',SC1STFLD                                                
         BE    ADDT10                                                           
*                                                                               
ADDT03   LA    R3,SCBLKLQ(R3)                                                   
         BCT   R0,ADDT02                                                        
         B     ADDT20                                                           
*                                                                               
ADDT04   LA    R0,7                7 POSSIBLE COLOURS                           
         LA    RF,1                                                             
         XR    RE,RE                                                            
         IC    RE,SC2NDLEN                                                      
         SH    RE,=H'1'                                                         
         BM    EXITNV                                                           
         LA    R1,UC@BLUE                                                       
ADDT06   EX    RE,*+8                                                           
         BE    ADDT08                                                           
         CLC   0(0,R1),SC2NDFLD    MATCH ON UPPERCASE                           
*                                                                               
         LA    RF,1(RF)                                                         
         LA    R1,L'UC@BLUE(R1)                                                 
         BCT   R0,ADDT06                                                        
         B     EXITNV              NO MATCH ON ANY COLOUR                       
*                                                                               
ADDT08   STC   RF,MYBYTE           MYBYTE HOLDS NUMBER FOR COLOUR               
         B     ADDT03                                                           
*                                                                               
ADDT10   OI    MYBYTE1,FHATHI      MYBYTE1 HOLDS ATTRIBUTE BYTE                 
         B     ADDT03                                                           
*                                                                               
ADDT20   XR    RF,RF                                                            
         IC    RF,FSRKSYS                                                       
         GOTOX ('VALDIC',AGROUTS),BOPARM,(RF)                                   
         BNE   EXITL                                                            
*                                                                               
         CLI   BCPFKEY,PFK02       EQUATE FOR ADD PFKEY ???                     
         BNE   EXITOK              DON`T WANT TO ADD RIGHT NOW                  
*                                                                               
         XC    BOELEM(FSRLN2Q),BOELEM                                           
         PUSH  USING                                                            
         USING FSRELD,BOELEM                                                    
         MVI   FSREL,FSRELQ        BUILD A TAG FIELD FSREL                      
         MVI   FSRLN,FSRLN2Q                                                    
         MVC   FSRPOSN,CSCURDSP                                                 
         OI    FSRIND1,FSRITAG                                                  
         MVC   FSRTLN,BCFULL+3                                                  
         MVC   FSRTDICT,BCFULL                                                  
         MVC   FSRTFHAT,MYBYTE1    PROTECTED + OTHERS                           
         OC    FSRTFHXT,MYBYTE                                                  
         GOTO1 AADDEL,BOPARM,FSRRECD                                            
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DELETE TAG OR FIELD FROM SCREEN                                     *         
***********************************************************************         
         SPACE 1                                                                
DELFLD   XR    RF,RF                                                            
         ICM   RF,3,CSCURDSP       CURRENT CURSOR POSITION                      
         XR    RE,RE                                                            
         D     RE,=F'80'                                                        
         MH    RF,=H'80'                                                        
         STH   RF,BOHALF1          DISPLACEMENT TO START OF THIS LINE           
         LA    RF,80(RF)                                                        
         STH   RF,BOHALF2          DISPLACEMENT TO START OF NEXT LINE           
*                                                                               
         XC    BCFULL,BCFULL                                                    
         LA    RF,FSRRECD+FSRFIRST START OF RECORD                              
         USING FSRELD,RF                                                        
         XR    RE,RE                                                            
*                                                                               
DFLD02   CLI   FSREL,0             REACHED END OF RECORD                        
         BE    DFLD06                                                           
         CLI   FSREL,FSRELQ        SCREEN FIELD ELEMENT                         
         BNE   DFLD04                                                           
*                                                                               
         CLC   FSRPOSN,BOHALF1     REACHED ELEMENTS FOR THIS LINE YET?          
         BL    DFLD04              NO                                           
         CLC   FSRPOSN,CSCURDSP    IS THIS FIELD AFTER CURSOR?                  
         BH    DFLD06              YES                                          
         CLC   FSRPOSN,BOHALF2     REACHED ELEMENTS FOR NEXT LINE YET?          
         BNL   DFLD06              YES                                          
*                                                                               
         ST    RF,BCFULL           SAVE A(THIS ELEMENT)                         
*                                                                               
DFLD04   IC    RE,FSRLN                                                         
         LA    RF,0(RE,RF)                                                      
         B     DFLD02                                                           
*                                                                               
DFLD06   ICM   RF,15,BCFULL        RF=A(ELEMENT TO DELETE)                      
         BZ    EXITOK              NOTHING ON THIS LINE TO DELETE               
*                                                                               
         MVC   DELPOSN,FSRPOSN                                                  
         GOTO1 ADELEL,BOPARM,('FSRELQ',FSRRECD),(L'DELPOSN,DELPOSN)             
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* ADD LINE TO SCREEN                                                  *         
***********************************************************************         
         SPACE 1                                                                
ADDLINE  XR    RF,RF                                                            
         ICM   RF,3,CSCURDSP       CURRENT CURSOR POSITION                      
         XR    RE,RE                                                            
         D     RE,=F'80'                                                        
         MH    RF,=H'80'                                                        
         STH   RF,BOHALF1          DISPLACEMENT TO START OF THIS LINE           
*                                                                               
         LA    RF,FSRRECD+FSRFIRST START OF RECORD                              
         USING FSRELD,RF                                                        
         XR    RE,RE                                                            
         XR    R1,R1                                                            
*                                                                               
ALIN02   CLI   FSREL,0             REACHED END OF RECORD                        
         BE    ALIN06                                                           
         CLI   FSREL,FSRELQ        SCREEN FIELD ELEMENT                         
         BNE   ALIN04                                                           
*                                                                               
         CLC   FSRPOSN,BOHALF1     REACHED ELEMENTS FOR THIS LINE YET?          
         BL    ALIN04              NO                                           
         ICM   R1,3,FSRPOSN        ADD 1 LINE TO BUILD POSITION                 
         LA    R1,80(R1)           ...                                          
         STCM  R1,3,FSRPOSN        AND PUT IT BACK INTO THE RECORD              
*                                                                               
ALIN04   IC    RE,FSRLN                                                         
         LA    RF,0(RE,RF)                                                      
         B     ALIN02                                                           
*                                                                               
ALIN06   B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* DELETE LINE FROM SCREEN                                             *         
***********************************************************************         
         SPACE 1                                                                
DELLINE  XR    RF,RF                                                            
         ICM   RF,3,CSCURDSP       CURRENT CURSOR POSITION                      
         XR    RE,RE                                                            
         D     RE,=F'80'                                                        
         MH    RF,=H'80'                                                        
         STH   RF,BOHALF1          DISPLACEMENT TO START OF THIS LINE           
         LA    RF,80(RF)                                                        
         STH   RF,BOHALF2          DISPLACEMENT TO START OF NEXT LINE           
*                                                                               
         LA    R5,FSRRECD+FSRFIRST START OF RECORD                              
         USING FSRELD,R5                                                        
         XR    R1,R1                                                            
*                                                                               
DLIN02   CLI   FSREL,0             REACHED END OF RECORD                        
         BE    EXITOK                                                           
         CLI   FSREL,FSRELQ        SCREEN FIELD ELEMENT                         
         BNE   DLIN06                                                           
*                                                                               
         CLC   FSRPOSN,BOHALF1     REACHED ELEMENTS FOR THIS LINE YET?          
         BL    DLIN06              NO                                           
         CLC   FSRPOSN,BOHALF2                                                  
         BNL   DLIN04                                                           
*                                                                               
         MVC   DELPOSN,FSRPOSN     DELETE ELEMENT ON OFFENDING LINE             
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('FSRELQ',FSRRECD),       *        
               (L'DELPOSN,DELPOSN)                                              
         B     DLIN02              ELEMENTS WILL HAVE CLOSED UP                 
*                                                                               
DLIN04   XR    R1,R1                                                            
         ICM   R1,3,FSRPOSN        REMOVE 1 LINE FROM BUILD POSITION            
         SH    R1,=H'80'           ...                                          
         STCM  R1,3,FSRPOSN        AND PUT IT BACK INTO THE RECORD              
*                                                                               
DLIN06   XR    RF,RF                                                            
         IC    RF,FSRLN                                                         
         LA    R5,0(RF,R5)                                                      
         B     DLIN02                                                           
         DROP  R5                                                               
         SPACE 2                                                                
***********************************************************************         
* SELECT FIELD FOR ADDING TO SCREEN                                   *         
***********************************************************************         
         SPACE 1                                                                
SFFORAD  LA    RF,NSSAV                                                         
         USING SSAVD,RF                                                         
         OI    SNINDS1,SNIBLDKY+SNISEL+SNIPARMS                                 
         MVC   SRECACT,=AL1(R#FIELD,A#LST)                                      
         GOTO1 AGEN,BOPARM,OSES,SNTR                                            
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* EDIT FIELD CHARACTERISTICS ON SCREEN                                *         
***********************************************************************         
         SPACE 1                                                                
EFCONSC  XR    RF,RF                                                            
         ICM   RF,3,CSCURDSP       CURRENT CURSOR POSITION                      
         XR    RE,RE                                                            
         D     RE,=F'80'                                                        
         MH    RF,=H'80'                                                        
         STH   RF,BOHALF1          DISPLACEMENT TO START OF THIS LINE           
         LA    RF,80(RF)                                                        
         STH   RF,BOHALF2          DISPLACEMENT TO START OF NEXT LINE           
*                                                                               
         XC    BCFULL,BCFULL                                                    
         LA    RF,FSRRECD+FSRFIRST START OF RECORD                              
         USING FSRELD,RF                                                        
         XR    RE,RE                                                            
*                                                                               
EFLD02   CLI   FSREL,0             REACHED END OF RECORD                        
         BE    EFLD06                                                           
         CLI   FSREL,FSRELQ        SCREEN FIELD ELEMENT                         
         BNE   EFLD04                                                           
*                                                                               
         CLC   FSRPOSN,BOHALF1     REACHED ELEMENTS FOR THIS LINE YET?          
         BL    EFLD04              NO                                           
         CLC   FSRPOSN,CSCURDSP    IS THIS FIELD AFTER CURSOR?                  
         BH    EFLD06              YES                                          
         CLC   FSRPOSN,BOHALF2     REACHED ELEMENTS FOR NEXT LINE YET?          
         BNL   EFLD06              YES                                          
*                                                                               
         ST    RF,BCFULL           SAVE A(THIS ELEMENT)                         
*                                                                               
EFLD04   IC    RE,FSRLN                                                         
         LA    RF,0(RE,RF)                                                      
         B     EFLD02                                                           
*                                                                               
EFLD06   ICM   RF,15,BCFULL        RF=A(ELEMENT TO WITH NUMBER TO EDIT)         
         BZ    EXITOK              NOTHING ON THIS LINE TO EDIT                 
         MVC   FIELDN,FSRNUM                                                    
*                                                                               
         LA    RF,NSSAV                                                         
         USING SSAVD,RF                                                         
         OI    SNINDS1,SNIBLDKY+SNIPARMS                                        
         MVC   SRECACT,=AL1(R#FIELD,A#CHA)                                      
         GOTO1 AGEN,BOPARM,OSES,SNTR                                            
         B     EXITOK                                                           
         DROP  RF                                                               
***********************************************************************         
* SET START OF LIST ON SCREEN                                         *         
***********************************************************************         
         SPACE 1                                                                
SLST     LA    R5,FSRRECD+FSRFIRST START OF RECORD                              
         USING FSRELD,R5                                                        
*                                                                               
SLST02   CLI   FSREL,0             REACHED END OF RECORD                        
         BE    SLST06                                                           
         CLI   FSREL,FSRELQ        SCREEN FIELD ELEMENT                         
         BNE   SLST04                                                           
*                                                                               
         TM    FSRIND1,FSRSLST     OLD START OF LIST?                           
         BZ    SLST04              NO                                           
*                                                                               
         MVC   DELPOSN,FSRPOSN     DELETE OLD S.O.L. ELEMENT                    
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('FSRELQ',FSRRECD),       *        
               (L'DELPOSN,DELPOSN)                                              
*                                                                               
SLST04   XR    RF,RF                                                            
         IC    RF,FSRLN                                                         
         LA    R5,0(RF,R5)                                                      
         B     SLST02                                                           
         DROP  R5                                                               
*                                                                               
SLST06   XC    BOELEM(FSRLN2Q),BOELEM                                           
         PUSH  USING                                                            
         USING FSRELD,BOELEM                                                    
         MVI   FSREL,FSRELQ        BUILD A START OF LIST FSREL                  
         MVI   FSRLN,FSRLN2Q                                                    
         MVC   FSRPOSN,CSCURDSP                                                 
         OI    FSRIND1,FSRSLST                                                  
         GOTO1 AADDEL,BOPARM,FSRRECD                                            
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* SET END OF LIST ON SCREEN                                           *         
***********************************************************************         
         SPACE 1                                                                
ELST     LA    R5,FSRRECD+FSRFIRST START OF RECORD                              
         USING FSRELD,R5                                                        
*                                                                               
ELST02   CLI   FSREL,0             REACHED END OF RECORD                        
         BE    ELST06                                                           
         CLI   FSREL,FSRELQ        SCREEN FIELD ELEMENT                         
         BNE   ELST04                                                           
*                                                                               
         TM    FSRIND1,FSRELST     OLD START OF LIST?                           
         BZ    ELST04              NO                                           
*                                                                               
         MVC   DELPOSN,FSRPOSN     DELETE OLD E.O.L. ELEMENT                    
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('FSRELQ',FSRRECD),       *        
               (L'DELPOSN,DELPOSN)                                              
*                                                                               
ELST04   XR    RF,RF                                                            
         IC    RF,FSRLN                                                         
         LA    R5,0(RF,R5)                                                      
         B     ELST02                                                           
         DROP  R5                                                               
*                                                                               
ELST06   XC    BOELEM(FSRLN2Q),BOELEM                                           
         PUSH  USING                                                            
         USING FSRELD,BOELEM                                                    
         MVI   FSREL,FSRELQ        BUILD AN END OF LIST FSREL                   
         MVI   FSRLN,FSRLN2Q                                                    
         MVC   FSRPOSN,CSCURDSP                                                 
         OI    FSRIND1,FSRELST                                                  
         GOTO1 AADDEL,BOPARM,FSRRECD                                            
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* NTRSES OBJECT                                                       *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(BLOCK) COVERED BY SSAVD                                  *         
***********************************************************************         
         SPACE 1                                                                
NTRSES   LM    R0,R3,SVPARMS                                                    
         USING SSAVD,R2                                                         
         LA    RF,NSSTABL                                                       
         B     ITER                                                             
*                                                                               
NSSTABL  DC    AL1(SNTROUT),AL1(0,0,0),AL4(NTROUT)                              
         DC    AL1(SNTRIN),AL1(0,0,0),AL4(NTRIN)                                
         DC    AL1(SXITIN),AL1(0,0,0),AL4(XITIN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER                            *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING FSRRECD,GSRECKEY                                                 
NTROUT   CLI   FSRKPAGE,FF                                                      
         BNE   *+8                                                              
         MVI   SMPAGE,3            FILTER STUFF PAGE 3 OF FIELD RECORD          
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER (OUT)                    *         
***********************************************************************         
         SPACE 1                                                                
NTRIN    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER (BACK)                   *         
***********************************************************************         
         SPACE 1                                                                
XITIN    CLC   SRECACT,=AL1(R#FIELD,A#LST)                                      
         BNE   EXITOK              WRONG PREVIOUS RECORD                        
         TM    SXINDS1,SXISEL                                                   
         BZ    EXITOK              NOTHING SELECTED                             
*                                                                               
         LH    RF,GSDSPOPT                                                      
         LA    RF,FHDAD(RF)                                                     
         A     RF,ATWA                                                          
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,SDATA          NUMBER SELECTED PASSED HERE                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CURED (R0),(5,(RF)),0,DMCB=BOPARM,ALIGN=LEFT                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LIST OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS CURRENT KEY BUILD AREA                                     *         
* P4 HOLDS PREVIOUS KEY                                               *         
***********************************************************************         
         DROP  R2                                                               
         SPACE 1                                                                
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING FSRRECD,R2                                                       
LAST     USING FSRRECD,R3                                                       
         LA    RF,LISTABL                                                       
         B     ITER                                                             
*                                                                               
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST PAGE                                                 *         
***********************************************************************         
         SPACE 1                                                                
FLST     MVC   IOKEY(L'FSRKEY),THIS.FSRRECD                                     
         ICM   R1,15,=AL4(XIO11+XOGENDIR+XOHIGH)                                
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               FUCK UP ON THE READ HIGH                     
         B     NLST02                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     ICM   R1,15,=AL4(XIO11+XOSEQ+XOGENDIR)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               END OF FILE                                  
*                                                                               
NLST02   CLC   IOKEY(FSRKSYS-FSRRECD),THIS.FSRRECD                              
         BNE   EXITL                                                            
*                                                                               
         MVC   THIS.FSRKEY(FSRKLEN),IOKEY   WE WANT THIS KEY HERE...            
         B     EXITOK                                                           
         DROP  THIS,LAST                                                        
         SPACE 2                                                                
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
*                                                                               
       ++INCLUDE FACTRYTAB                                                      
*                                                                               
         DS    0D                                                               
DCLIST   DCDDL GE#KEY,10,L                                                      
         DCDDL GE#DATA,10,L                                                     
         DCDDL GE#LIS,10,L                                                      
         DCDDL GE#DLOAD,10,L                                                    
         DCDDL GE#REP,10,L                                                      
         DCDDL GE#BLUE,10,L                                                     
         DCDDL GE#RED,10,L                                                      
         DCDDL GE#PINK,10,L                                                     
         DCDDL GE#GRN,10,L                                                      
         DCDDL GE#TURQ,10,L                                                     
         DCDDL GE#YELLO,10,L                                                    
         DCDDL GE#WHITE,10,L                                                    
         DC    X'00'                                                            
         SPACE 2                                                                
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
SAVED    DSECT                                                                  
KYCFLD   DS    XL1                                                              
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
EFLAG    DS    XL1                                                              
ESCLEN   DS    XL1                                                              
ESCCHAR  DS    XL2                                                              
DELPOSN  DS    XL(L'FSRPOSN)                                                    
FIELDN   DS    XL2                                                              
FLDNUM   DS    XL2                                                              
MYBYTE   DS    XL1                                                              
MYBYTE1  DS    XL1                                                              
MYSCAN   DS    4CL(SCBLKLQ)                                                     
*                                                                               
DSLIST   DS    0D                                                               
GE@KEY   DS    XL10                                                             
GE@DATA  DS    XL10                                                             
GE@LIST  DS    XL10                                                             
GE@DLOAD DS    XL10                                                             
GE@REP   DS    XL10                                                             
UC@BLUE  DS    XL10                                                             
UC@RED   DS    XL10                                                             
UC@PINK  DS    XL10                                                             
UC@GRN   DS    XL10                                                             
UC@TURQ  DS    XL10                                                             
UC@YELLO DS    XL10                                                             
UC@WHITE DS    XL10                                                             
         SPACE 1                                                                
TABD     DSECT                                                                  
*        ORG   TRECOVR                                                          
SAVNUM   DS    XL(L'FSRNUM)                                                     
         SPACE 2                                                                
*        CTFILWORK                                                              
         PRINT OFF                                                              
       ++INCLUDE CTFILWORK                                                      
         PRINT ON                                                               
*        FACTRYEQUS                                                             
         PRINT OFF                                                              
       ++INCLUDE FACTRYEQUS                                                     
         PRINT ON                                                               
*        FACTRY                                                                 
         PRINT OFF                                                              
       ++INCLUDE FACTRY                                                         
         PRINT ON                                                               
*        FAFACTS                                                                
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
*        FASYSLSTD                                                              
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
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
*        DDFLDHDR                                                               
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008CTFIL11X  08/22/00'                                      
         END                                                                    
