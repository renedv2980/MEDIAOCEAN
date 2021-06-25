*          DATA SET CTFIL10X   AT LEVEL 020 AS OF 08/22/00                      
*&&      SET   NOP=N                                                            
*PHASE TA1310A                                                                  
         TITLE 'OBJECT VERSION OF FIELD RECORDS'                                
FIL10    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,CTFIL10*,R5,R6,R7,RR=RE                                        
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
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OREP),AL1(0,0,0),AL4(REPORT)                                 
         DC    AL1(ODLOAD),AL1(0,0,0),AL4(EXITH)                                
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     GOTO1 VDICTAT,BOPARM,C'LU  ',DCLIST,DSLISTU                            
         GOTO1 (RF),(R1),C'LL  ',,DSLISTL                                       
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
         USING FDRRECD,R2                                                       
         LA    RF,KEYTABL                                                       
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(KLAST),AL1(0,0,0),AL4(KEYLAST)                               
         DC    AL1(KMBUILD),AL1(0,0,0),AL4(KEYMBLD)                             
         DC    AL1(KLBUILD),AL1(0,0,0),AL4(KEYLBLD)                             
         DC    AL1(KHEIR),AL1(0,0,0),AL4(KEYHEIR)                               
         DC    AL1(KMASK),AL1(0,0,0),AL4(KEYMASK)                               
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
KFKVAL   XC    FDRKEY,FDRKEY       INITIALIZE KEY OF FIELD RECORD               
         MVI   FDRKMIN,FDRKMINQ    SET MINOR SYSTEM                             
         MVI   FDRKTYP,FDRKTYPQ    SET FIELD RECORD                             
         MVI   FDRKSUB,FF                                                       
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
KFKFVAL  XC    FDRKEY,FDRKEY       INITIALIZE KEY OF FIELD RECORD               
         MVI   FDRKMIN,FDRKMINQ    SET MINOR SYSTEM                             
         MVI   FDRKTYP,FDRKTYPQ    SET FIELD RECORD                             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR KEY OBJECT                                            *         
***********************************************************************         
         SPACE 1                                                                
KEYLAST  L     R1,SVPARMS4         R3=INVOKING ACTION                           
         LA    RF,KLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
*                             *** LAST TIME FOR KEY OBJECT ***                  
*                                 ------------------------                      
KLTABL   DC    AL1(KDIS),AL1(0,0,0),AL4(KLKDIS)      DISPLAY                    
         DC    AL1(KVAL),AL1(0,0,0),AL4(KLKVAL)      VALIDATE                   
         DC    AL1(KFDIS),AL1(0,0,0),AL4(KLKFDIS)    DISPLAY FILTER             
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KLKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR DISPLAY OF A KEY OBJECT                               *         
***********************************************************************         
         SPACE 1                                                                
KLKDIS   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A KEY OBJECT                              *         
***********************************************************************         
         SPACE 1                                                                
KLKVAL   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR DISPLAY OF A KEY FILTER                               *         
***********************************************************************         
         SPACE 1                                                                
KLKFDIS  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A KEY FILTER                              *         
***********************************************************************         
         SPACE 1                                                                
KLKFVAL  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* BUILD KEY FOR LIST NTRSES ENTRY                                     *         
* P4 = A(SSAVD)                                                       *         
***********************************************************************         
         SPACE 1                                                                
KEYLBLD  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* BUILD KEY FOR MAINTENANCE NTRSES ENTRY                              *         
* P4 = A(SSAVD)                                                       *         
***********************************************************************         
         SPACE 1                                                                
KEYMBLD  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* BUILD KEY FOR HEIRARCHICAL RECORD GET                               *         
* P3 = A(BUILD AREA FOR KEY TO INHERIT FROM)                          *         
* P4 = A(KEY WHICH WILL INHERIT FOR REFERENCE)                        *         
***********************************************************************         
         SPACE 1                                                                
KEYHEIR  L     R3,SVPARMS4                                                      
X        USING FDRRECD,R3                                                       
         MVI   FDRKMIN,FDRKMINQ    SEE IF INHERIT FROM PROGRAM LEVEL            
         MVI   FDRKTYP,FDRKTYPQ                                                 
         MVC   FDRKSYS,X.FDRKSYS                                                
         MVC   FDRKPRG,X.FDRKPRG                                                
         MVC   FDRKNUM,X.FDRKNUM                                                
         MVC   IOKEY(L'FDRKEY),FDRKEY                                           
         L     R1,=AL4(XOREAD+XOGENDIR)                                         
         GOTOX ('XIO',AGROUTS)                                                  
         BE    EXITOK                                                           
*                                                                               
         XC    FDRKEY,FDRKEY       SEE IF INHERIT FROM SYSTEM LEVEL             
         MVI   FDRKMIN,FDRKMINQ                                                 
         MVI   FDRKTYP,FDRKTYPQ                                                 
         MVC   FDRKSYS,X.FDRKSYS                                                
         MVC   FDRKNUM,X.FDRKNUM                                                
         MVC   IOKEY(L'FDRKEY),FDRKEY                                           
         L     R1,=AL4(XOREAD+XOGENDIR)                                         
         GOTOX ('XIO',AGROUTS)                                                  
         BE    EXITOK                                                           
*                                                                               
         XC    FDRKEY,FDRKEY                                                    
         B     EXITH               NO KEY TO INHERIT FROM                       
         DROP  X                                                                
         SPACE 2                                                                
***********************************************************************         
* BUILD KEY FOR MAINTENANCE NTRSES ENTRY                              *         
* P3 = A(KEY)                                                         *         
* P4 = A(RECORD MASK)                                                 *         
***********************************************************************         
         SPACE 1                                                                
KEYMASK  L     R1,SVPARMS4                                                      
*        NC    0(4,R1),=AL4(FFFFFFFF-MK#DEL)                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* REPORT OBJECT          - ADDED IN FOR THE REPORT OBJECT             *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(KEY)                                                           *         
* P4 HOLDS SUB-ACTION                                                 *         
***********************************************************************         
         SPACE 1                                                                
REPORT   LM    R0,R3,SVPARMS                                                    
         USING FDRRECD,R2                                                       
         LA    RF,REPTABL                                                       
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
REPTABL  DC    AL1(RPFIRST),AL1(0,0,0),AL4(REPFRST)                             
         DC    AL1(RPQINIT),AL1(0,0,0),AL4(RPINIT)                              
         DC    AL1(RDO),AL1(0,0,0),AL4(REPDO)                                   
         DC    AL1(RPLAST),AL1(0,0,0),AL4(REPLAST)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR REPORT  OBJECT                                       *         
***********************************************************************         
         SPACE 1                                                                
REPFRST  MVC   INSYSID,=CL2'CT'                                                 
         MVC   INPRGID,=CL2'NF'                                                 
         MVC   INJCLID,=CL2'FD'                                                 
         MVI   INPRTY1,C' '                                                     
         MVI   INPRTY2,C' '                                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* INITIALISE FAREPBLK                                                 *         
***********************************************************************         
         SPACE 1                                                                
RPINIT   DS    0H                                                               
         L     R3,AREP                                                          
         USING REPD,R3                                                          
         MVI   REPMAXL,75                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO THE REPORT                                                       *         
***********************************************************************         
         SPACE 1                                                                
REPDO    LA    R2,IOKEY                                                         
         USING FDRRECD,R2                                                       
         XC    FDRKEY,FDRKEY       INITIALIZE KEY OF FIELD RECORD               
         MVI   FDRKMIN,FDRKMINQ    SET MINOR SYSTEM                             
         MVI   FDRKTYP,FDRKTYPQ    SET FIELD RECORD                             
*                                                                               
         L     R1,=AL4(XOHI+XOGENDIR+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITOK                                                           
         B     REPDO02                                                          
*                                                                               
REPDO00  MVC   IOKEY,BCWORK                                                     
         L     R1,=AL4(XOHI+XOGENDIR+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,=AL4(XOSQ+XOGENDIR+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITOK                                                           
*                                                                               
REPDO02  CLI   FDRKMIN,FDRKMINQ                                                 
         BNE   EXITOK                                                           
         CLI   FDRKTYP,FDRKTYPQ                                                 
         BNE   EXITOK                                                           
*                                                                               
         MVC   BCWORK(L'IOKEY),IOKEY  SAVE KEY AS SEQUENCE CLOBBERED            
         L     R3,AREP                                                          
         USING REPD,R3                                                          
         L     RE,ASYSLST          DISPLAY A SYSTEM FIELD                       
         LA    RE,6(RE)                                                         
         USING SYSLSTD,RE          RE=A(SYSTEM LIST)                            
*                                                                               
REPDO04  CLI   0(RE),0             TEST E-O-T                                   
         BE    REPDO06                                                          
         CLC   SYSLNUM,FDRKSYS     MATCH ON SYSTEM NUMBER                       
         BE    *+12                                                             
         LA    RE,SYSLLEN(RE)      NO - BUMP TO NEXT TABLE ENTRY                
         B     REPDO04                                                          
         SPACE 1                                                                
         MVC   REPP1(L'SYSLNAME),SYSLNAME                                       
         B     REPDO08                                                          
         DROP  RE                                                               
*                                                                               
REPDO06  CURED FDRKSYS,(3,REPP1),0,DMCB=BOPARM,ALIGN=LEFT                       
*                                                                               
REPDO08  OC    FDRKPRG,FDRKPRG     PROGRAM FOR THIS FIELD?                      
         B     REPDO18             NO                                           
*                                                                               
         L     R1,ASYSFAC          HOLDS SELIST ENTRY                           
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FDRKSYS                                                  
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     REPDO16             CAN'T FIND SYSTEM FOR THIS RECORD            
*                                                                               
         L     R1,SEPGMS                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1          FIND PGMLST ENTRY                            
*                                                                               
REPDO10  CLC   PGMNUM,FDRKPRG                                                   
         BNE   REPDO12                                                          
         CLI   PGMCTRY,0           MATCH ON COUNTRY CODE                        
         BE    REPDO14                                                          
         CLC   PGMCTRY,CUCTRY                                                   
         BE    REPDO14                                                          
*                                                                               
REPDO12  BXLE  R1,RE,REPDO10                                                    
         B     REPDO16                                                          
*                                                                               
REPDO14  MVC   REPP1+10(L'PGMNAME),PGMNAME                                      
         B     REPDO18                                                          
*                                                                               
REPDO16  CURED FDRKPRG,(3,REPP1+10),0,DMCB=BOPARM,ALIGN=LEFT                    
         DROP  R1                                                               
*                                                                               
REPDO18  XR    RF,RF               DISPLAY FIELD NUMBER                         
         ICM   RF,3,FDRKNUM                                                     
         CURED (RF),(6,REPP1+40),0,DMCB=BOPARM,ALIGN=LEFT                       
*                                                                               
         OC    FDRKREC,FDRKREC     DISPLAY RECORD FIELD                         
         BZ    REPDO22                                                          
         XR    RF,RF                                                            
         ICM   RF,8,FDRKSYS                                                     
         ICM   RF,4,FDRKPRG                                                     
         ICM   RF,2,FDRKREC                                                     
         GOTOX AGEN,BOPARM,ORTYPE,RTXDIS,(RF)                                   
         BE    REPDO20                                                          
         GOTOX VHEXOUT,BODMCB,FDRKREC,REPP1+30,L'FDRKREC,0                      
                                                                                
REPDO20  MVC   REPP1+30(10),FVIFLD                                              
*                                                                               
REPDO22  GOTO1 VREPORT,REPD         PRINT THE LINE                              
         B     REPDO00                                                          
         SPACE 1                                                                
***********************************************************************         
* AFTER REPORT IS DONE                                                *         
***********************************************************************         
         SPACE 1                                                                
REPLAST  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
         DROP  R2,R3                                                            
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
         USING FDRRECD,R2                                                       
         LA    RF,TABLREC                                                       
         B     ITER                ITERATE TABLE                                
*                                                                               
TABLREC  DC    AL1(RFIRST),AL1(0,0,0),AL4(RECFRST)                              
         DC    AL1(RLAST),AL1(0,0,0),AL4(RECLAST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT                                        *         
***********************************************************************         
         SPACE 1                                                                
RECFRST  L     R1,SVPARMS4         R1=INVOKER`S VERB                            
         LA    RF,RFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
RFTABL   DC    AL1(RDIS),AL1(0,0,0),AL4(RFDISF)                                 
         DC    AL1(RVAL),AL1(0,0,0),AL4(RFVALF)                                 
         DC    AL1(RADD),AL1(0,0,0),AL4(RFADD)                                  
         DC    AL1(RDEL),AL1(0,0,0),AL4(RFDEL)                                  
         DC    AL1(RRES),AL1(0,0,0),AL4(RFRES)                                  
         DC    AL1(RWRT),AL1(0,0,0),AL4(RFWRT)                                  
         DC    AL1(RCPY),AL1(0,0,0),AL4(RFCPY)                                  
         DC    AL1(RREN),AL1(0,0,0),AL4(RFREN)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - DISPLAY                              *         
***********************************************************************         
         SPACE 1                                                                
RFDISF   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - VALIDATE                             *         
***********************************************************************         
         SPACE 1                                                                
RFVALF   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - ADD                                  *         
***********************************************************************         
         SPACE 1                                                                
RFADD    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - DELETE                               *         
***********************************************************************         
         SPACE 1                                                                
RFDEL    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - RESTORE                              *         
***********************************************************************         
         SPACE 1                                                                
RFRES    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - WRITE TO FILE                        *         
***********************************************************************         
         SPACE 1                                                                
RFWRT    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - COPY RECORD                          *         
***********************************************************************         
         SPACE 1                                                                
RFCPY    GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FDRELQ',(R2)),0                  
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(R1)                                                        
         USING FDRELD,RF                                                        
         MVC   FDRNUM,FDRKNUM      COPY KEY ELEMENT NUMBER INTO FILE            
*                                                                               
TEMP     USING FDRKSTAT,GSRECSTA                                                
         NI    TEMP.FDRKSTAT,FF-(FDRKKEY+FDRKFILE+FDRKSTT+FDRKTSAR)             
         TM    FDRLVL,FDRIKEY                                                   
         BZ    *+8                                                              
         OI    TEMP.FDRKSTAT,FDRKKEY                                            
         TM    FDRLVL,FDRIFILE                                                  
         BZ    *+8                                                              
         OI    TEMP.FDRKSTAT,FDRKFILE                                           
         TM    FDRLVL,FDRISTT                                                   
         BZ    *+8                                                              
         OI    TEMP.FDRKSTAT,FDRKSTT                                            
         TM    FDRLVL,FDRITSAR                                                  
         BZ    *+8                                                              
         OI    TEMP.FDRKSTAT,FDRKTSAR                                           
         TM    FDRLVL,FDRIUSR                                                   
         BZ    *+8                                                              
         OI    TEMP.FDRKSTAT,FDRKUSR                                            
         DROP  TEMP                                                             
*                                                                               
         TM    FDRINDS2,FDR2FILT   FILTER ELEMENT ATTACHED?                     
         BZ    EXITOK                                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FLTRLQ',(R2)),0                  
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(R1)                                                        
         MVC   FDRNUM,FDRKNUM      COPY KEY ELEMENT NUMBER INTO FILE            
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - RENAME RECORD                        *         
***********************************************************************         
         SPACE 1                                                                
RFREN    GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FDRELQ',(R2)),0                  
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(R1)                                                        
         USING FDRELD,RF                                                        
         MVC   FDRNUM,FDRKNUM      COPY KEY ELEMENT NUMBER INTO FILE            
*                                                                               
TEMP     USING FDRKSTAT,GSRECSTA                                                
         NI    TEMP.FDRKSTAT,FF-(FDRKKEY+FDRKFILE+FDRKSTT+FDRKTSAR)             
         TM    FDRLVL,FDRIKEY                                                   
         BZ    *+8                                                              
         OI    TEMP.FDRKSTAT,FDRKKEY                                            
         TM    FDRLVL,FDRIFILE                                                  
         BZ    *+8                                                              
         OI    TEMP.FDRKSTAT,FDRKFILE                                           
         TM    FDRLVL,FDRISTT                                                   
         BZ    *+8                                                              
         OI    TEMP.FDRKSTAT,FDRKSTT                                            
         TM    FDRLVL,FDRITSAR                                                  
         BZ    *+8                                                              
         OI    TEMP.FDRKSTAT,FDRKTSAR                                           
         TM    FDRLVL,FDRIUSR                                                   
         BZ    *+8                                                              
         OI    TEMP.FDRKSTAT,FDRKUSR                                            
         DROP  TEMP                                                             
*                                                                               
         TM    FDRINDS2,FDR2FILT   FILTER ELEMENT ATTACHED?                     
         BZ    EXITOK                                                           
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FLTRLQ',(R2)),0                  
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(R1)                                                        
         MVC   FDRNUM,FDRKNUM      COPY KEY ELEMENT NUMBER INTO FILE            
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT                                         *         
***********************************************************************         
         SPACE 1                                                                
RECLAST  L     R1,SVPARMS4         R1=INVOKER`S VERB                            
         LA    RF,RLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
RLTABL   DC    AL1(RDIS),AL1(0,0,0),AL4(RLDIS)                                  
         DC    AL1(RVAL),AL1(0,0,0),AL4(RLVAL)                                  
         DC    AL1(RADD),AL1(0,0,0),AL4(RLADD)                                  
         DC    AL1(RDEL),AL1(0,0,0),AL4(RLDEL)                                  
         DC    AL1(RRES),AL1(0,0,0),AL4(RLRES)                                  
         DC    AL1(RWRT),AL1(0,0,0),AL4(RLWRT)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - DISPLAY                               *         
***********************************************************************         
         SPACE 1                                                                
RLDIS    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - VALIDATE                              *         
***********************************************************************         
         SPACE 1                                                                
RLVAL    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - ADD                                   *         
***********************************************************************         
         SPACE 1                                                                
RLADD    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - DELETE                                *         
***********************************************************************         
         SPACE 1                                                                
RLDEL    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - RESTORE                               *         
***********************************************************************         
         SPACE 1                                                                
RLRES    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - WRITE TO FILE                         *         
***********************************************************************         
         SPACE 1                                                                
RLWRT    L     RF,ACOM                                                          
         ICM   RF,15,CGETFLD-COMFACSD(RF)                                       
         GOTO1 (RF),BOPARM,GFUREC,AIOREC                                        
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
         USING FDRRECD,R2                                                       
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
         USING FDRRECD,R2          R2 HOLDS A(RECORD)                           
         L     R3,AFDREL                                                        
         USING FDRELD,R3           R3=A(FDREL ON RECORD)                        
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
KNOWTAB  DC    AL2(00001),AL4(SYSDTA)    SYSTEM                                 
         DC    AL2(00002),AL4(PRGDTA)    PROGRAM                                
         DC    AL2(00003),AL4(RCODTA)    RECORD                                 
         DC    AL2(00004),AL4(FNODTA)    FIELD NUMBER                           
         DC    AL2(00005),AL4(TDRDTA)    TAG DICTIONARY REFERENCE               
         DC    AL2(00006),AL4(IFLDTA)    LENGTH OF INPUT FIELD                  
         DC    AL2(00007),AL4(MNLDTA)    MINIMUM LENGTH OF INPUT DATA           
         DC    AL2(00008),AL4(MXLDTA)    MAXIMUM LENGTH OF INPUT DATA           
         DC    AL2(00009),AL4(TFCDTA)    TAG FIELD COLOUR                       
         DC    AL2(00010),AL4(IFCDTA)    INPUT FIELD COLOUR                     
         DC    AL2(00011),AL4(TFHDTA)    TAG FIELD HIGHLIGHT                    
         DC    AL2(00012),AL4(IFHDTA)    INPUT FIELD HIGHLIGHT                  
         DC    AL2(00013),AL4(TFNDTA)    TAG FIELD CASE                         
         DC    AL2(00014),AL4(IFNDTA)    INPUT FIELD CASE                       
         DC    AL2(00015),AL4(TFIDTA)    TAG FIELD INTENSITY                    
         DC    AL2(00016),AL4(IFIDTA)    INPUT FIELD INTENSITY                  
         DC    AL2(00017),AL4(RDVDTA)    REDISPLAY IF VALID                     
         DC    AL2(00064),AL4(RDFDTA)    REDISPLAY IF VALID (FILTER)            
         DC    AL2(00018),AL4(STFDTA)    SUPPRESS TAG FIELD                     
         DC    AL2(00019),AL4(TDDDTA)    TAG DD FIELD DISPLAY ROUTINE           
         DC    AL2(00020),AL4(PIFDTA)    INPUT FIELD IS PROTECTED               
         DC    AL2(00021),AL4(FIKDTA)    FIELD IN KEY                           
         DC    AL2(00022),AL4(FDODTA)    FIELD IS DDS ONLY                      
         DC    AL2(00023),AL4(CWDDTA)    COLUMN WIDTH                           
         DC    AL2(00024),AL4(DWDDTA)    DATA WIDTH                             
         DC    AL2(00025),AL4(DFCDTA)    DISP TO FIELD IN COLUMN                
         DC    AL2(00026),AL4(CH1DTA)    COLUMN HEADING 1                       
         DC    AL2(00027),AL4(CD1DTA)    COLUMN HEADING 1 DISPLAY               
         DC    AL2(00028),AL4(CH2DTA)    COLUMN HEADING 2                       
         DC    AL2(00029),AL4(CD2DTA)    COLUMN HEADING 2 DISPLAY               
         DC    AL2(00030),AL4(EXITOK)    OPTION HELP 1                          
         DC    AL2(00031),AL4(EXITOK)    OPTION HELP 2                          
         DC    AL2(00032),AL4(HLPDTA)    HELP NUMBER                            
         DC    AL2(00033),AL4(SCRDTA)    SECURITY NUMBER                        
         DC    AL2(00034),AL4(EXITOK)    RECORD LEVEL OF DATA                   
         DC    AL2(00035),AL4(FRQDTA)    FIELD IS REQUIRED                      
         DC    AL2(00036),AL4(FDFDTA)    FIELD HAS DEFAULT VALUE                
         DC    AL2(00065),AL4(FFDDTA)    FILTER HAS DEFAULT VALUE               
         DC    AL2(00066),AL4(NOVDTA)    FILTER HAS NO OVERRIDES                
         DC    AL2(00037),AL4(EXITOK)    COUNTRY BITMASK                        
         DC    AL2(00038),AL4(FFVDTA)    FIELD IS FILTERABLE                    
         DC    AL2(00039),AL4(FDRDTA)    FILTER TAG DD REF                      
         DC    AL2(00040),AL4(FFLDTA)    FILTER FIELD LENGTH                    
         DC    AL2(00041),AL4(MNFDTA)    MIN FILTER FIELD INPUT                 
         DC    AL2(00042),AL4(MXFDTA)    MAX FILTER FIELD INPUT                 
         DC    AL2(00043),AL4(FTCDTA)    FILTER TAG FIELD COLOUR                
         DC    AL2(00044),AL4(FICDTA)    FILTER INPUT FIELD COLOUR              
         DC    AL2(00045),AL4(FFHDTA)    FILTER TAG FIELD HIGHLIGHT             
         DC    AL2(00046),AL4(FIHDTA)    FILTER INPUT FIELD HIGHLIGHT           
         DC    AL2(00047),AL4(FFCDTA)    FILTER TAG FIELD COLOUR                
         DC    AL2(00048),AL4(CIFDTA)    FILTER INPUT FIELD COLOUR              
         DC    AL2(00049),AL4(FFIDTA)    FILTER TAG FIELD INTENSITY             
         DC    AL2(00050),AL4(IFFDTA)    FILTER INPUT FIELD INTENSITY           
         DC    AL2(00051),AL4(XFLDTA)    FILTER SAVE LENGTH                     
         DC    AL2(00052),AL4(FDDDTA)    FILTER TAG DD REF DISPLAY              
         DC    AL2(00053),AL4(NOTDTA)    VALID FILTER VALUE - NOT               
         DC    AL2(00054),AL4(NEQDTA)    VALID FILTER VALUE - EQUAL TO          
         DC    AL2(00055),AL4(NGTDTA)    VALID FILTER VALUE - GREATER           
         DC    AL2(00056),AL4(NLTDTA)    VALID FILTER VALUE - LESS THAN         
         DC    AL2(00057),AL4(NDFDTA)    VALID FILTER DEFAULT VALUE             
         DC    AL2(00058),AL4(FFQDTA)    FILTER FIELD IS REQUIRED               
         DC    AL2(00059),AL4(EOPDTA)    SELECT TO END OF PAGE VALID            
         DC    AL2(00060),AL4(EOLDTA)    SELECT TO END OF LIST VALID            
         DC    AL2(00061),AL4(RNGDTA)    RANGE VALID FOR FILTERING              
         DC    AL2(00062),AL4(LSTDTA)    LIST VALID FOR FILTERING               
         DC    AL2(00063),AL4(OWNDTA)    OWNER HANDLES FILTERING                
         DC    AL2(00116),AL4(TSTDTA)    TEST VERSION                           
         DC    AL2(00150),AL4(CTRDTA)    COUNTRY FILTER                         
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL10    CSECT                                                                  
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
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFDFDIS)    DISPLAY FILTER             
         DC    AL1(DFVAL),AL1(0,0,0),AL4(DFDFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DFDDIS   XC    AFDREL,AFDREL                                                    
         XC    AFLTRL,AFLTRL                                                    
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FDRELQ',FDRRECD),0               
         CLI   12(R1),0                                                         
         BNE   DFDDIS02            NO FDREL FOUND                               
         L     RF,12(R1)                                                        
         ST    RF,AFDREL                                                        
*                                                                               
DFDDIS02 GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FLTRLQ',FDRRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITOK              NO FLTRL FOUND                               
         L     RF,12(R1)                                                        
         ST    RF,AFLTRL                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   XC    AFDREL,AFDREL                                                    
         XC    AFLTRL,AFLTRL                                                    
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FLTRLQ',FDRRECD),0               
         CLI   12(R1),0                                                         
         BNE   DFVAL02             NO FLTRL FOUND                               
         L     RF,12(R1)                                                        
         ST    RF,AFLTRL                                                        
         MVC   FDRNUM-FDRELD(L'FDRNUM,RF),FDRKNUM                               
*                                                                               
DFVAL02  GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FDRELQ',FDRRECD),0               
         CLI   12(R1),0                                                         
         BNE   DFVAL04             NO FDREL FOUND                               
         L     RF,12(R1)                                                        
         ST    RF,AFDREL                                                        
         MVC   FDRNUM-FDRELD(L'FDRNUM,RF),FDRKNUM                               
         B     EXITOK              ALREADY HAVE A FDREL ON RECORD               
*                                                                               
TEMP     USING FDRELD,BOELEM                                                    
DFVAL04  XC    TEMP.FDREL(FDRLNQ),TEMP.FDREL                                    
         MVI   TEMP.FDREL,FDRELQ                                                
         MVI   TEMP.FDRLN,FDRLNQ                                                
         MVC   TEMP.FDRNUM,FDRKNUM                                              
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),FDRRECD,TEMP.FDRELD                
         CLI   12(R1),0                                                         
         BE    *+6                 ERROR IN HELLO                               
         DC    H'0'                                                             
         L     RF,16(R1)           A(ELEMENT) HERE AFTER PUT                    
         ST    RF,AFDREL                                                        
         B     EXITOK              FDREL ADDED TO RECORD                        
         DROP  TEMP                                                             
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A DATA FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
DFDFDIS  XC    AFDREL,AFDREL                                                    
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FDRELQ',FDRRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITOK              NO FDREL FOUND                               
         L     RF,12(R1)                                                        
         ST    RF,AFDREL                                                        
         B     EXITOK              ALREADY HAVE A FDREL ON RECORD               
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA FILTER                            *         
***********************************************************************         
         SPACE 1                                                                
DFDFVAL  XC    AFDREL,AFDREL                                                    
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FDRELQ',FDRRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITOK              NO FDREL FOUND                               
         L     RF,12(R1)                                                        
         ST    RF,AFDREL                                                        
         MVC   FDRNUM-FDRELD(L'FDRNUM,RF),FDRKNUM                               
         B     EXITOK              ALREADY HAVE A FDREL ON RECORD               
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR DATA OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
DTALAST  L     R1,SVPARMS3         VERB IN R1                                   
         LA    RF,DLTABL                                                        
         B     ITER                ITERATE TABLE                                
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
* LAST TIME FOR VALIDATE OF A DATA OBJECT (IN CASE OF COPY)           *         
***********************************************************************         
         SPACE 1                                                                
DLDVAL   ICM   R3,15,AFDREL                                                     
         BZ    DIE                                                              
         MVC   FDRNUM,FDRKNUM                                                   
         ICM   R4,15,AFLTRL                                                     
         BZ    EXITOK                                                           
FL       USING FDRELD,R4                                                        
*        MVC   FL.FDRNUM,FDRNUM    FIELD NUMBER                                 
*        MVC   FL.FDRHLP,FDRHLP    HELP NUMBER                                  
*        MVC   FL.FDRSEC,FDRSEC    SECURITY REF NUMBER                          
*        MVC   FL.FDRCTRY,FDRCTRY  COUNTRY BITMASK                              
*        MVC   FL.FDRLVL,FDRLVL    RECORD LEVEL                                 
*        MVC   FL.FDRINDS1,FDRINDS1                                             
*        MVC   FL.FDRINDS2,FDRINDS2                                             
         B     EXITOK                                                           
         DROP  FL                                                               
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
         EJECT                                                                  
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
         CLC   SYSLNUM,FDRKSYS     MATCH ON SYSTEM NUMBER                       
         BE    *+12                                                             
         LA    RE,SYSLLEN(RE)      NO - BUMP TO NEXT TABLE ENTRY                
         B     DSYS02                                                           
         SPACE 1                                                                
         MVC   FVIFLD(L'SYSLNAME),SYSLNAME                                      
         B     EXITOK                                                           
         DROP  RE                                                               
         SPACE 1                                                                
DSYS04   CURED FDRKSYS,(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                      
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
VSYS06   MVC   FDRKSYS,SYSLNUM     SYSTEM NUMBER                                
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
VFSYS06  MVC   FDRKSYS,SYSLNUM     SYSTEM NUMBER INTO KEY                       
         MVC   FLTIFLD(L'SYSLNUM),SYSLNUM    - AND FILTER                       
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* DO SYSTEM FILTERING                                                 *         
***********************************************************************         
         SPACE 1                                                                
DOFTSYS  CLC   FDRKSYS,FLTIFLD                                                  
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
DISPRG   OC    FDRKPRG,FDRKPRG     WAS A PROGRAM ENTERED?                       
         BZ    EXITOK              NO                                           
         SPACE 1                                                                
         L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FDRKSYS                                                  
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     DPRG08                                                           
         SPACE 1                                                                
         L     R1,SEPGMS                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1          FIND PGMLST ENTRY                            
DPRG02   CLC   PGMNUM,FDRKPRG                                                   
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
DPRG08   CURED FDRKPRG,(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                      
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PROGRAM FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
VALPRG   MVI   FDRKPRG,0           DEFAULT IS SYSTEM LEVEL ONLY                 
         CLI   FVILEN,0            TRYING TO SET TO SYSTEM LEVEL ONLY?          
         BE    EXITOK              YES                                          
         SPACE 1                                                                
         L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FDRKSYS                                                  
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
VPRG08   MVC   FDRKPRG,PGMNUM      PROGRAM NUMBER                               
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
DFLTPRG  OC    FLTIFLD(L'FDRKPRG),FLTIFLD PROGRAM ENTERED?                      
         BZ    EXITOK              NO                                           
         SPACE 1                                                                
         L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FDRKSYS                                                  
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
VFLTPRG  MVI   FDRKPRG,0           DEFAULT IS SYSTEM LEVEL ONLY                 
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
         CLC   SEOVSYS,FDRKSYS                                                  
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
VFPRG08  MVC   FDRKPRG,PGMNUM      PROGRAM NUMBER                               
         MVC   FLTIFLD(L'PGMNUM),PGMNUM                                         
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR PROGRAM FIELD                                      *         
***********************************************************************         
DOFTPRG  CLC   FDRKPRG,FLTIFLD                                                  
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
DISRCO   OC    FDRKREC,FDRKREC     GLOBAL FIELD?                                
         BZ    EXITOK                                                           
         ICM   RF,8,FDRKSYS                                                     
         ICM   RF,4,FDRKPRG                                                     
         ICM   RF,2,FDRKREC                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXDIS,(RF)                                   
         BE    EXITOK                                                           
         CURED FDRKREC,(5,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALRCO   MVI   FDRKREC,0           IF NO RECORD                                 
         CLI   FVILEN,0            FIELD SET AT LEVELS ABOVE                    
         BE    EXITOK                                                           
         SPACE 1                                                                
         OC    FDRKPRG,FDRKPRG     TRYING TO OVERRIDE RECORD - MUST             
         BZ    EXITNV              INPUT THE PROGRAM                            
         SPACE 1                                                                
         XR    RF,RF                                                            
         ICM   RF,8,FDRKSYS                                                     
         ICM   RF,4,FDRKPRG                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXVAL,(RF)                                   
         BNE   EXITL               NOT FOUND                                    
         L     RF,8(R1)                                                         
         STCM  RF,2,FDRKREC                                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD FILTER FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DFLTRCO  OC    FLTIFLD(L'FDRKREC),FLTIFLD RECORD FIELD?                         
         BZ    EXITOK                                                           
         ICM   RF,8,FDRKSYS                                                     
         ICM   RF,4,FDRKPRG                                                     
         ICM   RF,2,FLTIFLD                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXDIS,(RF)                                   
         BE    EXITOK                                                           
         XR    RF,RF                                                            
         IC    RF,FLTIFLD                                                       
         CURED (RF),(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD FILTER FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
VFLTRCO  OC    FDRKPRG,FDRKPRG     TRYING TO FILTER RECORD - MUST               
         BZ    EXITNV              INPUT THE PROGRAM                            
         SPACE 1                                                                
         XR    RF,RF                                                            
         ICM   RF,8,FDRKSYS                                                     
         ICM   RF,4,FDRKPRG                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXVAL,(RF)                                   
         BNE   EXITL               NOT FOUND                                    
         L     RF,8(R1)                                                         
         STCM  RF,2,FDRKREC                                                     
         STCM  RF,2,FLTIFLD                                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR RECORD                                             *         
***********************************************************************         
         SPACE 1                                                                
DOFTRCO  CLC   FDRKREC,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR FIELD NUMBER                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
FNODTA   LA    RF,FNOTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
FNOTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFNO)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFNO)                                 
         DC    AL1(DHED),AL1(0,0,0),AL4(HEDFNO)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFNO)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTFNO)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTFNO)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A FIELD NUMBER FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
DISFNO   CURED FDRKNUM,(5,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                      
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE A FIELD NUMBER FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
VALFNO   TM    FVIIND,FVINUM       FIELD NUMBER MUST BE NUMERIC                 
         BZ    EXITNOTN                                                         
         ICM   RF,15,BCFULL        NUMERIC VALUE IS IN BCFULL                   
         LTR   RF,RF                                                            
         BZ    EXITNV              NUMBER CANNOT BE ZERO                        
         SPACE 1                                                                
         STCM  RF,3,FDRKNUM        FIELD NUMBER HERE                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* CREATE A FIELD NUMBER HEADLINE FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
HEDFNO   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A FIELD NUMBER FILTER FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
DFLTFNO  XR    RF,RF               OUTPUT FIELD NUMBER FILTER                   
         ICM   RF,3,FLTIFLD                                                     
         BZ    EXITOK                                                           
         CURED (RF),(5,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A FIELD NUMBER FILTER FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
VFLTFNO  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         TM    FVIIND,FVINUM       FIELD NUMBER MUST BE NUMERIC                 
         BZ    EXITNOTN                                                         
         ICM   RF,15,BCFULL        NUMERIC VALUE IS IN BCFULL                   
         LTR   RF,RF                                                            
         BZ    EXITNV              NUMBER CANNOT BE ZERO                        
         SPACE 1                                                                
         STCM  RF,3,FDRKNUM        FIELD NUMBER HERE                            
         STCM  RF,3,FLTIFLD                                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR RECORD                                             *         
***********************************************************************         
         SPACE 1                                                                
DOFTFNO  CLC   FDRKNUM,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR TAG DICTIONARY REFERENCE                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
TDRDTA   LA    RF,TDRTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
TDRTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTDR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTDR)                                 
         DC    AL1(DHED),AL1(0,0,0),AL4(HEDTDR)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTTDR)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTTDR)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A TAG DICTIONARY REFERENCE FIELD                            *         
***********************************************************************         
         SPACE 1                                                                
DISTDR   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
*                                                                               
         OC    FDRRTAG,FDRRTAG     TAG?                                         
         BZ    EXITOK                                                           
         MVC   BCFULL,FDRRTAG      TAG DICTIONARY REFERENCE                     
         XR    RF,RF                                                            
         IC    RF,FDRKSYS                                                       
         GOTOX ('DISDIC',AGROUTS),BOPARM,(RF)                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A TAG DICTIONARY REFERENCE FIELD                           *         
***********************************************************************         
         SPACE 1                                                                
VALTDR   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
*                                                                               
         XC    FDRRTAG,FDRRTAG                                                  
         CLI   FVILEN,0                                                         
         BNE   VTDR02                                                           
         TM    FDRINDS1,FDR1XTAG   SUPPRESS TAG?                                
         BZ    EXITNO              NO - FIELD NEEDS A DICTIONARY REF            
         B     EXITOK                                                           
*                                                                               
VTDR02   XR    RF,RF                                                            
         IC    RF,FDRKSYS                                                       
         GOTOX ('VALDIC',AGROUTS),BOPARM,(RF) VALIDATE TAG REF                  
         BNE   EXITL               VALDIC SETS OWN ERROR MESSAGES               
         SPACE 1                                                                
         MVC   FDRRTAG,BCFULL      DATA DICTIONARY REF BACK IN BCFULL           
         MVC   FDRTDEFL,BCFULL+3   SET LENGTH OF TAG                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* CREATE A TAG DICTIONARY REFERENCE HEADLINE FIELD                    *         
***********************************************************************         
         SPACE 1                                                                
HEDTDR   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A TAG DICTIONARY REFERENCE FILTER FIELD                     *         
***********************************************************************         
         SPACE 1                                                                
DFLTTDR  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A TAG DICTIONARY REFERENCE FILTER FIELD                    *         
***********************************************************************         
         SPACE 1                                                                
VFLTTDR  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR INPUT FIELD LENGTH                                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
IFLDTA   LA    RF,IFLTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
IFLTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISIFL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALIFL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LENGTH OF THE INPUT FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
DISIFL   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         CURED FDRFDEFL,(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE LENGTH OF THE INPUT FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
VALIFL   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
         TM    FVIIND,FVINUM       FIELD LENGTH MUST BE A NUMBER                
         BZ    EXITNOTN                                                         
         ICM   RF,15,BCFULL                                                     
         BZ    EXITNV                                                           
         CH    RF,=H'255'          INPUT LENGTH CONSTRAINED TO BE <255          
         BH    EXITNV                                                           
         STC   RF,FDRFDEFL                                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR MINIMUM INPUT LENGTH                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
MNLDTA   LA    RF,MNLTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
MNLTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISMNL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALMNL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY MINIMUM IMPUT LENGTH                                        *         
***********************************************************************         
         SPACE 1                                                                
DISMNL   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         CLI   FDRFMIN,0           MINIMUM LENGTH - OPTIONAL                    
         BE    EXITOK                                                           
         CURED FDRFMIN,(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE MINIMUM INPUT LENGTH                                       *         
***********************************************************************         
         SPACE 1                                                                
VALMNL   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
         MVI   FDRFMIN,0           MINIMUM INPUT LENGTH IS OPTIONAL             
         CLI   FVILEN,0            DEFAULT IS 0                                 
         BE    EXITOK                                                           
*                                                                               
         TM    FVIIND,FVINUM       ANY INPUT MUST BE NUMERIC                    
         BZ    EXITNOTN                                                         
         ICM   RF,15,BCFULL        IF THEY TYPE IN A 0                          
         BZ    EXITOK                                                           
         CLM   RF,1,FDRFDEFL       INPUT LENGTH CAN`T BE < FIELD LENGTH         
         BH    EXITNV                                                           
         STC   RF,FDRFMIN                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR MAXIMUM INPUT LENGTH                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
MXLDTA   LA    RF,MXLTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
MXLTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISMXL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALMXL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY MAXIMUM INPUT LENGTH                                        *         
***********************************************************************         
         SPACE 1                                                                
DISMXL   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         CLI   FDRFMAX,0           MAXIMUM LENGTH - OPTIONAL                    
         BE    EXITOK                                                           
         CURED FDRFMAX,(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE MAXIMUM INPUT LENGTH                                       *         
***********************************************************************         
         SPACE 1                                                                
VALMXL   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
         MVI   FDRFMAX,0           MAXIMUM INPUT LENGTH IS OPTIONAL             
         CLI   FVILEN,0            DEFAULT IS 0                                 
         BE    EXITOK                                                           
         SPACE 1                                                                
         TM    FVIIND,FVINUM       ANY INPUT MUST BE NUMERIC                    
         BZ    EXITNOTN                                                         
         ICM   RF,15,BCFULL        IF THEY TYPE IN A 0                          
         BZ    EXITOK                                                           
         CLM   RF,1,FDRFDEFL       INPUT LENGTH CAN`T BE < FIELD LENGTH         
         BH    EXITNV                                                           
         STC   RF,FDRFMAX                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR TAG FIELD COLOUR                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
TFCDTA   LA    RF,TFCTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
TFCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTFC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTFC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY TAG FIELD COLOUR                                            *         
***********************************************************************         
         SPACE 1                                                                
DISTFC   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         IC    RF,FDRTFHXT                                                      
         SLL   RF,32-3                                                          
         SRL   RF,32-3             RF CONTAINS ONLY COLOUR INFO                 
         LTR   RF,RF                                                            
         BZ    EXITOK              NOTHING SET                                  
*                                                                               
         BCTR  RF,0                MAKE ZERO BASED                              
         MH    RF,=Y(L'LC@BLUE)    INDEX INTO LIST OF COLOURS                   
         LA    RF,LC@BLUE(RF)                                                   
         MVC   FVIFLD(L'LC@BLUE),0(RF)                                          
         B     EXITOK              COLOUR IS NOW IN FVIFLD                      
         SPACE 2                                                                
***********************************************************************         
* VALIDATE TAG FIELD COLOUR                                           *         
***********************************************************************         
         SPACE 1                                                                
VALTFC   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
         NI    FDRTFHXT,X'F8'      NO EQUATE SET UP FOR THIS                    
         CLI   FVILEN,0                                                         
         BE    EXITOK              NO COLOUR SET                                
*                                                                               
         LA    R0,7                7 POSSIBLE COLOURS                           
         LA    RF,1                                                             
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         LA    R1,UC@BLUE                                                       
         LA    R4,LC@BLUE                                                       
VTCL02   EX    RE,*+8                                                           
         BE    VTCL04                                                           
         CLC   0(0,R1),FVIFLD      MATCH ON UPPERCASE                           
         EX    RE,*+8                                                           
         BE    VTCL04                                                           
         CLC   0(0,R4),FVIFLD      MATCH ON LOWERCASE                           
*                                                                               
         LA    RF,1(RF)                                                         
         LA    R1,L'UC@BLUE(R1)                                                 
         LA    R4,L'LC@BLUE(R4)                                                 
         BCT   R0,VTCL02                                                        
         B     EXITNV              NO MATCH ON ANY COLOUR                       
*                                                                               
VTCL04   STC   RF,BOBYTE1                                                       
         OC    FDRTFHXT,BOBYTE1                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR INPUT FIELD COLOUR                                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
IFCDTA   LA    RF,IFCTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
IFCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISIFC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALIFC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY INPUT FIELD COLOUR                                          *         
***********************************************************************         
         SPACE 1                                                                
DISIFC   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         IC    RF,FDRFHXA                                                       
         SLL   RF,32-3                                                          
         SRL   RF,32-3             RF CONTAINS ONLY COLOUR INFO                 
         LTR   RF,RF                                                            
         BZ    EXITOK              NOTHING SET                                  
*                                                                               
         BCTR  RF,0                MAKE ZERO BASED                              
         MH    RF,=Y(L'LC@BLUE)    INDEX INTO LIST OF COLOURS                   
         LA    RF,LC@BLUE(RF)                                                   
         MVC   FVIFLD(L'LC@BLUE),0(RF)                                          
         B     EXITOK              COLOUR IS NOW IN FVIFLD                      
         SPACE 2                                                                
***********************************************************************         
* VALIDATE INPUT FIELD COLOUR                                         *         
***********************************************************************         
         SPACE 1                                                                
VALIFC   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
         NI    FDRFHXA,X'F8'                                                    
         CLI   FVILEN,0                                                         
         BE    EXITOK              NO COLOUR SET                                
*                                                                               
         LA    R0,7                7 POSSIBLE COLOURS                           
         LA    RF,1                                                             
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         LA    R1,UC@BLUE                                                       
         LA    R4,LC@BLUE                                                       
VICL02   EX    RE,*+8                                                           
         BE    VICL04                                                           
         CLC   0(0,R1),FVIFLD      MATCH ON UPPERCASE                           
         EX    RE,*+8                                                           
         BE    VICL04                                                           
         CLC   0(0,R4),FVIFLD      MATCH ON LOWERCASE                           
*                                                                               
         LA    RF,1(RF)                                                         
         LA    R1,L'UC@BLUE(R1)                                                 
         LA    R4,L'LC@BLUE(R4)                                                 
         BCT   R0,VICL02                                                        
         B     EXITNV              NO MATCH ON ANY COLOUR                       
*                                                                               
VICL04   STC   RF,BOBYTE1                                                       
         OC    FDRFHXA,BOBYTE1                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR TAG FIELD HIGHLIGHT                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
TFHDTA   LA    RF,TFHTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
TFHTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTFH)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTFH)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY TAG FIELD HIGHLIGHT                                         *         
***********************************************************************         
         SPACE 1                                                                
DISTFH   LTR   R3,R3                FDREL?                                      
         BZ    EXITOK                                                           
         TM    FDRTFHXT,FXATHUND    X'C0' - UNDERSCORE                          
         BZ    EXITOK              NOTHING SET                                  
         BNO   *+14                                                             
         MVC   FVIFLD(L'LC@UNDR),LC@UNDR                                        
         B     EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@REV),LC@REV                                          
         TM    FDRTFHXT,FXATHREV    X'80' - REVERSE VIDEO                       
         BO    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@BLNK),LC@BLNK                                        
         B     EXITOK              MUST BE BLINK THEN                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE TAG FIELD HIGHLIGHT                                        *         
***********************************************************************         
         SPACE 1                                                                
VALTFH   LTR   R3,R3                FDREL?                                      
         BZ    DIE                                                              
         NI    FDRTFHXT,FF-(FXATHUND)                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK              NO HIGHLIGHT SET                             
*                                                                               
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         BE    VTFH02                                                           
         CLC   FVIFLD(0),LC@UNDR   UNDERSCORE ?                                 
         EX    RE,*+8                                                           
         BE    VTFH02                                                           
         CLC   FVIFLD(0),UC@UNDR   UNDERSCORE ?                                 
         B     VTFH04                                                           
*                                                                               
VTFH02   OI    FDRTFHXT,FXATHUND                                                
         B     EXITOK                                                           
*                                                                               
VTFH04   EX    RE,*+8                                                           
         BE    VTFH06                                                           
         CLC   FVIFLD(0),LC@REV    REVERSE ?                                    
         EX    RE,*+8                                                           
         BE    VTFH06                                                           
         CLC   FVIFLD(0),UC@REV    REVERSE ?                                    
         B     VTFH08                                                           
*                                                                               
VTFH06   OI    FDRTFHXT,FXATHREV                                                
         B     EXITOK                                                           
*                                                                               
VTFH08   EX    RE,*+8                                                           
         BE    VTFH10                                                           
         CLC   FVIFLD(0),LC@BLNK   BLINK ?                                      
         EX    RE,*+8                                                           
         BE    VTFH10                                                           
         CLC   FVIFLD(0),UC@BLNK   BLINK ?                                      
         B     VTFH12                                                           
*                                                                               
VTFH10   OI    FDRTFHXT,FXATHBLK                                                
         B     EXITOK                                                           
*                                                                               
VTFH12   B     EXITNV              ONLY THESE ARE VALID OPTIONS                 
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR INPUT FIELD HIGHLIGHT                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
IFHDTA   LA    RF,IFHTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
IFHTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISIFH)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALIFH)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY INPUT FIELD HIGHLIGHT                                       *         
***********************************************************************         
         SPACE 1                                                                
DISIFH   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         TM    FDRFHXA,FXATHUND    X'C0' - UNDERSCORE                           
         BZ    EXITOK              NOTHING SET                                  
         BNO   *+14                                                             
         MVC   FVIFLD(L'LC@UNDR),LC@UNDR                                        
         B     EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@REV),LC@REV                                          
         TM    FDRFHXA,FXATHREV    X'80' - REVERSE VIDEO                        
         BO    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@BLNK),LC@BLNK                                        
         B     EXITOK              MUST BE BLINK THEN                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE INPUT FIELD HIGHLIGHT                                      *         
***********************************************************************         
         SPACE 1                                                                
VALIFH   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
         NI    FDRFHXA,FF-(FXATHUND)                                            
         CLI   FVILEN,0                                                         
         BE    EXITOK              NO HIGHLIGHT SET                             
*                                                                               
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         BE    VIFH02                                                           
         CLC   FVIFLD(0),LC@UNDR   UNDERSCORE ?                                 
         EX    RE,*+8                                                           
         BE    VIFH02                                                           
         CLC   FVIFLD(0),UC@UNDR   UNDERSCORE ?                                 
         B     VIFH04                                                           
*                                                                               
VIFH02   OI    FDRFHXA,FXATHUND                                                 
         B     EXITOK                                                           
*                                                                               
VIFH04   EX    RE,*+8                                                           
         BE    VIFH06                                                           
         CLC   FVIFLD(0),LC@REV    REVERSE ?                                    
         EX    RE,*+8                                                           
         BE    VIFH06                                                           
         CLC   FVIFLD(0),UC@REV    REVERSE ?                                    
         B     VIFH08                                                           
*                                                                               
VIFH06   OI    FDRFHXA,FXATHREV                                                 
         B     EXITOK                                                           
*                                                                               
VIFH08   EX    RE,*+8                                                           
         BE    VIFH10                                                           
         CLC   FVIFLD(0),LC@BLNK   BLINK ?                                      
         EX    RE,*+8                                                           
         BE    VIFH10                                                           
         CLC   FVIFLD(0),UC@BLNK   BLINK ?                                      
         B     VIFH12                                                           
*                                                                               
VIFH10   OI    FDRFHXA,FXATHBLK                                                 
         B     EXITOK                                                           
*                                                                               
VIFH12   B     EXITNV              ONLY THESE ARE VALID OPTIONS                 
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR TAG FIELD CASE                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
TFNDTA   LA    RF,TFNTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
TFNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTFN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTFN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY TAG FIELD CASE                                              *         
***********************************************************************         
         SPACE 1                                                                
DISTFN   LTR   R3,R3                                                            
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@LOWER),LC@LOWER                                      
         TM    FDRTFHAT,FATBLC                                                  
         BO    EXITOK              LOWER CASE IS SET                            
*                                                                               
         MVC   FVIFLD(L'LC@NUMBR),LC@NUMBR                                      
         TM    FDRTFHAT,FATBNUM                                                 
         BO    EXITOK              NUMERIC FIELD SET                            
*                                                                               
         MVC   FVIFLD(L'LC@UPPER),LC@UPPER                                      
         B     EXITOK              DEFAULT IS UPPERCASE                         
         SPACE 2                                                                
***********************************************************************         
* VALIDATE TAG FIELD CASE                                             *         
***********************************************************************         
         SPACE 1                                                                
VALTFN   LTR   R3,R3                                                            
         BZ    DIE                                                              
         NI    FDRTFHAT,FF-(FATBLC+FATBNUM)                                     
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS UPPERCASE                         
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8                                                           
         BE    VTFN02                                                           
         CLC   FVIFLD(0),LC@LOWER                                               
         EX    RE,*+8                                                           
         BE    VTFN02                                                           
         CLC   FVIFLD(0),UC@LOWER                                               
         B     VTFN04                                                           
*                                                                               
VTFN02   OI    FDRTFHAT,FATBLC                                                  
         B     EXITOK              LOWER CASE IS SET                            
*                                                                               
VTFN04   EX    RE,*+8                                                           
         BE    VTFN06                                                           
         CLC   FVIFLD(0),LC@NUMBR                                               
         EX    RE,*+8                                                           
         BE    VTFN06                                                           
         CLC   FVIFLD(0),UC@NUMBR                                               
         B     VTFN08                                                           
*                                                                               
VTFN06   OI    FDRTFHAT,FATBNUM                                                 
         B     EXITOK              NUMERIC FIELD SET                            
*                                                                               
VTFN08   EX    RE,*+8                                                           
         BE    VTFN10                                                           
         CLC   FVIFLD(0),LC@UPPER                                               
         EX    RE,*+8                                                           
         BE    VTFN10                                                           
         CLC   FVIFLD(0),UC@UPPER                                               
         B     VTFN12                                                           
*                                                                               
VTFN10   B     EXITOK              UPPERCASE IS SET AS DEFAULT                  
*                                                                               
VTFN12   B     EXITNV              NOTHING ELSE IS VALID                        
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR INPUT FIELD CASE                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
IFNDTA   LA    RF,IFNTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
IFNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISIFN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALIFN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY INPUT FIELD CASE                                            *         
***********************************************************************         
         SPACE 1                                                                
DISIFN   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@LOWER),LC@LOWER                                      
         TM    FDRFHAT,FATBLC                                                   
         BO    EXITOK              LOWER CASE IS SET                            
*                                                                               
         MVC   FVIFLD(L'LC@NUMBR),LC@NUMBR                                      
         TM    FDRFHAT,FATBNUM                                                  
         BO    EXITOK              NUMERIC FIELD SET                            
*                                                                               
         MVC   FVIFLD(L'LC@UPPER),LC@UPPER                                      
         B     EXITOK              DEFAULT IS UPPERCASE                         
         SPACE 2                                                                
***********************************************************************         
* VALIDATE INPUT FIELD CASE                                           *         
***********************************************************************         
         SPACE 1                                                                
VALIFN   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
         NI    FDRFHAT,FF-(FATBLC+FATBNUM)                                      
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS UPPERCASE                         
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8                                                           
         BE    VIFN02                                                           
         CLC   FVIFLD(0),LC@LOWER                                               
         EX    RE,*+8                                                           
         BE    VIFN02                                                           
         CLC   FVIFLD(0),UC@LOWER                                               
         B     VIFN04                                                           
*                                                                               
VIFN02   OI    FDRFHAT,FATBLC                                                   
         B     EXITOK              LOWER CASE IS SET                            
*                                                                               
VIFN04   EX    RE,*+8                                                           
         BE    VIFN06                                                           
         CLC   FVIFLD(0),LC@NUMBR                                               
         EX    RE,*+8                                                           
         BE    VIFN06                                                           
         CLC   FVIFLD(0),UC@NUMBR                                               
         B     VIFN08                                                           
*                                                                               
VIFN06   OI    FDRFHAT,FATBNUM                                                  
         B     EXITOK              NUMERIC FIELD SET                            
*                                                                               
VIFN08   EX    RE,*+8                                                           
         BE    VIFN10                                                           
         CLC   FVIFLD(0),LC@UPPER                                               
         EX    RE,*+8                                                           
         BE    VIFN10                                                           
         CLC   FVIFLD(0),UC@UPPER                                               
         B     VIFN12                                                           
*                                                                               
VIFN10   B     EXITOK              UPPERCASE IS SET                             
*                                                                               
VIFN12   B     EXITNV              NOTHING ELSE IS VALID                        
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR TAG FIELD INTENSITY                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
TFIDTA   LA    RF,TFITBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
TFITBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTFI)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTFI)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY TAG FIELD INTENSITY                                         *         
***********************************************************************         
         SPACE 1                                                                
DISTFI   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@LOW),LC@LOW                                          
         TM    FDRTFHAT,FATBLOW                                                 
         BO    EXITOK              LOW INTENSITY SET                            
*                                                                               
         MVC   FVIFLD(L'LC@HIGH),LC@HIGH                                        
         TM    FDRTFHAT,FATBHIGH                                                
         BO    EXITOK              HIGH INTENSITY SET                           
*                                                                               
         MVC   FVIFLD(L'LC@NORM),LC@NORM                                        
         B     EXITOK              DEFAULT IS NORMAL                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE TAG FIELD INTENSITY                                        *         
***********************************************************************         
         SPACE 1                                                                
VALTFI   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
         NI    FDRTFHAT,FF-(FATBLOW)                                            
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NORMAL                            
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8                                                           
         BE    VTFI02                                                           
         CLC   FVIFLD(0),LC@LOW                                                 
         EX    RE,*+8                                                           
         BE    VTFI02                                                           
         CLC   FVIFLD(0),UC@LOW                                                 
         B     VTFI04                                                           
*                                                                               
VTFI02   OI    FDRTFHAT,FATBLOW                                                 
         B     EXITOK              LOW INTENSITY SET                            
*                                                                               
VTFI04   EX    RE,*+8                                                           
         BE    VTFI06                                                           
         CLC   FVIFLD(0),LC@HIGH                                                
         EX    RE,*+8                                                           
         BE    VTFI06                                                           
         CLC   FVIFLD(0),UC@HIGH                                                
         B     VTFI08                                                           
*                                                                               
VTFI06   OI    FDRTFHAT,FATBHIGH                                                
         B     EXITOK              HIGH INTENSITY SET                           
*                                                                               
VTFI08   EX    RE,*+8                                                           
         BE    VTFI10                                                           
         CLC   FVIFLD(0),LC@NORM                                                
         EX    RE,*+8                                                           
         BE    VTFI10                                                           
         CLC   FVIFLD(0),UC@NORM                                                
         B     VTFI12                                                           
*                                                                               
VTFI10   B     EXITOK              NORMAL INTENSITY IS SET                      
*                                                                               
VTFI12   B     EXITNV              NOTHING ELSE IS VALID                        
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR INPUT FIELD INTENSITY                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
IFIDTA   LA    RF,IFITBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
IFITBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISIFI)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALIFI)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY INPUT FIELD INTENSITY                                       *         
***********************************************************************         
         SPACE 1                                                                
DISIFI   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@LOW),LC@LOW                                          
         TM    FDRFHAT,FATBLOW                                                  
         BO    EXITOK              LOW INTENSITY SET                            
*                                                                               
         MVC   FVIFLD(L'LC@HIGH),LC@HIGH                                        
         TM    FDRFHAT,FATBHIGH                                                 
         BO    EXITOK              HIGH INTENSITY SET                           
*                                                                               
         MVC   FVIFLD(L'LC@NORM),LC@NORM                                        
         B     EXITOK              DEFAULT IS NORMAL                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE INPUT FIELD INTENSITY                                      *         
***********************************************************************         
         SPACE 1                                                                
VALIFI   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
         NI    FDRFHAT,FF-(FATBLOW)                                             
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NORMAL                            
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8                                                           
         BE    VIFI02                                                           
         CLC   FVIFLD(0),LC@LOW                                                 
         EX    RE,*+8                                                           
         BE    VIFI02                                                           
         CLC   FVIFLD(0),UC@LOW                                                 
         B     VIFI04                                                           
*                                                                               
VIFI02   OI    FDRFHAT,FATBLOW                                                  
         B     EXITOK              LOW INTENSITY SET                            
*                                                                               
VIFI04   EX    RE,*+8                                                           
         BE    VIFI06                                                           
         CLC   FVIFLD(0),LC@HIGH                                                
         EX    RE,*+8                                                           
         BE    VIFI06                                                           
         CLC   FVIFLD(0),UC@HIGH                                                
         B     VIFI08                                                           
*                                                                               
VIFI06   OI    FDRFHAT,FATBHIGH                                                 
         B     EXITOK              HIGH INTENSITY SET                           
*                                                                               
VIFI08   EX    RE,*+8                                                           
         BE    VIFI10                                                           
         CLC   FVIFLD(0),LC@NORM                                                
         EX    RE,*+8                                                           
         BE    VIFI10                                                           
         CLC   FVIFLD(0),UC@NORM                                                
         BE    VIFI12                                                           
*                                                                               
VIFI10   B     EXITOK              NORMAL INTENSITY IS SET                      
*                                                                               
VIFI12   B     EXITNV              NOTHING ELSE IS VALID                        
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR REDISPLAY INPUT FIELD IF VALID                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
RDVDTA   LA    RF,RDVTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
RDVTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISRDV)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRDV)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY REDISPLAY FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
DISRDV   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    FDRINDS1,FDR1RDIS                                                
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE REDISPLAY FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
VALRDV   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
         NI    FDRINDS1,FF-(FDR1RDIS)                                           
         L     R3,AFDREL                                                        
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
VRDV02   EX    RE,*+8              YES                                          
         BE    VRDV04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VRDV04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VRDV06                                                           
*                                                                               
VRDV04   OI    FDRINDS1,FDR1RDIS                                                
         B     EXITOK                                                           
*                                                                               
VRDV06   B     EXITNV              NOTHING ELSE IS VALID                        
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR REDISPLAY INPUT FIELD FILTER IF VALID               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
RDFDTA   LA    RF,RDFTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
RDFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISRDF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRDF)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY REDISPLAY FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
DISRDF   ICM   R3,15,AFLTRL        FLTRL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    FDRINDS1,FDR1RDIS                                                
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE REDISPLAY FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
VALRDF   ICM   R3,15,AFLTRL        FLTRL?                                       
         BNZ   *+16                                                             
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         B     EXITNV                                                           
*                                                                               
         NI    FDRINDS1,FF-(FDR1RDIS)                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
VRDF02   EX    RE,*+8              YES                                          
         BE    VRDF04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VRDF04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VRDF06                                                           
*                                                                               
VRDF04   OI    FDRINDS1,FDR1RDIS                                                
         B     EXITOK                                                           
*                                                                               
VRDF06   B     EXITNV              NOTHING ELSE IS VALID                        
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR SUPPRESS TAG FIELD                                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
STFDTA   LA    RF,STFTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
STFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSTF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSTF)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SUPPRESS TAG FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
DISSTF   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    FDRINDS1,FDR1XTAG                                                
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SUPPRESS TAG FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
VALSTF   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
         NI    FDRINDS1,FF-(FDR1XTAG)                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
VSTF02   EX    RE,*+8              YES                                          
         BE    VSTF04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VSTF04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VSTF06                                                           
*                                                                               
VSTF04   OI    FDRINDS1,FDR1XTAG                                                
         B     EXITOK                                                           
*                                                                               
VSTF06   B     EXITNV              NOTHING ELSE IS VALID                        
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR TAG DICTIONARY FIELD DISPLAY                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
TDDDTA   LA    RF,TDDTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
TDDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTDD)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY TAG FIELD DICTIONARY EQUATE                                 *         
***********************************************************************         
         SPACE 1                                                                
DISTDD   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         OC    FDRRTAG,FDRRTAG                                                  
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'FDRRTAG),FDRRTAG                                        
         ICM   RF,15,=C'SL  '                                                   
         ICM   RF,2,FDRKSYS                                                     
         GOTO1 VDICTAT,BOPARM,(RF),FVIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR INPUT FIELD IS PROTECTED                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
PIFDTA   LA    RF,PIFTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
PIFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPIF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPIF)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY INPUT FIELD IS PROTECTED FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
DISPIF   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    FDRINDS1,FDR1PRO                                                 
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE INPUT FIELD IS PROTECTED                                   *         
***********************************************************************         
         SPACE 1                                                                
VALPIF   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
         NI    FDRINDS1,FF-(FDR1PRO)                                            
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
VPIF02   EX    RE,*+8              YES                                          
         BE    VPIF04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VPIF04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VPIF06                                                           
*                                                                               
VPIF04   OI    FDRINDS1,FDR1PRO                                                 
         B     EXITOK                                                           
*                                                                               
VPIF06   B     EXITNV              NOTHING ELSE IS VALID                        
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FIELD DATA LEVEL                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
FIKDTA   LA    RF,FIKTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
FIKTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFIK)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFIK)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FIELD DATA LEVEL                                            *         
***********************************************************************         
         SPACE 1                                                                
DISFIK   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         TM    FDRLVL,FDRIKEY+FDRIFILE+FDRISTT+FDRITSAR+FDRIUSR                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@KEY),LC@KEY                                          
         TM    FDRLVL,FDRIKEY                                                   
         BO    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@REC),LC@REC                                          
         TM    FDRLVL,FDRIFILE                                                  
         BO    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@STT),LC@STT                                          
         TM    FDRLVL,FDRISTT                                                   
         BO    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@TSAR),LC@TSAR                                        
         TM    FDRLVL,FDRITSAR                                                  
         BO    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@USER),LC@USER                                        
         TM    FDRLVL,FDRIUSR                                                   
         BO    EXITOK                                                           
         DC    H'0'                                                             
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FIELD DATA LEVEL                                           *         
***********************************************************************         
         SPACE 1                                                                
TEMP     USING FDRKSTAT,GSRECSTA                                                
VALFIK   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
         NI    FDRLVL,FF-(FDRIKEY+FDRIFILE+FDRISTT+FDRITSAR+FDRIUSR)            
         NI    TEMP.FDRKSTAT,FF-(FDRKKEY+FDRKFILE+FDRKSTT)                      
         NI    TEMP.FDRKSTAT,FF-(FDRKTSAR+FDRKUSR)                              
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              KEY                                          
         BE    VFIK02                                                           
         CLC   FVIFLD(0),LC@KEY                                                 
         EX    RE,*+8                                                           
         BE    VFIK02                                                           
         CLC   FVIFLD(0),UC@KEY                                                 
         B     VFIK04                                                           
*                                                                               
VFIK02   OI    FDRLVL,FDRIKEY                                                   
         OI    TEMP.FDRKSTAT,FDRKKEY                                            
         B     VFIKOK                                                           
*                                                                               
VFIK04   EX    RE,*+8              RECORD                                       
         BE    VFIK06                                                           
         CLC   FVIFLD(0),LC@REC                                                 
         EX    RE,*+8                                                           
         BE    VFIK06                                                           
         CLC   FVIFLD(0),UC@REC                                                 
         B     VFIK08                                                           
*                                                                               
VFIK06   OI    FDRLVL,FDRIFILE                                                  
         OI    TEMP.FDRKSTAT,FDRKFILE                                           
         B     VFIKOK                                                           
*                                                                               
VFIK08   EX    RE,*+8              STATUS ONLY                                  
         BE    VFIK10                                                           
         CLC   FVIFLD(0),LC@STT                                                 
         EX    RE,*+8                                                           
         BE    VFIK10                                                           
         CLC   FVIFLD(0),UC@STT                                                 
         B     VFIK12                                                           
*                                                                               
VFIK10   OI    FDRLVL,FDRISTT                                                   
         OI    TEMP.FDRKSTAT,FDRKSTT                                            
         B     VFIKOK                                                           
*                                                                               
VFIK12   EX    RE,*+8              TSAR RECORD                                  
         BE    VFIK14                                                           
         CLC   FVIFLD(0),LC@TSAR                                                
         EX    RE,*+8                                                           
         BE    VFIK14                                                           
         CLC   FVIFLD(0),UC@TSAR                                                
         B     VFIK16                                                           
*                                                                               
VFIK14   OI    FDRLVL,FDRITSAR                                                  
         OI    TEMP.FDRKSTAT,FDRKTSAR                                           
         B     VFIKOK                                                           
*                                                                               
VFIK16   EX    RE,*+8              TSAR RECORD                                  
         BE    VFIK18                                                           
         CLC   FVIFLD(0),LC@USER                                                
         EX    RE,*+8                                                           
         BE    VFIK18                                                           
         CLC   FVIFLD(0),UC@USER                                                
         B     VFIK20                                                           
*                                                                               
VFIK18   OI    FDRLVL,FDRIUSR                                                   
         OI    TEMP.FDRKSTAT,FDRKUSR                                            
         B     VFIKOK                                                           
*                                                                               
VFIK20   B     EXITNV              NOTHING ELSE IS VALID                        
*                                                                               
VFIKOK   ICM   R4,15,AFLTRL                                                     
         BZ    EXITOK                                                           
Z        USING FDRELD,R4                                                        
         MVC   Z.FDRLVL,FDRLVL                                                  
         DROP  TEMP,Z                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FIELD IS A DDS ONLY FIELD                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
FDODTA   LA    RF,FDOTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
FDOTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFDO)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFDO)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FIELD IS DDS ONLY                                           *         
***********************************************************************         
         SPACE 1                                                                
DISFDO   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    FDRINDS1,FDR1DDS                                                 
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FIELD IS DDS ONLY                                          *         
***********************************************************************         
         SPACE 1                                                                
VALFDO   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
         NI    FDRINDS1,FF-(FDR1DDS)                                            
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
VFDO02   EX    RE,*+8              YES                                          
         BE    VFDO04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VFDO04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VFDO06                                                           
*                                                                               
VFDO04   OI    FDRINDS1,FDR1DDS                                                 
         B     EXITOK                                                           
*                                                                               
VFDO06   B     EXITNV              NOTHING ELSE IS VALID                        
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR COLUMN WIDTH                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
CWDDTA   LA    RF,CWDTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
CWDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCWD)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCWD)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY COLUMN WIDTH                                                *         
***********************************************************************         
         SPACE 1                                                                
DISCWD   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         CURED FDRLHLEN,(2,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                     
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE COLUMN WIDTH                                               *         
***********************************************************************         
         SPACE 1                                                                
VALCWD   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
         MVI   FDRLHLEN,0                                                       
         TM    FVIIND,FVINUM       FIELD NUMBER MUST BE NUMERIC                 
         BZ    EXITNOTN                                                         
         ICM   RF,15,BCFULL        NUMERIC VALUE IS IN BCFULL                   
*        LTR   RF,RF                                                            
*        BZ    EXITNV              NUMBER CANNOT BE ZERO                        
         SPACE 1                                                                
         CH    RF,=H'79'           TOO WIDE?                                    
         BNL   EXITNV                                                           
         STC   RF,FDRLHLEN         COLUMN WIDTH HERE                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DATA WIDTH IN COLUMN                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
DWDDTA   LA    RF,DWDTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DWDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDWD)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDWD)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DATA WIDTH IN COLUMN                                        *         
***********************************************************************         
         SPACE 1                                                                
DISDWD   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         CURED FDRLCLEN,(2,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                     
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE DATA WIDTH IN COLUMN                                       *         
***********************************************************************         
         SPACE 1                                                                
VALDWD   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
         MVI   FDRLCLEN,0                                                       
         TM    FVIIND,FVINUM       FIELD NUMBER MUST BE NUMERIC                 
         BZ    EXITNOTN                                                         
         ICM   RF,15,BCFULL        NUMERIC VALUE IS IN BCFULL                   
*        LTR   RF,RF                                                            
*        BZ    EXITNV              NUMBER CANNOT BE ZERO                        
         SPACE 1                                                                
         CH    RF,=H'79'           TOO WIDE?                                    
         BNL   EXITNV                                                           
         STC   RF,FDRLCLEN         COLUMN WIDTH HERE                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLACEMENT TO START OF DATA IN COLUMN             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
DFCDTA   LA    RF,DFCTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DFCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDFC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDFC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DISPLACEMENT TO FIRST DATA IN COLUMN                        *         
***********************************************************************         
         SPACE 1                                                                
DISDFC   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         CLI   FDRLCDSP,0          ZERO DISPLACEMENT?                           
         BE    EXITOK                                                           
*                                                                               
         CURED FDRLCDSP,(2,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                     
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE DISPLACEMENT TO FIRST DATA IN COLUMN                       *         
***********************************************************************         
         SPACE 1                                                                
VALDFC   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
         MVI   FDRLCDSP,0                                                       
         CLI   FVILEN,0            NUMBER IS OPTIONAL                           
         BE    EXITOK                                                           
*                                                                               
         TM    FVIIND,FVINUM       FIELD NUMBER MUST BE NUMERIC                 
         BZ    EXITNOTN                                                         
         ICM   RF,15,BCFULL        NUMERIC VALUE IS IN BCFULL                   
         LTR   RF,RF                                                            
         BZ    EXITOK              ZERO?                                        
*                                                                               
         CH    RF,=H'75'           TOO WIDE?                                    
         BNL   EXITNV                                                           
*                                                                               
         XR    R0,R0                                                            
         IC    R0,FDRLHLEN         R0=WIDTH OF COLUMN                           
         XR    RE,RE                                                            
         IC    RE,FDRLCLEN         RE=WIDTH OF DATA                             
         AR    RE,RF               ADD DISPLACEMENT TO DATA START -             
         CR    R0,RE               CANNOT BE WIDER THAN WIDTH OF COLUMN         
         BL    EXITNV                                                           
*                                                                               
         STC   RF,FDRLCDSP         DISP TO FIRST DATA HERE                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR COLUMN HEADING 1 DICTIONARY EQUATE                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
CH1DTA   LA    RF,CH1TBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
CH1TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCH1)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCH1)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY COLUMN 1 DICTIONARY EQUATE FIELD                            *         
***********************************************************************         
         SPACE 1                                                                
DISCH1   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         OC    FDRLHED1,FDRLHED1                                                
         BZ    EXITOK                                                           
         MVC   BCFULL,FDRLHED1     COL 1 DICTIONARY EQUATE                      
         XR    RF,RF                                                            
         IC    RF,FDRKSYS                                                       
         GOTOX ('DISDIC',AGROUTS),BOPARM,(RF)                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE COLUMN 1 DICTIONARY EQUATE FIELD                           *         
***********************************************************************         
         SPACE 1                                                                
VALCH1   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
         XC    FDRLHED1,FDRLHED1                                                
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FDRKSYS                                                       
         GOTOX ('VALDIC',AGROUTS),BOPARM,(RF) VALIDATE COLUMN 1 DIC             
         BNE   EXITL               VALDIC SETS OWN ERROR MESSAGES               
         SPACE 1                                                                
         MVC   FDRLHED1,BCFULL     DATA DICTIONARY REF BACK IN BCFULL           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR COLUMN HEADING 1 DICTIONARY DISPLAY                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
CD1DTA   LA    RF,CD1TBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
CD1TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCD1)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY COLUMN HEADING 1 DICTIONARY EQUATE                          *         
***********************************************************************         
         SPACE 1                                                                
DISCD1   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         OC    FDRLHED1,FDRLHED1                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'FDRLHED1),FDRLHED1                                      
         ICM   RF,15,=C'SL  '                                                   
         ICM   RF,2,FDRKSYS                                                     
         GOTO1 VDICTAT,BOPARM,(RF),FVIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR COLUMN HEADING 2 DICTIONARY EQUATE                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
CH2DTA   LA    RF,CH2TBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
CH2TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCH2)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCH2)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY COLUMN 2 DICTIONARY EQUATE FIELD                            *         
***********************************************************************         
         SPACE 1                                                                
DISCH2   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         OC    FDRLHED2,FDRLHED2                                                
         BZ    EXITOK                                                           
         MVC   BCFULL,FDRLHED2     DISPLAY COLUMN 2 DICTIONARY EQUATE           
         XR    RF,RF                                                            
         IC    RF,FDRKSYS                                                       
         GOTOX ('DISDIC',AGROUTS),BOPARM,(RF)                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE COLUMN 2 DICTIONARY EQUATE FIELD                           *         
***********************************************************************         
         SPACE 1                                                                
VALCH2   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
         XC    FDRLHED2,FDRLHED2                                                
         TM    FDRINDS1,FDR1XTAG   SUPPRESS TAG FIELD?                          
         BZ    VCH202                                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         B     VCH204                                                           
*                                                                               
VCH202   CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
VCH204   XR    RF,RF                                                            
         IC    RF,FDRKSYS                                                       
         GOTOX ('VALDIC',AGROUTS),BOPARM,(RF) VALIDATE COLUMN 1 DIC             
         BNE   EXITL                                                            
*                                                                               
         MVC   FDRLHED2,BCFULL     DATA DICTIONARY REF BACK IN BCFULL           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR COLUMN HEADING 2 DICTIONARY DISPLAY                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
CD2DTA   LA    RF,CD2TBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
CD2TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCD2)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY COLUMN 2 DICTIONARY EQUATE VALUE                            *         
***********************************************************************         
         SPACE 1                                                                
DISCD2   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         OC    FDRLHED2,FDRLHED2                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'FDRLHED2),FDRLHED2                                      
         ICM   RF,15,=C'SL  '                                                   
         ICM   RF,2,FDRKSYS                                                     
         GOTO1 VDICTAT,BOPARM,(RF),FVIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR HELP NUMBER                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
HLPDTA   LA    RF,HLPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
HLPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISHLP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALHLP)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY HELP NUMBER                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISHLP   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         XR    R0,R0                                                            
         ICM   R0,1,FDRHLP                                                      
         CURED (R0),(4,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                         
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE HELP NUMBER                                                *         
***********************************************************************         
         SPACE 1                                                                
VALHLP   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
         MVI   FDRHLP,0            RESET HELP NUMBER                            
         CLI   FVILEN,0                                                         
         BE    EXITOK              NUMBER IS OPTIONAL                           
*                                                                               
         TM    FVIIND,FVINUM       FIELD NUMBER MUST BE NUMERIC                 
         BZ    EXITNOTN                                                         
         ICM   RF,15,BCFULL        NUMERIC VALUE IS IN BCFULL                   
         LTR   RF,RF                                                            
         BZ    EXITOK              IN CASE ZERO INPUT                           
         SPACE 1                                                                
         CH    RF,=H'255'          TOO BIG?                                     
         BH    EXITNV                                                           
         STCM  RF,1,FDRHLP         HELP NUMBER HERE                             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR SECURITY REFERENCE NUMBER                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
SCRDTA   LA    RF,SCRTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SCRTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSCR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSCR)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SECURITY NUMBER                                             *         
***********************************************************************         
         SPACE 1                                                                
DISSCR   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         CURED FDRSEC,(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                       
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE SECURITY NUMBER                                            *         
***********************************************************************         
         SPACE 1                                                                
VALSCR   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
         MVI   FDRSEC,0            RESET SECURITY REFERENCE                     
         CLI   FVILEN,0                                                         
         BE    EXITOK              NUMBER IS OPTIONAL                           
*                                                                               
         TM    FVIIND,FVINUM       FIELD NUMBER MUST BE NUMERIC                 
         BZ    EXITNOTN                                                         
         ICM   RF,15,BCFULL        NUMERIC VALUE IS IN BCFULL                   
         LTR   RF,RF                                                            
         BZ    EXITOK              IN CASE ZERO INPUT                           
         SPACE 1                                                                
         CH    RF,=H'255'          TOO BIG?                                     
         BH    EXITNV                                                           
         STCM  RF,1,FDRSEC         SECURITY REFERENCE                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FIELD IS REQUIRED                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
FRQDTA   LA    RF,FRQTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
FRQTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFRQ)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFRQ)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FIELD IS REQUIRED                                           *         
***********************************************************************         
         SPACE 1                                                                
DISFRQ   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    FDRINDS1,FDR1REQ                                                 
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FIELD IS REQUIRED                                          *         
***********************************************************************         
         SPACE 1                                                                
VALFRQ   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
         NI    FDRINDS1,FF-(FDR1REQ)                                            
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
VFRQ02   EX    RE,*+8              YES                                          
         BE    VFRQ04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VFRQ04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VFRQ06                                                           
*                                                                               
VFRQ04   OI    FDRINDS1,FDR1REQ                                                 
         B     EXITOK                                                           
*                                                                               
VFRQ06   B     EXITNV              NOTHING ELSE IS VALID                        
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FIELD HAS DEFAULT VALUE                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
FDFDTA   LA    RF,FDFTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
FDFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFDF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFDF)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FIELD HAS DEFAULT VALUE                                     *         
***********************************************************************         
         SPACE 1                                                                
DISFDF   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    FDRINDS1,FDR1IDEF                                                
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FIELD HAS DEFAULT VALUE                                    *         
***********************************************************************         
         SPACE 1                                                                
VALFDF   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
         NI    FDRINDS1,FF-(FDR1IDEF)                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
VFDF02   EX    RE,*+8              YES                                          
         BE    VFDF04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VFDF04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VFDF06                                                           
*                                                                               
VFDF04   OI    FDRINDS1,FDR1IDEF                                                
         B     EXITOK                                                           
*                                                                               
VFDF06   B     EXITNV              NOTHING ELSE IS VALID                        
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER HAS DEFAULT VALUE                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
FFDDTA   LA    RF,FFDTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
FFDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFFD)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFFD)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FIELD HAS DEFAULT VALUE                                     *         
***********************************************************************         
         SPACE 1                                                                
DISFFD   ICM   R3,15,AFLTRL        FLTRL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    FDRINDS1,FDR1IDEF                                                
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FIELD HAS DEFAULT VALUE                                    *         
***********************************************************************         
         SPACE 1                                                                
VALFFD   ICM   R3,15,AFLTRL        FDREL?                                       
         BNZ   *+16                                                             
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         B     EXITNV                                                           
*                                                                               
         NI    FDRINDS1,FF-(FDR1IDEF)                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
VFFD02   EX    RE,*+8              YES                                          
         BE    VFFD04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VFFD04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VFFD06                                                           
*                                                                               
VFFD04   OI    FDRINDS1,FDR1IDEF                                                
         B     EXITOK                                                           
*                                                                               
VFFD06   B     EXITNV              NOTHING ELSE IS VALID                        
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER HAS NO OVERRIDES                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
NOVDTA   LA    RF,NOVTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
NOVTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNOV)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNOV)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FIELD HAS DEFAULT VALUE                                     *         
***********************************************************************         
         SPACE 1                                                                
DISNOV   ICM   R3,15,AFLTRL        FLTRL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    FDRFVAL2,FDRF2NOV                                                
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FIELD HAS DEFAULT VALUE                                    *         
***********************************************************************         
         SPACE 1                                                                
VALNOV   ICM   R3,15,AFLTRL        FDREL?                                       
         BNZ   *+16                                                             
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         B     EXITNV                                                           
*                                                                               
         NI    FDRFVAL2,FF-(FDRF2NOV)                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
VNOV02   EX    RE,*+8              YES                                          
         BE    VNOV04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VNOV04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VNOV06                                                           
*                                                                               
VNOV04   OI    FDRFVAL2,FDRF2NOV                                                
         B     EXITOK                                                           
*                                                                               
VNOV06   B     EXITNV              NOTHING ELSE IS VALID                        
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FIELD IS FILTERABLE                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
FFVDTA   LA    RF,FFVTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
FFVTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFFV)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFFV)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FIELD IS FILTERABLE                                         *         
***********************************************************************         
         SPACE 1                                                                
DISFFV   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    FDRINDS2,FDR2FILT                                                
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FIELD IS FILTERABLE                                        *         
***********************************************************************         
         SPACE 1                                                                
VALFFV   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
Z        USING FDRKSTAT,GSRECSTA                                                
         NI    Z.FDRKSTAT,FF-(FDRKFLT)  DIRECTORY FILTER INDICATOR              
         NI    FDRINDS2,FF-(FDR2FILT)   FILE INDICATOR                          
         DROP  Z                                                                
*                                                                               
         CLI   FVILEN,0                                                         
         BE    VFFDEL              NO INPUT - DEFAULT IS NO                     
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    VFFDEL                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    VFFDEL                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VFFE02                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VFFE02                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     EXITNV                                                           
*                                                                               
Z        USING FDRKSTAT,GSRECSTA                                                
VFFE02   OI    Z.FDRKSTAT,FDRKFLT  DIRECTORY FILTER INDICATOR                   
         OI    FDRINDS2,FDR2FILT   FILE INDICATOR                               
         DROP  Z                                                                
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FLTRLQ',FDRRECD),0               
         CLI   12(R1),0                                                         
         BE    EXITOK              ALREADY HAVE A FLTRL ON RECORD               
*                                                                               
TEMP     USING FDRELD,BOELEM                                                    
         XC    TEMP.FDREL(FLTRLNQ),TEMP.FDREL                                   
         MVI   TEMP.FDREL,FLTRLQ   BUILD FILTER ELEMENT                         
         MVI   TEMP.FDRLN,FLTRLNQ                                               
         MVC   TEMP.FDRNUM,FDRKNUM                                              
         MVC   TEMP.FDRHLP,FDRHLP                                               
         MVC   TEMP.FDRSEC,FDRSEC                                               
         MVC   TEMP.FDRCTRY,FDRCTRY                                             
         MVC   TEMP.FDRLVL,FDRLVL                                               
         MVC   TEMP.FDRINDS1,FDRINDS1                                           
         MVC   TEMP.FDRINDS2,FDRINDS2                                           
         MVC   TEMP.FDRTFHAT,FDRTFHAT                                           
         MVC   TEMP.FDRFHAT,FDRFHAT                                             
         MVC   TEMP.FDRTFHXT,FDRTFHXT                                           
         MVC   TEMP.FDRFHXA,FDRFHXA                                             
         MVC   TEMP.FDRTDEFL,FDRTDEFL                                           
         MVC   TEMP.FDRFDEFL,FDRFDEFL                                           
         MVC   TEMP.FDRRTAG,FDRRTAG                                             
         MVC   TEMP.FDRFMIN,FDRFMIN                                             
         MVC   TEMP.FDRFMAX,FDRFMAX                                             
         MVI   TEMP.FDRFSAVL,1                                                  
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),FDRRECD,TEMP.FDRELD                
         CLI   12(R1),0                                                         
         BE    *+6                 ERROR IN HELLO                               
         DC    H'0'                                                             
         L     RF,16(R1)           A(ELEMENT) HERE AFTER PUT                    
         ST    RF,AFLTRL           SAVE FOR THE REST OF THE ROUTINES            
*                                                                               
         CLI   CSACT,A#LST         NO REDISPLAY IF IN A LIST                    
         BE    EXITOK                                                           
         MVC   AFVADDR,FVADDR                                                   
         GOTO1 AGEN,BOPARM,ORECH,RDIS,FDRRECD,0                                 
*                                                                               
         MVC   FVADDR,AFVADDR                                                   
         MVC   FVIFLD,BCSPACES                                                  
         GOTO1 AGEN,BOPARM,ODATA,DDIS,FVADDR,0                                  
         BE    EXITOK              REDISPLAY RECORD SCREEN                      
         DC    H'0'                                                             
         DROP  TEMP                                                             
*                                                                               
VFFDEL   GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('FLTRLQ',FDRRECD),0               
         XC    AFLTRL,AFLTRL                                                    
         CLI   CSACT,A#LST         NO REDISPLAY IF IN A LIST                    
         BE    EXITOK                                                           
*                                                                               
         MVC   AFVADDR,FVADDR                                                   
         GOTO1 AGEN,BOPARM,ORECH,RDIS,FDRRECD,0                                 
*                                                                               
         MVC   FVADDR,AFVADDR                                                   
         MVC   FVIFLD,BCSPACES                                                  
         GOTO1 AGEN,BOPARM,ODATA,DDIS,FVADDR,0                                  
         BE    EXITOK              REDISPLAY RECORD SCREEN                      
         DC    H'0'                                                             
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER DICTIONARY REFERENCE                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
FDRDTA   LA    RF,FDRTBL           TABLE OF KNOWN VERBS                         
         L     R3,AFLTRL           FILTER ELEMENT                               
         B     ITER                ITERATE TABLE                                
*                                                                               
FDRTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFDR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFDR)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A FILTER DICTIONARY REFERENCE FIELD                         *         
***********************************************************************         
         SPACE 1                                                                
DISFDR   LTR   R3,R3               FLTRL?                                       
         BZ    EXITOK                                                           
*                                                                               
         OC    FDRRTAG,FDRRTAG     FILTER?                                      
         BZ    EXITOK                                                           
         MVC   BCFULL,FDRRTAG      FILTER DICTIONARY REFERENCE                  
         XR    RF,RF                                                            
         IC    RF,FDRKSYS                                                       
         GOTOX ('DISDIC',AGROUTS),BOPARM,(RF)                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A FILTER DICTIONARY REFERENCE FIELD                        *         
***********************************************************************         
         SPACE 1                                                                
VALFDR   LTR   R3,R3               FLTRL?                                       
         BNZ   VFDR02              YES                                          
         CLI   FVILEN,0                                                         
         BNZ   EXITNV              NO INPUT VALID HERE                          
         B     EXITOK                                                           
*                                                                               
VFDR02   XC    FDRRTAG,FDRRTAG                                                  
         CLI   FVILEN,0                                                         
         BNE   VFDR04                                                           
         TM    FDRINDS1,FDR1XTAG   SUPPRESS FILTER?                             
         BZ    EXITNO              NO - FIELD NEEDS A DICTIONARY REF            
         B     EXITOK                                                           
*                                                                               
VFDR04   XR    RF,RF                                                            
         IC    RF,FDRKSYS                                                       
         GOTOX ('VALDIC',AGROUTS),BOPARM,(RF) VALIDATE FILTER DIC               
         BNE   EXITL               VALDIC SETS OWN ERROR MESSAGES               
         SPACE 1                                                                
         MVC   FDRRTAG,BCFULL      DATA DICTIONARY REF BACK IN BCFULL           
         MVC   FDRTDEFL,BCFULL+3   SET LENGTH OF FILTER                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER FIELD LENGTH                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
FFLDTA   LA    RF,FFLTBL           TABLE OF KNOWN VERBS                         
         L     R3,AFLTRL           FILTER ELEMENT                               
         B     ITER                ITERATE TABLE                                
*                                                                               
FFLTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFFL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFFL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LENGTH OF THE FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DISFFL   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         CURED FDRFDEFL,(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE LENGTH OF THE FILTER FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
VALFFL   LTR   R3,R3               FLTRL?                                       
         BNZ   VFFL02                                                           
         CLI   FVILEN,0                                                         
         BNE   EXITNV              NO INPUT IS VALID                            
         B     EXITOK                                                           
*                                                                               
VFFL02   TM    FVIIND,FVINUM       FIELD LENGTH MUST BE A NUMBER                
         BZ    EXITNOTN                                                         
         ICM   RF,15,BCFULL                                                     
         BZ    EXITNV                                                           
         CH    RF,=H'255'          INPUT LENGTH CONSTRAINED TO BE <255          
         BH    EXITNV                                                           
         STC   RF,FDRFDEFL                                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR MINIMUM INPUT LENGTH OF FILTER FIELD                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
MNFDTA   LA    RF,MNFTBL           TABLE OF KNOWN VERBS                         
         L     R3,AFLTRL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
MNFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISMNF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALMNF)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY MINIMUM IMPUT LENGTH OF FILTER FIELD                        *         
***********************************************************************         
         SPACE 1                                                                
DISMNF   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         CLI   FDRFMIN,0           MINIMUM LENGTH - OPTIONAL                    
         BE    EXITOK                                                           
         CURED FDRFMIN,(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE MINIMUM INPUT LENGTH OF FILTER FIELD                       *         
***********************************************************************         
         SPACE 1                                                                
VALMNF   LTR   R3,R3               FLTRL?                                       
         BNZ   VMNF02                                                           
         CLI   FVILEN,0                                                         
         BNE   EXITNV              NO INPUT VALID                               
         B     EXITOK                                                           
*                                                                               
VMNF02   MVI   FDRFMIN,0           MINIMUM INPUT LENGTH IS OPTIONAL             
         CLI   FVILEN,0            DEFAULT IS 0                                 
         BE    EXITOK                                                           
*                                                                               
         TM    FVIIND,FVINUM       ANY INPUT MUST BE NUMERIC                    
         BZ    EXITNOTN                                                         
         ICM   RF,15,BCFULL        IF THEY TYPE IN A 0                          
         BZ    EXITOK                                                           
         CLM   RF,1,FDRFDEFL       INPUT LENGTH CAN`T BE < FIELD LENGTH         
         BH    EXITNV                                                           
         STC   RF,FDRFMIN                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR MAXIMUM INPUT LENGTH OF FILTER FIELD                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
MXFDTA   LA    RF,MXFTBL           TABLE OF KNOWN VERBS                         
         L     R3,AFLTRL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
MXFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISMXF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALMXF)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY MAXIMUM INPUT LENGTH OF FILTER FIELD                        *         
***********************************************************************         
         SPACE 1                                                                
DISMXF   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         CLI   FDRFMAX,0           MAXIMUM LENGTH - OPTIONAL                    
         BE    EXITOK                                                           
         CURED FDRFMAX,(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE MAXIMUM INPUT LENGTH OF FILTER FIELD                       *         
***********************************************************************         
         SPACE 1                                                                
VALMXF   LTR   R3,R3               FDREL?                                       
         BNZ   VMXF02                                                           
         CLI   FVILEN,0                                                         
         BNE   EXITNV                                                           
         B     EXITOK                                                           
*                                                                               
VMXF02   MVI   FDRFMAX,0           MAXIMUM INPUT LENGTH IS OPTIONAL             
         CLI   FVILEN,0            DEFAULT IS 0                                 
         BE    EXITOK                                                           
         SPACE 1                                                                
         TM    FVIIND,FVINUM       ANY INPUT MUST BE NUMERIC                    
         BZ    EXITNOTN                                                         
         ICM   RF,15,BCFULL        IF THEY TYPE IN A 0                          
         BZ    EXITOK                                                           
         CLM   RF,1,FDRFDEFL       INPUT LENGTH CAN`T BE < FIELD LENGTH         
         BH    EXITNV                                                           
         STC   RF,FDRFMAX                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER TAG FIELD COLOUR                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
FTCDTA   LA    RF,FTCTBL           TABLE OF KNOWN VERBS                         
         L     R3,AFLTRL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
FTCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFTC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFTC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER TAG FIELD COLOUR                                     *         
***********************************************************************         
         SPACE 1                                                                
DISFTC   LTR   R3,R3               FLTRL?                                       
         BZ    EXITOK                                                           
         IC    RF,FDRTFHXT                                                      
         SLL   RF,32-3                                                          
         SRL   RF,32-3             RF CONTAINS ONLY COLOUR INFO                 
         LTR   RF,RF                                                            
         BZ    EXITOK              NOTHING SET                                  
*                                                                               
         BCTR  RF,0                MAKE ZERO BASED                              
         MH    RF,=Y(L'LC@BLUE)    INDEX INTO LIST OF COLOURS                   
         LA    RF,LC@BLUE(RF)                                                   
         MVC   FVIFLD(L'LC@BLUE),0(RF)                                          
         B     EXITOK              COLOUR IS NOW IN FVIFLD                      
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER TAG FIELD COLOUR                                    *         
***********************************************************************         
         SPACE 1                                                                
VALFTC   LTR   R3,R3               FLTRL?                                       
         BNZ   VFTC01                                                           
         CLI   FVILEN,0                                                         
         BNE   EXITNV              NO INPUT IS VALID                            
         B     EXITOK                                                           
*                                                                               
VFTC01   NI    FDRTFHXT,X'F8'                                                   
         CLI   FVILEN,0                                                         
         BE    EXITOK              NO COLOUR SET                                
*                                                                               
         LA    R0,7                7 POSSIBLE COLOURS                           
         LA    RF,1                                                             
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         LA    R1,UC@BLUE                                                       
         LA    R4,LC@BLUE                                                       
VFTC02   EX    RE,*+8                                                           
         BE    VFTC04                                                           
         CLC   0(0,R1),FVIFLD                                                   
         EX    RE,*+8                                                           
         BE    VFTC04                                                           
         CLC   0(0,R4),FVIFLD                                                   
         LA    RF,1(RF)                                                         
         LA    R1,L'UC@BLUE(R1)                                                 
         LA    R4,L'LC@BLUE(R4)                                                 
         BCT   R0,VFTC02                                                        
         B     EXITNV              NO MATCH ON ANY COLOUR                       
*                                                                               
VFTC04   STC   RF,BOBYTE1                                                       
         OC    FDRTFHXT,BOBYTE1                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER INPUT FIELD COLOUR                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
FICDTA   LA    RF,FICTBL           TABLE OF KNOWN VERBS                         
         L     R3,AFLTRL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
FICTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFIC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFIC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER INPUT FIELD COLOUR                                   *         
***********************************************************************         
         SPACE 1                                                                
DISFIC   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         IC    RF,FDRFHXA                                                       
         SLL   RF,32-3                                                          
         SRL   RF,32-3             RF CONTAINS ONLY COLOUR INFO                 
         LTR   RF,RF                                                            
         BZ    EXITOK              NOTHING SET                                  
*                                                                               
         BCTR  RF,0                MAKE ZERO BASED                              
         MH    RF,=Y(L'LC@BLUE)    INDEX INTO LIST OF COLOURS                   
         LA    RF,LC@BLUE(RF)                                                   
         MVC   FVIFLD(L'LC@BLUE),0(RF)                                          
         B     EXITOK              COLOUR IS NOW IN FVIFLD                      
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER INPUT FIELD COLOUR                                  *         
***********************************************************************         
         SPACE 1                                                                
VALFIC   LTR   R3,R3               FLTRL?                                       
         BNZ   VFIC01                                                           
         CLI   FVILEN,0                                                         
         BNE   EXITNV                                                           
         B     EXITOK                                                           
*                                                                               
VFIC01   NI    FDRFHXA,X'F8'                                                    
         CLI   FVILEN,0                                                         
         BE    EXITOK              NO COLOUR SET                                
*                                                                               
         LA    R0,7                7 POSSIBLE COLOURS                           
         LA    RF,1                                                             
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         LA    R1,UC@BLUE                                                       
         LA    R4,LC@BLUE                                                       
VFIC02   EX    RE,*+8                                                           
         BE    VFIC04                                                           
         CLC   0(0,R1),FVIFLD                                                   
         EX    RE,*+8                                                           
         BE    VFIC04                                                           
         CLC   0(0,R4),FVIFLD                                                   
         LA    RF,1(RF)                                                         
         LA    R1,L'UC@BLUE(R1)                                                 
         LA    R4,L'LC@BLUE(R4)                                                 
         BCT   R0,VFIC02                                                        
         B     EXITNV              NO MATCH ON ANY COLOUR                       
*                                                                               
VFIC04   STC   RF,BOBYTE1                                                       
         OC    FDRFHXA,BOBYTE1                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER TAG FIELD HIGHLIGHT                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
FFHDTA   LA    RF,FFHTBL           TABLE OF KNOWN VERBS                         
         L     R3,AFLTRL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
FFHTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFFH)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFFH)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER TAG FIELD HIGHLIGHT                                  *         
***********************************************************************         
         SPACE 1                                                                
DISFFH   LTR   R3,R3                FDREL?                                      
         BZ    EXITOK                                                           
         TM    FDRTFHXT,FXATHUND    X'C0' - UNDERSCORE                          
         BZ    EXITOK              NOTHING SET                                  
         BNO   *+14                                                             
         MVC   FVIFLD(L'LC@UNDR),LC@UNDR                                        
         B     EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@REV),LC@REV                                          
         TM    FDRTFHXT,FXATHREV    X'80' - REVERSE VIDEO                       
         BO    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@BLNK),LC@BLNK                                        
         B     EXITOK              MUST BE BLINK THEN                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER TAG FIELD HIGHLIGHT                                 *         
***********************************************************************         
         SPACE 1                                                                
VALFFH   LTR   R3,R3               FDREL?                                       
         BNZ   VFFH01                                                           
         CLI   FVILEN,0                                                         
         BNE   EXITNV              NO INPUT IS VALID                            
         B     EXITOK                                                           
*                                                                               
VFFH01   NI    FDRTFHXT,FF-(FXATHUND)                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK              NO HIGHLIGHT SET                             
*                                                                               
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         BE    VFFH02                                                           
         CLC   FVIFLD(0),LC@UNDR   UNDERSCORE ?                                 
         EX    RE,*+8                                                           
         BE    VFFH02                                                           
         CLC   FVIFLD(0),UC@UNDR   UNDERSCORE ?                                 
         B     VFFH04                                                           
*                                                                               
VFFH02   OI    FDRTFHXT,FXATHUND                                                
         B     EXITOK                                                           
*                                                                               
VFFH04   EX    RE,*+8                                                           
         BE    VFFH06                                                           
         CLC   FVIFLD(0),LC@REV    REVERSE ?                                    
         EX    RE,*+8                                                           
         BE    VFFH06                                                           
         CLC   FVIFLD(0),UC@REV    REVERSE ?                                    
         B     VFFH08                                                           
*                                                                               
VFFH06   OI    FDRTFHXT,FXATHREV                                                
         B     EXITOK                                                           
*                                                                               
VFFH08   EX    RE,*+8                                                           
         BE    VFFH10                                                           
         CLC   FVIFLD(0),LC@BLNK   BLINK ?                                      
         EX    RE,*+8                                                           
         BE    VFFH10                                                           
         CLC   FVIFLD(0),UC@BLNK   BLINK ?                                      
         B     VFFH12                                                           
*                                                                               
VFFH10   OI    FDRTFHXT,FXATHBLK                                                
         B     EXITOK                                                           
*                                                                               
VFFH12   B     EXITNV              ONLY THESE ARE VALID OPTIONS                 
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER INPUT FIELD HIGHLIGHT                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
FIHDTA   LA    RF,FIHTBL           TABLE OF KNOWN VERBS                         
         L     R3,AFLTRL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
FIHTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFIH)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFIH)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER INPUT FIELD HIGHLIGHT                                *         
***********************************************************************         
         SPACE 1                                                                
DISFIH   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         TM    FDRFHXA,FXATHUND    X'C0' - UNDERSCORE                           
         BZ    EXITOK              NOTHING SET                                  
         BNO   *+14                                                             
         MVC   FVIFLD(L'LC@UNDR),LC@UNDR                                        
         B     EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@REV),LC@REV                                          
         TM    FDRFHXA,FXATHREV    X'80' - REVERSE VIDEO                        
         BO    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@BLNK),LC@BLNK                                        
         B     EXITOK              MUST BE BLINK THEN                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER INPUT FIELD HIGHLIGHT                               *         
***********************************************************************         
         SPACE 1                                                                
VALFIH   LTR   R3,R3               FDREL?                                       
         BNZ   VFIH01                                                           
         CLI   FVILEN,0                                                         
         BNE   EXITNV              NO INPUT IS VALID                            
         B     EXITOK                                                           
*                                                                               
VFIH01   NI    FDRFHXA,FF-(FXATHUND)                                            
         CLI   FVILEN,0                                                         
         BE    EXITOK              NO HIGHLIGHT SET                             
*                                                                               
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         BE    VFIH02                                                           
         CLC   FVIFLD(0),LC@UNDR   UNDERSCORE ?                                 
         EX    RE,*+8                                                           
         BE    VFIH02                                                           
         CLC   FVIFLD(0),UC@UNDR   UNDERSCORE ?                                 
         B     VFIH04                                                           
*                                                                               
VFIH02   OI    FDRFHXA,FXATHUND                                                 
         B     EXITOK                                                           
*                                                                               
VFIH04   EX    RE,*+8                                                           
         BE    VFIH06                                                           
         CLC   FVIFLD(0),LC@REV    REVERSE ?                                    
         EX    RE,*+8                                                           
         BE    VFIH06                                                           
         CLC   FVIFLD(0),UC@REV    REVERSE ?                                    
         B     VFIH08                                                           
*                                                                               
VFIH06   OI    FDRFHXA,FXATHREV                                                 
         B     EXITOK                                                           
*                                                                               
VFIH08   EX    RE,*+8                                                           
         BE    VFIH10                                                           
         CLC   FVIFLD(0),LC@BLNK   BLINK ?                                      
         EX    RE,*+8                                                           
         BE    VFIH10                                                           
         CLC   FVIFLD(0),UC@BLNK   BLINK ?                                      
         B     VFIH12                                                           
*                                                                               
VFIH10   OI    FDRFHXA,FXATHBLK                                                 
         B     EXITOK                                                           
*                                                                               
VFIH12   B     EXITNV              ONLY THESE ARE VALID OPTIONS                 
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER TAG FIELD CASE                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
FFCDTA   LA    RF,FFCTBL           TABLE OF KNOWN VERBS                         
         L     R3,AFLTRL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
FFCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFFC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFFC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER TAG FIELD CASE                                       *         
***********************************************************************         
         SPACE 1                                                                
DISFFC   LTR   R3,R3                                                            
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@LOWER),LC@LOWER                                      
         TM    FDRTFHAT,FATBLC                                                  
         BO    EXITOK              LOWER CASE IS SET                            
*                                                                               
         MVC   FVIFLD(L'LC@NUMBR),LC@NUMBR                                      
         TM    FDRTFHAT,FATBNUM                                                 
         BO    EXITOK              NUMERIC FIELD SET                            
*                                                                               
         MVC   FVIFLD(L'LC@UPPER),LC@UPPER                                      
         B     EXITOK              DEFAULT IS UPPERCASE                         
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER TAG FIELD CASE                                      *         
***********************************************************************         
         SPACE 1                                                                
VALFFC   LTR   R3,R3                                                            
         BNZ   VFFC01                                                           
         CLI   FVILEN,0                                                         
         BNE   EXITNV                                                           
         B     EXITOK                                                           
*                                                                               
VFFC01   NI    FDRTFHAT,FF-(FATBLC+FATBNUM)                                     
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS UPPERCASE                         
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8                                                           
         BE    VFFC02                                                           
         CLC   FVIFLD(0),LC@LOWER                                               
         EX    RE,*+8                                                           
         BE    VFFC02                                                           
         CLC   FVIFLD(0),UC@LOWER                                               
         B     VFFC04                                                           
*                                                                               
VFFC02   OI    FDRTFHAT,FATBLC                                                  
         B     EXITOK              LOWER CASE IS SET                            
*                                                                               
VFFC04   EX    RE,*+8                                                           
         BE    VFFC06                                                           
         CLC   FVIFLD(0),LC@NUMBR                                               
         EX    RE,*+8                                                           
         BE    VFFC06                                                           
         CLC   FVIFLD(0),UC@NUMBR                                               
         B     VFFC08                                                           
*                                                                               
VFFC06   OI    FDRTFHAT,FATBNUM                                                 
         B     EXITOK              NUMERIC FIELD SET                            
*                                                                               
VFFC08   EX    RE,*+8                                                           
         BE    VFFC10                                                           
         CLC   FVIFLD(0),LC@UPPER                                               
         EX    RE,*+8                                                           
         BE    VFFC10                                                           
         CLC   FVIFLD(0),UC@UPPER                                               
         B     VFFC12                                                           
*                                                                               
VFFC10   B     EXITOK              UPPERCASE IS SET                             
*                                                                               
VFFC12   B     EXITNV              NOTHING ELSE IS VALID                        
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER INPUT FIELD CASE                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
CIFDTA   LA    RF,CIFTBL           TABLE OF KNOWN VERBS                         
         L     R3,AFLTRL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
CIFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCIF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCIF)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER INPUT FIELD CASE                                     *         
***********************************************************************         
         SPACE 1                                                                
DISCIF   LTR   R3,R3                                                            
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@LOWER),LC@LOWER                                      
         TM    FDRFHAT,FATBLC                                                   
         BO    EXITOK              LOWER CASE IS SET                            
*                                                                               
         MVC   FVIFLD(L'LC@NUMBR),LC@NUMBR                                      
         TM    FDRFHAT,FATBNUM                                                  
         BO    EXITOK              NUMERIC FIELD SET                            
*                                                                               
         MVC   FVIFLD(L'LC@UPPER),LC@UPPER                                      
         B     EXITOK              DEFAULT IS UPPERCASE                         
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER INPUT FIELD CASE                                    *         
***********************************************************************         
         SPACE 1                                                                
VALCIF   LTR   R3,R3                                                            
         BNZ   VCIF01                                                           
         CLI   FVILEN,0                                                         
         BNE   EXITNV                                                           
         B     EXITOK                                                           
*                                                                               
VCIF01   NI    FDRFHAT,FF-(FATBLC+FATBNUM)                                      
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS UPPERCASE                         
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8                                                           
         BE    VCIF02                                                           
         CLC   FVIFLD(0),LC@LOWER                                               
         EX    RE,*+8                                                           
         BE    VCIF02                                                           
         CLC   FVIFLD(0),UC@LOWER                                               
         B     VCIF04                                                           
*                                                                               
VCIF02   OI    FDRFHAT,FATBLC                                                   
         B     EXITOK              LOWER CASE IS SET                            
*                                                                               
VCIF04   EX    RE,*+8                                                           
         BE    VCIF06                                                           
         CLC   FVIFLD(0),LC@NUMBR                                               
         EX    RE,*+8                                                           
         BE    VCIF06                                                           
         CLC   FVIFLD(0),UC@NUMBR                                               
         B     VCIF08                                                           
*                                                                               
VCIF06   OI    FDRFHAT,FATBNUM                                                  
         B     EXITOK              NUMERIC FIELD SET                            
*                                                                               
VCIF08   EX    RE,*+8                                                           
         BE    VCIF10                                                           
         CLC   FVIFLD(0),LC@UPPER                                               
         EX    RE,*+8                                                           
         BE    VCIF10                                                           
         CLC   FVIFLD(0),UC@UPPER                                               
         B     VCIF12                                                           
*                                                                               
VCIF10   B     EXITOK              UPPERCASE IS SET                             
*                                                                               
VCIF12   B     EXITNV              NOTHING ELSE IS VALID                        
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER TAG FIELD INTENSITY                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
FFIDTA   LA    RF,FFITBL           TABLE OF KNOWN VERBS                         
         L     R3,AFLTRL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
FFITBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFFI)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFFI)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER TAG FIELD INTENSITY                                  *         
***********************************************************************         
         SPACE 1                                                                
DISFFI   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@LOW),LC@LOW                                          
         TM    FDRTFHAT,FATBLOW                                                 
         BO    EXITOK              LOW INTENSITY SET                            
*                                                                               
         MVC   FVIFLD(L'LC@HIGH),LC@HIGH                                        
         TM    FDRTFHAT,FATBHIGH                                                
         BO    EXITOK              HIGH INTENSITY SET                           
*                                                                               
         MVC   FVIFLD(L'LC@NORM),LC@NORM                                        
         B     EXITOK              DEFAULT IS NORMAL                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER TAG FIELD INTENSITY                                 *         
***********************************************************************         
         SPACE 1                                                                
VALFFI   LTR   R3,R3               FLTRL?                                       
         BNZ   VFFI01                                                           
         CLI   FVILEN,0                                                         
         BNE   EXITNV                                                           
         B     EXITOK                                                           
*                                                                               
VFFI01   NI    FDRTFHAT,FF-(FATBLOW)                                            
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NORMAL                            
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8                                                           
         BE    VFFI02                                                           
         CLC   FVIFLD(0),LC@LOW                                                 
         EX    RE,*+8                                                           
         BE    VFFI02                                                           
         CLC   FVIFLD(0),UC@LOW                                                 
         B     VFFI04                                                           
*                                                                               
VFFI02   OI    FDRTFHAT,FATBLOW                                                 
         B     EXITOK              LOW INTENSITY SET                            
*                                                                               
VFFI04   EX    RE,*+8                                                           
         BE    VFFI06                                                           
         CLC   FVIFLD(0),LC@HIGH                                                
         EX    RE,*+8                                                           
         BE    VFFI06                                                           
         CLC   FVIFLD(0),UC@HIGH                                                
         B     VFFI08                                                           
*                                                                               
VFFI06   OI    FDRTFHAT,FATBHIGH                                                
         B     EXITOK              HIGH INTENSITY SET                           
*                                                                               
VFFI08   EX    RE,*+8                                                           
         BE    VFFI10                                                           
         CLC   FVIFLD(0),LC@NORM                                                
         EX    RE,*+8                                                           
         BE    VFFI10                                                           
         CLC   FVIFLD(0),UC@NORM                                                
         B     VFFI12                                                           
*                                                                               
VFFI10   B     EXITOK              NORMAL INTENSITY IS SET                      
*                                                                               
VFFI12   B     EXITNV              NOTHING ELSE IS VALID                        
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER INPUT FIELD INTENSITY                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
IFFDTA   LA    RF,IFFTBL           TABLE OF KNOWN VERBS                         
         L     R3,AFLTRL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
IFFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISIFF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALIFF)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER INPUT FIELD INTENSITY                                *         
***********************************************************************         
         SPACE 1                                                                
DISIFF   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@LOW),LC@LOW                                          
         TM    FDRFHAT,FATBLOW                                                  
         BO    EXITOK              LOW INTENSITY SET                            
*                                                                               
         MVC   FVIFLD(L'LC@HIGH),LC@HIGH                                        
         TM    FDRFHAT,FATBHIGH                                                 
         BO    EXITOK              HIGH INTENSITY SET                           
*                                                                               
         MVC   FVIFLD(L'LC@NORM),LC@NORM                                        
         B     EXITOK              DEFAULT IS NORMAL                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER INPUT FIELD INTENSITY                               *         
***********************************************************************         
         SPACE 1                                                                
VALIFF   LTR   R3,R3               FLTRL?                                       
         BNZ   VIFF01                                                           
         CLI   FVILEN,0                                                         
         BNE   EXITNV                                                           
         B     EXITOK                                                           
*                                                                               
VIFF01   NI    FDRFHAT,FF-(FATBLOW)                                             
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NORMAL                            
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8                                                           
         BE    VIFF02                                                           
         CLC   FVIFLD(0),LC@LOW                                                 
         EX    RE,*+8                                                           
         BE    VIFF02                                                           
         CLC   FVIFLD(0),UC@LOW                                                 
         B     VIFF04                                                           
*                                                                               
VIFF02   OI    FDRFHAT,FATBLOW                                                  
         B     EXITOK              LOW INTENSITY SET                            
*                                                                               
VIFF04   EX    RE,*+8                                                           
         BE    VIFF06                                                           
         CLC   FVIFLD(0),LC@HIGH                                                
         EX    RE,*+8                                                           
         BE    VIFF06                                                           
         CLC   FVIFLD(0),UC@HIGH                                                
         B     VIFF08                                                           
*                                                                               
VIFF06   OI    FDRFHAT,FATBHIGH                                                 
         B     EXITOK              HIGH INTENSITY SET                           
*                                                                               
VIFF08   EX    RE,*+8                                                           
         BE    VIFF10                                                           
         CLC   FVIFLD(0),LC@NORM                                                
         EX    RE,*+8                                                           
         BE    VIFF10                                                           
         CLC   FVIFLD(0),UC@NORM                                                
*                                                                               
VIFF10   B     EXITOK              NORMAL INTENSITY IS SET                      
*                                                                               
VIFF12   B     EXITNV              NOTHING ELSE IS VALID                        
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR LENGTH OF FILTERABLE DATA (STORED ON FLTEL)         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
XFLDTA   LA    RF,XFLTBL           TABLE OF KNOWN VERBS                         
         L     R3,AFLTRL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
XFLTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISXFL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALXFL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LENGTH OF FILTER DATA TO BE SAVED                           *         
***********************************************************************         
         SPACE 1                                                                
DISXFL   LTR   R3,R3               FLTRL?                                       
         BZ    EXITOK                                                           
         CLI   FDRFSAVL,0          FILTER DATA LENGTH IS REQUIRED               
         BE    EXITOK                                                           
         CURED FDRFSAVL,(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE LENGTH OF FILTER DATA TO BE SAVED                          *         
***********************************************************************         
         SPACE 1                                                                
VALXFL   LTR   R3,R3               FLTRL?                                       
         BNZ   VXFL02                                                           
         CLI   FVILEN,0                                                         
         BNE   EXITNV                                                           
         B     EXITOK                                                           
*                                                                               
VXFL02   MVI   FDRFSAVL,0          MAXIMUM INPUT LENGTH IS OPTIONAL             
         TM    FVIIND,FVINUM       ANY INPUT MUST BE NUMERIC                    
         BZ    EXITNOTN                                                         
         ICM   RF,15,BCFULL        IF THEY TYPE IN A 0                          
         BZ    EXITNV              CANNOT FILTER WITH NO DATA                   
         STC   RF,FDRFSAVL                                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER DICTIONARY FIELD DISPLAY                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
FDDDTA   LA    RF,FDDTBL           TABLE OF KNOWN VERBS                         
         L     R3,AFLTRL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
FDDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFDD)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER FIELD DICTIONARY EQUATE                              *         
***********************************************************************         
         SPACE 1                                                                
DISFDD   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         OC    FDRRTAG,FDRRTAG                                                  
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'FDRRTAG),FDRRTAG                                        
         ICM   RF,15,=C'SL  '                                                   
         ICM   RF,2,FDRKSYS                                                     
         GOTO1 VDICTAT,BOPARM,(RF),FVIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER - NOT                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
NOTDTA   LA    RF,NOTTBL           TABLE OF KNOWN VERBS                         
         L     R3,AFLTRL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
NOTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNOT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNOT)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER CAN BE NOT                                           *         
***********************************************************************         
         SPACE 1                                                                
DISNOT   LTR   R3,R3               FLTRL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    FDRFVAL1,FDRF1NOT                                                
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER CAN BE NOT                                          *         
***********************************************************************         
         SPACE 1                                                                
VALNOT   LTR   R3,R3               FLTRL?                                       
         BNZ   VNOT02                                                           
         CLI   FVILEN,0                                                         
         BNE   EXITNV                                                           
         B     EXITOK                                                           
*                                                                               
VNOT02   NI    FDRFVAL1,FF-(FDRF1NOT)                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VNOT04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VNOT04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VNOT06                                                           
*                                                                               
VNOT04   OI    FDRFVAL1,FDRF1NOT                                                
         B     EXITOK                                                           
*                                                                               
VNOT06   B     EXITNV              NOTHING ELSE IS VALID                        
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER - EQUAL TO                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
NEQDTA   LA    RF,NEQTBL           TABLE OF KNOWN VERBS                         
         L     R3,AFLTRL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
NEQTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNEQ)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNEQ)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER CAN BE EQUAL TO                                      *         
***********************************************************************         
         SPACE 1                                                                
DISNEQ   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    FDRFVAL1,FDRF1EQ                                                 
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER CAN BE EQUAL TO                                     *         
***********************************************************************         
         SPACE 1                                                                
VALNEQ   LTR   R3,R3               FDREL?                                       
         BNZ   VNEQ02                                                           
         CLI   FVILEN,0                                                         
         BNE   EXITNV                                                           
         B     EXITOK                                                           
*                                                                               
VNEQ02   NI    FDRFVAL1,FF-(FDRF1EQ)                                            
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VNEQ04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VNEQ04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VNEQ06                                                           
*                                                                               
VNEQ04   OI    FDRFVAL1,FDRF1EQ                                                 
         B     EXITOK                                                           
*                                                                               
VNEQ06   B     EXITNV              NOTHING ELSE IS VALID                        
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER - GREATER THAN                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
NGTDTA   LA    RF,NGTTBL           TABLE OF KNOWN VERBS                         
         L     R3,AFLTRL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
NGTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNGT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNGT)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER CAN BE GREATER THAN                                  *         
***********************************************************************         
         SPACE 1                                                                
DISNGT   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    FDRFVAL1,FDRF1GT                                                 
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER CAN BE GREATER THAN                                 *         
***********************************************************************         
         SPACE 1                                                                
VALNGT   LTR   R3,R3               FDREL?                                       
         BNZ   VNGT02                                                           
         CLI   FVILEN,0                                                         
         BNE   EXITNV                                                           
         B     EXITOK                                                           
*                                                                               
VNGT02   NI    FDRFVAL1,FF-(FDRF1GT)                                            
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VNGT04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VNGT04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         B     VNGT06                                                           
*                                                                               
VNGT04   OI    FDRFVAL1,FDRF1GT                                                 
         B     EXITOK                                                           
*                                                                               
VNGT06   B     EXITNV              NOTHING ELSE IS VALID                        
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER - LESS THAN                                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
NLTDTA   LA    RF,NLTTBL           TABLE OF KNOWN VERBS                         
         L     R3,AFLTRL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
NLTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNLT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNLT)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER CAN BE LESS THAN                                     *         
***********************************************************************         
         SPACE 1                                                                
DISNLT   LTR   R3,R3               FLTRL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    FDRFVAL1,FDRF1LT                                                 
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER CAN BE LESS THAN                                    *         
***********************************************************************         
         SPACE 1                                                                
VALNLT   LTR   R3,R3               FLTRL?                                       
         BNZ   VNLT02                                                           
         CLI   FVILEN,0                                                         
         BNE   EXITNV                                                           
         B     EXITOK                                                           
*                                                                               
VNLT02   NI    FDRFVAL1,FF-(FDRF1LT)                                            
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VNLT04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VNLT04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
*                                                                               
VNLT04   OI    FDRFVAL1,FDRF1LT                                                 
         B     EXITOK                                                           
*                                                                               
VNLT06   B     EXITNV              NOTHING ELSE IS VALID                        
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER - DEFAULT VALUE                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
NDFDTA   LA    RF,NDFTBL           TABLE OF KNOWN VERBS                         
         L     R3,AFLTRL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
NDFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNDF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNDF)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER DEFAULT VALUE                                        *         
***********************************************************************         
         SPACE 1                                                                
DISNDF   LTR   R3,R3               FLTRL?                                       
         BZ    EXITOK                                                           
*                                                                               
         LA    RF,FVIFLD                                                        
         TM    FDRFVAL1,FDRD1NOT   TEST 'NOT'                                   
         BZ    *+12                                                             
         MVI   0(RF),C'*'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
         TM    FDRFVAL1,FDRD1LT    TEST 'LESS THAN'                             
         BZ    *+12                                                             
         MVI   0(RF),C'<'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
         TM    FDRFVAL1,FDRD1GT    TEST 'GREATER THAN'                          
         BZ    *+12                                                             
         MVI   0(RF),C'>'                                                       
         LA    RF,1(RF)                                                         
*                                                                               
         TM    FDRFVAL1,FDRD1EQ    TEST 'EQUAL TO'                              
         BZ    *+12                                                             
         MVI   0(RF),C'='                                                       
         LA    RF,1(RF)                                                         
*                                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER DEFAULT VALUE                                       *         
***********************************************************************         
         SPACE 1                                                                
VALNDF   LTR   R3,R3               FLTRL?                                       
         BNZ   VNDF02                                                           
         CLI   FVILEN,0                                                         
         BNE   EXITNV                                                           
         B     EXITOK                                                           
*                                                                               
VNDF02   NI    FDRFVAL1,FF-(FDRD1NOT+FDRD1LT+FDRD1EQ+FDRD1GT)                   
         CLI   FVILEN,1                                                         
         BNE   VNDF04                                                           
*                                                                               
         CLI   FVIFLD,C'*'         TEST 'NOT'                                   
         BNE   *+12                                                             
         OI    FDRFVAL1,FDRD1NOT                                                
         B     EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'<'         TEST 'LESS THAN'                             
         BNE   *+12                                                             
         OI    FDRFVAL1,FDRD1LT                                                 
         B     EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'>'         TEST 'GREATER THAN'                          
         BNE   *+12                                                             
         OI    FDRFVAL1,FDRD1GT                                                 
         B     EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'='                                                      
         BNE   *+12                                                             
         OI    FDRFVAL1,FDRD1EQ    TEST 'EQUAL TO'                              
         B     EXITOK                                                           
*                                                                               
         B     EXITNV              NO OTHER 1 CHAR I/P IS VALID                 
*                                                                               
VNDF04   CLI   FVILEN,2                                                         
         BNE   EXITNV                                                           
*                                                                               
         CLC   =C'*=',FVIFLD       TEST 'NOT EQUAL'                             
         BNE   *+12                                                             
         OI    FDRFVAL1,FDRD1NOT+FDRD1EQ                                        
         B     EXITOK                                                           
*                                                                               
         CLC   =C'<>',FVIFLD       TEST 'NOT EQUAL'                             
         BNE   *+12                                                             
         OI    FDRFVAL1,FDRD1NOT+FDRD1EQ                                        
         B     EXITOK                                                           
*                                                                               
         CLC   =C'*<',FVIFLD       TEST 'NOT LESS THAN'                         
         BNE   *+12                                                             
         OI    FDRFVAL1,FDRD1NOT+FDRD1LT                                        
         B     EXITOK                                                           
*                                                                               
         CLC   =C'>=',FVIFLD       TEST 'NOT LESS THAN'                         
         BNE   *+12                                                             
         OI    FDRFVAL1,FDRD1NOT+FDRD1LT                                        
         B     EXITOK                                                           
*                                                                               
         CLC   =C'*>',FVIFLD       TEST 'NOT GREATER THEN'                      
         BNE   *+12                                                             
         OI    FDRFVAL1,FDRD1NOT+FDRD1GT                                        
         B     EXITOK                                                           
*                                                                               
         CLC   =C'<=',FVIFLD       TEST 'NOT GREATER THEN'                      
         BNE   *+12                                                             
         OI    FDRFVAL1,FDRD1NOT+FDRD1GT                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER FIELD IS REQUIRED                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
FFQDTA   LA    RF,FFQTBL           TABLE OF KNOWN VERBS                         
         L     R3,AFLTRL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
FFQTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFFQ)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFFQ)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FIELD IS REQUIRED                                           *         
***********************************************************************         
         SPACE 1                                                                
DISFFQ   LTR   R3,R3               FLTRL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    FDRINDS1,FDR1REQ                                                 
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FIELD IS REQUIRED                                          *         
***********************************************************************         
         SPACE 1                                                                
VALFFQ   LTR   R3,R3               FLTRL?                                       
         BNZ   VFFQ02                                                           
         CLI   FVILEN,0                                                         
         BNE   EXITNV                                                           
         B     EXITOK                                                           
*                                                                               
VFFQ02   NI    FDRINDS1,FF-(FDR1REQ)                                            
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VFFQ04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VFFQ04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VFFQ06                                                           
*                                                                               
VFFQ04   OI    FDRINDS1,FDR1REQ                                                 
         B     EXITOK                                                           
*                                                                               
VFFQ06   B     EXITNV              NOTHING ELSE IS VALID                        
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR SELECT TO END OF LIST IS VALID                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
EOLDTA   LA    RF,EOLTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
EOLTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISEOL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALEOL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SELECT TO END OF LIST IS VALID                              *         
***********************************************************************         
         SPACE 1                                                                
DISEOL   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    FDRLIND1,FDRLIEOL                                                
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FIELD IS REQUIRED                                          *         
***********************************************************************         
         SPACE 1                                                                
VALEOL   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
*                                                                               
VEOL02   NI    FDRLIND1,FF-(FDRLIEOL)                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VEOL04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VEOL04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VEOL06                                                           
*                                                                               
VEOL04   OI    FDRLIND1,FDRLIEOL                                                
         B     EXITOK                                                           
*                                                                               
VEOL06   B     EXITNV              NOTHING ELSE IS VALID                        
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SELECT TO END OF PAGE IS VALID                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
EOPDTA   LA    RF,EOPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
EOPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISEOP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALEOP)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SELECT TO END OF PAGE IS VALID                              *         
***********************************************************************         
         SPACE 1                                                                
DISEOP   LTR   R3,R3               FDREL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    FDRLIND1,FDRLIEOP                                                
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SELECT TO END OF PAGE IS VALID                             *         
***********************************************************************         
         SPACE 1                                                                
VALEOP   LTR   R3,R3               FDREL?                                       
         BZ    DIE                                                              
*                                                                               
VEOP02   NI    FDRLIND1,FF-(FDRLIEOP)                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VEOP04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VEOP04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VEOP06                                                           
*                                                                               
VEOP04   OI    FDRLIND1,FDRLIEOP                                                
         B     EXITOK                                                           
*                                                                               
VEOP06   B     EXITNV              NOTHING ELSE IS VALID                        
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR FILTER - RANGE VALID                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
RNGDTA   LA    RF,RNGTBL           TABLE OF KNOWN VERBS                         
         L     R3,AFLTRL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
RNGTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISRNG)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRNG)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER RANGE IS VALID                                       *         
***********************************************************************         
         SPACE 1                                                                
DISRNG   LTR   R3,R3               FLTRL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    FDRFVAL2,FDRF2RNG                                                
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER RANGE IS VALID                                      *         
***********************************************************************         
         SPACE 1                                                                
VALRNG   LTR   R3,R3               FLTRL?                                       
         BNZ   VRNG02                                                           
         CLI   FVILEN,0                                                         
         BNE   EXITNV                                                           
         B     EXITOK                                                           
*                                                                               
VRNG02   NI    FDRFVAL2,FF-(FDRF2RNG)                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VRNG04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VRNG04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VRNG06                                                           
*                                                                               
VRNG04   OI    FDRFVAL2,FDRF2RNG                                                
         B     EXITOK                                                           
*                                                                               
VRNG06   B     EXITNV              NOTHING ELSE IS VALID                        
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER - LIST VALID                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
LSTDTA   LA    RF,LSTTBL           TABLE OF KNOWN VERBS                         
         L     R3,AFLTRL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
LSTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLST)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLST)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER RANGE IS VALID                                       *         
***********************************************************************         
         SPACE 1                                                                
DISLST   LTR   R3,R3               FLTRL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    FDRFVAL2,FDRF2LST                                                
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER RANGE IS VALID                                      *         
***********************************************************************         
         SPACE 1                                                                
VALLST   LTR   R3,R3               FLTRL?                                       
         BNZ   VLST02                                                           
         CLI   FVILEN,0                                                         
         BNE   EXITNV                                                           
         B     EXITOK                                                           
*                                                                               
VLST02   NI    FDRFVAL2,FF-(FDRF2LST)                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VLST04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VLST04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VLST06                                                           
*                                                                               
VLST04   OI    FDRFVAL2,FDRF2LST                                                
         B     EXITOK                                                           
*                                                                               
VLST06   B     EXITNV              NOTHING ELSE IS VALID                        
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER - OWNER HANDLES FILTERING                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
OWNDTA   LA    RF,OWNTBL           TABLE OF KNOWN VERBS                         
         L     R3,AFLTRL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
OWNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISOWN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOWN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER IS HANDLED BY OWNER                                  *         
***********************************************************************         
         SPACE 1                                                                
DISOWN   LTR   R3,R3               FLTRL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    FDRFVAL2,FDRF2OWN                                                
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER IS HANDLED BY OWNER                                 *         
***********************************************************************         
         SPACE 1                                                                
VALOWN   LTR   R3,R3               FLTRL?                                       
         BNZ   VOWN02                                                           
         CLI   FVILEN,0                                                         
         BNE   EXITNV                                                           
         B     EXITOK                                                           
*                                                                               
VOWN02   NI    FDRFVAL2,FF-(FDRF2OWN)                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VOWN04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VOWN04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VOWN06                                                           
*                                                                               
VOWN04   OI    FDRFVAL2,FDRF2OWN                                                
         B     EXITOK                                                           
*                                                                               
VOWN06   B     EXITNV              NOTHING ELSE IS VALID                        
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
DISTST   OC    FDRKTEST,FDRKTEST                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'FDRKTEST),FDRKTEST                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE TEST VERSION                                               *         
***********************************************************************         
         SPACE 1                                                                
VALTST   XC    FDRKTEST,FDRKTEST                                                
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'A'                                                      
         BNE   *+12                                                             
         MVI   FDRKTEST,C'A'                                                    
         B     EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'B'                                                      
         BNE   *+12                                                             
         MVI   FDRKTEST,C'B'                                                    
         B     EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'C'                                                      
         BNE   *+12                                                             
         MVI   FDRKTEST,C'C'                                                    
         B     EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'X'                                                      
         BNE   *+12                                                             
         MVI   FDRKTEST,C'X'                                                    
         B     EXITOK                                                           
*                                                                               
         B     EXITNV              NOTHING ELSE IS VALID                        
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
DISCTR   MVC   BOBYTE1,FDRKCTRY    COUNTRY CODE IS 1`S COMP IN KEY              
         XC    BOBYTE1,BCEFFS                                                   
         LA    R1,CTRYTAB                                                       
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         A     RF,BORELO                                                        
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
         MVC   FDRKCTRY,BCEFFS     SET DEFAULT TO NATIVE COUNTRY                
         B     EXITOK                                                           
*                                                                               
VCTR01   XR    R4,R4                                                            
         IC    R4,FVXLEN                                                        
         LA    R1,CTRYTAB                                                       
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
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
VCTR06   MVC   FDRKCTRY,CTRYCODE   MOVE IN COUNTRY CODE                         
         XC    FDRKCTRY,BCEFFS     1`S COMPLIMENT IT                            
                                                                                
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
         BNE   *+16                                                             
         MVI   FDRKCTRY,FF         SET DEFAULT TO NATIVE COUNTRY                
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
VFCTR06  MVC   FDRKCTRY,CTRYCODE   MOVE IN COUNTRY CODE                         
         MVC   FLTIFLD(L'CTRYCODE),CTRYCODE                                     
         XC    FDRKCTRY,BCEFFS     1`S COMPLIMENT CODE IN KEY                   
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* DO COUNTRY FILTERING                                                *         
***********************************************************************         
         SPACE 1                                                                
DOFTCTR  MVC   BOBYTE1,FVIFLD                                                   
         XC    BOBYTE1,BCEFFS                                                   
         CLC   FDRKCTRY,BOBYTE1                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* NTRSES OBJECT                                                       *         
* -------------                                                       *         
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
         DC    AL1(SXITOUT),AL1(0,0,0),AL4(XITOUT)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER (OUT)                      *         
***********************************************************************         
         SPACE 1                                                                
NTROUT   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER (BACK)                     *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING FDRRECD,GSRECKEY                                                 
XITOUT   TM    SXINDS1,SXISEL                                                   
         BZ    EXITOK                                                           
         MVC   SDATA(2),FDRKNUM                                                 
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER                          *         
***********************************************************************         
         SPACE 1                                                                
NTRIN    DS    0H                                                               
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
THIS     USING FDRRECD,R2                                                       
LAST     USING FDRRECD,R3                                                       
         LA    RF,LISTABL                                                       
         B     ITER                                                             
*                                                                               
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
FLST     MVC   IOKEY(L'FDRKEY),THIS.FDRRECD                                     
         ICM   R1,15,=AL4(XIO11+XOGENDIR+XOHIGH)                                
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               FUCK UP ON THE READ HIGH                     
         B     NLST02                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     ICM   R1,15,=AL4(XIO11+XOGENDIR+XOSEQ)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               END OF FILE                                  
         SPACE 1                                                                
NLST02   CLC   IOKEY(FDRKSYS-FDRRECD),THIS.FDRRECD                              
         BNE   EXITL                                                            
         SPACE 1                                                                
         MVC   THIS.FDRKEY(FDRKLEN),IOKEY   WE WANT THIS KEY HERE...            
         B     EXITOK                                                           
         DROP  THIS,LAST                                                        
         SPACE 2                                                                
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
         SPACE 1                                                                
         DS    0D                                                               
DCLIST   DCDDL GE#BLUE,10,L                                                     
         DCDDL GE#RED,10,L                                                      
         DCDDL GE#PINK,10,L                                                     
         DCDDL GE#GRN,10,L                                                      
         DCDDL GE#TURQ,10,L                                                     
         DCDDL GE#YELLO,10,L                                                    
         DCDDL GE#WHITE,10,L                                                    
         DCDDL GE#UNDR,10,L                                                     
         DCDDL GE#REV,10,L                                                      
         DCDDL GE#BLNK,10,L                                                     
         DCDDL GE#UPPER,10,L                                                    
         DCDDL GE#LOWER,10,L                                                    
         DCDDL GE#NUMBR,10,L                                                    
         DCDDL GE#HIGH,10,L                                                     
         DCDDL GE#LOW,10,L                                                      
         DCDDL GE#NORM,10,L                                                     
         DCDDL GE#KEY,8,L                                                       
         DCDDL GE#RECD,8,L                                                      
         DCDDL CT#STT,8,L                                                       
         DCDDL GE#TSAR,8,L                                                      
         DCDDL GE#USER,8,L                                                      
         DCDDL CT#YES,4,L                                                       
         DCDDL GE#NO,4,L                                                        
DCLISTX  DC    X'00'                                                            
*                                                                               
       ++INCLUDE FACTRYTAB                                                      
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
AFDREL   DS    A                                                                
AFLTRL   DS    A                                                                
AFVADDR  DS    A                                                                
ESCLEN   DS    XL1                                                              
ESCCHAR  DS    XL2                                                              
MYSCAN   DS    3CL(SCBLKLQ)                                                     
*                                                                               
DSLISTU  DS    0D                                                               
UC@BLUE  DS    XL10                                                             
UC@RED   DS    XL10                                                             
UC@PINK  DS    XL10                                                             
UC@GRN   DS    XL10                                                             
UC@TURQ  DS    XL10                                                             
UC@YELLO DS    XL10                                                             
UC@WHITE DS    XL10                                                             
UC@UNDR  DS    XL10                                                             
UC@REV   DS    XL10                                                             
UC@BLNK  DS    XL10                                                             
UC@UPPER DS    XL10                                                             
UC@LOWER DS    XL10                                                             
UC@NUMBR DS    XL10                                                             
UC@HIGH  DS    XL10                                                             
UC@LOW   DS    XL10                                                             
UC@NORM  DS    XL10                                                             
UC@KEY   DS    XL8                                                              
UC@REC   DS    XL8                                                              
UC@STT   DS    XL8                                                              
UC@TSAR  DS    XL8                                                              
UC@USER  DS    XL8                                                              
UE@YES   DS    XL4                                                              
UE@NO    DS    XL4                                                              
*                                                                               
DSLISTL  DS    0D                                                               
LC@BLUE  DS    XL10                                                             
LC@RED   DS    XL10                                                             
LC@PINK  DS    XL10                                                             
LC@GRN   DS    XL10                                                             
LC@TURQ  DS    XL10                                                             
LC@YELLO DS    XL10                                                             
LC@WHITE DS    XL10                                                             
LC@UNDR  DS    XL10                                                             
LC@REV   DS    XL10                                                             
LC@BLNK  DS    XL10                                                             
LC@UPPER DS    XL10                                                             
LC@LOWER DS    XL10                                                             
LC@NUMBR DS    XL10                                                             
LC@HIGH  DS    XL10                                                             
LC@LOW   DS    XL10                                                             
LC@NORM  DS    XL10                                                             
LC@KEY   DS    XL8                                                              
LC@REC   DS    XL8                                                              
LC@STT   DS    XL8                                                              
LC@TSAR  DS    XL8                                                              
LC@USER  DS    XL8                                                              
LC@YES   DS    XL4                                                              
LC@NO    DS    XL4                                                              
         SPACE 2                                                                
*        CTFILWORK                                                              
         PRINT OFF                                                              
       ++INCLUDE CTFILWORK                                                      
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
*        DDCOMFACS                                                              
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*        DDFLDHDR                                                               
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
*        FACTRY                                                                 
         PRINT OFF                                                              
       ++INCLUDE FACTRY                                                         
         PRINT ON                                                               
*        FACTRYEQUS                                                             
         PRINT OFF                                                              
       ++INCLUDE FACTRYEQUS                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020CTFIL10X  08/22/00'                                      
         END                                                                    
