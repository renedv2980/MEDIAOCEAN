*          DATA SET SPTRA12    AT LEVEL 185 AS OF 04/19/10                      
*PHASE T21612A                                                                  
*                                                                               
*  TITLE: T21612 - NETWORK TRAFFIC PRODUCT DELETION ONLINE REPORT     *         
*                                                                     *         
*  COMMENTS:                                                          *         
*                                                                     *         
*                                                                     *         
*  CALLED FROM: TRAFFIC CONTROLLER (T21600), WHICH CALLS              *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATA MANAGER                                          *         
*                                                                     *         
*                                                                     *         
*  INPUTS: SEE SCREEN SPTRAA2 (T216A2)                                *         
*          SPTRAWORKD (SYSD)                                          *         
*          DDSPLWORKD (GEND)                                          *         
*                                                                     *         
*  OUTPUTS: ONLINE LIST OF PRODUCT CODE OCCURENCES                    *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R4 -                                                       *         
*          R5 -                                                       *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R8 - POINTER TO SPOOLD                                     *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - BASE REGISTER                                         *         
*          RC - POINTER TO GEND                                       *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)         *         
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER      *         
*                    (DDGENCON-T00A30)                                *         
*             AIO2 -                                                  *         
*             AIO3 -                                                  *         
*                                                                     *         
***********************************************************************         
***********************************************************************         
*                        CHANGES LOG                                  *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21612 PRODUCT DELETION ONLINE REPORT'                          
T21612   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21612*                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
                                                                                
         CLI   OFFLINE,C'Y'        BYPASS FOR OFFLINE                           
         BE    EXIT                                                             
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    EXIT                                                             
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    EXIT                                                             
         GOTO1 VSOXERR                                                          
                                                                                
EXIT     XIT1                                                                   
         EJECT                                                                  
*------------------------------------------------------------------*            
*        VALIDATE KEY ROUTINE                                                   
*------------------------------------------------------------------*            
                                                                                
VK       DS    0H                                                               
         LA    R2,FLDH             FAKE VALIDATE MEDIA                          
         MVC   FLDH,=X'0A01000184010001'                                        
         MVI   FLD,C'N'                                                         
         GOTO1 VALIMED                                                          
                                                                                
         XC    BCLT,BCLT           CLIENT                                       
         XC    QCLT,QCLT                                                        
         XC    WORK,WORK                                                        
         MVC   WORK(3),TRACLT                                                   
         LA    R2,TRACLTH                                                       
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         GOTO1 ANY                                                              
         GOTO1 VALICLT                                                          
         MVC   WKBINCLT,BCLT                                                    
                                                                                
         XC    PRODTAB,PRODTAB     CLEAR PRODUCT CODE TABLE                     
         LA    R2,TRAPRD1H                                                      
         CLI   5(R2),0             TEST PROD GROUP ENTERED                      
         BE    MISSERR                                                          
                                                                                
         LA    R4,PRDCNT           MAX NUMBER OF PRODS TO PROCESS               
         LA    R3,PRODTAB          TABLE TO STORE VALID PRD CODES               
         LA    R2,TRAPRD1H         POINT TO FIRST FIELD HEADER FOR PRD          
                                                                                
VK030    DS    0H                                                               
         CLI   5(R2),0             END OF INPUT PRODUCTS                        
         BE    VK080                                                            
         CLI   5(R2),1                                                          
         BE    INVERR                                                           
         GOTO1 VALIPRD          VALIDATE PRODUCT                                
         CLI   ERROR,0                                                          
         BNE   INVERR                                                           
         MVC   0(L'PRD1,R3),8(R2)                                               
         LLC   R5,0(R2)                                                         
         AR    R2,R5                                                            
         LA    R3,L'PRD1(R3)                                                    
         BCT   R4,VK030                                                         
                                                                                
VK080    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
*------------------------------------------------------------------*            
*        VALIDATE RECORD ROUTINE                                                
*------------------------------------------------------------------*            
VR       DS    0H                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    VR01                                                             
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VR01                                                             
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VR01                                                             
         GOTO1 VSOXERR                                                          
                                                                                
VR01     DS    0H                                                               
         L     R4,AIO                                                           
         LR    R6,R4                                                            
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
*-------------------------------------------------------------------*           
*        DISPLAY KEY                                                            
*-------------------------------------------------------------------*           
DK       DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
*-------------------------------------------------------------------*           
*        DISPLAY RECORD                                                         
*-------------------------------------------------------------------*           
DR       DS    0H                                                               
         XC    RECCOUNT(RECTLEN),RECCOUNT    CLEAR TAB OF PREV COUNTS           
                                                                                
         LA    R4,RECTCNT                                                       
         LA    R2,TRACTA1H                                                      
DR010    LA    R3,PRDCNT                                                        
DR020    XC    8(L'TRACTA1,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         LLC   R5,0(R2)                                                         
         AR    R2,R5                                                            
         BCT   R3,DR020                                                         
         LLC   R5,0(R2)               BUMP PAST BLANK LINE                      
         AR    R2,R5                                                            
         LLC   R5,0(R2)               BUMP PAST RECORD TYPE LABEL               
         AR    R2,R5                                                            
         BCT   R4,DR010                                                         
                                                                                
         MVC   TRAPTG1,SPACES                                                   
         MVC   TRAPTG1(3),PRD1                                                  
         OC    PRD1,PRD1                                                        
         BNZ   *+10                                                             
         MVC   TRAPTG1,=C'Prd1'                                                 
         OI    TRAPTG1H+6,X'80'                                                 
                                                                                
         MVC   TRAPTG2,SPACES                                                   
         MVC   TRAPTG2(3),PRD2                                                  
         OC    PRD2,PRD2                                                        
         BNZ   *+10                                                             
         MVC   TRAPTG2,=C'Prd2'                                                 
         OI    TRAPTG2H+6,X'80'                                                 
                                                                                
         MVC   TRAPTG3,SPACES                                                   
         MVC   TRAPTG3(3),PRD3                                                  
         OC    PRD3,PRD3                                                        
         BNZ   *+10                                                             
         MVC   TRAPTG3,=C'Prd3'                                                 
         OI    TRAPTG3H+6,X'80'                                                 
                                                                                
         MVC   TRAPTG4,SPACES                                                   
         MVC   TRAPTG4(3),PRD4                                                  
         OC    PRD4,PRD4                                                        
         BNZ   *+10                                                             
         MVC   TRAPTG4,=C'Prd4'                                                 
         OI    TRAPTG4H+6,X'80'                                                 
                                                                                
         MVC   TRAPTG5,SPACES                                                   
         MVC   TRAPTG5(3),PRD5                                                  
         OC    PRD5,PRD5                                                        
         BNZ   *+10                                                             
         MVC   TRAPTG5,=C'Prd5'                                                 
         OI    TRAPTG5H+6,X'80'                                                 
                                                                                
         MVC   TRAPTG6,SPACES                                                   
         MVC   TRAPTG6(3),PRD6                                                  
         OC    PRD6,PRD6                                                        
         BNZ   *+10                                                             
         MVC   TRAPTG6,=C'Prd6'                                                 
         OI    TRAPTG6H+6,X'80'                                                 
                                                                                
         MVC   TRAPTG7,SPACES                                                   
         MVC   TRAPTG7(3),PRD7                                                  
         OC    PRD7,PRD7                                                        
         BNZ   *+10                                                             
         MVC   TRAPTG7,=C'Prd7'                                                 
         OI    TRAPTG7H+6,X'80'                                                 
                                                                                
         MVC   TRAPTG8,SPACES                                                   
         MVC   TRAPTG8(3),PRD8                                                  
         OC    PRD8,PRD8                                                        
         BNZ   *+10                                                             
         MVC   TRAPTG8,=C'Prd8'                                                 
         OI    TRAPTG8H+6,X'80'                                                 
                                                                                
                                                                                
         BRAS  RE,PROCCMML                                                      
         LA    R4,PRDCNT                                                        
         LA    R3,COMLCNT                                                       
         LA    R2,TRACTA1H                                                      
DR050    DS    0H                                                               
         MVC   TEMPHALF,0(R3)                                                   
         EDIT  TEMPHALF,(4,8(R2)),0,ZERO=NOBLANK                                
         OI    6(R2),X'80'                                                      
         LA    R3,L'TEMPHALF(R3)                                                
         LLC   R5,0(R2)                                                         
         AR    R2,R5                                                            
         BCT   R4,DR050                                                         
                                                                                
         BRAS  RE,PROCXNPT                                                      
         LA    R4,PRDCNT                                                        
         LA    R3,PTRNCNT                                                       
         LA    R2,TRACTB1H                                                      
DR060    DS    0H                                                               
         MVC   TEMPHALF,0(R3)                                                   
         EDIT  TEMPHALF,(4,8(R2)),0,ZERO=NOBLANK                                
         OI    6(R2),X'80'                                                      
         LA    R3,L'TEMPHALF(R3)                                                
         LLC   R5,0(R2)                                                         
         AR    R2,R5                                                            
         BCT   R4,DR060                                                         
                                                                                
                                                                                
         BRAS  RE,PROCXREV                                                      
         LA    R4,PRDCNT                                                        
         LA    R3,REVNCNT                                                       
         LA    R2,TRACTC1H                                                      
DR070    DS    0H                                                               
         MVC   TEMPHALF,0(R3)                                                   
         EDIT  TEMPHALF,(4,8(R2)),0,ZERO=NOBLANK                                
         OI    6(R2),X'80'                                                      
         LA    R3,L'TEMPHALF(R3)                                                
         LLC   R5,0(R2)                                                         
         AR    R2,R5                                                            
         BCT   R4,DR070                                                         
                                                                                
                                                                                
         BRAS  RE,PROCDT2                                                       
         LA    R4,PRDCNT                                                        
         LA    R3,CMNTCNT                                                       
         LA    R2,TRACTD1H                                                      
DR080    DS    0H                                                               
         MVC   TEMPHALF,0(R3)                                                   
         EDIT  TEMPHALF,(4,8(R2)),0,ZERO=NOBLANK                                
         OI    6(R2),X'80'                                                      
         LA    R3,L'TEMPHALF(R3)                                                
         LLC   R5,0(R2)                                                         
         AR    R2,R5                                                            
         BCT   R4,DR080                                                         
                                                                                
                                                                                
         BRAS  RE,PROCPRDL                                                      
         LA    R4,PRDCNT                                                        
         LA    R3,PCONCNT                                                       
         LA    R2,TRACTE1H                                                      
DR090    DS    0H                                                               
         MVC   TEMPHALF,0(R3)                                                   
         EDIT  TEMPHALF,(4,8(R2)),0,ZERO=NOBLANK                                
         OI    6(R2),X'80'                                                      
         LA    R3,L'TEMPHALF(R3)                                                
         LLC   R5,0(R2)                                                         
         AR    R2,R5                                                            
         BCT   R4,DR090                                                         
                                                                                
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
*---------------------------------------------------------------------*         
*        COMMERCIAL RECORD                                                      
*---------------------------------------------------------------------*         
PROCCMML NTR1                                                                   
         LA    R4,WKKEY                                                         
         XC    WKKEY,WKKEY                                                      
         MVC   WKKEY(2),=X'0A21'   COMMERCIAL RECORD (SPTDIR/SPTFIL)            
         MVC   WKKEY+2(1),BAGYMD   AGY/MEDIA                                    
         MVC   WKKEY+3(2),WKBINCLT CLIENT                                       
         MVC   WKKEYCMP,WKKEY      FOR COMPARISON                               
                                                                                
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',WKKEY,WKKEY                   
         B     PCML060                                                          
                                                                                
PCML050  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'SPTDIR',WKKEY,WKKEY                   
PCML060  CLC   WKKEY(5),WKKEYCMP                                                
         BNE   PCMLXIT                                                          
                                                                                
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',WKKEY+14,AIO3                 
                                                                                
         MVI   DATADISP+1,24                                                    
         MVI   ELCODE,X'29'                                                     
         L     R6,AIO3                                                          
         BAS   RE,GETEL                                                         
         BNE   PCML050                                                          
         USING CMLMPREL,R6                                                      
         SR    R5,R5                                                            
         IC    R5,CMLMPRLN                                                      
         SH    R5,=H'2'                                                         
         CH    R5,=H'3'                                                         
         BL    PCML050                                                          
         LA    R4,2(R6)                                                         
PCML080  DS    0H                                                               
                                                                                
         MVC   PRDCODE,0(R4)                                                    
         LA    R6,COMLCNT                                                       
         ST    R6,ACCUMADR                                                      
         BRAS  RE,PRDCOMP                                                       
                                                                                
         SH    R5,=H'3'                                                         
         LTR   R5,R5                                                            
         BZ    PCML050                                                          
         LA    R4,3(R4)                                                         
         B     PCML080                                                          
                                                                                
PCMLXIT  B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        XSP NETWORK PATTERN RECORD                                             
*---------------------------------------------------------------------*         
PROCXNPT NTR1                                                                   
         LA    R4,WKKEY                                                         
         XC    WKKEY,WKKEY                                                      
         MVC   WKKEY(2),=X'0A61'   XSP NETWORK PATTERN(XSPDIR/XSPFIL)           
         MVC   WKKEY+2(1),BAGYMD   AGY/MEDIA                                    
         MVC   WKKEY+3(2),WKBINCLT CLIENT                                       
         MVC   WKKEYCMP,WKKEY      FOR COMPARISON                               
                                                                                
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',WKKEY,WKKEY                   
         B     PPTX060                                                          
                                                                                
PPTX050  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',WKKEY,WKKEY                   
PPTX060  CLC   WKKEY(5),WKKEYCMP                                                
         BNE   PPTXXIT                                                          
         OC    WKKEY+25(2),WKKEY+25                                             
         BNZ   PPTX050                                                          
                                                                                
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',WKKEY+36,AIO3                 
                                                                                
         USING NPTRECD,R5                                                       
         L     R5,AIO3                                                          
         LA    R6,PTRNCNT                                                       
         ST    R6,ACCUMADR                                                      
         OC    NPTXPRD,NPTXPRD                                                  
         BZ    *+14                                                             
         MVC   PRDCODE,NPTXPRD                                                  
         BRAS  RE,PRDCOMP                                                       
         OC    NPTXPRD2,NPTXPRD2                                                
         BZ    *+14                                                             
         MVC   PRDCODE,NPTXPRD2                                                 
         BRAS  RE,PRDCOMP                                                       
         B     PPTX050                                                          
                                                                                
PPTXXIT  B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        XSP REVISION RECORD                                                    
*---------------------------------------------------------------------*         
PROCXREV NTR1                                                                   
         LA    R4,WKKEY                                                         
         XC    WKKEY,WKKEY                                                      
         MVC   WKKEY(2),=X'0A1D'   XSP REVISION RECORD(XSPDIR/XSPFIL)           
         MVC   WKKEY+2(1),BAGYMD   AGY/MEDIA                                    
         MVC   WKKEY+3(2),WKBINCLT CLIENT                                       
         MVC   WKKEYCMP,WKKEY      FOR COMPARISON                               
                                                                                
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',WKKEY,WKKEY                   
         B     PRVX060                                                          
                                                                                
PRVX050  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',WKKEY,WKKEY                   
PRVX060  CLC   WKKEY(5),WKKEYCMP                                                
         BNE   PRVXXIT                                                          
                                                                                
*        GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',WKKEY+36,AIO3                 
                                                                                
         USING REVRECD,R5                                                       
         LA    R5,WKKEY                                                         
         OC    REVXKPRD,REVXKPRD                                                
         BZ    PRVX050                                                          
         LA    R6,REVNCNT                                                       
         ST    R6,ACCUMADR                                                      
         MVC   PRDCODE,REVXKPRD                                                 
         BRAS  RE,PRDCOMP                                                       
         B     PRVX050                                                          
                                                                                
PRVXXIT  B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
                                                                                
*---------------------------------------------------------------------*         
*        XSP COMMENT RECORD                                                     
*---------------------------------------------------------------------*         
PROCDT2  NTR1                                                                   
         LA    R4,WKKEY                                                         
         XC    WKKEY,WKKEY                                                      
         MVC   WKKEY(2),=X'0A2D'   XSP COMMENT RECORD (XSPDIR/XSPFIL)           
         MVC   WKKEY+2(1),BAGYMD   AGY/MEDIA                                    
         MVC   WKKEY+3(2),WKBINCLT CLIENT                                       
         MVC   WKKEYCMP,WKKEY      FOR COMPARISON                               
                                                                                
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',WKKEY,WKKEY                   
         B     PDT2060                                                          
                                                                                
PDT2050  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',WKKEY,WKKEY                   
PDT2060  CLC   WKKEY(5),WKKEYCMP                                                
         BNE   PDT2XIT                                                          
                                                                                
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',WKKEY+36,AIO3                 
                                                                                
         USING DT2RECD,R5                                                       
         L     R5,AIO3                                                          
         OC    DT2KPRD,DT2KPRD                                                  
         BZ    PDT2050                                                          
         LA    R6,CMNTCNT                                                       
         ST    R6,ACCUMADR                                                      
         MVC   PRDCODE,DT2KPRD                                                  
         BRAS  RE,PRDCOMP                                                       
         B     PDT2050                                                          
                                                                                
PDT2XIT  B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
                                                                                
*---------------------------------------------------------------------*         
*        NETWORK PRODUCT DISTRIBUTION LIST                                      
*---------------------------------------------------------------------*         
PROCPRDL NTR1                                                                   
         LA    R3,PRDCNT           8 PRODS MAX IN THE TABLE                     
         LA    R5,PRODTAB          POINT TO PRODUCT CODE TABLE                  
PPDL020  DS    0H                                                               
         OC    0(3,R5),0(R5)       EMPTY ENTRY                                  
         BZ    PDLTXIT             DONE                                         
         MVC   PRDCODE,0(R5)       STORE PROD CODE FOR THIS PASS                
                                                                                
PPDL030  LA    R4,WKKEY                                                         
         XC    WKKEY,WKKEY                                                      
         MVC   WKKEY(2),=X'0A42'   NET PRD DIST LIST (SPTDIR/SPTFIL)            
         MVC   WKKEY+2(1),BAGYMD   AGY/MEDIA                                    
         MVC   WKKEY+3(2),WKBINCLT CLIENT                                       
         MVC   WKKEY+5(3),PRDCODE  PRODUCT                                      
         MVC   WKKEYCMP,WKKEY      FOR COMPARISON                               
                                                                                
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',WKKEY,WKKEY                   
         B     PPDL060                                                          
                                                                                
PPDL050  GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'SPTDIR',WKKEY,WKKEY                   
PPDL060  CLC   WKKEY(8),WKKEYCMP                                                
         BNE   PPDL080                                                          
                                                                                
         LA    R6,PCONCNT                                                       
         ST    R6,ACCUMADR                                                      
         BRAS  RE,PRDCOMP                                                       
         B     PPDL050                                                          
                                                                                
PPDL080  LA    R5,L'PRD1(R5)                                                    
         BCT   R3,PPDL020                                                       
                                                                                
PDLTXIT  B     EXIT                                                             
         EJECT                                                                  
                                                                                
*---------------------------------------------------------------------*         
*        NETWORK PRODUCT DISTRIBUTION LIST                                      
*---------------------------------------------------------------------*         
PRDCOMP  NTR1                                                                   
         L     R6,ACCUMADR                                                      
         LA    R2,PRDCNT                                                        
         LA    R3,PRODTAB                                                       
PRDC010  CLC   PRDCODE,0(R3)                                                    
         BNE   PRDC015                                                          
         MVC   HALF,0(R6)                                                       
         SR    R5,R5                                                            
         LH    R5,HALF                                                          
         LA    R5,1(R5)                                                         
         STH   R5,HALF                                                          
         MVC   0(L'HALF,R6),HALF                                                
PRDC015  LA    R3,L'PRD1(R3)                                                    
         LA    R6,2(R6)                                                         
         BCT   R2,PRDC010                                                       
                                                                                
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------------------*         
*                                                                               
*---------------------------------------------------------------------*         
                                                                                
         GETEL R6,DATADISP,ELCODE                                               
                                                                                
NOCLTERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOCLTMS),NOCLTMS                                       
         GOTO1 ERREX2                                                           
NOCLTMS  DC    C'* ERROR * PGROUP REQUIRES CLIENT *'                            
                                                                                
                                                                                
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
                                                                                
MISSERR  MVI   ERROR,MISSING                                                    
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
                                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
*---------------------------------------------------------------*               
*                                                                               
*---------------------------------------------------------------*               
RECCOUNT DS    0H                                                               
COMLCNT  DC    8H'0'                                                            
RECTENT  EQU   *-RECCOUNT                                                       
PTRNCNT  DC    8H'0'                                                            
REVNCNT  DC    8H'0'                                                            
CMNTCNT  DC    8H'0'                                                            
PCONCNT  DC    8H'0'                                                            
*        DC    8H'0'              FOR FUTURE USE                                
*        DC    8H'0'              FOR FUTURE USE                                
*        DC    8H'0'              FOR FUTURE USE                                
RECTCNT  EQU   (*-RECCOUNT)/RECTENT                                             
RECTLEN  EQU   *-RECCOUNT                                                       
                                                                                
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,33,C'PRODUCT DELETION REPORT '                                
         SSPEC H2,33,C'------------------------'                                
         SSPEC H1,65,AGYNAME                                                    
         SSPEC H2,65,AGYADD                                                     
         SSPEC H4,3,C'CLIENT'                                                   
         SSPEC H4,78,RUN                                                        
         SSPEC H4,65,REPORT                                                     
         SSPEC H5,65,REQUESTOR                                                  
         SSPEC H5,95,PAGE                                                       
         SSPEC H8,3,C'PGROUP'                                                   
         SSPEC H9,3,C'------'                                                   
         SSPEC H8,46,C'NAMES'                                                   
         SSPEC H9,46,C'-----'                                                   
         SSPEC H8,79,C'NAMES'                                                   
         SSPEC H9,79,C'-----'                                                   
         DC    X'00'               END MARKER FOR SSPECS                        
         EJECT                                                                  
                                                                                
         PRINT OFF                                                              
       ++INCLUDE SPTRCMML                                                       
       ++INCLUDE SPTRNREV                                                       
       ++INCLUDE SPTRNPAT                                                       
       ++INCLUDE SPTRNCMT                                                       
       ++INCLUDE SPTRNPRD                                                       
       ++INCLUDE SPGENCLT                                                       
                                                                                
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
                                                                                
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
                                                                                
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE SPTRAFFD                                                       
                                                                                
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAA2D                                                       
         PRINT OFF                                                              
                                                                                
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
                                                                                
         PRINT ON                                                               
         EJECT                                                                  
*------------------------------------------------------------------*            
*        LOCAL WS                                                               
*------------------------------------------------------------------*            
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
         DS    0D                                                               
FLDH     DS    CL8                                                              
FLD      DS    CL64                                                             
SEQNUM   DS    XL1                                                              
SAVEKEY  DS    XL13                                                             
MYKEY    DS    XL13                                                             
NETWORK  DS    CL4                                                              
PRDCODE  DS    XL3                                                              
                                                                                
PRODTAB  DS    0CL24                                                            
PRD1     DS    CL3                                                              
PRD2     DS    CL3                                                              
PRD3     DS    CL3                                                              
PRD4     DS    CL3                                                              
PRD5     DS    CL3                                                              
PRD6     DS    CL3                                                              
PRD7     DS    CL3                                                              
PRD8     DS    CL3                                                              
PRDCNT   EQU   (*-PRODTAB)/L'PRD1                                               
PRDTLEN  EQU   *-PRODTAB                                                        
                                                                                
WKKEY    DS    XL48                                                             
WKKEYCMP DS    XL48                                                             
WKBINCLT DS    XL2                                                              
ACCUMADR DS    F                                                                
TEMPHALF DS    H                                                                
                                                                                
*        ONLINE LIST LINE                                                       
                                                                                
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LBLURB   DS    CL45                                                             
         DS    CL3                                                              
LCOUNT   DS    CL8                                                              
                                                                                
*        OFFLINE REPORT LINE                                                    
                                                                                
SPOOLD   DSECT                                                                  
         ORG   P                                                                
         DS    CL3                                                              
PPGROUP  DS    CL3                                                              
         DS    CL6                                                              
PPRODNM  DS    CL20                                                             
         DS    CL13                                                             
PNAME1   DS    CL30                                                             
         DS    CL3                                                              
PNAME2   DS    CL30                                                             
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'185SPTRA12   04/19/10'                                      
         END                                                                    
