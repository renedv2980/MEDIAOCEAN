*          DATA SET ACREPXK02  AT LEVEL 076 AS OF 07/17/14                      
*PHASE ACXK02A                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE PRINT                                                                  
         TITLE 'DELETE EXTRANEOUS BRANDO ELEMENTS'                              
ACXK03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXK**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACXKD,RC                                                         
*                                                                               
         CLI   MODE,COMPFRST                                                    
         JE    COMF                                                             
         CLI   MODE,LEDGFRST                                                    
         JE    LDGF                                                             
         CLI   MODE,PROCACC                                                     
         JE    PACC                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LEDGER FIRST                                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING CPYELD,R1                                                        
LDGF     DS    0H                                                               
         L     R1,ADCMPEL          GET X'10' ELEMENT COMPANY ELEM               
         MVC   SVCLOGO,CPYLOGO     GET LOGIN ID / CONNECT ID                    
         DROP  R1                                                               
*                                                                               
         BAS   RE,GETLEVS                                                       
         B     EXIT                                                             
**********************************************************************          
* ACCOUNT FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
COMF     DS    0H                                                               
         ZAP   DMPCNT,=P'0'                                                     
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         B     EXIT                                                             
**********************************************************************          
* PROCESS AN ACCOUNT                                                 *          
**********************************************************************          
         SPACE 1                                                                
         USING BIND,R1                                                          
PACC     LA    R1,GDATAB                                                        
         XC    BININ,BININ         CLEAR TABLE FOR EACH COMPANY                 
         DROP  R1                                                               
*                                                                               
         MVI   RCSUBPRG,1                                                       
         XC    SVGDAEL,SVGDAEL                                                  
         ZAP   ELMCNT,=P'0'                                                     
         MVI   FLAG,0                                                           
*                                                                               
         BAS   RE,SETCDE                                                        
*                                                                               
         USING ACTRECD,R2                                                       
         L     R2,ADACC                                                         
         LR    R3,R2                                                            
         AH    R3,DATADISP                                                      
         ST    R3,FULL             SAVE THE ADD OF THE START OF THE EL          
         CLC   ACTKACT,=C'NA0100331200'                                         
         BNE   *+8                                                              
         B     *+4                                                              
*                                                                               
PACC10   CLI   0(R3),0                                                          
         BE    PACC40                                                           
         CLI   0(R3),GDAELQ        X'E5' ELEMENT                                
         BE    PACC30                                                           
PACC20   SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     PACC10                                                           
*                                                                               
         USING GDAELD,R3                                                        
PACC30   CLI   GDATYPE,GDATMCST                                                 
         BNE   PACC20                                                           
         AP    ELMCNT,=P'1'                                                     
*                                                                               
* BUILD TABLE                                                                   
*                                                                               
         GOTO1 BINADD,DMCB,GDAELD,GDATAB   ADD TABLE ENTRY                      
*                                                                               
         B     PACC20                                                           
         DROP  R3                                                               
*                                                                               
PACC40   CP    ELMCNT,=P'0'        ANY ELEMENTS FOUND?                          
         BE    PACCX               NO - EXIT                                    
         TM    FLAG,FLGDUP         DID WE FIND DUPS?                            
         BO    *+14                                                             
         CP    ELMCNT,MAXELM       DID WE EXCEED THE MAX?                       
         BL    PACCX                                                            
*                                                                               
         MVC   MSG,=CL10'B4 REC  '                                              
         SR    RE,RE                                                            
         ICM   RE,3,ACTRLEN                                                     
         GOTO1 DUMP,DMCB,(R2),(RE)                                              
*                                                                               
         USING PLINED,R7                                                        
         LA    R7,P                                                             
*                                                                               
         MVC   PCLOGO,SVCLOGO      COMPANY LOGO                                 
         MVC   PACCT,ACTKACT       1R ACCOUNT CODE                              
         EDIT  ELMCNT,PELCNT                                                    
*                                                                               
         L     R3,FULL             RESTORE ADD OF THE START OF THE EL           
PACC50   CLI   0(R3),0                                                          
         BE    PACC90                                                           
         CLI   0(R3),NAMELQ        X'20' ELEMENT                                
         BE    PACC70                                                           
         CLI   0(R3),GDAELQ        X'E5' ELEMENT                                
         BE    PACC80                                                           
PACC60   SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     PACC50                                                           
*                                                                               
         USING NAMELD,R3                                                        
PACC70   LLC   RF,NAMLN                                                         
         SHI   RF,3                                                             
         MVC   PNAME(0),NAMEREC                                                 
         EX    RF,*-6                                                           
         B     PACC60                                                           
         DROP  R3                                                               
*                                                                               
         USING GDAELD,R3                                                        
PACC80   GOTO1 DATCON,DMCB,(1,GDADATE),(21,PBROST)                              
         GOTO1 DATCON,DMCB,(1,GDADATE2),(21,PBROET)                             
         GOTO1 ACREPORT                                                         
         MVI   GDAEL,DELELQ              INIT ELEMENT TO BE DELETED             
         B     PACC60                                                           
         DROP  R3                                                               
*                                                                               
PACC90   GOTO1 HELLO,DMCB,(C'D',ACCFIL),('DELELQ',(R2)),0,0                     
         GOTO1 ACREPORT                                                         
*                                                                               
         USING BIND,R5                                                          
         LA    R5,GDATAB                                                        
         ICM   R3,15,BININ         NUMBER IN TABLE                              
         BZ    PACC999                                                          
*                                                                               
         USING GDATABD,R6                                                       
         LA    R6,BINTAB           A(TABLE)                                     
*                                                                               
PACC100  DS    0H                                                               
         MVC   ELEM(GDALQ),0(R6)     RESTORE ELEMENTS TO RECORD                 
         GOTO1 HELLO,DMCB,(C'P',ACCFIL),ACTRECD,ELEM,0                          
*                                                                               
         LA    R6,GDALQ(R6)                                                     
         BCT   R3,PACC100                                                       
*                                                                               
PACC999  CLI   QOPT2,C'Y'          DO WE WANT TO UPDATE?                        
         BNE   PACCX                                                            
         CLI   RCWRITE,C'N'                                                     
         BE    PACCX                                                            
         MVI   MODE,WRITACC        WRITE BACK ACCOUNT RECORD                    
*                                                                               
PACCX    B     EXIT                                                             
         EJECT                                                                  
         DROP  R2,R5,R6,R7                                                      
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NTR1                                                                   
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         CP    DMPCNT,MAXDMP                                                    
         BH    EXIT                                                             
         AP    DMPCNT,=P'1'                                                     
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,0(R1)                                                         
         L     R4,4(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADD ITEM TO BINSRCH TABLE AND ACCUMULATE TOTALS                     *         
*  P1    A(ITEM TO BE ADDED)                                          *         
*  P2    A(TABLE)                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING BIND,R5                                                          
BINADD   NTR1                                                                   
         L     R5,4(R1)                                                         
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTAB           A(TABLE)                                     
         L     R3,0(R1)            A(ITEM)                                      
         GOTO1 BINSRCH,DMCB,(X'01',(R3)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1              RECORD WAS ADDED                             
         BE    BINXIT                                                           
         OI    FLAG,FLGDUP         MARK DUP FOUND                               
         L     R4,DMCB             A(RECORD FOUND)                              
         SR    R0,R0                                                            
         ICM   R0,1,BINNUM         NUMBER OF BUCKETS                            
         BZ    BINXIT                NO BUCKETS - EXIT                          
         DC    H'0'                NO BUCKETS                                   
*        SR    R6,R6                                                            
*        IC    R6,BINFST           DISPLACEMENT TO FIRST BUCKET                 
*        AR    R4,R6               BUMP TO FIRST BUCKET IN TABLE                
*        AR    R3,R6               BUMP TO FIRST BUCKET IN NEW ITEM             
*INA10   AP    0(L'NCPBKT,R4),0(L'NCPBKT,R3)   ADD TO BUCKET                    
*        LA    R3,L'NCPBKT(R3)     BUMP TO NEXT ENTRY IN NEW ITEM               
*        LA    R4,L'NCPBKT(R4)     BUMP TO NEXT ENTRY IN TABLE                  
*        BCT   R0,BINA10                                                        
*                                                                               
BINXIT   B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* GET HEIRARCHY LEVEL STRUCTURE                                      *          
**********************************************************************          
         SPACE 1                                                                
         USING ACLELD,R5                                                        
GETLEVS  NTR1                                                                   
         L     R5,ADLDGHIR         GET HEIRARCHY LEVELS                         
         MVC   LEVELS(LEVLNQ),SPACES                                            
         SR    R1,R1                                                            
         IC    R1,ACLLN            R1 = LENGTH OF ELEMENT                       
         SH    R1,=Y(ACLLN1Q+1)    SUBTRACT OVERHEAD                            
         MVC   LEVELS(0),ACLVALS   MVC LEVEL LENGTHS/NAMES INTO STORAGE         
         EX    R1,*-6                                                           
*                                                                               
* CONVERT 1,3,5,12 -> 1,2,2,7                                                   
*                                                                               
         LA    R0,LEVELQ           R0 = MAXIMUM NUMBER OF LEVELS                
         STC   R0,LEVNUM           ASSUME 4 LEVEL STRUCTURE                     
         LA    R1,LEVELS           R1 = FIRST LEVEL LENGTH                      
         LA    R2,LEVLNQS          R2 = FIRST LEVEL INDIVIDUAL LENGTH           
         SR    R3,R3               R3 = PREV LEVEL LENGTH (INITIALLY 0)         
         SR    R4,R4                                                            
GLEV10   ICM   R4,1,0(R1)          CURRENT LEVEL LENGTH                         
         BZ    GLEV20              NO MORE LEVELS - ADJUST LEVNUM               
         SR    R4,R3               SUBTRACT CURRENT FROM PREVIOUS               
         STC   R4,0(R2)            CURRENT INDIVIDUAL LENGTH                    
         IC    R3,0(R1)            UPDATE R3                                    
         LA    R1,L'ACLVALS(R1)                                                 
         LA    R2,1(R2)                                                         
         BCT   R0,GLEV10                                                        
         B     GLEVX                                                            
*                                                                               
GLEV20   LA    R1,LEVELQ           R1 = MAXIMUM NUMBER OF LEVELS                
         SR    R1,R0               R0 = NUM OF LEVELS OF MAX NOT USED           
         STC   R1,LEVNUM           LEVNUM = ACTUAL NUMBER OF LEVELS             
*                                                                               
GLEVX    B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* SET LEVEL CODES                                                    *          
**********************************************************************          
         SPACE 1                                                                
         USING ACTRECD,R5                                                       
SETCDE   NTR1                                                                   
         L     R5,ADACC                                                         
         MVC   LEVSCDE(LVCDLNQ),SPACES     CLEAR LEVEL CODES AREA               
         SR    R0,R0                                                            
         IC    R0,LEVNUM           NUMBER OF LEVELS/LOOPS                       
         LA    R1,ACTKACT          FULL ACCOUNT CODE                            
         LA    R2,LEVSCDE          FIRST LEVEL CODE                             
         LA    R3,LEVLNQS          FIRST LEVEL LENGTHS                          
*                                                                               
SETCDE10 SR    R4,R4                                                            
         IC    R4,0(R3)            CURRENT INDIVIDUAL LEV LENGTH                
         BCTR  R4,0                                                             
         MVC   0(0,R2),0(R1)                                                    
         EX    R4,*-6                                                           
         LA    R1,1(R4,R1)         BUMP TO NEXT LEVEL IN ACCOUNT CODE           
         LA    R2,L'LEVSCDE(R2)    BUMP TO NEXT LEVEL CODE AREA                 
         LA    R3,1(R3)            BUMP TO NEXT INDIVIUAL LEV LENGTH            
         BCT   R0,SETCDE10                                                      
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* GETEL                                                               *         
***********************************************************************         
         SPACE 1                                                                
         GETEL R2,DISP2,ELCODE                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
ACCFIL   DC    CL8'ACCFIL'                                                      
*                                                                               
MAXDMP   DC    PL4'20'                                                          
MAXELM   DC    PL4'2'                                                           
FIXRECS  DC    PL4'0'                                                           
FIXELS   DC    PL4'0'                                                           
DRNODB   DC    PL4'0'                                                           
*                                                                               
EOF      EQU   X'FF'                                                            
DELELQ   EQU   EOF                       DELETE ELEMENT FF                      
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMSEQDR  NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'08',DMRSEQ),=C'ACCDIR ',SVKEY,IOKEY,0            
         B     DMX                                                              
*                                                                               
DMHIGHDR NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'08',DMRDHI),=C'ACCDIR ',SVKEY,IOKEY,0            
         B     DMX                                                              
*                                                                               
DMREADDR NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCDIR ',SVKEY,IOKEY,0                    
         B     DMX                                                              
*                                                                               
DMWRTDR  NMOD1 0,WRT                                                            
         L     RC,0(R1)                                                         
         GOTO1 DATAMGR,DMCB,DMWRT,=C'ACCDIR ',0,IOKEY,0                         
         B     DMX                                                              
*                                                                               
DMGETREC NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         USING ACCRECD,R3                                                       
         LA    R3,IO                                                            
         MVC   SVDA,ACCKDA                                                      
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',SVDA,IO,DMWORK                    
         B     DMX                                                              
*                                                                               
DMPUTREC NMOD1 0,PREC              PUT RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',=C'ACCMST',SVDA,IO,DMWORK              
*                                                                               
DMX      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* TABLES                                                             *          
**********************************************************************          
         SPACE 1                                                                
         DC    C'**TABLE**'                                                     
GDATAB   DS    0D                                                               
         DC    F'0'                    NUMBER IN TABLE                          
         DC    AL4(GDALQ)              LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(GDAKLNQ)            KEY LENGTH                               
         DC    AL4(GDAMAX)             MAX IN TABLE                             
         DC    AL1(0)                  NUMBER OF BUCKETS - NO BUCKETS           
         DC    AL1(0)                  DISPLACEMENT TO FIRST BUCKET             
         DS    (GDAMAX*GDALQ)XL1       TABLE                                    
*                                                                               
GDAMAX   EQU   1000                                                             
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
         SPACE 1                                                                
ACXKD    DSECT                                                                  
SVDA     DS    F                                                                
DISP2    DS    H                                                                
MSG      DS    CL10                                                             
ELCODE   DS    CL1                                                              
ELEM     DS    CL255                                                            
SVKEY    DS    CL42                                                             
SVGDAEL  DS    CL9                                                              
MYKEY    DS    CL42                                                             
DMPCNT   DS    PL4                                                              
ELMCNT   DS    PL4                                                              
SVPCODE  DS    CL8                 PERSON CODE                                  
SVLSTD   DS    PL3                 LOCATION START DATE                          
SVPSTD   DS    PL3                 PERIOD START DATE                            
SVPRD    DS    XL1                 PERIOD NUMBER                                
SVODS    DS    0CL14               OFFICE/DEPT/SUB-DEPT                         
SVOFF    DS    CL2                                                              
SVDEPT   DS    CL6                                                              
SVSUB    DS    CL6                                                              
SVLDTES  DS    0PL6                SAVED START AND END DATES                    
SVLSDTE  DS    PL3                                                              
SVLEDTE  DS    PL3                                                              
SVCLOGO  DS    CL7                 COMPANY LOGO                                 
*                                                                               
FLAG     DS    XL1                                                              
FLGDUP   EQU   X'80'                                                            
*                                                                               
LEVELS   DS    0XL16                                                            
LEVA     DS    XL1                 LEVEL A LENGTH                               
LEVADSC  DS    CL15                LEVEL A NAME                                 
LEVB     DS    XL1                 LEVEL B LENGTH (A+B)                         
LEVBDSC  DS    CL15                LEVEL B NAME                                 
LEVC     DS    XL1                 LEVEL C LENGTH (A+B+C)                       
LEVCDSC  DS    CL15                LEVEL C NAME                                 
LEVD     DS    XL1                 LEVEL D LENGTH (A+B+C+D)                     
LEVDDSC  DS    CL15                LEVEL D NAME                                 
LEVLNQ   EQU   *-LEVELS                                                         
*                                                                               
LEVLNQS  DS    0XL4                                                             
LEVLNQA  DS    XL1                 LEVEL A INDIVIDUAL LENGTH                    
LEVLNQB  DS    XL1                 LEVEL B INDIVIDUAL LENGTH                    
LEVLNQC  DS    XL1                 LEVEL C INDIVIDUAL LENGTH                    
LEVLNQD  DS    XL1                 LEVEL D INDIVIDUAL LENGTH                    
*                                                                               
LEVNUM   DS    XL1                 NUMBER OF LEVELS IN HEIRARCHY                
LEVELQ   EQU   4                   MAXIMUM NUMBER OF LEVELS                     
*                                                                               
LEVSCDE  DS    0CL6                LEVEL CODES                                  
LEVACDE  DS    CL12                LEVEL A CODE                                 
LEVBCDE  DS    CL12                LEVEL B CODE                                 
LEVCCDE  DS    CL12                LEVEL C CODE                                 
LEVDCDE  DS    CL12                LEVEL D CODE                                 
LVCDLNQ  EQU   *-LEVSCDE                                                        
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                                                             
IOAREA   DS    CL2000                                                           
IOLNQ    EQU   *-IO                                                             
         EJECT                                                                  
***********************************************************************         
* DSECT FOR TABLE                                                     *         
***********************************************************************         
         SPACE 1                                                                
GDATABD  DSECT                                                                  
GDAELTB  DS    XL1                                                              
GDALNTB  DS    XL1                                                              
GDATYTB  DS    XL1                                                              
GDADT1TB DS    PL3                                                              
GDADT2TB DS    PL3                                                              
GDAKLNQ  EQU   *-GDATABD                                                        
GDALQ    EQU   *-GDATABD                                                        
         EJECT                                                                  
***********************************************************************         
* DSECT FOR BINSRCH PARAMETERS                                        *         
***********************************************************************         
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINKLN   EQU   *-BIND                                                           
BINNUM   DS    CL1                 NUMBER OF BUCKETS                            
BINFST   DS    CL1                 DISPLACEMENT TO FIRST BUCKET                 
BINTAB   DS    0C                  THE TABLE                                    
         EJECT                                                                  
***********************************************************************         
* DSECT FOR HEADLINES                                                 *         
***********************************************************************         
         SPACE 1                                                                
HEADD    DSECT                                                                  
         DS    CL1                                                              
HEADDESC DS    CL15                                                             
         DS    CL1                                                              
HEADCODE DS    CL6                 1R OFFICE                                    
         DS    CL1                                                              
HEADNAME DS    CL36                DECSRIPTION                                  
HEADLN   EQU   *-HEADD                                                          
         EJECT                                                                  
**********************************************************************          
* PRINT-LINE DSECT                                                   *          
**********************************************************************          
         SPACE 1                                                                
PLINED   DSECT                                                                  
         DS    CL1                                                              
PCLOGO   DS    CL7                 COMPANY LOGO                                 
         DS    CL10                                                             
PACCT    DS    CL12                1R ACCOUNT                                   
         DS    CL2                                                              
PNAME    DS    CL36                1R ACCOUNT NAME                              
         DS    CL2                                                              
PELCNT   DS    CL10                # OF ELEMENTS FOUND ON ACC                   
         DS    CL8                                                              
PBROST   DS    CL10                BRANDO START DATE                            
         DS    CL10                                                             
PBROET   DS    CL10                BRANDO END DATE                              
         DS    CL2                                                              
PLINELQ  EQU   *-PLINED                                                         
         EJECT                                                                  
**********************************************************************          
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'076ACREPXK02 07/17/14'                                      
         END                                                                    
