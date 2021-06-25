*          DATA SET PRSFM19    AT LEVEL 007 AS OF 06/15/15                      
*PHASE T41C19A                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* BPLA 05/11/15 ALLOW 8 ESTIMATE UCOMMS AND NO PRD UCOMMS                       
*               FOR H7'S AT&T CLIENTS                                           
*                                                                               
* KWAN 08/22/13 BYPASS DELETED RECORDS WHEN CHECKING CLIENT LEVEL UCOMM         
*                                                                               
* KWAN 06/08/01 NEW KEY DIVISION, REGION AND DISTRICT FIELDS                    
*                                                                               
* SMYE 11/03/00 ADDED CLIENT VALIDATION (SECURITY) TO DK                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  TITLE        T41C19 - UCOMM RECORDS MAINT/LIST                     *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41C00 (SFM PRINT CONTROLLER)              *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, DISPLAY, CHANGE, LIST                   *         
*                                                                     *         
*  INPUTS       SCREEN T41CD9 (MAINTENANCE)                           *         
*               SCREEN T41CC9 (LIST)                                  *         
*                                                                     *         
*  OUTPUTS      UPDATED UCOMM RECORDS                                 *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - GETEL REGISTER/WORK                                   *         
*          R7 - SECOND BASE                                           *         
*          R8 - SPOOLD/WORK                                           *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM/WORK                                           *         
*          RF - SYSTEM/WORK                                           *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
         TITLE 'T41C19  UCOMM RECORDS'                                          
T41C19   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C19,R7         *** NOTE R7 AS SECOND BASE                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
*                                                                               
         OI    GENSTAT4,NODELLST   DISALLOW DELETES FROM LIST                   
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,RECDEL         DELETE RECORD                                
         BE    RDEL                                                             
         CLI   MODE,RECREST        RESTORE RECORD                               
         BE    RRES                                                             
*                                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   EXIT                                                             
*                                                                               
         LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        VALIDATE KEY ROUTINE                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VK       DS    0H                                                               
*                                                                               
         MVI   RECTYPE,C' '        CLEAR RECORD TYPE INDICATOR                  
         BAS   RE,CLRNAME                                                       
         XC    KEY,KEY                                                          
*                                                                               
         LA    R2,COMMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         CLI   MEDNM,0             MEANS MEDIA NOT FOUND                        
         BE    VKDERR                                                           
         OI    4(R2),X'20'         SET VALIDATED                                
         FOUT  COMMEDNH,MEDNM,10                                                
         MVC   SVMED,QMED                                                       
*                                                                               
VK1      XC    QCLT,QCLT                                                        
         XC    MYKEY,MYKEY                                                      
         LA    R6,MYKEY                                                         
         USING UCOMRECD,R6                                                      
         MVC   PUCMKAGY,AGENCY                                                  
         MVC   PUCMKMED,QMED                                                    
         MVI   PUCMKRCD,X'12'      UCOMM RECORD CODE                            
*                                                                               
         LA    R2,COMCLIH          CLIENT                                       
         CLI   5(R2),0                                                          
         BNE   VK2                                                              
         CLI   ACTNUM,ACTLIST      NO CLT  - OK IF LIST                         
         BE    VK3                                                              
*                                                                               
VKMISS   MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VK2      DS    0H                                                               
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   VK2D                                                             
         MVC   CLTNM(11),=C'ALL CLIENTS'   "ALL" ENTERED AS CLIENT CODE         
         FOUT  COMCLINH,CLTNM,11                                                
         CLI   ACTNUM,ACTLIST               OK IF LIST                          
         BE    VK3                                                              
         MVI   ERROR,INVCLI                 ERROR IF NOT                        
         B     TRAPERR                                                          
*                                                                               
VK2D     GOTO1 VALICLT                                                          
         CLI   CLTNM,0              MEANS CLIENT NOT FOUND                      
         BE    VKDERR                                                           
         FOUT  COMCLINH,CLTNM,20                                                
         OC    QCLT,SPACES                                                      
         MVC   PUCMKCLT,QCLT                                                    
         MVC   SVCLT,QCLT                                                       
         OI    4(R2),X'20'         SET VALIDATED                                
         CLI   SVCPROF+5,C'1'      MASTER CLIENT?                               
         BNE   VK3                 NO                                           
         MVI   ERROR,INVCLI                                                     
         B     TRAPERR                                                          
*                                                                               
VK3      DS    0H                                                               
         XC    QPRD,QPRD                                                        
         LA    R2,COMPROH            PRODUCT                                    
         CLI   5(R2),0                                                          
         BNE   VK3E                                                             
*                                                                               
         MVI   RECTYPE,C'X'        ONLY FIELD NAME ENTRY ALLOWED                
*                                                                               
         CLI   ACTNUM,ACTLIST       NO PRD - OK IF LIST                         
         BE    VK4                                                              
*                                                                               
VK3E     DS    0H                                                               
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   VK3K                                                             
         MVC   PRDNM(12),=C'ALL PRODUCTS'  "ALL" ENTERED AS PROD CODE           
         FOUT  COMPRONH,PRDNM,12                                                
         CLI   ACTNUM,ACTLIST               OK IF LIST                          
         BE    VK4                                                              
         MVI   ERROR,INVPRD                 ERROR IF NOT                        
         B     TRAPERR                                                          
*                                                                               
VK3K     DS    0H                                                               
         CLI   5(R2),0                                                          
         BE    VK4                 NO PRD ENTERED                               
*                                                                               
VK3P     GOTO1 VALIPRD                                                          
         CLI   PRDNM,0              MEANS PRODUCT NOT FOUND                     
         BE    VKDERR                                                           
         FOUT  COMPRONH,PRDNM,20                                                
         OC    QPRD,SPACES                                                      
         MVC   PUCMKPRD,QPRD                   PRODUCT                          
         MVC   SVPRD,QPRD                                                       
         OI    4(R2),X'20'         SET VALIDATED                                
*                                                                               
         MVI   RECTYPE,C'P'       "MAYBE" UCOM PROD REC (SEE EST BELOW)         
*                                                                               
VK4      DS    0H                                                               
         XC    QEST,QEST                                                        
         XC    BEST,BEST                                                        
         LA    R2,COMESTH            ESTIMATE                                   
         CLI   5(R2),0                                                          
         BNE   VK4E                                                             
         B     VK5                  NO EST                                      
*                                                                               
VK4E     DS    0H                                                               
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   VK4K                                                             
         MVC   ESTNM(13),=C'ALL ESTIMATES'  "ALL" ENTERED AS ESTIMATE           
         FOUT  COMESTNH,ESTNM,13                                                
         CLI   ACTNUM,ACTLIST               OK IF LIST                          
         BE    VK5                                                              
         MVI   ERROR,INVEST                 ERROR IF NOT                        
         B     TRAPERR                                                          
*                                                                               
VK4K     DS    0H                                                               
         OC    QPRD,QPRD           ANY PRODUCT ?                                
         BNZ   VK4P                YES                                          
         LA    R2,COMPROH          NO - SEND PRODUCT MISSING ERROR              
         B     VKMISS                                                           
VK4P     GOTO1 VALIEST                                                          
         CLI   ESTNM,0              MEANS ESTIMATE NOT FOUND                    
         BE    VKDERR                                                           
*                                                                               
         FOUT  COMESTNH,ESTNM,20                                                
         MVC   PUCMKEST,BEST       ESTIMATE                                     
         MVC   SVEST,BEST                                                       
         OI    4(R2),X'20'         SET VALIDATED                                
         MVI   RECTYPE,C'E'        UCOM ESTIMATE RECORD                         
*                                                                               
         DROP  R6                                                               
*                                                                               
VK5      DS    0H                                                               
         XC    SVQDIV,SVQDIV                                                    
         LA    R2,COMDIVH          DIVISION                                     
         CLI   5(R2),0             ANYTHING IN DIVISION?                        
         BE    VK6                                                              
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   VK5H                                                             
         CLC   =C'ALL',8(R2)       ALL DIVISION?                                
         BE    VK6                                                              
         TM    4(R2),X'08'         VALID NUMBERIC?                              
         BZ    INVERR                                                           
         LA    RF,SVQDIV                                                        
         BAS   RE,MVQDRD           MOVE PROPER 3 BYTES DIV/REG/DST              
         B     VK6                                                              
*                                                                               
VK5H     GOTO1 VALIDIV                                                          
         CLI   DIVNM,0             MEANS DIVISION NOT FOUND                     
         BE    VKDERR                                                           
         FOUT  COMDIVNH,DIVNM,20                                                
         OC    QDIV,SPACES                                                      
         LA    R2,COMREGH                                                       
         CLI   5(R2),0             IF DIV IS ENTERED, REG IS REQUIRED           
         BE    VKMISS                                                           
         LA    R2,COMESTH                                                       
         CLI   5(R2),0             IF DIV IS ENTERED, EST IS REQUIRED           
         BE    VKMISS                                                           
         LA    R2,COMDIVH          POINT TO DIV FIELD HEADER AGAIN              
*                                                                               
A        USING PUCMKEY,MYKEY                                                    
         MVC   A.PUCMKDIV,QDIV                                                  
*                                                                               
VK6      DS    0H                                                               
         XC    SVQREG,SVQREG                                                    
         LA    R2,COMREGH          REGION                                       
         CLI   5(R2),0             ANYTHING IN REGION?                          
         BE    VK7                                                              
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   VK6H                                                             
         CLC   =C'ALL',8(R2)       ALL DIVISION?                                
         BE    VK7                                                              
         TM    4(R2),X'08'         VALID NUMBERIC?                              
         BZ    INVERR                                                           
         LA    RF,SVQREG                                                        
         BAS   RE,MVQDRD           MOVE PROPER 3 BYTES DIV/REG/DST              
         B     VK7                                                              
*                                                                               
VK6H     LA    R2,COMDIVH          DIV IS REQUIRED FOR REGION INPUT             
         CLI   5(R2),0                                                          
         BE    VKMISS                                                           
         LA    R2,COMREGH                                                       
         GOTO1 VALIREG                                                          
         CLI   REGNM,0             MEANS REGION NOT FOUND                       
         BE    VKDERR                                                           
         FOUT  COMREGNH,REGNM,20                                                
         OC    QREG,SPACES                                                      
         MVC   A.PUCMKREG,QREG                                                  
         MVI   RECTYPE,C'R'        UCOM REGION RECORD                           
*                                                                               
VK7      DS    0H                                                               
         XC    SVQDST,SVQDST                                                    
         LA    R2,COMDSTH          DISTRICT                                     
         CLI   5(R2),0             ANYTHING IN DISTRICT?                        
         BE    VK800                                                            
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   VK7H                                                             
         CLC   =C'ALL',8(R2)       ALL DIVISION?                                
         BE    VK800                                                            
         TM    4(R2),X'08'         VALID NUMBERIC?                              
         BZ    INVERR                                                           
         LA    RF,SVQDST                                                        
         BAS   RE,MVQDRD           MOVE PROPER 3 BYTES DIV/REG/DST              
         B     VK800                                                            
*                                                                               
VK7H     LA    R2,COMREGH          REG IS REQUIRED FOR DISTRICT INPUT           
         CLI   5(R2),0                                                          
         BE    VKMISS                                                           
         LA    R2,COMDSTH                                                       
         GOTO1 VALIDST                                                          
         CLI   DSTNM,0             MEANS DISTRICT NOT FOUND                     
         BE    VKDERR                                                           
         FOUT  COMDSTNH,DSTNM,20                                                
         OC    QDST,SPACES                                                      
         MVC   A.PUCMKDST,QDST                                                  
         MVI   RECTYPE,C'D'        UCOM DISTRICT RECORD                         
         DROP  A                                                                
*                                                                               
VK800    CLI   ACTNUM,ACTLIST                                                   
         BE    VK810               IF LIST, DONE                                
*                                                                               
         BAS   RE,SWTCHLUP         SET FIELD PROTECTS/UNPROTECTS                
*                                                                               
VK810    DS    0H                                                               
         XC    KEY,KEY                                                          
         CLI   ACTNUM,ACTLIST                                                   
         BNE   VK820                                                            
         XC    SVCLTKEY,SVCLTKEY                                                
         B     EXIT                IF LIST, DONE                                
VK820    MVC   KEY(25),MYKEY       SET KEY                                      
*                                                                               
VK900    DS    0H                                                               
         CLI   RECTYPE,C'X'        CLT LEVEL ONLY ?                             
         BNE   VK920                                                            
         MVC   SVRECTYP,RECTYPE    YES                                          
         B     EXIT                                                             
VK920    DS    0H                  MUST BE PRD OR EST LEVEL                     
         CLI   ACTNUM,ACTADD       ADD ?                                        
         BE    VKCKREC             YES - CHECK IF HAVE CLIENT LEVEL             
         MVC   SVRECTYP,RECTYPE                                                 
         B     EXIT                                                             
*                                                                               
VKCKREC  DS    0H                                                               
         MVC   SAVEKEY,KEY         SEE IF HAVE CLIENT LEVEL RECORD              
         XC    KEY+7(18),KEY+7     CLEAR ALL AFTER CLT CODE                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'PUCMKEY),KEYSAVE    CLT UCOM REC FOUND ?                   
         BE    VKCKR10             YES                                          
         MVI   ERROR,NOCLTLEV      MUST HAVE CLIENT LEVEL RECORD                
         B     TRAPERR                                                          
*                                                                               
VKCKR10  DS    0H                  GET THE CLIENT LEVEL RECORD                  
         MVC   AIO1,AIO                                                         
         MVC   AIO,AIO2                                                         
*                                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
         L     R6,AIO2             POINT R6 TO UCOM CLIENT REC                  
         MVC   AIO,AIO1            RESTORE AIO AND KEY                          
         MVC   KEY(L'PUCMKEY),SAVEKEY                                           
*                                                                               
         CLI   RECTYPE,C'P'        PRODUCT LEVEL?                               
         BNE   *+16                                                             
         MVI   ERROR,NOCLTPRD      PRD LEVEL NOT CREATED ERROR                  
         MVI   ELCODE,X'41'        PRD LEVEL ELEM CODE                          
         B     VKCKR15                                                          
         CLI   RECTYPE,C'E'        ESTIMATE LEVEL?                              
         BNE   *+16                                                             
         MVI   ERROR,NOCLTEST      EST LEVEL NOT CREATED ERROR                  
         MVI   ELCODE,X'51'        EST LEVEL ELEM CODE                          
         B     VKCKR15                                                          
         CLI   RECTYPE,C'R'        REGION LEVEL?                                
         BNE   *+16                                                             
         MVI   ERROR,NOCLTREG      REG LEVEL NOT CREATED ERROR                  
         MVI   ELCODE,X'61'        REG LEVEL ELEM CODE                          
         B     VKCKR15                                                          
         CLI   RECTYPE,C'D'        DISTRICT LEVEL?                              
         BNE   *+16                                                             
         MVI   ERROR,NOCLTDST      DST LEVEL NOT CREATED ERROR                  
         MVI   ELCODE,X'71'        DST LEVEL ELEM CODE                          
         B     VKCKR15                                                          
*                                                                               
         DC    H'0'                NO OTHER DEFINED TYPE                        
*                                                                               
VKCKR15  BAS   RE,GETEL                                                         
         BNE   TRAPERR             NOT FOUND, ERROR MSG ALREADY SET             
*                                                                               
         L     R5,AIO2                                                          
         CLC   SVCLTKEY(L'PUCMKEY),0(R5)      DID WE ALREADY DISPLAY            
         BNE   VKCKR30             NO                                           
         CLC   SVRECTYP,RECTYPE    DID LEVEL CHANGE?                            
         BE    EXIT                NO                                           
*                                                                               
* LEVEL CHANGED OR CLIENT LEVEL NOT DISPLAYED                                   
*                                                                               
VKCKR30  MVC   SVCLTKEY(L'PUCMKEY),0(R5)                                        
         MVC   SVRECTYP,RECTYPE                                                 
         BAS   RE,DCLT             DISPLAY CLIENT LEVEL DATA                    
         BAS   RE,SWTCHLUP         SET FIELD PROTECTS/UNPROTECTS                
         LA    R0,COMLN1DH                                                      
         ST    R0,ACURFORC         FORCE CURSOR TO "FIELD DATA"                 
         B     EXIT                                                             
*                                                                               
VKDERR   MVI   ERROR,NOTFOUND                                                   
         B     TRAPERR                                                          
*                                                                               
MVQDRD   MVC   0(3,RF),=C'000'     INITIALIZE SAVE STORAGES TO ZEROS            
         CLI   5(R2),1                                                          
         BNE   *+14                                                             
         MVC   2(1,RF),8(R2)       RIGHT ALIGNED, ZERO PADDED                   
         B     MVQDRDX                                                          
*                                                                               
         CLI   5(R2),2                                                          
         BNE   *+14                                                             
         MVC   1(2,RF),8(R2)       RIGHT ALIGNED, ZERO PADDED                   
         B     MVQDRDX                                                          
*                                                                               
         CLI   5(R2),3                                                          
         BE    *+6                                                              
         DC    H'0'                NO OTHER INPUT LENGHT!                       
         MVC   0(3,RF),8(R2)       ALL 3 INPUT DIGITS ARE MOVED                 
*                                                                               
MVQDRDX  CLC   0(3,RF),=C'000'     ALL ZEROS AFTER MOVE?                        
         BNE   *+10                                                             
         XC    0(3,RF),0(RF)                                                    
         BR    RE                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        VALIDATE RECORD                                                        
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VR       DS    0H                                                               
*                                                                               
         CLI   RECTYPE,C'X'        CLIENT LEVEL ?                               
         BNE   VROTHER             NO - PRD/EST/REG/DST                         
*                                                                               
         L     R5,AIO                                                           
         USING UCOMRECD,R5                                                      
         MVC   PUCMREC(L'PUCMKEY),KEY       WILL BE REBUILDING REC              
         DROP  R5                                                               
*                                                                               
*NOP*    MVI   ACTELOPT,C'N'       SET FOR NO ACTIVITY ELEMENT                  
*                                                                               
         MVI   ATTSW,C'N'                                                       
         CLC   AGENCY,=C'SJ'       SJR FOR TESTING                              
         BE    VR0                                                              
         CLC   AGENCY,=C'H7'       GROUP M                                      
         BNE   VR0X                                                             
VR0      CLC   SVCLT(2),=C'AT'     ATT AND ONLY ATT CLTS                        
         BNE   VR0X                WILL BEGIN WITH AT                           
         MVI   ATTSW,C'Y'                                                       
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADD, SKIP COUNT BELOW                     
         BE    VR3                                                              
*                                                                               
* COUNT PRD & EST ELEMS IN UCOM CLT                                             
*                                                                               
         SR    R3,R3               FOR PRD/EST/REG/DST COUNTS                   
         L     R5,AIO              POINT TO CLIENT LEVEL RECORD                 
         LA    R5,33(R5)           FIRST ELEMENT                                
VR0X     MVI   RPRDCNT,0           PRD COUNT                                    
         MVI   RESTCNT,0           EST COUNT                                    
         MVI   RREGCNT,0           REG COUNT                                    
         MVI   RDSTCNT,0           DST COUNT                                    
*                                                                               
VR1      CLI   0(R5),0             END OF RECORD ?                              
         BE    VR1END              YES - STORE COUNTS                           
         CLI   0(R5),X'49'         PRODUCT ELEM ?                               
         BH    VR1E                NO                                           
         ZIC   R3,RPRDCNT                                                       
         AHI   R3,1                ADD TO PRD COUNT                             
         STC   R3,RPRDCNT                                                       
         B     VR1NXT                                                           
VR1E     CLI   0(R5),X'59'         ESTIMATE ELEM ?                              
         BH    VR1F                NO                                           
         ZIC   R3,RESTCNT                                                       
         AHI   R3,1                ADD TO EST COUNT                             
         STC   R3,RESTCNT                                                       
         B     VR1NXT                                                           
VR1F     CLI   0(R5),X'69'         REGION ELEM ?                                
         BH    VR1G                NO                                           
         ZIC   R3,RREGCNT                                                       
         AHI   R3,1                ADD TO REG COUNT                             
         STC   R3,RREGCNT                                                       
         B     VR1NXT                                                           
VR1G     CLI   0(R5),X'79'         DISTRICT ELEM ?                              
         BH    VR1NXT              NO - MUST BE ACTIVITY ELEM                   
         ZIC   R3,RDSTCNT                                                       
         AHI   R3,1                ADD TO DST COUNT                             
         STC   R3,RDSTCNT                                                       
VR1NXT   ZIC   R0,1(R5)                                                         
         AR    R5,R0               POINT TO NEXT ELEM                           
         B     VR1                 LOOK FOR MORE ELEMENTS                       
VR1END   DS    0H                                                               
         CLI   ATTSW,C'Y'          ATT CLIENT?                                  
         BNE   VR1END4                                                          
         CLI   RPRDCNT,4           4 IS MAX, SOMETHING WRONG WITH REC!          
         BNH   *+6                                                              
         DC    H'0'                0 IS MAX, SOMETHING WRONG WITH REC!          
         CLI   RESTCNT,8                                                        
         BNH   VR1END4             GO CHECK REG AND DST                         
         DC    H'0'                8 IS MAX, SOMETHING WRONG WITH REC!          
*                                                                               
VR1END2  CLI   RPRDCNT,4                                                        
         BNH   *+6                                                              
         DC    H'0'                4 IS MAX, SOMETHING WRONG WITH REC!          
         CLI   RESTCNT,4                                                        
         BNH   *+6                                                              
         DC    H'0'                4 IS MAX, SOMETHING WRONG WITH REC!          
VR1END4  CLI   RREGCNT,2                                                        
         BNH   *+6                                                              
         DC    H'0'                2 IS MAX, SOMETHING WRONG WITH REC!          
         CLI   RDSTCNT,2                                                        
         BNH   *+6                                                              
         DC    H'0'                2 IS MAX, SOMETHING WRONG WITH REC!          
*                                                                               
VR3      DS    0H                                                               
         MVI   SPRDCNT,0           ZERO SCREEN EST & PRD COUNTERS               
         MVI   SESTCNT,0                                                        
         MVI   SREGCNT,0           ZERO SCREEN REG & DST COUNTERS               
         MVI   SDSTCNT,0                                                        
*                                                                               
* DELETE ALL ELEMENTS EXCEPT ACTIVITY                                           
*                                                                               
         L     R5,AIO                                                           
         LA    R5,33(R5)           FIRST ELEMENT                                
VR3CLR   CLI   0(R5),0             END OF RECORD?                               
         BE    VR3CLRX             YES - DELETE ALL X'FF' ELEMENTS              
         CLI   0(R5),X'7E'         PRD OR EST ELEMENT ?                         
         BH    *+8                 NO - MUST BE ACTIVITY ELEMENT                
         MVI   0(R5),X'FF'         SET ELEMENT CODE FOR DELETION BELOW          
         ZIC   R0,1(R5)                                                         
         AR    R5,R0               BUMP TO NEXT ELEMENT                         
         B     VR3CLR              TEST NEXT                                    
VR3CLRX  MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM             DELETE ALL ELEMENTS EXCEPT ACTIVITY          
*                                                                               
VR5      LA    R4,12               SET # OF ROWS ON SCREEN FOR BCT              
         LA    R2,COMLN1TH                                                      
         MVI   PRDELC,X'41'        SET DEFAULT VALUES                           
         MVI   ESTELC,X'51'                                                     
         MVI   REGELC,X'61'                                                     
         MVI   DSTELC,X'71'                                                     
*                                                                               
VR10     DS    0H                                                               
         CLI   5(R2),0             IS TYPE FIELD EMPTY ?                        
         BNE   VR20                                                             
         BAS   RE,CHKROW           YES, WHOLE ROW MUST BE EMPTY                 
*                                                                               
* IF RETURNS, ROW IS EMPTY, R2 POINTS TO FID NAME, SKIP TO NEXT ROW             
*                                                                               
         B     VRNEXT                                                           
*                                                                               
VR20     CLI   8(R2),C'P'          TYPE P?                                      
         BNE   VR21                                                             
*                                                                               
         ZIC   R0,SPRDCNT          GET SCREEN PRD COUNT                         
         AHI   R0,1                ADD TO IT                                    
         STC   R0,SPRDCNT          SAVE SCREEN PRD COUNT                        
         CHI   R0,4                                                             
         BH    MAXUCERR            MORE THAN 4 PRD LEVEL ENTRIES                
         MVC   ELCODE,PRDELC       PUT ELCODE FOR PRODUCT                       
         ZIC   R0,PRDELC                                                        
         AHI   R0,1                GET NXT AVAILABLE ELCODE FOR PRD             
         STC   R0,PRDELC                                                        
         B     VR40                                                             
*                                                                               
VR21     CLI   8(R2),C'E'          TYPE E?                                      
         BNE   VR22                                                             
         ZIC   R0,SESTCNT          GET SCREEN EST COUNT                         
         AHI   R0,1                ADD TO IT                                    
         STC   R0,SESTCNT          SAVE SCREEN EST COUNT                        
*                                                                               
         CLI   ATTSW,C'Y'          ATT CLIENT?                                  
         BNE   VR214                                                            
         CHI   R0,8                                                             
         BH    MAXUCERR            MORE THAN 8 EST LEVEL ENTRIES                
         B     VR21X                                                            
*                                                                               
VR214    CHI   R0,4                                                             
         BH    MAXUCERR            MORE THAN 4 EST LEVEL ENTRIES                
VR21X    MVC   ELCODE,ESTELC       PUT ELCODE FOR EST                           
         ZIC   R0,ESTELC                                                        
         AHI   R0,1                                                             
         STC   R0,ESTELC                                                        
         B     VR40                                                             
*                                                                               
VR22     CLI   8(R2),C'R'          TYPE R?                                      
         BNE   VR23                                                             
         ZIC   R0,SREGCNT          GET SCREEN REG COUNT                         
         AHI   R0,1                ADD TO IT                                    
         STC   R0,SREGCNT          SAVE SCREEN REG COUNT                        
         CHI   R0,2                                                             
         BH    MAXUCERR            MORE THAN 2 REG LEVEL ENTRIES                
         MVC   ELCODE,REGELC       PUT ELCODE FOR REG                           
         ZIC   R0,REGELC                                                        
         AHI   R0,1                                                             
         STC   R0,REGELC                                                        
         B     VR40                                                             
*                                                                               
VR23     CLI   8(R2),C'D'          TYPE D?                                      
         BNE   INVERR              IF NOT TYPE P/E/R/D, ERROR                   
         ZIC   R0,SDSTCNT          GET SCREEN DST COUNT                         
         AHI   R0,1                ADD TO IT                                    
         STC   R0,SDSTCNT          SAVE SCREEN DST COUNT                        
         CHI   R0,2                                                             
         BH    MAXUCERR            MORE THAN 2 DST LEVEL ENTRIES                
         MVC   ELCODE,DSTELC       PUT ELCODE FOR DST                           
         ZIC   R0,DSTELC                                                        
         AHI   R0,1                                                             
         STC   R0,DSTELC                                                        
*                                                                               
VR40     DS    0H                                                               
         XC    ELEM,ELEM                                                        
         USING PUCMELEM,R3         REBUILD NEW                                  
         LA    R3,ELEM                                                          
         MVC   PUCMELEM,ELCODE                                                  
*                                                                               
         ZIC   R0,0(R2)            NEXT FIELD (EDT)                             
         AR    R2,R0                                                            
         CLI   8(R2),C' '          FREE FORM ?                                  
         BNH   VR40EDTH            YES                                          
         CLI   8(R2),C'C'          CHARACTER ?                                  
         BE    VR40EDT                                                          
         CLI   8(R2),C'D'          DATE ?                                       
         BE    VR40EDT                                                          
         CLI   8(R2),C'N'          NUMERIC ?                                    
         BE    VR40EDT                                                          
         B     INVERR              IF NOT, ERROR                                
*                                                                               
VR40EDT  MVC   PUCMEDIT,8(R2)                                                   
VR40EDTH ZIC   R0,0(R2)            NEXT FIELD (LEN)                             
         AR    R2,R0                                                            
         CLI   5(R2),0             MISSING ?                                    
         BE    VKMISS              YES                                          
         TM    4(R2),X'08'         NUMERIC?                                     
         BO    VR42                YES                                          
         B     INVERR              IF NOT, ERROR                                
*                                                                               
VR42     ZIC   R1,5(R2)              CHK THE LENGTH LESS THAN 32                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         CHI   R1,32                                                            
         BH    INVERR              INVALID - LEN GT 32                          
         CHI   R1,1                                                             
         BL    INVERR              INVALID - LEN LT 1                           
         CLI   PUCMEDIT,C'D'       (D)ATE TYP ?                                 
         BNE   VR42X               NO                                           
         CHI   R1,8                                                             
         BL    DLNERR              LENGTH NOT 8 OR MORE FOR TYP (D)ATE          
VR42X    STC   R1,PUCMLEN                                                       
*                                                                               
         ZIC   R0,0(R2)            NEXT FIELD (MBI)                             
         AR    R2,R0                                                            
         CLI   5(R2),0             ANYTHING IN MBI ?                            
         BE    VR50                IT'S OK                                      
         CLI   8(R2),C'Y'                                                       
         BNE   INVERR              INVALID - NOT 'Y' OR BLANK                   
         OI    PUCMUSE1,X'08'                                                   
*                                                                               
VR50     DS    0H                  NEXT FIELD (FIELD NAME)                      
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ZIC   R1,5(R2)                                                         
         LTR   R1,R1               MINIMUM LENGTH OF 1 ?                        
         BZ    VKMISS              NO - MISSING ERROR                           
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PUCMDT(0),8(R2)     PUT FIELD NAME IN ELEM                       
         AHI   R1,7                6-OVERHEAD, 1 FOR EXECUTE                    
         STC   R1,PUCMELEN         ELEM LENGTH (VARIABLE 6+DATA)                
*                                     ** NOTE BELOW **                          
         OI    PUCMUSE1,X'70'      SET FOR POSSIBLE FUTURE USE                  
*                                  ONLY IN CLIENT REC FOR NOW                   
*                                     ** NOTE ABOVE **                          
         GOTO1 ADDELEM                                                          
         DROP  R3                                                               
*                                                                               
VRNEXT   ZIC   R0,0(R2)            GO TO NEXT ROW                               
         AR    R2,R0               FIELD DATA                                   
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               R2 POINTING TO TYP                           
         BCT   R4,VR10                                                          
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VRX                                                              
*                                                                               
*   COMPARE SCREEN "TYPES" TO COUNTS FROM RECORD. IF FEWER ON SCREEN,           
*   CHECK FOR PRD AND/OR EST RECS - IF NONE, ALLOW CHANGE, ELSE ERROR           
*                                                                               
VRCOUNT  DS    0H                                                               
         CLC   RPRDCNT,SPRDCNT                                                  
         BH    VRCREAD             FEWER PRD TYPES ON SCREEN                    
         CLC   RESTCNT,SESTCNT                                                  
         BH    VRCREAD             FEWER EST TYPES ON SCREEN                    
         CLC   RREGCNT,SREGCNT                                                  
         BH    VRCREAD             FEWER REG TYPES ON SCREEN                    
         CLC   RDSTCNT,SDSTCNT                                                  
         BNH   VRX                 OKAY                                         
VRCREAD  DS    0H                                                               
         MVC   SAVEKEY,KEY                                                      
         XC    KEY+7(18),KEY+7     CLEAR ALL AFTER CLT CODE                     
         GOTO1 HIGH                                                             
         CLC   KEY(L'PUCMKEY),KEYSAVE                                           
         BE    *+6                 CLIENT UCOM REC FOUND ?                      
         DC    H'0'                NO - DIE                                     
         GOTO1 SEQ                 NEXT RECORD                                  
         CLC   SAVEKEY(7),KEY      COMPARE THRU CLIENT CODE                     
         BNE   VRCOK               OK - NO OTHER REC'S FOR CLIENT               
*                                                                               
VRCWRONG DS    0H                  SEND "CANNOT REDUCE TYPES" MESSAGE           
         OI    COMMEDH+6,X'81'     SET MODIFIED (FORCE REVALIDATION)            
         LA    R2,CONACTH                                                       
         B     ALTERERR            NO CHG OR REMOVE TYP ERROR                   
*                                                                               
VRCOK    DS    0H                  RESTORE SEQUENCE                             
         MVC   KEY(L'PUCMKEY),SAVEKEY                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'PUCMKEY),SAVEKEY       SAME RECORD?                        
         BE    VRX                 YES - OK                                     
         DC    H'0'                SOMETHING WRONG                              
*                                                                               
* * * * * * * * * * * * * * * * * ** * * * ** * * * * * * * * * * * * *         
*                                                                               
VROTHER  DS    0H                  VALIDATA FIELD DATA (TYPE=P/E/R/D)           
         BAS   RE,GETLVLC          GET CLT LEVEL REC INTO AIO2                  
*                                                                               
         L     R5,AIO                                                           
         USING UCOMRECD,R5                                                      
         MVC   PUCMREC(L'PUCMKEY),KEY       WILL BE REBUILDING REC              
         DROP  R5                                                               
*                                                                               
* DELETE ALL ELEMENTS EXCEPT ACTIVITY                                           
*                                                                               
         L     R5,AIO                                                           
         LA    R5,33(R5)           FIRST ELEMENT                                
VROCLR   CLI   0(R5),0             END OF RECORD?                               
         BE    VROCLRX             YES - DELETE ALL X'FF' ELEMENTS              
         CLI   0(R5),X'7E'         PRD/EST/REG/DST ELEMENT?                     
         BH    *+8                 NO - MUST BE ACTIVITY ELEMENT                
         MVI   0(R5),X'FF'         SET ELEMENT CODE FOR DELETION BELOW          
         ZIC   R0,1(R5)                                                         
         AR    R5,R0               BUMP TO NEXT ELEMENT                         
         B     VROCLR              TEST NEXT                                    
VROCLRX  MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM             DELETE ALL ELEMENTS EXCEPT ACTIVITY          
*                                                                               
         LA    R2,COMLN1TH                                                      
         MVI   PRDELC,X'41'        SET DEFAULT VALUES                           
         MVI   ESTELC,X'51'                                                     
         MVI   REGELC,X'61'                                                     
         MVI   DSTELC,X'71'                                                     
*                                                                               
         CLI   COMLN1T,C' '        ANYTHING IN TYP?                             
         BNH   VRX                 NO - DONE                                    
*                                                                               
         LA    R2,COMLN1TH                                                      
         CLC   RECTYPE,8(R2)       DO WE WANT THIS ROW?                         
         BNE   VRO20               NO - NOT SAME "LEVEL" - NEXT ROW             
         BAS   RE,GETELCOD         GET ELEM CODE TO ACCESS CLT LVL REC          
         CLC   COMLN1D,SPACES      ANYTHING IN FIELD DATA?                      
         BNH   VRO20               NO - NEXT ROW                                
         LA    R2,COMLN1DH                                                      
         BAS   RE,VROADDEL         GO EDIT DATA AND ADD ELEM IF OK              
*                                                                               
VRO20    DS    0H                                                               
         CLI   COMLN2T,C' '        ANYTHING IN TYP ?                            
         BNH   VRX                 NO - DONE                                    
         LA    R2,COMLN2TH                                                      
         CLC   RECTYPE,8(R2)       DO WE WANT THIS ROW ?                        
         BNE   VRO30               NO - NOT SAME "LEVEL" - NEXT ROW             
         BAS   RE,GETELCOD         GET ELEM CODE TO ACCESS CLT LVL REC          
         CLC   COMLN2D,SPACES      ANYTHING IN FIELD DATA ?                     
         BNH   VRO30               NO - NEXT ROW                                
         LA    R2,COMLN2DH                                                      
         BAS   RE,VROADDEL         GO EDIT DATA AND ADD ELEM IF OK              
*                                                                               
VRO30    DS    0H                                                               
         CLI   COMLN3T,C' '                                                     
         BNH   VRX                                                              
         LA    R2,COMLN3TH         SAME FOR ALL 8 ROWS FOLLOWS                  
         CLC   RECTYPE,8(R2)                                                    
         BNE   VRO40                                                            
         BAS   RE,GETELCOD                                                      
         CLC   COMLN3D,SPACES                                                   
         BNH   VRO40                                                            
         LA    R2,COMLN3DH                                                      
         BAS   RE,VROADDEL                                                      
*                                                                               
VRO40    DS    0H                                                               
         CLI   COMLN4T,C' '                                                     
         BNH   VRX                                                              
         LA    R2,COMLN4TH                                                      
         CLC   RECTYPE,8(R2)                                                    
         BNE   VRO50                                                            
         BAS   RE,GETELCOD                                                      
         CLC   COMLN4D,SPACES                                                   
         BNH   VRO50                                                            
         LA    R2,COMLN4DH                                                      
         BAS   RE,VROADDEL                                                      
*                                                                               
VRO50    DS    0H                                                               
         CLI   COMLN5T,C' '                                                     
         BNH   VRX                                                              
         LA    R2,COMLN5TH                                                      
         CLC   RECTYPE,8(R2)                                                    
         BNE   VRO60                                                            
         BAS   RE,GETELCOD                                                      
         CLC   COMLN5D,SPACES                                                   
         BNH   VRO60                                                            
         LA    R2,COMLN5DH                                                      
         BAS   RE,VROADDEL                                                      
*                                                                               
VRO60    DS    0H                                                               
         CLI   COMLN6T,C' '                                                     
         BNH   VRX                                                              
         LA    R2,COMLN6TH                                                      
         CLC   RECTYPE,8(R2)                                                    
         BNE   VRO70                                                            
         BAS   RE,GETELCOD                                                      
         CLC   COMLN6D,SPACES                                                   
         BNH   VRO70                                                            
         LA    R2,COMLN6DH                                                      
         BAS   RE,VROADDEL                                                      
*                                                                               
VRO70    DS    0H                                                               
         CLI   COMLN7T,C' '                                                     
         BNH   VRX                                                              
         LA    R2,COMLN7TH                                                      
         CLC   RECTYPE,8(R2)                                                    
         BNE   VRO80                                                            
         BAS   RE,GETELCOD                                                      
         CLC   COMLN7D,SPACES                                                   
         BNH   VRO80                                                            
         LA    R2,COMLN7DH                                                      
         BAS   RE,VROADDEL                                                      
*                                                                               
VRO80    DS    0H                                                               
         CLI   COMLN8T,C' '                                                     
         BNH   VRX                                                              
         LA    R2,COMLN8TH                                                      
         CLC   RECTYPE,8(R2)                                                    
         BNE   VRX                                                              
         BAS   RE,GETELCOD                                                      
         CLC   COMLN8D,SPACES                                                   
         BNH   VRX                                                              
         LA    R2,COMLN8DH                                                      
         BAS   RE,VROADDEL                                                      
*                                                                               
VRO90    DS    0H                                                               
         CLI   COMLN9T,C' '                                                     
         BNH   VRX                                                              
         LA    R2,COMLN9TH                                                      
         CLC   RECTYPE,8(R2)                                                    
         BNE   VRX                                                              
         BAS   RE,GETELCOD                                                      
         CLC   COMLN9D,SPACES                                                   
         BNH   VRX                                                              
         LA    R2,COMLN9DH                                                      
         BAS   RE,VROADDEL                                                      
*                                                                               
VRO100   DS    0H                                                               
         CLI   COMLNAT,C' '                                                     
         BNH   VRX                                                              
         LA    R2,COMLNATH                                                      
         CLC   RECTYPE,8(R2)                                                    
         BNE   VRX                                                              
         BAS   RE,GETELCOD                                                      
         CLC   COMLNAD,SPACES                                                   
         BNH   VRX                                                              
         LA    R2,COMLNADH                                                      
         BAS   RE,VROADDEL                                                      
*                                                                               
VRO110   DS    0H                                                               
         CLI   COMLNBT,C' '                                                     
         BNH   VRX                                                              
         LA    R2,COMLNBTH                                                      
         CLC   RECTYPE,8(R2)                                                    
         BNE   VRX                                                              
         BAS   RE,GETELCOD                                                      
         CLC   COMLNBD,SPACES                                                   
         BNH   VRX                                                              
         LA    R2,COMLNBDH                                                      
         BAS   RE,VROADDEL                                                      
*                                                                               
VRO120   DS    0H                                                               
         CLI   COMLNCT,C' '                                                     
         BNH   VRX                                                              
         LA    R2,COMLNCTH                                                      
         CLC   RECTYPE,8(R2)                                                    
         BNE   VRX                                                              
         BAS   RE,GETELCOD                                                      
         CLC   COMLNCD,SPACES                                                   
         BNH   VRX                                                              
         LA    R2,COMLNCDH                                                      
         BAS   RE,VROADDEL                                                      
*                                                                               
         B     VRX                                                              
*                                                                               
**********************************************************************          
*                                                                               
*        R2 POINTING TO HEADER OF TYP FIELD                                     
*                                                                               
**********************************************************************          
*                                                                               
GETELCOD NTR1                                                                   
         CLI   8(R2),C'P'                                                       
         BNE   GETLC20                                                          
         MVC   ELCODE,PRDELC       PUT ELCODE FOR PRODUCT                       
         ZIC   R0,PRDELC                                                        
         AHI   R0,1                GET NXT AVAILABLE ELCODE FOR PRD             
         STC   R0,PRDELC                                                        
         B     EXIT                                                             
*                                                                               
GETLC20  CLI   8(R2),C'E'          TYPE E?                                      
         BNE   GETLC30                                                          
         MVC   ELCODE,ESTELC       PUT ELCODE FOR ESTIMATE                      
         ZIC   R0,ESTELC                                                        
         AHI   R0,1                                                             
         STC   R0,ESTELC                                                        
         B     EXIT                                                             
*                                                                               
GETLC30  CLI   8(R2),C'R'          TYPE R?                                      
         BNE   GETLC40                                                          
         MVC   ELCODE,REGELC       PUT ELCODE FOR REGION                        
         ZIC   R0,REGELC                                                        
         AHI   R0,1                                                             
         STC   R0,REGELC                                                        
         B     EXIT                                                             
*                                                                               
GETLC40  CLI   8(R2),C'D'          TYPE D?                                      
         BE    GETLC50                                                          
         DC    H'0'                SHOULD NOT HAPPEN                            
*                                                                               
GETLC50  DS    0H                                                               
         MVC   ELCODE,DSTELC       PUT ELCODE FOR DISTRICT                      
         ZIC   R0,DSTELC                                                        
         AHI   R0,1                                                             
         STC   R0,DSTELC                                                        
         B     EXIT                                                             
*                                                                               
**********************************************************************          
*                                                                               
*        R2 POINTING TO HEADER OF FIELD DATA TO BE EDITED                       
*                                                                               
**********************************************************************          
*                                                                               
VROADDEL NTR1                                                                   
         XC    ELEM,ELEM                                                        
         MVC   ELEM(1),ELCODE                                                   
*                                                                               
         L     R6,AIO2             POINT TO CLT LEVEL REC                       
         BAS   RE,GETEL            GET THE NEEDED "EDIT" ELEMENT                
         BE    *+6                                                              
         DC    H'0'                MUST BE THERE                                
         USING PUCMELEM,R6                                                      
*                                                                               
         CLC   5(1,R2),PUCMLEN     CHECK IF L'INPUT IS VALID                    
         BH    LONGERR             INPUT TOO LONG ERROR                         
*                                                                               
VRUSR5   CLI   PUCMEDIT,C' '       ACCEPT ANY INPUT                             
         BNH   VRUSR30                                                          
         CLI   PUCMEDIT,C'N'       IF TYPE IS NUMERIC                           
         BNE   VRUSR10                                                          
         TM    4(R2),X'08'         INPUT MUST BE NUMERIC                        
         BO    VRUSR30                                                          
         ZIC   R1,5(R2)            BUT ALLOW '- /'                              
         LA    R5,8(R2)                                                         
*                                                                               
VRUSR7   CLI   0(R5),C'0'                                                       
         BL    VRUSR8                                                           
         CLI   0(R5),C'9'                                                       
         BNH   VRUSR9                                                           
*                                                                               
VRUSR8   CLI   0(R5),C' '                                                       
         BE    VRUSR9                                                           
         CLI   0(R5),C'/'                                                       
         BE    VRUSR9                                                           
         CLI   0(R5),C'-'                                                       
         BNE   NUMBERR             NOT NUMERIC ERROR                            
*                                                                               
VRUSR9   LA    R5,1(R5)                                                         
         BCT   R1,VRUSR7                                                        
         B     VRUSR30                                                          
*                                                                               
VRUSR10  CLI   PUCMEDIT,C'C'       IF TYPE IS ALPHABETIC                        
         BNE   VRUSR20                                                          
         ZIC   R1,5(R2)            ALLOW ALL INPUT EXCEPT NUMBERS               
         LA    R5,8(R2)                                                         
*                                                                               
VRUSR15  CLI   0(R5),C'0'                                                       
         BL    VRUSR17                                                          
         CLI   0(R5),C'9'                                                       
         BNH   ALPHERR             NOT ALL ALPHABETIC                           
*                                                                               
VRUSR17  LA    R5,1(R5)                                                         
         BCT   R1,VRUSR15                                                       
         B     VRUSR30                                                          
*                                                                               
VRUSR20  CLI   PUCMEDIT,C'D'       IF TYPE DATE                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    DTERR                                                            
         L     R5,0(R1)            L'DATE                                       
         ZIC   R1,5(R2)            L'INPUT                                      
         SR    R1,R5                                                            
         BNZ   INVERR                                                           
*                                                                               
VRUSR30  ZIC   R1,5(R2)            R1=L(INPUT)                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+6(0),8(R2)     PUT FIELD DATA IN ELEM                       
         AHI   R1,7                6-OVERHEAD, 1 FOR EXECUTE                    
         STC   R1,ELEM+1           ELEM LENGTH (VARIABLE 6+DATA)                
*                                                                               
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
         B     EXIT                DONE WITH THIS ELEM                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VRX      DS    0H                                                               
         CLI   RECTYPE,C'X'                                                     
         BNE   EXIT                GO TO DR ONLY ON CLT LEVEL REC               
         B     DR                  GOTO DISP REC LOGIC                          
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* ROUTINE TO CHK IF WHOLE ROW IS EMPTY                                          
* IF PARTIALLY EMPTY, EXIT WITH ERROR                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CHKROW   DS    0H                                                               
         ST    R2,SAVER2           POINTING TO TYP                              
         ST    RE,SAVERE           FOR RETURN                                   
         LA    RE,4                TEST EDT,LEN,MBI,NAME FIELDS                 
CHKTOP   ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   5(R2),0             MISSING?                                     
         BE    CHKBACK             YES - GOOD                                   
         L     R2,SAVER2           POINT R2 BACK TO TYP                         
         B     VKMISS              MISSING ERROR                                
CHKBACK  BCT   RE,CHKTOP                                                        
         L     RE,SAVERE           ROW ALL EMPTY - GOOD - SO RETURN             
         BR    RE                  WITH R2 POINTING TO FIELD NAME               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        ERROR OUTPUTS FOLLOW                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DTERR    MVI   ERROR,INVDATE       INVALID DATE                                 
         B     TRAPERR                                                          
*                                                                               
DLNERR   MVI   ERROR,INVDTLN       LENGTH NOT 8 OR MORE FOR TYP (D)ATE          
         B     TRAPERR                                                          
*                                                                               
INVERR   MVI   ERROR,INVALID       INVALID INPUT                                
         B     TRAPERR                                                          
*                                                                               
LONGERR  MVI   ERROR,BADLNTH       INPUT IS TOO LONG                            
         B     TRAPERR                                                          
*                                                                               
NUMBERR  MVI   ERROR,NOTNUM        INPUT NOT NUMERIC                            
         B     TRAPERR                                                          
*                                                                               
ALPHERR  MVI   ERROR,NOTALPHA      INPUT NOT ALPHABETIC                         
         B     TRAPERR                                                          
*                                                                               
ALTERERR MVI   ERROR,NOCHGTYP      CANNOT CHANGE OR REMOVE TYP                  
         B     TRAPERR                                                          
*                                                                               
MAXUCERR MVI   ERROR,MAXUCOMM      MAX UCOMM FIELDS REACHED                     
         LA    R2,CONACTH                                                       
         B     TRAPERR                                                          
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        DISPLAY RECORD                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DR       DS    0H                                                               
*                                                                               
         BAS   RE,CLRLUP           CLEAR ALL NON-KEY ENTRY FIELDS               
*                                                                               
         L     R5,AIO                                                           
         CLI   RECTYPE,C'X'        UCOM CLIENT RECORD ?                         
         BE    DREC                YES - RECORD IN AIO                          
*                                  NO - MUST GET UCOM CLIENT INTO AIO2          
         MVC   SAVEKEY,KEY                                                      
         MVC   SAVEKEYS,KEYSAVE                                                 
         XC    KEY+7(18),KEY+7     CLEAR ALL AFTER CLT CODE                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'PUCMKEY),KEYSAVE                                           
         BE    *+6                 CLIENT UCOM REC FOUND ?                      
         DC    H'0'                NO - DIE                                     
*                                                                               
         MVC   AIO1,AIO                                                         
         MVC   AIO,AIO2                                                         
*                                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
         L     R5,AIO2             POINT R5 TO UCOM CLIENT REC                  
         XC    KEY,KEY                                                          
         MVC   KEY(L'PUCMKEY),SAVEKEY     RESTORE KEY                           
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING, NO SEQ RESTORE                    
         BE    DREC                                                             
         GOTO1 HIGH                RESTORE KEY SEQ                              
         CLC   KEY(L'PUCMKEY),KEYSAVE                                           
         BE    *+6                 REC FOUND ?                                  
         DC    H'0'                NO - DIE                                     
*                                                                               
         MVC   AIO,AIO3                                                         
*                                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
         MVC   AIO,AIO1            RESTORE PRD/EST UCOM REC ADDRESS             
*                                                                               
DREC     DS    0H                  R5 POINTS TO UCOM CLIENT RECORD              
         LA    R5,33(R5)           1ST UCOM CLIENT RECORD ELEMENT               
         USING PUCMELEM,R5         R5 POINTING TO UCOM CLT REC ELEM             
         LA    R2,COMLN1TH         1ST SCREEN "DATA" FIELD                      
*                                                                               
DRSEL    DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    DREXIT              DONE                                         
         CLI   RECTYPE,C'X'        CLIENT ALONE ?                               
         BE    DRKEEP              YES                                          
         CLI   0(R5),X'4A'         PRODUCT ELEM?                                
         BH    DRSEL10             NO - SHOULD BE ESTIMATE                      
         CLI   RECTYPE,C'P'        DO WE WANT PRODUCT?                          
         BE    DRKEEP              YES                                          
         B     DRNEXT              SKIP THIS ELEMENT                            
DRSEL10  DS    0H                                                               
         CLI   0(R5),X'5A'         ESTIMATE ELEM?                               
         BH    DRSEL20             NO - SKIP IT                                 
         CLI   RECTYPE,C'E'        DO WE WANT ESTIMATE?                         
         BE    DRKEEP              YES                                          
         B     DRNEXT              SKIP THIS ELEMENT                            
DRSEL20  DS    0H                                                               
         CLI   0(R5),X'6A'         REGION ELEM?                                 
         BH    DRSEL30             NO - SKIP IT                                 
         CLI   RECTYPE,C'R'        DO WE WANT REGION?                           
         BE    DRKEEP              YES                                          
         B     DRNEXT              SKIP THIS ELEMENT                            
DRSEL30  DS    0H                                                               
         CLI   0(R5),X'7A'         DISTRICT ELEM?                               
         BH    DRNEXT              NO - SKIP IT                                 
         CLI   RECTYPE,C'D'        DO WE WANT DISTRICT?                         
         BE    DRKEEP              YES                                          
DRNEXT   DS    0H                  NEXT ELEMENT                                 
         ZIC   RE,1(R5)                                                         
         AR    R5,RE                                                            
         B     DRSEL                                                            
*                                                                               
DRKEEP   DS    0H                  R2 USED AS SCREEN POINTER                    
         CLI   PUCMELEM,X'7A'      ACTIVITY ELEMENT ?                           
         BH    DRNEXT              YES - SKIP IT                                
*                                                                               
         CLI   PUCMELEM,X'4A'      PRODUCT ELEM?                                
         BH    *+12                                                             
         MVI   8(R2),C'P'          "TYP" FIELD                                  
         B     DRKP10                                                           
         CLI   PUCMELEM,X'5A'      ESTIMATE ELEM?                               
         BH    *+12                                                             
         MVI   8(R2),C'E'          "TYP" FIELD                                  
         B     DRKP10                                                           
         CLI   PUCMELEM,X'6A'      REGION ELEM?                                 
         BH    *+12                                                             
         MVI   8(R2),C'R'          "TYP" FIELD                                  
         B     DRKP10                                                           
         CLI   PUCMELEM,X'7A'      DISTRICT ELEM?                               
         BH    *+12                                                             
         MVI   8(R2),C'D'          "TYP" FIELD                                  
         B     DRKP10                                                           
*                                                                               
         DC    H'0'                NO OTHER TPYES AT THIS TIME!                 
*                                                                               
DRKP10   BAS   RE,DRKXMIT          SEND "TYP" & POINT TO "EDT"                  
         MVC   8(1,R2),PUCMEDIT    "EDT" FIELD                                  
         BAS   RE,DRKXMIT          SEND "EDT" & POINT TO "LEN"                  
         EDIT  (1,PUCMLEN),(2,8(R2)),ALIGN=LEFT        "LEN" FIELD              
         BAS   RE,DRKXMIT          SEND "LEN" & POINT TO "MBI"                  
         MVI   8(R2),C'Y'          "MBI" FIELD                                  
         TM    PUCMUSE1,X'08'      MBI = Y ?                                    
         BNZ   *+8                 YES                                          
         MVI   8(R2),C' '          NO - CLEAR                                   
         BAS   RE,DRKXMIT          SEND "MBI" & POINT TO "FIELD NAME"           
         ZIC   RE,PUCMELEN         ELEMENT LENGTH                               
         AHI   RE,-7               ELEM OVERHEAD + 1 FOR EX MOVE                
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),PUCMDT      "FIELD NAME"                                 
         BAS   RE,DRKXMIT          SEND "NAME" & POINT TO "FIELD DATA"          
         MVC   8(L'COMLN1D,R2),SPACES        CLEAR "DATA"                       
         CLI   RECTYPE,C'X'        CLIENT ONLY ?                                
         BNE   DRKMORE             NO                                           
*                                                                               
DRKNXT   BAS   RE,DRKXMIT          SEND "DATA" & POINT TO NEXT ROW              
         B     DRNEXT              DONE WITH THIS ELEMENT                       
*                                                                               
DRKMORE  DS    0H                  CHECK FOR PRODUCT OR EST ELEM                
         L     R6,AIO              POINT R6 TO PRD OR EST UCOM REC              
         MVC   ELCODE,PUCMELEM     UCOM CLIENT REC ELEM CODE                    
         BAS   RE,GETEL            LOOK FOR UCOM PRD/EST REC ELEM               
         BNE   DRKNXT              NOT FOUND - FINISH ROW AND ELEM              
         ZIC   RE,1(R6)            ELEMENT LENGTH                               
         AHI   RE,-7               ELEM OVERHEAD + 1 FOR EX MOVE                
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),6(R6)       MOVE TO "FIELD DATA"                         
         B     DRKNXT              FINISH ROW AND ELEM                          
*                                                                               
         DROP  R5                                                               
*                                                                               
DRKXMIT  DS    0H                                                               
         OI    6(R2),X'80'         TRANSMIT FIELD                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               R2 POINTING TO NEXT SCREEN FIELD             
         BR    RE                                                               
*                                                                               
DREXIT   DS    0H                                                               
         BAS   RE,SWTCHLUP         SET FIELD PROTECTS, ETC.                     
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
******************************************************************              
*  DISPLAY CLIENT LEVEL REC - R5 POINT TO CLT REC                               
******************************************************************              
*                                                                               
DCLT     NTR1                      R5 -> TO UCOM CLT REC                        
*                                                                               
         BAS   RE,CLRLUP           CLEAR NON KEY FIELDS                         
*                                                                               
         LA    R2,COMLN1TH         1ST SCREEN DATA FIELD                        
         LA    R5,33(R5)           1ST UCOM CLT RECORD ELEM                     
         USING PUCMELEM,R5         R5 --> TO UCOM CLT REC ELEM                  
*                                                                               
DCLT10   DS    0H                                                               
         CLI   0(R5),0                                                          
         BE    DCLTX               DONE                                         
*                                                                               
         CLI   0(R5),X'4A'         PRODUCT ELEM ?                               
         BH    DCLT20              NO, MUST BE EST                              
         CLI   RECTYPE,C'P'        DO WE WANT PRODUCT                           
         BE    DCLT60                                                           
         B     DCLT50                                                           
*                                                                               
DCLT20   CLI   0(R5),X'5A'         ESTIMATE ELEM ?                              
         BH    DCLT30              NO - SKIP                                    
         CLI   RECTYPE,C'E'        DO WE WANT ESTIMATE                          
         BE    DCLT60                                                           
         B     DCLT50                                                           
*                                                                               
DCLT30   CLI   0(R5),X'6A'         REGION ELEM ?                                
         BH    DCLT40              NO - SKIP                                    
         CLI   RECTYPE,C'R'        DO WE WANT REGION                            
         BE    DCLT60                                                           
         B     DCLT50                                                           
*                                                                               
DCLT40   CLI   0(R5),X'7A'         DISTRICT ELEM ?                              
         BH    DCLT50              NO - SKIP                                    
         CLI   RECTYPE,C'D'        DO WE WANT DISTRICT                          
         BE    DCLT60                                                           
*                                                                               
DCLT50   ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     DCLT10                                                           
*                                                                               
DCLT60   DS    0H                                                               
         CLI   PUCMELEM,X'4A'      PRODUCT ELEM?                                
         BH    *+12                                                             
         MVI   8(R2),C'P'          "TYP" FIELD                                  
         B     DCLT70                                                           
         CLI   PUCMELEM,X'5A'      ESTIMATE ELEM?                               
         BH    *+12                                                             
         MVI   8(R2),C'E'          "TYP" FIELD                                  
         B     DCLT70                                                           
         CLI   PUCMELEM,X'6A'      REGION ELEM?                                 
         BH    *+12                                                             
         MVI   8(R2),C'R'          "TYP" FIELD                                  
         B     DCLT70                                                           
         CLI   PUCMELEM,X'7A'      DISTRICT ELEM?                               
         BH    *+12                                                             
         MVI   8(R2),C'D'          "TYP" FIELD                                  
         B     DCLT70                                                           
*                                                                               
         DC    H'0'                NO OTHER TPYES AT THIS TIME!                 
*                                                                               
DCLT70   BAS   RE,DRKXMIT          SEND 'TYP' & -> TO 'EDT'                     
*                                                                               
         MVC   8(1,R2),PUCMEDIT                                                 
         BAS   RE,DRKXMIT          SEND 'EDT' GET 'LEN'                         
*                                                                               
         EDIT  (1,PUCMLEN),(2,8(R2)),ALIGN=LEFT                                 
         BAS   RE,DRKXMIT          SEND 'LEN' GET 'MBI'                         
*                                                                               
         MVI   8(R2),C'Y'                                                       
         TM    PUCMUSE1,X'08'      MBI = Y ?                                    
         BO    *+8                 YES                                          
         MVI   8(R2),C' '          NO,CLEAR                                     
         BAS   RE,DRKXMIT          SEND 'MBI', POINT TO 'FIELD NAME'            
*                                                                               
         ZIC   RE,PUCMELEN         ELEMENT LENGTH                               
         AHI   RE,-7               ELEM OVERHEAD + 1 FOR EX MOVE                
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),PUCMDT      "FIELD NAME"                                 
         BAS   RE,DRKXMIT          SEND "NAME" & POINT TO "FIELD DATA"          
         MVC   8(L'COMLN1D,R2),SPACES        CLEAR "DATA"                       
*                                                                               
         BAS   RE,DRKXMIT                                                       
         B     DCLT50                                                           
*                                                                               
DCLTX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        DELETE RECORD                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RDEL     DS    0H                                                               
*                                                                               
*NOP*    MVI   ACTELOPT,C'N'       SET FOR NO ACTIVITY ELEMENT                  
*                                                                               
         CLI   RECTYPE,C'X'        UCOM CLT LEVEL REC ?                         
         BNE   EXIT                NO - OK TO DELETE                            
*                                  CANNOT DELETE CLT LEVEL REC IF ANY           
*                                  OTHER LEVEL RECS EXIST FOR CLIENT            
         MVC   SAVEKEY,KEY                                                      
         XC    KEY+7(18),KEY+7     CLEAR ALL AFTER CLT CODE                     
         MVI   RDUPDATE,C'N'                                                    
         MVC   SVDMINBT,DMINBTS                                                 
         NI    DMINBTS,X'F7'       DO NOT PASS DELETES                          
         GOTO1 HIGH                                                             
         CLC   KEY(L'PUCMKEY),KEYSAVE                                           
         BE    *+6                 CLIENT UCOM REC FOUND ?                      
         DC    H'0'                NO - DIE                                     
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                 NEXT RECORD                                  
         CLC   SAVEKEY(7),KEY      COMPARE THRU CLIENT CODE                     
         BNE   RDEOK               OK - NO OTHER REC'S FOR CLIENT               
*                                                                               
         LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT        CANNOT DELETE - INVALID ACTION               
         B     TRAPERR                                                          
*                                                                               
RDEOK    DS    0H                  RESTORE SEQUENCE                             
         MVC   DMINBTS,SVDMINBT                                                 
         MVC   KEY(L'PUCMKEY),SAVEKEY                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'PUCMKEY),SAVEKEY       SAME RECORD ?                       
         BE    EXIT                YES - OK                                     
         DC    H'0'                SOMETHING WRONG                              
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        RESTORE RECORD           ???????                                       
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
RRES     DS    0H                                                               
*                                                                               
*NOP*    MVI   ACTELOPT,C'N'       SET FOR NO ACTIVITY ELEMENT                  
*                                                                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        DISPLAY KEY                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DK       DS    0H                                                               
*                                                                               
         BAS   RE,CLRNAME                                                       
         MVI   RECTYPE,C' '        CLEAR RECORD TYPE INDICATOR                  
*                                                                               
         MVC   AIO1,AIO                                                         
         MVC   SAVEKEY,KEY         SAVE KEYS FOR DISPREC                        
         MVC   SAVEKEYS,KEYSAVE    SAVE KEYS FOR DISPREC                        
         L     R6,AIO                                                           
         USING UCOMRECD,R6                                                      
*                                                                               
         CLI   PUCMKPRD,C' '       CLIENT ONLY ?                                
         BH    DK10                NO                                           
         MVI   RECTYPE,C'X'        CLIENT UCOM RECORD                           
*                                                                               
DK10     DS    0H                                                               
         FOUT  COMMEDH,PUCMKMED,1                                               
         FOUT  COMCLIH,PUCMKCLT,3                                               
*                                                                               
         LA    R2,COMMEDH                                                       
         MVI   5(R2),1                                                          
         GOTO1 VALIMED             NEEDED FOR SECURITY                          
*                                                                               
         LA    R2,COMCLIH                                                       
         MVI   5(R2),3                                                          
         GOTO1 VALICLT             NEEDED FOR SECURITY                          
*                                                                               
K        USING PUCMKEY,SAVEKEY     DISPLAY DIV/REG/DST KEY FIELDS               
*                                                                               
         OC    K.PUCMKDIV,K.PUCMKDIV                                            
         BZ    DK15                                                             
         LA    R2,COMDIVH                                                       
         MVI   5(R2),3             FAKE INPUT LENGTH TO THREE                   
         MVC   8(3,R2),K.PUCMKDIV                                               
         OI    6(R2),X'80'         TRANSMIT DIVISION CODE                       
         GOTO1 VALIDIV                                                          
         FOUT  COMDIVNH,DIVNM,20   TRANSMIT DIVISION NAME                       
*                                                                               
         OC    K.PUCMKREG,K.PUCMKREG                                            
         BZ    DK15                                                             
         LA    R2,COMREGH                                                       
         MVI   5(R2),3             FAKE INPUT LENGTH TO THREE                   
         MVC   8(3,R2),K.PUCMKREG                                               
         OI    6(R2),X'80'         TRANSMIT REGION CODE                         
         GOTO1 VALIREG                                                          
         FOUT  COMREGNH,REGNM,20   TRANSMIT REGION NAME                         
*                                                                               
         OC    K.PUCMKDST,K.PUCMKDST                                            
         BZ    DK15                                                             
         LA    R2,COMDSTH                                                       
         MVI   5(R2),3             FAKE INPUT LENGTH TO THREE                   
         MVC   8(3,R2),K.PUCMKDST                                               
         OI    6(R2),X'80'         TRANSMIT DISTRICT CODE                       
         GOTO1 VALIDST                                                          
         FOUT  COMDSTNH,DSTNM,20   TRANSMIT DISTRICT NAME                       
         DROP  K                                                                
*                                                                               
* RESTORE KEY & RECORD, AIO1 GOT CREAMED FROM VALIMED, VALICLT ETC...           
*                                                                               
DK15     MVC   KEY(L'PUCMKEY),SAVEKEY                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'PUCMKEY),SAVEKEY                                           
         BE    *+6                                                              
         DC    H'0'                RECORD IS NOT RESTORED!                      
         GOTO1 GETREC                                                           
*                                                                               
         FOUT  COMPROH,PUCMKPRD,3                                               
         FOUT  COMESTH,=3C' ',3                                                 
         OC    PUCMKEST,PUCMKEST   ESTIMATE ?                                   
         BZ    DK20                NO                                           
         ZICM  R0,PUCMKEST,2                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QEST,DUB                                                         
         FOUT  COMESTH,QEST,3                                                   
DK20     MVC   SVCOMKEY,PUCMKEY    SET FIELDS NEEDED FOR GET RTNS.              
         MVC   AIO,AIO2                                                         
*                                                                               
         FOUT  COMMEDNH,MEDNM,10                                                
         BAS   RE,GETCLT           GET CLIENT RECORD                            
         FOUT  COMCLINH,CLTNM,20                                                
         CLI   PUCMKPRD,C' '       PRODUCT CODE ?                               
         BNH   DKX                 DONE WITH KEY DISPLAY                        
         BAS   RE,GETPRD           GET PRODUCT RECORD                           
         FOUT  COMPRONH,PRDNM,20                                                
         MVI   RECTYPE,C'P'        SET TYPE TO PRD UCOMM FOR NOW                
*                                                                               
         OC    PUCMKEST,PUCMKEST   ESTIMATE CODE ?                              
         BZ    DKX                 DONE WITH KEY DISPLAY                        
         BAS   RE,GETEST           GET ESTIMATE RECORD                          
         FOUT  COMESTNH,ESTNM,20                                                
         MVI   RECTYPE,C'E'        TYPE IS NOW EST UCOMM                        
*                                                                               
         OC    PUCMKREG,PUCMKREG   REGION?                                      
         BZ    DKX                 DONE WITH KEY DISPLAY                        
         MVI   RECTYPE,C'R'        REGION UCOM RECORD                           
*                                                                               
         OC    PUCMKDST,PUCMKDST   DISTRICT?                                    
         BZ    DKX                 DONE WITH KEY DISPLAY                        
         MVI   RECTYPE,C'D'        DISTRICT UCOM RECORD                         
*                                                                               
DKX      DS    0H                  RESET AIO TO UCOMM REC                       
         MVC   AIO,AIO1            FOR DR (DISPLAY REC)                         
         MVC   KEY,SAVEKEY         RESTORE KEYS FOR LISTREC                     
         MVC   KEYSAVE,SAVEKEYS                                                 
*                                                                               
         BAS   RE,SWTCHLUP         SET FIELD PROTECTS/UNPROTECTS                
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*        LIST RECORDS                                                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LR       DS    0H                                                               
*                                                                               
         MVC   AIO,AIO1                                                         
         OC    KEY,KEY             TEST FIRST TIME                              
         BNZ   LR030               KEY IS LAST RECORD READ                      
*                                  CHECK VS. KEYSAVE                            
*                                                                               
         MVC   KEY(31),MYKEY       MYKEY IS VALIDATED KEY                       
*                                                                               
LR010    GOTO1 HIGH                                                             
         B     LR030                                                            
*                                                                               
LR020    GOTO1 SEQ                                                              
*                                                                               
LR030    CLC   KEY(4),KEYSAVE      TEST FOR ALL DONE                            
         BNE   LR900                                                            
         CLI   KEY+3,X'12'         SEE IF UCOM REC                              
         BNE   LR900                                                            
         OC    QCLT,QCLT           SEE IF CLIENT GIVEN                          
         BZ    LR032                                                            
         CLC   KEY+4(3),QCLT                                                    
         BNE   LR900                                                            
*                                                                               
LR032    OC    QPRD,QPRD           SEE IF PRODUCT GIVEN                         
         BZ    LR034                                                            
         CLC   KEY+7(3),QPRD                                                    
         BNE   LR900                                                            
*                                                                               
LR034    OC    BEST,BEST           SEE IF ESTIMATE GIVEN                        
         BZ    LR040                                                            
         CLC   KEY+10(2),BEST                                                   
         BNE   LR900                                                            
*                                                                               
LR040    OC    SVQDIV,SVQDIV       ANYTHING IN STARTING DIV?                    
         BZ    LR040H              NO, CHECK REGION                             
         CLC   KEY+12(3),SVQDIV    EQUAL OR GREATER THAN STARTING DIV?          
         BL    LR020               NO, TRY NEXT RECORD                          
*                                                                               
LR040H   OC    SVQREG,SVQREG       ANYTHING IN STARTING REG?                    
         BZ    LR040M              NO, CHECK DISTRICT                           
         CLC   KEY+15(3),SVQREG    EQUAL OR GREATER THAN STARTING REG?          
         BL    LR020               NO, TRY NEXT RECORD                          
*                                                                               
LR040M   OC    SVQDST,SVQDST       ANYTHING IN STARTING DST?                    
         BZ    LR050               NO, CHECK OTHER CRITERIA IF PRESENT          
         CLC   KEY+18(3),SVQDST    EQUAL OR GREATER THAN STARTING DST?          
         BL    LR020               NO, TRY NEXT RECORD                          
*                                                                               
LR050    GOTO1 GETREC              GET THE UCOM RECORD                          
         L     R6,AIO                                                           
         USING UCOMRECD,R6                                                      
*                                                                               
         LA    R5,LISTAR           LIST AREA                                    
         MVC   LISTAR,SPACES                                                    
         MVC   SELHED(28),=C'CLT  PRD  EST  DIV  REG  DST'                      
         OC    PUCMKDIV,PUCMKDIV                                                
         BZ    LR060                                                            
         MVC   15(3,R5),PUCMKDIV                                                
         MVC   20(3,R5),PUCMKREG                                                
         MVC   25(3,R5),PUCMKDST                                                
*                                                                               
LR060    MVC   00(3,R5),PUCMKCLT                                                
         MVC   05(3,R5),PUCMKPRD                                                
         OC    PUCMKEST,PUCMKEST   ESTIMATE ?                                   
         BZ    DDISP               NO                                           
         ZICM  R0,PUCMKEST,2                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QEST,DUB                                                         
         MVC   10(3,R5),QEST                                                    
DDISP    DS    0H                                                               
         FOUT  SELHEDH                                                          
*                                                                               
         GOTO1 LISTMON                                                          
         B     LR020                                                            
*                                                                               
LR900    DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
* IF PROCESSING A PRODUCT OR ESTIMATE UCOMM RECORD, ONLY "FIELD DATA" *         
*  (IDENTIFIED BY FIELD ID NUMBER OF 30 IN EXTENDED HEADER) MAY GET   *         
*    DATA ENTRY, SO ALL OTHER FIELDS (ID 20) ARE SET PROTECTED.       *         
*       IF ONLY A CLIENT UCOMM RECORD IS BEING PROCESSED,             *         
*    (RECTYPE ='X', THE REVERSE OF ABOVE ACTIONS TAKES PLACE.         *         
*                                                                     *         
***********************************************************************         
SWTCHLUP NTR1                                                                   
         LA    R6,COMLN1TH         1ST DATA FIELD ON SCREEN                     
SLUP     CLI   0(R6),0             END OF SCREEN ?                              
         BE    SLUPXIT             DONE                                         
         TM    1(R6),X'02'         EXTENDED HEADER ?                            
         BZ    SLUPSKP             NO - NEXT FIELD                              
         ZIC   RE,0(R6)                                                         
         AHI   RE,-8                                                            
         AR    RE,R6               RE POINTING TO EXTENDED HEADER               
         CLI   0(RE),20            FIELD NAME "TYPE" AREA ?                     
         BNE   SLUP30              NO - MUST BE "FIELD DATA"                    
         CLI   RECTYPE,C'X'        CLIENT UCOMM RECORD ?                        
         BNE   SLUP20              NO - MUST BE PRODUCT OR ESTIMATE             
         NI    1(R6),X'FF'-X'20'   UNPROTECT FIELD NAME "TYPE" AREA             
         B     SLUPTMT             TRANSMIT FIELD AND CONTINUE                  
SLUP20   OI    1(R6),X'20'         PROTECT FIELD NAME "TYPE" AREA               
         B     SLUPTMT             TRANSMIT FIELD AND CONTINUE                  
SLUP30   CLI   RECTYPE,C'X'        CLIENT UCOMM RECORD ?                        
         BNE   SLUP40              NO - MUST BE PRODUCT OR ESTIMATE             
         OI    1(R6),X'20'         PROTECT "FIELD DATA" AREA                    
         XC    8(L'COMLN1D,R6),8(R6)    CLEAR THIS AREA                         
         B     SLUPTMT             TRANSMIT FIELD AND CONTINUE                  
SLUP40   NI    1(R6),X'FF'-X'20'   UNPROTECT "FIELD DATA" AREA                  
*****    B     SLUPTMT             TRANSMIT FIELD AND CONTINUE                  
SLUPTMT  DS    0H                                                               
         OI    6(R6),X'80'         TRANSMIT FIELD                               
SLUPSKP  DS    0H                                                               
         ZIC   RE,0(R6)                                                         
         AR    R6,RE                                                            
         B     SLUP                GO TEST NEXT FIELD                           
*                                                                               
SLUPXIT  DS    0H                                                               
         CLI   RECTYPE,C'X'        CLIENT UCOMM RECORD ?                        
         BE    SLUPXIT2                                                         
         MVC   COMD1,=C'FIELD DATA'                                             
         MVC   COMD2,=C'----------'                                             
         B     SLUPXIT6                                                         
SLUPXIT2 MVC   COMD1,SPACES                                                     
         MVC   COMD2,SPACES                                                     
SLUPXIT6 FOUT  COMD1H                                                           
         FOUT  COMD2H                                                           
*                                                                               
         CLI   RECTYPE,C'X'        CLIENT UCOMM RECORD ?                        
         BE    EXIT                YES - DONE                                   
*                                                                               
*                                                                               
SLUPFDTA DS    0H                  RECTYPE = 'P' OR 'E'                         
         CLC   COMLN1T,RECTYPE     IS FIELD DATA ENTRY OK ?                     
         BE    *+12                YES - LEAVE FIELD DATA UNPROTECTED           
         OI    COMLN1DH+1,X'20'    PROTECT FIELD DATA                           
         OI    COMLN1DH+6,X'80'    TRANSMIT FIELD                               
         CLC   COMLN2T,RECTYPE     IS FIELD DATA ENTRY OK ?                     
         BE    *+12                YES - LEAVE FIELD DATA UNPROTECTED           
         OI    COMLN2DH+1,X'20'    PROTECT FIELD DATA                           
         OI    COMLN2DH+6,X'80'    TRANSMIT FIELD                               
         CLC   COMLN3T,RECTYPE                                                  
         BE    *+12                SAME FOLLOWS FOR ALL 8 ROWS                  
         OI    COMLN3DH+1,X'20'                                                 
         OI    COMLN3DH+6,X'80'                                                 
         CLC   COMLN4T,RECTYPE                                                  
         BE    *+12                                                             
         OI    COMLN4DH+1,X'20'                                                 
         OI    COMLN4DH+6,X'80'                                                 
         CLC   COMLN5T,RECTYPE                                                  
         BE    *+12                                                             
         OI    COMLN5DH+1,X'20'                                                 
         OI    COMLN5DH+6,X'80'                                                 
         CLC   COMLN6T,RECTYPE                                                  
         BE    *+12                                                             
         OI    COMLN6DH+1,X'20'                                                 
         OI    COMLN6DH+6,X'80'                                                 
         CLC   COMLN7T,RECTYPE                                                  
         BE    *+12                                                             
         OI    COMLN7DH+1,X'20'                                                 
         OI    COMLN7DH+6,X'80'                                                 
         CLC   COMLN8T,RECTYPE                                                  
         BE    *+12                                                             
         OI    COMLN8DH+1,X'20'                                                 
         OI    COMLN8DH+6,X'80'                                                 
         CLC   COMLN9T,RECTYPE                                                  
         BE    *+12                                                             
         OI    COMLN9DH+1,X'20'                                                 
         OI    COMLN9DH+6,X'80'                                                 
         CLC   COMLNAT,RECTYPE                                                  
         BE    *+12                                                             
         OI    COMLNADH+1,X'20'                                                 
         OI    COMLNADH+6,X'80'                                                 
         CLC   COMLNBT,RECTYPE                                                  
         BE    *+12                                                             
         OI    COMLNBDH+1,X'20'                                                 
         OI    COMLNBDH+6,X'80'                                                 
         CLC   COMLNCT,RECTYPE                                                  
         BE    *+12                                                             
         OI    COMLNCDH+1,X'20'                                                 
         OI    COMLNCDH+6,X'80'                                                 
         B     EXIT                DONE                                         
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*       CLEAR ALL SCREEN FIELDS WITH EXTENDED HEADERS                 *         
*                                                                     *         
***********************************************************************         
CLRLUP   NTR1                                                                   
         LA    R6,COMLN1TH         1ST DATA FIELD ON SCREEN                     
CLUP     CLI   0(R6),0             END OF SCREEN ?                              
         BE    CLUPXIT             DONE                                         
         TM    1(R6),X'02'         EXTENDED HEADER ?                            
         BZ    CLUPSKP             NO - NEXT FIELD                              
         ZIC   RE,0(R6)                                                         
         AHI   RE,-17              FOR EXECUTED CLEAR                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R6),8(R6)       CLEAR THIS AREA                              
         OI    6(R6),X'80'         TRANSMIT FIELD                               
CLUPSKP  DS    0H                                                               
         ZIC   RE,0(R6)                                                         
         AR    R6,RE                                                            
         B     CLUP                GO TEST NEXT FIELD                           
*                                                                               
CLUPXIT  DS    0H                                                               
         XIT1                                                                   
*                                                                               
*                                                                               
CLRNAME  XC    COMMEDN,COMMEDN                                                  
         XC    COMCLIN,COMCLIN                                                  
         XC    COMPRON,COMPRON                                                  
         XC    COMESTN,COMESTN                                                  
         XC    COMDIVN,COMDIVN                                                  
         XC    COMREGN,COMREGN                                                  
         XC    COMDSTN,COMDSTN                                                  
         FOUT  COMMEDNH                                                         
         FOUT  COMCLINH                                                         
         FOUT  COMPRONH                                                         
         FOUT  COMESTNH                                                         
         FOUT  COMDIVNH                                                         
         FOUT  COMREGNH                                                         
         FOUT  COMDSTNH                                                         
         BR    RE                  RETURN                                       
*                                                                               
         EJECT                                                                  
*                                                                               
*                              ROUTINES FOR READING VARIOUS RECORDS             
*                                                                               
GETLVLC  NTR1                  MUST GET UCOM CLIENT INTO AIO2                   
         MVC   SAVEKEY,KEY                                                      
         XC    KEY+7(18),KEY+7     CLEAR ALL AFTER CLT CODE                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'PUCMKEY),KEYSAVE                                           
         BE    *+6                 CLIENT UCOM REC FOUND ?                      
         DC    H'0'                NO - DIE                                     
*                                                                               
         MVC   AIO1,AIO                                                         
         MVC   AIO,AIO2                                                         
*                                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(L'PUCMKEY),SAVEKEY     RESTORE KEY                           
*                                                                               
         CLI   ACTNUM,ACTADD       IF ADDING, NO SEQ RESTORE                    
         BE    GETLVLCX                                                         
         GOTO1 HIGH                RESTORE KEY SEQ                              
         CLC   KEY(L'PUCMKEY),KEYSAVE                                           
         BE    *+6                 REC FOUND ?                                  
         DC    H'0'                NO - DIE                                     
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC              REREAD RECORD                                
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                DIE ON ANY ERROR                             
*                                                                               
GETLVLCX DS    0H                                                               
         MVC   AIO,AIO1            RESTORE PRD/EST UCOM REC ADDRESS             
         B     EXIT                RETURN                                       
*                                                                               
*                                                                               
GETCLT   NTR1                      GET CLIENT RECORD                            
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CLTHDRD,R6                                                       
         MVC   PCLTKAGY,AGENCY                                                  
         MVC   PCLTKMED,SVMED                                                   
         MVI   PCLTKRCD,X'02'                                                   
         MVC   PCLTKCLT,SVCLT                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         XC    CLTNM,CLTNM                                                      
         MVC   CLTNM(9),=C'NOT FOUND'                                           
         CLC   KEY(25),KEYSAVE                                                  
         BNE   GETCLTX                                                          
*                                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         MVC   CLTNM,PCLTNAME                                                   
*                                                                               
GETCLTX  DS    0H                                                               
         B     EXIT                RETURN                                       
*                                                                               
GETPRD   NTR1                      GET PRODUCT RECORD                           
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PRDHDRD,R6                                                       
         MVC   PPRDKAGY,AGENCY                                                  
         MVC   PPRDKMED,SVMED                                                   
         MVI   PPRDKRCD,X'06'                                                   
         MVC   PPRDKCLT,SVCLT                                                   
         MVC   PPRDKPRD,SVPRD                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         XC    PRDNM,PRDNM                                                      
         MVC   PRDNM(9),=C'NOT FOUND'                                           
         CLC   KEY(25),KEYSAVE                                                  
         BNE   GETPRDX                                                          
*                                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         MVC   PRDNM,PPRDNAME                                                   
*                                                                               
GETPRDX  DS    0H                                                               
         B     EXIT                RETURN                                       
*                                                                               
GETEST   NTR1                      GET ESTIMATE RECORD                          
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING ESTHDRD,R6                                                       
         MVC   PESTKAGY,AGENCY                                                  
         MVC   PESTKMED,SVMED                                                   
         MVI   PESTKRCD,X'07'                                                   
         MVC   PESTKCLT,SVCLT                                                   
         MVC   PESTKPRD,SVPRD                                                   
         MVC   PESTKEST,SVEST                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         XC    ESTNM,ESTNM                                                      
         MVC   ESTNM(9),=C'NOT FOUND'                                           
         CLC   KEY(25),KEYSAVE                                                  
         BNE   GETESTX                                                          
*                                                                               
         GOTO1 GETREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO                                                           
         MVC   ESTNM,PESTNAME                                                   
*                                                                               
GETESTX  DS    0H                                                               
         B     EXIT                RETURN                                       
*                                                                               
         DROP  R6                                                               
*                                                                               
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
TRAPERRX GOTO1 ERREX2                                                           
         B     EXIT                                                             
*                                                                               
BADLNTH  EQU   071                                                              
NOCHGTYP EQU   072                                                              
NOCLTLEV EQU   073                                                              
NOCLTPRD EQU   074                                                              
NOCLTEST EQU   075                                                              
INVDTLN  EQU   076                                                              
NOCLTREG EQU   077                                                              
NOCLTDST EQU   078                                                              
MAXUCOMM EQU   237                                                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMC9D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMD9D                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE PRSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE                                                         
SVAREA   DS    0H                                                               
FIRSTT   DS    X                 FIRST TIME FOR REPORT                          
SVCOMP   DS    X                                                                
SVCOMKEY DS    0CL25                                                            
SVAGY    DS    CL2                                                              
SVMED    DS    CL1                                                              
SVRCD    DS    X                                                                
SVCLT    DS    CL3                                                              
SVPRD    DS    CL3                                                              
SVEST    DS    XL2                                                              
WORK2    DS    CL64                                                             
SAVERE   DS    F                                                                
SAVER2   DS    F                                                                
NEWCLT   DS    X                                                                
ERRSW    DS    X                                                                
ENDSW    DS    X                                                                
ADDSW    DS    X                                                                
ATTSW    DS    C                   Y= AT&T CLIENT                               
*                                  8 EST AND 0 PRD ALLOWED                      
RECTYPE  DS    CL1                 "X" = CLT LEVEL RECORD                       
*                                  "P" = PRD LEVEL RECORD                       
*                                  "E" = EST LEVEL RECORD                       
*                                  "R" = REG LEVEL RECORD                       
*                                  "D" = DST LEVEL RECORD                       
SVRECTYP DS    CL1                                                              
PRDELC   DS    X                   USED IN VAL REC, SET TO X'41'                
ESTELC   DS    X                   USED IN VAL REC, SET TO X'51'                
REGELC   DS    X                   USED IN VAL REC, SET TO X'61'                
DSTELC   DS    X                   USED IN VAL REC, SET TO X'71'                
*                                                                               
RPRDCNT  DS    X                   NO. OF PRD LEVEL ELEMENTS                    
RESTCNT  DS    X                   NO. OF EST LEVEL ELEMENTS                    
RREGCNT  DS    X                   NO. OF REG LEVEL ELEMENTS                    
RDSTCNT  DS    X                   NO. OF DST LEVEL ELEMENTS                    
*                                                                               
SPRDCNT  DS    X                   NO. OF PRD LEVEL SCREEN ENTRIES              
SESTCNT  DS    X                   NO. OF EST LEVEL SCREEN ENTRIES              
SREGCNT  DS    X                   NO. OF REG LEVEL SCREEN ENTRIES              
SDSTCNT  DS    X                   NO. OF DST LEVEL SCREEN ENTRIES              
*                                                                               
MYKEY    DS    CL31                                                             
SAVEKEY  DS    CL31                                                             
SAVEKEYS DS    CL31               KEYSAVE                                       
SVCLTKEY DS    CL31                                                             
*                                                                               
SVQDIV   DS    CL3                                                              
SVQREG   DS    CL3                                                              
SVQDST   DS    CL3                                                              
*                                                                               
SVDMINBT DS    XL(L'DMINBTS)                                                    
*                                                                               
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE PCLTREC                                                        
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE PPRDREC                                                        
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE PESTREC                                                        
         EJECT                                                                  
UCOMRECD DSECT                                                                  
       ++INCLUDE PUCOMREC                                                       
*                                                                               
         ORG                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDFLDIND                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007PRSFM19   06/15/15'                                      
         END                                                                    
