*          DATA SET PPREP7802  AT LEVEL 063 AS OF 04/26/06                      
*PHASE PP7802A                                                                  
*INCLUDE PRNTOFC                                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'PP7802 - PRINTPAK TRAFFIC TURNAROUND'                           
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SMYE 04/26/06 FIX COSMETIC BUG AT TLOUT18 INTRODUCED WITH BELOW               
*                 STEWARDSHIP CHANGES                                           
*                                                                               
* SMYE 03/06    ADD CODE FOR STEWARDSHIP BUYS                                   
*                                                                               
* SMYE 12/05    PRINT 2-CHARACTER CLIENT OFFICES                                
*                                                                               
* SMYE 05/05    ALWAYS REPLACE "AD NO" WITH AD ID IF Ad ID USED                 
*                                                                               
* SMYE 01/05    CONDITIONALLY REPLACE "AD NO" WITH Ad ID                        
*                                                                               
* SMYE 01/05-12/05  ADD WEB INSERTION ORDER HANDLING                            
*                                                                               
* SMYE 06/04    FIX STD COMMENT HANDLING IN PRTCOM (PER YKAP IN 7702)           
*                                                                               
* SMYE 07/01    DISPLAY LEGAL WARNING INFO IF PRESENT AND CHANGE                
*               PP78WRK AND SOME INTERNAL CODING (USE BRAS) FOR                 
*               ADDRESSABILITY PROBLEMS (INSTAB MOVED TO A CSECT)               
*               AND SKIP "NO TRAFFIC" (X'20' IN PBDSTAT) BUYS                   
*                                                                               
*  BPLA  03/2101   FIX BUG INTRODUCED WITH GETCCAP MOVE                         
*                                                                               
* KWAN 01/31/01 PRINT EXTENSION DATE INFO IF PRESENT                            
*                                                                               
* SMYE 05/00    CHANGES FOR LARGER PPNEWFILE                                    
*                                                                               
* BPLA 11/99    REMOVE RATE CHG CHECK FROM P78 PROFILE                          
*               ONLY CHECK P72A PROFILE                                         
*                                                                               
* BPLA 10/99    FLAG RATE CHANGES (IF PROFILE OPTION IS SET)                    
*               ALSO READ AND CHECK P72A PROFILE TO THE IF RATE                 
*               CHANGE SHOULD CAUSE A CHANGE ORDER                              
*                                                                               
* SMYE 11/97    DISPLAY SFH=  (AT TL3201..)                                     
*                                                                               
* BPLA 6/96     USE ACTIVITY DATE IN QOPT 5-7 (MOVED TO ACTDATE)                
*               INSTEAD OF TODAY                                                
*                                                                               
* SMYE 2/96     DISPLAY REPEAT PUB FROM PIOELEM (X'70' ELEMENT)                 
*               COMMENTED OUT MOST OF ENDEST, ENDPRD & ENDCLT ROUTINES          
*               WHICH ARE CURRENTLY UNUSED  (TLOUT CSECT AT 4K+)                
*                                                                               
* SMYE 12/13/95 CHANGED DTCNV TO DATCON WITH NEW PARAM'S                        
*                                                                               
* BPLA 6/95     DISPLAY SHIP DATE DATA  (X'86' ELEMENT)                         
*                                                                               
* BPLA 12/9/93  IF QPROG(23) CHG (CLT/PRD/EST CHANGE) - FORCE NEW PAGE          
*               SINCE CLT/PRD/EST ARE IN HEADLINES.                             
*                                                                               
* LWEI 02/02/93 DISPLAY REF=                                                    
*                                                                               
* BPLA 12/30/92 DISPLAY FSI=                                                    
*                                                                               
* BPLA 5/5/92   SKIP BETWEEN INSERTIONS                                         
*                                                                               
* BPLA 5/4/92   DON'T CHOP IC= AND PI= FREE FORM COMMENTS                       
*                                                                               
* BPLA 4/4/92   CHANGES TO HANDLE LARGE IC= AND PI= COMMENTS                    
*                                                                               
* ROSA 3/15/90  INCLUDE STANDARD COMMENTS IN THE 2/26/90 CHANGE                 
*                                                                               
* ROSA 2/26/90  BASED ON 78 PROFILE PRINT ALL BUYS WHOSE MEDIA,CLI              
*               PRD,EST ARE EQUAL ON THE SAME PAGE WHEN OPTION IS E             
*               WHEN OPTION IS P BREAK ON PUB NUMBER CHANGE                     
*               NOTE THERE IS ONE REQUEST PER BUY FOR THIS REPORT               
*                                                                               
*        QOPT1  BLANK = ALL INSERTIONS                                          
*               N = UNORDERED INSERTIONS ONLY                                   
*               O = ORDERED INSERTIONS ONLY                                     
*               *NOTE PPF3 (DAILY ACTIVITY) SETS QOPT1 TO 'N'                   
*                                                                               
*        QOPT2  BLANK= USE PROFILE +0                                           
*               S = SHOW SPACE DESC.                                            
*               $ = SHOW GROSS                                                  
*                                                                               
*        QOPT4  BLANK = USE PROFILE +1                                          
*               N = NO ADFILE INFO                                              
*               1 = SHOW COPY NUMBER                                            
*               2 = SHOW COPY NUMBER AND CAPTION                                
*                                                                               
*        QOPT6  BLANK=USE PROFILE +4                                            
*               N = DON'T SHOW TRAFFIC ADDRESS                                  
*               Y = SHOW TRAFFIC ADDRESS                                        
*               S = SHOW SHIPPING ADDRESS                                       
*                                                                               
*        QOPT7  Y = INCLUDE TEST BUYS- FROM PROGPROF+2                          
*                                                                               
*        PROFILE OPTIONS                                                        
*               +0 = QOPT2 OVERRRIDE      S,$                                   
*               +1 = QOPT4 OVERRRIDE      N,1,2                                 
*               +2 = QOPT7 OVERRRIDE      N,Y                                   
*               +3 = Y= LIST STANDARD COMMENTS                                  
*               +4 = Y= SHOW TRAFFIC ADDRESS                                    
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PP7802   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PP7802                                                         
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         MVI   RC2DSECT,C'Y'                                                    
         L     R7,PPWORK2C                                                      
         USING PPWORK2D,R7                                                      
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PP78WRKD,R8                                                      
*                                                                               
         MVC   ACONIO1,ACONIO      (A)PCONREC                                   
*                                                                               
         L     R1,=A(SINSTAB)                                                   
         ST    R1,ASINSTAB                                                      
*                                                                               
         XC    DMCB(4),DMCB        NEED ADDRESS OF OFFICER                      
         MVC   DMCB+4(4),=X'D9000A38'                                           
         L     RF,VCOMFACS                                                      
         L     RF,(CCALLOV-COMFACSD)(RF)                                        
         GOTO1 (RF),DMCB                                                        
         MVC   VOFFICER,DMCB                                                    
*                                                                               
         L     R1,=V(PRNTOFC)                                                   
         ST    R1,VPRNTOFC         STORE PRNTOFC ADDRESS                        
*                                                                               
         CLI   MODE,PROCBUY                                                     
         BE    PROCESS                                                          
         CLI   MODE,FBUYREQ                                                     
         BE    FIRST                                                            
         CLI   MODE,FBUYCLI                                                     
         BE    CFIRST                                                           
         CLI   MODE,RUNFRST                                                     
         BE    BINTAB                                                           
         CLI   MODE,LBUYREQ                                                     
         BNE   EXIT                                                             
         B     LAST                                                             
*                                                                               
BINTAB   DS    0H                                                               
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   ACTIVITY,0                                                       
         XC    OLDPROF,OLDPROF                                                  
         XC    OLDKEY,OLDKEY                                                    
         XC    OLDJOB,OLDJOB                                                    
         XC    ESTTOTS(3*L'ESTTOTS),ESTTOTS                                     
         L     R1,=A(TLPRT)                                                     
         ST    R1,ATLPRT                                                        
         L     R1,=A(PRTCOM)                                                    
         ST    R1,APRTCOM                                                       
         L     R1,=A(TLOUT)                                                     
         ST    R1,ATLOUT                                                        
         MVC   WORK+0(2),RCDATE+6                                               
         MVC   WORK+2(2),RCDATE+0                                               
         MVC   WORK+4(2),RCDATE+3                                               
         MVC   TODAY,WORK                                                       
         GOTO1 DATCON,DMCB,WORK,(3,BTODAY)                                      
         XC    REQOLD,REQOLD                                                    
         L     R1,=A(SCOMTAB)                                                   
         ST    R1,ASCOMTAB                                                      
         SR    R0,R0               SET BSPARS FOR BINSRCH                       
         L     R1,ASCOMTAB                                                      
         SR    R2,R2                                                            
         LA    R3,7                                                             
         LA    R4,6                                                             
         LHI   R5,1000                                                          
         STM   R0,R5,BSPARS                                                     
         B     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FIRST    DS    0H                                                               
         XC    ACTDATE,ACTDATE     CLEAR ACTIVITY DATE                          
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         MVI   FCRDACTV,C'Y'                                                    
*                                                                               
         MVC   P1,SPACES                                                        
         MVI   PUBTABX,X'FF'                                                    
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
CFIRST   DS    0H                                                               
*                                                                               
         XC    WORK,WORK           GET P72A PROFILE                             
         MVC   WORK(4),=C'P72A'                                                 
         NI    WORK,X'BF'          MAKE SYSTEM LOWER CASE                       
         MVC   WORK+4(2),PCLTKAGY                                               
         MVC   WORK+6(1),PCLTKMED                                               
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
         GOTO1 GETPROF,DMCB,WORK,P72APROF,DATAMGR                               
*                                                                               
         CLC   REQOLD(23),QPROG    CHECK FOR CHANGE OF CLT/PRD/EST              
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'       NEW EST FORCE NEW PAGE - SINCE               
*                                  CLT/PRD/EST IS IN HEADLINES                  
         OC    OLDPROF,OLDPROF     FIRST TIME?                                  
         BNZ   *+10                                                             
         MVC   OLDPROF,PROGPROF                                                 
         CLI   OLDPROF+5,C'E'                                                   
         BNE   CHK4PUB                                                          
         CLC   REQOLD(23),QPROG    CHANGE IN M,CLI,PRD,EST                      
         BE    NEWAY                                                            
*                                                                               
SEEREQ   OC    REQOLD,REQOLD                                                    
         BZ    MOVEREQ                                                          
         CLI   OLDPROF+3,C'Y'      EXPAND STANDARD COMMENTS                     
         BNE   MOVEREQ                                                          
         GOTO1 APRTCOM                                                          
         MVI   FORCEHED,C'Y'                                                    
MOVEREQ  MVC   REQOLD,QPROG                                                     
         B     NEWAY                                                            
*                                                                               
CHK4PUB  CLI   OLDPROF+5,C'P'      BREAK ON PUB                                 
         BNE   OLDWAYA                                                          
*                                                                               
         CLC   REQOLD,QPROG        SAME REQUEST                                 
         BE    NEWAY                                                            
         B     SEEREQ                                                           
*                                                                               
OLDWAYA  MVC   REQOLD,QPROG                                                     
OLDWAY   MVI   FORCEHED,C'Y'                                                    
         MVI   FORSTDCO,C'Y'       FORCE STANDARD COMMENTS                      
*                                                                               
NEWAY    DS    0H                                                               
         MVC   OLDPROF,PROGPROF                                                 
         MVC   LASTAGY,QAGENCY                                                  
         MVI   FCRDTEST,C'N'                                                    
*                                                                               
         MVC   ACTDATE,BTODAY                                                   
         CLC   QOPT5(3),SPACES                                                  
         BE    *+10                                                             
         MVC   ACTDATE,QOPT5       SAVE ACTIVITY DATE                           
         MVC   QOPT5(3),SPACES                                                  
*                                                                               
         OC    PROGPROF,PROGPROF   CHK FOR PROFILE                              
         BZ    CFIRSTX                                                          
*                                                                               
* SET QOPTS FROM PROFILE UNLESS OVERRIDDEN IN $REQ                              
*                                                                               
         CLI   QOPT2,C' '                                                       
         BNE   CF5                                                              
         MVC   QOPT2,PROGPROF                                                   
CF5      CLI   QOPT4,C' '                                                       
         BNE   CF8                                                              
         MVC   QOPT4,PROGPROF+1                                                 
*                                                                               
CF8      DS    0H                  FOR QOPT5 - DOUBLE SPACING                   
         CLI   QOPT5,C' '                                                       
         BNE   CF10                                                             
         MVC   QOPT5,PROGPROF+6                                                 
*                                                                               
CF10     DS    0H                                                               
         CLI   QOPT6,C' '                                                       
         BNE   CF15                                                             
         MVC   QOPT6,PROGPROF+4    TRAFFIC ADDRESS                              
*                                                                               
CF15     MVI   FCGTTREP,C'N'                                                    
         CLI   QOPT6,C'S'                                                       
         BE    CF17                                                             
         CLI   QOPT6,C'Y'                                                       
         BNE   CF18                                                             
CF16     MVI   FCGTTREP,C'Y'       TO GET TRAFFIC ADDRESS                       
         B     CF18                                                             
CF17     MVI   FCGTTREP,C'S'       TO GET SHIPING ADDRESS                       
*                                                                               
CF18     CLI   QOPT7,C' '          TEST BUYS                                    
         BNE   CFIRSTX                                                          
         MVC   QOPT7,PROGPROF+2                                                 
*                                                                               
CFIRSTX  CLI   QOPT7,C'Y'                                                       
         BNE   EXIT                                                             
         MVI   FCRDTEST,C'Y'       SO PPG WILL PASS TEST BUYS                   
         B     EXIT                                                             
*                                                                               
LAST     DS    0H                                                               
         CLI   ACTIVITY,0                                                       
         BE    CLEANUP                                                          
         MVI   ACTIVITY,0                                                       
*                                                                               
LASTA    MVI   PBUYREC,X'FF'                                                    
         MVC   PBUYREC+1(24),PBUYREC                                            
         GOTO1 ATLOUT                                                           
         B     EXIT                                                             
*                                                                               
EXIT     DS    0H                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CLEANUP  CLI   OLDPROF+5,C'E'                                                   
         BE    XIT                                                              
         CLI   OLDPROF+5,C'P'                                                   
         BE    XIT                                                              
*                                                                               
         CLI   OLDPROF+3,C'Y'      EXPAND STANDARD COMMENTS                     
         BNE   XIT                                                              
         GOTO1 APRTCOM                                                          
*                                                                               
XIT      DS    0H                                                               
         XIT1                                                                   
*                                                                               
FORSTDCO DC    X'0'                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PROCESS  DS    0H                  PROCESS BUY                                  
         TM    PBDSTAT,X'20'       "NO TRAFFIC" BUY ?                           
         BO    EXIT                YES - SKIP                                   
         TM    PBDSTAT2,X'40'      STEWARD BUY ?                                
         BO    EXIT                YES - SKIP                                   
         CLI   QBUYLIN,C' '        SEE IF ONE BUY                               
*SMY*    BE    TLIN18B                                                          
         BE    TLIN17C                                                          
         PACK  DUB,QBUYLIN                                                      
         CVB   R0,DUB                                                           
         STC   R0,BYTE                                                          
         CLC   PBUYKLIN,BYTE                                                    
         BNE   EXIT                NOT RIGHT LINE NUMBER                        
*                                                                               
TLIN17C  DS    0H                  FIND MOST RECENT I/O (IF ANY)                
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'70'                                                     
         MVI   IOELCOD,X'70'                                                    
         XC    LASTDAT,LASTDAT    CLEAR MOST RECENT I/O DATE                    
*                                                                               
TLIN17F  DS    0H                  LOOK FOR "REGULAR" I/O                       
         BAS   RE,INEXTEL                                                       
         BNE   TLIN17J                                                          
         USING PIOELEM,R2                                                       
         MVC   LASTDAT,PIODATE     SAVE DATE OF MOST RECENT I/O                 
         B     TLIN17F             LOOK FOR MORE                                
*                                                                               
TLIN17J  DS    0H                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'71'                                                     
*                                                                               
TLIN17M  DS    0H                  LOOK FOR WEB I/O                             
         BAS   RE,INEXTEL                                                       
         BNE   TLIN17X                                                          
         CLC   PIODATE,LASTDAT     PREVIOUS TO "REGULAR" I/O ?                  
         BL    TLIN17M             YES - IGNORE                                 
         MVI   IOELCOD,X'71'     USE WEB I/O ELEM FOR "CHANGE" TESTING          
         DROP  R2                    (SEE TLIN18B BELOW)                        
*                                                                               
TLIN17X  DS    0H                                                               
*                                                                               
TLIN18B  DS    0H                                                               
         XC    LASTIOD,LASTIOD     CLEAR DATE OF LASTIO                         
         XC    LASTDAT,LASTDAT                                                  
         XC    ALASTIO,ALASTIO     CLEAR ADDR OF LAST IO                        
         MVI   CHGSW,0                                                          
         MVI   CHGSW2,0                                                         
         MVI   IOSW,C'N'                                                        
*                                  FIND LAST I/O                                
         LA    R2,PBUYREC+33                                                    
*SMY*    MVI   ELCODE,X'70'                                                     
         MVC   ELCODE,IOELCOD      IOELCOD SET IN TLIN17C TO TLIN17X            
TLIN18D  BAS   RE,INEXTEL                                                       
         BNE   TLIN18DX                                                         
         USING PIOELEM,R2                                                       
         OC    PIODATE,PIODATE                                                  
         BZ    TLIN18D                                                          
         CLC   PIODATE,LASTDAT                                                  
         BL    TLIN18D                                                          
         MVC   LASTDAT,PIODATE                                                  
         MVI   IOSW,C'O'                                                        
         CLI   PIOTYP,C'D'                                                      
         BNE   *+8                                                              
         MVI   IOSW,C'C'           CANCELLED                                    
         ST    R2,ALASTIO          SAVE A(LAST ELEM)                            
         GOTO1 DATCON,DMCB,(3,PIODATE),(2,LASTIOD)                              
*                                  LASTIOD - PACKED                             
         B     TLIN18D                                                          
*                                                                               
TLIN18DX DS    0H                                                               
*                                                                               
TLIN18E  DS    0H                                                               
         MVC   FULL(1),QOPT1                                                    
         TM    PBUYCNTL,X'80'                                                   
         BNZ   TLIN18F                                                          
*                                  NOT DELETED                                  
         CLI   FULL,C' '                                                        
         BE    TLIN19                                                           
         CLC   IOSW,FULL                                                        
         BE    TLIN19                                                           
         CLI   FULL,C'N'           IF LISTING UNORDERED CHECK                   
         BNE   TLINX               CHANGE STATUS                                
*                                                                               
         OC    LASTIOD,LASTIOD     CHK FOR LAST IO DATE                         
         BZ    TLIN18E3                                                         
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'24'        SEE IF IC COMM CHANGED SINCE LASTIO          
*                                                                               
TLIN18E1 DS    0H                                                               
         BAS   RE,INEXTEL                                                       
         BNE   TLIN18E3                                                         
         USING PCHGELEM,R2                                                      
         CLC   PCHGDAT,LASTIOD                                                  
         BNH   TLIN18E1            WAS BL                                       
         TM    PCHGIND1,X'01'      INSERTION ORDER COMMENT CHANGE               
         BZ    *+8                                                              
         OI    CHGSW,X'80'                                                      
         TM    PCHGIND3,X'01'      POSITION INSTRUCTIONS CHANGE                 
         BZ    *+8                                                              
         OI    CHGSW,X'01'                                                      
*                                                                               
         CLI   P72APROF+14,C'Y'    SEE IF RATE CHG SHOULD CAUSE CHG             
         BNE   TLIN18E1            IF ON, FLAG RATE CHANGES                     
*                                                                               
TLIN18E2 TM    PCHGIND1,X'40'      RATE CHANGE?                                 
         BZ    *+8                                                              
         OI    CHGSW,X'02'                                                      
*                                                                               
         B     TLIN18E1            KEEP LOOKING                                 
         DROP  R2                                                               
*                                                                               
         USING PIOELEM,R2                                                       
*                                                                               
TLIN18E3 L     R2,ALASTIO          RESTORE R2 TO LAST IO ELEM                   
         LTR   R2,R2                                                            
         BZ    TLIN18EX            NO ELEM                                      
*                                                                               
         CLC   PIOIDATE,PBUYKDAT   INS DATE                                     
         BE    *+8                                                              
         OI    CHGSW,X'10'         DATE CHG                                     
         CLC   PIOJOB,PBDJOB       JOB NO.                                      
         BE    *+8                                                              
         OI    CHGSW,X'20'         JOB CHANGE                                   
         CLI   QMEDIA,C'N'                                                      
         BE    TLIN18E8                                                         
*                                                                               
         CLI   PBDSPACE,X'FF'      OUTDOOR                                      
         BNE   TLIN18E4                                                         
         CLC   PIOSPACE,PBDSPACE                                                
         BE    *+8                                                              
         OI    CHGSW,X'40'         SPACE CHG                                    
         B     TLIN18EX                                                         
*                                                                               
TLIN18E4 DS    0H                  MAGE SPACE                                   
         MVC   WORK(17),PBDSPACE                                                
         MVC   WORK+17(17),PIOSPACE                                             
         OC    WORK(34),SPACES                                                  
         CLC   WORK(17),WORK+17                                                 
         BE    *+8                                                              
         OI    CHGSW,X'40'         SPACE CHG                                    
         B     TLIN18EX                                                         
*                                                                               
TLIN18E8 DS    0H                  NEWSPAPERS                                   
         MVC   WORK(6),PIOSAU                                                   
         MVC   WORK+6(6),PBDSPACE                                               
         OC    WORK(12),SPACES                                                  
         CLC   WORK(6),WORK+6                                                   
         BE    *+8                                                              
         OI    CHGSW,X'40'         SPACE CHG                                    
         CP    PIOUNITS,PBDUNITS                                                
         BE    *+8                                                              
         OI    CHGSW,X'40'         SPACE CHG                                    
         CP    PIOCLMS,PBDCLMS                                                  
         BE    *+8                                                              
         OI    CHGSW,X'40'         SPACE CHG                                    
         CLC   PIOUIND,PBDUIND                                                  
         BE    *+8                                                              
         OI    CHGSW,X'40'         SPACE CHG                                    
         CLC   PBDCL,PIOPRM                                                     
         BE    *+8                                                              
         OI    CHGSW,X'40'         SPACE CHG                                    
*                                                                               
TLIN18EX CLI   CHGSW,0                                                          
         BE    TLINX               NO CHANGES SINCE LAST I/O                    
*                                  DATE/JOB/SPACE/IC COM/PI COM                 
         B     TLIN20                                                           
*                                                                               
TLIN18F  DS    0H                  DELETED BUYS                                 
******** CLI   IOSW,C'N'           BYPASS UNORDED                               
******** BE    TLINX                                                            
         CLI   FULL,C'N'           LIST 'UNCANCELLED'                           
         BNE   TLIN20                                                           
         CLI   IOSW,C'C'                                                        
         BE    TLINX               ONLY BYPASS DELETED BUYS WITH                
         B     TLIN20              CANCELLATION ORDERS                          
*                                                                               
TLIN19   DS    0H                                                               
         CLC   PBDDATE,ACTDATE     SEE IF LAST CHANGE TODAY                     
         BNE   TLIN20                                                           
         TM    PBDDTIND,X'08'      INS DATE CHANGE                              
         BZ    *+8                                                              
         OI    CHGSW,X'10'                                                      
         TM    PBDDTIND,X'01'      IC COMMENT                                   
         BZ    *+8                                                              
         OI    CHGSW,X'80'                                                      
         TM    PBDDTIN3,X'01'      PI COMMENT                                   
         BZ    *+8                                                              
         OI    CHGSW,X'01'                                                      
         TM    PBDDTIN2,X'08'      JOB CHANGE                                   
         BZ    *+8                                                              
         OI    CHGSW,X'20'                                                      
         TM    PBDDTIND,X'20'      UNITS  - SET SPACE CHANGE                    
         BZ    *+8                                                              
         OI    CHGSW,X'40'                                                      
         TM    PBDDTIND,X'10'      SPACE DESCRIPTION                            
         BZ    *+8                                                              
         OI    CHGSW,X'40'                                                      
         TM    PBDDTIN2,X'80'      CLOSING DATE                                 
         BZ    *+8                                                              
         OI    CHGSW2,X'80'                                                     
         TM    PBDDTIN2,X'01'      IO DATE CHANGE                               
         BZ    *+8                                                              
         OI    CHGSW2,X'01'                                                     
         TM    PBDDTIN3,X'02'      MATERIALS CLOSING                            
         BZ    *+8                                                              
         OI    CHGSW2,X'02'                                                     
         TM    PBDDTIN3,X'04'      MADE LIVE                                    
         BZ    *+8                                                              
         OI    CHGSW2,X'04'                                                     
         TM    PBDDTIN3,X'40'      AD CODE ADDED                                
         BZ    *+8                                                              
         OI    CHGSW2,X'08'        AD CODE ADDED                                
*                                                                               
         CLI   P72APROF+14,C'Y'    RATE CHG P72A IO GENERATION                  
         BNE   TLIN20                                                           
*                                                                               
         TM    PBDDTIND,X'40'      RATE CHANGE                                  
         BZ    *+8                                                              
         OI    CHGSW,X'02'                                                      
*                                                                               
TLIN20   DS    0H                                                               
         MVI   ACTIVITY,C'Y'                                                    
         GOTO1 ATLOUT                                                           
*                                                                               
TLINX    B     EXIT                SKIP THIS BUY                                
*                                                                               
INEXTEL  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    INEXTEL2                                                         
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     INEXTEL+2                                                        
INEXTEL2 LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TLOUT    CSECT                     OUTPUT                                       
         NMOD1 0,TLOUT                                                          
         L     RC,PPFILEC                                                       
*                                                                               
TLOUT2   DS    0H                                                               
         CLC   QCLIENT,SPACES                                                   
         BE    TLOUT4                                                           
         CLC   PBUYKCLT,OLDCLT                                                  
         BE    TLOUT4                                                           
*                                                                               
         CLI   OLDCLT,0            SEE IF FIRST TIME                            
         BE    TLOUT10                                                          
         GOTO1 ATLPRT                                                           
         CLI   PBUYREC,X'FF'                                                    
         BE    TLOUTX                                                           
*                                                                               
TLOUT2B  DS    0H                                                               
         XC    OLDPRD,OLDPRD                                                    
         XC    OLDEST,OLDEST                                                    
*                                                                               
TLOUT4   DS    0H                                                               
         CLC   QPRODUCT,SPACES                                                  
         BE    TLOUT5                                                           
         CLI   RUNSW,X'21'         PUB/PRD SEQ                                  
         BE    TLOUT5                                                           
         CLC   PBUYKPRD,OLDPRD                                                  
         BE    TLOUT5                                                           
         CLI   OLDPRD,0                                                         
         BE    TLOUT4B                                                          
         GOTO1 ATLPRT                                                           
*                                                                               
TLOUT4B  DS    0H                                                               
         XC    OLDEST,OLDEST                                                    
*                                                                               
TLOUT5   DS    0H                                                               
         CLC   QEST,SPACES                                                      
         BE    TLOUT6                                                           
         CLC   QESTEND,SPACES                                                   
         BNE   TLOUT6                                                           
         CLC   PBUYKEST,OLDEST                                                  
         BE    TLOUT6                                                           
         OC    OLDEST,OLDEST                                                    
         BZ    TLOUT5B                                                          
         GOTO1 ATLPRT              SKIP A LINE                                  
*                                                                               
TLOUT5B  DS    0H                                                               
*                                                                               
TLOUT6   DS    0H                                                               
         CLI   PBUYREC,X'FF'                                                    
         BE    TLOUTX                                                           
         CLC   QPUB,SPACES                                                      
         BE    TLOUT8                                                           
         OC    OLDPUB,OLDPUB       SEE IF FIRST TIME                            
         BZ    TLOUT10                                                          
         CLC   PBUYKPUB(6),OLDPUB                                               
         BE    TLOUT8                                                           
         GOTO1 ATLPRT              SPACE BETWEEN PUBS                           
         B     TLOUT10                                                          
*                                                                               
TLOUT8   DS    0H                                                               
         CLC   QJOB,SPACES                                                      
         BE    TLOUT10                                                          
         CLC   PBDJOB,OLDJOB                                                    
         BE    TLOUT10                                                          
         OC    OLDJOB,OLDJOB       SEE IF FIRST TIME                            
         BZ    TLOUT10                                                          
         GOTO1 ATLPRT                                                           
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                  COUNT LINES NEEDED                           
TLOUT10  DS    0H                                                               
         BRAS  RE,SETLW            SET LEGAL WARNING "FIELDS"                   
         BRAS  RE,LNCNT            COUNT BOTH REGULAR AND WEB I/O               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                  BUILD PRINT LINE                             
         LA    R4,P1                                                            
         USING TLLIND,R4                                                        
         MVC   TLJOB,PBDJOB                                                     
*                                                                               
TLOUT15  OC    PBDMDATE,PBDMDATE   MATERIAL CLOSING DATE                        
         BZ    TLOUT16                                                          
         GOTO1 DATCON,DMCB,(3,PBDMDATE),(5,TLMDAT)                              
*                                                                               
TLOUT16  DS    0H                  CLOSING DATE                                 
         OC    PBDCDATE,PBDCDATE                                                
         BZ    TLOUT18                                                          
         GOTO1 DATCON,DMCB,(3,PBDCDATE),(5,TLCDAT)                              
*                                                                               
***MAT***                                                                       
*                                                                               
TLOUT18  DS    0H                                                               
         MVC   SPAC1(61),SPACES                                                 
         GOTO1 GETINS,DMCB,PBUYREC,GROSS,PBUYKEY+7                              
         TM    PBUYCNTL,X'80'                                                   
         BZ    *+10                                                             
         XC    GROSS,GROSS                                                      
         LA    R6,PPBYOWRK                                                      
         USING PPBYOUTD,R6                                                      
         LA    RF,PBUYREC                                                       
         ST    RF,PBYOINPT                                                      
         LA    RF,GROSS                                                         
         ST    RF,PBYOVALS                                                      
         MVC   PBYODTCN,DATCON                                                  
         MVI   PBYOCTL,X'24'       FOR ZZZ ALLOCATIONS AND I/O COMMS            
         MVI   PBYOCLT2,0                                                       
*                                                                               
         GOTO1 PPBYOUT,DMCB,PPBYOUTD                                            
*                                                                               
         MVC   SAVDAT2,PBYOMDY2    HOLD 2ND INS DATE                            
*                                                                               
         CLI   PBDBFD,C'T'                                                      
         BNE   TLOUT18B                                                         
         MVI   WORK,C'T'                                                        
         TM    PBDSTAT2,X'40'      STEWARD BUY ?                                
         BNO   *+8                 NO                                           
         MVI   WORK,C'S'           REPLACE T WITH S                             
         LA    R1,TLIDAT                                                        
         LA    RF,7                                                             
         CLI   PBYOMDY+3,C'/'                                                   
         BNE   *+8                                                              
         LA    RF,5                                                             
*SMY*    MVI   TLIDAT,C'T'                                                      
         MVC   TLIDAT(1),WORK                                                   
         EX    RF,*+8                                                           
         B     TLOUT18F                                                         
         MVC   1(0,R1),PBYOMDY                                                  
*                                                                               
TLOUT18B MVC   TLIDAT(11),PBYOMDY                                               
         MVC   BYTE,PBDBFD                                                      
         CLI   BYTE,C'B'                                                        
         BE    TLOUT18C                                                         
         CLI   BYTE,C'W'                                                        
         BE    TLOUT18C                                                         
         MVI   BYTE,C' '                                                        
TLOUT18C TM    PBUYCNTL,X'80'                                                   
         BZ    *+8                                                              
         MVI   BYTE,C'D'                                                        
*                                                                               
TLOUT18F LA    RF,TLIDAT+8                                                      
         CLI   0(RF),C' '                                                       
         BE    *+8                                                              
         LA    RF,TLIDAT+11                                                     
         MVC   0(1,RF),BYTE                                                     
*                                                                               
         MVC   SPAC1(14),PBYOGRS                                                
         CLI   QOPT2,C'$'                                                       
         BNE   TLOUT19                                                          
*                                                                               
         B     TLOUT22                                                          
*                                                                               
TLOUT19  DS    0H                                                               
         MVC   SPAC1(40),PBYOSPC                                                
         CLI   QMEDIA,C'N'                                                      
         BNE   TLOUT22                                                          
*                                                                               
         CLI   PBYOSPC,C' '                                                     
         BH    *+10                                                             
         MVC   SPAC1(7),PBYOUNTS   UNITS                                        
         LA    RF,SPAC1+15                                                      
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVC   2(2,RF),PBYOPRM                                                  
*                                                                               
         CLI   PBYOLBC,C' '        LINES X COLS                                 
         BNH   *+10                                                             
         MVC   SPAC2(17),PBYOLBC                                                
*                                                                               
         DROP  R6                                                               
*                                                                               
TLOUT22  DS    0H                                                               
         L     R3,ASINSTAB                                                      
         MVI   0(R3),C' '          CLEAR INSTAB                                 
         MVI   ORDSW,0                                                          
*                                                                               
         BRAS  RE,SETLW            SET LEGAL WARNING "FIELDS"                   
         BRAS  RE,OUTIO            OUTPUT BOTH REGULAR AND WEB I/O DATA         
*                                                                               
*                                                                               
TLOUT24  DS    0H                                                               
         CLI   QOPT5,C'Y'          SEE IF DOUBLE SPACING                        
         BNE   *+18                                                             
         XC    0(34,R3),0(R3)                                                   
         LA    R3,34(R3)                                                        
         MVI   0(R3),C' '                                                       
         CLI   QOPT2,C'$'          $ TOTALS                                     
         BNE   TLOUT24D                                                         
         SR    RF,RF                                                            
         CLI   ORDSW,1             SEE IF ORDERED                               
         BE    *+8                 YES - POST TO ORDERED ACCUM                  
         LA    RF,8                                                             
         LA    R3,ESTTOTS                                                       
         LA    R6,3                                                             
         AR    R3,RF                                                            
*                                                                               
TLOUT24B DS    0H                                                               
         LM    R0,R2,0(R3)                                                      
         A     R0,GROSS                                                         
         LA    R1,1(R1)                                                         
         STM   R0,R2,0(R3)                                                      
         LA    R3,L'ESTTOTS(R3)                                                 
         BCT   R6,TLOUT24B                                                      
*                                                                               
TLOUT24D DS    0H                                                               
         CLC   OLDPUB,PBUYKPUB                                                  
         BNE   TL26                                                             
         BAS   RE,PTLCKHD                                                       
         CLI   FORCEHED,C'Y'                                                    
         BE    TL26                                                             
         MVC   PUBTAB+16(84),SPACES      LEAVE NUMBER                           
         MVC   PUBTAB+100(100),SPACES                                           
         MVC   PUBTAB+200(100),SPACES                                           
         MVC   PUBTAB+300(60),SPACES                                            
         MVI   PUBTABX,X'FF'                                                    
         MVC   PUBTAB+20(2),=C''''''     DITTOS                                 
         MVI   BYTE,1              NEEDED AT TL3200 FOR LINES NEEDED            
         B     TL3200                                                           
*                                                                               
TL26     DS    0H                  GET PUBNAME                                  
         MVC   PUBTAB(100),SPACES                                               
         MVC   PUBTAB+100(100),SPACES                                           
         MVC   PUBTAB+200(100),SPACES                                           
         MVC   PUBTAB+300(60),SPACES                                            
         MVI   PUBTABX,X'FF'                                                    
         IC    R3,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R3),PBUYKPUB),(C'S',PUBT1)                        
         MVC   PUBT1+16(20),PUBNAME     NAME & ZONE NAME                        
         MVC   PUBT2+16(20),PUBZNAME                                            
         CLI   PAGYKMED,C'N'                                                    
         BNE   TL30                                                             
*                                                                               
         LA    RF,PUBT2+16                                                      
         CLI   PUBT2+16,C' '                                                    
         BNH   *+8                                                              
         LA    RF,PUBT3+16                                                      
         MVC   0(16,RF),PUBCITY                                                 
         LA    RF,16(RF)                                                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C','                                                       
         MVC   3(2,RF),PUBSTATE                                                 
*                                                                               
TL30     DS    0H                                                               
         OC    PUBT1(72),SPACES                                                 
*                                                                               
TL31     DS    0H                                                               
         LA    RF,1                                                             
         CLI   PUBT2+16,C' '                                                    
         BNH   *+8                                                              
         LA    RF,1(RF)                                                         
         CLI   PUBT3+16,C' '                                                    
         BNH   *+8                                                              
         LA    RF,1(RF)                                                         
         STC   RF,BYTE                                                          
         CLC   BYTE,LINENEED                                                    
         BNH   *+10                                                             
         MVC   LINENEED,BYTE                                                    
         CLI   QOPT6,C'S'          SHIPPING ADDRESS                             
         BE    TL32                                                             
         CLI   QOPT6,C'Y'          TRAFFIC ADDRESS                              
         BNE   TL3200                                                           
TL32     LA    R3,PUBT3                                                         
         CLC   PUBT3,SPACES                                                     
         BE    *+8                                                              
         LA    R3,PUBT4                                                         
         MVI   0(R3),0             SKIP A LINE                                  
         LA    R3,36(R3)                                                        
         CLI   PREPREC,0                                                        
         BNE   TL32AF                                                           
         MVC   0(17,R3),=C'**DIRECT TO PUB**'                                   
         MVC   36(30,R3),PUBLINE1                                               
         MVC   72(30,R3),PUBLINE2                                               
         MVI   108(R3),X'FF'       SET END OF PUBTAB                            
         LA    RF,3(RF)                                                         
         B     TL3200                                                           
*                                                                               
TL32AF   MVC   0(30,R3),PREPNAME                                                
         ZIC   RF,BYTE                                                          
         LA    RF,1(RF)                                                         
         LA    R3,36(R3)                                                        
         CLC   PREPLIN1(30),SPACES                                              
         BNH   *+18                                                             
         MVC   0(30,R3),PREPLIN1                                                
         LA    RF,1(RF)                                                         
         LA    R3,36(R3)                                                        
         CLC   PREPLIN2(30),SPACES                                              
         BNH   *+18                                                             
         MVC   0(30,R3),PREPLIN2                                                
         LA    RF,1(RF)                                                         
         LA    R3,36(R3)                                                        
         MVC   02(30,R3),PREPLIN2                                               
         CLC   PREPATTN(20),SPACES                                              
         BNH   *+18                                                             
         MVC   0(20,R3),PREPATTN                                                
         LA    RF,1(RF)                                                         
         LA    R3,36(R3)                                                        
         CLC   PREPTEL(12),SPACES                                               
         BNH   *+18                                                             
         MVC   0(12,R3),PREPTEL                                                 
         LA    RF,1(RF)                                                         
         LA    R3,36(R3)                                                        
         MVI   0(R3),X'FF'         SET END OF PUBTAB                            
         STC   RF,BYTE                                                          
         CLC   BYTE,LINENEED                                                    
         BNH   *+10                                                             
         MVC   LINENEED,BYTE                                                    
*                                                                               
TL3200   DS    0H                                                               
         CLI   QOPT5,C'Y'                                                       
         BNE   TL3200F                                                          
         LA    R3,PUBTAB                                                        
TL3200B  CLC   0(36,R3),SPACES                                                  
         BE    TL3200D                                                          
         CLI   0(R3),X'FF'         OR END OF PUBTAB                             
         BE    TL3200D                                                          
         LA    R3,36(R3)                                                        
         B     TL3200B                                                          
*                                                                               
TL3200D  MVI   0(R3),0                                                          
         MVI   36(R3),X'FF'        SET END OF PUBTAB                            
*                                                                               
TL3200F  MVC   COMTAB(44),SPACES   INITIALIZE COMMENT TABLE                     
         LA    R3,COMTAB                                                        
         ZIC   RF,BYTE                                                          
         CLI   SAVDAT2,C' '        TEST 2ND INS DATE                            
         BNH   TL3201                                                           
         LA    RF,1(RF)                                                         
         MVC   0(44,R3),SPACES                                                  
         MVI   0(R3),C'+'                                                       
         MVC   1(8,R3),SAVDAT2                                                  
         LA    R3,44(R3)                                                        
*                                                                               
TL3201   DS    0H                                                               
         LA    R2,PBUYREC+33       FIND REF ELEMENT                             
         MVI   ELCODE,X'83'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   TL32010                                                          
         ZIC   R1,1(R2)                                                         
         AHI   R1,-3               ELCODE + LEN + 1 FOR EX                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   2(0,R2),SPACES                                                   
         BNH   TL32010                                                          
         MVC   0(44,R3),SPACES                                                  
         MVC   0(4,R3),=C'REF='                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R3),2(R2)                                                    
         LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
*                                                                               
TL32010  LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'82'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   TL320102                                                         
         USING PBFSIELD,R2                                                      
         CP    PBFSI,=P'0'                                                      
         BE    TL320102                                                         
         MVC   0(44,R3),SPACES                                                  
         MVC   0(4,R3),=C'FSI='                                                 
         EDIT  PBFSI,(8,4(R3)),0,ALIGN=LEFT                                     
         LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
*                                                                               
         DROP  R2                                                               
*                                                                               
TL320102 DS    0H                                                               
         TM    PBDSTAT,X'04'       SFH BUY ?                                    
         BNO   TL3201A             NO                                           
         MVC   0(44,R3),SPACES                                                  
         MVC   0(8,R3),=C'SFH=HOLD'                                             
         TM    PBDSTAT,X'08'       BUY HELD ?                                   
         BO    *+10                YES                                          
         MVC   4(7,R3),=C'RELEASE' REPLACE "HOLD"                               
         LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
*                                                                               
TL3201A  DS    0H                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'86'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   TL3201AB                                                         
         USING PBSHPDEL,R2                                                      
         OC    PBSHDATE,PBSHDATE                                                
         BZ    TL3201AB                                                         
         MVC   0(44,R3),SPACES                                                  
         MVC   0(10,R3),=C'SHIP DATE='                                          
         ST    RF,FULL                                                          
         GOTO1 DATCON,DMCB,(3,PBSHDATE),(5,10(R3))                              
         L     RF,FULL                                                          
         LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
         DROP  R2                                                               
*                                                                               
* EXTENSION DAYS=NNN (12/30/98)                                                 
*                                                                               
TL3201AB DS    0H                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'89'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   TL3201AH                                                         
         USING PEXDAYEL,R2                                                      
         CP    PEXDAYS,=P'0'                                                    
         BE    TL3201AH                                                         
         MVC   0(44,R3),SPACES                                                  
         MVC   0(15,R3),=C'EXTENSION DAYS='                                     
         EDIT  PEXDAYS,(3,15(R3)),0,ALIGN=LEFT                                  
         LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
         DROP  R2                                                               
*                                                                               
* EXTENSION DATE=MMMDD/YY (01/31/01)                                            
*                                                                               
TL3201AH DS    0H                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'96'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   TL3201AP                                                         
         USING PEXDATEL,R2                                                      
         MVC   0(44,R3),SPACES                                                  
         MVC   0(15,R3),=C'EXTENSION DATE='                                     
         ST    RF,FULL                                                          
         GOTO1 DATCON,DMCB,(3,PEXDATE),(5,15(R3))                               
         L     RF,FULL                                                          
         LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
         DROP  R2                                                               
*                                                                               
TL3201AP DS    0H                  FOR FUTURE USES                              
*                                                                               
TL3201AX DS    0H                                                               
         TM    PBUYCNTL,X'80'      FIRST CHECK DELETED                          
         BZ    TL3201B                                                          
         MVC   0(44,R3),SPACES                                                  
         MVC   0(9,R3),=C'*DELETED*'                                            
         CLI   IOSW,C'N'           SEE IF NO I/O WAS PRODUCED                   
         BNE   TL3209                                                           
         MVC   10(20,R3),=C'- NO CAN I/O NEEDED*'                               
         B     TL3209                                                           
*                                                                               
TL3201B  CLC   PBDBUYDT,ACTDATE    SEE IF ADDED TODAY                           
         BNE   TL3201C                                                          
         MVC   0(44,R3),SPACES                                                  
         MVC   0(5,R3),=C'*NEW*'                                                
         TM    CHGSW2,X'04'        SEE IF MADE LIVE                             
         BZ    *+10                                                             
         MVC   0(10,R3),=C'*APPROVED*'                                          
         LA    R3,44(R3)                                                        
         LA    RF,1(RF)                                                         
*                                                                               
TL3201C  CLI   CHGSW,0                                                          
         BE    TL3205              NO CHGS SINCE LAST IO                        
         MVC   0(44,R3),SPACES                                                  
         LA    RE,0(R3)                                                         
         MVI   0(RE),C'*'                                                       
         LA    RE,1(RE)                                                         
         TM    CHGSW,X'40'         CHK FOR SPACE CHG                            
         BZ    TL3202              NO                                           
         MVC   0(6,RE),=C'SPACE,'                                               
         LA    RE,6(RE)                                                         
*                                                                               
TL3202   TM    CHGSW,X'20'         JOB CHG                                      
         BZ    TL3203              NO                                           
         MVC   0(4,RE),=C'JOB,'                                                 
         LA    RE,4(RE)                                                         
*                                                                               
TL3203   TM    CHGSW,X'10'         DATE CHG                                     
         BZ    TL3203C                                                          
         MVC   0(5,RE),=C'DATE,'                                                
         LA    RE,5(RE)                                                         
*                                                                               
TL3203C  TM    CHGSW,X'80'         IC COMMENT CHANGE                            
         BZ    TL3203F                                                          
         MVC   0(07,RE),=C'IC COM,'                                             
         LA    RE,7(RE)                                                         
*                                                                               
TL3203F  TM    CHGSW,X'01'         POSITION INSTRUCTIONS CHANGE                 
         BZ    TL3203H                                                          
         MVC   0(08,RE),=C'POS.INS,'                                            
         LA    RE,8(RE)                                                         
*                                                                               
TL3203H  TM    CHGSW,X'02'         RATE CHANGE                                  
         BZ    TL3204                                                           
         MVC   0(05,RE),=C'RATE,'                                               
         LA    RE,5(RE)                                                         
*                                                                               
TL3204   BCTR  RE,0                                                             
         CLI   0(RE),C','                                                       
         BNE   *+8                                                              
         MVI   0(RE),C' '          BLANK LAST COMMA                             
         MVC   1(4,RE),=C'CHG*'                                                 
         LA    R3,44(R3)                                                        
         LA    RF,1(RF)                                                         
*                                                                               
TL3205   DS    0H                                                               
         CLI   CHGSW2,0                                                         
         BE    TL32A0                                                           
         CLI   CHGSW2,X'04'        OR ONLY MADE LIVE                            
         BE    TL32A0                                                           
         MVC   0(44,R3),SPACES                                                  
*                                                                               
         TM    CHGSW2,X'08'        SEE IF ADCODE ADDED-ON SEPERATE LINE         
         BZ    TL3205C                                                          
         MVC   0(11,R3),=C'*JOB ADDED*'                                         
         LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
         MVC   0(44,R3),SPACES                                                  
         CLI   CHGSW2,X'08'        AD CODE ADDED ONLY THEN DONE                 
         BE    TL32A0                                                           
*                                                                               
TL3205C  LA    RE,0(R3)                                                         
         MVI   0(RE),C'*'                                                       
         LA    RE,1(RE)                                                         
         TM    CHGSW2,X'80'        CLOSING DATE                                 
         BZ    TL3206              NO                                           
         MVC   0(08,RE),=C'CL DATE,'                                            
         LA    RE,08(RE)                                                        
*                                                                               
TL3206   TM    CHGSW2,X'02'        MATERIALS CLOSING DATE                       
         BZ    TL3207              NO                                           
         MVC   0(12,RE),=C'MAT CL DATE,'                                        
         LA    RE,12(RE)                                                        
*                                                                               
TL3207   TM    CHGSW2,X'01'        IO DATE                                      
         BZ    TL3208              NO                                           
         MVC   0(08,RE),=C'I/O DATE'                                            
         LA    RE,8(RE)                                                         
*                                                                               
TL3208   BCTR  RE,0                                                             
         CLI   0(RE),C','                                                       
         BNE   *+8                                                              
         MVI   0(RE),C' '          BLANK LAST COMMA                             
         MVC   1(4,RE),=C'CHG*'                                                 
TL3209   LA    R3,44(R3)                                                        
         LA    RF,1(RF)                                                         
*                                                                               
TL32A0   DS    0H                                                               
         CLI   QOPT4,C'1'                                                       
         BL    TL32A4              NOT PRINTING COPY OR CAPTION                 
*                                                                               
* GET COPY/CAPTION OVERRIDES RETURNED IN MYCOPY AND MYCAP1+2                    
* IF SPACES - NO OVERRIDE                                                       
*                                                                               
         ST    RF,FULL        SAVE RF                                           
         GOTO1 =A(GETCCAP),DMCB,RR=Y                                            
         L     RF,FULL        RESTORE RF                                        
*                                                                               
         MVI   NOJOB,C'Y'                                                       
         OC    PBDJOB,PBDJOB                                                    
         BZ    TL32A2              NO JOB NUMBER                                
         MVI   NOJOB,C'N'                                                       
         ST    RF,FULL                                                          
         MVC   SAVKEYS,KEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),PAGYKAGY                                                  
         MVC   KEY+2(1),PAGYKMED                                                
         MVI   KEY+3,X'15'                                                      
         MVC   KEY+4(3),PBUYKCLT                                                
         MVC   KEY+7(3),PBUYKPRD                                                
         MVC   KEY+10(6),PBDJOB                                                 
         CLC   PJOBREC(17),KEY                                                  
         BE    TL32A               HAVE JOB REC                                 
         GOTO1 HIGH                                                             
         CLC   KEY(17),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                JOB NOT ON FILE                              
         LA    R0,PJOBREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
TL32A    MVC   KEY(64),SAVKEYS     RESTORE KEYS                                 
         GOTO1 HIGH                RESTORE FOR SEQ READ                         
         L     RF,FULL             RESTORE RF - COUNTER                         
*                                                                               
*SMY*    CLI   PBDJOB,X'FF'        "AD-ID ONLY" JOB CODE ?                      
*SMY*    BE    TL32A1              YES                                          
*                                                                               
*SMY*    CLI   PROGPROF+08,C'Y'    REPLACE AD CODE WITH AD ID ?                 
*SMY*    BNE   TL32A2              NO                                           
*                                                                               
TL32A1   CLI   PJOBADID,C' '       AD ID THERE ?                                
         BNH   TL32A2              NO                                           
*                                                                               
         MVC   TLJOB,=C'*ADID*'    REPLACE PBDJOB                               
*                                                                               
         MVC   0(44,R3),SPACES                                                  
         MVC   0(6,R3),=C'Ad-ID='                                               
         MVC   6(L'PJOBADID,R3),PJOBADID                                        
         LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
*                                                                               
TL32A2   MVC   0(44,R3),SPACES                                                  
         CLC   MYCOPY,SPACES       CHECK FOR OVERRIDE                           
         BE    TL32A2C                                                          
         MVC   0(6,R3),=C'COPY ='                                               
         MVC   6(17,R3),MYCOPY                                                  
         LA    R3,44(R3)                                                        
         LA    RF,1(RF)                                                         
         B     TL32A2G                                                          
*                                                                               
TL32A2C  CLI   NOJOB,C'Y'                                                       
         BE    TL32A2G                                                          
         MVC   0(6,R3),=C'COPY ='                                               
         MVC   6(17,R3),PJOBCPY                                                 
         LA    R3,44(R3)                                                        
         LA    RF,1(RF)                                                         
*                                                                               
TL32A2G  CLI   QOPT4,C'2'                                                       
         BNE   TL32A4              NO CAPTIONS                                  
         MVC   0(44,R3),SPACES                                                  
         CLC   MYCAP1,SPACES       CHECK FOR OVERRIDE                           
         BE    TL32A2M                                                          
         MVC   0(8,R3),=C'CAPTION='                                             
         MVC   8(25,R3),MYCAP1                                                  
         LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
         MVC   0(44,R3),SPACES                                                  
         MVC   8(25,R3),MYCAP2                                                  
         CLC   0(44,R3),SPACES                                                  
         BE    TL32A4                                                           
         MVC   0(8,R3),=C'CAPTION='                                             
         LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
         B     TL32A4                                                           
*                                                                               
TL32A2M  CLI   NOJOB,C'Y'                                                       
         BE    TL32A4                                                           
         MVC   0(8,R3),=C'CAPTION='                                             
         MVC   8(25,R3),PJOBCAP1                                                
         LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
         MVC   0(44,R3),SPACES                                                  
         MVC   8(25,R3),PJOBCAP2                                                
         CLC   0(44,R3),SPACES                                                  
         BE    TL32A4                                                           
         MVC   0(8,R3),=C'CAPTION='                                             
         LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
*                                                                               
TL32A4   DS    0H                                                               
         BRAS  RE,SETLW            SET LEGAL WARNING "FIELDS"                   
*                                                                               
         OC    SAVLWCD(3),SAVLWCD  ANY LEGAL WARNING INFO ?                     
         BZ    LEGLW30             NO                                           
*                                                                               
         BRAS  RE,PUTLW            OUTPUT LEGAL WARNING "FIELDS"                
*                                                                               
LEGLW30  DS    0H                                                               
*                                                                               
         LA    R6,PPBYOWRK                                                      
         USING PPBYOUTD,R6                                                      
         CLC   PBUYKPRD,=C'ZZZ'    SEE IF ZZZ BUY                               
         BNE   TL32A6                                                           
         OC    PBYOZZZ,SPACES                                                   
         CLI   PBYOZZZ,C' '                                                     
         BE    TL32A6              NO ALLOCATION                                
         CLC   PBYOZZZ+34(3),SPACES                                             
         BNE   TL32A5              NO, DESN'T FIT ON ONE LINE                   
         MVC   0(44,R3),PBYOZZZ                                                 
         LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
         B     TL32A6                                                           
*                                                                               
TL32A5   LA    R1,33               SET FOR VARIABLE MOVE                        
         LA    R2,PBYOZZZ+34       SCAN BACKWARD FOR ,                          
TL32A5A  CLI   0(R2),C','                                                       
         BE    TL32A5C                                                          
         BCTR  R2,0                                                             
         BCTR  R1,0                                                             
         B     TL32A5A                                                          
*                                                                               
TL32A5C  EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),PBYOZZZ     EXECUTED                                     
         LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
         MVC   0(44,R3),SPACES                                                  
         LA    R1,1(R1)                                                         
         LA    R2,PBYOZZZ                                                       
         AR    R2,R1                                                            
         L     R5,=F'49'           49 BECAUSE OF EXECUTE                        
         SR    R5,R1                                                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),1(R2)       REST OF ALLOCATION LINE                      
*                                  1(R2) TO GET ME PAST ,                       
         LA    R3,44(R3)                                                        
         LA    RF,1(RF)                                                         
*                                                                               
* NOTE THAT IC= FREE FORM COMMENTS WON'T EXCEED 44 CHARACTERS                   
* SINCE 'IC=' IS DROPPED BY PPBYOUT                                             
*                                                                               
TL32A6   LA    R2,PBYOCOMS         IO COMMENTS                                  
         LA    R5,5                FOR BCT                                      
TL32A8   DS    0H                                                               
         CLC   0(44,R2),SPACES                                                  
         BE    TL32A9                                                           
         CLI   PROGPROF+3,C'Y'     SEE IF LISTING STANDARD COMMENTS             
         BNE   TL32A8F             AT END                                       
         CLC   0(4,R2),=C'COM='    CHK FOR STANDARD COMMENT                     
         BNE   TL32A8F                                                          
         ST    RF,FULL                                                          
         GOTO1 BINSRCH,BSPARS,(1,4(R2))                                         
         L     RF,FULL                                                          
*                                                                               
TL32A8F  DS    0H                                                               
         MVC   0(44,R3),0(R2)                                                   
         CLI   0(R3),C' '          PREVENT COMMENT FROM HAVING BLANK            
         BNE   *+8                 IN FIRST POS// CONTROL PROBLEMS              
         MVI   0(R3),0                                                          
         LA    R3,44(R3)                                                        
         LA    RF,1(RF)                                                         
*                                                                               
TL32A8H  DS    0H                                                               
*                                                                               
TL32A9   LA    R2,47(R2)           NEXT IO COMMENT                              
         BCT   R5,TL32A8                                                        
*                                                                               
         MVI   PBYOCLT2,X'80'      FOR POSITION INSTRUCTIONS                    
         MVI   PBYOCTL,0                                                        
*                                                                               
         ST    RF,FULL                                                          
         GOTO1 PPBYOUT,DMCB,PPBYOUTD                                            
         L     RF,FULL                                                          
*                                                                               
* NOTE THAT IC= FREE FORM COMMENTS WON'T EXCEED 44 CHARACTERS                   
* SINCE 'IC=' IS DROPPED BY PPBYOUT                                             
*                                                                               
         CLC   PBYOCOMS(94),SPACES SEE IF ANY EXIST                             
         BE    TL32PX                                                           
         XC    0(44,R3),0(R3)      TO SKIP A LINE                               
         LA    R3,44(R3)                                                        
         MVC   0(44,R3),SPACES                                                  
         LA    RF,1(RF)                                                         
         MVC   0(27,R3),=C'***POSITION INSTRUCTIONS***'                         
         LA    R3,44(R3)                                                        
         LA    RF,1(RF)                                                         
*                                                                               
TL32P6   LA    R2,PBYOCOMS                                                      
         LA    R5,5                FOR BCT                                      
*                                                                               
TL32P8   DS    0H                                                               
         CLC   0(44,R2),SPACES                                                  
         BE    TL32P9                                                           
         CLI   PROGPROF+3,C'Y'     SEE IF LISTING STANDARD COMMENTS             
         BNE   TL32P8F             AT END                                       
         CLC   0(4,R2),=C'COM='    CHK FOR STANDARD COMMENT                     
         BNE   TL32P8F                                                          
         ST    RF,FULL                                                          
         GOTO1 BINSRCH,BSPARS,(1,4(R2))                                         
         L     RF,FULL                                                          
*                                                                               
TL32P8F  DS    0H                                                               
         MVC   0(44,R3),0(R2)                                                   
*                                                                               
         CLI   0(R3),C' '          PREVENT COMMENT FROM HAVING BLANK            
         BNE   *+8                 IN FIRST POS// CONTROL PROBLEMS              
         MVI   0(R3),0                                                          
         LA    R3,44(R3)                                                        
         LA    RF,1(RF)                                                         
TL32P8H  DS    0H                                                               
*                                                                               
TL32P9   LA    R2,47(R2)           NEXT PI COMMENT                              
         BCT   R5,TL32P8                                                        
         DROP  R6                                                               
*                                                                               
TL32PX   DS    0H                                                               
*                                                                               
TL32AX   DS    0H                                                               
         CLI   QOPT5,C'Y'                                                       
         BNE   TL32AX2                                                          
         XC    0(44,R3),0(R3)       SO I'LL SKIP BETWEEN INSERTIONS             
         LA    R3,44(R3)                                                        
TL32AX2  MVC   0(44,R3),SPACES                                                  
*                                                                               
TL32AX5  DS 0H                                                                  
         STC   RF,BYTE                                                          
         CLC   BYTE,LINENEED                                                    
         BNH   *+10                                                             
         MVC   LINENEED,BYTE                                                    
*                                                                               
         LA    R3,PUBTAB                                                        
         LA    R2,COMTAB                                                        
         LA    R6,SPAC1                                                         
         L     R5,ASINSTAB                                                      
TL32B    DS    0H                                                               
         CLI   0(R3),X'FF'                                                      
         BE    TL33                NO MORE PUBNAME                              
         MVC   TLPUB(36),0(R3)                                                  
         LA    R3,36(R3)                                                        
*                                                                               
TL33     DS    0H                                                               
         CLC   0(20,R6),SPACES     NO MORE SPACE DESC                           
         BNH   TL33B                                                            
         MVC   TLSPACE,0(R6)                                                    
         LA    R6,20(R6)                                                        
         CLC   0(44,R2),SPACES     END OF TABLE                                 
         BE    TL33F                                                            
         CLC   25(19,R2),SPACES    SEE IF COMMENT WILL FIT ON SAME LINE         
         BH    TL33F               NO                                           
         CLI   TLIDAT,C' '         IF NOTHING ALREADY THERE                     
         BNE   TL33F                                                            
         MVC   TLIDAT(25),0(R2)                                                 
         LA    R2,44(R2)                                                        
         B     TL33F                                                            
*                                                                               
TL33B    DS    0H                                                               
         CLC   0(44,R2),SPACES     END OF TABLE                                 
         BE    TL33F                                                            
         CLI   TLIDAT,C' '         SEE IF SOMETHING ALREADY THERE               
         BNE   TL33F               YES                                          
         MVC   TLIDAT(44),0(R2)                                                 
         LA    R2,44(R2)                                                        
*                                                                               
TL33F    DS    0H                                                               
         CLI   0(R5),C' '                                                       
         BE    TL34                NO MORE IO'S                                 
         MVC   TLIODAT(34),0(R5)                                                
         LA    R5,34(R5)                                                        
*                                                                               
TL34     DS    0H                                                               
         CLC   P1,SPACES                                                        
         BE    TLOUTX              NO MORE TO PRINT                             
*                                                                               
         GOTO1 ATLPRT                                                           
         B     TL32B                                                            
*                                                                               
TLOUTX   DS    0H                                                               
         MVC   OLDKEY,PBUYKEY                                                   
         MVC   OLDJOB,PBDJOB                                                    
         CLI   PBUYKEY,X'FF'       AT LAST BUY REQ                              
         BNE   TLXIT                                                            
         XC    OLDKEY,OLDKEY                                                    
         XC    OLDJOB,OLDJOB                                                    
TLXIT    XIT1                                                                   
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
NEXTEL   DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    NEXTEL2                                                          
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     NEXTEL+2                                                         
NEXTEL2  LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PTLCKHD  DS    0H                                                               
         CLI   FORCEHED,C'Y'                                                    
         BER   RE                                                               
*                                                                               
         SR    R0,R0                                                            
         SR    RF,RF                                                            
         IC    R0,LINE                                                          
         IC    RF,LINENEED                                                      
         AR    R0,RF                                                            
         STC   R0,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BLR   RE                                                               
         MVI   FORCEHED,C'Y'                                                    
         BR    RE                                                               
*                                                                               
ORDWRDS  DC    CL9'  ORDERED'                                                   
         DC    CL9'UNORDERED'                                                   
         DC    CL9'    TOTAL'                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*    COUNT LINES NEEDED FOR BOTH REGULAR AND WEB INSERTION ORDERS     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
LNCNT    NTR1  BASE=*,LABEL=*                                                   
         LA    R4,1                                                             
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'70'        REGULAR INSERTION ORDERS                     
         USING PIOELEM,R2                                                       
LNCNT12  DS    0H                                                               
         BAS   RE,LNEXTEL                                                       
         BNE   LNCNT20                                                          
         LA    R4,1(R4)                                                         
*                                                                               
         OC    PIOLWCD(2),PIOLWCD  ANY LEGAL WARNING ENTRIES                    
         BZ    LNCNT13             NO                                           
         CLC   PIOLWCD(2),SAVLWCD  LW USED=PRD OR BUY LW ?                      
         BE    LNCNT13             YES                                          
         LA    R4,1(R4)            ANOTHER LINE IF "LEGAL WARNING USED          
*****    B     LNCNT13             DIFFERS FROM DEFAULT OR OVERRIDE LW"         
LNCNT13  DS    0H                                                               
* CHECK FOR "REPEAT" PUB HERE                                                   
*                                                                               
         CLI   1(R2),X'32'         PIOELEM GT 50?                               
         BNH   LNCNT12             NO                                           
         OC    PIORPUB,PIORPUB     ANY DATA HERE?                               
         BZ    LNCNT12             NO                                           
         LA    R4,1(R4)            ANOTHER LINE FOR "REPEAT" PUB                
         B     LNCNT12                                                          
*                                                                               
         DROP  R2                                                               
*                                  NOW CHECK WEBIO                              
LNCNT20  DS    0H                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'71'        WEB INSERTION ORDERS                         
         USING PWIOELD,R2                                                       
LNCNT22  DS    0H                                                               
         BAS   RE,LNEXTEL                                                       
         BNE   LNCNTX              DONE                                         
         LA    R4,1(R4)                                                         
*                                                                               
         OC    PWIOLWCD(2),PWIOLWCD  ANY LEGAL WARNING ENTRIES                  
         BZ    LNCNT23               NO                                         
         CLC   PWIOLWCD(2),SAVLWCD   LW USED=PRD OR BUY LW ?                    
         BE    LNCNT23               YES                                        
         LA    R4,1(R4)            ANOTHER LINE IF "LEGAL WARNING USED          
*****    B     LNCNT23             DIFFERS FROM DEFAULT OR OVERRIDE LW"         
LNCNT23  DS    0H                                                               
* CHECK FOR "REPEAT" PUB HERE                                                   
*NOP*    OC    PWIORPUB,PWIORPUB   ANY DATA HERE?                               
*NOP*    BZ    LNCNT22             NO                                           
*NOP*    LA    R4,1(R4)            ANOTHER LINE FOR "REPEAT" PUB                
         B     LNCNT22                                                          
*                                                                               
         DROP  R2                                                               
*                                                                               
LNCNTX   DS    0H                                                               
         STC   R4,LINENEED                                                      
         XIT1                                                                   
*                                                                               
LNEXTEL  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    LNEXTEL2                                                         
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     LNEXTEL+2                                                        
LNEXTEL2 LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*          OUTPUT INSERTION ORDER DATA - REGULAR AND WEB              *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
OUTIO    NTR1  BASE=*,LABEL=*                                                   
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'70'        REGULAR I/O ELEM                             
OUTIO22B DS    0H                                                               
         BAS   RE,ONEXTEL                                                       
         BNE   OUTIO30                                                          
*                                                                               
         USING PIOELEM,R2                                                       
         OC    PIODATE,PIODATE                                                  
         BZ    OUTIO22B                                                         
         OI    ORDSW,1             SET ON ORDERED SWITCH                        
*                                                                               
         MVC   0(34,R3),SPACES                                                  
         GOTO1 DATCON,DMCB,(3,PIODATE),(5,0(R3))                                
*                                                                               
         OC    PIORPTDT,PIORPTDT                                                
         BZ    OUTIO22D                                                         
         GOTO1 DATCON,DMCB,(3,PIORPTDT),(5,26(R3))                              
*                                                                               
OUTIO22D DS    0H                                                               
         LA    R5,13(R3)                                                        
         MVC   0(1,R5),PAGYKMED                                                 
         MVI   1(R5),C'-'                                                       
         GOTO1 DATCON,DMCB,(3,PIODATE),(0,WORK)                                 
*                                                                               
         MVC   2(5,R5),WORK+1                                                   
         MVI   7(R5),C'-'                                                       
         MVC   HALF,PIONUM                                                      
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(4,R5),DUB                                                      
*                                                                               
         MVC   9(3,R3),=C'ORI'                                                  
         CLI   PIOTYP,C'N'                                                      
         BE    OUTIO22F                                                         
         MVC   9(3,R3),=C'CHG'                                                  
         CLI   PIOTYP,C'C'                                                      
         BE    OUTIO22F                                                         
         MVC   9(3,R3),=C'CAN'                                                  
*                                                                               
OUTIO22F DS    0H                                                               
**** OUTPUT LEGAL WARNING PRINTED=XX AND REPEAT PUB HERE *****                  
         BRAS  RE,PUTLW2                                                        
*                                                                               
OUTIO22H DS    0H                                                               
         LA    R3,34(R3)           NEXT ENTRY                                   
         MVI   0(R3),C' '                                                       
         B     OUTIO22B                                                         
*                                                                               
OUTIO30  DS    0H                  NOW CHECK FOR WEB INSERTION ORDERS           
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'71'        WEB I/O ELEM                                 
OUTIO32B DS    0H                                                               
         BAS   RE,ONEXTEL                                                       
         BNE   OUTIOX              DONE                                         
*                                                                               
         USING PWIOELD,R2                                                       
         OC    PWIODATE,PWIODATE                                                
         BZ    OUTIO32B                                                         
         OI    ORDSW,1             SET ON ORDERED SWITCH                        
*                                                                               
         MVC   0(34,R3),SPACES                                                  
         GOTO1 DATCON,DMCB,(3,PWIODATE),(5,0(R3))                               
*                                                                               
*NOP*    OC    PWIORPDT,PWIORPDT                                                
*NOP*    BZ    OUTIO32D                                                         
*NOP*    GOTO1 DATCON,DMCB,(3,PWIORPDT),(5,26(R3))                              
*                                                                               
OUTIO32D DS    0H                                                               
         LA    R5,9(R3)                 FROM PPREP7702                          
         MVC   0(1,R5),PAGYKMED                                                 
*                                                                               
         AHI   R5,1                BUMP TO NEXT POSITION                        
*                                                                               
         SR    RF,RF                                                            
         IC    RF,PWIO#YER         GET IO# YEAR                                 
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  0(2,R5),DUB         SET YEAR                                     
*                                                                               
         AHI   R5,2                BUMP TO CLIENT PART                          
         MVC   0(3,R5),PBUYKCLT    SET CLIENT                                   
         AHI   R5,2                BUMP POINTER                                 
         CLI   0(R5),C' '          IF EMPTY                                     
         BH    *+8                                                              
         MVI   0(R5),C'-'             FILL IN WITH DASH                         
*                                                                               
         AHI   R5,1                 BUMP PAST LAST OF CODE                      
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,7,PWIO#SQ#       GET SEQUENCE NUMBER                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  0(4,R5),DUB         SEQUENCE NUMBER                              
*                                                                               
         AHI   R5,4                NEXT OUTPUT AREA                             
*                                                                               
         OC    PWIO#REV,PWIO#REV   SKIP IF NO REVISION NUMBER                   
         BZ    OUTIO32F            DONE WITH WEBIO NUMBER                       
*                                                                               
         MVC   0(3,R5),=C'REV'     REVISION INDICATOR                           
         AHI   R5,3                                                             
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,PWIO#REV       GET REVISION NUMBER                          
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  0(3,R5),DUB         SEQUENCE NUMBER                              
*                                                                               
*                                                                               
OUTIO32F DS    0H                                                               
**** OUTPUT LEGAL WARNING PRINTED=XX AND REPEAT PUB HERE *****                  
         BRAS  RE,PUTLW3                                                        
*                                                                               
OUTIO32H DS    0H                                                               
         LA    R3,34(R3)           NEXT ENTRY                                   
         MVI   0(R3),C' '                                                       
         B     OUTIO32B                                                         
*                                                                               
OUTIOX   DS    0H                                                               
         XIT1  REGS=(R3)           RETURN R3 "AS IS"                            
*                                                                               
*                                                                               
ONEXTEL  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    ONEXTEL2                                                         
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     ONEXTEL+2                                                        
ONEXTEL2 LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETCCAP  NTR1  BASE=*,LABEL=*      GET COPY AND CAPTION OVERRIDES               
*                                                                               
         MVC   MYCOPY,SPACES                                                    
         MVC   MYCAP1,SPACES                                                    
         MVC   MYCAP2,SPACES                                                    
         MVI   MYCAPEND,X'FF'                                                   
         LA    R4,MYCAP1                                                        
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'66'                                                     
GETCC5   BAS   RE,GNEXTEL                                                       
         BNE   GETCCX                                                           
         CLC   2(5,R2),=C'COPY='                                                
         BNE   GETCC10                                                          
         ZIC   R1,1(R2)                                                         
         AHI   R1,-8                                                            
         BNP   GETCC5                                                           
         CHI   R1,16                                                            
         BNH   *+8                                                              
         LHI   R1,16                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   MYCOPY(0),7(R2)                                                  
         B     GETCC5                                                           
*                                                                               
GETCC10  CLC   2(4,R2),=C'CAP='                                                 
         BNE   GETCC5                                                           
         ZIC   R1,1(R2)                                                         
         AHI   R1,-7                                                            
         BNP   GETCC5                                                           
         CLC   0(25,R4),SPACES                                                  
         BNE   GETCC5              TOO MANY CAP= LINES JUST BYPASS              
*                                                                               
         CHI   R1,24               JUST IN CASE                                 
         BNH   *+8                                                              
         LHI   R1,24                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),6(R2)                                                    
         LA    R4,25(R4)           BUMP TO NEXT CAPTION LINE                    
         B     GETCC5                                                           
*                                                                               
GETCCX   XIT1                                                                   
*                                                                               
***********************************************************************         
*                                                                               
GNEXTEL  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    GNEXTEL2                                                         
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     GNEXTEL+2                                                        
GNEXTEL2 LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* SET UP LEGAL WARNING DEFAULT AND OVERRIDE DATA IN WORKING STORAGE   *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
SETLW    NTR1  BASE=*,LABEL=*                                                   
         XC    SAVLWCD(3),SAVLWCD                                               
         LA    R2,PBUYREC+33       LOOK FOR LEGAL WARN OVERRIDE                 
         MVI   ELCODE,X'94'                                                     
         BAS   RE,SNEXTEL          LEGAL WARNING ELEM FOUND IN BUY?             
         JNE   SETLW20             NO, CHECK IN PRODUCT RECORD                  
*                                                                               
         MVC   SAVLWCD(2),2(R2)    LW CODE AND QUARTERLY CODE                   
         MVI   SAVLWTYP,C'O'       LEGAL WARNING "OVERRIDE" FROM BUYREC         
         J     SETLWXT             DONE                                         
*                                                                               
*                                                                               
SETLW20  DS    0H                                                               
         BRAS  RE,GETPRD                                                        
         LA    R2,PPRDREC+33                                                    
         MVI   ELCODE,X'40'        LEGAL WARNING ELEM CODE IN PRD               
         BAS   RE,SNEXTEL                                                       
         JNE   SETLWXT             NO LW CODES FOUND IN THIS ROUTINE            
*                                                                               
         MVI   SAVLWTYP,C'D'       LEGAL WARNING "DEFAULT" FROM PRDREC          
*                                                                               
         CLI   PBUYKDAT+1,X'03'    MONTH IS GREATER THAN MARCH ?                
         JH    SETLW22             YES                                          
         MVC   SAVLWCD,2(R2)                                                    
         MVI   SAVLWQC,C'1'        QUARTER 1                                    
         J     SETLWXT             DONE                                         
SETLW22  CLI   PBUYKDAT+1,X'06'    MONTH IS GREATER THAN JUNE ?                 
         JH    SETLW24             YES                                          
         MVC   SAVLWCD,3(R2)                                                    
         MVI   SAVLWQC,C'2'        QUARTER 2                                    
         J     SETLWXT             DONE                                         
SETLW24  CLI   PBUYKDAT+1,X'09'    MONTH IS GREATER THAN SEPTEMBER ?            
         JH    SETLW26             YES                                          
         MVC   SAVLWCD,4(R2)                                                    
         MVI   SAVLWQC,C'3'        QUARTER 3                                    
         J     SETLWXT             DONE                                         
SETLW26  DS    0H                  MONTH IS GREATER THAN SEPTEMBER              
         MVC   SAVLWCD,5(R2)                                                    
         MVI   SAVLWQC,C'4'        QUARTER 4                                    
*****    J     SETLWXT             DONE                                         
SETLWXT  DS    0H                                                               
         LA    R0,PBUYREC          RESTORE AREC                                 
         ST    R0,AREC                                                          
         XIT1                                                                   
*                                                                               
***********************************************************************         
*                                                                               
SNEXTEL  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    SNEXTEL2                                                         
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     SNEXTEL+2                                                        
SNEXTEL2 LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*       OUTPUT LEGAL WARNING DEFAULT AND OVERRIDE DATA                *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PUTLW    NTR1  BASE=*,LABEL=*                                                   
         MVC   0(44,R3),SPACES                                                  
         MVC   0(14,R3),=C'LEGAL WARNING '                                      
         MVC   14(9,R3),=C'DEFAULT= '                                           
         CLI   SAVLWTYP,C'D'                                                    
         BE    *+10                                                             
         MVC   14(9,R3),=C'OVERRIDE='                                           
         MVC   23(2,R3),SAVLWCD    LW CODE AND QUARTERLY CODE                   
*                                                                               
         CLI   SAVLWCD,C'X'        LEGAL WARNING CODE IS X? (NONE)              
         BNE   PUTLW10                                                          
         MVC   23(4,R3),=C'NONE'   REPLACE LW AND QUARTERLY CODES               
         B     PUTLW20             DONE - BUMP TO NEXT LINE                     
*                                                                               
PUTLW10  CLI   SAVLWQC,C'X'        QUARTERLY CODE IS X?  (NONE)                 
         BNE   PUTLW20             DONE - BUMP TO NEXT LINE                     
         MVC   24(5,R3),=C' NONE'  REPLACE QUARTERLY CODE                       
*****    B     PUTLW20             DONE - BUMP TO NEXT LINE                     
PUTLW20  DS    0H                  BUMP TO NEXT "LINE"                          
         LA    RF,1(RF)                                                         
         LA    R3,44(R3)                                                        
         MVI   0(R3),C' '                                                       
         XIT1  REGS=(R3)           RETURN THIS REGISTER "AS IS"                 
*                                                                               
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*      OUTPUT I/O LEGAL WARNING PRINTED AND REPEAT PUB DATA           *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PUTLW2   NTR1  BASE=*,LABEL=*                                                   
         USING PIOELEM,R2                                                       
         OC    PIOLWCD(2),PIOLWCD  ANY LEGAL WARNING ENTRIES                    
         BZ    PUTLW2G             NO                                           
         CLC   PIOLWCD(2),SAVLWCD  LW USED=PRD OR BUY LW ?                      
         BE    PUTLW2G             YES                                          
**** OUTPUT LEGAL WARNING PRINTED=XX HERE *****                                 
         LA    R3,34(R3)           NEXT ENTRY                                   
         XC    0(34,R3),0(R3)                                                   
         MVC   2(22,R3),=C'LEGAL WARNING PRINTED='                              
         MVC   25(2,R3),PIOLWCD                                                 
*****    B     PUTLW2G                                                          
PUTLW2G  DS    0H                                                               
         CLI   1(R2),X'32'         PIOELEM GT 50?                               
         BNH   PUTLW2X                                                          
         OC    PIORPUB,PIORPUB     ANY DATA HERE?                               
         BZ    PUTLW2X             NO                                           
*                                                                               
* OUTPUT REPEAT PUB HERE                                                        
*                                                                               
         LA    R3,34(R3)           NEXT ENTRY                                   
         XC    0(34,R3),0(R3)                                                   
         MVC   0(11,R3),=C'REPEAT PUB='                                         
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),PIORPUB),(C'S',11(R3))                        
*                                                                               
PUTLW2X  DS    0H                                                               
         XIT1  REGS=(R3)           RETURN THIS REGISTER "AS IS"                 
*                                                                               
         DROP R2                                                                
*                                                                               
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*       OUTPUT WEBIO LEGAL WARNING PRINTED AND REPEAT PUB DATA        *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
PUTLW3   NTR1  BASE=*,LABEL=*                                                   
         USING PWIOELD,R2                                                       
         OC    PWIOLWCD(2),PWIOLWCD  ANY LEGAL WARNING ENTRIES                  
         BZ    PUTLW3G               NO                                         
         CLC   PWIOLWCD(2),SAVLWCD   LW USED=PRD OR BUY LW ?                    
         BE    PUTLW3G               YES                                        
**** OUTPUT LEGAL WARNING PRINTED=XX HERE *****                                 
         LA    R3,34(R3)           NEXT ENTRY                                   
         XC    0(34,R3),0(R3)                                                   
         MVC   2(22,R3),=C'LEGAL WARNING PRINTED='                              
         MVC   25(2,R3),PWIOLWCD                                                
*****    B     PUTLW3G                                                          
PUTLW3G  DS    0H                                                               
*NOP*    OC    PWIORPUB,PWIORPUB   ANY DATA HERE?                               
*NOP*    BZ    PUTLW3X             NO                                           
*                                                                               
* OUTPUT REPEAT PUB HERE                                                        
*                                                                               
*NOP*    LA    R3,34(R3)           NEXT ENTRY                                   
*NOP*    XC    0(34,R3),0(R3)                                                   
*NOP*    MVC   0(11,R3),=C'REPEAT PUB='                                         
*NOP*    IC    R0,PAGYPROF+12                                                   
*NOP*    GOTO1 PUBEDIT,DMCB,((R0),PWIORPUB),(C'S',11(R3))                       
*                                                                               
PUTLW3X  DS    0H                                                               
         XIT1  REGS=(R3)           RETURN THIS REGISTER "AS IS"                 
*                                                                               
         DROP R2                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'GETPRD  GET PRODUCT'                                            
GETPRD   NTR1  BASE=*,LABEL=*                                                   
         CLI   PBUYKEY,255                                                      
         JE    GETPRDX                                                          
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(10),PBUYKEY                                                  
         MVI   KEY+3,6                                                          
         LA    R0,PPRDREC                                                       
         CLC   PPRDREC(10),KEY          SEE IF ALREADY THERE                    
         JNE   GETPRD5                                                          
         MVC   KEY(64),WORK                                                     
         J     GETPRDX                                                          
*                                                                               
GETPRD5  GOTO1 READ                                                             
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         CLI   RUNSW,0                                                          
         JE    GETPRDX                                                          
         MVC   KEY(64),WORK                                                     
         GOTO1 HIGH                                                             
GETPRDX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
TLPRT    CSECT                     PRINTING                                     
         NMOD1 0,TLPRT                                                          
         L     RC,PPFILEC                                                       
         SPACE 2                                                                
         BAS   RE,TLCKHD                                                        
*                                                                               
TLPRT2   DS    0H                                                               
         CLI   FORCEHED,C'Y'                                                    
         BNE   *+8                                                              
         BAS   RE,TLHEAD                                                        
         GOTO1 REPORT                                                           
*                                                                               
         MVI   LINENEED,0                                                       
TLPXIT   XIT1                                                                   
*                                                                               
*                                                                               
*                                                                               
         SPACE 3                                                                
TLHEAD   DS    0H                                                               
         MVI   RCSUBPRG,0                                                       
         CLC   QCLIENT,SPACES                                                   
         BE    *+8                                                              
         MVI   RCSUBPRG,1                                                       
         CLC   QPRODUCT,SPACES                                                  
         BE    TLHEAD0                                                          
         CLI   RUNSW,X'21'         PUB/PRD SEQ                                  
         BE    TLHEAD0                                                          
         MVI   RCSUBPRG,2                                                       
TLHEAD0  DS    0H                                                               
         CLC   QEST,SPACES                                                      
         BE    *+8                                                              
         MVI   RCSUBPRG,3                                                       
         CLI   QOPT2,C'$'                                                       
         BNE   TLHEAD1                                                          
         SR    RF,RF                                                            
         IC    RF,RCSUBPRG                                                      
         LA    RF,10(RF)                                                        
         STC   RF,RCSUBPRG                                                      
TLHEAD1  DS    0H                                                               
TLHEAD4  DS    0H                                                               
         LA    RF,SPACES                                                        
         CLI   QBPDATE,C' '                                                     
         BE    TLHEAD5                                                          
         LA    RF,BILHD                                                         
         CLI   QBPDATE,C'B'                                                     
         BE    TLHEAD5                                                          
         LA    RF,PAYHD                                                         
         CLI   QBPDATE,C'P'                                                     
         BE    TLHEAD5                                                          
         LA    RF,CLOSHD                                                        
         CLI   QBPDATE,C'C'                                                     
         BE    TLHEAD5                                                          
***MAT***                                                                       
         LA    RF,MCLOSHD                                                       
         CLI   QBPDATE,C'M'               MATERIALS CLOSING                     
         BE    TLHEAD5                                                          
***MAT***                                                                       
         LA    RF,OSDHD                                                         
         CLI   QBPDATE,C'S'                                                     
         BE    TLHEAD5                                                          
         LA    RF,IODHD                                                         
         CLI   QBPDATE,C'I'                                                     
TLHEAD5  DS    0H                                                               
         MVC   HEAD5+50(22),0(RF)                                               
         CLI   QCLIENT,C'*'                                                     
         BNE   TLHEAD6                                                          
         MVC   HEAD8+1(6),=C'OFFICE'                                            
*SMY*    MVC   HEAD8+10(2),QCLIENT+1                                            
*                                                                               
*        PRINT OFFICE CODE                                                      
*                                                                               
         ST    RE,FULL             SAVE RE                                      
*                                                                               
         GOTOR VPRNTOFC,DMCB,QCLIENT+1,(C'L',HEAD8+10),VOFFICER,       X        
               QAGENCY,VCOMFACS                                                 
*                                                                               
         L     RE,FULL             RESTORE RE                                   
*                                                                               
TLHEAD6  DS    0H                                                               
         CLI   QOPT7,C'Y'                                                       
         BNE   TLHEADX                                                          
         MVI   HEAD4+115,C'T'                                                   
         MVC   HEAD7+97(34),=C'*INCLUDES ANY PROPOSED INSERTIONS*'              
         CLI   QMEDIA,C'O'                                                      
         BNE   TLHEADX                                                          
         MVC   HEAD7+120(11),=C'POSTINGS*  '                                    
TLHEADX  DS    0H                                                BUG01          
         CLI   OLDPROF+5,C'E'                                    BUG01          
         BE    TLHPERCL                                          BUG01          
         CLI   OLDPROF+5,C'P'                                    BUG01          
         BNER  RE                                                BUG01          
TLHPERCL MVC   QSTART(12),SPACES                                 BUG01          
         BR    RE                                                               
         SPACE 2                                                                
BILHD    DC    C' ** BILLING PERIOD ** '                                        
PAYHD    DC    C' ** PAYABLE DATES **  '                                        
CLOSHD   DC    C' ** CLOSING DATES **  '                                        
MCLOSHD  DC    C'**MAT. CLOSING DATES**'                                        
IODHD    DC    C'** INS. ORDER DATES **'                                        
OSDHD    DC    C' ** ON-SALE DATES **  '                                        
*                                                                               
         SPACE 3                                                                
TLCKHD   DS    0H                                                               
         CLI   FORCEHED,C'Y'                                                    
         BER   RE                                                               
*                                                                               
         SR    R0,R0                                                            
         SR    RF,RF                                                            
         IC    R0,LINE                                                          
         IC    RF,LINENEED                                                      
         AR    R0,RF                                                            
         STC   R0,BYTE                                                          
         CLC   BYTE,MAXLINES                                                    
         BLR   RE                                                               
         MVI   FORCEHED,C'Y'                                                    
         BR    RE                                                               
         SPACE 3                                                                
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
PRTCOM   CSECT                                                                  
         NMOD1 0,PRTCOM                                                         
         L     RC,PPFILEC                                                       
         L     R6,ASCOMTAB                                                      
         L     R4,BSPAR3                 NUMBER OF COMMENTS                     
         OC    BSPAR3,BSPAR3                                                    
         BZ    PRTCX                                                            
PRTC5    CLI   0(R6),X'FF'                                                      
         BE    PRTCX                                                            
         GOTO1 ATLPRT                    SKIP A LINE                            
         LR    R3,R6                                                            
PRTC8    CLI   0(R3),C' '                                                       
         BNE   PRTC9                                                            
         LA    R3,1(R3)                                                         
         B     PRTC8                                                            
*                                                                               
PRTC9    CLI   0(R3),C' '                                                       
         BE    PRTC10                                                           
         LA    R3,1(R3)                                                         
         B     PRTC9                                                            
*                                                                               
PRTC10   SR    R3,R6                                                            
         CHI   R3,7                                                             
         BL    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   WORK(7),SPACES                                                   
         LA    R1,WORK+6                                                        
         SR    R1,R3                                                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R6)         RIGHT ALIGN IN WORK                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),LASTAGY                                                   
         MVI   KEY+3,X'40'                                                      
         MVC   KEY+4(6),WORK                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'              COMMENT NOT FOUND                              
         L     R0,ACONIO1          (A)PCONREC                                   
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         LA    R0,PBUYREC                                                       
         ST    R0,AREC                                                          
         MVC   P1+1(17),=C'STANDARD COMMENT='                                   
         MVC   P1+20(6),0(R6)                                                   
         GOTO1 ATLPRT                                                           
         L     R2,ACONIO1          (A)PCONREC                                   
         LA    R2,33(R2)                                                        
         MVI   ELCODE,X'40'                                                     
         CLI   0(R2),X'40'                                                      
         BE    PRTC15                                                           
PRTC12   BAS   RE,PNEXTEL                                                       
         BNE   PRTC30                                                           
PRTC15   ZIC   R5,1(R2)                                                         
         LA    R3,2(R2)                                                         
         CLI   2(R2),C'+'                                                       
         BNE   PRTC17                                                           
         MVC   SPACING,3(R2)                                                    
         NI    SPACING,X'0F'                                                    
         CLI   SPACING,3                                                        
         BNH   *+8                                                              
         MVI   SPACING,3                                                        
         CLI   SPACING,0                                                        
         BNE   *+8                                                              
         MVI   SPACING,1                                                        
         MVI   P1,0                                                             
         GOTO1 ATLPRT                                                           
         AHI   R5,-2           FOR '+' AND NUMBER OF LINES                      
         LA    R3,4(R2)                                                         
*                                                                               
PRTC17   AHI   R5,-3           FOR ELEM CODE+LENGHT +1 FOR EXECUTE              
         BM    PRTC18          NOTHING TO PRINT SO PRINT BLANK LINE             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P1+10(0),0(R3)                                                   
PRTC18   MVI   P1,0                                                             
         GOTO1 ATLPRT                                                           
         B     PRTC12                                                           
*                                                                               
PRTC30   LA    R6,7(R6)         GO DO NEXT STANDARD COMMENT                     
         BCT   R4,PRTC5                                                         
         SPACE 2                                                                
PRTCX    L     R1,=A(SCOMTAB)                                    BUG01          
         ST    R1,ASCOMTAB                                       BUG01          
         SR    R0,R0            SET BSPARS FOR BINSRCH           BUG01          
         L     R1,ASCOMTAB                                       BUG01          
         SR    R2,R2                                             BUG01          
         LA    R3,7                                              BUG01          
         LA    R4,6                                              BUG01          
         LHI   R5,1000                                           BUG01          
         STM   R0,R5,BSPARS                                      BUG01          
         XIT1                                                                   
*                                                                               
*                                                                               
PNEXTEL  DS    0H                                                               
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    PNEXTEL2                                                         
         CLC   ELCODE,0(R2)                                                     
         BER   RE                                                               
         B     PNEXTEL+2                                                        
PNEXTEL2 LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
SCOMTAB  CSECT                                                                  
         DS    7000C               1000 X 6+1                                   
*                                                                               
SINSTAB  CSECT                                                                  
         DS    1700C                 50 X 34                                    
*                                                                               
         SPACE 3                                                                
PP78WRKD DSECT                                                                  
RUNSW    DS    X                                                                
ACTIVITY DS    CL1                                                              
ESAVKEY  DS    CL32                                                             
CHGSW    DS    CL1                                                              
CHGSW2   DS    CL1                                                              
ORDSW    DS    CL1                                                              
BSTART   DS    XL3                                                              
BEND     DS    XL3                                                              
*                                                                               
BTODAY   DS    CL3                                                              
ACTDATE  DS    CL3                                                              
ASCOMTAB DS    A                                                                
ANXTSCOM DS    A                                                                
ASINSTAB DS    A                                                                
ATLPRT   DS    A                                                                
APRTCOM  DS    A                                                                
ATLOUT   DS    A                                                                
*                                                                               
ACONIO1  DS    A                   PCONREC ADDRESS FROM PPG                     
*                                                                               
VOFFICER DS    V                                                                
VPRNTOFC DS    V                                                                
*                                                                               
BSPARS   DS    0F                                                               
BSPAR1   DS    F                                                                
BSPAR2   DS    F                                                                
BSPAR3   DS    F                                                                
BSPAR4   DS    F                                                                
BSPAR5   DS    F                                                                
BSPAR6   DS    F                                                                
*                                                                               
NOJOB    DS    CL1                                                              
MYCOPY   DS    CL17                                                             
MYCAP1   DS    CL25                                                             
MYCAP2   DS    CL25                                                             
MYCAPEND DS    CL1                                                              
*                                                                               
OLDKEY   DS    0CL25                                                            
         DS    CL4                                                              
OLDCLT   DS    CL3                                                              
OLDPRD   DS    CL3                                                              
OLDPUB   DS    CL6                                                              
         DS    CL3                                                              
OLDEST   DS    CL2                                                              
         DS    CL4                                                              
OLDJOB   DS    CL6                                                              
*                                                                               
         DS    0F                                                               
SAVPARS  DS    0CL24                                                            
         DS    6F                                                               
ALASTIO  DS    F                   ADDR OF LAST IO ELEM                         
LASTIOD  DS    CL2                 PACKED DATE OF LAST I/O ELEM                 
LASTDAT  DS    CL3                 DATE OF LAST I/O ELEM                        
IOELCOD  DS    CL1                 ELEMENT CODE OF LAST I/O ELEM                
IOSW     DS    CL1                 N= NO I/O,O=I/O,C=CANCELLATION I/O           
LINENEED DS    X                                                                
SRTLIN   DS    H                                                                
SAVKEYS  DS    CL64                                                             
SAVDAT2  DS    CL8                                                              
         DS    0D                                                               
ESTTOTS  DS    CL16                                                             
PRDTOTS  DS    CL16                                                             
CLTTOTS  DS    CL16                                                             
*                                                                               
TOTWRK   DS    CL16                                                             
TOTTOT   DS    CL8                                                              
*                                                                               
PUBTAB   DS    0CL324              36 X 10= 360                                 
PUBT1    DS    CL36                                                             
PUBT2    DS    CL36                                                             
PUBT3    DS    CL36                                                             
PUBT4    DS    CL36                                                             
PUBT5    DS    CL36                                                             
PUBT6    DS    CL36                                                             
PUBT7    DS    CL36                                                             
PUBT8    DS    CL36                                                             
PUBT9    DS    CL36                                                             
PUBT10   DS    CL36                ONLY NEEDED FOR SKIPPING                     
PUBTABX  DS    CL1                                                              
*                                                                               
SPAC1    DS    CL20                                                             
SPAC2    DS    CL20                                                             
SPAC3    DS    CL20                                                             
SPACX    DS    CL1                                                              
*                                                                               
REQOLD   DS    CL37                LAST REQUEST PROCESSED                       
LASTAGY  DS    CL3                                                              
OLDPROF  DS    CL16                                                             
P72APROF DS    CL16                                                             
*                                                                               
SAVLWCD  DS    CL1                 LEGAL WARNING CODE                           
SAVLWQC  DS    CL1                 LEGAL WARNING QUARTER                        
SAVLWTYP DS    CL1                 O = "OVERRIDE" (FROM BUYREC)                 
*                                  D = "DEFAULT"  (FROM PRDREC)                 
PPBYOWRK DS    CL600                                                            
*                                                                               
COMTAB   DS    26CL44   COPY + 2 CAPTIONS + 2 ALLOS + 5 COMS + CHANGES          
*                       POSITION INS MESSAGE + 5 POSITION INSTRUCTIONS          
COMTABX  DS    CL1                                                              
         SPACE 3                                                                
SORTRECD DSECT                                                                  
SORTREC  DS    0CL46                                                            
SORTKEY  DS    0CL41                                                            
SKCLT    DS    CL3                                                              
SKPRD    DS    CL3                                                              
SKEST    DS    XL2                                                              
SKJOB    DS    CL6                                                              
SKPUB    DS    CL19                                                             
SKDAT1   DS    CL3                                                              
SKDAT2   DS    CL3                                                              
SKLIN    DS    CL2                 TO PRESERV ORIG SEQ                          
SCHGSW   DS    CL1                                                              
SRECDA   DS    CL4                                                              
*                                                                               
TLLIND   DSECT                     LINE DSECT                                   
         DS    CL1                                                              
TLJOB    DS    CL6                                                              
         DS    CL1                                                              
TLPUB    DS    CL15                                                             
         DS    CL1                                                              
TLPNAME  DS    CL20                                                             
         DS    CL1                                                              
TLIDAT   DS    CL12                                                             
         DS    CL1                                                              
TLCDAT   DS    CL8                                                              
         DS    CL1                                                              
TLMDAT   DS    CL8                                                              
         DS    CL1                                                              
TLSPACE  DS    CL17                                                             
         DS    CL1                                                              
TLIODAT  DS    CL8                                                              
         DS    CL1                                                              
TLIOTYP  DS    CL3                                                              
         DS    CL1                                                              
TLREFNO  DS    CL12                                                             
         DS    CL1                                                              
TLRPTDT  DS    CL8                                                              
         SPACE 3                                                                
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPMODEQU                                                       
*                                                                               
       ++INCLUDE PPREPWORK                                                      
*                                                                               
       ++INCLUDE PPREPWORK2                                                     
*                                                                               
         PRINT ON                                                               
PPWORKD  DSECT                                                                  
         ORG   QREGION                                                          
QJOB     DS    CL6                                                              
         ORG   QPUBFREQ                                                         
QBUYLIN  DS    CL2                   COLUMN 60                                  
*                                                                               
PCHGELD  DSECT                                                                  
       ++INCLUDE PCHGELEM                                                       
PBFSIELD DSECT                                                                  
       ++INCLUDE PBFSIEL                                                        
PBSHPD   DSECT                                                                  
       ++INCLUDE PBSHPDEL                                                       
PXDAYELD DSECT                                                                  
       ++INCLUDE PEXDAYEL                                                       
PXDATELD DSECT                                                                  
       ++INCLUDE PEXDATEL                                                       
PWIOELD  DSECT                                                                  
       ++INCLUDE PPGENBYIO                                                      
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PPNEWFILE                                                      
*                                                                               
       ++INCLUDE DDCOMFACSD                                                     
*                                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'063PPREP7802 04/26/06'                                      
         END                                                                    
