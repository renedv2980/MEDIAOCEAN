*          DATA SET NEMED2A    AT LEVEL 040 AS OF 05/01/02                      
*PHASE T31E2AA                                                                  
*                                                                               
         TITLE 'NEMED2A (T31E2A) - NETWORK ARMY INTERFACE TAPE'                 
*****************************************************                           
*                                                   *                           
*  NETARMY PRODUCES INTERFACE TAPE FOR NWAYER       *                           
*                                                   *                           
*****************************************************                           
         SPACE 2                                                                
T31E2A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**NXM**                                                        
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         LA    R1,HDHOOK                                                        
         ST    R1,HEADHOOK                                                      
         L     R7,ANETWS2                                                       
         USING WORKD,R7                                                         
         SPACE                                                                  
         XC    WORKD,WORKD                                                      
         SPACE                                                                  
*                                                                               
MAINLINE DS    0H                                                               
         SPACE                                                                  
         CLI   MODE,RUNLAST        CLOSE TAPE AT RUNLAST                        
         BNE   MAIN2                                                            
         CLI   SPLTST,C'N'         IS IT TEST RUN                               
         BNE   MAIN1               NO/SO CLOSE OPENED TAPE                      
         CLOSE (SPXMTP)                                                         
MAIN1    B     EXIT                                                             
         SPACE                                                                  
MAIN2    BAS   RE,INIT                                                          
         BAS   RE,NET1         GET RECS/SET UP/PASS TO SORTER                   
         BAS   RE,NET2         GET RECS FROM SORT/PRINT                         
         BAS   RE,NET3         WRITE TOTALS                                     
         SPACE                                                                  
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
         SPACE                                                                  
***********************************************                                 
*                                             *                                 
*  INITIALIZATION: GET DATES                  *                                 
*                  ZAP TOTALS FIELDS          *                                 
*                  SET UP SORTER              *                                 
*                  OPEN FILES                 *                                 
***********************************************                                 
         SPACE                                                                  
INIT     NTR1                                                                   
         ZAP   DOLTOTS,=P'0'                                                    
         ZAP   SPOTS,=P'0'                                                      
         SPACE                                                                  
* SET UP SORTER *                                                               
*                                                                               
INIT1    XC    DMCB(12),DMCB                                                    
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD                                     
         B     INIT2                                                            
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,9,A),FORMAT=BI,WORK=1'                       
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=106'                                   
         SPACE 2                                                                
INIT2    DS    0H                                                               
         CLI   SPLTST,C'N'         IS IT TEST RUN                               
         BNE   INIT3                                                            
         L     R2,BOOKVAL                                                       
         ST    R2,ADCB                                                          
         CLC   0(2,R2),=2X'90EC'                                                
         BNE   INIT3                                                            
         MVC   0(256,R2),SPXMTP                                                 
         OPEN  ((R2),OUTPUT)     NO/SO OPEN TAPE                                
INIT3    B     INITX                                                            
         SPACE                                                                  
*                                                                               
SPXMTP   DCB   DDNAME=SPXMTP,                                          X        
               DSORG=PS,RECFM=FB,LRECL=00080,BLKSIZE=00800,MACRF=PM             
         SPACE                                                                  
INITX    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
********************************************************                        
* THIS ROUTINE GETS UNIT RECS THROUGH NETIO            *                        
* PROCESSES THEM AND PASSES THEM TO SORTER             *                        
*                                                      *                        
*  INPUT: REC FROM NETIO                               *                        
*                                                      *                        
* OUTPUT: SORT REC PASSED TO SORTER                    *                        
*                                                      *                        
********************************************************                        
         SPACE 2                                                                
NET1     NTR1                                                                   
         SPACE                                                                  
         MVI   NBDATA,C'U'         UNITS ONLY                                   
         MVI   NBSELUOP,C'A'       ACTUAL SCHEDULE                              
         L     R2,NBAIO            SET UP SORTREC IN NBAIO                      
         USING PLINED,R2                                                        
         SPACE                                                                  
NETGET   GOTO1 NSNETIO,DMCB,NETBLOCK                                            
         SPACE                                                                  
         CLI   NBMODE,NBREQLST                                                  
         BE    NET1X                                                            
         CLI   NBMODE,NBPROCUN     ONLY DEAL WITH UNIT RECS                     
         BNE   NETGET                                                           
         SPACE                                                                  
         XC    REPREC,REPREC                                                    
*                                  GET COMMENT FOR LEAD GENERATION              
         L     R6,NBAIO                                                         
         MVI   ELCODE,X'44'        COMMENT ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   NET10                                                            
         USING NUCOMD,R6                                                        
COMLOOP  CLC   =C'LEAD=',NUCOMMNT                                               
         BNE   NXTCOM                                                           
         MVC   RPTLDGEN,NUCOMMNT+5                                              
         B     NET10                                                            
NXTCOM   BAS   RE,NEXTEL                                                        
         BE    COMLOOP                                                          
         DROP  R6                                                               
*                                                                               
NET10    MVC   KPRD,NBSELPGR             * = KEY SORT FIELDS                    
         MVC   KDTE,NBACTDAT             *                                      
         MVC   KTIME,NBTIME              *                                      
         MVC   RPTLNM,NBSUBOUT                                                  
         MVC   RPTAGY,NBSELAGY                                                  
         MVC   RPTMED,NBSELMED                                                  
         MVI   RPTMED+1,C'T'                                                    
         MVC   RPTCLT,NBSELCLI                                                  
         MVC   RPTPRD,SPLPRO                                                    
         ZIC   R0,NBACTEST                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RPTEST,DUB                                                       
         GOTO1 DATCON,DMCB,(2,NBACTDAT),(0,RPTDATE)                             
         MVC   RPTSTA(4),NBACTNET                                               
         MVC   RPTDPT(1),NBACTDP                                                
         MVC   RPTPRG,NBACTPRG                                                  
         ZIC   R0,NBLEN                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RPTSLEN,DUB                                                      
         XC    WORK(4),WORK                                                     
         MVC   WORK(2),NBTIME      MILITARY(START)TIME                          
         BAS   RE,TIMERTN                                                       
         MVC   RPTSTRTM,WORK                                                    
         XC    WORK(4),WORK                                                     
         MVC   WORK(2),NBTIME+2    MILITARY(END)TIME                            
         BAS   RE,TIMERTN                                                       
         MVC   RPTENDTM,WORK                                                    
         SPACE                                                                  
         DS    0H                  DAYS OF THE WEEK                             
         LA    R5,RPTDAYS                                                       
         TM    NBDAY,X'40'         MONDAY                                       
         BZ    W70A                                                             
         MVI   0(R5),C'1'                                                       
         AH    R5,=H'1'                                                         
W70A     TM    NBDAY,X'20'         TUESDAY                                      
         BZ    W70B                                                             
         MVI   0(R5),C'2'                                                       
         AH    R5,=H'1'                                                         
W70B     TM    NBDAY,X'10'         WEDNESDAY                                    
         BZ    W70C                                                             
         MVI   0(R5),C'3'                                                       
         AH    R5,=H'1'                                                         
W70C     TM    NBDAY,X'08'         THURSDAY                                     
         BZ    W70D                                                             
         MVI   0(R5),C'4'                                                       
         AH    R5,=H'1'                                                         
W70D     TM    NBDAY,X'04'         FRIDAY                                       
         BZ    W70E                                                             
         MVI   0(R5),C'5'                                                       
         AH    R5,=H'1'                                                         
W70E     TM    NBDAY,X'02'         SATURDAY                                     
         BZ    W70F                                                             
         MVI   0(R5),C'6'                                                       
         AH    R5,=H'1'                                                         
W70F     TM    NBDAY,X'01'         SUNDAY                                       
         BZ    WRT72                                                            
         MVI   0(R5),C'7'                                                       
         SPACE                                                                  
WRT72    DS    0H                                                               
         L     R3,NBBILTNT                                                      
*        LTR   R3,R3              A FAVOR FOR FRAN MAY NEED IT AGAIN            
*        BNZ   *+8                                                              
*        L     R3,=F'1700000'                                                   
         CVD   R3,DUB                                                           
         AP    DOLTOTS,DUB                                                      
         OI    DUB+7,X'0F'                                                      
         UNPK  RPTCOST,DUB                                                      
         AP    SPOTS,=P'1'                                                      
         SPACE                                                                  
         GOTO1 SORTER,DMCB,=C'PUT',(R2)                                         
         SPACE                                                                  
         B     NETGET                                                           
         SPACE 2                                                                
NET1X    B     EXIT                                                             
         EJECT                                                                  
*  TIME ROUTINE  *                                                              
TIMERTN  DS    0H                  CONVERT MILITARY TIME TO STANDARD            
         SPACE                                                                  
         SR    R0,R0                                                            
         ICM   R0,3,WORK                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(4),DUB                                                      
         CLC   WORK(4),=C'1200'                                                 
         BNE   TIME03                                                           
         MVI   WORK+4,C'N'         EQUALS 1200                                  
         BR    RE                                                               
TIME03   CLC   WORK(4),=C'0060'    UNDER 60                                     
         BNL   TIME05                                                           
         MVC   WORK(2),=C'12'                                                   
         MVI   WORK+4,C'A'                                                      
         BR    RE                                                               
TIME05   CLC   WORK(4),=C'1200'                                                 
         BNL   TIME10                                                           
         MVI   WORK+4,C'A'         UNDER 1200                                   
         BR    RE                                                               
TIME10   CLC   WORK(4),=C'2400'    EQUALS 2400                                  
         BNE   TIME20                                                           
         MVC   WORK(5),=C'1200M'                                                
         BR    RE                                                               
TIME20   CLC   WORK(4),=C'1300'                                                 
         BNL   TIME30                                                           
         MVI   WORK+4,C'P'         UNDER 1300                                   
         BR    RE                                                               
TIME30   S     R0,=F'1200'         OVER 1300                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(4),DUB                                                      
         MVI   WORK+4,C'P'                                                      
         BR    RE                                                               
         EJECT                                                                  
*****************************************************                           
*                                                   *                           
* NET2 TAKES RECS FROM SORTER AND WRITES THEM       *                           
*                                                   *                           
*****************************************************                           
         SPACE                                                                  
NET2     NTR1                                                                   
         MVI   FRST,C'Y'                                                        
         L     R3,ANETWS1                                                       
         USING TAPREC,R3                                                        
         SPACE                                                                  
NET22    GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R6,4(R1)                                                         
         LTR   R6,R6                                                            
         BNZ   NET23                                                            
         SPACE                                                                  
         CLI   FRST,C'Y'           TEST FIRST TIME                              
         BE    ERR                                                              
         B     NET2X                                                            
ERR      MVC   P(29),=C'NO RECORDS RECEIVED FROM SORT'                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         SPACE                                                                  
NET23    LA    R2,P                                                             
         MVC   0(PLENE,R2),9(R6)   HARDCODED TO SKIP SORT KEY                   
         USING RPTLNM,R2                                                        
         CLI   SPLTST,C'N'         IS IT TEST RUN                               
         BNE   NET24                                                            
         SPACE                                                                  
         XC    TAPREC,TAPREC     NO/WRITE TAPE                                  
         MVC   TPAGY,RPTAGY                                                     
         MVC   TPMED,RPTMED                                                     
         MVC   TPCLT,RPTCLT                                                     
         MVC   TPPRD,RPTPRD                                                     
         MVC   TPEST,RPTEST                                                     
         MVC   TPDATE,RPTDATE                                                   
         MVC   TPSTA,RPTSTA                                                     
         MVC   TPPRG,RPTPRG                                                     
         MVC   TPDPT,RPTDPT                                                     
         MVC   TPSLEN,RPTSLEN                                                   
         MVC   TPSTRTM,RPTSTRTM                                                 
         MVC   TPENDTM,RPTENDTM                                                 
         MVC   TPDAYS,RPTDAYS                                                   
         MVC   TPCOST,RPTCOST                                                   
         MVC   TPANSMT,RPTANSMT                                                 
         MVC   TPLDGEN,RPTLDGEN                                                 
         L     R1,ADCB                                                          
         PUT   (R1),TAPREC      WRITE TAPE                                      
         SPACE                                                                  
NET24    MVI   FRST,C'N'                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*        LA    R4,P                            ****TESTING****                  
*        GOTO1 HEXOUT,DMCB,TAPREC,(R4),80,0                                     
*        GOTO1 SPOOL,DMCB,(R8)                                                  
         B     NET22                                                            
         SPACE                                                                  
NET2X    DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*************************************                                           
*  PRINT TOTALS   CLOSE FILES       *                                           
*************************************                                           
NET3     NTR1                                                                   
*    WRITE TOTALS  *                                                            
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE                             
         SPACE                                                                  
         LA    R2,P                                                             
         USING RPTLNM,R2                                                        
         MVC   RPTCLT(14),=C'*** TOTALS ***'                                    
         MVC   RPTDPT(5),=C'SPOTS'                                              
         EDIT  (P4,SPOTS),(5,RPTCOST)                                           
         LA    R2,132(R2)                                                       
         MVC   RPTDPT(4),=C'COST'                                               
         LA    R5,RPTCOST-6                                                     
         EDIT  (P8,DOLTOTS),(15,(R5)),2,FLOAT=$                                 
         GOTO1 SPOOL,DMCB,(R8)                                                  
*    CLOSE FILES   *                                                            
         GOTO1 SORTER,DMCB,=C'END'                                              
         SPACE                                                                  
NET3X    B     EXIT                                                             
         SPACE 3                                                                
*********************                                                           
* HEAD HOOK ROUTINE *                                                           
*********************                                                           
HDHOOK   NTR1                                                                   
         B     EXIT                                                             
*                                                                               
         GETEL    (R6),DATADISP,ELCODE                                          
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
SORTRECD DSECT                                                                  
SRTRCLNE EQU   *-SORTRECD                                                       
         EJECT                                                                  
*                                                                               
PLINED   DSECT                     *** REPORT RECORD DSECT ***                  
*                                                                               
REPREC   DS    0CL106                                                           
KPRD     DS    CL3               * NBSELPGR      (* = KEY SORT FIELDS)          
KDTE     DS    CL2               * NBACTDAT                                     
KTIME    DS    CL4               * NBTIME(MILITARY)                             
*                                                                               
RPTLNM   DS    CL3                 BUY LINE NUMBER                              
         DS    CL1                                                              
RPTAGY   DS    CL2                 AGENCY                                       
         DS    CL1                                                              
RPTMED   DS    CL2                 MEDIA                                        
         DS    CL1                                                              
RPTCLT   DS    CL4                 CLIENT                                       
         DS    CL1                                                              
RPTPRD   DS    CL3                 PRODUCT                                      
         DS    CL1                                                              
RPTEST   DS    CL6                 ESTIMATE                                     
         DS    CL1                                                              
RPTDATE  DS    CL6                 DATE(YYMMDD)                                 
         DS    CL2                 SPARE                                        
RPTSTA   DS    CL6                 STATION CALL LETTERS (PLUS AM/FM)            
         DS    CL1                                                              
RPTPRG   DS    CL6                 PROGRAM CODE(NETWORK)                        
         DS    CL1                                                              
RPTDPT   DS    CL3                 DAYPART CODE                                 
         DS    CL1                                                              
RPTSLEN  DS    CL3                 SPOT LENGTH                                  
         DS    CL1                                                              
RPTSTRTM DS    CL5                 START TIME                                   
         DS    CL1                                                              
RPTENDTM DS    CL5                 END TIME                                     
         DS    CL1                                                              
RPTDAYS  DS    CL7                 DAYS OF WEEK(1=MONDAY,234=TUEWEDTH)          
         DS    CL1                                                              
RPTCOST  DS    CL9                 COST                                         
         DS    CL1                                                              
RPTANSMT DS    CL3                 NUMBER OF ANNOUNCEMENTS                      
RPTLDGEN DS    CL1                 LEAD GENERATION CODE                         
         DS    CL6                 SPARE                                        
RPTBKDT  DS    CL1                 BACK UP DATA                                 
PLENE    EQU   *-RPTLNM                                                         
*                                                                               
         EJECT                                                                  
TAPERECD DSECT                                                                  
*                                                                               
TAPREC   DS    0CL80               *** TAPE RECORD ***                          
TPAGY    DS    CL2                 AGENCY                                       
TPMED    DS    CL2                 MEDIA                                        
TPCLT    DS    CL4                 CLIENT                                       
TPPRD    DS    CL3                 PRODUCT                                      
TPEST    DS    CL6                 ESTIMATE                                     
TPDATE   DS    CL6                 DATE(YYMMDD)                                 
         DS    CL2                 SPARE                                        
TPSTA    DS    CL6                 STATION CALL LETTERS (PLUS AM/FM)            
TPPRG    DS    CL6                 PROGRAM CODE(NETWORK)                        
TPDPT    DS    CL3                 DAYPART CODE                                 
TPSLEN   DS    CL3                 SPOT LENGTH                                  
TPSTRTM  DS    CL5                 START TIME                                   
TPENDTM  DS    CL5                 END TIME                                     
TPDAYS   DS    CL7                 DAYS OF WEEK(1=MONDAY,234=TUEWEDTH)          
TPCOST   DS    CL9                 COST                                         
TPANSMT  DS    CL3                 NUMBER OF ANNOUNCEMENTS                      
TPLDGEN  DS    CL1                 LEAD GENERATION CODE                         
         DS    CL6                 SPARE                                        
TPBKDT   DS    CL1                 BACK UP DATA                                 
*                                                                               
         EJECT                                                                  
WORKD    DSECT                                                                  
ADCB     DS    V                                                                
DOLTOTS  DS    PL8                                                              
SPOTS    DS    PL4                                                              
EOFSRTRC DS    CL1                                                              
FRST     DS    CL1                                                              
WRKDLE   EQU   *-WORKD                                                          
         EJECT                                                                  
*                                                                               
RECD     DSECT                                                                  
       EJECT                                                                    
* DSECT TO COVER NETBLOCK                                                       
*                                                                               
         EJECT                                                                  
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDF7D                                                       
       ++INCLUDE NEGENUNIT                                                      
       PRINT ON                                                                 
 END                                                                            
