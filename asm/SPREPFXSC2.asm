*          DATA SET SPREPFXSC2 AT LEVEL 118 AS OF 12/16/99                      
*                                                                               
***********************************************************************         
*   FILE-FIX TO CLOSE OUT PW RECORDS (NEWLY ADDED TO THE ESTIMATE     *         
*   CLOSEOUT PROGRAM.                                                           
*   RULES:                                                                      
*        1: CLOSE PW RECORDS WHERE THE ESTIMATE NO LONGER EXISTS.               
*        2: CLOSE PW RECORDS WITH DATES OUTSIDE OF ESTIMATE RANGE.              
*        3: PRINT PW RECORDS WITH CONFUSING WEEK ELEMENTS.                      
***********************************************************************         
*                                                                               
*PHASE SPFX02S                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'SPFX02 - CLOSING OUT PW RECORDS'                                
SPFX02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RR=R2                                                   
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPFX02+4096,RC                                                   
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
EXIT     XIT1                                                                   
DMXIT    XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
                                                                                
************************  REQFRST  **********************************           
REQF     DS    0H                                                               
         XC    COUNT,COUNT         COUNT OF RECORD WITH DATES OFF               
         LA    R1,TEMPIO                                                        
         ST    R1,AREC                                                          
*                                                                               
         SR    R7,R7               COUNTERS                                     
         XC    KEY,KEY                                                          
*                                                                               
         MVC   P+2(24),=24CL2'* '                                               
         GOTO1 REPORT                                                           
         MVC   P+12(2),=C'PW'                                                   
         GOTO1 REPORT                                                           
         MVC   P+2(24),=24CL2'* '                                               
         GOTO1 REPORT                                                           
*                                                                               
         LA    R2,KEY                                                           
         USING PWRECD,R2                                                        
*                                                                               
         MVC   PWKTYP,=X'0D7A'                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(2),KEY      ARE WE STILL ON PW REC'S?                    
         BNE   REQFX                                                            
         B     REQF110                                                          
*                                                                               
REQF100  DS    0H                  YYMMDD                                       
         XC    RECDATE,RECDATE     DATE IN RECORD FOR COMPARISON                
         XC    ESTSTRT,ESTSTRT     ESTIMATE START DATE                          
         XC    ESTEND,ESTEND       ESTIMATE END DATE                            
         LA    R2,KEY                                                           
         GOTO1 SEQ                                                              
         CLC   KEYSAVE(2),KEY      ARE WE STILL ON PW REC'S?                    
         BNE   REQFX                                                            
*                                                                               
REQF110  MVC   SVKEY,KEY           GET CLT RECORD TO GET PRODUCT CODE           
         LA    R2,SVKEY                                                         
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING CLIENTD,R3                                                       
*                                                                               
         MVI   CKEYTYPE,X'00'      CLIENT RECORD                                
         MVC   CKEYAM,PWKAGMD                                                   
         MVC   CKEYCLT,PWKCLT                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,ADCLT                                                         
         GOTO1 GETCLT                                                           
*                                                                               
         LA    R6,CLIST            MATCH AGAINST CLIST                          
REQF120  CLC   3(1,R6),PWKPRD                                                   
         BE    REQF125                                                          
         LA    R6,4(R6)                                                         
         CLI   R6,0                                                             
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     REQF120                                                          
*                                                                               
REQF125  XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING ESTHDR,R3                                                        
         MVI   EKEYTYPE,X'00'      ESTIMATE RECORDS                             
         MVC   EKEYAM,PWKAGMD                                                   
         MVC   EKEYCLT,PWKCLT                                                   
         MVC   EKEYPRD,0(R6)                                                    
         MVC   PRDCODE,0(R6)       SAVE FOR PRINTING PURPOSE                    
         MVC   EKEYEST,PWKEST                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(8),KEY      ESTIMATE FOUND?                              
         BE    REQF127                                                          
         MVI   REASON,1            REASON FOR CLOSE=EST NOT FOUND               
         B     REQF130                                                          
*                                                                               
REQF127  L     R3,ADEST                                                         
         GOTO1 GETEST                                                           
*                                                                               
         MVC   ESTSTRT,ESTART      STORE THE DATE RANGE FOR COMPARISON          
         MVC   ESTEND,EEND                                                      
*                                                                               
REQF130  MVC   KEY,SVKEY           NOW LET'S GO THROUGH THE PW REOCORD          
         GOTO1 HIGH                                                             
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',KEY+14,AREC,DMWORK            
*                                                                               
         CLI   REASON,1            ARE WE CLOSING THIS RECORD?                  
         BE    DELREC                                                           
*                                                                               
         L     R2,AREC                                                          
         XC    GRNFLAG,GRNFLAG     GREEN FLAG -> GOOD RECORD, WE KEEP           
         XC    YLWFLAG,YLWFLAG     YELLOW FLAG-> HMM? RECORD, WE PRINT          
         XC    REDFLAG,REDFLAG     RED FLAG   -> BAD! RECORD, WE CLOSE          
*                                                                               
         LA    R6,PWEL             POINT TO THE FIRST ELEMENT                   
         LA    R4,ELCDTAB                                                       
         MVC   ELCODE,0(R4)        FIRST ELEMENT IN ELCDTAB                     
*                                                                               
REQF135  BAS   RE,NEXTEL                                                        
         BNE   REQF150                                                          
*                                                                               
         CLI   ELCODE,X'26'        DATE IN X'26' IS YYMM, NOT YYMMDD            
         BE    REQF136                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(2,2(R6)),(0,RECDATE)                                
         B     REQF136A                                                         
*                                                                               
REQF136  MVC   TREBYTE(2),2(R6)                                                 
         MVI   TREBYTE+2,1                                                      
         GOTO1 DATCON,DMCB,(3,TREBYTE),(0,RECDATE)                              
*                                                                               
REQF136A CLC   RECDATE,ESTSTRT                                                  
         BL    REQF140             RED IF LOWER THAN ESTIMATE RANGE             
         CLC   RECDATE,ESTEND                                                   
         BH    REQF140             RED IF HIGHER THAN ESTIMATE RANGE            
         B     REQF137                                                          
*                                                                               
REQF137  CLI   REDFLAG,1           HAS RED FLAG BEEN TURNED ON                  
         BNE   *+18                NO?  THEN TURN GREEN FLAG ON                 
         MVI   YLWFLAG,1           YES? GOOD AND BAD DATES : PRINT              
         MVC   RECDATE2,RECDATE                                                 
         B     REQF150                                                          
         MVI   GRNFLAG,1           TURN ON GREEN FLAG                           
         MVC   RECDATE1,RECDATE                                                 
         MVC   RECDATE2,RECDATE1                                                
         B     REQF135             NEXT ELEMENT                                 
*                                                                               
REQF140  CLI   GRNFLAG,1           HAS GREEN FLAG BEEN TURNED ON                
         BNE   *+18                                                             
         MVI   YLWFLAG,1           YELLOW: PRINT                                
         MVC   RECDATE2,RECDATE                                                 
         B     REQF150                                                          
         MVI   REDFLAG,1                                                        
         MVC   RECDATE1,RECDATE                                                 
         MVC   RECDATE2,RECDATE1                                                
         B     REQF135                                                          
*                                                                               
REQF150  CLI   YLWFLAG,1           SUSPECIOUS RECORD?                           
         BE    PRTREC                                                           
         CLI   REDFLAG,1           BAD RECORD?                                  
         BE    DELREC                                                           
         CLI   GRNFLAG,1           GOOD RECORD?                                 
         BNE   REQF100             NEXT RECORD                                  
*                                                                               
         LA    R4,1(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BE    REQF100                                                          
         MVC   ELCODE,0(R4)                                                     
         LA    R6,PWEL                                                          
         B     REQF135             NEXT ELEMENT WITH DATE                       
*                                                                               
*********************************************************************           
*  PRTREC:PRINT OUT THE KEY INFO OF THE RECORD WHEN THERE ARE DATES *           
*  IN THE RECORD THAT FALLS IN AND FALLS OUT OF THE ESTIMATE RANGE. *           
*********************************************************************           
PRTREC   DS    0H                                                               
         GOTO1 HEXOUT,DMCB,KEY,P+2,13                                           
         MVC   P+30(3),=C'***'                                                  
         GOTO1 CLUNPK,DMCB,PWKCLT,CLTCODE                                       
         MVC   P+43(3),CLTCODE      CLIENT CODE                                 
         MVC   P+49(3),PRDCODE      PRODUCT CODE                                
         EDIT  PWKEST,(3,P+55),ALIGN=LEFT,ZERO=NOBLANK                          
         GOTO1 DATCON,DMCB,(0,ESTSTRT),(8,P+60)                                 
         GOTO1 DATCON,DMCB,(0,ESTEND),(8,P+70)                                  
         GOTO1 DATCON,DMCB,(0,RECDATE1),(8,P+80)                                
         GOTO1 DATCON,DMCB,(0,RECDATE2),(8,P+90)                                
         GOTO1 REPORT                                                           
PRTRECX  B     REQF100                                                          
*********************************************************************           
*       1:CLOSE OUT THE PW REC'S WITH ESTIMATE WHICH NO LONGER EXIST*           
*       2:CLOSE OUT THE PW REC'S WITH DATES NOT WITHING EST RANGE!  *           
*********************************************************************           
DELREC   DS    0H                                                               
         LA    R7,1(R7)                                                         
         GOTO1 HEXOUT,DMCB,KEY,P+2,13                                           
         CLI   QOPT1,C'L'                                                       
         BNE   DELX                                                             
*                                                                               
         LA    R1,KEY                                                           
         OI    13(R1),X'C0'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR',KEYSAVE,KEY                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AREC                                                          
         OI    15(R1),X'C0'                                                     
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'SPTFIL',KEY,AREC,DMWORK               
*                                                                               
DELX     GOTO1 CLUNPK,DMCB,PWKCLT,CLTCODE                                       
         MVC   P+43(3),CLTCODE     CLIENT CODE                                  
         MVC   P+49(3),PRDCODE     PRODUCT CODE                                 
         EDIT  PWKEST,(3,P+55),ALIGN=LEFT,ZERO=NOBLANK                          
         MVC   ESTCODE,P+55                                                     
         CLC   CLTCODE(9),SVCODE   SAME CLT PRD EST?                            
         BE    DELX10A                                                          
         MVC   SVCODE(9),CLTCODE                                                
         CLI   REASON,1                                                         
         BE    DELX10                                                           
         GOTO1 DATCON,DMCB,(0,ESTSTRT),(8,P+60)                                 
         GOTO1 DATCON,DMCB,(0,ESTEND),(8,P+70)                                  
         GOTO1 DATCON,DMCB,(0,RECDATE1),(8,P+80)                                
         GOTO1 DATCON,DMCB,(0,RECDATE2),(8,P+90)                                
DELX10   GOTO1 REPORT                                                           
DELX10A  MVI   REASON,0            RESET CLOSING REASON                         
         B     REQF100                                                          
*********************************************************************           
*        REQFX: PRINT OUT THE NUMBER OF RECORDS                     *           
*********************************************************************           
REQFX    ST    R7,DELPW                                                         
         MVC   P+15(5),=C'COUNT'                                                
         EDIT  DELPW,(10,P+27),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK               
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
**************************  REQL  ***********************************           
REQL     MVC   P(17),=C'NUMBER OF RECORDS'                                      
         EDIT  COUNT,(10,P+20),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK               
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
         GETEL R6,24,ELCODE                                                     
         EJECT                                                                  
**********************************************************************          
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
ELCODE   DS    X                                                                
CHANGED  DS    X                                                                
COUNT    DS    F                                                                
TEMP     DS    XL80                                                             
CLTCODE  DS    CL3                                                              
PRDCODE  DS    CL3                                                              
ESTCODE  DS    CL3                                                              
SVCODE   DS    CL9                                                              
SVKEY    DS    XL24                                                             
RECDATE  DS    CL6                                                              
RECDATE1 DS    CL6                                                              
RECDATE2 DS    CL6                                                              
TREBYTE  DS    XL3                 3 BYTES FOR DATCON                           
ESTSTRT  DS    CL6                 DATE RANGE OF ESTIMATE                       
ESTEND   DS    CL6                                                              
REASON   DS    X                   REASON OF RECORD BEING CLOSED                
DELPW    DS    F                                                                
RECFLAG  DS    C                                                                
GRNFLAG  DS    CL1                                                              
YLWFLAG  DS    CL1                                                              
REDFLAG  DS    CL1                                                              
ELCDTAB  DC    X'050607151626FF'   TABLE OF ELEMENT TO RUN THROUGH              
*                                                                               
TEMPIO   DS    XL2000                                                           
         SPACE 1                                                                
         EJECT                                                                  
CLIENTD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENWIPW                                                      
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'118SPREPFXSC212/16/99'                                      
         END                                                                    
