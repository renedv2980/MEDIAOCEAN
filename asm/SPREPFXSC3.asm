*          DATA SET SPREPFXSC3 AT LEVEL 093 AS OF 05/01/02                      
*                                                                               
***********************************************************************         
*  FILE-FIX TO CLOSE-OUT RECORDS THAT WERE LEFT OUT BY THE ORIGINAL   *         
*  CLOSE-OUT PROGRAM. (DARE, DARE MAKE GOODS, DARE MAKE GOOD NOCTICE  *         
*  MSR, BGR, STATUS, PGEST, BWS, CHILD-SPOT)                          *         
*  STANDARDS USED TO DETERMINE CLOSING:                               *         
*  1:  ESTIMATE IN RECORD NO LONGER EXIST.                            *         
*  2:  RECORD DATE IS OLDER THAN THE ESTIMATE BY 1 YEAR OR MORE.      *         
*      WHICH MEANS THE ESTIMATE PROBABLY HAS BEEN RE-OPENED.          *         
*  *   NOT CLOSING RECORDS WITH PRD OR EST = NULLS. IN ACCORD WITH    *         
*      THE CLOSE-OUT PROGRAM (SPWRI20)                                *         
*  *   NOT CLOSING RECORDS WITH DATE = NULLS UNLESS ESTIMATE DOESNT   *         
*      EXIST.                                                         *         
***********************************************************************         
*                                                                               
*PHASE SPFX02S                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'SPFX02 - CLOSING OUT MSR,BGR AND DARE RECORDS'                  
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
         XC    XKEY,XKEY                                                        
         LA    R1,TEMPIO                                                        
         ST    R1,AREC                                                          
*                                                                               
*********************************************************************           
*        DARE CLOSE OUT                                             *           
*********************************************************************           
         SR    R7,R7               COUNTERS                                     
         XC    KEY,KEY                                                          
*                                                                               
         MVC   P+2(24),=24CL2'* '                                               
         GOTO1 REPORT                                                           
         MVC   P+12(4),=C'DARE'                                                 
         GOTO1 REPORT                                                           
         MVC   P+2(24),=24CL2'* '                                               
         GOTO1 REPORT                                                           
*                                                                               
         LA    R2,KEY                                                           
         USING DAREORDD,R2                                                      
*                                                                               
         MVI   DCKTYPE,X'0D'                                                    
         MVI   DCKSUBTY,X'B5'      CLIENT PASSIVE PTR FOR DARE REC              
*                                                                               
         OI    DMCB,X'08'                                                       
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(2),KEY      ARE WE STILL ON DARE REC'S?                  
         BNE   REQFX                                                            
         B     REQF110                                                          
*                                                                               
REQF100  DS    0H                                                               
         XC    RECDATE,RECDATE                                                  
         XC    ESTDATE,ESTDATE                                                  
         XC    RANGE,RANGE                                                      
         LA    R2,KEY                                                           
         OI    DMCB,X'08'                                                       
         GOTO1 SEQ                                                              
         CLC   KEYSAVE(2),KEY                                                   
         BNE   REQFX                                                            
*                                                                               
REQF110  CLI   DCKPRD,0            SKIP RECORDS WITH PRODUCT = NULLS            
         BE    REQF100                                                          
         CLI   DCKEST,0            SKIP RECORDS WITH ESTIMATE = NULLS           
         BE    REQF100                                                          
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',KEY+14,AREC,DMWORK            
         LA    R2,TEMPIO                                                        
         LA    R6,DORFRST          DISPLACEMENT TO FIRST ELEMENT                
         USING DOXMTELD,R6                                                      
         MVI   ELCODE,X'11'                                                     
         BAS   RE,NEXTEL           WALTERS SAYS IF NO TRANSMISSION              
         BNE   REQF100             ELEMENT, THEN LEAVE THIS REC                 
*                                                                               
         GOTO1 DATCON,DMCB,(8,DOXMTYMD),(0,RECDATE)                             
*                                                                               
         MVC   SVKEY,KEY                                                        
         LA    R2,SVKEY            NEED CLIENT POINT TO BUILD CLI KEY           
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING CLTHDR,R3                                                        
*                                                                               
         MVI   CKEYTYPE,X'00'      CLIENT RECORD                                
         MVC   CKEYAM,DCKAGMD                                                   
         MVC   CKEYCLT,DCKCLT                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,ADCLT                                                         
         GOTO1 GETCLT                                                           
*                                                                               
         LA    R6,CLIST            MATCH AGAINST CLIST                          
REQF120  CLC   3(1,R6),DCKPRD                                                   
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
         MVC   EKEYAM,DCKAGMD                                                   
         MVC   EKEYCLT,DCKCLT                                                   
         MVC   EKEYPRD,0(R6)                                                    
         MVC   EKEYEST,DCKEST                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(8),KEY      ESTIMATE FOUND?                              
         BE    REQF127                                                          
         MVI   CLSNUM2,1                                                        
         B     REQF130                                                          
*                                                                               
REQF127  L     R3,ADEST                                                         
         GOTO1 GETEST                                                           
*                                                                               
         MVC   ESTDATE,ESTART                                                   
*                                                                               
         L     R0,=F'-365'                                                      
         GOTO1 ADDAY,DMCB,(C'D',ESTDATE),RANGE,(R0)                             
         CLC   RANGE,RECDATE                                                    
         BNH   REQF128                                                          
         MVI   CLSNUM2,3                                                        
         B     REQF130                                                          
*                                                                               
REQF128  MVC   KEY,SVKEY                                                        
         OI    DMCB,X'08'                                                       
         GOTO1 HIGH                                                             
         B     REQF100                                                          
*                                                                               
REQF130  MVC   KEY,SVKEY                                                        
         OI    DMCB,X'08'                                                       
         GOTO1 HIGH                                                             
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',KEY+14,AREC,DMWORK            
         L     R2,AREC                                                          
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING DAREMGND,R3                                                      
         MVI   MNKTYPE,MNKTYPQ     0D                                           
         MVI   MNKSUBTY,MNKSTYPQ   36                                           
         MVC   MNKAGMD,DOKAGMD     AGY/MD                                       
         LA    R6,DORFRST                                                       
         USING DOIDELD,R6                                                       
         MVC   MNKBYR,DOIDBYR      BUYER ID                                     
         MVC   MNKORDER,DOKORDER   ORDER NUMBER                                 
         OI    DMCB,X'08'                                                       
         GOTO1 HIGH                                                             
         B     REQF130B                                                         
         DROP  R6                                                               
*                                                                               
REQF130A OI    DMCB,X'08'                                                       
         GOTO1 SEQ                                                              
*                                                                               
REQF130B CLC   KEY(10),KEYSAVE     SAME ORDER NUMBER, BUYER CODE?               
         BNE   REQF140                                                          
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',KEY+14,AREC,DMWORK            
         MVI   CLSNUM,2                                                         
         BAS   RE,DELETE                                                        
         B     REQF130A                                                         
         DROP  R2                                                               
         DROP  R3                                                               
*                                                                               
REQF140  LA    R3,KEY                                                           
         USING DAREMGOD,R3                                                      
         L     R2,AREC                                                          
         USING DAREMGND,R2                                                      
*                                                                               
         MVI   MOKTYPE,MOKTYPQ     0D                                           
         MVI   MOKSUBTY,MOKSTYPQ   37                                           
         MVC   MOKAGMD,MNKAGMD     AGY/MD                                       
         MVC   MOKORDER,MNKORDER   ORDER NUMBER                                 
         OI    DMCB,X'08'                                                       
         GOTO1 HIGH                                                             
         B     REQF140B                                                         
*                                                                               
REQF140A OI    DMCB,X'08'                                                       
         GOTO1 SEQ                                                              
*                                                                               
REQF140B CLC   KEY(7),KEYSAVE      SAME ORDER NUMBER?                           
         BE    REQF140C                                                         
         MVC   CLSNUM,CLSNUM2                                                   
         B     REQF150                                                          
REQF140C GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',KEY+14,AREC,DMWORK            
         MVI   CLSNUM,2                                                         
         BAS   RE,DELETE                                                        
         B     REQF140A                                                         
*                                                                               
REQF150  MVC   KEY,SVKEY                                                        
         OI    DMCB,X'08'                                                       
         GOTO1 HIGH                                                             
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',KEY+14,AREC,DMWORK            
         BAS   RE,DELETE                                                        
         LA    R7,1(R7)                                                         
         B     REQF100             NEXT RECORD                                  
         EJECT                                                                  
*********************************************************************           
*        REQFX: PRINT OUT THE NUMBER OF RECORDS                     *           
*********************************************************************           
REQFX    ST    R7,DELDARE                                                       
         MVC   P+15(5),=C'COUNT'                                                
         EDIT  DELDARE,(10,P+27),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK             
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
*        CLOSING DARE SPOT RECORDS                                   *          
**********************************************************************          
DELETE   NTR1                                                                   
*                                                                               
         CLC   ESTDATE,=6X'00'                                                  
         BE    DELX                                                             
         CLC   RECDATE(2),=C'99'                                                
         BNE   DELX                                                             
*                                                                               
         BAS   RE,PRTDEL                                                        
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY,P+2,13                                           
         GOTO1 REPORT                                                           
         L     R6,AREC                                                          
         GOTO1 HEXOUT,DMCB,0(R6),P+2,15                                         
         MVC   P+50(7),=C'CLOSED!'                                              
         CLI   CLSNUM,1                                                         
         BNE   *+14                                                             
         MVC   P+50(21),=C'ESTIMATE WAS DELETED!'                               
         B     DEL10                                                            
*                                                                               
         CLI   CLSNUM,2                                                         
         BNE   *+14                                                             
         MVC   P+50(21),=C'CLOSE DARE MAKE GOODS'                               
         B     DEL10                                                            
*                                                                               
         MVC   P+50(19),=C'ESTIMATE DATES OFF!'                                 
*                                                                               
DEL10    GOTO1 REPORT                                                           
         CLI   QOPT1,C'L'                                                       
         BNE   DELX                                                             
*                                                                               
         LA    R1,KEY                                                           
         OI    13(R1),X'C0'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR',KEYSAVE,KEY                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*        GOTO1 WRITE                                                            
*                                                                               
         L     R1,AREC                                                          
         OI    15(R1),X'C0'                                                     
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'SPTFIL',KEY,AREC,DMWORK               
*        GOTO1 PUTREC                                                           
         GOTO1 REPORT                                                           
DELX     XIT1                                                                   
**********************************************************************          
*        PRINT OUT A REPORT ON DELETED RECORDS                       *          
**********************************************************************          
PRTDEL   NTR1                                                                   
         GOTO1 DATCON,DMCB,(0,ESTDATE),(8,P+2)                                  
         MVC   P+20(19),=C'ESTIMATE START DATE'                                 
         GOTO1 REPORT                                                           
         GOTO1 DATCON,DMCB,(0,RECDATE),(8,P+2)                                  
         MVC   P+20(18),=C'DATE IN THE RECORD'                                  
PRTDELX  GOTO1 REPORT                                                           
*                                                                               
         XIT1                                                                   
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
TEMP     DS    CL80                                                             
INVDA    DS    XL4                                                              
*                                                                               
XHALF    DS    XL2                                                              
*                                                                               
XKEY     DS    XL64                                                             
XKEYSAVE DS    XL64                                                             
SVXKEY   DS    XL64                                                             
SVKEY    DS    XL24                                                             
SVKEY2   DS    XL24                                                             
RECDATE  DS    CL6                                                              
ESTDATE  DS    CL6                                                              
RANGE    DS    CL6                 DATE USED TO COMPARE ESTIMATE                
CLSNUM   DS    X                   REASON OF RECORD BEING CLOSED OUT            
CLSNUM2  DS    X                                                                
DELMSR   DS    F                                                                
DELBGR   DS    F                                                                
DELDARE  DS    F                                                                
DELSTAT  DS    F                                                                
DELPGST  DS    F                                                                
DELCSO   DS    F                                                                
DELBWS   DS    F                                                                
RECFLAG  DS    C                                                                
*                                                                               
TEMPIO   DS    XL2000                                                           
         SPACE 1                                                                
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMSR                                                       
         EJECT                                                                  
       ++INCLUDE SPGENBGR                                                       
         EJECT                                                                  
       ++INCLUDE SPGENDRORD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENDRMKN                                                     
         EJECT                                                                  
       ++INCLUDE SPGENDRMKO                                                     
         EJECT                                                                  
       ++INCLUDE SPGENSTAT                                                      
         EJECT                                                                  
       ++INCLUDE SPGENPGEST                                                     
         EJECT                                                                  
       ++INCLUDE SPGENCSO                                                       
         EJECT                                                                  
       ++INCLUDE SPNWSCAM                                                       
         EJECT                                                                  
       ++INCLUDE SPNWSHDR                                                       
         EJECT                                                                  
       ++INCLUDE SPNWSDTL                                                       
         EJECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'093SPREPFXSC305/01/02'                                      
         END                                                                    
