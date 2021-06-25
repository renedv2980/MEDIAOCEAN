*          DATA SET SPREPFXCO  AT LEVEL 099 AS OF 05/01/02                      
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
*        CHECKING MATCHING STATUS RECORD                            *           
*********************************************************************           
         SR    R7,R7               COUNTERS                                     
*                                                                               
         MVC   P+2(24),=24CL2'* '                                               
         GOTO1 REPORT                                                           
         MVC   P+12(3),=C'MSR'                                                  
         GOTO1 REPORT                                                           
         MVC   P+2(24),=24CL2'* '                                               
         GOTO1 REPORT                                                           
*                                                                               
         LA    R2,XKEY                                                          
         USING MSRKEYD,R2                                                       
*                                                                               
         MVI   MSRKTYPE,MSRKTYPQ   X'0E'                                        
         MVI   MSRKSUB,MSRKSUBQ    X'04'                                        
         MVC   XKEYSAVE,XKEY       BACKING UP A COPY OF THE KEY                 
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
         B     REQF15                                                           
*                                                                               
REQF10   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,0                   
REQF15   DS    0H                                                               
         CLC   XKEY(2),XKEYSAVE    ARE WE STILL ON MSR?                         
         BNE   REQF45                                                           
         CLI   MSRKPRD,0           IF RECORD HAS NULLS FOR ESTIMATE             
         BE    REQF10              OR PRODUCT, SKIP IT.                         
         CLI   MSRKEST,0                                                        
         BE    REQF10                                                           
*                                                                               
         MVC   SVXKEY,XKEY         SAVE THE KEY FOR REQ CALLS                   
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING CLTHDR,R3                                                        
*                                                                               
         MVI   CKEYTYPE,X'00'      CLIENT RECORD                                
         MVC   CKEYAM,MSRKAM                                                    
         MVC   CKEYCLT,MSRKCLT                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,ADCLT                                                         
         GOTO1 GETCLT                                                           
*                                                                               
         LA    R6,CLIST            MATCH AGAINST CLIST                          
REQF20   CLC   3(1,R6),MSRKPRD                                                  
         BE    REQF25                                                           
         LA    R6,4(R6)                                                         
         CLI   R6,0                                                             
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     REQF20                                                           
*                                                                               
REQF25   XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING ESTHDR,R3                                                        
         MVI   EKEYTYPE,X'00'      ESTIMATE RECORDS                             
         MVC   EKEYAM,MSRKAM                                                    
         MVC   EKEYCLT,MSRKCLT                                                  
         MVC   EKEYPRD,0(R6)                                                    
         MVC   EKEYEST,MSRKEST                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(8),KEY                                                   
         BE    REQF33                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
         CLC   XKEY(32),SVXKEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',MSRDDA,AREC,DMWORK            
         MVI   CLSNUM,1                                                         
         B     REQF35                                                           
*                                                                               
REQF33   LA    R6,XKEY             DONT CLOSE OUT THIS SET                      
         CLI   15(R6),X'FF'                                                     
         BE    REQF10              NEXT SET                                     
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,0                   
         B     REQF33                                                           
*                                                                               
REQF35   LA    R6,XKEY             CLOSE OUT THE ENTIRE SET                     
         CLI   15(R6),X'FF'                                                     
         BE    REQF40                                                           
         GOTO1 =A(XDEL)                                                         
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,0                   
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',MSRDDA,AREC,DMWORK            
         B     REQF35                                                           
*                                                                               
REQF40   GOTO1 =A(XDEL)                                                         
         LA    R7,1(R7)                                                         
         B     REQF10                                                           
*                                                                               
         DROP  R2                                                               
*********************************************************************           
*        BGR CLOSE OUTS                                                         
*********************************************************************           
REQF45   ST    R7,DELMSR                                                        
         SR    R7,R7                                                            
         MVC   P+2(24),=24CL2'* '                                               
         GOTO1 REPORT                                                           
         MVC   P+12(3),=C'BGR'                                                  
         GOTO1 REPORT                                                           
         MVC   P+2(24),=24CL2'* '                                               
         GOTO1 REPORT                                                           
         XC    XKEY,XKEY                                                        
         LA    R2,XKEY                                                          
         USING BGRKEYD,R2                                                       
*                                                                               
         MVI   BGRKTYPE,BGRKTYPQ   X'0E'                                        
         MVI   BGRKSUB,BGRKSUBQ    X'05'                                        
         MVC   XKEYSAVE,XKEY       BACKING UP A COPY OF THE KEY                 
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
         B     REQF55                                                           
*                                                                               
REQF50   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,0                   
REQF55   DS    0H                                                               
         CLC   XKEY(2),XKEYSAVE    ARE WE STILL ON BGR?                         
         BNE   REQF90              !!!!!! GOTO DARE                             
         MVC   SVXKEY,XKEY         SAVE THE KEY FOR REQ CALLS                   
         CLI   BGRKEST,0                                                        
         BNE   *+8                                                              
         B     REQF50                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING CLTHDR,R3                                                        
*                                                                               
         MVI   CKEYTYPE,X'00'      CLIENT RECORD                                
         MVC   CKEYAM,BGRKAM                                                    
         MVC   CKEYCLT,BGRKCLT                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,ADCLT                                                         
         GOTO1 GETCLT                                                           
*                                                                               
         LA    R6,CLIST            MATCH AGAINST CLIST                          
REQF60   CLC   3(1,R6),BGRKPRD                                                  
         BE    REQF65                                                           
         LA    R6,4(R6)                                                         
         CLI   R6,0                                                             
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     REQF60                                                           
*                                                                               
REQF65   XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING ESTHDR,R3                                                        
         MVI   EKEYTYPE,X'00'      ESTIMATE RECORDS                             
         MVC   EKEYAM,BGRKAM                                                    
         MVC   EKEYCLT,BGRKCLT                                                  
         MVC   EKEYPRD,0(R6)                                                    
         MVC   EKEYEST,BGRKEST                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(8),KEY                                                   
         BE    REQF73                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'XSPDIR',XKEY,XKEY,0                   
         CLC   XKEY(32),SVXKEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',BGRDDA,AREC,DMWORK            
         MVI   CLSNUM,1                                                         
         B     REQF75                                                           
*                                                                               
REQF73   LA    R6,XKEY             DONT CLOSE OUT THIS SET                      
         CLI   15(R6),X'FF'                                                     
         BE    REQF50              NEXT SET                                     
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,0                   
         B     REQF73                                                           
*                                                                               
REQF75   LA    R6,XKEY             CLOSE OUT THE ENTIRE SET                     
         CLI   15(R6),X'FF'                                                     
         BE    REQF80                                                           
         GOTO1 =A(XDEL)                                                         
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',XKEY,XKEY,0                   
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'XSPFIL',BGRDDA,AREC,DMWORK            
         B     REQF75                                                           
*                                                                               
REQF80   GOTO1 =A(XDEL)                                                         
         LA    R7,1(R7)                                                         
         B     REQF50                                                           
*                                                                               
         DROP  R2                                                               
*********************************************************************           
*        DARE CLOSE OUT                                             *           
*********************************************************************           
REQF90   XC    KEY,KEY                                                          
*                                                                               
         MVC   P+2(24),=24CL2'* '                                               
         GOTO1 REPORT                                                           
         MVC   P+12(4),=C'DARE'                                                 
         GOTO1 REPORT                                                           
         MVC   P+2(24),=24CL2'* '                                               
         GOTO1 REPORT                                                           
*                                                                               
         ST    R7,DELBGR                                                        
         SR    R7,R7                                                            
         LA    R2,KEY                                                           
         USING DAREORDD,R2                                                      
*                                                                               
         MVI   DCKTYPE,X'0D'                                                    
         MVI   DCKSUBTY,X'B5'      CLIENT PASSIVE PTR FOR DARE REC              
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(2),KEY      ARE WE STILL ON DARE REC'S?                  
         BNE   CLSTAT              NO, MOVE ON TO STATUS REC                    
         B     REQF110                                                          
*                                                                               
REQF100  DS    0H                                                               
         LA    R2,KEY                                                           
         GOTO1 SEQ                                                              
         CLC   KEYSAVE(2),KEY                                                   
         BNE   CLSTAT                                                           
*                                                                               
REQF110  CLI   DCKPRD,0            SKIP RECORDS WITH PRODUCT = NULLS            
         BE    REQF100                                                          
         CLI   DCKEST,0            SKIP RECORDS WITH ESTIMATE = NULLS           
         BE    REQF100                                                          
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
         BE    REQF128                                                          
         MVI   CLSNUM2,1                                                        
         B     REQF130                                                          
*                                                                               
REQF128  MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         B     REQF100                                                          
*                                                                               
REQF130  MVC   KEY,SVKEY                                                        
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
         GOTO1 HIGH                                                             
         B     REQF130B                                                         
         DROP  R6                                                               
*                                                                               
REQF130A GOTO1 SEQ                                                              
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
         GOTO1 HIGH                                                             
         B     REQF140B                                                         
*                                                                               
REQF140A GOTO1 SEQ                                                              
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
         GOTO1 HIGH                                                             
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',KEY+14,AREC,DMWORK            
*                                                                               
         L     R2,AREC                                                          
         USING DAREORDD,R2         WE NEED TO ALSO CLOSE COMMENT REC'S          
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R2)                                                    
         MVI   KEY+12,X'01'                                                     
         GOTO1 HIGH                                                             
         B     REQF160                                                          
REQF155  GOTO1 SEQ                                                              
         DS    0H                                                               
REQF160  CLC   KEY(13),KEYSAVE                                                  
         BNE   REQF170                                                          
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',KEY+14,AREC,DMWORK            
         BAS   RE,DELETE                                                        
         B     REQF155                                                          
*                                                                               
REQF170  XC    KEY,KEY                                                          
         MVC   KEY(13),0(R2)                                                    
         MVI   KEY+12,X'02'                                                     
         GOTO1 HIGH                                                             
         B     REQF180                                                          
REQF175  GOTO1 SEQ                                                              
         DS    0H                                                               
REQF180  CLC   KEY(13),KEYSAVE                                                  
         BNE   REQF190                                                          
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',KEY+14,AREC,DMWORK            
         BAS   RE,DELETE                                                        
         B     REQF175                                                          
*                                                                               
REQF190  MVC   KEY,SVKEY           FINALLY THE DARE ORDER REC                   
         GOTO1 HIGH                                                             
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',KEY+14,AREC,DMWORK            
         BAS   RE,DELETE                                                        
         LA    R7,1(R7)                                                         
         B     REQF100             NEXT RECORD                                  
*********************************************************************           
*        CLSTAT: STATUS RECORD CLOSEOUTS                            *           
*********************************************************************           
CLSTAT   DS    0H                                                               
*                                                                               
         ST    R7,DELDARE                                                       
         SR    R7,R7                                                            
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING STATD,R2                                                         
*                                                                               
         MVC   P+2(24),=24CL2'* '                                               
         GOTO1 REPORT                                                           
         MVC   P+12(6),=C'STATUS'                                               
         GOTO1 REPORT                                                           
         MVC   P+2(24),=24CL2'* '                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   KEY(2),=X'0D71'                                                  
         GOTO1 HIGH                                                             
         B     CLSTAT15                                                         
*                                                                               
CLSTAT10 DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
CLSTAT15 CLC   KEYSAVE(2),KEY                                                   
         BNE   CLPGST                                                           
*                                                                               
         MVC   SVKEY,KEY                                                        
         LA    R2,SVKEY                                                         
*                                                                               
         CLI   STKPRD,X'00'                                                     
         BE    CLSTAT10                                                         
         CLI   STKEST,X'00'                                                     
         BE    CLSTAT10                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING CLTHDR,R3                                                        
*                                                                               
         MVI   CKEYTYPE,X'00'      CLIENT RECORD                                
         MVC   CKEYAM,STKAGMD                                                   
         MVC   CKEYCLT,STKCLT                                                   
         MVI   CLSNUM,1            SET ERROR CODE FIRST                         
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   CLSTAT60            CLIENT NOT FOUND, EST WONT BE FOUND          
                                                                                
         L     R3,ADCLT                                                         
         GOTO1 GETCLT                                                           
*                                                                               
         LA    R6,CLIST            MATCH AGAINST CLIST                          
CLSTAT20 CLC   3(1,R6),STKPRD                                                   
         BE    CLSTAT30                                                         
         LA    R6,4(R6)                                                         
         CLI   0(R6),0                                                          
         BNE   CLSTAT20                                                         
         MVI   CLSNUM,1                                                         
         B     CLSTAT60                                                         
*                                                                               
CLSTAT30 XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING ESTHDR,R3                                                        
         MVI   EKEYTYPE,X'00'      ESTIMATE RECORDS                             
         MVC   EKEYAM,STKAGMD                                                   
         MVC   EKEYCLT,STKCLT                                                   
         MVC   EKEYPRD,0(R6)                                                    
         MVC   EKEYEST,STKEST                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(8),KEY      ESTIMATE EXIST?                              
         BE    CLSTAT40                                                         
         MVI   CLSNUM,1                                                         
         B     CLSTAT60                                                         
*                                                                               
CLSTAT40 MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         B     CLSTAT10            MOVE ONTO THE NEXT STAT RECORD               
*                                                                               
CLSTAT60 MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',KEY+14,AREC,DMWORK            
         BAS   RE,DELETE                                                        
         LA    R7,1(R7)                                                         
         B     CLSTAT10            NEXT RECORD                                  
*********************************************************************           
*        CLPGST: PGEST RECORD CLOSEOUTS                             *           
*********************************************************************           
CLPGST   DS    0H                                                               
         ST    R7,DELSTAT                                                       
         SR    R7,R7                                                            
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING PGESTD,R2                                                        
*                                                                               
         MVC   P+2(24),=24CL2'* '                                               
         GOTO1 REPORT                                                           
         MVC   P+12(5),=C'PGEST'                                                
         GOTO1 REPORT                                                           
         MVC   P+2(24),=24CL2'* '                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   KEY(2),=X'0D5D'                                                  
         GOTO1 HIGH                                                             
         B     CLPGST15                                                         
*                                                                               
CLPGST10 DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
CLPGST15 CLC   KEYSAVE(2),KEY                                                   
         BNE   CLSCSO                                                           
*                                                                               
         MVC   SVKEY,KEY                                                        
         LA    R2,KEY                                                           
         CLI   PGKPRD,X'00'                                                     
         BE    CLPGST10                                                         
         CLI   PGKEST,X'00'                                                     
         BE    CLPGST10                                                         
*                                                                               
         LA    R2,SVKEY                                                         
*                                                                               
CLPGST30 XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING ESTHDR,R3                                                        
         MVI   EKEYTYPE,X'00'      ESTIMATE RECORDS                             
         MVC   EKEYAM,PGKAM                                                     
         MVC   EKEYCLT,PGKCLT                                                   
         MVC   EKEYPRD,PGKPRD                                                   
         MVC   EKEYEST,PGKEST                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(8),KEY      ESTIMATE EXIST?                              
         BE    CLPGST40                                                         
         MVI   CLSNUM,1                                                         
         B     CLPGST60                                                         
*                                                                               
CLPGST40 MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         B     CLPGST10            MOVE ONTO THE NEXT PGEST RECORD              
*                                                                               
CLPGST60 MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',KEY+14,AREC,DMWORK            
         BAS   RE,DELETE                                                        
         LA    R7,1(R7)                                                         
         B     CLPGST10            NEXT RECORD                                  
*********************************************************************           
*        CLSCSO: CHILD SPOT RECORDS CLOSEOUT                        *           
*********************************************************************           
CLSCSO   DS    0H                                                               
         ST    R7,DELPGST                                                       
         SR    R7,R7                                                            
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING CSORECD,R2                                                       
*                                                                               
         MVC   P+2(24),=24CL2'* '                                               
         GOTO1 REPORT                                                           
         MVC   P+12(10),=C'CHILD SPOT'                                          
         GOTO1 REPORT                                                           
         MVC   P+2(24),=24CL2'* '                                               
         GOTO1 REPORT                                                           
*                                                                               
CLSCSOA  MVC   KEY(2),=X'0D49'     CHILD SPOT PROGRAM RECORD                    
         MVI   RECFLAG,C'B'                                                     
         GOTO1 HIGH                                                             
         B     CLSCSO15                                                         
*                                                                               
CLSCSOB  XC    KEY,KEY                                                          
         XC    SVKEY,SVKEY                                                      
         MVC   KEY(2),=X'0D58'     HEADER/FOOTLINE RECORD                       
         MVI   RECFLAG,C'C'                                                     
         GOTO1 HIGH                                                             
         B     CLSCSO15                                                         
*                                                                               
CLSCSOC  XC    KEY,KEY                                                          
         XC    SVKEY,SVKEY                                                      
         MVC   KEY(2),=X'0D57'     DEMO OVER-RIDE RECORD                        
         MVI   RECFLAG,C'D'                                                     
         GOTO1 HIGH                                                             
         B     CLSCSO15                                                         
*                                                                               
CLSCSOD  XC    KEY,KEY                                                          
         XC    SVKEY,SVKEY                                                      
         MVC   KEY(2),=X'0D5C'     NTPCALC RECORD                               
         MVI   RECFLAG,C'X'                                                     
         GOTO1 HIGH                                                             
         B     CLSCSO15                                                         
*                                                                               
CLSCSO10 DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
CLSCSO15 CLC   KEYSAVE(2),KEY                                                   
         BE    CLSCSO20                                                         
*                                                                               
CLSCSO17 CLI   RECFLAG,C'B'                                                     
         BE    CLSCSOB                                                          
         CLI   RECFLAG,C'C'                                                     
         BE    CLSCSOC                                                          
         CLI   RECFLAG,C'D'                                                     
         BE    CLSCSOD                                                          
         CLI   RECFLAG,C'X'                                                     
         BE    CLSBWS                                                           
*                                                                               
CLSCSO20 MVC   SVKEY,KEY                                                        
         LA    R2,SVKEY                                                         
         CLI   10(R2),X'00'                                                     
         BE    CLSCSO10                                                         
*                                                                               
CLSCSO30 XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING ESTHDR,R3                                                        
         MVI   EKEYTYPE,X'00'      ESTIMATE RECORDS                             
         MVC   EKEYAM,2(R2)        AGY/MD                                       
         MVC   EKEYCLT,3(R2)       CLT                                          
         MVC   EKEYPRD,=C'POL'     PRD = POL                                    
         MVC   EKEYEST,10(R2)      EST                                          
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(8),KEY      ESTIMATE EXIST?                              
         BE    CLSCSO40                                                         
         MVI   CLSNUM,1                                                         
         B     CLSCSO60                                                         
*                                                                               
CLSCSO40 MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         B     CLSCSO10            MOVE ONTO THE NEXT STAT RECORD               
*                                                                               
CLSCSO60 MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',KEY+14,AREC,DMWORK            
         BAS   RE,DELETE                                                        
         LA    R7,1(R7)                                                         
         B     CLSCSO10            NEXT RECORD                                  
*********************************************************************           
*        CLSBWS: CLOSEOUT BUYER WORKSHEET RECORD                    *           
*********************************************************************           
CLSBWS   DS    0H                                                               
         ST    R7,DELCSO                                                        
         SR    R7,R7                                                            
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING CAMRECD,R2                                                       
*                                                                               
         MVC   P+2(24),=24CL2'* '                                               
         GOTO1 REPORT                                                           
         MVC   P+12(3),=C'BWS'                                                  
         GOTO1 REPORT                                                           
         MVC   P+2(24),=24CL2'* '                                               
         GOTO1 REPORT                                                           
*                                                                               
         MVC   KEY(2),=X'0DE6'     NEW BUYER'S WORKSHEET CAMPAIGN REC           
         GOTO1 HIGH                PASSIVE POINTER                              
         B     CLSBWS15                                                         
*                                                                               
CLSBWS10 DS    0H                                                               
         GOTO1 SEQ                                                              
*                                                                               
CLSBWS15 CLC   KEYSAVE(2),KEY                                                   
         BNE   REQFX                                                            
*                                                                               
         MVC   SVKEY,KEY                                                        
         LA    R2,SVKEY                                                         
         CLI   CAMPKEST,X'00'                                                   
         BE    CLSBWS10                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING CLTHDR,R3                                                        
*                                                                               
         MVI   CKEYTYPE,X'00'      CLIENT RECORD                                
         MVC   CKEYAM,CAMPKAM                                                   
         MVC   CKEYCLT,CAMPKCLT                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BE    *+8                                                              
         B     CLSBWS30                                                         
*                                                                               
         L     R3,ADCLT                                                         
         GOTO1 GETCLT                                                           
*                                                                               
         LA    R6,CLIST            MATCH AGAINST CLIST                          
CLSBWS20 CLC   3(1,R6),CAMPKPRD                                                 
         BE    CLSBWS25                                                         
         LA    R6,4(R6)                                                         
         CLI   0(R6),0                                                          
         BNE   CLSBWS20                                                         
         MVI   CLSNUM,1                                                         
         B     CLSBWS30                                                         
*                                                                               
CLSBWS25 XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING ESTHDR,R3                                                        
         MVI   EKEYTYPE,X'00'      ESTIMATE RECORDS                             
         MVC   EKEYAM,CAMPKAM                                                   
         MVC   EKEYCLT,CAMPKCLT                                                 
         MVC   EKEYPRD,0(R6)                                                    
         MVC   EKEYEST,CAMPKEST                                                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(8),KEY      ESTIMATE EXIST?                              
         BNE   CLSBWS30            CONTINUE PROCESSING BWS                      
*                                                                               
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         B     CLSBWS10            MOVE ONTO THE NEXT CAMPAIGN RECORD           
*                                                                               
CLSBWS30 MVC   KEY,SVKEY           SVKEY HAS CAMPAIGN KEY                       
         MVI   CLSNUM,1                                                         
         GOTO1 HIGH                                                             
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',KEY+14,AREC,DMWORK            
         L     R2,AREC                                                          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING BWHRECD,R4          HEADER RECORD                                
         MVC   KEY(2),=X'0D67'                                                  
         MVC   BWHKAGMD,CAMKAGMD                                                
         MVC   BWHKBYR,CAMKBYR                                                  
         MVC   BWHKCAM,CAMKCAM                                                  
*                                                                               
         GOTO1 HIGH                                                             
         B     CLSBWS40                                                         
*                                                                               
CLSBWS35 GOTO1 SEQ                                                              
*                                                                               
CLSBWS40 CLC   KEY(BWHKMKT-BWHKEY),KEYSAVE                                      
         BNE   CLSBWS80                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',KEY+14,AREC,DMWORK            
         L     R4,AREC                                                          
         MVC   SVKEY2,KEY          SVKEY2 HAS HEADER KEY                        
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING BWDRECD,R5                                                       
         MVI   BWDKTYP,BWDKTYPQ    READ ALL DETAIL RECORDS                      
         MVI   BWDKSUB,BWDKSUBQ                                                 
         MVC   BWDKAGMD,BWHKAGMD-BWHKEY(R4)                                     
         MVC   BWDKBYR,BWHKBYR-BWHKEY(R4)                                       
         MVC   BWDKSEQ,BWHKSEQ-BWHKEY(R4)                                       
         GOTO1 HIGH                                                             
         B     CLSBWS50                                                         
*                                                                               
CLSBWS45 GOTO1 SEQ                                                              
*                                                                               
CLSBWS50 CLC   KEY(BWDKEL-BWDKEY),KEYSAVE                                       
         BNE   CLSBWS60                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',KEY+14,AREC,DMWORK            
         BAS   RE,DELETE           DELETE DETAIL RECORD                         
         LA    R7,1(R7)                                                         
         B     CLSBWS45                                                         
*                                                                               
CLSBWS60 MVC   KEY,SVKEY2                                                       
         GOTO1 HIGH                                                             
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',KEY+14,AREC,DMWORK            
         BAS   RE,DELETE                                                        
         LA    R7,1(R7)                                                         
         B     CLSBWS35            NEXT HEADER RECORD                           
*                                                                               
CLSBWS80 MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',KEY+14,AREC,DMWORK            
         BAS   RE,DELETE                                                        
         LA    R7,1(R7)                                                         
         B     CLSBWS10            NEXT CAMPAIGN                                
*                                                                               
         EJECT                                                                  
*********************************************************************           
*        REQFX: PRINT OUT THE NUMBER OF RECORDS                     *           
*********************************************************************           
REQFX    ST    R7,DELBWS                                                        
         MVC   P+15(20),=20C'*'                                                 
         GOTO1 REPORT                                                           
         MVC   P+18(14),=C'RECORDS CLOSED'                                      
         GOTO1 REPORT                                                           
         MVC   P+15(3),=C'MSR'                                                  
         EDIT  DELMSR,(10,P+27),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK              
         GOTO1 REPORT                                                           
         MVC   P+15(3),=C'BGR'                                                  
         EDIT  DELBGR,(10,P+27),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK              
         GOTO1 REPORT                                                           
         MVC   P+15(4),=C'DARE'                                                 
         EDIT  DELDARE,(10,P+27),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK             
         GOTO1 REPORT                                                           
         MVC   P+15(6),=C'STATUS'                                               
         EDIT  DELSTAT,(10,P+27),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK             
         GOTO1 REPORT                                                           
         MVC   P+15(5),=C'PGEST'                                                
         EDIT  DELPGST,(10,P+27),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK             
         GOTO1 REPORT                                                           
         MVC   P+15(10),=C'CHILD SPOT'                                          
         EDIT  DELCSO,(10,P+27),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK              
         GOTO1 REPORT                                                           
         MVC   P+15(3),=C'BWS'                                                  
         EDIT  DELBWS,(10,P+27),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK              
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
         GOTO1 HEXOUT,DMCB,KEY,P+2,13                                           
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
DEL11    CLI   QOPT1,C'L'                                                       
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
*                                                                               
DELX     XIT1                                                                   
**********************************************************************          
*        CLOSING OUT XSPOT RECORDS                                   *          
**********************************************************************          
XDEL     NTR1                                                                   
*                                                                               
*        BAS   RE,PRTDEL                                                        
                                                                                
         GOTO1 HEXOUT,DMCB,XKEY,P+2,20                                          
         CLI   CLSNUM,1                                                         
         BNE   *+14                                                             
         MVC   P+50(21),=C'ESTIMATE WAS DELETED!'                               
         B     XDEL10                                                           
*                                                                               
         CLI   CLSNUM,2                                                         
         BNE   *+14                                                             
         MVC   P+50(21),=C'CLOSE DARE MAKE GOODS'                               
         B     XDEL10                                                           
*                                                                               
         MVC   P+50(19),=C'ESTIMATE DATES OFF!'                                 
*                                                                               
XDEL10   GOTO1 REPORT                                                           
         CLI   QOPT1,C'L'          LIVE?                                        
         BNE   XDELX                                                            
*                                                                               
         LA    R1,XKEY             THEN DELETE                                  
         OI    32(R1),X'C0'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'XSPDIR',XKEYSAVE,XKEY                  
         TM    8(R1),X'40'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AREC                                                          
         OI    34(R1),X'C0'        X'C0' MARKS RECORD AS CLOSED OUT             
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'XSPFIL',XKEY,AREC,DMWORK              
         TM    8(R1),X'40'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
XDELX    XIT1                                                                   
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
**PAN#1  DC    CL21'099SPREPFXCO 05/01/02'                                      
         END                                                                    
