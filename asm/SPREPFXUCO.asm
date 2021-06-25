*          DATA SET SPREPFXUCO AT LEVEL 125 AS OF 05/01/02                      
***********************************************************************         
*        RESTORE RECORDS WE CLOSED WHEN ESTIMATES WERE SUSPECTED TO             
*        HAVE BEEN RE-USED!!!                                                   
***********************************************************************         
*PHASE SPFX02W                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'SPFX02 - UNCLOSE'                                               
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
         MVC   P+12(11),=C'UNCLOSE MSR'                                         
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
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'XSPDIR',XKEY,XKEY,0           
         B     REQF15                                                           
*                                                                               
REQF10   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRSEQ'),=C'XSPDIR',XKEY,XKEY,0           
REQF15   DS    0H                                                               
         CLC   XKEY(2),XKEYSAVE    ARE WE STILL ON MSR?                         
         BNE   REQF45                                                           
         CLI   MSRKPRD,0           IF RECORD HAS NULLS FOR ESTIMATE             
         BE    REQF10              OR PRODUCT, SKIP IT.                         
         CLI   MSRKEST,0                                                        
         BE    REQF10                                                           
*                                                                               
         TM    XKEY+32,X'C0'                                                    
         BNO   REQF10              NOT CLOSED? NEXT REC                         
*                                  CLOSED, LOOK FOR ESTIMATE                    
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
         GOTO1 HIGH                ESTIMATE NOT FOUND? GOOD CLOSE               
         CLC   KEYSAVE(8),KEY      SKIP ENTIRE SET                              
         BNE   REQF33                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'XSPDIR',XKEY,XKEY,0           
         CLC   XKEY(32),SVXKEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',=C'GETREC'),=C'XSPFIL',MSRDDA,AREC, +        
               DMWORK                                                           
         B     REQF35                                                           
*                                                                               
REQF33   LA    R6,XKEY             DONT UNCLOSE THIS SET                        
         CLI   15(R6),X'FF'                                                     
         BE    REQF10              NEXT SET                                     
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRSEQ'),=C'XSPDIR',XKEY,XKEY,0           
         B     REQF33                                                           
*                                                                               
REQF35   LA    R6,XKEY             UNCLOSE THE ENTIRE SET                       
         CLI   15(R6),X'FF'                                                     
         BE    REQF40                                                           
         GOTO1 =A(XUNDEL)                                                       
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRSEQ'),=C'XSPDIR',XKEY,XKEY,0           
         GOTO1 DATAMGR,DMCB,(X'08',=C'GETREC'),=C'XSPFIL',MSRDDA,AREC, +        
               DMWORK                                                           
         B     REQF35                                                           
*                                                                               
REQF40   GOTO1 =A(XUNDEL)                                                       
         LA    R7,1(R7)                                                         
         B     REQF10                                                           
*                                                                               
         DROP  R2                                                               
*********************************************************************           
*        CHECKING BGR RECS                                                      
*********************************************************************           
REQF45   ST    R7,DELMSR                                                        
         SR    R7,R7                                                            
         MVC   P+2(24),=24CL2'* '                                               
         GOTO1 REPORT                                                           
         MVC   P+12(11),=C'UNCLOSE BGR'                                         
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
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'XSPDIR',XKEY,XKEY,0           
         B     REQF55                                                           
*                                                                               
REQF50   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRSEQ'),=C'XSPDIR',XKEY,XKEY,0           
REQF55   DS    0H                                                               
         CLC   XKEY(2),XKEYSAVE    ARE WE STILL ON BGR?                         
         BNE   REQF90              !!!!!! GOTO DARE                             
         TM    XKEY+32,X'C0'                                                    
         BNO   REQF50                                                           
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
         GOTO1 HIGH                ESTIMATE NOT FOUND? GOOD CLOSE               
         CLC   KEYSAVE(8),KEY      SKIP ENTIRE SET                              
         BNE   REQF73                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'XSPDIR',XKEY,XKEY,0           
         CLC   XKEY(32),SVXKEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',=C'GETREC'),=C'XSPFIL',BGRDDA,AREC, +        
               DMWORK                                                           
         B     REQF75                                                           
*                                                                               
REQF73   LA    R6,XKEY             DONT CLOSE OUT THIS SET                      
         CLI   15(R6),X'FF'                                                     
         BE    REQF50              NEXT SET                                     
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRSEQ'),=C'XSPDIR',XKEY,XKEY,0           
         B     REQF73                                                           
*                                                                               
REQF75   LA    R6,XKEY             CLOSE OUT THE ENTIRE SET                     
         CLI   15(R6),X'FF'                                                     
         BE    REQF80                                                           
         GOTO1 =A(XUNDEL)                                                       
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRSEQ'),=C'XSPDIR',XKEY,XKEY,0           
         GOTO1 DATAMGR,DMCB,(X'08',=C'GETREC'),=C'XSPFIL',BGRDDA,AREC, +        
               DMWORK                                                           
         B     REQF75                                                           
*                                                                               
REQF80   GOTO1 =A(XUNDEL)                                                       
         LA    R7,1(R7)                                                         
         B     REQF50                                                           
*                                                                               
         DROP  R2                                                               
*********************************************************************           
*        CHECK DARE RECORDS                                         *           
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
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'SPTDIR',KEY,KEY,0             
         CLC   KEY(2),KEYSAVE      ARE WE STILL ON DARE REC'S?                  
         BNE   REQFX               NO? WE ARE DONE WITH UNCLOSE                 
         B     REQF110                                                          
*                                                                               
REQF100  DS    0H                                                               
         LA    R2,KEY                                                           
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRSEQ'),=C'SPTDIR',KEY,KEY,0             
         CLC   KEY(2),KEYSAVE                                                   
         BNE   REQFX                                                            
*                                                                               
REQF110  CLI   DCKPRD,0            SKIP RECORDS WITH PRODUCT = NULLS            
         BE    REQF100                                                          
         CLI   DCKEST,0            SKIP RECORDS WITH ESTIMATE = NULLS           
         BE    REQF100                                                          
*                                                                               
         TM    KEY+13,X'C0'                                                     
         BNO   REQF100                                                          
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
         CLC   KEYSAVE(8),KEY      ESTIMATE FOUND? BEGIN UNCLOSE                
         BE    REQF130             (WHERE ALL THE ACTION IS)                    
**********************************************************************          
*        THIS IS WHERE WE SHOULD PUT IN THE COMMENT REC CODES        *          
**********************************************************************          
         MVC   KEY,SVKEY           GO ON TO NEXT REC IF EST NOT FOUND           
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'SPTDIR',KEY,KEY,0             
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',KEY+14,AREC,DMWORK            
         L     R2,AREC                                                          
         USING DAREORDD,R2         WE NEED TO ALSO CLOSE COMMENT REC'S          
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R2)                                                    
         MVI   KEY+12,X'01'                                                     
         GOTO1 HIGH                                                             
         B     COMX20                                                           
COMX10   GOTO1 SEQ                                                              
         DS    0H                                                               
COMX20   CLC   KEY(13),KEYSAVE                                                  
         BNE   COMX30                                                           
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',KEY+14,AREC,DMWORK            
         BAS   RE,DELETE                                                        
         B     COMX10                                                           
*                                                                               
COMX30   XC    KEY,KEY                                                          
         MVC   KEY(13),0(R2)                                                    
         MVI   KEY+12,X'02'                                                     
         GOTO1 HIGH                                                             
         B     COMX50                                                           
COMX40   GOTO1 SEQ                                                              
         DS    0H                                                               
COMX50   CLC   KEY(13),KEYSAVE                                                  
         BNE   COMX60                                                           
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',KEY+14,AREC,DMWORK            
         BAS   RE,DELETE                                                        
         B     COMX40                                                           
*                                                                               
COMX60   MVC   KEY,SVKEY           GO ON TO NEXT CLOSED DARE RECORD             
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'SPTDIR',KEY,KEY,0             
         B     REQF100                                                          
**********************************************************************          
*                                                                               
REQF130  MVC   KEY,SVKEY                                                        
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'SPTDIR',KEY,KEY,0             
         GOTO1 DATAMGR,DMCB,(X'08',=C'GETREC'),=C'SPTFIL',KEY+14,AREC, +        
               DMWORK                                                           
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
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'SPTDIR',KEY,KEY,0             
         B     REQF130B                                                         
         DROP  R6                                                               
*                                                                               
REQF130A MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRSEQ'),=C'SPTDIR',KEY,KEY,0             
*                                                                               
REQF130B CLC   KEY(10),KEYSAVE     SAME ORDER NUMBER, BUYER CODE?               
         BNE   REQF140                                                          
         GOTO1 DATAMGR,DMCB,(X'08',=C'GETREC'),=C'SPTFIL',KEY+14,AREC, +        
               DMWORK                                                           
         BAS   RE,UNDELETE                                                      
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
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'SPTDIR',KEY,KEY,0             
         B     REQF140B                                                         
*                                                                               
REQF140A MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRSEQ'),=C'SPTDIR',KEY,KEY,0             
*                                                                               
REQF140B CLC   KEY(7),KEYSAVE      SAME ORDER NUMBER?                           
         BNE   REQF150                                                          
REQF140C GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',KEY+14,AREC,DMWORK            
         BAS   RE,UNDELETE                                                      
         B     REQF140A                                                         
*                                                                               
REQF150  MVC   KEY,SVKEY                                                        
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'SPTDIR',KEY,KEY,0             
         GOTO1 DATAMGR,DMCB,(X'08',=C'GETREC'),=C'SPTFIL',KEY+14,AREC, +        
               DMWORK                                                           
         BAS   RE,UD               DARE ORDER SPECIFIC UNDEL ROUTINE            
         MVC   KEY,SVKEY                                                        
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'SPTDIR',KEY,KEY,0             
         LA    R7,1(R7)                                                         
         B     REQF100             NEXT RECORD                                  
         EJECT                                                                  
*********************************************************************           
*        REQFX: PRINT OUT THE NUMBER OF RECORDS                     *           
*********************************************************************           
REQFX    ST    R7,DELDARE                                                       
         MVC   P+15(20),=20C'*'                                                 
         GOTO1 REPORT                                                           
         MVC   P+18(16),=C'RECORDS UNCLOSED'                                    
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
*        UNCLOSE DARE ORDER AND ALL THE STUPID PASSIVE KEYS          *          
**********************************************************************          
UD       NTR1                                                                   
         XC    KEY,KEY                                                          
*                                                                               
* 1ST: THE MAIN KEY                                                             
*                                                                               
         L     R2,AREC                                                          
         USING DAREORDD,R2                                                      
         MVC   KEY(13),0(R2)                                                    
         BAS   RE,UNCKEY                                                        
*                                                                               
* 2ND: THE PASSIVE POINTER BY BUYER                                             
*                                                                               
UD10     XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0DB4'                                                  
         MVC   KEY+2(1),DOKAGMD                                                 
         L     R6,AREC                                                          
         USING DOIDELD,R6                                                       
         MVI   ELCODE,X'01'        GET PRIMARY ID ELEMENT FOR BYR ID            
         BAS   RE,GETEL                                                         
         MVC   KEY+3(3),DOIDBYR                                                 
         DROP  R6                                                               
         MVC   KEY+6(4),DOKORDER                                                
         MVC   KEY+10(3),DOKSTA                                                 
         BAS   RE,UNCKEY                                                        
*                                                                               
* 3RD: PASSIVE POINTER BY CLIENT, WHICH IS IN SVKEY                             
*                                                                               
UD20     MVC   KEY,SVKEY                                                        
         BAS   RE,UNCKEY                                                        
*                                                                               
* 4TH    PASSIVE POINTER BY POL ORDER # + BRAND ORDER #.                        
*                                                                               
UD30     XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0DB6'                                                  
         MVC   KEY+2(1),DOKAGMD                                                 
         MVC   KEY+3(4),DOKORDER                                                
         BAS   RE,UNCKEY                                                        
*                                                                               
* 5TH    PASSIVE POINTER BY STATION                                             
*                                                                               
UD40     XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0DB7'                                                  
         MVC   KEY+2(1),DOKAGMD                                                 
         L     R6,AREC                                                          
         USING DOIDELD,R6                                                       
         MVI   ELCODE,X'01'        GET PRIMARY ID ELEMENT FOR BYR ID            
         BAS   RE,GETEL                                                         
         MVC   KEY+3(3),DOIDBYR                                                 
         DROP  R6                                                               
         MVC   KEY+6(3),DOKSTA                                                  
         MVC   KEY+9(4),DOKORDER                                                
         BAS   RE,UNCKEY                                                        
*                                                                               
         CLI   QOPT1,C'L'          LIVE UPDATE?                                 
         BNE   UDX                                                              
         L     R1,AREC                                                          
         NI    15(R1),X'FF'-X'C0'                                               
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'SPTFIL',KEY,AREC,DMWORK               
*                                                                               
UDX      XIT1                                                                   
**********************************************************************          
UNCKEY   NTR1                                                                   
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI'),=C'SPTDIR',KEY,KEY,0             
         CLC   KEYSAVE(13),KEY                                                  
         BNE   UNCKEYX                                                          
         GOTO1 HEXOUT,DMCB,KEY,P+2,13                                           
         GOTO1 REPORT                                                           
         CLI   QOPT1,C'L'          LIVE UPDATE?                                 
         BNE   UNCKEYX                                                          
         LA    R1,KEY                                                           
         NI    13(R1),X'FF'-X'C0'                                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR',KEYSAVE,KEY                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UNCKEYX   XIT1                                                                  
**********************************************************************          
*        CLOSING DARE SPOT RECORDS                                   *          
**********************************************************************          
UNDELETE NTR1                                                                   
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY,P+2,13                                           
*                                                                               
         GOTO1 REPORT                                                           
         CLI   QOPT1,C'L'                                                       
         BNE   UNDELX                                                           
*                                                                               
         LA    R1,KEY                                                           
         NI    13(R1),X'FF'-X'C0'                                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR',KEYSAVE,KEY                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AREC                                                          
         NI    15(R1),X'FF'-X'C0'                                               
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'SPTFIL',KEY,AREC,DMWORK               
*                                                                               
UNDELX   XIT1                                                                   
**********************************************************************          
*        CLOSING OUT XSPOT RECORDS                                   *          
**********************************************************************          
XUNDEL   NTR1                                                                   
*                                                                               
         GOTO1 HEXOUT,DMCB,XKEY,P+2,20                                          
*                                                                               
         GOTO1 REPORT                                                           
         CLI   QOPT1,C'L'          LIVE?                                        
         BNE   XUNDELX                                                          
*                                                                               
         LA    R1,XKEY             THEN UNCLOSE                                 
         NI    32(R1),X'FF'-X'C0'                                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'XSPDIR',XKEYSAVE,XKEY                  
         TM    8(R1),X'40'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AREC                                                          
         NI    34(R1),X'FF'-X'C0'                                               
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'XSPFIL',XKEY,AREC,DMWORK              
         TM    8(R1),X'40'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
XUNDELX  XIT1                                                                   
**********************************************************************          
*        CLOSE THE DARE COMMENT RECORDS LEFT OVER FROM FIX           *          
**********************************************************************          
DELETE   NTR1                                                                   
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY,P+2,13                                           
         MVC   P+50(7),=C'CLOSED!'                                              
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
*                                                                               
         L     R1,AREC                                                          
         OI    15(R1),X'C0'                                                     
         GOTO1 DATAMGR,DMCB,=C'PUTREC',=C'SPTFIL',KEY,AREC,DMWORK               
*                                                                               
DELX     XIT1                                                                   
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
**PAN#1  DC    CL21'125SPREPFXUCO05/01/02'                                      
         END                                                                    
