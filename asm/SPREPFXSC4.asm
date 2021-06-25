*          DATA SET SPREPFXSC4 AT LEVEL 004 AS OF 11/24/99                      
*                                                                               
***********************************************************************         
*        MATCH-COMPARE DARE REC'S ON LIVE SYS VS THOSE ON IM TAPES              
***********************************************************************         
*                                                                               
*PHASE SPFX02S                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'SPFX02 - COMPARISON REPORT LIVE VS TAPE'                        
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
         OPEN  (FILE,INPUT)                                                     
*                                                                               
         SR    R7,R7                                                            
         XC    COUNT,COUNT         COUNT OF MISMATCHED RECORDS                  
         LA    R1,TEMPIO                                                        
         ST    R1,AREC                                                          
*********************************************************************           
*        1: COMPARE TRANSMISSION DATE FROM TAPE TO ESTIMATE ON LIVE *           
*********************************************************************           
REQF10   GET   FILE,DBLOCK                                                      
*                                                                               
         MVC   SVKEY2(13),DBLOCK   RECORD KEY FROM TAPE                         
*                                                                               
         XC    SVKEY1,SVKEY1                                                    
         MVC   SVKEY1(13),DBLOCK+13                                             
         MVC   RECDATE,DBLOCK+26                                                
         LA    R2,SVKEY1           CLIENT PASSIVE KEY                           
         USING DAREORDD,R2                                                      
*                                                                               
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
REQF20   CLC   3(1,R6),DCKPRD                                                   
         BE    REQF25                                                           
         LA    R6,4(R6)                                                         
         CLI   R6,0                                                             
         BNE   *+6                                                              
         DC    H'0'                                                             
         B     REQF20                                                           
*                                                                               
REQF25   LA    R3,KEY                                                           
         USING ESTHDR,R3                                                        
         MVI   EKEYTYPE,X'00'      ESTIMATE RECORDS                             
         MVC   EKEYAM,DCKAGMD                                                   
         MVC   EKEYCLT,DCKCLT                                                   
         MVC   EKEYPRD,0(R6)                                                    
         MVC   EKEYEST,DCKEST                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(8),KEY      ESTIMATE FOUND?  TEST RE-ORDER               
         BE    REQF30                                                           
         B     REQF10              NEXT RECORD ON THE DATASET                   
*                                                                               
*********************************************************************           
*        COMPARE DARE RECORDS                                       *           
*********************************************************************           
REQF30   MVC   KEY,SVKEY1          READ WITH CLIENT PASSIVE POINTER             
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(12),KEY     ONLY COMPARE 12, LAST BYTE'S SPARE           
         BNE   REQF10                                                           
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',KEY+14,AREC,DMWORK            
         L     R6,AREC                                                          
         CLC   SVKEY2(13),0(R6)    COMPARE RECORD KEY, SHOULD BE SAME           
         BNE   PRTBAD                                                           
         B     REQF10              NEXT RECORD                                  
*                                                                               
         EJECT                                                                  
*********************************************************************           
*        REQFX: PRINT OUT THE NUMBER OF RECORDS                     *           
*********************************************************************           
REQFX    CLOSE FILE                                                             
         ST    R7,COUNT                                                         
         MVC   P+15(5),=C'COUNT'                                                
         EDIT  COUNT,(10,P+27),COMMAS=YES,ALIGN=LEFT,ZERO=NOBLANK               
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
**************************  REQL  ***********************************           
REQL     B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
         GETEL R6,24,ELCODE                                                     
         EJECT                                                                  
**********************************************************************          
*        PRINT OUT THE KEY OF THE BAD RECORDS                        *          
**********************************************************************          
PRTBAD   NTR1                                                                   
         GOTO1 HEXOUT,DMCB,DBLOCK,P+2,13                                        
         GOTO1 REPORT                                                           
         GOTO1 HEXOUT,DMCB,DBLOCK+13,P+30,13                                    
         GOTO1 REPORT                                                           
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
SVKEY1   DS    XL24                                                             
SVKEY2   DS    XL24                                                             
RECDATE  DS    CL6                                                              
ESTDATE  DS    CL6                                                              
RANGE    DS    CL6                 DATE USED TO COMPARE ESTIMATE                
CLSNUM   DS    X                   REASON OF RECORD BEING CLOSED OUT            
CLSNUM2  DS    X                                                                
DELDARE  DS    F                                                                
RECFLAG  DS    C                                                                
*                                                                               
DBLOCK   DS    CL40                                                             
FILE     DCB   DDNAME=EXTRACT,DSORG=PS,MACRF=PM,RECFM=VB,BLKSIZE=700,  X        
               LRECL=70,EODAD=REQFX                                             
TEMPIO   DS    XL2000                                                           
         SPACE 1                                                                
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENDRORD                                                     
         EJECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
DSETD    DSECT                                                                  
RECKEY   CL13                                                                   
PASKEY   CL13                                                                   
TRDATE   CL3                                                                    
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPREPFXSC411/24/99'                                      
         END                                                                    
