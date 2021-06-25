*          DATA SET SPREPFXST1 AT LEVEL 070 AS OF 05/01/02                      
*PHASE SPFX02C                                                                  
         TITLE 'SPFX02 - FIX CCUSA RADIO MARKETS'                               
SPFX02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC                                                      
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
EXIT     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
* REQF - CLEAR RECORD COUNTER                                                   
*                                                                               
REQF     DS    0H                                                               
         ZAP   RECCNT,=P'0'                                                     
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* - REQL - GO THROUGH STATION MASTER RECORDS                                    
*                                                                               
REQL     XC    KEY,KEY            CLEAR KEY                                     
         LA    R3,KEY                                                           
         USING STARECD,R3                                                       
         MVI   STAKTYPE,C'S'      STATION RECORD                                
         MVC   STAKMED,QMED       MEDIA                                         
         GOTO1 HIGHSTA                                                          
         L     R3,ADSTAT          ADSTAT POINTS TO RECORD FOUND                 
         B     REQL20                                                           
*                                                                               
REQL10   GOTO1 SEQSTA             GET NEXT RECORD                               
*                                                                               
REQL20   CLC   0(STAKCALL-STAKEY,R3),KEY                                        
         BNE   REQL90             MAKE SURE STILL C'S' AND SAME MEDIA           
*                                                                               
         CLC   =C'IR',STAKAGY     MAKE SURE YOU HAVE CORRECT AGENCY             
         BNE   REQL10                                                           
         CLC   SPAYREP,=C'000'                                                  
         BE    REQL10                                                           
         OC    SPAYREP,SPAYREP                                                  
         BZ    REQL10                                                           
         CLC   SCONREP,=C'000'                                                  
         BE    REQL30                                                           
         OC    SCONREP,SPAYREP                                                  
         BZ    REQL30                                                           
         MVC   P(5),=C'ERROR'                                                   
         GOTO1 REPORT                                                           
         BAS   RE,PRINTIT                                                       
         B     REQL10                                                           
*                                                                               
REQL30   AP    RECCNT,=P'1'                                                     
         MVC   SCONREP,SPAYREP                                                  
         MVC   SPAYREP,=C'000'                                                  
         BAS   RE,PRINTIT                                                       
         CLI   RCWRITE,C'Y'                                                     
         BNE   REQL40                                                           
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'STATION',ADSTAT,ADSTAT                 
*                                                                               
REQL40   B     REQL10             GET NEXT STATION RECORD                       
*                                                                               
REQL90   MVC   P(5),=C'TOTAL'                                                   
         EDIT  RECCNT,(12,P+10)                                                 
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
* - PRINT OUT DETAILS OF STATION RECORD TO DELETE                               
*                                                                               
PRINTIT  NTR1                                                                   
         GOTO1 HEXOUT,DMCB,0(R3),P,17,0                                         
         MVC   P+40(4),=C'STA='                                                 
         MVC   P+45(5),STAKCALL      STATION                                    
         MVC   P+54(7),=C'PAY REP'                                              
         MVC   P+62(3),SPAYREP       PAYING REP                                 
         MVC   P+70(8),=C'TIME REP'                                             
         MVC   P+80(3),SCONREP       TIME SHEET REP                             
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* CONSTANTS                                                                     
*                                                                               
         LTORG                                                                  
RECCNT   DS    PL6                                                              
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'070SPREPFXST105/01/02'                                      
         END                                                                    
