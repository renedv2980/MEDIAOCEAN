*          DATA SET SPREPFXDST AT LEVEL 045 AS OF 05/01/02                      
*PHASE SPFX025                                                                  
         TITLE 'SPFX02 - DELETE STATION REPS WITH INVALID AS NAME'              
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
EXIT     XIT1                                                                   
*                                                                               
REQF     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* - REQL -                                                                      
*                                                                               
REQL     DS    0H                                                               
         ZAP   RECCNT,=P'0'                                                     
         XC    KEY,KEY             CLEAR KEY                                    
         LA    R3,KEY                                                           
         USING REPRECD,R3                                                       
         MVI   REPKTYPE,C'R'       REP RECORD                                   
         GOTO1 HIGHSTA                                                          
         B     REQL20                                                           
*                                                                               
REQL10   GOTO1 SEQSTA              GET NEXT RECORD                              
REQL20   L     R3,ADSTAT           ADSTAT POINTS TO RECORD FOUND                
         CLI   0(R3),C'R'          REP REC?                                     
         BNE   REQL90                                                           
         CLC   REPKAGY,=C'JW'      JW RECS ONLY                                 
         BNE   REQL10                                                           
         CLC   =C'INVALID',RNAME   DELETE IF NAME=INVALID                       
         BNE   REQL10                                                           
*                                                                               
*  DELETE REC                                                                   
*                                                                               
REQL70   DS    0H                                                               
         AP    RECCNT,=P'1'                                                     
         OI    17(R3),X'80'        MARK DELETED                                 
         BAS   RE,PRINTIT          PRINT IT                                     
         CLI   RCWRITE,C'Y'                                                     
         BNE   REQL10                                                           
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'STATION',ADSTAT,ADSTAT                 
         B     REQL10                                                           
*                                                                               
REQL90   MVC   P(7),=C'ERRORS='                                                 
         EDIT  RECCNT,(12,P+10)                                                 
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* - PRINT OUT DETAILS OF STATION RECORD TO DELETE                               
*                                                                               
PRINTIT  NTR1                                                                   
         MVC   P(4),=C'MED='                                                    
         MVC   P+4(1),REPKMED                                                   
         MVC   P+7(4),=C'REP='                                                  
         MVC   P+11(3),REPKREP                                                  
         MVC   P+16(5),=C'NAME='                                                
         MVC   P+21(22),RNAME                                                   
*        MVC   P+50(30),0(R3)                                                   
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* CONSTANTS                                                                     
*                                                                               
         LTORG                                                                  
RECCNT   DS    PL6                                                              
MKTSTA   DS    XL5                                                              
MAKT     DS    CL4                                                              
STATN    DS    CL5                                                              
         DS    XL1                                                              
         EJECT                                                                  
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045SPREPFXDST05/01/02'                                      
         END                                                                    
