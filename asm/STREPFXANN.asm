*          DATA SET STREPFXANN AT LEVEL 012 AS OF 12/17/98                      
*PHASE SPFX02A                                                                  
*INCLUDE BINSRCH2                                                               
         TITLE 'CHECK OMDTO FOR DUP STATION #S'                                 
SPFX02   CSECT                                                                  
         PRINT NOGEN                                                            
         DS    8192C                                                            
         ORG   SPFX02                                                           
         NMOD1 0,SPFX02,R8                                                      
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,REQFRST                                                     
         BE    FX                                                               
*                                                                               
EXIT     XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
FX       DS    0H                                                               
         GOTO1 ,BINPARMS,,TABLE,0,7,(0,2),5000                                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING STARECD,R5                                                       
         MVI   STXKTYPE,C'X'                                                    
         MVC   STXKAGY,QAGY                                                     
         MVC   SAVEKEY,KEY                                                      
         GOTO1 HIGHSTA             READ FIRST RECORD.                           
         B     FX420                                                            
*                                                                               
FX420SEQ GOTO1 SEQSTA                                                           
FX420    DS    0H                                                               
         L     R5,ADSTAT               ADSTAT-->RECORD FOUND.                   
         CLC   STXKTYPE(3),SAVEKEY     TYP(1),AGY(1)                            
         BNE   FX1000                                                           
*                                                                               
         MVC   SEQNO,STXKNUM                                                    
         MVC   STATN,STXKSTA                                                    
*                                                                               
         GOTO1 BINSRCH,BINPARMS,(1,SEQNO)                                       
         OC    BINPARMS,BINPARMS   TABLE FULL                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   BINPARMS,1          REC NOT FOUND = GOOD                         
         BE    FX420SEQ                                                         
*                                                                               
         L     R2,BINPARMS                                                      
         MVC   P2(5),0(R2)                                                      
         MVC   P2+10(5),SEQNO                                                   
         GOTO1 REPORT                                                           
         B     FX420SEQ                                                         
*                                                                               
FX1000   MVC   P2(2),=C'OK'                                                     
         GOTO1 REPORT                                                           
         LA    R2,TABLE                                                         
FX1100   MVC   P2(5),2(R2)                                                      
*        MVC   P2+10(2),0(R2)                                                   
         GOTO1 HEXOUT,DMCB,0(R2),P2+10,2,=C'TOG'                                
         GOTO1 REPORT                                                           
         LA    R2,7(R2)                                                         
         OC    0(5,R2),0(R2)                                                    
         BZ    FX2000                                                           
         B     FX1100                                                           
*                                                                               
FX2000   GOTO1 AENDREQ                                                          
***********************************************************************         
         LTORG                                                                  
BINPARMS DS    6F                  BINSRCH PARAMETERS - MARKETS                 
SAVEKEY  DS    XL32                                                             
SEQNO    DS    XL2                                                              
STATN    DS    CL5                                                              
TABLE    DS    5000XL(L'STATN+L'SEQ)                                            
         EJECT                                                                  
************************* FIXED-RECORDS DSECT *************************         
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012STREPFXANN12/17/98'                                      
         END                                                                    
