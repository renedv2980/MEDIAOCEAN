*          DATA SET ACREPXE02  AT LEVEL 011 AS OF 05/01/02                      
*PHASE ACXE02A                                                                  
         TITLE 'PRINT XJOBS THAT HAVE TYPE 50 TRANSACTIONS'                     
ACXE02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACXE**,RR=R5                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACXED,RC                                                         
         ST    R5,RELO                                                          
         EJECT                                                                  
         CLI   MODE,RUNFRST                                                     
         BNE   REQF                                                             
         L     RF,GETOPT                                                        
         MVC   0(2,RF),=X'07FE'                                                 
         MVI   FCPRORAT,C'N'                                                    
         MVI   FCSUPOFC,C'Y'                                                    
         B     XIT                                                              
*                                                                               
REQF     CLI   MODE,REQFRST                                                     
         BNE   ACCF                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
XIT      XIT1                                                                   
*                                                                               
         USING ACTRECD,RF                                                       
ACCF     CLI   MODE,PROCACC                                                     
         BNE   TRNS                                                             
         XC    STATUS,STATUS       CLEAR STATUS INDICATOR                       
         L     RF,ADACC                                                         
         CLC   ACTKUNT(2),=C'SJ'                                                
         BNE   XIT                 LOOK AT SJ'S ONLY                            
*                                                                               
         USING JOBELD,R1                                                        
         LR    R1,RF                                                            
         AH    R1,DATADISP                                                      
         SR    R0,R0                                                            
*                                                                               
ACCF2    CLI   JOBEL,0             EOR?                                         
         BE    XIT                 YES                                          
         CLI   JOBEL,JOBELQ        JOB ELEMENT?                                 
         BE    *+14                YES                                          
         IC    R0,JOBLN                                                         
         AR    R1,R0                                                            
         B     ACCF2                                                            
*                                                                               
         TM    JOBSTA1,JOBSXJOB    TEST FOR X-JOB                               
         BZ    XIT                 NO                                           
         OI    STATUS,XJOB         UPDATE STATUS                                
*                                                                               
         MVC   ACCOUNT,0(RF)           SAVE ACCOUNT NUMBER                      
         MVC   ACCTNAME,SPACES                                                  
*                                                                               
         L     RF,ADACCNAM             AND NAME                                 
         ZIC   RE,1(RF)                                                         
         SH    RE,=H'3'                                                         
         EX    RE,*+8                                                           
         B     XIT                                                              
         MVC   ACCTNAME(0),2(RF)                                                
         DROP  RF                                                               
         EJECT                                                                  
TRNS     CLI   MODE,PROCTRNS                                                    
         BNE   XIT                                                              
         TM    STATUS,XJOB         IS THIS AN XJOB?                             
         BZ    XIT                 NO                                           
*                                                                               
         USING TRNELD,R6                                                        
         L     R6,ADTRANS                                                       
         CLI   TRNTYPE,50          TYPE 50'S ONLY                               
         BNE   XIT                                                              
*                                                                               
         BAS   RE,PRNTIT                                                        
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO FORMAT AND PRINT A LINE OF OUTPUT                     
PRNTIT   NTR1                                                                   
         TM    STATUS,PRINTED      PRINT ACCOUNT ONCE                           
         BO    PRNT4                                                            
         MVC   P+1(12),ACCOUNT+3   ACCOUNT CODE                                 
         MVC   P+15(36),ACCTNAME   AND NAME                                     
         OI    STATUS,PRINTED                                                   
         B     PRNT4                                                            
*                                                                               
PRNT4    LA    R3,2                                                             
         LA    R4,P+60                                                          
*                                                                               
PRNT5    EDIT  TRNAMNT,(14,(R4)),2,MINUS=YES                                    
         MVC   P+75(L'TRNREF),TRNREF                                            
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
XJOB     EQU   X'01'                                                            
PRINTED  EQU   X'02'                                                            
*                                                                               
ACXED    DSECT                                                                  
RELO     DS    F                                                                
STATUS   DS    CL1                 SEE EQUATES                                  
ACCOUNT  DS    CL15                CURRENT ACCOUNT                              
ACCTNAME DS    CL36                        AND NAME                             
         EJECT                                                                  
*  ACREPWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*  ACGENFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*  ACGENMODES                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011ACREPXE02 05/01/02'                                      
         END                                                                    
