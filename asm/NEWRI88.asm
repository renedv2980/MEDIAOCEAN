*          DATA SET NEWRI88    AT LEVEL 009 AS OF 04/30/98                      
*PHASE T32088A,+0                                                               
*                                                                               
         TITLE 'T32088 - NETWORK UNIT HISTORY UPDATE'                           
************************************************************                    
*                                                                               
*                                                                               
*  ** EDIT MODULE **                                                            
*                                                                               
* THIS IS EDIT MODULE FOR BOTH HISTORY AND AUDIT REPORTS                        
* CAN ONLY BE REQUESTED FRO DDS TERMINALS                                       
*                                                                               
*************************************************************                   
         SPACE 2                                                                
T32088   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*NET88**,RA,R8                                                 
         USING T32088,RB,RA,R8                                                  
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
                                                                                
         L     R6,ATWA                                                          
         USING T320FFD,R6                                                       
                                                                                
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R1,ANETWS1                                                       
         ST    R1,NBACLI                                                        
*                                                                               
         L     R7,ANETWS3                                                       
         USING MYWORKD,R7                                                       
                                                                                
         L     R5,ASPOOLD                                                       
         USING SPOOLD,R5                                                        
                                                                                
         DROP  R5                                                               
*                                                                               
*                                                                               
         CLI   MODE,VALREC                                                      
         BE    EDLINE                                                           
EXIT     XIT1                                                                   
         EJECT                                                                  
EDLINE   DS    0H                                                               
         MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
*                                                                               
         LA    R2,SPLCLIH                CLIENT                                 
         CLI   OFFLINE,C'Y'        ONLY DDS TERMINALS                           
         BE    ED00                                                             
         CLI   1(R6),C'*'                                                       
         BNE   EDERR                                                            
ED00     NETGO NVCLIALL,DMCB                                                    
         L     R1,ANETWS1          CLIENT REC SITS HERE                         
         USING CLTHDR,R1                                                        
         CLC   =C'ALL',SPLCLI                                                   
         BE    *+10                                                             
         MVC   ONECLT,CKEYCLT      SAVE CLIENT FOR FILTER                       
         MVC   AMSAVE,NBACTAM                                                   
         DROP  R1                                                               
*                                                                               
*                                                                               
         LA    R2,SPLPROH                PRODUCT                                
         CLC   =C'ALL',SPLPRO                                                   
         BE    EDT10                                                            
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
EDT10    NETGO NVPRDALL,DMCB                                                    
*                                                                               
         LA    R2,SPLESTH                ESTIMATE                               
         XC    DMCB(16),DMCB                                                    
         NETGO NVESTRNG,DMCB                                                    
         B     EXIT                                                             
*                                                                               
EDERR    GOTO1 ERREX,DMCB                                                       
*                                                                               
         DROP  R6                                                               
                                                                                
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
                                                                                
         EJECT                                                                  
*                                                                               
                                                                                
MYWORKD  DSECT                                                                  
AMSAVE   DS    CL1   *** PASSED TO PRINT MODULE - DO NOT MOVE                   
ONECLT   DS    CL2   ***                                                        
*                                                                               
                                                                                
         EJECT                                                                  
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRICFD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009NEWRI88   04/30/98'                                      
         END                                                                    
