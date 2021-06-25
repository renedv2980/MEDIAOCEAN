*          DATA SET NEWRI88X   AT LEVEL 005 AS OF 07/30/01                      
*          DATA SET NEWRI88    AT LEVEL 009 AS OF 04/30/98                      
*PHASE T32088C,+0                                                               
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
**************************************************                              
* - GET SE NUMBER                                                               
LOCK0    GOTO1 GETFACT,DMCB,(2,0)                                               
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         CLI   FAOVSYS,3           MUST BE NET                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   BYTE,FASYS          GET SE NUMBER                                
         DROP  R1                                                               
* - LOCK / UNLOCK                                                               
         LA    R3,KEY                                                           
         USING LKKEYD,R3                                                        
         XC    KEY(L'LOCKEY),KEY                                                
         MVC   LOCKSE,BYTE         SET SE NUMBER                                
         MVC   LOCKAGY,NBSELAGY    AGENCY                                       
         MVC   LOCKRTY,=C'UN'      UNIT RECORDS                                 
         MVC   LOCKKEY(3),NBSELCLI    3 BYTE CLIENT CODE                        
         MVC   LOCKKEY+3(4),NBSELNET  4 BYTE NETWORK                            
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,X'7E'                                                     
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R2,DMCB                                                          
         XC    DMCB(24),DMCB                                                    
         L     R6,ACOMFACS                                                      
         LTR   R6,R6                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   LOCKSW,C'L'                                                      
         GOTO1 (R2),(R1),(LOCKSW,KEY),(R6)                                      
         CLI   DMCB+4,0            ANY ERRORS                                   
         BE    LOCKX                                                            
         XC    CONHEAD,CONHEAD    YES/ERRORS                                    
         MVC   CONHEAD(33),=C'*** CLIENT LOCKED - TRY LATER ***'                
         GOTO1 ERREX2                                                           
         DROP  R3                                                               
LOCKX    DS    0H                                                               
         DROP  R5                                                               
**************************************************                              
         B     EXIT                                                             
*                                                                               
LOCKSW   DS    CL1                                                              
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
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DDMASTC                                                        
       ++INCLUDE FASSB                                                          
       ++INCLUDE FALOCKETD                                                      
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRICFD                                                       
       ++INCLUDE DDGENTWA                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005NEWRI88X  07/30/01'                                      
         END                                                                    
