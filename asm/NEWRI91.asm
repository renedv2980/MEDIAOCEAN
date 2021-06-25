*          DATA SET NEWRI91    AT LEVEL 021 AS OF 10/27/99                      
*PHASE T32091A,+0                                                               
*INCLUDE NUMVAL                                                                 
*                                                                               
*        TITLE 'T32091 - NETWORK ANALYSIS CHANGE REPORT'                        
************************************************************                    
*                                                                               
*                                                                               
*  ** EDIT MODULE **                                                            
*                                                                               
*                                                                               
*************************************************************                   
         SPACE 2                                                                
T32091   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*NET91**,RA,R8                                                 
         USING T32091,RB,RA,R8                                                  
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
         MVI   FTERMFLG,1         OPTIONAL                                      
*                                                                               
         LA    R2,SPLCLIH                CLIENT                                 
         NETGO NVCLIALL,DMCB                                                    
         L     R1,ANETWS1          CLIENT REC SITS HERE                         
         USING CLTHDR,R1                                                        
         CLC   =C'ALL',NBSELCLI                                                 
         BE    *+10                                                             
         MVC   ONECLT,CKEYCLT      SAVE CLIENT FOR FILTER                       
         MVC   AMSAVE,NBACTAM                                                   
         DROP  R1                                                               
*                                                                               
*                                                                               
         LA    R2,SPLPROH                PRODUCT                                
         CLI   5(R2),0                                                          
         BE    EDT10                                                            
         NETGO NVPRDALL,DMCB                                                    
*                                                                               
EDT10    LA    R2,SPLESTH                ESTIMATE                               
         NETGO NVESTRNG,DMCB                                                    
                                                                                
*                                                                               
         LA    R2,SPLNETH                NETWORK                                
         CLI   5(R2),0                                                          
         BE    EDT13                                                            
         NETGO NVNETALL,DMCB                                                    
*                                                                               
EDT13    LA    R2,SPLPRGH                PROGRAM                                
         MVC   REQPROG,8(R2)                                                    
         OC    REQPROG,=X'404040404040'                                         
*                                                                               
         LA    R2,SPLUSTH                UNIT START DATE                        
         NETGO NVSTRDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLUNDH                UNIT END DATE                          
         NETGO NVENDDAT,DMCB                                                    
*                                                                               
         LA    R2,SPLASTH                ACTIVITY START DATE                    
         XC    REQACTST(4),REQACTST       CLEAR START/END DATES                 
         CLI   5(R2),0                                                          
         BE    EDT19                                                            
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    EDERR                                                            
         GOTO1 DATCON,DMCB,WORK,(2,REQACTST)                                    
*                                                                               
EDT19    LA    R2,SPLANDH                ACTIVITY END DATE                      
         CLI   5(R2),0                                                          
         BE    EDT20                                                            
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    EDERR                                                            
         GOTO1 DATCON,DMCB,WORK,(2,REQACTND)                                    
*                                                                               
EDT20    LA    R2,SPLRSNH                REASON CODE                            
         MVC   REQRSN,8(R2)                                                     
*                                                                               
         LA    R2,SPLRTNH                ROTATION                               
         MVI   REQRTN,0                                                         
         ZIC   R0,5(R2)                                                         
         LTR   R0,R0                                                            
         BZ    EDT22                                                            
         GOTO1 DAYVAL,DMCB,((R0),8(R2)),REQRTN,BYTE                             
         CLI   REQRTN,0                                                         
         BE    EDERR                                                            
*                                                                               
EDT22    LA    R2,SPLCSTH                COST $                                 
         XC    REQCOST,REQCOST                                                  
         CLI   5(R2),0                                                          
         BE    EDT25                                                            
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,SPLCST,(R3)                                         
         CLI   DMCB,0                                                           
         BNE   EDERR                                                            
         MVC   REQCOST,DMCB+4            BINARY (INCLUDING PENNIES)             
         OC    REQCOST,REQCOST                                                  
         BNZ   EDT25                                                            
         MVI   REQCOST,C'0'        ZERO COST                                    
*                                                                               
EDT25    LA    R2,SPLTIMH                UNIT START-END TIME                    
         XC    REQTIME,REQTIME                                                  
         CLI   5(R2),0                                                          
         BE    EDT30                                                            
         ZIC   R3,5(R2)                                                         
         GOTO1 TIMVAL,DMCB,((R3),SPLTIM),REQTIME                                
*                                                                               
EDT30    LA    R2,SPLLENH                LENGTH                                 
         CLI   5(R2),0                                                          
         BE    EDT35                                                            
         ZIC   R3,5(R2)                                                         
         GOTO1 =V(NUMVAL),DMCB,SPLLEN,(R3),RR=YES                               
         CLI   DMCB,0                                                           
         BNE   EDERR                                                            
         L     R1,DMCB+4                                                        
         STC   R1,REQLEN                                                        
*                                                                               
EDT35    DS    0H                                                               
         LA    R2,SPLCOMH          COMMENTS                                     
         CLI   5(R2),0                                                          
         BE    EDT36                                                            
         CLI   8(R2),C'Y'          COMMENTS ONLY                                
         BNE   EDERR                                                            
         MVI   REQCMNT,C'Y'                                                     
*                                                                               
EDT36    DS    0H                                                               
         LA    R2,SPLPBKH          PRODUCT BREAK?                               
         CLI   5(R2),0                                                          
         BE    EDT37                                                            
         MVI   PRDBRK,C'Y'                                                      
         CLI   8(R2),C'Y'          BREAK ON PRODUCT                             
         BE    EDT37                                                            
         MVI   PRDBRK,1                                                         
         CLI   8(R2),C'A'          ALWAYS PROD BREAK                            
         BE    EDT37                                                            
         MVI   PRDBRK,C'G'         PRDGROUP BREAKS                              
         CLI   8(R2),C'G'                                                       
         BNE   EDERR                                                            
*                                                                               
EDT37    DS    0H                                                               
         LA    R2,SPLGRPH          AUDIT GROUP                                  
         CLI   5(R2),0                                                          
         BE    EDT40                                                            
         MVC   REQGRP,8(R2)                                                     
*                                                                               
EDT40    DS    0H                                                               
         LA    R2,SPLOPTH          OPTION?                                      
         CLI   5(R2),0                                                          
         BE    EDT45                                                            
         CLC   =C'CODE',8(R2)            SHOW PRD CODE/EST /PKG ?               
         BNE   *+8                                                              
         MVI   REQCODE,C'Y'              YES                                    
*                                                                               
EDT45    DS    0H                                                               
         LA    R2,SPLTITH          TITLE ?                                      
         CLI   5(R2),0                                                          
         BE    EDTXIT                                                           
         MVC   HDTITLE,8(R2)                                                    
*                                                                               
EDTXIT   B     EXIT                                                             
                                                                                
EDERR    DS    0H                                                               
         MVI   ERROR,INVALID                                                    
         GOTO1 ERREX,DMCB                                                       
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
DISTCOD  DS    CL4   ***                                                        
HDTITLE  DS    CL60  ***                                                        
REQPROG  DS    CL6   ***     PROGRAM                                            
REQRSN   DS    CL3   ***     REASON CODE                                        
REQCOST  DS    CL4   ***     ACTUAL COST                                        
REQTIME  DS    CL4   ***     UNIT START-END TIME                                
REQLEN   DS    CL1   ***     LENGTH                                             
REQCMNT  DS    CL1   ***     COMMENT                                            
REQACTST DS    CL2   ***     ACTIVITY START DATE                                
REQACTND DS    CL2   ***     ACTIVITY END DATE                                  
REQRTN   DS    CL1   ***     ROTATION                                           
PRDBRK   DS    CL1   ***     PRODBREAK                                          
REQGRP   DS    CL4   ***     AUDIT GROUP FILTER                                 
REQCODE  DS    CL1   ***     SHOW PROG CODE/EST/PKG                             
*                                                                               
                                                                                
         EJECT                                                                  
       ++INCLUDE NETINCLS                                                       
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
       ++INCLUDE NEWRIFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEWRICBD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021NEWRI91   10/27/99'                                      
         END                                                                    
