*          DATA SET CTPEREX    AT LEVEL 095 AS OF 09/26/08                      
*PHASE CTPEREXA                                                                 
***********************************************************************         
*ADD X'040A6401010000000000' ELEM TO PERSON RECORDS                   *         
***********************************************************************         
         TITLE 'CT PERSON RECORD EXTERM'                                        
CTPEREX  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,CTPEREX                                                        
*                                                                               
         LR    RC,R1               GET W/S POINTER                              
         USING CONWORKD,RC                                                      
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
         MVI   DATADISP+1,28                                                    
*                                                                               
         L     R2,AIOAREA          POINT TO RECORD                              
         LA    R2,4(R2)            POINT TO FIRST BYTE OF RECORD                
*                                                                               
         CLI   0(R2),X'FF'         TEST FILE TRAILER                            
         BE    EXIT                                                             
*                                                                               
         LA    RF,RECTAB           RECTAB HAS THE REC TYPES WE WANT             
CREC     CLC   0(2,RF),0(R2)                                                    
         BE    MREC                                                             
         LA    RF,L'RECTAB(RF)                                                  
         CLI   0(RF),X'FF'         EOT?                                         
         BNE   CREC                 NO                                          
         B     EXIT                 YES - LEAVE IT ALONE                        
*                                                                               
MREC     TM    27(R2),X'80'        IS THIS REC DELETED?                         
         BNZ   EXIT                                                             
         BAS   RE,CHANGE           MAKE CHANGE IF NECESSARY                     
         B     EXIT                                                             
*                                                                               
*                                                                               
CHANGE   NTR1                                                                   
*                                                                               
*PRINT THE OLD RECORD KEY BOTH IN HEX AND CHAR                                  
         MVC   P(10),=CL10'OLDX AIO='                                           
         GOTO1 VHEXOUT,DMCB,0(R2),P+10,28,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         MVC   P(10),=CL10'OLDC AIO='                                           
         MVC   P+10(28),0(R2)                                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         CLC   =H'980',25(R2)      REC >= 980 BYTES?                            
         BNH   PRNERR                                                           
*                                                                               
* ADD X'040A6401010000000000'     (CONNECT ACTIVITY ELEMENT)                    
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM(10),=X'040A6401010000000000'                                
         GOTO1 VHELLO,DMCB,(C'P',=C'CTFILE'),(X'04',(R2)),ELEM,        +        
               =C'ADD=CODE'                                                     
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*  UPDATE REC HDR BY USING ACTUAL LENGTH IN KEY...                              
         ZICM  R1,25(R2),2                                                      
         LA    R1,4(R1)                                                         
         L     R3,AIOAREA                                                       
         STCM  R1,3,0(R3)                                                       
*                                                                               
* PRINT KEYS OF ALTERED RECS IN HEX                                             
         MVC   P(10),=CL10'NEWX AIO='                                           
         GOTO1 VHEXOUT,DMCB,0(R2),P+10,28,=C'TOG'                               
         GOTO1 VPRINTER                                                         
* PRINT KEYS OF ALTERED RECS IN HEX                                             
         L     R3,AIOAREA                                                       
         MVC   P(10),=CL10'NEWC AIO='                                           
         MVC   P+10(32),0(R3)                                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* PRINT ERROR RECS *                                                            
         SPACE                                                                  
PRNERR   DS    0H                                                               
         MVC   P(14),=C'LENGTH ERROR: '                                         
         GOTO1 VHEXOUT,DMCB,0(R2),P+14,25,=C'TOG'                               
         GOTO1 VPRINTER                                                         
         MVC   P(25),0(R2)                                                      
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
DATADISP DS    H                                                                
ELCODE   DS    C                                                                
ELEM     DS    CL256                                                            
DATA     DS    CL10                                                             
*                                                                               
*                                                                               
* BYTES 00 - 01  REC KEY                                                        
*                                                                               
RECTAB   DS    0CL2                                                             
         DC    X'C604'                 PERSON RECORD                            
         DC    X'FF'                                                            
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'095CTPEREX   09/26/08'                                      
         END                                                                    
