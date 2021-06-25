*          DATA SET CTFIX01    AT LEVEL 001 AS OF 07/13/89                      
*PHASE CTFIX01,*                                                                
         TITLE 'UPDATE SECURITY CODES FOR NEW PROD'                             
CTFIX01  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,CTFIX01                                                        
         USING CONWORKD,R6         R6=A(GLOBAL W/S)                             
*                                                                               
         L     R2,AIOAREA          R2=A(INPUT RECORD)                           
         LA    R5,4(R2)            R5=A(KEY)                                    
         L     RA,VCPRINT                                                       
         USING DPRINT,RA           RA=A(PRINT CSECT)                            
         CLI   OVSWITCH,X'FF'                                                   
         BE    IDEXIT                                                           
         CLI   WRITE,X'FF'         DELETED RECORD ?                             
         BE    IDEXIT              YES, SKIP IT                                 
         LR    R3,R5               SAVE ADDRESS                                 
         SPACE 3                                                                
         USING CTIREC,R3                                                        
         CLI   PROCREC,USERID      ID RECORD ?                                  
         BE    GETDATA             YES                                          
         CLI   PROCREC,USERPROF                                                 
         BE    GETDATA                                                          
*                                                                               
         CLI   PROCREC,TERMINAL    TERMINAL RECORD ?                            
         BE    GETDATA                                                          
         CLI   PROCREC,TERMPASS                                                 
         BNE   IDEXIT              NO, SKIP IT                                  
*                                                                               
GETDATA  SR    R2,R2                                                            
         ICM   R2,3,CTILEN                                                      
         LA    R2,0(R3,R2)                                                      
         XC    0(1,R2),0(R2)       CLEAR AS END MARKER                          
*                                                                               
         LA    R5,CTIDATA                                                       
*                                                                               
FIND21   CLI   0(R5),X'21'                                                      
         BE    PROCESS                                                          
         CLI   0(R5),0                                                          
         BE    IDEXIT                                                           
*                                                                               
NEXT21   SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     FIND21                                                           
*                                                                               
         USING CTSYSD,R5                                                        
PROCESS  CLI   CTSYSNUM,6          IS THIS ACCOUNTING ?                         
         BNE   NEXT21              NO, TRY AGAIN                                
*                                                                               
         XC    ELEMENT,ELEMENT     CLEAR NEW ELEMENT AREA                       
         SR    R1,R1                                                            
         IC    R1,CTSYSLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),CTSYSEL  MOVE OLD TO NEW                              
*                                                                               
         GOTO1 VHELLO,DMCB,(C'D',=C'CTFILE '),(X'21',(R5)),(1,X'06')            
*                                                                               
         LA    R5,ELEMENT                                                       
         CLI   CTSYSLEN,X'50'      YES, IS THERE PROGRAM SECURITY ?             
         BNE   EXPAND21            NO, EXPAND THE ELEMENT                       
*                                                                               
GETFILE  LA    R1,CTSYSALL         GET TO 'FILE' PORTION                        
         LA    R1,FILE*2(R1)                                                    
         MVC   FILEAC,1(R1)        SAVE THE ACCESS CODE                         
*                                                                               
         LA    R1,CTSYSALL         GET TO 'PROD' PORTION                        
         LA    R1,PROD*2(R1)                                                    
         MVC   0(2,R1),PRODAC      UPDATE ACCESS                                
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',=C'CTFILE '),(X'21',(R5))                      
*                                                                               
         CLI   DMCB+12,0           NO ERRORS, EXIT                              
         BE    IDEXIT                                                           
         CLI   DMCB+12,5           ERROR TOO LONG ?                             
         BE    PRINTIT             YES, PRINT KEY                               
         DC    H'0'                NO, DUMP                                     
*                                                                               
EXPAND21 MVC   CTSYSPGM,CTSYSALL   PROPAGATE ALL                                
         MVC   CTSYSPGM+2(62),CTSYSPGM                                          
         MVI   CTSYSLEN,X'50'      SET NEW LENGTH                               
         B     GETFILE                                                          
*                                                                               
PRINTIT  GOTO1 VHEXOUT,DMCB,0(R3),P+2,25,=C'TOG'                                
         GOTO1 VPRINTER            YES, PRINT RECORD KEY OF ERROR               
*                                                                               
IDEXIT   XIT1                                                                   
         EJECT                                                                  
FILE     EQU   3                                                                
PROD     EQU   11                                                               
         SPACE 3                                                                
PRODAC   DS    0XL2                                                             
         DC    X'FF'                                                            
FILEAC   DS    X                                                                
         EJECT                                                                  
* GEGENMSG                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEGENMSG                                                       
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
* CTCONWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTCONWORKD                                                     
         PRINT ON                                                               
         ORG   WORKEND                                                          
ELEMENT  DS    XL80                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001CTFIX01   07/13/89'                                      
         END                                                                    
