*          DATA SET REINF99    AT LEVEL 075 AS OF 05/01/02                      
*PHASE T80B99A,*                                                                
         TITLE 'T80B99 - REINF99 - SPECIAL FIX OVERLAY IN INFO'                 
*                                                                               
*********************************************************************           
*                                                                   *           
*        REINF99 --- REP FILE FIXER.  RECORD @@**                   *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 11/08/89  PJS  CREATED.   LOAD AS 'C' LEVEL.                      *           
*                                                                   *           
*********************************************************************           
*                                                                               
T80B99   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**8B99**,RR=R5                                                 
*                                                                               
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         USING T80BFFD,RA                                                       
*                                                                               
         ST    R5,RELO1            SAVE RELO FACTOR                             
         SPACE 2                                                                
         EJECT                                                                  
*                                                                               
*- REMOVE TRAILING 0 AT END OF CONTRACT RECORDS                                 
*                                                                               
*                                                                               
*- BUILD STARTING KEY                                                           
         XC    KEY,KEY                                                          
         MVI   KEY,X'0C'           KEY ID                                       
         MVC   KEY+2(2),REPALPHA  '                                             
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         SR    R8,R8                                                            
         SR    R9,R9                                                            
*                                                                               
*- READ KEY                                                                     
         BAS   RE,HIGH                                                          
         B     BREAK                                                            
*                                                                               
*- READ NEXT KEY                                                                
NEXTSEQ  BAS   RE,SEQ                                                           
*                                                                               
*- CHECK FOR END OF DATA                                                        
BREAK    CLC   KEY(4),SAVEKEY                                                   
         BNE   DONE                                                             
*                                                                               
         LA    R8,1(R8)            COUNT # READ                                 
*                                                                               
         MVC   SAVEKEY,KEY         SAVE NEW KEY                                 
*                                                                               
         MVC   RCONREC+IO+000(250),ZERO                                         
         MVC   RCONREC+IO+250(250),ZERO                                         
         MVC   RCONREC+IO+500(250),ZERO                                         
         MVC   RCONREC+IO+750(250),ZERO                                         
         BAS   RE,GETREC           READ IN RECORD                               
*                                                                               
         LA    R2,RCONREC+34+IO     A(1ST ELEMENT)                              
         SR    R5,R5                                                            
         ICM   R5,3,RCONLEN+IO     RECORD LENGTH                                
         LR    R6,R5               SAVE RECORD LENGTH.                          
         AR    R5,R2               R5=A(1 BYTE PAST END)                        
*                                                                               
         LA    R7,34               MIN LENGTH                                   
*                                                                               
LOOP100  EQU   *                                                                
         ZIC   RF,1(R2)                                                         
         LTR   RF,RF                                                            
         BZ    LOOP200             0 ELEMENT LENGTH                             
*                                                                               
         AR    R7,RF               ADD TO RUNNING LEN                           
*                                                                               
         AR    R2,RF               NEXT ELEMENT                                 
         CR    R2,R5                                                            
         BL    LOOP100                                                          
         SPACE                                                                  
LOOP200  EQU   *                                                                
         CR    R6,R7               FILE-VS-ACCUME RECORD LENGHT                 
         BE    NEXTSEQ                                                          
*                                                                               
         STCM  R7,3,RCONLEN+IO     CORRECT RECORD LEN                           
*                                                                               
         CLC   =C'WPJS',RCONKSTA+IO                                             
         BE    FIXIT                                                            
         CLC   =C'WPPP',RCONKSTA+IO                                             
         BE    FIXIT                                                            
         CLC   =C'WTVJ',RCONKSTA+IO                                             
         BE    FIXIT                                                            
         DC    H'0'                WHO ELSE IS BROKEN                           
*                                                                               
FIXIT    GOTO1 PUTREC                                                           
*                                                                               
         LA    R9,1(R9)            COUNT # FIXED                                
         B     NEXTSEQ                                                          
*                                                                               
DONE     EQU   *                                                                
         EDIT  (R8),(4,INFSTRT)                                                 
         FOUT  INFSTRTH                                                         
         EDIT  (R9),(4,INFFLTR)                                                 
         FOUT  INFFLTRH                                                         
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
ZERO     DC    XL250'00'                                                        
*                                                                               
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
       ++INCLUDE REINFWRK                                                       
*                                                                               
IO       EQU   IOAREA-RCONREC                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'075REINF99   05/01/02'                                      
         END                                                                    
