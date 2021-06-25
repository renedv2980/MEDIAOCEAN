*          DATA SET AAFILEFIX  AT LEVEL 027 AS OF 03/22/01                      
*PHASE FILEFIXA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE KHDUMMY                                                                
*INCLUDE HELLO                                                                  
*INCLUDE HELEN                                                                  
         TITLE 'FIX FOR GOAL RECORDS'                                           
FILEFIX  CSECT    
*Encore un fois
         NBASE 0,FILEFIX*,=V(REGSAVE)                                           
*                                                                               
         OPEN  (FILEIN,INPUT)                                                   
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (FILEOUT,OUTPUT)                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LOOP     GET   FILEIN              REMEMBER FILEIN HAS BEEN SORTED              
         LR    R2,R1                                                            
         CLI   4(R2),X'00'         ESTIMATE RECORD?                             
         BE    LOOP04              YES                                          
         CLI   4(R2),X'02'         GOAL RECORD?                                 
         BE    LOOP04              YES                                          
         BNH   LOOP02                                                           
*                                                                               
         L     R3,AIO1                                                          
         OC    0(4,R3),0(R3)       PUT LAST 02 RECORD ALREADY?                  
         BZ    LOOP02                                                           
*                                                                               
         PUT   FILEOUT,(R3)        WRITE LAST 02 RECORD                         
         XC    0(4,R3),0(R3)       BUT ONLY ONCE                                
*                                                                               
LOOP02   PUT   FILEOUT,(R2)                                                     
         B     LOOP                                                             
*                                                                               
LOOP04   L     R3,AIO1                                                          
         OC    0(4,R3),0(R3)       FOR FIRST TIME THROUGH                       
         BZ    LOOP16                                                           
         CLI   4(R2),0                                                          
         BE    LOOP10                                                           
         CLC   4(13,R2),4(R3)      SEE IF RECORDS HAVE SAME KEY                 
         BNE   LOOP12              NO - JUST WRITE THE OLDER ONE                
*                                                                               
         LA    R4,4(R2)                                                         
         LA    R5,4(R3)                                                         
         USING GOALREC,R4                                                       
         LA    R6,GDELEM                                                        
*                                                                               
LOOP06   LR    RF,R6               COPY IN ALL ELEMENTS >= X'21'                
         SR    RF,R2                                                            
         CH    RF,0(R2)                                                         
         BNL   LOOP                                                             
         CLI   0(R6),X'21'                                                      
         BL    LOOP08                                                           
         MVI   3(R3),255           FLAG WE DID HELLO CALL                       
*                                                                               
         GOTO1 VHELLO,DMCB,(C'P',SPTFIL),(R5),(R6),0,0                          
         XR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         LH    R0,0(R3)                                                         
         AR    R0,RF                                                            
         STH   R0,0(R3)                                                         
*                                                                               
LOOP08   XR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         BXH   R6,RF,LOOP06                                                     
         DC    H'0'                                                             
         DROP  R4                                                               
*                                                                               
LOOP10   LA    R4,4(R2)                                                         
         LA    R5,4(R3)                                                         
R        USING ESTHDR,R4                                                        
S        USING ESTHDR,R5                                                        
         CLC   4(8,R2),4(R3)       SEE IF RECORDS HAVE SAME KEY                 
         BNE   LOOP12              NO - JUST WRITE THE OLDER ONE                
*                                                                               
         CLC   R.ESTART,S.ESTART                                                
         BH    *+10                                                             
         MVC   S.ESTART,R.ESTART                                                
         CLC   R.EEND,S.EEND                                                    
         BL    *+10                                                             
         MVC   S.EEND,R.EEND                                                    
         B     LOOP                                                             
         DROP  R,S                                                              
*                                                                               
LOOP12   CLI   3(R3),255           GOAL RECORD WITH HELLO?                      
         BNE   LOOP14              NO                                           
         MVI   3(R3),0                                                          
         LH    R1,0(R3)                                                         
         BCTR  R1,0                                                             
         STH   R1,0(R3)            ADJUST LENGTH OF RECORD                      
*                                                                               
         LA    R4,4(R3)                                                         
         USING GOALREC,R4                                                       
         ICM   R1,3,GLENGTH                                                     
         BCTR  R1,0                                                             
         STCM  R1,3,GLENGTH        ADJUST LENGTH IN RECORD                      
         DROP  R4                                                               
*                                                                               
LOOP14   PUT   FILEOUT,(R3)        DIFFERENT - PUT THIS RECORD TO FILE          
*                                                                               
LOOP16   L     R0,AIO1                                                          
         LR    RE,R2                                                            
         LH    R1,0(R2)                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE               MOVE RECORD INTO IO AREA                     
         L     RF,AIO1                                                          
         LH    R0,0(RF)                                                         
         AR    RF,R0                                                            
         MVI   0(RF),0                                                          
         B     LOOP                                                             
*                                                                               
END      CLOSE FILEIN                                                           
         CLOSE FILEOUT                                                          
         XBASE ,                                                                
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
VHELLO   DC    V(HELLO)                                                         
*                                                                               
SPTFIL   DC    CL8'SPTFIL  '                                                    
*                                                                               
FILEIN   DCB   DSORG=PS,RECFM=VB,MACRF=GL,DDNAME=FILEIN,EODAD=END,     *        
               BLKSIZE=25000,LRECL=4096,BUFNO=2                                 
*                                                                               
FILEOUT  DCB   DSORG=PS,RECFM=VB,MACRF=PM,DDNAME=FILEOUT,              *        
               BLKSIZE=25000,LRECL=4096                                         
*                                                                               
DMCB     DS    6F                                                               
DUB      DS    D                                                                
*                                                                               
AIO1     DC    A(IOA1H)                                                         
*                                                                               
         DS    0D                                                               
         DC    CL16'*IOAREA 1*******'                                           
IOA1H    DS    F                                                                
         DS    4096C                                                            
IOL      EQU   *-IOA1H                                                          
*                                                                               
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPGENEST                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027AAFILEFIX 03/22/01'                                      
         END                                                                    
