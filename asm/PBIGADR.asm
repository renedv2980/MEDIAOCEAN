*          DATA SET PBIGADR    AT LEVEL 073 AS OF 08/09/00                      
*PHASE PBIGADRA                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE REGSAVE                                                                
*                                                                               
         TITLE 'PBIGADR- EXPAND ADDRESS RECORD'                                 
*                                                                               
* COPIES ALL RECORDS                                                            
*                                                                               
PBIGADR CSECT                                                                   
         NBASE 0,PBIGADR,=V(REGSAVE)                                            
         SPACE 2                                                                
         LA    R6,PBIGADR+4095                                                  
         LA    R6,1(R6)                                                         
         USING PBIGADR+4096,R6     NOTE USE OF R6 AS 2ND BAS REG                
*                                                                               
         BAS   RE,PRNT                                                          
         GOTO1 =V(DATCON),DMCB,(5,0),(8,TODAY)                                  
*                                                                               
         OPEN  (IN,(INPUT),OUT,(OUTPUT))                                        
*                                                                               
*                                                                               
*                                                                               
START1   DS    0H                                                               
         BAS   RE,CARDS                                                         
         CLC   =C'/*',CARD                                                      
         BE    START10                                                          
         CLC   =C'DUMP=',CARD                                                   
         BNE   START2                                                           
         PACK  DMPCNT,CARD+5(4)                                                 
         B     START1                                                           
START2   DS    0H                                                               
         CLC   =C'PRINT',CARD                                                   
         BNE   START3                                                           
         MVI   PRTSW,C'Y'                                                       
         B     START1                                                           
START3   DS    0H                                                               
         B     START1                                                           
*                                                                               
START10  DS    0H                                                               
*                                                                               
*                                                                               
*                                                                               
GET      DS    0H                                                               
*                                                                               
         BAS   RE,GETREC                                                        
*                                                                               
* ADD CODE FOR THIS RUN HERE                                                    
*                                                                               
* EXPAND PUBAOVEL FROM 225 TO 254 BYTES                                         
*                                                                               
         CLC   REC(3),=3X'FF'      EOF RECORD                                   
         BE    EOF                                                              
         CLI   REC+9,X'82'         PUB ADDRESS RECORDS                          
         BE    PUBADDR                                                          
         BNE   PUTXX                                                            
*                                                                               
*                                                                               
*                                                                               
PUBADDR  DS    0H                                                               
         XC    ELEM,ELEM                                                        
         LA    R2,REC+33                                                        
         LA    R8,ELEM                                                          
*                                                                               
         USING PUBAOVEL,R8                                                      
*                                                                               
         CLI   1(R2),254           ALREADY IN NEW LENGTH FORMAT                 
         BE    PUTXX                                                            
*                                                                               
         MVC   ELEM(1),0(R2)       ELEMENT CODE                                 
         MVI   ELEM+1,254          NEW ELEMENT LENGTH                           
         MVC   PUBAOFF,2(R2)                                                    
         MVC   PUBAONAM,5(R2)                                                   
         MVC   PUBAOLN1,35(R2)                                                  
         MVC   PUBAOLN2,65(R2)                                                  
         MVC   PUBAOATN,95(R2)                                                  
         MVC   PUBAOTEL,115(R2)                                                 
*                                                                               
         CLI   1(R2),165                                                        
         BNE   PUBA30                                                           
         MVC   PUBAOLN3,127(R2)                                                 
         MVC   PUBAOFAX,153(R2)                                                 
*                                                                               
PUBA30   DS    0H                                                               
         CLI   1(R2),225                                                        
         BNE   PUBA60                                                           
         MVC   PUBAOLN3,127(R2)                                                 
         MVC   PUBAOFAX,153(R2)                                                 
         MVC   PUBAOEAD,165(R2)                                                 
*                                                                               
         DROP  R8                                                               
*                                                                               
PUBA60   DS    0H                                                               
*                                                                               
         AP    RECCNT,=P'1'        PROCESSING RECORD COUNTER                    
*                                                                               
         MVI   DUB,C'B'                                                         
         BAS   RE,DMPREC           PRINT RECORD BEFORE PROCESSING               
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 =V(RECUP),DMCB,(1,REC),(R2)             DELETE ELEMENT           
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,3,REC+25         RECORD LENGTH                                
         LA    RE,REC                                                           
         AR    RE,R4               POINT RE TO EOR                              
         LA    RF,4000                                                          
         SR    RF,R4               NUMBER OF BYTES TO BE CLEARED                
         XCEF                                                                   
*                                                                               
         GOTO1 =V(RECUP),DMCB,(1,REC),ELEM,(R2)        ADD NEW ELEMENT          
*                                                                               
         MVI   DUB,C'A'                                                         
         BAS   RE,DMPREC           PRINT RECORD AFTER PROCESSING                
*                                                                               
PUT      DS    0H                                                               
*                                                                               
PUTXX    BAS   RE,PUTREC                                                        
         B     GET                                                              
*                                                                               
*                                                                               
*                                                                               
EOF      CLOSE (IN,)                                                            
         CLOSE (OUT,)                                                           
*                                                                               
         BAS   RE,PRNT                                                          
*                                                                               
         SP    GETCNT,=P'1'        LAST GETREC IS EOF, DOESN'T COUNT            
*                                                                               
         LA    R3,COUNTS                                                        
         LA    R4,25               LENGTH OF EACH COUNT                         
         LA    R5,COUNTSX                                                       
*                                                                               
EOF2     MVC   P+1(20),5(R3)                                                    
         OI    4(R3),X'0F'                                                      
         UNPK  P+22(7),0(5,R3)                                                  
         BAS   RE,PRNT                                                          
         BXLE  R3,R4,EOF2                                                       
EOJ      DS    0H                                                               
*                                                                               
         XBASE                                                                  
*                                                                               
*                                                                               
*                                                                               
SKIP     MVC   PCOM,=C'BC01'                                                    
         ZAP   LNCNT,=P'0'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT3    MVC   PCOM,=C'BL03'                                                    
         AP    LNCNT,=P'3'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT2    MVC   PCOM,=C'BL02'                                                    
         AP    LNCNT,=P'2'                                                      
         B     PRNTR                                                            
*                                                                               
PRNT     MVC   PCOM,=C'BL01'                                                    
         AP    LNCNT,=P'1'                                                      
*                                                                               
PRNTR    NTR1                                                                   
*                                                                               
         GOTO1 =V(PRINT),DMCB,P,PCOM                                            
         MVI   P,C' '                                                           
         MVC   P+1(132),P                                                       
         B     XIT                                                              
*                                                                               
***********************************************************************         
*                                                                               
CARDS    NTR1                                                                   
*                                                                               
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                                     
*                                                                               
         LA    R2,4(R2)                                                         
*                                                                               
         B     XIT                                                              
*                                                                               
***********************************************************************         
*                                                                               
DMPREC   NTR1                                                                   
*                                                                               
         SP    DMPCNT,=P'1'                                                     
         BNP   DMPRX                                                            
         MVI   P,C' '                                                           
         MVC   P+1(132),P                                                       
         MVC   P+1(5),=C'AFTER'                                                 
         CLI   DUB,C'B'                                                         
         BNE   *+10                                                             
         MVC   P+1(6),=C'BEFORE'                                                
*                                                                               
         EDIT  (P5,RECCNT),(8,P+10),0,ALIGN=LEFT,COMMAS=YES,ZERO=BLANK          
         MVC   PCOM,=C'BL01'                                                    
         GOTO1 =V(PRINT),DMCB,P,PCOM                                            
         MVI   P,C' '                                                           
         MVC   P+1(132),P                                                       
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,C'REC',REC,C'DUMP',310,=C'1D'                    
*                                                                               
DMPRX    B     XIT                                                              
*                                                                               
***********************************************************************         
*                                                                               
GETREC   NTR1                                                                   
*                                                                               
         LA    RE,REC                                                           
         LA    RF,4000                                                          
         XCEF                                                                   
*                                                                               
         GET   IN,REC-4                                                         
*                                                                               
         MVC   HALF,REC+25                                                      
         SR    R2,R2                                                            
         LH    R2,HALF                                                          
         LA    R3,REC(R2)                                                       
         MVI   0(R3),0             EOR                                          
         AP    GETCNT,=P'1'                                                     
*                                                                               
         B     XIT                                                              
*                                                                               
***********************************************************************         
*                                                                               
PUTREC   NTR1                                                                   
*                                                                               
         MVC   HALF,REC+25                                                      
         SR    R1,R1                                                            
         LH    R1,HALF                                                          
         LA    R1,4(R1)                                                         
         STH   R1,REC-4                                                         
         PUT   OUT,REC-4                                                        
         AP    PUTCNT,=P'1'                                                     
         B     XIT                                                              
*                                                                               
*                                                                               
*                                                                               
XIT      XIT1                                                                   
*                                                                               
*                                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
IN       DCB   DDNAME=IN,              DOS SYS010                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=32760               X        
               MACRF=GM,                                               X        
               EODAD=EOF                                                        
*                                                                               
*                                                                               
*                                                                               
OUT      DCB   DDNAME=OUT,             DOS SYS011                      X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=04004,                                            X        
               BLKSIZE=32760,          DOS BLKSIZE=32760               X        
               MACRF=PM                                                         
*                                                                               
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
DMCB     DC    6F'0'                                                            
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DC    X'00'                                                            
ELCODE   DS    X                                                                
UPSI     DS    XL1                                                              
         DS    0F                                                               
WORK     DS    CL256                                                            
KLEN     EQU   25                                                               
PCOM     DS    CL4                                                              
LNCNT    DC    PL2'99'                                                          
*                                                                               
DMPCNT   DC    PL5'201'           TEST 1ST 100 RECS (DECREASED BY 2)            
*                                                                               
LASTIN   DC    XL50'00'                                                         
LASTOUT  DC    XL50'00'                                                         
X        DS    CL100                                                            
BSPARS   DS    6F                                                               
CARD     DS    CL80                                                             
LKPARS   DS    6F                                                               
ESTPARS  DS    6F                                                               
TODAY    DS    CL8                                                              
PRTSW    DS    CL1                                                              
SAVAMCL  DS    CL6                                                              
         DS    0D                                                               
MYDUB    DS    PL8                                                              
*                                                                               
ELEM     DS    CL255                                                            
*                                                                               
       ++INCLUDE PVALUES                                                        
*                                                                               
COUNTS   DS    0C                                                               
*                                                                               
GETCNT   DC    PL5'0',CL20'RECORDS GET:'                                        
PUTCNT   DC    PL5'0',CL20'RECORDS PUT:'                                        
RECCNT   DC    PL5'0',CL20'RECORDS PROCESSED:'                                  
*                                                                               
* OTHER COUNTERS ADDED HERE WILL BE AUTOMATICALLY PRINTED AT EOJ                
*                                                                               
COUNTSX  EQU   *-1                                                              
*                                                                               
P        DC    CL133' '                                                         
*                                                                               
         DS    F                                                                
REC      DS    4000C                                                            
         DS    D                                                                
*                                                                               
*                                                                               
*                                                                               
         ORG   REC                                                              
       ++INCLUDE PPUBARECK                                                      
*                                                                               
*                                                                               
*                                                                               
         CSECT                                                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073PBIGADR   08/09/00'                                      
         END                                                                    
