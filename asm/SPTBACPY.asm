*          DATA SET SPTBACPY   AT LEVEL 002 AS OF 11/11/11                      
*PHASE SPTBACPA     <WAS SPTBACPY                                               
*INCLUDE DATCON                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE STXITER                                                                
         TITLE 'SPTBACPY -- SPOTPAK TRAFFIC COPY BUYS TO RECV FORMAT'           
***********************************************************************         
*                                                                     *         
* LEV 02    OCT17/00                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE                                                                  
TBACPY   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NBASE 0,*TBACPY*,VREGSAVE                                              
*                                                                               
         LR    RC,RB                                                            
         AHI   RC,4096                                                          
         LR    RA,RC               <<< WAS L    RA,RC                           
         AHI   RA,4096                                                          
         USING TBACPY,RB,RC,RA                                                  
*                                                                               
         L     R8,=V(CPRINT)                                                    
         USING DPRINT,R8                                                        
*                                                                               
         GOTO1 =V(STXITER),DMCB,A(DUMPLIST)                                     
         XC    WORK,WORK                                                        
         ST    RB,WORK                                                          
         L     R4,VREGSAVE                                                      
         A     R4,=F'20000'                                                     
         ST    R4,WORK+4                                                        
         OI    WORK+4,X'80'                                                     
         GOTO1 =V(STXITER),DMCB,WORK                                            
         B     INIT2                                                            
*                                                                               
VREGSAVE DC    V(REGSAVE)                                                       
QSPTFIL  EQU   X'21'                                                            
         EJECT                                                                  
INIT2    DS   0H                                                                
*                                                                               
         ZAP   LINE,=P'99'                                                      
         GOTO1 =V(DATCON),DMCB,(5,0),(3,TODAY)                                  
*                                                                               
         SPACE                                                                  
         OPEN  (BUYSIN,(INPUT))                                                 
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OPEN  (RECVOUT,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TITLE(33),=CL33'COPY BUYS TO RECOVERY FILE FORMAT'               
         MVI   RSIN+3,1                                                         
*                                                                               
*** READ COPIED BUYS AND CREATE RECOVERY FILE ***                               
*                                                                               
IN2      DS   0H                                                                
         LA    R0,RKEY-4                                                        
         GET   BUYSIN,(0)                                                       
*                                                                               
         CLI   RKEY,X'10'                                                       
         BH    IN4                                                              
         AP    NONBUYCT,=P'1'                                                   
         B     IN2                                                              
*                                                                               
IN4      DS   0H                                                                
         AP    BUYCT,=P'1'                                                      
*                                                                               
         MVC   RCVLEN,RKEY-4       MOVE REC LEN                                 
         SR    RE,RE                                                            
         ICM   RE,3,RCVLEN                                                      
         LA    RE,24(,RE)          ADD 24 FOR RECOVERY HEADER                   
         STCM  RE,3,RCVLEN                                                      
         MVI   RFILTY,QSPTFIL      SET AS SPTFILE                               
*                                                                               
         LA    RE,RECVHDR-4                                                     
         AH    RE,0(RE)                                                         
         XC    0(2,RE),0(RE)       PUT ZEROES AT END OF RECORD                  
*                                                                               
         MVI   RRECTY,3            MAKE ADD                                     
         L     RF,RSIN             SET SIN NON-ZERO                             
         LA    RF,1(,RF)                                                        
         ST    RF,RSIN             SET SIN NON-ZERO                             
*                                                                               
         PUT   RECVOUT,RCVLEN                                                   
         SPACE                                                                  
         B     IN2                                                              
*                                                                               
EOFBUY   DS   0H                                                                
         CLOSE (BUYSIN,)                                                        
         CLOSE (RECVOUT,)                                                       
*                                                                               
         CP    BUYCT,=P'0'                                                      
         BNE   EOJ                                                              
*                                                                               
         MVC   P(16),=C'NO INPUT RECORDS'                                       
         GOTO1 =V(PRINTER)                                                      
         B     EOJ                                                              
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
EOJ      DS    0H                                                               
         MVC   P+1(24),=C'NON-BUY RECORDS COPIED ='                             
         LA    R2,P+25                                                          
         EDIT  NONBUYCT,(11,(R2)),0,COMMAS=YES,MINUS=YES                        
         GOTO1 =V(PRINTER)                                                      
         MVC   P+5(20),=C'BUY RECORDS COPIED ='                                 
         LA    R2,P+25                                                          
         EDIT  BUYCT,(11,(R2)),0,COMMAS=YES,MINUS=YES                           
         GOTO1 =V(PRINTER)                                                      
EOJ2     XBASE                                                                  
         EJECT                                                                  
*                                                                               
DUMPLIST DS    0F                                                               
         DC    A(TBACPY,65000)                                                  
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
*                                                                               
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
BYTE     DS    X                                                                
BYTE2    DS    X                                                                
         DC    C'**DMCB**'                                                      
DMCB     DS    6F                                                               
         DC    C'***KEY**'                                                      
KEY      DS    XL20                                                             
KEYSAVE  DS    XL20                                                             
DMWORK   DS    XL96                                                             
*                                                                               
WORK     DS    CL64                                                             
CARD     DS    CL80                                                             
BLOWKEY  DS    XL13                                                             
SVSYS    DS    CL8                                                              
TODAY    DS    XL3                                                              
*                                                                               
*                                                                               
BUYCT    DC    PL4'0'                                                           
NONBUYCT DC    PL4'0'                                                           
         EJECT                                                                  
*                                                                               
TBAMID1  DC    CL132'AG M CLT MKT  STAT    PRODUCT EST ACTIVE WEEKS'            
TBAMID2  DC    CL132'-- - --- ---  ----    ------- --- ------------'            
*                                                                               
P1       DC    CL132' '                                                         
P2       DC    CL132' '                                                         
P3       DC    CL132' '                                                         
P4       DC    CL132' '                                                         
P5       DC    CL132' '                                                         
P6       DC    CL132' '                                                         
P7       DC    CL132' '                                                         
P8       DC    CL132' '                                                         
P9       DC    CL132' '                                                         
P10      DC    CL132' '                                                         
P11      DC    CL132' '                                                         
P12      DC    CL132' '                                                         
*                                                                               
*                                                                               
*                                                                               
BUYSIN   DCB   DDNAME=BUYSIN,DSORG=PS,RECFM=VB,LRECL=4048,             +        
               MACRF=GM,EODAD=EOFBUY                                            
*                                                                               
RECVOUT  DCB   DDNAME=RECVOUT,DSORG=PS,RECFM=VB,LRECL=8200,            +        
               BLKSIZE=27648,MACRF=PM                                           
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 10                                                               
         DS    0D                                                               
         DS    0D                                                               
         DC    C'*RECVREC'                                                      
RCVLEN   DS    H                   LOGICAL IOCS LEN                             
         DS    H                                                                
*                                                                               
*                                                                               
       ++INCLUDE DMRCVRHDR                                                      
         SPACE 5                                                                
RKEY     DS    0CL13               IOAREA FOR RECOVERY RECORD                   
         DS    4048C                                                            
       ++INCLUDE DDDPRINT                                                       
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPTBACPY  11/11/11'                                      
         END                                                                    
